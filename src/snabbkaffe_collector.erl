-module(snabbkaffe_collector).

-ifndef(SNK_COLLECTOR).
-define(SNK_COLLECTOR, true).
-endif.

-include("snabbkaffe.hrl").

-behaviour(gen_server).

%% API
-export([start/0, stop/0, get_trace/1, get_stats/0, block_until/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-type datapoints() :: [{number(), number()}]
                    | number().

-record(callback,
        { from      :: gen_server:from()
        , predicate :: snabbkaffe:predicate()
        , tref      :: reference() | undefined
        }).

-record(s,
        { trace = []        :: [snabbkaffe:timed_event()]
        , stats = #{}       :: #{snabbkaffe:metric() => datapoints()}
        , last_event_ts = 0 :: integer()
        , callbacks = []    :: [#callback{}]
        }).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
  case whereis(?SERVER) of
    undefined ->
      gen_server:start({local, ?SERVER}, ?MODULE, [], []);
    Pid ->
      {ok, Pid}
  end.

stop() ->
  gen_server:stop(?SERVER).

-spec get_stats() -> datapoints().
get_stats() ->
  gen_server:call(?SERVER, get_stats).

-spec get_trace(integer()) -> snabbkaffe:timed_trace().
get_trace(Timeout) ->
  {ok, Trace} = gen_server:call(?SERVER, {get_trace, Timeout}, infinity),
  Trace.

-spec block_until(snabbkaffe:predicate(), timeout(), timeout()) ->
                     snabbkaffe:event() | timeout.
block_until(Predicate, Timeout, BackInTime0) ->
  Infimum = case BackInTime0 of
              infinity ->
                0;
              _ ->
                BackInTime = erlang:convert_time_unit( BackInTime0
                                                     , millisecond
                                                     , native
                                                     ),
                erlang:monotonic_time() - BackInTime
            end,
  {ok, Ret} = gen_server:call( ?SERVER
                             , {block_until, Predicate, Timeout, Infimum}
                             , infinity
                             ),
  Ret.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  process_flag(trap_exit, true),
  snabbkaffe:tp('$trace_begin', #{}),
  {ok, #s{}}.

handle_cast(Evt, S = #s{trace = T0, callbacks = CB0}) ->
  CB = maybe_unblock_someone(Evt, CB0),
  {noreply, S#s{ trace         = [Evt|T0]
               , last_event_ts = erlang:monotonic_time()
               , callbacks     = CB
               }}.

handle_call({push_stat, Metric, Stat}, _From, State0) ->
  Stats = maps:update_with( Metric
                          , fun(L) -> [Stat|L] end
                          , [Stat]
                          , State0#s.stats
                          ),
  {reply, ok, State0#s{stats = Stats}};
handle_call(get_stats, _From, State) ->
  {reply, {ok, State#s.stats}, State};
handle_call({get_trace, Timeout}, From, State) ->
  timer:send_after(Timeout, {flush, From, Timeout}),
  {noreply, State};
handle_call({block_until, Predicate, Timeout, Infimum}, From, State) ->
  handle_block_until(Predicate, Timeout, Infimum, From, State);
handle_call(_Request, _From, State) ->
  Reply = unknown_call,
  {reply, Reply, State}.

handle_info({timeout, From}, State) ->
  #s{callbacks = CB0} = State,
  Fun = fun(#callback{from = F}) when F =:= From ->
            gen_server:reply(From, {ok, timeout}),
            false;
           (C) ->
            {true, C}
        end,
  CB = lists:filtermap(Fun, CB0),
  {noreply, State#s{callbacks = CB}};
handle_info(Event = {flush, To, Timeout}, State) ->
  #s{ trace         = Trace
    , last_event_ts = LastEventTs
    } = State,
  Dt = erlang:convert_time_unit( erlang:monotonic_time() - LastEventTs
                               , native
                               , millisecond
                               ),
  if Dt >= Timeout ->
      TraceEnd = #{ kind => '$trace_end'
                  , ts   => LastEventTs
                  },
      Result = lists:reverse([TraceEnd|Trace]),
      gen_server:reply(To, {ok, Result}),
      {noreply, State #s{trace = []}};
     true ->
      timer:send_after(Timeout, Event),
      {noreply, State}
  end;
handle_info(_, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec maybe_unblock_someone( snabbkaffe:event()
                           , [#callback{}]
                           ) -> [#callback{}].
maybe_unblock_someone(Evt, Callbacks) ->
  Fun = fun(Callback) ->
            #callback{ predicate = Predicate
                     , from      = From
                     , tref      = TRef
                     } = Callback,
            case Predicate(Evt) of
              false ->
                {true, Callback};
              true  ->
                cancel_timer(TRef),
                gen_server:reply(From, {ok, Evt}),
                false
            end
        end,
  lists:filtermap(Fun, Callbacks).

-spec handle_block_until( snabbkaffe:predicate()
                        , timeout()
                        , integer()
                        , gen_server:from()
                        , #s{}
                        ) ->
                            {reply, _, #s{}}
                          | {noreply, #s{}}.
handle_block_until(Predicate, Timeout, Infimum, From, State0) ->
  #s{ trace     = Trace
    , callbacks = CB0
    } = State0,
  try
    %% 1. Search in the past events
    [case Evt of
       #{ts := Ts} when Ts > Infimum ->
         case Predicate(Evt) of
           true ->
             throw({found, Evt});
           false ->
             ok
         end;
       _ ->
         throw(not_found)
     end
     || Evt <- Trace],
    throw(not_found)
  catch
    {found, Event} ->
      {reply, {ok, Event}, State0};
    not_found ->
      %% 2. Postpone reply
      TRef = send_after(Timeout, self(), {timeout, From}),
      Callback = #callback{ from      = From
                          , predicate = Predicate
                          , tref      = TRef
                          },
      State = State0#s{ callbacks = [Callback|CB0]
                      },
      {noreply, State}
  end.

-spec send_after(timeout(), pid(), _Msg) ->
                    reference() | undefined.
send_after(infinity, _, _) ->
  undefined;
send_after(Timeout, Pid, Msg) ->
  erlang:send_after(Timeout, Pid, Msg).

-spec cancel_timer(reference() | undefined) -> _.
cancel_timer(undefined) ->
  ok;
cancel_timer(TRef) ->
  erlang:cancel_timer(TRef).
