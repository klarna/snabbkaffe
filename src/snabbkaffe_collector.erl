-module(snabbkaffe_collector).

-include("snabbkaffe_internal.hrl").

-behaviour(gen_server).

%% API
-export([ start/0
        , stop/0
        , get_trace/1
        , get_stats/0
        , block_until/3
        , notify_on_event/3
        , tp/2
        , push_stat/3
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export_type([async_action/0]).

-define(SERVER, ?MODULE).

-type datapoints() :: [{number(), number()}]
                    | number().

-type async_action() :: fun(({ok, snabbkaffe:event()} | timeout) -> _).

-record(callback,
        { async_action :: async_action()
        , predicate    :: snabbkaffe:predicate()
        , tref         :: reference() | undefined
        , ref          :: reference()
        }).

-record(s,
        { trace             :: [snabbkaffe:timed_event()]
        , stats = #{}       :: #{snabbkaffe:metric() => datapoints()}
        , last_event_ts = 0 :: integer()
        , callbacks = []    :: [#callback{}]
        }).

%%%===================================================================
%%% API
%%%===================================================================

-spec tp(atom(), map()) -> ok.
tp(Kind, Event) ->
  Event1 = Event #{ ts   => timestamp()
                  , kind => Kind
                  },
  ?slog(debug, Event1),
  gen_server:call(?SERVER, {trace, Event1}).

-spec push_stat(snabbkaffe:metric(), number() | undefined, number()) -> ok.
push_stat(Metric, X, Y) ->
  Val = case X of
          undefined ->
            Y;
          _ ->
            {X, Y}
        end,
  gen_server:call(?SERVER, {push_stat, Metric, Val}).

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
                     {ok, snabbkaffe:event()} | timeout.
block_until(Predicate, Timeout, BackInTime0) ->
  Infimum = case BackInTime0 of
              infinity ->
                beginning_of_times();
              _ ->
                BackInTime = erlang:convert_time_unit( BackInTime0
                                                     , millisecond
                                                     , native
                                                     ),
                erlang:monotonic_time() - BackInTime
            end,
  gen_server:call( ?SERVER
                 , {block_until, Predicate, Timeout, Infimum}
                 , infinity
                 ).

-spec notify_on_event(snabbkaffe:predicate(), timeout(), async_action()) ->
                         ok.
notify_on_event(Predicate, Timeout, Callback) ->
  gen_server:call( ?SERVER
                 , {notify_on_event, Callback, Predicate, Timeout}
                 ).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  process_flag(trap_exit, true),
  TS = timestamp(),
  BeginTrace = #{ ts   => TS
                , kind => '$trace_begin'
                },
  {ok, #s{ trace         = [BeginTrace]
         , last_event_ts = TS
         }}.

handle_cast(Evt, State) ->
  {noreply, State}.

handle_call({trace, Evt}, _From, State0 = #s{trace = T0, callbacks = CB0}) ->
  CB = maybe_unblock_someone(Evt, CB0),
  State = State0#s{ trace         = [Evt|T0]
                  , last_event_ts = timestamp()
                  , callbacks     = CB
                  },
  {reply, ok, State};
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
handle_call({block_until, Predicate, Timeout, Infimum}, From, State0) ->
  Callback = fun(Result) ->
                 gen_server:reply(From, Result)
             end,
  State = maybe_subscribe(Predicate, Timeout, Infimum, Callback, State0),
  {noreply, State};
handle_call({notify_on_event, Callback, Predicate, Timeout}, _From, State0) ->
  Now = erlang:monotonic_time(),
  State = maybe_subscribe(Predicate, Timeout, Now, Callback, State0),
  {reply, ok, State};
handle_call(_Request, _From, State) ->
  Reply = unknown_call,
  {reply, Reply, State}.

handle_info({timeout, Ref}, State) ->
  #s{callbacks = CB0} = State,
  Fun = fun(#callback{ref = Ref1, async_action = AsyncAction})
            when Ref1 =:= Ref ->
            AsyncAction(timeout),
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
  Dt = erlang:convert_time_unit( timestamp() - LastEventTs
                               , native
                               , millisecond
                               ),
  case is_finished(Dt, Timeout) of
    true ->
      TraceEnd = #{ kind => '$trace_end'
                  , ts   => LastEventTs
                  },
      Result = lists:reverse([TraceEnd|Trace]),
      gen_server:reply(To, {ok, Result}),
      {noreply, State #s{trace = []}};
    false ->
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
            #callback{ predicate    = Predicate
                     , async_action = AsyncAction
                     , tref         = TRef
                     } = Callback,
            case Predicate(Evt) of
              false ->
                {true, Callback};
              true  ->
                cancel_timer(TRef),
                AsyncAction({ok, Evt}),
                false
            end
        end,
  lists:filtermap(Fun, Callbacks).

-spec maybe_subscribe( snabbkaffe:predicate()
                     , timeout()
                     , integer()
                     , async_action()
                     , #s{}
                     ) -> #s{}.
maybe_subscribe(Predicate, Timeout, Infimum, AsyncAction, State0) ->
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
      AsyncAction({ok, Event}),
      State0;
    not_found ->
      %% 2. Postpone reply
      Ref = make_ref(),
      TRef = send_after(Timeout, self(), {timeout, Ref}),
      Callback = #callback{ async_action = AsyncAction
                          , predicate    = Predicate
                          , tref         = TRef
                          , ref          = Ref
                          },
      State0#s{ callbacks = [Callback|CB0]
              }
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

-spec timestamp() -> integer().
-ifndef(CONCUERROR).
timestamp() ->
  erlang:monotonic_time().
-else.
timestamp() ->
  -1.
-endif.

-spec beginning_of_times() -> integer().
-ifndef(CONCUERROR).
beginning_of_times() ->
  erlang:system_info(start_time).
-else.
beginning_of_times() ->
  -2.
-endif.

-spec is_finished(integer(), integer()) -> boolean().
-ifndef(CONCUERROR).
is_finished(Dt, SilenceInterval) ->
  Dt >= SilenceInterval.
-else.
is_finished(_, _) ->
  %% "Silence interval" feature doesn't work with Concuerror
  true.
-endif.
