-module(snabbkaffe_collector).

-ifndef(SNK_COLLECTOR).
-define(SNK_COLLECTOR, true).
-endif.

-include("snabbkaffe.hrl").

-behaviour(gen_server).

%% API
-export([start/0, stop/0, get_trace/1, get_stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-type datapoints() :: [{number(), number()}]
                    | number().

-record(s,
        { trace = []        :: [snabbkaffe:timed_event()]
        , stats = #{}       :: #{snabbkaffe:metric() => datapoints()}
        , last_event_ts = 0 :: integer()
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
  snabbkaffe:tp('$trace_end', #{}),
  {ok, Trace} = gen_server:call(?SERVER, {get_trace, Timeout}),
  Trace.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  process_flag(trap_exit, true),
  snabbkaffe:tp('$trace_begin', #{}),
  {ok, #s{}}.

handle_cast(Evt, S = #s{trace = T0}) ->
  {noreply, S#s{ trace         = [Evt|T0]
               , last_event_ts = erlang:monotonic_time()
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
handle_call(_Request, _From, State) ->
  Reply = unknown_call,
  {reply, Reply, State}.

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
