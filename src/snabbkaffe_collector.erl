-module(snabbkaffe_collector).

-behaviour(gen_server).

%% API
-export([start_link/0, get_trace/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(s,
        { trace = [] :: [snabbkaffe:timed_event()]
        }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec get_trace() -> snabbkaffe:timed_trace().
get_trace() ->
  get_trace(0).

-spec get_trace(integer()) -> snabbkaffe:timed_trace().
get_trace(Timeout) ->
  snabbkaffe:tp('$trace_end', #{}),
  Trace = gen_server:call(?SERVER, {get_trace, Timeout}),
  gen_server:stop(?SERVER),
  Trace.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  process_flag(trap_exit, true),
  snabbkaffe:tp('$trace_begin', #{}),
  {ok, #s{}}.

handle_cast(Evt, S = #s{trace = T0}) ->
  {noreply, S#s{trace = [Evt|T0]}}.

handle_call({get_trace, Timeout}, From, State) ->
  timer:send_after(Timeout, {flush, From}),
  {noreply, State};
handle_call(_Request, _From, State) ->
  Reply = unknown_call,
  {reply, Reply, State}.

handle_info({flush, To}, State = #s{trace = Trace}) ->
  gen_server:reply(To, lists:reverse(Trace)),
  {noreply, State};
handle_info(_, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
