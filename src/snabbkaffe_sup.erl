-module(snabbkaffe_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, stop/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

stop() ->
  case whereis(?MODULE) of
    undefined ->
      ok;
    Pid ->
      monitor(process, Pid),
      unlink(Pid),
      exit(Pid, shutdown),
      receive
        {'DOWN', _MRef, process, Pid, _} -> ok
      end
  end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  SupFlags = #{ strategy  => one_for_all
              , intensity => 0
              },
  Collector = #{ id    => snabbkaffe_collector
               , start => {snabbkaffe_collector, start_link, []}
               },
  Nemesis = #{ id    => snabbkaffe_nemesis
             , start => {snabbkaffe_nemesis, start_link, []}
             },
  {ok, {SupFlags, [Collector, Nemesis]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
