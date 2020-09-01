%% Copyright 2019-2020 Klarna Bank AB
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
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
