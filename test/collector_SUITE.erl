-module(collector_SUITE).

-compile(export_all).

-include_lib("snabbkaffe/include/ct_boilerplate.hrl").

%%====================================================================
%% CT callbacks
%%====================================================================

suite() ->
  [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

init_per_group(_GroupName, Config) ->
  Config.

end_per_group(_GroupName, _Config) ->
  ok.

groups() ->
  [].

%%====================================================================
%% Testcases
%%====================================================================

t_foo(_Config) when is_list(_Config) ->
  [?tp(foo, #{foo => I}) || I <- lists:seq(1, 1000)],
  Trace = snabbkaffe:collect_trace(),
  ?assertMatch(1000, length(snabbkaffe:events_of_kind(foo, Trace))),
  ok.

t_bar({init, Config}) ->
  Config;
t_bar({'end', _Config}) ->
  ok;
t_bar(Config) when is_list(Config) ->
  ok.

t_simple_metric(_Config) when is_list(_Config) ->
  [snabbkaffe:push_stat(test, rand:uniform())
   || I <- lists:seq(1, 100)],
  ok.

t_bucket_metric(_Config) when is_list(_Config) ->
  [snabbkaffe:push_stat(test, 100 + I*10, I + rand:uniform())
   || I <- lists:seq(1, 100)
    , _ <- lists:seq(1, 10)],
  ok.
