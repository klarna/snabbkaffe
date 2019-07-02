-module(collector_SUITE).

-compile(export_all).

-include_lib("snabbkaffe/include/ct_boilerplate.hrl").

%%====================================================================
%% CT callbacks
%%====================================================================

suite() ->
  [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
  snabbkaffe:fix_ct_logging(),
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

t_all_collected(_Config) when is_list(_Config) ->
  [?tp(foo, #{foo => I}) || I <- lists:seq(1, 1000)],
  Trace = snabbkaffe:collect_trace(),
  ?assertMatch(1000, length(?of_kind(foo, Trace))),
  ok.

t_check_trace(_Config) when is_list(_Config) ->
  ?check_trace(
     42,
     fun(Ret, Trace) ->
         ?assertMatch(42, Ret),
         ?assertMatch( [ #{kind := '$trace_begin'}
                       , #{kind := '$trace_end'}
                       ]
                     , Trace)
     end).

prop_async_collect() ->
  ?FORALL(
     {MaxWaitTime, Events},
     ?LET(MaxWaitTime, range(1, 100),
          {MaxWaitTime, [range(0, MaxWaitTime)]}),
     ?check_trace(
        #{timeout => MaxWaitTime + 10},
        %% Emit events with some sleep in between:
        [begin
           Id = make_ref(),
           spawn(fun() ->
                     timer:sleep(Sleep),
                     ?tp(async, #{id => Id})
                 end),
           Id
         end || Sleep <- Events],
        %% Check that all events have been collected:
        fun(Ids, Trace) ->
            ?projection_complete( id
                                , ?of_kind(async, Trace)
                                , Ids
                                )
        end)).

t_async_collect(Config) when is_list(Config) ->
  %% Verify that trace collection is delayed until last event (within
  %% timeout) is received:
  ?run_prop(Config, prop_async_collect()).

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

t_pair_metric(_Config) when is_list(_Config) ->
  [?tp(foo, #{i => I}) || I <- lists:seq(1, 100)],
  timer:sleep(10),
  [?tp(bar, #{i => I}) || I <- lists:seq(1, 100)],
  Trace = snabbkaffe:collect_trace(),
  Pairs = ?find_pairs( true
                     , #{kind := foo, i := I}, #{kind := bar, i := I}
                     , Trace
                     ),
  snabbkaffe:push_stats(foo_bar, Pairs).

t_pair_metric_buckets(_Config) when is_list(_Config) ->
  [?tp(foo, #{i => I}) || I <- lists:seq(1, 100)],
  timer:sleep(10),
  [?tp(bar, #{i => I}) || I <- lists:seq(1, 100)],
  Trace = snabbkaffe:collect_trace(),
  Pairs = ?find_pairs( true
                     , #{kind := foo, i := I}, #{kind := bar, i := I}
                     , Trace
                     ),
  snabbkaffe:push_stats(foo_bar, 10, Pairs).

t_run_1(_Config) when is_list(_Config) ->
  [?check_trace( I
               , begin
                   [?tp(foo, #{}) || J <- lists:seq(1, I)],
                   true
                 end
               , fun(Ret, Trace) ->
                     ?assertMatch(true, Ret),
                     ?assertMatch(I, length(?of_kind(foo, Trace)))
                 end
               )
   || I <- lists:seq(1, 1000)].

prop1() ->
  ?FORALL(
     {Ret, L}, {term(), list()},
     ?check_trace(
        length(L),
        begin
          [?tp(foo, #{i => I}) || I <- L],
          Ret
        end,
        fun(Ret1, Trace) ->
            ?assertMatch(Ret, Ret1),
            Foos = ?of_kind(foo, Trace),
            ?assertMatch(L, ?projection(i, Foos)),
            true
        end)).

t_proper(Config) when is_list(Config) ->
  ?run_prop(Config, prop1()).

t_forall_trace(Config0) when is_list(Config0) ->
  Config = [{proper, #{ max_size => 100
                      , numtests => 10000
                      }} | Config0],
  Prop =
    ?forall_trace(
       {Ret, L}, {term(), list()},
       length(L), %% Bucket
       begin
         [?tp(foo, #{i => I}) || I <- L],
         Ret
       end,
       fun(Ret1, Trace) ->
           ?assertMatch(Ret, Ret1),
           ?assertMatch(L, ?projection(i, ?of_kind(foo, Trace))),
           true
       end),
  ?run_prop(Config, Prop).

t_prop_fail_false(Config) when is_list(Config) ->
  Prop = ?forall_trace(
            X, list(),
            42,
            fun(_, _) ->
                false
            end
           ),
  ?assertExit( fail
             , ?run_prop(Config, Prop)
             ).

t_prop_run_exception(Config) when is_list(Config) ->
  Prop = ?forall_trace(
            X, list(),
            42, %% Bucket
            begin
              1 = 2
            end,
            fun(_, _) ->
                false
            end
           ),
  ?assertExit( fail
             , ?run_prop(Config, Prop)
             ).

t_prop_check_exception(Config) when is_list(Config) ->
  Prop = ?forall_trace(
            X, list(),
            42, %% Bucket
            ok,
            fun(_, _) ->
                1 = 2
            end
           ),
  ?assertExit( fail
             , ?run_prop(Config, Prop)
             ).
