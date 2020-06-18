-module(nemesis_SUITE).

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

%%====================================================================
%% Testcases
%%====================================================================

t_always_crash(Config) when is_list(Config) ->
  Run = fun(N) ->
            ?maybe_crash(foo, #{data => N})
        end,
  ?check_trace(
     begin
       ?assertMatch(ok, Run(1)),
       Ref = ?inject_crash( #{?snk_kind := foo, data := 1}
                          , snabbkaffe_nemesis:always_crash()
                          ),
       ?assertMatch(ok, Run(2)),
       ?assertError(notmyday, Run(1)),
       snabbkaffe_nemesis:fix_crash(Ref),
       ?assertMatch(ok, Run(1))
     end,
     fun(_Result, Trace) ->
         ?assertMatch( [#{crash_kind := foo, data := 1}]
                     , ?of_kind(snabbkaffe_crash, Trace)
                     )
     end).

t_recover(Config) when is_list(Config) ->
  N = 4,
  ?check_trace(
     begin
       ?inject_crash( #{?snk_kind := foo}
                    , snabbkaffe_nemesis:recover_after(N)
                    ),
       [catch ?maybe_crash(#{?snk_kind => foo}) || _ <- lists:seq(1, 2*N)]
     end,
     fun(_Result, Trace) ->
         ?assertEqual( N
                     , length(?of_kind(snabbkaffe_crash, Trace))
                     )
     end).

t_periodic(Config) when is_list(Config) ->
  F1 = snabbkaffe_nemesis:periodic_crash(5, 0.6, 0),
  ?assertEqual( [false, false, false, true, true, false, false, false, true, true]
              , [F1(I) || I <- lists:seq(1, 10)]
              ),
  F2 = snabbkaffe_nemesis:periodic_crash(5, 0.6, math:pi()),
  ?assertEqual( [true, true, false, false, false, true, true, false, false, false]
              , [F2(I) || I <- lists:seq(1, 10)]
              ).

%% Check that error can be injected at random trace point
t_break_trace_point(Config) when is_list(Config) ->
  N = 4,
  ?check_trace(
     begin
       ?inject_crash( #{?snk_kind := foo}
                    , snabbkaffe_nemesis:recover_after(N)
                    ),
       [catch ?tp(foo, #{}) || _ <- lists:seq(1, 2*N)]
     end,
     fun(_Result, Trace) ->
         ?assertEqual( N
                     , length(?of_kind(snabbkaffe_crash, Trace))
                     )
     end).

%% Check that static unique tokens are indeed unique
t_static_unique_points(Config) when is_list(Config) ->
  A = ?__snkStaticUniqueToken, B = ?__snkStaticUniqueToken,
  ?assertEqual(false, A =:= B).
