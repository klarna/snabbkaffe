-ifndef(SNABBKAFFE_HRL).
-define(SNABBKAFFE_HRL, true).

-include_lib("hut/include/hut.hrl").

-ifdef(TEST).
-ifndef(SNK_COLLECTOR).
-define(SNK_COLLECTOR, true).
-endif. %% SNK_COLLECTOR
-endif. %% TEST

-ifdef(SNK_COLLECTOR).

-define(tp(Level, Kind, Evt), snabbkaffe_collector:tp(Kind, Evt)).
-define(tp(Kind, Evt), snabbkaffe_collector:tp(Kind, Evt)).

-define(of_kind(Kind, Trace),
        snabbkaffe:events_of_kind(Kind, Trace)).

-define(projection(Fields, Trace),
        snabbkaffe:projection(Fields, Trace)).

-define(find_pairs(Strict, M1, M2, Guard, Trace),
        snabbkaffe:find_pairs( Strict
                             , fun(__SnkArg) ->
                                   case __SnkArg of
                                     M1 -> true;
                                     _  -> false
                                   end
                               end
                             , fun(__SnkArg) ->
                                   case __SnkArg of
                                     M2 -> true;
                                     _  -> false
                                   end
                               end
                             , fun(M1, M2) -> (Guard) end
                             , (Trace)
                             )).

-define(find_pairs(Strict, M1, M2, Trace),
        ?find_pairs(Strict, M1, M2, true, Trace)).

-define(causality(M1, M2, Guard, Trace),
        snabbkaffe:causality( false
                            , fun(M1) -> true end
                            , fun(M2) -> true end
                            , fun(M1, M2) -> (Guard) end
                            , (Trace)
                            )).

-define(causality(M1, M2, Trace),
        ?causality(M1, M2, true, Trace)).

-define(strict_causality(M1, M2, Guard, Trace),
        snabbkaffe:causality( true
                            , fun(M1) -> true end
                            , fun(M2) -> true end
                            , fun(M1, M2) -> (Guard) end
                            , (Trace)
                            )).

-define(strict_causality(M1, M2, Trace),
        ?strict_causality(M1, M2, true, Trace)).

-define(pair_max_depth(L), snabbkaffe:pair_max_depth(L)).

-define(projection_complete(Field, Trace, L),
        snabbkaffe:projection_complete(Field, Trace, L)).

-define(projection_is_subset(Field, Trace, L),
        snabbkaffe:projection_is_subset(Field, Trace, L)).

-define(check_trace(Bucket, Run, Check),
        snabbkaffe:run( (fun() -> Bucket end)()
                      , fun() -> Run end
                      , Check
                      )).

-define(check_trace(RUN, CHECK),
        ?check_trace(#{}, RUN, CHECK)).

-define(run_prop(CONFIG, PROPERTY),
        (fun() ->
             __SnkTimeout  = snabbkaffe:get_cfg([proper, timeout], CONFIG, 5000),
             __SnkNumtests = snabbkaffe:get_cfg([proper, numtests], CONFIG, 100),
             __SnkMaxSize  = snabbkaffe:get_cfg([proper, max_size], CONFIG, 100),
             __SnkColors   = case os:getenv("TERM") of
                               "dumb" -> [nocolors];
                               _      -> []
                             end,
             __SnkPrint = fun(Fmt, Args) -> ?log(notice, Fmt, Args) end,
             __SnkRet = proper:quickcheck(
                          ?TIMEOUT( __SnkTimeout
                                  , begin
                                      ?log(info, asciiart:visible($', "Runnung ~s", [??PROPERTY])),
                                      PROPERTY
                                    end)
                         , [ {numtests, __SnkNumtests}
                           , {max_size, __SnkMaxSize}
                           , {on_output, __SnkPrint}
                           ] ++ __SnkColors
                         ),
             case __SnkRet of
               true ->
                 ok;
               Error ->
                 ?log(critical, asciiart:visible($!, "Proper test failed: ~p", [Error])),
                 exit(fail)
             end
         end)()).

-define(forall_trace(Xs, Xg, Bucket, Run, Check),
        ?FORALL(Xs, Xg, ?check_trace(Bucket, Run, Check))).

-define(forall_trace(Xs, Xg, Run, Check),
        ?forall_trace(Xs, Xg, #{}, Run, Check)).

-define(give_or_take(Expected, Deviation, Value),
        (fun() ->
             __SnkValue = (Value),
             __SnkExpected = (Expected),
             __SnkDeviation = (Deviation),
             case catch erlang:abs(__SnkValue - __SnkExpected) of
                 __SnkDelta when is_integer(__SnkDelta),
                                 __SnkDelta =< __SnkDeviation ->
                 true;
               _ ->
                 erlang:error({assertGiveOrTake,
                               [ {module, ?MODULE}
                               , {line, ?LINE}
                               , {expected, __SnkExpected}
                               , {value, __SnkValue}
                               , {expression, (??Value)}
                               , {max_deviation, __SnkDeviation}
                               ]})
             end
         end)()).

-define(retry(Timeout, N, Fun), snabbkaffe:retry(Timeout, N, fun() -> Fun end)).

-define(block_until(Match, Timeout, BackInTime),
        (fun() ->
             __SnkPredFun = fun(__SnkEvt) ->
                                case __SnkEvt of
                                  Match ->
                                    true;
                                  _ ->
                                    false
                                end
                            end,
             snabbkaffe:block_until(__SnkPredFun, (Timeout), (BackInTime))
         end)()).

-define(wait_async_action(Action, Match, Timeout),
        (fun() ->
             __SnkPredFun = fun(__SnkEvt) ->
                                case __SnkEvt of
                                  Match ->
                                    true;
                                  _ ->
                                    false
                                end
                            end,
             snabbkaffe:wait_async_action( fun() -> Action end
                                         , __SnkPredFun
                                         , (Timeout)
                                         )
         end)()).

-define(wait_async_action(Action, Match),
        ?wait_async_action(Action, Match, infinity)).

-define(block_until(Match, Timeout),
        ?block_until(Match, (Timeout), 100)).

-define(block_until(Match),
        ?block_until(Match, infinity)).

-define(split_trace_at(Pattern, Trace),
        (fun() ->
             __SnkSplitFun = fun(__SnkSplitArg) ->
                                 case __SnkSplitArg of
                                   Pattern -> false;
                                   _       -> true
                                 end
                             end,
             lists:splitwith(__SnkSplitFun, (Trace))
         end)()).

-else. %% SNK_COLLECTOR

-define(tp(Level, Kind, Evt), ?slog(Level, Evt #{kind => Kind})).

-define(tp(Kind, Evt), ?tp(debug, Kind, Evt)).

-endif. %% SNK_COLLECTOR
-endif. %% SNABBKAFFE_HRL
