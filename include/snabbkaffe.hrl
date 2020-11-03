-ifndef(SNABBKAFFE_HRL).
-define(SNABBKAFFE_HRL, true).

-include_lib("hut/include/hut.hrl").

-ifdef(TEST).
-ifndef(SNK_COLLECTOR).
-define(SNK_COLLECTOR, true).
-endif. %% SNK_COLLECTOR
-endif. %% TEST

-define(snk_kind, '$kind'). %% "$" will make kind field go first when maps are printed.

-ifdef(SNK_COLLECTOR).

-define(panic(Kind, Args),
        error({panic, (Args) #{?snk_kind => (Kind)}})).

%% Dirty hack: we use reference to a local function as a key that can
%% be used to refer error injection points. This works, because all
%% invokations of this macro create a new fun object with unique id.
-define(__snkStaticUniqueToken, fun() -> ok end).

-define(maybe_crash(Kind, Data),
        snabbkaffe_nemesis:maybe_crash(Kind, Data#{?snk_kind => Kind})).

-define(maybe_crash(Data),
        snabbkaffe_nemesis:maybe_crash(?__snkStaticUniqueToken, Data)).

-define(tp(Level, Kind, Evt),
        (fun() ->
             ?maybe_crash(Evt #{?snk_kind => Kind}),
             snabbkaffe_collector:tp(Kind, Evt)
         end)()).

-define(tp(Kind, Evt), ?tp(debug, Kind, Evt)).

-define(of_kind(Kind, Trace),
        snabbkaffe:events_of_kind(Kind, Trace)).

-define(projection(Fields, Trace),
        snabbkaffe:projection(Fields, Trace)).

-define(snk_int_match_arg(ARG),
        fun(__SnkArg) ->
            case __SnkArg of
              ARG -> true;
              _   -> false
            end
        end).

-define(snk_int_match_arg2(M1, M2, Guard),
        fun(__SnkArg1, __SnkArg2) ->
            case __SnkArg1 of
              M1 ->
                case __SnkArg2 of
                  M2 -> (Guard);
                  _  -> false
                end;
              _ -> false
            end
        end).

-define(find_pairs(Strict, M1, M2, Guard, Trace),
        snabbkaffe:find_pairs( Strict
                             , ?snk_int_match_arg(M1)
                             , ?snk_int_match_arg(M2)
                             , ?snk_int_match_arg2(M1, M2, Guard)
                             , (Trace)
                             )).

-define(find_pairs(Strict, M1, M2, Trace),
        ?find_pairs(Strict, M1, M2, true, Trace)).

-define(causality(M1, M2, Guard, Trace),
        snabbkaffe:causality( false
                            , ?snk_int_match_arg(M1)
                            , ?snk_int_match_arg(M2)
                            , ?snk_int_match_arg2(M1, M2, Guard)
                            , (Trace)
                            )).

-define(causality(M1, M2, Trace),
        ?causality(M1, M2, true, Trace)).

-define(strict_causality(M1, M2, Guard, Trace),
        snabbkaffe:causality( true
                            , ?snk_int_match_arg(M1)
                            , ?snk_int_match_arg(M2)
                            , ?snk_int_match_arg2(M1, M2, Guard)
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
        (case snabbkaffe:run( (fun() -> Bucket end)()
                            , fun() -> Run end
                            , begin Check end
                            ) of
           true -> true;
           ok   -> true;
           {error, {panic, CrashKind, Args}} -> ?panic(CrashKind, Args);
           A -> ?panic("Unexpected result", #{result => A})
         end)).

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
             __SnkRet = proper:quickcheck(
                          ?TIMEOUT( __SnkTimeout
                                  , begin
                                      ?log(info, asciiart:visible($', "Runnung ~s", [??PROPERTY])),
                                      PROPERTY
                                    end)
                         , [ {numtests, __SnkNumtests}
                           , {max_size, __SnkMaxSize}
                           , {on_output, fun snabbkaffe:proper_printout/2}
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

-define(block_until(Pattern, Timeout, BackInTime),
        snabbkaffe:block_until(?snk_int_match_arg(Pattern), (Timeout), (BackInTime))).

-define(wait_async_action(Action, Pattern, Timeout),
        snabbkaffe:wait_async_action( fun() -> Action end
                                    , ?snk_int_match_arg(Pattern)
                                    , (Timeout)
                                    )).

-define(wait_async_action(Action, Pattern),
        ?wait_async_action(Action, Pattern, infinity)).

-define(block_until(Pattern, Timeout),
        ?block_until(Pattern, (Timeout), infinity)).

-define(block_until(Match),
        ?block_until(Match, infinity)).

-define(split_trace_at(Pattern, Trace),
        lists:splitwith(?snk_int_match_arg(Pattern), (Trace))).

-define(splitl_trace(Pattern, Trace),
        snabbkaffe:splitl(?snk_int_match_arg(Pattern), (Trace))).

-define(splitr_trace(Pattern, Trace),
        snabbkaffe:splitr(?snk_int_match_arg(Pattern), (Trace))).

-define(inject_crash(Pattern, Strategy, Reason),
        snabbkaffe_nemesis:inject_crash( ?snk_int_match_arg(Pattern)
                                       , (Strategy)
                                       , (Reason)
                                       )).

-define(inject_crash(Pattern, Strategy),
        ?inject_crash(Pattern, Strategy, notmyday)).

-else. %% SNK_COLLECTOR

-define(tp(Level, Kind, Evt), ?slog(Level, Evt #{?snk_kind => Kind})).

-define(tp(Kind, Evt), ?tp(debug, Kind, Evt)).

-define(maybe_crash(Kind, Data), ok).

-define(maybe_crash(Data), ok).

-endif. %% SNK_COLLECTOR
-endif. %% SNABBKAFFE_HRL
