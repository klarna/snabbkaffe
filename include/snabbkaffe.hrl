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

-define(panic(KIND, ARGS),
        error({panic, (ARGS) #{?snk_kind => (KIND)}})).

%% Dirty hack: we use reference to a local function as a key that can
%% be used to refer error injection points. This works, because all
%% invokations of this macro create a new fun object with unique id.
-define(__snkStaticUniqueToken, fun() -> ok end).

-define(maybe_crash(KIND, DATA),
        snabbkaffe_nemesis:maybe_crash(KIND, DATA#{?snk_kind => KIND})).

-define(maybe_crash(DATA),
        snabbkaffe_nemesis:maybe_crash(?__snkStaticUniqueToken, DATA)).

-define(tp(LEVEL, KIND, EVT),
        (fun() ->
             ?maybe_crash(EVT #{?snk_kind => KIND}),
             snabbkaffe_collector:tp(KIND, EVT)
         end)()).

-define(tp(KIND, EVT), ?tp(debug, KIND, EVT)).

-define(of_kind(KIND, TRACE),
        snabbkaffe:events_of_kind(KIND, TRACE)).

-define(projection(FIELDS, TRACE),
        snabbkaffe:projection(FIELDS, TRACE)).

-define(snk_int_match_arg(ARG),
        fun(__SnkArg) ->
            case __SnkArg of
              ARG -> true;
              _   -> false
            end
        end).

-define(snk_int_match_arg2(M1, M2, GUARD),
        fun(__SnkArg1, __SnkArg2) ->
            case __SnkArg1 of
              M1 ->
                case __SnkArg2 of
                  M2 -> (GUARD);
                  _  -> false
                end;
              _ -> false
            end
        end).

-define(find_pairs(STRICT, M1, M2, GUARD, TRACE),
        snabbkaffe:find_pairs( STRICT
                             , ?snk_int_match_arg(M1)
                             , ?snk_int_match_arg(M2)
                             , ?snk_int_match_arg2(M1, M2, GUARD)
                             , (TRACE)
                             )).

-define(find_pairs(STRICT, M1, M2, TRACE),
        ?find_pairs(STRICT, M1, M2, true, TRACE)).

-define(causality(M1, M2, GUARD, TRACE),
        snabbkaffe:causality( false
                            , ?snk_int_match_arg(M1)
                            , ?snk_int_match_arg(M2)
                            , ?snk_int_match_arg2(M1, M2, GUARD)
                            , (TRACE)
                            )).

-define(causality(M1, M2, Trace),
        ?causality(M1, M2, true, Trace)).

-define(strict_causality(M1, M2, GUARD, TRACE),
        snabbkaffe:causality( true
                            , ?snk_int_match_arg(M1)
                            , ?snk_int_match_arg(M2)
                            , ?snk_int_match_arg2(M1, M2, GUARD)
                            , (TRACE)
                            )).

-define(strict_causality(M1, M2, TRACE),
        ?strict_causality(M1, M2, true, TRACE)).

-define(pair_max_depth(PAIRS), snabbkaffe:pair_max_depth(PAIRS)).

-define(projection_complete(FIELD, TRACE, L),
        snabbkaffe:projection_complete(FIELD, TRACE, L)).

-define(projection_is_subset(FIELD, TRACE, L),
        snabbkaffe:projection_is_subset(FIELD, TRACE, L)).

-define(check_trace(BUCKET, RUN, CHECK),
        (case snabbkaffe:run( (fun() -> BUCKET end)()
                            , fun() -> RUN end
                            , begin CHECK end
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

-define(forall_trace(XS, XG, BUCKET, RUN, CHECK),
        ?FORALL(XS, XG, ?check_trace(BUCKET, RUN, CHECK))).

-define(forall_trace(XS, XG, RUN, CHECK),
        ?forall_trace(XS, XG, #{}, RUN, CHECK)).

-define(give_or_take(EXPECTED, DEVIATION, VALUE),
        (fun() ->
             __SnkValue = (VALUE),
             __SnkExpected = (EXPECTED),
             __SnkDeviation = (DEVIATION),
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
                               , {expression, (??VALUE)}
                               , {max_deviation, __SnkDeviation}
                               ]})
             end
         end)()).

-define(retry(TIMEOUT, N, FUN), snabbkaffe:retry(TIMEOUT, N, fun() -> FUN end)).

-define(block_until(PATTERN, TIMEOUT, BACK_IN_TIME),
        snabbkaffe:block_until(?snk_int_match_arg(PATTERN), (TIMEOUT), (BACK_IN_TIME))).

-define(wait_async_action(ACTION, PATTERN, TIMEOUT),
        snabbkaffe:wait_async_action( fun() -> ACTION end
                                    , ?snk_int_match_arg(PATTERN)
                                    , (TIMEOUT)
                                    )).

-define(wait_async_action(ACTION, PATTERN),
        ?wait_async_action(ACTION, PATTERN, infinity)).

-define(block_until(PATTERN, TIMEOUT),
        ?block_until(PATTERN, (TIMEOUT), infinity)).

-define(block_until(MATCH),
        ?block_until(MATCH, infinity)).

-define(split_trace_at(PATTERN, TRACE),
        lists:splitwith(?snk_int_match_arg(PATTERN), (TRACE))).

-define(splitl_trace(PATTERN, TRACE),
        snabbkaffe:splitl(?snk_int_match_arg(PATTERN), (TRACE))).

-define(splitr_trace(PATTERN, TRACE),
        snabbkaffe:splitr(?snk_int_match_arg(PATTERN), (TRACE))).

-define(inject_crash(PATTERN, STRATEGY, REASON),
        snabbkaffe_nemesis:inject_crash( ?snk_int_match_arg(PATTERN)
                                       , (STRATEGY)
                                       , (REASON)
                                       )).

-define(inject_crash(PATTERN, STRATEGY),
        ?inject_crash(PATTERN, STRATEGY, notmyday)).

-else. %% SNK_COLLECTOR

-define(tp(LEVEL, KIND, EVT), ?slog(LEVEL, EVT #{?snk_kind => KIND})).

-define(tp(KIND, EVT), ?tp(debug, KIND, EVT)).

-define(maybe_crash(KIND, DATA), ok).

-define(maybe_crash(DATA), ok).

-endif. %% SNK_COLLECTOR
-endif. %% SNABBKAFFE_HRL
