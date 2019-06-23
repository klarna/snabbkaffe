-ifndef(SNABBKAFFE_HRL).
-define(SNABBKAFFE_HRL, true).

-ifdef(TEST).
-define(tp(Kind, Evt), snabbkaffe:tp(Kind, Evt)).
-else.
-define(tp(Kind, Evt), ?slog(debug, Evt #{kind => Kind})).
-endif.

-define(of_kind(Kind, Trace),
        snabbkaffe:events_of_kind(Kind, Trace)).

-define(projection(Fields, Trace),
        snabbkaffe:projection(Fields, Trace)).

-define(find_pairs(Strict, M1, M2, Guard, Trace),
        snabbkaffe:find_pairs( Strict
                             , fun(M1) -> true end
                             , fun(M2) -> true end
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

-define(check_trace(Bucket, Run, Check),
        snabbkaffe:run( (fun() -> Bucket end)()
                      , fun() -> Run end
                      , Check
                      )).

-define(check_trace(RUN, CHECK),
        ?check_trace(undefined, RUN, CHECK)).

-define(run_prop(CONFIG, PROPERTY),
        (fun() ->
             __SnkTimeout  = snabbkaffe:get_cfg([proper, timeout], CONFIG, 5000),
             __SnkNumtests = snabbkaffe:get_cfg([proper, numtests], CONFIG, 100),
             __SnkMaxSize  = snabbkaffe:get_cfg([proper, max_size], CONFIG, 100),
             __SnkColors   = case os:getenv("TERM") of
                               "dumb" -> [nocolors];
                               _      -> []
                             end,
             __SnkPrint = fun logger:notice/2,
             __SnkRet = proper:quickcheck( ?TIMEOUT(__SnkTimeout, PROPERTY)
                                         , [ {numtests, __SnkNumtests}
                                           , {max_size, __SnkMaxSize}
                                           , {on_output, __SnkPrint}
                                           ] ++ __SnkColors
                                         ),
             case __SnkRet of
               true ->
                 ok;
               Error ->
                 ?LOG_CRITICAL("!!!! Proper test failed: ~p~n", [Error]),
                 exit(fail)
             end
         end)()).

-define(forall_trace(Xs, Xg, Bucket, Run, Check),
        ?FORALL(Xs, Xg, ?check_trace(Bucket, Run, Check))).

-endif.
