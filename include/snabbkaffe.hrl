-ifndef(SNABBKAFFE_HRL).
-define(SNABBKAFFE_HRL, true).

-ifdef(TEST).
-define(tp(Kind, Evt), snabbkaffe:tp(Kind, Evt)).
-else.
-define(tp(Kind, Evt), ?slog(debug, Evt #{kind => Kind})).
-endif.

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

-endif.
