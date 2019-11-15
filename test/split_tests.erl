-module(split_tests).

-include_lib("eunit/include/eunit.hrl").

splitl_test() ->
    Pred = fun is_atom/1,
    ?assertMatch( []
                , snabbkaffe:splitl(Pred, [])
                ),
    L1 = [[a]],
    ?assertMatch( L1
                , snabbkaffe:splitl(Pred, lists:append(L1))
                ),
    L2 = [[a, b, 1], [c, d, 2], [d]],
    ?assertMatch( L2
                , snabbkaffe:splitl(Pred, lists:append(L2))
                ),
    L3 = [[1], [2], [3]],
    ?assertMatch( L3
                , snabbkaffe:splitl(Pred, lists:append(L3))
                ).

splitr_test() ->
    Pred = fun is_atom/1,
    ?assertMatch( []
                , snabbkaffe:splitr(Pred, [])
                ),
    L1 = [[a]],
    ?assertMatch( L1
                , snabbkaffe:splitr(Pred, lists:append(L1))
                ),
    L2 = [[a, b], [1, c, d], [2, d]],
    ?assertMatch( L2
                , snabbkaffe:splitr(Pred, lists:append(L2))
                ),
    L3 = [[1], [2], [3]],
    ?assertMatch( L3
                , snabbkaffe:splitr(Pred, lists:append(L3))
                ).
