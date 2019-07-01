-module(overlap_depth_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("snabbkaffe.hrl").

-define(p(A, B), {pair, #{ts => A}, #{ts => B}}).
-define(s(A), {singleton, #{ts => A}}).

-define(matchDepth(N, L),
        ?assertMatch(N, ?pair_max_depth(L))).

disjoint_test() ->
  ?matchDepth(0, []),
  ?matchDepth(1, [?p(1, 1)]),
  ?matchDepth(1, [?p(0, 1)]),
  ?matchDepth(1, [?p(0, 1), ?p(1, 2)]),
  ?matchDepth(1, [?s(4), ?p(0, 1), ?p(2, 3)]).

nested_test() ->
  ?matchDepth(2, [?s(0), ?p(0, 1), ?p(2, 3)]),
  ?matchDepth(2, [?p(0, 3), ?p(1, 2), ?p(2, 3)]),
  ?matchDepth(3, [?p(0, 3), ?p(1, 3), ?p(2, 2)]),
  ?matchDepth(5, [?p(0, 3), ?s(1), ?p(1, 3), ?s(2), ?p(2, 2)]).

overlap_test() ->
  ?matchDepth(2, [?p(0, 2), ?p(1, 3)]),
  ?matchDepth(3, [?p(0, 2), ?p(1, 3), ?p(1.5, 2.5)]).
