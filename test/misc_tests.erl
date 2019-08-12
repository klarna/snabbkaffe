-module(misc_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("snabbkaffe.hrl").

-define(foo(A), #{foo => A, bar => bar, baz => baz}).

projection_1_test() ->
  ?assertMatch( [1, 2, 3]
              , snabbkaffe:projection( foo
                                     , [?foo(1), ?foo(2), ?foo(3)]
                                     )).

projection_2_test() ->
  ?assertMatch( [{1, bar}, {2, bar}, {3, bar}]
              , snabbkaffe:projection( [foo, bar]
                                     , [?foo(1), ?foo(2), ?foo(3)]
                                     )).

strictly_increasing_test() ->
  ?assertMatch( true
              , snabbkaffe:strictly_increasing([])
              ),
  ?assertMatch( true
              , snabbkaffe:strictly_increasing([1])
              ),
  ?assertMatch( true
              , snabbkaffe:strictly_increasing([1, 2, 5, 6])
              ),
  ?assertError( _
              , snabbkaffe:strictly_increasing([1, 2, 5, 3])
              ),
  ?assertError( _
              , snabbkaffe:strictly_increasing([1, 2, 2, 3])
              ).
