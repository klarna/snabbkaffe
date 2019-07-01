-module(unique_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("snabbkaffe.hrl").

-define(valid(L),
        ?assertMatch(true, snabbkaffe:unique(L))).

-define(invalid(L),
        ?assertError( {panic, "Duplicate elements found: ~p", _}
                    , snabbkaffe:unique(L)
                    )).

unique_succ_test() ->
  ?valid([]),
  ?valid([ #{foo => 1, ts => 1}
         , #{bar => 1, ts => 2}
         , #{bar => 2, ts => 2}
         ]).

unique_fail_test() ->
  ?invalid([ #{foo => 1, ts => 1}
           , #{bar => 1, ts => 2}
           , #{foo => 1, ts => 2}
           ]).
