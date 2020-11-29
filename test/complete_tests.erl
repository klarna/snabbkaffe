-module(complete_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("snabbkaffe.hrl").

-define(foo(A), #{foo => A, bar => bar}).

-define(valid(T, L),
        ?assertMatch( true
                    , ?projection_complete(foo, [?foo(I) || I <- T], L)
                    )).

-define(invalid(T, L),
        ?assertError( {panic, #{?snk_kind := "Trace is missing elements"}}
                    , ?projection_complete(foo, [?foo(I) || I <- T], L)
                    )).

complete_succ_test() ->
  ?valid([], []),
  ?valid([1, 3, 4], [1, 3, 4]),
  ?valid([1, 2, 2, 3], [1, 2, 3]).

complete_fail_test() ->
  ?invalid([1, 3, 4], [1, 3, 4, 5]),
  ?invalid([1, 2, 2, 3], [1, 2, 3, 4]).

multiple_fields_test() ->
  Fields = [foo, bar],
  Trace = [#{foo => 1, bar => 1}, #{foo => 2, bar => 2, baz => 2}],
  Pattern = [{1, 1}, {2, 2}],
  ?projection_complete(Fields, Trace, Pattern).
