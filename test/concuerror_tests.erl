-module(concuerror_tests).

-include("snabbkaffe.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([test/0]).

test() ->
  ?check_trace(
     begin
       Parent = self(),
       Pid = spawn_link(fun() ->
                            receive
                              {ping, N} ->
                                ?tp(pong, #{winner => N})
                            end,
                            Parent ! done
                        end),
       %% Spawn two processes competing to send ping message to the
       %% first one:
       spawn_link(fun() ->
                      ?tp(ping, #{id => 1}),
                      Pid ! {ping, 1},
                      ok
                  end),
       spawn_link(fun() ->
                      ?tp(ping, #{id => 2}),
                      Pid ! {ping, 2},
                      ok
                  end),
       receive done -> ok end
     end,
     fun(_Ret, Trace) ->
         %% Validate that there's always a pair of events
         ?assertMatch( [{pair, _, _} | _]
                     , ?find_pairs( true
                                  , #{?snk_kind := ping}
                                  , #{?snk_kind := pong}
                                  , Trace
                                  )
                     ),
         %% TODO: I validated manually that value of `winner' field is
         %% indeed nondeterministic, and therefore snabbkaffe doesn't
         %% interfere with concuerror interleavings; however, it would
         %% be nice to check this property automatically as well, but
         %% it requires "testing outside the box":
         %%
         %% Both asserts are true:
         %% ?assertMatch([#{winner := 2}], ?of_kind(pong, Trace)),
         %% ?assertMatch([#{winner := 1}], ?of_kind(pong, Trace)),
         true
     end).
