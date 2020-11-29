-module(concuerror_tests).

-include("snabbkaffe.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([race_test/0, causality_test/0, fail_test/0]).

race_test() ->
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

causality_test() ->
  ?check_trace(
     begin
       C = spawn(fun() ->
                     receive ping ->
                         ?tp(pong, #{id => c})
                     end
                 end),
       B = spawn(fun() ->
                     receive ping ->
                         ?tp(pong, #{id => b}),
                         C ! ping
                     end
                 end),
       A = spawn(fun() ->
                     ?tp(pong, #{id => a}),
                     B ! ping
                 end),
       ?block_until(#{?snk_kind := pong, id := c})
     end,
     fun(_, Trace) ->
         ?assertEqual([a,b,c], ?projection(id, ?of_kind(pong, Trace)))
     end).

%% Check that testcases fail gracefully and don't try to do anything
%% that concuerror doesn't understand, like opening files:
fail_test() ->
  try
    ?check_trace(
       begin
         ?tp(foo, #{})
       end,
       fun(_, _) ->
           error(deliberate)
       end)
  catch
    _:_ -> ok
  end.
