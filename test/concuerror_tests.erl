-module(concuerror_tests).

-include("snabbkaffe.hrl").

-export([test/0]).

test() ->
  ?check_trace(
     begin
       Parent = self(),
       Pid = spawn_link(fun() ->
                            receive ping -> ok end,
                            ?tp(pong, #{}),
                            Parent ! done
                        end),
       spawn_link(fun() ->
                      ?tp(ping, #{}),
                      Pid ! ping,
                      ok
                  end),
       receive done -> ok end,
       ok
     end,
     fun(_Ret, Trace) ->
         ?strict_causality( #{kind := ping}
                          , #{kind := pong}
                          , Trace
                          ),
         true
     end).
