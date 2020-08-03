%% Copyright 2019-2020 Klarna Bank AB
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(asciiart).

-export([ init/0
        , dimensions/1
        , render/1
        , char/3
        , char/2
        , line/4
        , line/3
        , string/4
        , string/3
        , plot/1
        , plot/2
        , draw/2
        , draw/1
        , visible/3
        ]).

%%====================================================================
%% Types
%%====================================================================

-type vector() :: {integer(), integer()}.

-type cont() :: fun((canvas()) -> canvas()).

-opaque canvas() :: #{vector() => char()}.

-type plot_data() :: [{char(), [{float(), float()}]}].

-export_type([vector/0, canvas/0]).

-define(epsilon, 1.0e-6).

%%====================================================================
%% API functions
%%====================================================================

-spec init() -> canvas().
init() ->
  #{}.

-spec render(canvas()) -> iolist().
render(Cnv) ->
  {{Xm, Ym}, {XM, YM}} = dimensions(Cnv),
  [[[maps:get({X, Y}, Cnv, $ ) || X <- lists:seq(Xm, XM)], $\n]
   || Y <- lists:reverse(lists:seq(Ym, YM))].

-spec draw([cont()], canvas()) -> canvas().
draw(Ops, Cnv) ->
  lists:foldl(fun(F, Acc) -> F(Acc) end, Cnv, Ops).

-spec draw([cont()]) -> canvas().
draw(Ops) ->
  draw(Ops, init()).

-spec dimensions(canvas()) -> {vector(), vector()}.
dimensions(Cnv) ->
  Fun = fun({X, Y}, _, {{Xm, Ym}, {XM, YM}}) ->
            { {min(X, Xm), min(Y, Ym)}
            , {max(X, XM), max(Y, YM)}
            }
        end,
  maps:fold(Fun, {{1, 1}, {1, 1}}, Cnv).

-spec char(canvas(), vector(), char()) -> canvas().
char(Cnv, Pos, Char) ->
  Cnv #{Pos => Char}.

-spec char(vector(), char()) -> cont().
char(Pos, Char) ->
  fun(Cnv) -> char(Cnv, Pos, Char) end.

-spec line(canvas(), vector(), vector(), char()) -> canvas().
line(Cnv, {X1, Y1}, {X2, Y2}, Char) ->
  X = X2 - X1,
  Y = Y2 - Y1,
  N = max(1, max(abs(X), abs(Y))),
  lists:foldl( fun(Pos, Cnv) -> char(Cnv, Pos, Char) end
             , Cnv
             , [{ X1 + round(X * I / N)
                , Y1 + round(Y * I / N)
                } || I <- lists:seq(0, N)]
             ).

-spec line(vector(), vector(), char()) -> cont().
line(F, T, C) ->
  fun(Cnv) -> line(Cnv, F, T, C) end.

-spec string(canvas(), vector(), string(), left | right) -> canvas().
string(Cnv, _, [], _) ->
  Cnv;
string(Cnv, {X, Y}, String, Direction) ->
  XL = case Direction of
         right ->
           lists:seq(X, X + length(String) - 1);
         left ->
           lists:seq(X - length(String) + 1, X)
       end,
  L = lists:zip(XL, String),
  lists:foldl( fun({X, Char}, Cnv) ->
                   char(Cnv, {X, Y}, Char)
               end
             , Cnv
             , L
             ).

-spec string(vector(), string(), left | right) -> cont().
string(Pos, Str, Dir) ->
  fun(Cnv) -> string(Cnv, Pos, Str, Dir) end.

-spec plot(plot_data()) -> canvas().
plot(Datapoints) ->
  plot(Datapoints, #{}).

-spec plot(plot_data(), map()) -> canvas().
plot(Datapoints, Config) ->
  AllDatapoints = lists:append([L || {_, L} <- Datapoints]),
  {XX, YY} = lists:unzip(AllDatapoints),
  Xm = bound(min, Config, XX),
  XM = bound(max, Config, XX),
  Ym = bound(min, Config, YY),
  YM = bound(max, Config, YY),
  DX = max(?epsilon, XM - Xm),
  DY = max(?epsilon, YM - Ym),
  %% Dimensions of the plot:
  AspectRatio = maps:get(aspect_ratio, Config, 0.2),
  Width = max(length(Datapoints) * 2, 70),
  Height = round(Width * AspectRatio),
  Frame = {{Xm, Ym}, {Width / DX, Height / DY}},
  %% Draw axis
  Cnv0 = draw( [ %% Vertical:
                 line({0, 0}, {0, Height - 1}, $|)
               , char({0, Height}, $^)
                 %% Labels:
               , string({-2, 0}, print_num(Ym), left)
               , string({-2, Height}, print_num(YM), left)
                 %% Horizontal:
               , line({0, 0}, {Width - 1, 0}, $-)
               , char({Width, 0}, $>)
               , char({0, 0}, $+)
                 %% Labels
               , string({0, -1}, print_num(Xm), right)
               , string({Width, -1}, print_num(XM), left)
               ]
             , init()
             ),
  lists:foldl( fun({Char, Data}, Acc) ->
                   draw_datapoints(Frame, Char, Data, Acc)
               end
             , Cnv0
             , Datapoints
             ).

draw_datapoints(Frame, Char, Data, Acc) ->
  lists:foldl( fun(Coords, Acc) ->
                   char(Acc, plot_coord(Frame, Coords), Char)
               end
             , Acc
             , Data
             ).

print_num(Num) when is_integer(Num) ->
  integer_to_list(Num);
print_num(Num) ->
  lists:flatten(io_lib:format("~.6..f", [Num])).

plot_coord({{Xm, Ym}, {SX, SY}}, {X, Y}) ->
  {round((X - Xm) * SX), round((Y - Ym) * SY)}.

bound(Fun, Cfg, L) ->
  N = case L of
        [] -> 0;
        _  -> lists:Fun(L)
      end,
  case maps:get(include_zero, Cfg, true) of
    true ->
      erlang:Fun(0, N);
    false ->
      N
  end.

-spec visible(char(), string(), [term()]) -> iolist().
visible(Char, Fmt, Args) ->
  Str = lines(lists:flatten(io_lib:format(Fmt, Args))),
  Width = max(79, lists:max([length(I) || I <- Str])) + 1,
  N = length(Str),
  Text = [string({4, Y}, S, right)
          || {Y, S} <- lists:zip( lists:seq(1, N)
                                , lists:reverse(Str)
                                )],
  Cnv = draw([ asciiart:line({1, -1}, {Width, -1}, Char)
             , asciiart:line({1, N + 2}, {Width, N + 2}, Char)
             , asciiart:line({1, 0}, {1, N + 1}, Char)
             , asciiart:line({2, 0}, {2, N + 1}, Char)
             , asciiart:line({Width - 1, 0}, {Width - 1, N + 1}, Char)
             , asciiart:line({Width, 0}, {Width, N + 1}, Char)
             ] ++ Text),
  [$\n, render(Cnv), $\n].

-spec lines(string()) -> [string()].
lines(Str) ->
  re:split(Str, "\n", [{return, list}]).
