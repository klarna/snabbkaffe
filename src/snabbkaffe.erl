-module(snabbkaffe).

-ifndef(SNK_COLLECTOR).
-define(SNK_COLLECTOR, true).
-endif.

-include("snabbkaffe.hrl").

%% API exports
-export([ start_trace/0
        , collect_trace/0
        , collect_trace/1
        , tp/2
        , push_stat/2
        , push_stat/3
        , push_stats/2
        , push_stats/3
        , analyze_statistics/0
        , get_stats/0
        , run/3
        , get_cfg/3
        , fix_ct_logging/0
        ]).

-export([ events_of_kind/2
        , projection/2
        , erase_timestamps/1
        , find_pairs/5
        , causality/5
        , unique/1
        , projection_complete/3
        , pair_max_depth/1
        , inc_counters/2
        , dec_counters/2
        ]).

-export([ mk_all/1
        , retry/3
        ]).

%%====================================================================
%% Types
%%====================================================================

-type kind() :: atom().

-type metric() :: atom().

-type timestamp() :: integer().

-type event() ::
        #{ kind := kind()
         , _ => _
         }.

-type timed_event() ::
        #{ kind := kind()
         , ts   := timestamp()
         , _ => _
         }.

-type trace() :: [event()].

-type maybe_pair() :: {pair, timed_event(), timed_event()}
                    | {singleton, timed_event()}.

-type maybe(A) :: {just, A} | nothing.

-type run_config() ::
        #{ bucket  => integer()
         , timeout => integer()
         }.

-export_type([ kind/0, timestamp/0, event/0, timed_event/0, trace/0
             , maybe_pair/0, maybe/1, metric/0, run_config/0
             ]).

-define(SERVER, snabbkaffe_collector).

%%====================================================================
%% API functions
%%====================================================================

-spec tp(atom(), map()) -> ok.
tp(Kind, Event) ->
  Event1 = Event #{ ts   => os:system_time()
                  , kind => Kind
                  },
  gen_server:cast(?SERVER, Event1).

-spec collect_trace() -> trace().
collect_trace() ->
  collect_trace(0).

-spec collect_trace(integer()) -> trace().
collect_trace(Timeout) ->
  snabbkaffe_collector:get_trace(Timeout).

-spec start_trace() -> ok.
start_trace() ->
  {ok, _} = snabbkaffe_collector:start(),
  ok.

%% @doc Extract events of certain kind(s) from the trace
-spec events_of_kind(kind() | [kind()], trace()) -> trace().
events_of_kind(Kind, Events) when is_atom(Kind) ->
  events_of_kind([Kind], Events);
events_of_kind(Kinds, Events) ->
  [E || E = #{kind := Kind} <- Events, lists:member(Kind, Kinds)].

-spec projection([atom()] | atom(), trace()) -> list().
projection(Field, Trace) when is_atom(Field) ->
  [maps:get(Field, I) || I <- Trace];
projection(Fields, Trace) ->
  [list_to_tuple([maps:get(F, I) || F <- Fields]) || I <- Trace].

-spec erase_timestamps(trace()) -> trace().
erase_timestamps(Trace) ->
  [maps:without([ts], I) || I <- Trace].

%% @doc Find pairs of complimentary events
-spec find_pairs( boolean()
                , fun((event()) -> boolean())
                , fun((event()) -> boolean())
                , fun((event(), event()) -> boolean())
                , trace()
                ) -> [maybe_pair()].
find_pairs(Strict, CauseP, EffectP, Guard, L) ->
  Fun = fun(A) ->
            C = fun_matches1(CauseP, A),
            E = fun_matches1(EffectP, A),
            if C orelse E ->
                {true, {A, C, E}};
               true ->
                false
            end
        end,
  L1 = lists:filtermap(Fun, L),
  do_find_pairs(Strict, Guard, L1).

-spec run( run_config() | integer()
         , fun()
         , fun()
         ) -> boolean().
run(Bucket, Run, Check) when is_integer(Bucket) ->
  run(#{bucket => Bucket}, Run, Check);
run(Config, Run, Check) ->
  Timeout = maps:get(timeout, Config, 0),
  Bucket  = maps:get(bucket, Config, undefined),
  start_trace(),
  %% Wipe the trace buffer clean:
  _ = collect_trace(0),
  snabbkaffe:tp('$trace_begin', #{}),
  try
    Return  = Run(),
    Trace   = collect_trace(Timeout),
    RunTime = ?find_pairs( false
                         , #{kind := '$trace_begin'}
                         , #{kind := '$trace_end'}
                         , Trace
                         ),
    push_stats(run_time, Bucket, RunTime),
    try Check(Return, Trace)
    catch EC1:Error1:Stack1 ->
        ?log(critical, "Check stage failed: ~p:~p~nStacktrace: ~p~n"
                     , [EC1, Error1, Stack1]
                     ),
        false
    end
  catch EC:Error:Stack ->
      ?log(critical, "Run stage failed: ~p:~p~nStacktrace: ~p~n"
                   , [EC, Error, Stack]
                   ),
      false
  end.

%%====================================================================
%% CT overhauls
%%====================================================================

%% @doc Implement `all/0' callback for Common Test
-spec mk_all(module()) -> [atom() | {group, atom()}].
mk_all(Module) ->
  io:format(user, "Module: ~p", [Module]),
  Groups = try Module:groups()
           catch
             error:undef -> []
           end,
  [{group, element(1, I)} || I <- Groups] ++
  [F || {F, _A} <- Module:module_info(exports),
        case atom_to_list(F) of
          "t_" ++ _ -> true;
          _         -> false
        end].

-spec retry(integer(), non_neg_integer(), fun(() -> Ret)) -> Ret.
retry(_, 0, Fun) ->
  Fun();
retry(Timeout, N, Fun) ->
  try Fun()
  catch
    EC:Err:Stack ->
      timer:sleep(Timeout),
      ?slog(debug, #{ what => retry_fun
                    , ec => EC
                    , error => Err
                    , stacktrace => Stack
                    }),
      retry(Timeout, N - 1, Fun)
  end.

-spec get_cfg([atom()], map() | proplists:proplist(), A) -> A.
get_cfg([Key|T], Cfg, Default) when is_list(Cfg) ->
  case lists:keyfind(Key, 1, Cfg) of
    false ->
      Default;
    {_, Val} ->
      case T of
        [] -> Val;
        _  -> get_cfg(T, Cfg, Default)
      end
  end;
get_cfg(Key, Cfg, Default) when is_map(Cfg) ->
  get_cfg(Key, maps:to_list(Cfg), Default).

-spec fix_ct_logging() -> ok.
fix_ct_logging() ->
  %% Fix CT logging by overriding it
  LogLevel = case os:getenv("LOGLEVEL") of
               S when S =:= "debug";
                      S =:= "info";
                      S =:= "error";
                      S =:= "critical";
                      S =:= "alert";
                      S =:= "emergency" ->
                 list_to_atom(S);
               _ ->
                 notice
             end,
  case os:getenv("KEEP_CT_LOGGING") of
    false ->
      logger:set_primary_config(level, LogLevel),
      logger:add_handler( full_log
                        , logger_std_h
                        , #{ formatter => {logger_formatter,
                                           #{ depth => 100
                                            , single_line => false
                                            , template => [msg]
                                            }}
                           }
                        );
    _ ->
      ok
  end.

%%====================================================================
%% Statistical functions
%%====================================================================

-spec push_stat(metric(), number()) -> ok.
push_stat(Metric, Num) ->
  push_stat(Metric, undefined, Num).

-spec push_stat(metric(), number() | undefined, number()) -> ok.
push_stat(Metric, X, Y) ->
  Val = case X of
          undefined ->
            Y;
          _ ->
            {X, Y}
        end,
  gen_server:call(?SERVER, {push_stat, Metric, Val}).

-spec push_stats(metric(), number(), [maybe_pair()] | number()) -> ok.
push_stats(Metric, Bucket, Pairs) ->
  lists:foreach( fun(Val) -> push_stat(Metric, Bucket, Val) end
               , transform_stats(Pairs)
               ).

-spec push_stats(metric(), [maybe_pair()] | number()) -> ok.
push_stats(Metric, Pairs) ->
  lists:foreach( fun(Val) -> push_stat(Metric, Val) end
               , transform_stats(Pairs)
               ).

get_stats() ->
  {ok, Stats} = gen_server:call(snabbkaffe_collector, get_stats),
  Stats.

analyze_statistics() ->
  Stats = get_stats(),
  maps:map(fun analyze_metric/2, Stats),
  ok.

%%====================================================================
%% Checks
%%====================================================================

-spec causality( boolean()
               , fun((event()) -> ok)
               , fun((event()) -> ok)
               , fun((event(), event()) -> boolean())
               , trace()
               ) -> ok.
causality(Strict, CauseP, EffectP, Guard, Trace) ->
  Pairs = find_pairs(true, CauseP, EffectP, Guard, Trace),
  if Strict ->
      [panic("Cause without effect: ~p", [I])
       || {singleton, I} <- Pairs];
     true ->
      ok
  end,
  ok.

-spec unique(trace()) -> true.
unique(Trace) ->
  Trace1 = erase_timestamps(Trace),
  Fun = fun(A, Acc) -> inc_counters([A], Acc) end,
  Counters = lists:foldl(Fun, #{}, Trace1),
  Dupes = [E || E = {_, Val} <- maps:to_list(Counters), Val > 1],
  case Dupes of
    [] ->
      true;
    _ ->
      panic("Duplicate elements found: ~p", [Dupes])
  end.

-spec projection_complete(atom(), trace(), [term()]) -> true.
projection_complete(Field, Trace, Expected) ->
  Got = ordsets:from_list([Val || #{Field := Val} <- Trace]),
  Expected1 = ordsets:from_list(Expected),
  case ordsets:subtract(Expected1, Got) of
    [] ->
      true;
    Missing ->
      panic("Trace is missing elements: ~p", [Missing])
  end.

-spec pair_max_depth([maybe_pair()]) -> non_neg_integer().
pair_max_depth(Pairs) ->
  TagPair =
    fun({pair, #{ts := T1}, #{ts := T2}}) ->
        [{T1, 1}, {T2, -1}];
       ({singleton, #{ts := T}}) ->
        [{T, 1}]
    end,
  L0 = lists:flatmap(TagPair, Pairs),
  L = lists:keysort(1, L0),
  CalcDepth =
    fun({_T, A}, {N0, Max}) ->
        N = N0 + A,
        {N, max(N, Max)}
    end,
  {_, Max} = lists:foldl(CalcDepth, {0, 0}, L),
  Max.

%%====================================================================
%% Internal functions
%%====================================================================

-spec panic(string(), [term()]) -> no_return().
panic(FmtString, Args) ->
  error({panic, FmtString, Args}).

-spec do_find_pairs( boolean()
                   , fun((event(), event()) -> boolean())
                   , [{event(), boolean(), boolean()}]
                   ) -> [maybe_pair()].
do_find_pairs(_Strict, _Guard, []) ->
  [];
do_find_pairs(Strict, Guard, [{A, C, E}|T]) ->
  FindEffect = fun({B, _, true}) ->
                   fun_matches2(Guard, A, B);
                  (_) ->
                   false
               end,
  case {C, E} of
    {true, _} ->
      case take(FindEffect, T) of
        {{B, _, _}, T1} ->
          [{pair, A, B}|do_find_pairs(Strict, Guard, T1)];
        T1 ->
          [{singleton, A}|do_find_pairs(Strict, Guard, T1)]
      end;
    {false, true} when Strict ->
      panic("Effect occures before cause: ~p", [A]);
    _ ->
      do_find_pairs(Strict, Guard, T)
  end.

-spec inc_counters([Key], Map) -> Map
        when Map :: #{Key => integer()}.
inc_counters(Keys, Map) ->
  Inc = fun(V) -> V + 1 end,
  lists:foldl( fun(Key, Acc) ->
                   maps:update_with(Key, Inc, 1, Acc)
               end
             , Map
             , Keys
             ).

-spec dec_counters([Key], Map) -> Map
        when Map :: #{Key => integer()}.
dec_counters(Keys, Map) ->
  Dec = fun(V) -> V - 1 end,
  lists:foldl( fun(Key, Acc) ->
                   maps:update_with(Key, Dec, -1, Acc)
               end
             , Map
             , Keys
             ).

-spec fun_matches1(fun((A) -> boolean()), A) -> boolean().
fun_matches1(Fun, A) ->
  try Fun(A)
  catch
    error:function_clause -> false
  end.

-spec fun_matches2(fun((A, B) -> boolean()), A, B) -> boolean().
fun_matches2(Fun, A, B) ->
  try Fun(A, B)
  catch
    error:function_clause -> false
  end.

-spec take(fun((A) -> boolean()), [A]) -> {A, [A]} | [A].
take(Pred, L) ->
  take(Pred, L, []).

take(_Pred, [], Acc) ->
  lists:reverse(Acc);
take(Pred, [A|T], Acc) ->
  case Pred(A) of
    true ->
      {A, lists:reverse(Acc) ++ T};
    false ->
      take(Pred, T, [A|Acc])
  end.

analyze_metric(MetricName, DataPoints = [N|_]) when is_number(N) ->
  %% This is a simple metric:
  Stats = bear:get_statistics(DataPoints),
  ?log(notice, "-------------------------------~n"
               "~p statistics:~n~p~n"
             , [MetricName, Stats]);
analyze_metric(MetricName, Datapoints = [{_, _}|_]) ->
  %% This "clustering" is not scientific at all
  {XX, _} = lists:unzip(Datapoints),
  Min = lists:min(XX),
  Max = lists:max(XX),
  NumBuckets = 10,
  BucketSize = max(1, (Max - Min) div NumBuckets),
  PushBucket =
    fun({X, Y}, Acc) ->
        B0 = (X - Min) div BucketSize,
        B = Min + B0 * BucketSize,
        maps:update_with( B
                        , fun(L) -> [Y|L] end
                        , [Y]
                        , Acc
                        )
    end,
  Buckets0 = lists:foldl(PushBucket, #{}, Datapoints),
  BucketStats =
    fun({Key, Vals}) when length(Vals) > 5 ->
        Stats = bear:get_statistics(Vals),
        {true, {Key, Stats}};
       (_) ->
        false
    end,
  Buckets = lists:filtermap( BucketStats
                           , lists:keysort(1, maps:to_list(Buckets0))
                           ),
  %% Print per-bucket stats:
  PlotPoints = [{Bucket, proplists:get_value(arithmetic_mean, Stats)}
                ||{Bucket, Stats} <- Buckets],
  Plot = asciiart:plot([{$*, PlotPoints}]),
  BucketStatsToString =
    fun({Key, Stats}) ->
        io_lib:format( "~10b ~e ~e ~e~n"
                     , [ Key
                       , proplists:get_value(min, Stats) * 1.0
                       , proplists:get_value(max, Stats) * 1.0
                       , proplists:get_value(arithmetic_mean, Stats) * 1.0
                       ])
    end,
  StatsStr = [ "Statisitics of ", atom_to_list(MetricName), $\n
             , asciiart:render(Plot)
             , "\n         N    min         max        avg\n"
             , [BucketStatsToString(I) || I <- Buckets]
             ],
  ?log(notice, "~s~n", [StatsStr]),
  %% Print more elaborate info for the last bucket
  case length(Buckets) of
    0 ->
      ok;
    _ ->
      {_, Last} = lists:last(Buckets),
      ?log(info, "Stats:~n~p~n", [Last])
  end.

transform_stats(Data) ->
  Fun = fun({pair, #{ts := T1}, #{ts := T2}}) ->
            Dt = erlang:convert_time_unit( T2 - T1
                                         , native
                                         , millisecond
                                         ),
            {true, Dt * 1.0e-6};
           (Num) when is_number(Num) ->
            {true, Num};
           (_) ->
            false
        end,
  lists:filtermap(Fun, Data).
