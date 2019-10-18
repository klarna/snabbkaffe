-export([init_per_testcase/2, end_per_testcase/2, all/0]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("snabbkaffe/include/snabbkaffe.hrl").

init_per_testcase(TestCase, Config) ->
  case os:getenv("KEEP_CT_LOGGING") of
    false ->
      ?log(notice, asciiart:visible($%, "Running ~p", [TestCase]));
    _ ->
      ok
  end,
  Config1 = try apply(?MODULE, common_init_per_testcase, [TestCase, Config])
            catch
              error:undef -> Config
            end,
  Config2 = try apply(?MODULE, TestCase, [{init, Config1}])
            catch
              error:function_clause -> Config1
            end,
  ok = snabbkaffe:start_trace(),
  Config2.

end_per_testcase(TestCase, Config) ->
  try apply(?MODULE, TestCase, [{'end', Config}])
  catch
    error:function_clause -> ok
  end,
  try apply(?MODULE, common_end_per_testcase, [TestCase, Config])
  catch
    error:undef -> ok
  end,
  snabbkaffe:analyze_statistics(),
  snabbkaffe:stop(),
  case os:getenv("KEEP_CT_LOGGING") of
    false ->
      ?log(notice, asciiart:visible($%, "End of ~p", [TestCase]));
    _ ->
      ok
  end,
  ok.

all() ->
  application:ensure_all_started(snabbkaffe),
  snabbkaffe:mk_all(?MODULE).
