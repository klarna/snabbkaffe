-export([init_per_testcase/2, end_per_testcase/2, all/0]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("snabbkaffe/include/snabbkaffe.hrl").

init_per_testcase(TestCase, Config) ->
  Config1 = try apply(?MODULE, TestCase, [{init, Config}])
            catch
              error:function_clause -> Config
            end,
  ok = snabbkaffe:start_trace(),
  Config1.

end_per_testcase(TestCase, Config) ->
  try apply(?MODULE, TestCase, [{'end', Config}])
  catch
    error:function_clause -> ok
  end,
  snabbkaffe:analyze_statistics(),
  snabbkaffe_collector:stop(),
  ok.

all() ->
  snabbkaffe:mk_all(?MODULE).
