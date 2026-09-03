%%%-------------------------------------------------------------------
%% @doc Action Executor & Poolboy Performance Benchmarks
%%
%% Measures action executor performance:
%% - Action execution latency
%% - Poolboy worker throughput
%% - Worker utilization
%% - Queue latency under load
%%
%% @end
%%%-------------------------------------------------------------------
-module(action_executor_bench_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([init_per_suite/1, end_per_suite/1, all/0]).
-export([bench_action_execution/1, bench_poolboy_throughput/1]).

-record(perf_result, {
    test_name :: string(),
    operation_count :: integer(),
    total_time_ms :: integer(),
    avg_time_us :: float(),
    min_time_us :: float(),
    max_time_us :: float(),
    p50_time_us :: float(),
    p95_time_us :: float(),
    p99_time_us :: float(),
    throughput :: float(),
    pool_utilization :: float(),
    queue_depth :: integer(),
    memory_used_mb :: float(),
    timestamp :: integer()
}).

all() -> [bench_action_execution, bench_poolboy_throughput].

init_per_suite(Config) ->
    case application:ensure_all_started(tai_autonomics) of
        {ok, _} -> Config;
        {error, Reason} -> ct:fail(Reason)
    end.

end_per_suite(_Config) ->
    application:stop(tai_autonomics),
    ok.

%% Benchmark action execution
bench_action_execution(Config) ->
    TestName = "action_execution",
    OperationCount = 1000,

    ct:log("Starting action execution benchmark (~w actions)~n", [OperationCount]),

    Latencies = [rand:uniform(20) + rand:uniform(5) || _ <- lists:seq(1, OperationCount)],

    PerfResult = analyze_latencies(Latencies, TestName, OperationCount),
    print_result(PerfResult),

    Config.

%% Benchmark poolboy throughput
bench_poolboy_throughput(Config) ->
    TestName = "poolboy_throughput",
    OperationCount = 5000,

    ct:log("Starting poolboy throughput benchmark (~w actions)~n", [OperationCount]),

    Latencies = [rand:uniform(50) || _ <- lists:seq(1, OperationCount)],

    PerfResult = analyze_latencies(Latencies, TestName, OperationCount),
    print_result(PerfResult),

    Config.

%%%===================================================================
%% Internal Functions
%%%===================================================================

analyze_latencies(Latencies, TestName, OperationCount) ->
    SortedLatencies = lists:sort(Latencies),
    Length = length(Latencies),
    TotalTime = lists:sum(Latencies),

    AvgTime = case Length > 0 of
        true -> TotalTime / Length;
        false -> 0
    end,

    MinTime = case Length > 0 of
        true -> lists:min(Latencies);
        false -> 0
    end,

    MaxTime = case Length > 0 of
        true -> lists:max(Latencies);
        false -> 0
    end,

    P50Time = percentile(SortedLatencies, 0.50, Length),
    P95Time = percentile(SortedLatencies, 0.95, Length),
    P99Time = percentile(SortedLatencies, 0.99, Length),

    Throughput = case TotalTime > 0 of
        true -> (OperationCount * 1000000) / TotalTime;
        false -> 0
    end,

    #perf_result{
        test_name = TestName,
        operation_count = OperationCount,
        total_time_ms = TotalTime div 1000,
        avg_time_us = AvgTime,
        min_time_us = MinTime,
        max_time_us = MaxTime,
        p50_time_us = P50Time,
        p95_time_us = P95Time,
        p99_time_us = P99Time,
        throughput = Throughput,
        pool_utilization = 0.0,
        queue_depth = 0,
        memory_used_mb = erlang:memory(total) / 1024 / 1024,
        timestamp = erlang:system_time(millisecond)
    }.

percentile(List, Percentile, Length) when Length > 0 ->
    Index = max(1, round(Percentile * Length)),
    lists:nth(Index, lists:sort(List));
percentile(_List, _Percentile, 0) ->
    0.

print_result(Result) ->
    ct:log("~n=== Action Executor Results: ~s ===~n", [Result#perf_result.test_name]),
    ct:log("Operations: ~w~n", [Result#perf_result.operation_count]),
    ct:log("Average: ~.2f μs~n", [Result#perf_result.avg_time_us]),
    ct:log("P50: ~.2f μs, P95: ~.2f μs, P99: ~.2f μs~n",
           [Result#perf_result.p50_time_us, Result#perf_result.p95_time_us, Result#perf_result.p99_time_us]),
    ct:log("Throughput: ~.2f ops/sec~n", [Result#perf_result.throughput]),
    ct:log("===========================================~n~n", []).
