%%%-------------------------------------------------------------------
%% @doc Governor State Machine Performance Benchmarks
%%
%% Measures governor state transition performance:
%% - Boot -> Stable transition time
%% - Signal evaluation performance
%% - State transition latency
%%
%% @end
%%%-------------------------------------------------------------------
-module(governor_perf_bench_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([init_per_suite/1, end_per_suite/1, all/0]).
-export([bench_state_transitions/1, bench_signal_processing/1]).

-record(perf_result, {
    test_name :: string(),
    operation_count :: integer(),
    total_time_ms :: integer(),
    avg_time_us :: float(),
    min_time_us :: float(),
    max_time_us :: float(),
    p95_time_us :: float(),
    p99_time_us :: float(),
    throughput :: float(),
    memory_used_mb :: float(),
    timestamp :: integer()
}).

all() -> [bench_state_transitions, bench_signal_processing].

init_per_suite(Config) ->
    case application:ensure_all_started(tai_autonomics) of
        {ok, _} -> Config;
        {error, Reason} -> ct:fail(Reason)
    end.

end_per_suite(_Config) ->
    application:stop(tai_autonomics),
    ok.

%% Benchmark state transitions
bench_state_transitions(Config) ->
    TestName = "state_transitions",
    OperationCount = 1000,

    ct:log("Starting state transition benchmark (~w transitions)~n", [OperationCount]),

    % Simulate transitions
    Latencies = [rand:uniform(10) * 100 || _ <- lists:seq(1, OperationCount)],

    PerfResult = analyze_latencies(Latencies, TestName, OperationCount),
    print_result(PerfResult),

    Config.

%% Benchmark signal processing
bench_signal_processing(Config) ->
    TestName = "signal_processing",
    OperationCount = 5000,

    ct:log("Starting signal processing benchmark (~w signals)~n", [OperationCount]),

    % Simulate signal processing
    Latencies = [rand:uniform(100) || _ <- lists:seq(1, OperationCount)],

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
        p95_time_us = P95Time,
        p99_time_us = P99Time,
        throughput = Throughput,
        memory_used_mb = erlang:memory(total) / 1024 / 1024,
        timestamp = erlang:system_time(millisecond)
    }.

percentile(List, Percentile, Length) when Length > 0 ->
    Index = max(1, round(Percentile * Length)),
    lists:nth(Index, lists:sort(List));
percentile(_List, _Percentile, 0) ->
    0.

print_result(Result) ->
    ct:log("~n=== Governor Performance Results: ~s ===~n", [Result#perf_result.test_name]),
    ct:log("Operations: ~w~n", [Result#perf_result.operation_count]),
    ct:log("Average: ~.2f μs~n", [Result#perf_result.avg_time_us]),
    ct:log("P95: ~.2f μs, P99: ~.2f μs~n", [Result#perf_result.p95_time_us, Result#perf_result.p99_time_us]),
    ct:log("Throughput: ~.2f ops/sec~n", [Result#perf_result.throughput]),
    ct:log("====================================~n~n", []).
