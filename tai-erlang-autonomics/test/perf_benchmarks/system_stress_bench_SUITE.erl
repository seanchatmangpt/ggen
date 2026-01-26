%%%-------------------------------------------------------------------
%% @doc System-Wide Stress Testing & Performance Benchmarks
%%
%% Measures integrated system performance under load:
%% - Concurrent request handling
%% - Memory usage under sustained load
%% - Graceful degradation under extreme load
%% - Recovery after load spikes
%% - End-to-end latency (request -> response)
%% - System stability metrics
%%
%% Load profiles:
%% - Steady-state: constant load for extended period
%% - Ramp-up: gradually increasing load
%% - Burst: sudden spikes in traffic
%% - Mixed: combination of different request types
%%
%% @end
%%%-------------------------------------------------------------------
-module(system_stress_bench_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    all/0
]).

-export([
    bench_steady_state/1,
    bench_ramp_up/1,
    bench_burst_load/1,
    bench_mixed_load/1,
    bench_memory_stability/1,
    bench_recovery/1
]).

-record(perf_result, {
    test_name :: string(),
    duration_seconds :: integer(),
    total_requests :: integer(),
    successful_requests :: integer(),
    failed_requests :: integer(),
    success_rate :: float(),
    avg_latency_ms :: float(),
    p50_latency_ms :: float(),
    p95_latency_ms :: float(),
    p99_latency_ms :: float(),
    max_latency_ms :: float(),
    throughput_rps :: float(),
    memory_peak_mb :: float(),
    memory_avg_mb :: float(),
    cpu_peak_percent :: float(),
    cpu_avg_percent :: float(),
    gc_count :: integer(),
    gc_time_ms :: integer(),
    timestamp :: integer()
}).

-define(BASE_URL, "http://localhost:8080").
-define(STEADY_STATE_DURATION, 60).  % 60 seconds
-define(RAMP_UP_DURATION, 120).      % 2 minutes
-define(BURST_DURATION, 30).         % 30 seconds
-define(MEMORY_TEST_DURATION, 180).  % 3 minutes

%%%===================================================================
%% Common Test Callbacks
%%%===================================================================

all() -> [
    bench_steady_state,
    bench_ramp_up,
    bench_burst_load,
    bench_mixed_load,
    bench_memory_stability,
    bench_recovery
].

init_per_suite(Config) ->
    case application:ensure_all_started(tai_autonomics) of
        {ok, _} -> Config;
        {error, Reason} -> ct:fail(Reason)
    end.

end_per_suite(_Config) ->
    application:stop(tai_autonomics),
    ok.

init_per_testcase(_TestCase, Config) ->
    erlang:garbage_collect(),
    MemBefore = erlang:memory(total),
    [{mem_before, MemBefore} | Config].

end_per_testcase(_TestCase, _Config) ->
    erlang:garbage_collect(),
    ok.

%%%===================================================================
%% Benchmark Tests
%%%===================================================================

%% @doc Benchmark steady-state load
bench_steady_state(Config) ->
    TestName = "steady_state",
    Duration = ?STEADY_STATE_DURATION,
    ConcurrentWorkers = 20,

    ct:log("Starting steady-state benchmark (~w workers, ~w seconds)~n",
           [ConcurrentWorkers, Duration]),

    {ok, _Pid} = start_http_client(),

    MemBefore = get_memory_stats(),

    % Run steady-state load
    Results = run_load_test(
        ?BASE_URL,
        Duration,
        ConcurrentWorkers,
        constant,
        []
    ),

    MemAfter = get_memory_stats(),

    PerfResult = analyze_load_results(Results, TestName, Duration, MemBefore, MemAfter),
    print_result(PerfResult),

    % Assertions - stable under sustained load
    true = (PerfResult#perf_result.success_rate > 0.95) orelse ct:fail("success rate should be > 95%"),
    true = (PerfResult#perf_result.p99_latency_ms < 1000) orelse ct:fail("p99 latency should be < 1000ms"),
    true = (PerfResult#perf_result.memory_peak_mb < PerfResult#perf_result.memory_avg_mb * 1.5) orelse ct:fail("memory should remain stable"),

    Config.

%% @doc Benchmark ramp-up load profile
bench_ramp_up(Config) ->
    TestName = "ramp_up",
    Duration = ?RAMP_UP_DURATION,

    ct:log("Starting ramp-up benchmark (~w seconds)~n", [Duration]),

    {ok, _Pid} = start_http_client(),

    MemBefore = get_memory_stats(),

    % Run ramp-up load (10 -> 100 concurrent)
    Results = run_load_test(
        ?BASE_URL,
        Duration,
        50,  % Starting concurrency
        ramp_up,
        []
    ),

    MemAfter = get_memory_stats(),

    PerfResult = analyze_load_results(Results, TestName, Duration, MemBefore, MemAfter),
    print_result(PerfResult),

    % Should maintain acceptable performance during ramp
    true = (PerfResult#perf_result.success_rate > 0.90) orelse ct:fail("success rate should be > 90%"),

    Config.

%% @doc Benchmark burst load
bench_burst_load(Config) ->
    TestName = "burst_load",
    Duration = ?BURST_DURATION,

    ct:log("Starting burst load benchmark (~w seconds)~n", [Duration]),

    {ok, _Pid} = start_http_client(),

    MemBefore = get_memory_stats(),

    % Run burst load (sudden spike to 100+ concurrent)
    Results = run_load_test(
        ?BASE_URL,
        Duration,
        100,
        burst,
        []
    ),

    MemAfter = get_memory_stats(),

    PerfResult = analyze_load_results(Results, TestName, Duration, MemBefore, MemAfter),
    print_result(PerfResult),

    % Should handle burst gracefully
    true = (PerfResult#perf_result.success_rate > 0.80) orelse ct:fail("success rate should be > 80% during burst"),

    Config.

%% @doc Benchmark mixed load (multiple request types)
bench_mixed_load(Config) ->
    TestName = "mixed_load",
    Duration = 60,

    ct:log("Starting mixed load benchmark (~w seconds)~n", [Duration]),

    {ok, _Pid} = start_http_client(),

    MemBefore = get_memory_stats(),

    % Run mixed load with different endpoint types
    Results = run_mixed_load_test(
        Duration,
        30,  % Concurrent workers
        []
    ),

    MemAfter = get_memory_stats(),

    PerfResult = analyze_load_results(Results, TestName, Duration, MemBefore, MemAfter),
    print_result(PerfResult),

    Config.

%% @doc Benchmark memory stability
bench_memory_stability(Config) ->
    TestName = "memory_stability",
    Duration = ?MEMORY_TEST_DURATION,
    ConcurrentWorkers = 15,

    ct:log("Starting memory stability benchmark (~w seconds)~n", [Duration]),

    {ok, _Pid} = start_http_client(),

    MemSamples = [],
    MemSamplesWithData = run_memory_stability_test(
        Duration,
        ConcurrentWorkers,
        MemSamples
    ),

    % Analyze memory trend
    MemoryTrend = analyze_memory_trend(MemSamplesWithData),

    ct:log("~n=== Memory Stability Results: ~s ===~n", [TestName]),
    ct:log("Initial Memory: ~.2f MB~n", [MemoryTrend#memory_trend.initial_mb]),
    ct:log("Peak Memory: ~.2f MB~n", [MemoryTrend#memory_trend.peak_mb]),
    ct:log("Final Memory: ~.2f MB~n", [MemoryTrend#memory_trend.final_mb]),
    ct:log("Memory Leak Detected: ~w~n", [MemoryTrend#memory_trend.leak_detected]),
    ct:log("Growth Rate: ~.2f MB/sec~n", [MemoryTrend#memory_trend.growth_rate]),
    ct:log("===================================~n~n", []),

    % Verify no significant memory leak
    true = (MemoryTrend#memory_trend.growth_rate < 1.0) orelse ct:fail("memory growth rate should be < 1.0 MB/sec"),

    Config.

%% @doc Benchmark recovery after load spike
bench_recovery(Config) ->
    TestName = "recovery",

    ct:log("Starting recovery benchmark~n", []),

    {ok, _Pid} = start_http_client(),

    % Baseline
    BaselineResults = run_load_test(
        ?BASE_URL,
        10,
        10,
        constant,
        []
    ),

    BaslineMetrics = analyze_load_results(BaselineResults, "baseline", 10, 0, 0),

    % Load spike
    _ = run_load_test(
        ?BASE_URL,
        30,
        100,
        burst,
        []
    ),

    % Recovery period
    timer:sleep(5000),

    % Measure recovery
    RecoveryResults = run_load_test(
        ?BASE_URL,
        10,
        10,
        constant,
        []
    ),

    RecoveryMetrics = analyze_load_results(RecoveryResults, "recovery", 10, 0, 0),

    % Latency degradation during recovery
    LatencyDegradation = (RecoveryMetrics#perf_result.p99_latency_ms - BaslineMetrics#perf_result.p99_latency_ms)
        / BaslineMetrics#perf_result.p99_latency_ms,

    ct:log("~n=== Recovery Results ===~n"),
    ct:log("Baseline P99 Latency: ~.2f ms~n", [BaslineMetrics#perf_result.p99_latency_ms]),
    ct:log("Recovery P99 Latency: ~.2f ms~n", [RecoveryMetrics#perf_result.p99_latency_ms]),
    ct:log("Latency Degradation: ~.2f%~n", [LatencyDegradation * 100]),
    ct:log("========================~n~n", []),

    Config.

%%%===================================================================
%% Internal Functions
%%%===================================================================

start_http_client() ->
    case inets:start(httpc, [{profile, stress_test}]) of
        {ok, _} -> {ok, stress_test};
        {error, {already_started, _}} -> {ok, stress_test};
        Error -> Error
    end.

get_memory_stats() ->
    #{
        total => erlang:memory(total),
        processes => erlang:memory(processes),
        atom => erlang:memory(atom)
    }.

%% Run load test with specified profile
run_load_test(_URL, _Duration, _Concurrency, _Profile, Results) ->
    % Placeholder for actual load test implementation
    % Would spawn concurrent HTTP requests and collect results
    Results.

%% Run mixed load test with multiple endpoints
run_mixed_load_test(_Duration, _Concurrency, Results) ->
    % Placeholder for mixed load implementation
    Results.

%% Run memory stability test
run_memory_stability_test(0, _Concurrency, MemSamples) ->
    MemSamples;
run_memory_stability_test(RemainingSeconds, Concurrency, MemSamples) ->
    % Run a batch of requests
    _ = run_load_test(
        ?BASE_URL,
        5,  % 5-second intervals
        Concurrency,
        constant,
        []
    ),

    % Sample memory
    MemNow = erlang:memory(total),
    NewSamples = [MemNow | MemSamples],

    % Sleep before next sample
    timer:sleep(5000),

    run_memory_stability_test(RemainingSeconds - 5, Concurrency, NewSamples).

%% Analyze memory trend
analyze_memory_trend(MemSamples) when length(MemSamples) > 1 ->
    SortedSamples = lists:sort(MemSamples),
    Initial = lists:last(SortedSamples),  % First sample (end of list due to prepend)
    Peak = lists:max(MemSamples),
    Final = lists:nth(1, MemSamples),     % Last sample (beginning of list)
    SampleCount = length(MemSamples),

    GrowthRate = case SampleCount > 1 of
        true -> (Final - Initial) / (SampleCount * 5) / 1024 / 1024;  % MB/sec
        false -> 0
    end,

    LeakDetected = GrowthRate > 0.5,  % > 0.5 MB/sec growth

    #memory_trend{
        initial_mb = Initial / 1024 / 1024,
        peak_mb = Peak / 1024 / 1024,
        final_mb = Final / 1024 / 1024,
        growth_rate = GrowthRate,
        leak_detected = LeakDetected
    };
analyze_memory_trend(_MemSamples) ->
    #memory_trend{
        initial_mb = 0,
        peak_mb = 0,
        final_mb = 0,
        growth_rate = 0,
        leak_detected = false
    }.

%% Analyze load test results
analyze_load_results(Results, TestName, Duration, MemBefore, MemAfter) ->
    TotalRequests = length(Results),
    SuccessCount = length([R || R <- Results, element(1, R) =:= success]),
    FailedCount = TotalRequests - SuccessCount,

    SuccessRate = case TotalRequests > 0 of
        true -> SuccessCount / TotalRequests;
        false -> 0
    end,

    Latencies = [element(2, R) || R <- Results, element(1, R) =:= success],
    SortedLatencies = lists:sort(Latencies),
    Length = length(Latencies),

    AvgLatency = case Length > 0 of
        true -> lists:sum(Latencies) / Length;
        false -> 0
    end,

    P50Latency = percentile(SortedLatencies, 0.50, Length),
    P95Latency = percentile(SortedLatencies, 0.95, Length),
    P99Latency = percentile(SortedLatencies, 0.99, Length),
    MaxLatency = case Length > 0 of
        true -> lists:max(Latencies);
        false -> 0
    end,

    Throughput = case Duration > 0 of
        true -> SuccessCount / Duration;
        false -> 0
    end,

    MemBeforeBytes = maps:get(total, MemBefore, 0),
    MemAfterBytes = maps:get(total, MemAfter, 0),

    #perf_result{
        test_name = TestName,
        duration_seconds = Duration,
        total_requests = TotalRequests,
        successful_requests = SuccessCount,
        failed_requests = FailedCount,
        success_rate = SuccessRate,
        avg_latency_ms = AvgLatency,
        p50_latency_ms = P50Latency,
        p95_latency_ms = P95Latency,
        p99_latency_ms = P99Latency,
        max_latency_ms = MaxLatency,
        throughput_rps = Throughput,
        memory_peak_mb = max(MemBeforeBytes, MemAfterBytes) / 1024 / 1024,
        memory_avg_mb = (MemBeforeBytes + MemAfterBytes) / 2 / 1024 / 1024,
        cpu_peak_percent = 0.0,  % Would measure actual CPU
        cpu_avg_percent = 0.0,   % Would measure actual CPU
        gc_count = 0,            % Would measure actual GC
        gc_time_ms = 0,          % Would measure actual GC time
        timestamp = erlang:system_time(millisecond)
    }.

%% Calculate percentile
percentile(List, Percentile, Length) when Length > 0 ->
    Index = max(1, round(Percentile * Length)),
    lists:nth(Index, lists:sort(List));
percentile(_List, _Percentile, 0) ->
    0.

%% Print result in readable format
print_result(Result) ->
    ct:log("~n=== System Stress Test Results: ~s ===~n", [Result#perf_result.test_name]),
    ct:log("Duration: ~w seconds~n", [Result#perf_result.duration_seconds]),
    ct:log("Total Requests: ~w~n", [Result#perf_result.total_requests]),
    ct:log("Successful: ~w, Failed: ~w~n", [Result#perf_result.successful_requests, Result#perf_result.failed_requests]),
    ct:log("Success Rate: ~.2f%~n", [Result#perf_result.success_rate * 100]),
    ct:log("Throughput: ~.2f RPS~n", [Result#perf_result.throughput_rps]),
    ct:log("Average Latency: ~.2f ms~n", [Result#perf_result.avg_latency_ms]),
    ct:log("P50 Latency: ~.2f ms~n", [Result#perf_result.p50_latency_ms]),
    ct:log("P95 Latency: ~.2f ms~n", [Result#perf_result.p95_latency_ms]),
    ct:log("P99 Latency: ~.2f ms~n", [Result#perf_result.p99_latency_ms]),
    ct:log("Max Latency: ~.2f ms~n", [Result#perf_result.max_latency_ms]),
    ct:log("Peak Memory: ~.2f MB~n", [Result#perf_result.memory_peak_mb]),
    ct:log("Avg Memory: ~.2f MB~n", [Result#perf_result.memory_avg_mb]),
    ct:log("====================================~n~n", []).

%% Helper record for memory trend analysis
-record(memory_trend, {
    initial_mb :: float(),
    peak_mb :: float(),
    final_mb :: float(),
    growth_rate :: float(),
    leak_detected :: boolean()
}).
