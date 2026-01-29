%%%-------------------------------------------------------------------
%%% @doc
%%% Custom throughput benchmark
%%%
%%% Measures maximum sustained throughput without SLA constraints.
%%% Useful for capacity planning and stress testing.
%%% @end
%%%-------------------------------------------------------------------
-module(throughput_bench).

-export([
    run/1,
    measure_throughput/2,
    analyze_results/1
]).

-record(benchmark_config, {
    workers :: pos_integer(),
    duration_sec :: pos_integer(),
    target_node :: node(),
    warmup_sec :: non_neg_integer()
}).

-record(worker_result, {
    worker_id :: pos_integer(),
    operations :: non_neg_integer(),
    errors :: non_neg_integer(),
    latencies :: [pos_integer()]
}).

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc Run throughput benchmark with default config
run(Config) when is_map(Config) ->
    BenchConfig = parse_config(Config),
    run_benchmark(BenchConfig).

%% @doc Measure throughput for specific duration
measure_throughput(NumWorkers, DurationSec) ->
    Config = #{
        workers => NumWorkers,
        duration_sec => DurationSec,
        warmup_sec => 5
    },
    run(Config).

%% @doc Analyze benchmark results
analyze_results(Results) ->
    TotalOps = lists:sum([R#worker_result.operations || R <- Results]),
    TotalErrors = lists:sum([R#worker_result.errors || R <- Results]),
    AllLatencies = lists:flatten([R#worker_result.latencies || R <- Results]),

    #{
        total_operations => TotalOps,
        total_errors => TotalErrors,
        error_rate => (TotalErrors / max(1, TotalOps)) * 100,
        latency_stats => calculate_percentiles(AllLatencies)
    }.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

parse_config(Config) ->
    #benchmark_config{
        workers = maps:get(workers, Config, 100),
        duration_sec = maps:get(duration_sec, Config, 60),
        target_node = maps:get(target_node, Config, node()),
        warmup_sec = maps:get(warmup_sec, Config, 5)
    }.

run_benchmark(Config) ->
    io:format("~n=== Throughput Benchmark ===~n"),
    io:format("Workers: ~p~n", [Config#benchmark_config.workers]),
    io:format("Duration: ~p seconds~n", [Config#benchmark_config.duration_sec]),
    io:format("Warmup: ~p seconds~n", [Config#benchmark_config.warmup_sec]),
    io:format("~n"),

    %% Warmup phase
    io:format("Warmup phase...~n"),
    warmup(Config#benchmark_config.warmup_sec),
    io:format("Warmup complete~n~n"),

    %% Start benchmark
    io:format("Starting benchmark...~n"),
    StartTime = erlang:system_time(second),

    %% Spawn workers
    Workers = spawn_workers(Config),

    %% Wait for completion
    wait_for_completion(Config#benchmark_config.duration_sec),

    %% Collect results
    io:format("~nCollecting results...~n"),
    Results = collect_results(Workers),

    EndTime = erlang:system_time(second),
    ActualDuration = EndTime - StartTime,

    %% Analyze and report
    Analysis = analyze_results(Results),
    report_results(Analysis, ActualDuration),

    {ok, Analysis}.

warmup(DurationSec) ->
    %% Send warmup requests
    EndTime = erlang:system_time(second) + DurationSec,
    warmup_loop(EndTime).

warmup_loop(EndTime) ->
    Now = erlang:system_time(second),
    case Now < EndTime of
        true ->
            %% Send dummy request
            _ = call_router_server:route_call(
                <<"warmup">>,
                <<"+15551234567">>
            ),
            timer:sleep(10),
            warmup_loop(EndTime);
        false ->
            ok
    end.

spawn_workers(Config) ->
    NumWorkers = Config#benchmark_config.workers,
    Duration = Config#benchmark_config.duration_sec,

    [spawn_link(fun() -> worker_loop(WorkerId, Duration) end)
     || WorkerId <- lists:seq(1, NumWorkers)].

worker_loop(WorkerId, DurationSec) ->
    EndTime = erlang:system_time(second) + DurationSec,
    worker_loop(WorkerId, EndTime, 0, 0, []).

worker_loop(WorkerId, EndTime, OpCount, ErrorCount, Latencies) ->
    Now = erlang:system_time(second),
    case Now < EndTime of
        true ->
            %% Perform operation
            CallId = integer_to_binary(OpCount),
            Dest = <<"+1555", CallId/binary>>,

            Start = erlang:monotonic_time(microsecond),
            Result = call_router_server:route_call(CallId, Dest),
            End = erlang:monotonic_time(microsecond),

            Latency = End - Start,

            %% Update counters
            {NewOpCount, NewErrorCount} = case Result of
                {ok, _} -> {OpCount + 1, ErrorCount};
                {error, _} -> {OpCount + 1, ErrorCount + 1}
            end,

            %% Sample latencies (keep last 1000)
            NewLatencies = case length(Latencies) < 1000 of
                true -> [Latency | Latencies];
                false -> [Latency | lists:sublist(Latencies, 999)]
            end,

            worker_loop(WorkerId, EndTime, NewOpCount, NewErrorCount, NewLatencies);
        false ->
            %% Send results back
            Result = #worker_result{
                worker_id = WorkerId,
                operations = OpCount,
                errors = ErrorCount,
                latencies = Latencies
            },
            self() ! {result, Result},

            %% Wait for collection
            receive
                stop -> ok
            end
    end.

wait_for_completion(DurationSec) ->
    timer:sleep((DurationSec + 1) * 1000).

collect_results(Workers) ->
    Results = [collect_worker_result(W) || W <- Workers],

    %% Stop workers
    [W ! stop || W <- Workers],

    Results.

collect_worker_result(Worker) ->
    receive
        {result, Result} ->
            Result
    after 5000 ->
        #worker_result{
            worker_id = 0,
            operations = 0,
            errors = 0,
            latencies = []
        }
    end.

calculate_percentiles([]) ->
    #{p50 => 0, p95 => 0, p99 => 0, p999 => 0};
calculate_percentiles(Latencies) ->
    Sorted = lists:sort(Latencies),
    Len = length(Sorted),

    #{
        p50 => percentile(Sorted, Len, 50),
        p95 => percentile(Sorted, Len, 95),
        p99 => percentile(Sorted, Len, 99),
        p999 => percentile(Sorted, Len, 99.9)
    }.

percentile(Sorted, Len, P) ->
    Index = round((P / 100) * Len),
    SafeIndex = max(1, min(Index, Len)),
    lists:nth(SafeIndex, Sorted).

report_results(Analysis, DurationSec) ->
    TotalOps = maps:get(total_operations, Analysis),
    Throughput = TotalOps / DurationSec,
    ErrorRate = maps:get(error_rate, Analysis),
    LatencyStats = maps:get(latency_stats, Analysis),

    io:format("~n=== Benchmark Results ===~n"),
    io:format("Duration: ~p seconds~n", [DurationSec]),
    io:format("Total Operations: ~p~n", [TotalOps]),
    io:format("Throughput: ~.2f ops/sec~n", [Throughput]),
    io:format("Error Rate: ~.2f%~n", [ErrorRate]),
    io:format("~n"),
    io:format("Latency Percentiles (μs):~n"),
    io:format("  P50:  ~p~n", [maps:get(p50, LatencyStats)]),
    io:format("  P95:  ~p~n", [maps:get(p95, LatencyStats)]),
    io:format("  P99:  ~p~n", [maps:get(p99, LatencyStats)]),
    io:format("  P99.9: ~p~n", [maps:get(p999, LatencyStats)]),
    io:format("~n").
