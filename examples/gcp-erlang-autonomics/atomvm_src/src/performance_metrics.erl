%%%-------------------------------------------------------------------
%% @doc performance_metrics - Runtime memory and CPU tracking
%%
%% Lightweight metrics collection suitable for edge deployment.
%% Minimal overhead: polling disabled by default, on-demand only.
%%
%% Key metrics:
%% - Memory: RSS, heap, external (updated on demand)
%% - CPU: wall-clock time per governor, GC pauses
%% - Throughput: requests per second, errors per second
%% - Latency: event processing p99, state update time
%%
%% Usage:
%%   Metrics = performance_metrics:collect(),
%%   maps:get(memory_total, Metrics)
%%
%% @end
%%%-------------------------------------------------------------------
-module(performance_metrics).

%% Public API
-export([
    start_collection/0,
    stop_collection/0,
    collect/0,
    get_memory/0,
    get_cpu/0,
    get_throughput/0,
    get_latency/0,
    reset_counters/0,
    benchmark_governors/1
]).

%% Internal state management
-export([init_metrics/0]).

%%===================================================================
%% Type Definitions
%%===================================================================

-type memory_metrics() :: #{
    total_bytes => non_neg_integer(),
    heap_bytes => non_neg_integer(),
    external_bytes => non_neg_integer(),
    processes => non_neg_integer(),
    atoms => non_neg_integer(),
    gc_minor => non_neg_integer(),
    gc_major => non_neg_integer()
}.

-type cpu_metrics() :: #{
    wall_time_ms => non_neg_integer(),
    cpu_time_ms => non_neg_integer(),
    gc_pause_max_ms => float(),
    gc_pause_avg_ms => float(),
    context_switches => non_neg_integer()
}.

-type throughput_metrics() :: #{
    requests_total => non_neg_integer(),
    requests_per_second => float(),
    errors_total => non_neg_integer(),
    error_rate => float()
}.

-type latency_metrics() :: #{
    min_ms => float(),
    max_ms => float(),
    avg_ms => float(),
    p50_ms => float(),
    p99_ms => float(),
    p999_ms => float()
}.

-type snapshot() :: #{
    timestamp => pos_integer(),
    memory => memory_metrics(),
    cpu => cpu_metrics(),
    throughput => throughput_metrics(),
    latency => latency_metrics()
}.

%% Process dictionary keys for counters
-define(REQUESTS_KEY, '$requests_counter').
-define(ERRORS_KEY, '$errors_counter').
-define(LATENCIES_KEY, '$latencies_list').
-define(START_TIME_KEY, '$start_time').

%%===================================================================
%% Public API
%%===================================================================

%% @doc Start metric collection (initializes counters)
-spec start_collection() -> ok.
start_collection() ->
    erlang:put(?REQUESTS_KEY, 0),
    erlang:put(?ERRORS_KEY, 0),
    erlang:put(?LATENCIES_KEY, []),
    erlang:put(?START_TIME_KEY, erlang:system_time(millisecond)),
    ok.

%% @doc Stop metric collection (final snapshot)
-spec stop_collection() -> snapshot().
stop_collection() ->
    Snapshot = collect(),
    erlang:erase(?REQUESTS_KEY),
    erlang:erase(?ERRORS_KEY),
    erlang:erase(?LATENCIES_KEY),
    erlang:erase(?START_TIME_KEY),
    Snapshot.

%% @doc Collect current metrics snapshot
-spec collect() -> snapshot().
collect() ->
    #{
        timestamp => erlang:system_time(millisecond),
        memory => get_memory(),
        cpu => get_cpu(),
        throughput => get_throughput(),
        latency => get_latency()
    }.

%% @doc Get memory metrics
-spec get_memory() -> memory_metrics().
get_memory() ->
    {Total, Heap, External} = erlang:memory(total, heap_size, external),
    {Processes, Atoms, GCMinor, GCMajor} = erlang:statistics(
        processes, atoms, gc_minor_count, gc_major_count
    ),

    #{
        total_bytes => Total,
        heap_bytes => Heap,
        external_bytes => External,
        processes => Processes,
        atoms => Atoms,
        gc_minor => GCMinor,
        gc_major => GCMajor
    }.

%% @doc Get CPU metrics
-spec get_cpu() -> cpu_metrics().
get_cpu() ->
    % Wall clock time
    {Wallclock, _} = erlang:statistics(wall_clock),

    % CPU time  (approximate via reductions for edge devices)
    {Reductions, _} = erlang:statistics(reductions),

    % GC metrics (get from memory info)
    {_, GCStats} = erlang:statistics(garbage_collection),

    #{
        wall_time_ms => Wallclock div 1000,
        cpu_time_ms => Reductions div 1000,  % Approximate
        gc_pause_max_ms => extract_gc_max(GCStats),
        gc_pause_avg_ms => extract_gc_avg(GCStats),
        context_switches => erlang:statistics(context_switches) div 1000
    }.

%% @doc Get throughput metrics
-spec get_throughput() -> throughput_metrics().
get_throughput() ->
    Requests = erlang:get(?REQUESTS_KEY) orelse 0,
    Errors = erlang:get(?ERRORS_KEY) orelse 0,

    StartTime = erlang:get(?START_TIME_KEY) orelse erlang:system_time(millisecond),
    ElapsedMs = erlang:system_time(millisecond) - StartTime,
    ElapsedSeconds = max(1, ElapsedMs div 1000),

    RequestsPerSecond = Requests / ElapsedSeconds,
    ErrorRate = case Requests > 0 of
        true -> (Errors / Requests) * 100;
        false -> 0.0
    end,

    #{
        requests_total => Requests,
        requests_per_second => RequestsPerSecond,
        errors_total => Errors,
        error_rate => ErrorRate
    }.

%% @doc Get latency metrics (from stored latencies)
-spec get_latency() -> latency_metrics().
get_latency() ->
    Latencies = erlang:get(?LATENCIES_KEY) orelse [],

    case length(Latencies) of
        0 ->
            #{
                min_ms => 0.0,
                max_ms => 0.0,
                avg_ms => 0.0,
                p50_ms => 0.0,
                p99_ms => 0.0,
                p999_ms => 0.0
            };

        N ->
            Sorted = lists:sort(Latencies),
            Min = lists:min(Sorted),
            Max = lists:max(Sorted),
            Avg = lists:sum(Sorted) / N,
            P50Index = (N * 50) div 100,
            P99Index = (N * 99) div 100,
            P999Index = (N * 999) div 1000,

            #{
                min_ms => Min,
                max_ms => Max,
                avg_ms => Avg,
                p50_ms => lists:nth(P50Index, Sorted),
                p99_ms => lists:nth(P99Index, Sorted),
                p999_ms => lists:nth(P999Index, Sorted)
            }
    end.

%% @doc Reset all counters
-spec reset_counters() -> ok.
reset_counters() ->
    erlang:put(?REQUESTS_KEY, 0),
    erlang:put(?ERRORS_KEY, 0),
    erlang:put(?LATENCIES_KEY, []),
    erlang:put(?START_TIME_KEY, erlang:system_time(millisecond)),
    ok.

%%===================================================================
%% Benchmark Functions
%%===================================================================

%% @doc Run benchmark comparing memory usage across governors
%% Returns: {NumGovernors, TotalMemory, MemoryPerGovernor}
-spec benchmark_governors(non_neg_integer()) -> {
    non_neg_integer(),
    non_neg_integer(),
    float()
}.
benchmark_governors(NumGovernors) ->
    % Initial memory snapshot
    #{total_bytes := InitialMem} = get_memory(),

    % Start governors
    Pids = [
        begin
            Type = case I rem 5 of
                0 -> entitlement;
                1 -> billing;
                2 -> quota;
                3 -> compliance;
                _ -> subscription
            end,
            {ok, Pid} = light_governors:start_link(Type),
            Pid
        end || I <- lists:seq(1, NumGovernors)
    ],

    % Memory after startup
    timer:sleep(100),  % Allow governors to initialize
    #{total_bytes := AfterStartupMem} = get_memory(),

    % Run some checks
    [
        _ = light_governors:check_entitlement(Pid, <<"tenant_1">>, #{})
        || Pid <- Pids
    ],

    % Final memory
    #{total_bytes := FinalMem} = get_memory(),

    % Clean up
    [light_governors:halt(Pid) || Pid <- Pids],

    % Calculate metrics
    UsedMemory = FinalMem - InitialMem,
    MemoryPerGovernor = UsedMemory / NumGovernors,

    {NumGovernors, UsedMemory, MemoryPerGovernor}.

%%===================================================================
%% Counter Management (called by governors)
%%===================================================================

%% @doc Increment request counter (called by light_governors)
-spec increment_request_counter() -> ok.
increment_request_counter() ->
    Count = erlang:get(?REQUESTS_KEY) orelse 0,
    erlang:put(?REQUESTS_KEY, Count + 1),
    ok.

%% @doc Increment error counter
-spec increment_error_counter() -> ok.
increment_error_counter() ->
    Count = erlang:get(?ERRORS_KEY) orelse 0,
    erlang:put(?ERRORS_KEY, Count + 1),
    ok.

%% @doc Record latency sample (milliseconds)
-spec record_latency(float()) -> ok.
record_latency(LatencyMs) ->
    Latencies = erlang:get(?LATENCIES_KEY) orelse [],
    % Keep only last 10000 samples (prevent unbounded growth)
    NewLatencies = case length(Latencies) >= 10000 of
        true -> tl(Latencies) ++ [LatencyMs];
        false -> Latencies ++ [LatencyMs]
    end,
    erlang:put(?LATENCIES_KEY, NewLatencies),
    ok.

%%===================================================================
%% Initialization
%%===================================================================

%% @private Initialize metrics (called once at startup)
-spec init_metrics() -> ok.
init_metrics() ->
    start_collection().

%%===================================================================
%% Helper Functions
%%===================================================================

%% @private Extract max GC pause from statistics
-spec extract_gc_max(list()) -> float().
extract_gc_max(GCStats) ->
    case lists:keyfind(max, 1, GCStats) of
        {max, Max} -> Max / 1000;  % Convert microseconds to ms
        false -> 0.0
    end.

%% @private Extract average GC pause from statistics
-spec extract_gc_avg(list()) -> float().
extract_gc_avg(GCStats) ->
    case {lists:keyfind(garbage_collection, 1, GCStats),
          lists:keyfind(number_of_gcs, 1, GCStats)} of
        {{garbage_collection, Total}, {number_of_gcs, Count}} when Count > 0 ->
            (Total / Count) / 1000;  % Convert to ms
        _ ->
            0.0
    end.

%%===================================================================
%% Reporting Utilities
%%===================================================================

%% @doc Format metrics as human-readable string
-spec format_metrics(snapshot()) -> string().
format_metrics(#{
    timestamp := Timestamp,
    memory := Memory,
    cpu := CPU,
    throughput := Throughput,
    latency := Latency
}) ->
    io_lib:format(
        "=== Performance Metrics (UTC ~w) ===~n"
        "Memory:~n"
        "  Total: ~wKB~n"
        "  Heap: ~wKB~n"
        "  External: ~wKB~n"
        "CPU:~n"
        "  Wall-clock: ~wms~n"
        "  GC pauses avg: ~.2fms~n"
        "Throughput:~n"
        "  Requests/sec: ~.2f~n"
        "  Error rate: ~.2f%~n"
        "Latency:~n"
        "  p99: ~.2fms~n"
        "  p999: ~.2fms~n",
        [
            Timestamp,
            maps:get(total_bytes, Memory) div 1024,
            maps:get(heap_bytes, Memory) div 1024,
            maps:get(external_bytes, Memory) div 1024,
            maps:get(wall_time_ms, CPU),
            maps:get(gc_pause_avg_ms, CPU),
            maps:get(requests_per_second, Throughput),
            maps:get(error_rate, Throughput),
            maps:get(p99_ms, Latency),
            maps:get(p999_ms, Latency)
        ]
    ).

