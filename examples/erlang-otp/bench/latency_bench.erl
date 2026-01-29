%%%-------------------------------------------------------------------
%%% @doc
%%% Latency-focused benchmark
%%%
%%% Measures detailed latency distributions and identifies outliers.
%%% Useful for SLA validation and performance tuning.
%%% @end
%%%-------------------------------------------------------------------
-module(latency_bench).

-export([
    run/1,
    measure_latency_distribution/1,
    find_outliers/2
]).

-record(latency_result, {
    total_samples :: pos_integer(),
    min :: pos_integer(),
    max :: pos_integer(),
    mean :: float(),
    median :: pos_integer(),
    std_dev :: float(),
    percentiles :: map(),
    outliers :: [pos_integer()]
}).

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc Run latency benchmark
run(NumSamples) when is_integer(NumSamples), NumSamples > 0 ->
    io:format("~n=== Latency Benchmark ===~n"),
    io:format("Samples: ~p~n~n", [NumSamples]),

    %% Collect latency samples
    io:format("Collecting samples...~n"),
    Samples = collect_samples(NumSamples),

    %% Analyze
    io:format("Analyzing distribution...~n~n"),
    Result = analyze_latencies(Samples),

    %% Report
    report_latency_results(Result),

    {ok, Result}.

%% @doc Measure detailed latency distribution
measure_latency_distribution(NumSamples) ->
    Samples = collect_samples(NumSamples),
    analyze_latencies(Samples).

%% @doc Find outliers using IQR method
find_outliers(Samples, Multiplier) ->
    Sorted = lists:sort(Samples),
    Len = length(Sorted),

    Q1 = percentile(Sorted, Len, 25),
    Q3 = percentile(Sorted, Len, 75),
    IQR = Q3 - Q1,

    LowerBound = Q1 - (Multiplier * IQR),
    UpperBound = Q3 + (Multiplier * IQR),

    [S || S <- Samples, S < LowerBound orelse S > UpperBound].

%%%===================================================================
%%% Internal Functions
%%%===================================================================

collect_samples(NumSamples) ->
    collect_samples(NumSamples, []).

collect_samples(0, Acc) ->
    Acc;
collect_samples(N, Acc) ->
    CallId = integer_to_binary(N),
    Dest = <<"+1555", CallId/binary>>,

    %% Measure single operation
    Start = erlang:monotonic_time(microsecond),
    _ = call_router_server:route_call(CallId, Dest),
    End = erlang:monotonic_time(microsecond),

    Latency = End - Start,

    %% Progress indicator
    case N rem 1000 of
        0 -> io:format("  ~p samples collected~n", [NumSamples - N]);
        _ -> ok
    end,

    collect_samples(N - 1, [Latency | Acc]).

analyze_latencies(Samples) ->
    Sorted = lists:sort(Samples),
    Len = length(Sorted),

    Min = lists:min(Sorted),
    Max = lists:max(Sorted),
    Mean = lists:sum(Sorted) / Len,
    Median = median(Sorted, Len),
    StdDev = standard_deviation(Sorted, Mean),

    Percentiles = #{
        p50 => percentile(Sorted, Len, 50),
        p75 => percentile(Sorted, Len, 75),
        p90 => percentile(Sorted, Len, 90),
        p95 => percentile(Sorted, Len, 95),
        p99 => percentile(Sorted, Len, 99),
        p999 => percentile(Sorted, Len, 99.9),
        p9999 => percentile(Sorted, Len, 99.99)
    },

    Outliers = find_outliers(Samples, 1.5),

    #latency_result{
        total_samples = Len,
        min = Min,
        max = Max,
        mean = Mean,
        median = Median,
        std_dev = StdDev,
        percentiles = Percentiles,
        outliers = Outliers
    }.

median(Sorted, Len) ->
    Mid = Len div 2,
    case Len rem 2 of
        0 ->
            %% Even length: average of middle two
            (lists:nth(Mid, Sorted) + lists:nth(Mid + 1, Sorted)) div 2;
        _ ->
            %% Odd length: middle element
            lists:nth(Mid + 1, Sorted)
    end.

standard_deviation(Samples, Mean) ->
    Variance = lists:sum([math:pow(S - Mean, 2) || S <- Samples]) / length(Samples),
    math:sqrt(Variance).

percentile(Sorted, Len, P) ->
    Index = round((P / 100) * Len),
    SafeIndex = max(1, min(Index, Len)),
    lists:nth(SafeIndex, Sorted).

report_latency_results(Result) ->
    io:format("=== Latency Analysis Results ===~n~n"),

    io:format("Summary Statistics (μs):~n"),
    io:format("  Samples:     ~p~n", [Result#latency_result.total_samples]),
    io:format("  Min:         ~p~n", [Result#latency_result.min]),
    io:format("  Max:         ~p~n", [Result#latency_result.max]),
    io:format("  Mean:        ~.2f~n", [Result#latency_result.mean]),
    io:format("  Median:      ~p~n", [Result#latency_result.median]),
    io:format("  Std Dev:     ~.2f~n", [Result#latency_result.std_dev]),
    io:format("~n"),

    Percentiles = Result#latency_result.percentiles,
    io:format("Latency Percentiles (μs):~n"),
    io:format("  P50:      ~p~n", [maps:get(p50, Percentiles)]),
    io:format("  P75:      ~p~n", [maps:get(p75, Percentiles)]),
    io:format("  P90:      ~p~n", [maps:get(p90, Percentiles)]),
    io:format("  P95:      ~p~n", [maps:get(p95, Percentiles)]),
    io:format("  P99:      ~p~n", [maps:get(p99, Percentiles)]),
    io:format("  P99.9:    ~p~n", [maps:get(p999, Percentiles)]),
    io:format("  P99.99:   ~p~n", [maps:get(p9999, Percentiles)]),
    io:format("~n"),

    NumOutliers = length(Result#latency_result.outliers),
    OutlierPct = (NumOutliers / Result#latency_result.total_samples) * 100,
    io:format("Outliers (IQR method):~n"),
    io:format("  Count:       ~p~n", [NumOutliers]),
    io:format("  Percentage:  ~.2f%~n", [OutlierPct]),

    %% Show worst outliers
    case NumOutliers > 0 of
        true ->
            WorstOutliers = lists:sublist(lists:reverse(lists:sort(Result#latency_result.outliers)), 10),
            io:format("  Top 10 worst: ~p~n", [WorstOutliers]);
        false ->
            ok
    end,

    io:format("~n"),

    %% SLA validation
    P99 = maps:get(p99, Percentiles),
    P95 = maps:get(p95, Percentiles),

    io:format("SLA Validation:~n"),
    validate_sla("P99 < 1000μs", P99, 1000),
    validate_sla("P95 < 500μs", P95, 500),
    io:format("~n").

validate_sla(Name, Actual, Threshold) ->
    Status = case Actual =< Threshold of
        true -> "✓ PASS";
        false -> "✗ FAIL"
    end,
    io:format("  ~s ~s (actual: ~p, threshold: ~p)~n",
              [Status, Name, Actual, Threshold]).
