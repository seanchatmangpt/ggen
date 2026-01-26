%%%-------------------------------------------------------------------
%% @doc HTTP Endpoint Performance Benchmarks
%%
%% Measures HTTP endpoint response times (p50, p95, p99) for:
%% - Health check endpoint
%% - Pub/Sub push handler
%% - Marketplace entitlement handler
%%
%% Metrics:
%% - Response time percentiles (p50, p95, p99, max)
%% - Throughput (requests/second)
%% - Error rates
%% - Resource usage
%%
%% @end
%%%-------------------------------------------------------------------
-module(http_endpoint_bench_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    all/0
]).

-export([
    bench_health_endpoint/1,
    bench_pubsub_endpoint/1,
    bench_marketplace_endpoint/1,
    bench_concurrent_requests/1,
    bench_large_payloads/1,
    bench_error_handling/1
]).

-record(perf_result, {
    test_name :: string(),
    duration_ms :: integer(),
    requests :: integer(),
    throughput_rps :: float(),
    p50_ms :: float(),
    p95_ms :: float(),
    p99_ms :: float(),
    max_ms :: float(),
    errors :: integer(),
    error_rate :: float(),
    memory_used_mb :: float(),
    timestamp :: integer()
}).

-define(DEFAULT_DURATION, 10000).
-define(DEFAULT_CONCURRENCY, 10).
-define(HEALTH_CHECK_URL, "http://localhost:8080/health").
-define(PUBSUB_URL, "http://localhost:8080/pubsub").
-define(MARKETPLACE_URL, "http://localhost:8080/marketplace").

%%%===================================================================
%% Common Test Callbacks
%%%===================================================================

all() -> [
    bench_health_endpoint,
    bench_pubsub_endpoint,
    bench_marketplace_endpoint,
    bench_concurrent_requests,
    bench_large_payloads,
    bench_error_handling
].

init_per_suite(Config) ->
    case application:ensure_all_started(tai_autonomics) of
        {ok, _} -> Config;
        {error, Reason} ->
            ct:fail(Reason)
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

bench_health_endpoint(Config) ->
    TestName = "health_endpoint",
    Duration = ?DEFAULT_DURATION,
    Concurrency = ?DEFAULT_CONCURRENCY,

    ct:log("Starting health endpoint benchmark (~w ms, concurrency: ~w)~n", [Duration, Concurrency]),

    {ok, _Pid} = start_http_client(),

    _ = httpc:request(get, {?HEALTH_CHECK_URL, []}, [], []),
    timer:sleep(100),

    Results = run_http_benchmark(
        ?HEALTH_CHECK_URL,
        get,
        [],
        Duration,
        Concurrency,
        []
    ),

    PerfResult = analyze_results(Results, TestName),
    print_result(PerfResult),

    Config.

bench_pubsub_endpoint(Config) ->
    TestName = "pubsub_endpoint",
    Duration = ?DEFAULT_DURATION,
    Concurrency = 5,

    ct:log("Starting pub/sub endpoint benchmark (~w ms, concurrency: ~w)~n", [Duration, Concurrency]),

    {ok, _Pid} = start_http_client(),

    Payload = generate_pubsub_payload(),

    _ = httpc:request(post, {?PUBSUB_URL, [], "application/json", Payload}, [], []),
    timer:sleep(100),

    Results = run_http_benchmark(
        ?PUBSUB_URL,
        post,
        Payload,
        Duration,
        Concurrency,
        []
    ),

    PerfResult = analyze_results(Results, TestName),
    print_result(PerfResult),

    Config.

bench_marketplace_endpoint(Config) ->
    TestName = "marketplace_endpoint",
    Duration = ?DEFAULT_DURATION,
    Concurrency = 5,

    ct:log("Starting marketplace endpoint benchmark (~w ms, concurrency: ~w)~n", [Duration, Concurrency]),

    {ok, _Pid} = start_http_client(),

    Payload = generate_marketplace_payload(),

    _ = httpc:request(post, {?MARKETPLACE_URL, [], "application/json", Payload}, [], []),
    timer:sleep(100),

    Results = run_http_benchmark(
        ?MARKETPLACE_URL,
        post,
        Payload,
        Duration,
        Concurrency,
        []
    ),

    PerfResult = analyze_results(Results, TestName),
    print_result(PerfResult),

    Config.

bench_concurrent_requests(Config) ->
    _TestName = "concurrent_requests",
    Duration = 15000,
    _Concurrency = 50,

    ct:log("Starting concurrent requests benchmark (~w ms, concurrency: ~w)~n", [Duration, 50]),

    {ok, _Pid} = start_http_client(),

    Results10 = run_http_benchmark(?HEALTH_CHECK_URL, get, [], Duration, 10, []),
    Results25 = run_http_benchmark(?HEALTH_CHECK_URL, get, [], Duration, 25, []),
    Results50 = run_http_benchmark(?HEALTH_CHECK_URL, get, [], Duration, 50, []),

    PerfResult10 = analyze_results(Results10, "concurrent_10"),
    PerfResult25 = analyze_results(Results25, "concurrent_25"),
    PerfResult50 = analyze_results(Results50, "concurrent_50"),

    print_result(PerfResult10),
    print_result(PerfResult25),
    print_result(PerfResult50),

    Degradation10to50 = (PerfResult50#perf_result.p99_ms - PerfResult10#perf_result.p99_ms)
        / PerfResult10#perf_result.p99_ms,
    ct:log("Latency degradation from 10 to 50 concurrent: ~.2f%~n", [Degradation10to50 * 100]),

    Config.

bench_large_payloads(Config) ->
    TestName = "large_payloads",
    Duration = ?DEFAULT_DURATION,
    Concurrency = 5,

    ct:log("Starting large payload benchmark (~w ms, concurrency: ~w)~n", [Duration, Concurrency]),

    {ok, _Pid} = start_http_client(),

    LargePayload = generate_large_payload(1000000),

    Results = run_http_benchmark(
        ?PUBSUB_URL,
        post,
        LargePayload,
        Duration,
        Concurrency,
        []
    ),

    PerfResult = analyze_results(Results, TestName),
    print_result(PerfResult),

    Config.

bench_error_handling(Config) ->
    TestName = "error_handling",
    Duration = ?DEFAULT_DURATION,
    Concurrency = 10,

    ct:log("Starting error handling benchmark (~w ms, concurrency: ~w)~n", [Duration, Concurrency]),

    {ok, _Pid} = start_http_client(),

    Results = run_http_benchmark(
        ?PUBSUB_URL,
        post,
        "invalid_json_payload",
        Duration,
        Concurrency,
        []
    ),

    PerfResult = analyze_results(Results, TestName),
    print_result(PerfResult),

    Config.

%%%===================================================================
%% Internal Functions
%%%===================================================================

start_http_client() ->
    case inets:start(httpc, [{profile, benchmark}]) of
        {ok, _} -> {ok, benchmark};
        {error, {already_started, _}} -> {ok, benchmark};
        Error -> Error
    end.

generate_pubsub_payload() ->
    Event = #{
        <<"message">> => #{
            <<"data">> => base64:encode(<<"test_data">>),
            <<"messageId">> => <<"msg_", (integer_to_binary(erlang:system_time()))/binary>>,
            <<"publishTime">> => <<"2024-01-25T12:00:00Z">>
        },
        <<"subscription">> => <<"projects/test/subscriptions/test-sub">>
    },
    jsx:encode(Event).

generate_marketplace_payload() ->
    Event = #{
        <<"entitlementId">> => <<"ent_", (integer_to_binary(erlang:system_time()))/binary>>,
        <<"tenantId">> => <<"tenant_123">>,
        <<"action">> => <<"activate">>,
        <<"timestamp">> => erlang:system_time(millisecond)
    },
    jsx:encode(Event).

generate_large_payload(SizeBytes) ->
    Data = base64:encode(crypto:strong_rand_bytes(SizeBytes)),
    Event = #{
        <<"message">> => #{
            <<"data">> => Data,
            <<"messageId">> => <<"msg_", (integer_to_binary(erlang:system_time()))/binary>>,
            <<"publishTime">> => <<"2024-01-25T12:00:00Z">>
        }
    },
    jsx:encode(Event).

run_http_benchmark(URL, Method, Payload, Duration, Concurrency, Acc) ->
    Pid = self(),
    StartTime = erlang:system_time(millisecond),
    EndTime = StartTime + Duration,

    Workers = spawn_workers(URL, Method, Payload, Concurrency, Pid, []),

    collect_results(Workers, EndTime, Acc).

spawn_workers(_URL, _Method, _Payload, 0, _Pid, Workers) ->
    Workers;
spawn_workers(URL, Method, Payload, Count, Pid, Workers) ->
    Worker = spawn_link(fun() ->
        worker_loop(URL, Method, Payload, Pid, erlang:system_time(millisecond))
    end),
    spawn_workers(URL, Method, Payload, Count - 1, Pid, [Worker | Workers]).

worker_loop(URL, Method, Payload, Pid, EndTime) ->
    CurrentTime = erlang:system_time(millisecond),
    if
        CurrentTime > EndTime ->
            ok;
        true ->
            StartTime = erlang:system_time(millisecond),
            Result = make_http_request(URL, Method, Payload),
            ElapsedTime = erlang:system_time(millisecond) - StartTime,

            Pid ! {result, Result, ElapsedTime},

            worker_loop(URL, Method, Payload, Pid, EndTime)
    end.

make_http_request(URL, get, _Payload) ->
    case httpc:request(get, {URL, []}, [{timeout, 5000}], [{body_format, binary}]) of
        {ok, {Status, _Headers, _Body}} ->
            case Status of
                {_, Code, _} when Code >= 200, Code < 300 -> success;
                _ -> {error, Status}
            end;
        {error, Reason} -> {error, Reason}
    end;
make_http_request(URL, post, Payload) ->
    case httpc:request(post, {URL, [], "application/json", Payload}, [{timeout, 5000}], [{body_format, binary}]) of
        {ok, {Status, _Headers, _Body}} ->
            case Status of
                {_, Code, _} when Code >= 200, Code < 300 -> success;
                _ -> {error, Status}
            end;
        {error, Reason} -> {error, Reason}
    end.

collect_results([], _EndTime, Acc) ->
    Acc;
collect_results(Workers, EndTime, Acc) ->
    receive
        {result, Result, ElapsedTime} ->
            NewAcc = [{{result, Result}, ElapsedTime} | Acc],

            CurrentTime = erlang:system_time(millisecond),
            if
                CurrentTime >= EndTime ->
                    NewAcc;
                true ->
                    collect_results(Workers, EndTime, NewAcc)
            end
    after
        30000 ->
            Acc
    end.

analyze_results(Results, TestName) ->
    StartTime = erlang:system_time(millisecond),

    {Successes, Failures, Latencies} = analyze_results_helper(Results, {0, 0, []}),

    TotalRequests = length(Results),
    ErrorCount = Failures,
    DurationMs = erlang:system_time(millisecond) - StartTime,

    Throughput = case DurationMs > 0 of
        true -> (TotalRequests / (DurationMs / 1000));
        false -> 0
    end,

    ErrorRate = case TotalRequests > 0 of
        true -> ErrorCount / TotalRequests;
        false -> 0
    end,

    MemUsed = (erlang:memory(total) / 1024 / 1024),

    {P50, P95, P99, Max} = calculate_percentiles(Latencies),

    #perf_result{
        test_name = TestName,
        duration_ms = DurationMs,
        requests = TotalRequests,
        throughput_rps = Throughput,
        p50_ms = P50,
        p95_ms = P95,
        p99_ms = P99,
        max_ms = Max,
        errors = ErrorCount,
        error_rate = ErrorRate,
        memory_used_mb = MemUsed,
        timestamp = erlang:system_time(millisecond)
    }.

analyze_results_helper([], {Successes, Failures, Latencies}) ->
    {Successes, Failures, Latencies};
analyze_results_helper([{{result, success}, Latency} | Rest], {Successes, Failures, Latencies}) ->
    analyze_results_helper(Rest, {Successes + 1, Failures, [Latency | Latencies]});
analyze_results_helper([{{result, {error, _}}, _Latency} | Rest], {Successes, Failures, Latencies}) ->
    analyze_results_helper(Rest, {Successes, Failures + 1, Latencies}).

calculate_percentiles(Latencies) ->
    SortedLatencies = lists:sort(Latencies),
    Length = length(SortedLatencies),

    P50 = percentile(SortedLatencies, 0.50, Length),
    P95 = percentile(SortedLatencies, 0.95, Length),
    P99 = percentile(SortedLatencies, 0.99, Length),
    Max = case Length > 0 of
        true -> lists:max(SortedLatencies);
        false -> 0
    end,

    {P50, P95, P99, Max}.

percentile(List, Percentile, Length) when Length > 0 ->
    Index = max(1, round(Percentile * Length)),
    lists:nth(Index, lists:sort(List));
percentile(_List, _Percentile, 0) ->
    0.

print_result(Result) ->
    ct:log("~n=== Benchmark Results: ~s ===~n", [Result#perf_result.test_name]),
    ct:log("Duration: ~w ms~n", [Result#perf_result.duration_ms]),
    ct:log("Total Requests: ~w~n", [Result#perf_result.requests]),
    ct:log("Throughput: ~.2f RPS~n", [Result#perf_result.throughput_rps]),
    ct:log("P50 Latency: ~.2f ms~n", [Result#perf_result.p50_ms]),
    ct:log("P95 Latency: ~.2f ms~n", [Result#perf_result.p95_ms]),
    ct:log("P99 Latency: ~.2f ms~n", [Result#perf_result.p99_ms]),
    ct:log("Max Latency: ~.2f ms~n", [Result#perf_result.max_ms]),
    ct:log("Errors: ~w (~.2f%)~n", [Result#perf_result.errors, Result#perf_result.error_rate * 100]),
    ct:log("Memory Used: ~.2f MB~n", [Result#perf_result.memory_used_mb]),
    ct:log("============================~n~n", []).
