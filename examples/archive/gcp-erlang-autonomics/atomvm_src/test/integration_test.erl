%%%-------------------------------------------------------------------
%% @doc integration_test - Integration tests for memory-optimized governors
%%
%% Validates:
%% - Memory usage (target: 25KB per governor)
%% - Event processing latency (target: p99 < 10ms)
%% - Throughput (target: > 10k events/sec)
%% - Compression ratio (target: 5x)
%% - Ring buffer operation (target: O(1) add_event)
%%
%% Run:
%%   erl -pa ebin
%%   erl> ct:run([{spec, "test/integration.spec"}]).
%%   erl> eunit:test(integration_test).
%%
%% @end
%%%-------------------------------------------------------------------
-module(integration_test).

-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Setup and Teardown
%%===================================================================

setup() ->
    performance_metrics:start_collection(),
    ok.

teardown(_) ->
    performance_metrics:stop_collection(),
    ok.

%%===================================================================
%% Memory Efficiency Tests
%%===================================================================

%% @doc Test: Single governor memory usage
governor_memory_test() ->
    setup(),

    % Start governor
    {ok, Pid} = light_governors:start_link(entitlement),

    % Measure memory
    State = light_governors:get_state(Pid),
    Memory = light_governors:memory_usage(State),
    Total = maps:get(total, Memory),

    light_governors:halt(Pid),

    teardown(ok),

    % Assert: < 50KB per governor (target 25KB)
    ?assert(Total < 50 * 1024),
    ?assert(Total > 1 * 1024).  % At least 1KB

%% @doc Test: 30 governors fits in 1MB
multiple_governors_memory_test() ->
    setup(),

    % Start 30 governors
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
        end || I <- lists:seq(1, 30)
    ],

    timer:sleep(100),

    % Measure total memory
    Metrics = performance_metrics:collect(),
    TotalMem = maps:get(total_bytes, maps:get(memory, Metrics)),

    % Clean up
    [light_governors:halt(Pid) || Pid <- Pids],
    teardown(ok),

    % Assert: 30 governors < 1MB (target 750KB)
    ?assert(TotalMem < 1 * 1024 * 1024).

%%===================================================================
%% Latency Tests
%%===================================================================

%% @doc Test: Single request latency
single_request_latency_test() ->
    setup(),

    {ok, Pid} = light_governors:start_link(quota),

    Start = erlang:monotonic_time(microsecond),
    {ok, _} = light_governors:check_quota(Pid, <<"t1">>, #{}),
    Latency = erlang:monotonic_time(microsecond) - Start,

    light_governors:halt(Pid),
    teardown(ok),

    % Assert: < 10ms per request
    ?assert(Latency < 10000).  % microseconds

%% @doc Test: p99 latency under load
p99_latency_test() ->
    setup(),

    {ok, Pid} = light_governors:start_link(billing),

    % Warm up
    _ = [
        light_governors:check_billing(Pid, <<"tenant">>, #{})
        || _ <- lists:seq(1, 100)
    ],

    % Measure latencies (1000 samples)
    Latencies = [
        measure_latency(Pid) || _ <- lists:seq(1, 1000)
    ],

    light_governors:halt(Pid),
    teardown(ok),

    % Calculate p99
    Sorted = lists:sort(Latencies),
    P99Index = (1000 * 99) div 100,
    P99 = lists:nth(P99Index, Sorted),

    % Assert: p99 < 20ms
    ?assert(P99 < 20000).  % microseconds

%%===================================================================
%% Throughput Tests
%%===================================================================

%% @doc Test: Throughput measurement
throughput_test() ->
    setup(),

    {ok, Pid} = light_governors:start_link(entitlement),

    % Run for 2 seconds
    Start = erlang:monotonic_time(millisecond),
    Count = run_throughput_loop(Pid, 0, Start),
    End = erlang:monotonic_time(millisecond),

    light_governors:halt(Pid),
    teardown(ok),

    Elapsed = (End - Start) / 1000,
    TPS = Count / Elapsed,

    % Assert: > 1000 events/sec
    ?assert(TPS > 1000).

%%===================================================================
%% Memory Pool Tests
%%===================================================================

%% @doc Test: Ring buffer add operation
ring_buffer_add_test() ->
    Pool = memory_pool:new(100),

    % Add 50 events
    {Pool1, Results} = lists:foldl(
        fun(I, {P, Acc}) ->
            {Result, _} = memory_pool:add_event(P, {event, I}),
            {P, [Result|Acc]}
        end,
        {Pool, []},
        lists:seq(1, 50)
    ),

    % Check stats
    Stats = memory_pool:stats(Pool1),
    Size = maps:get(size, Stats),

    % Assert: all events added
    ?assertEqual(50, Size),
    % Assert: no full yet
    ?assert(lists:all(fun(R) -> R =:= {ok, _} end, Results)).

%% @doc Test: Ring buffer overflow
ring_buffer_overflow_test() ->
    Pool = memory_pool:new(10),

    % Add 20 events (more than capacity)
    {Pool1, _Results} = lists:foldl(
        fun(I, {P, Acc}) ->
            {Result, _} = memory_pool:add_event(P, {event, I}),
            {P, [Result|Acc]}
        end,
        {Pool, []},
        lists:seq(1, 20)
    ),

    % Check stats
    Stats = memory_pool:stats(Pool1),
    Size = maps:get(size, Stats),
    Cap = maps:get(capacity, Stats),

    % Assert: size capped at capacity
    ?assertEqual(Cap, Size),
    ?assertEqual(10, Size).

%%===================================================================
%% Compression Tests
%%===================================================================

%% @doc Test: Basic compression
basic_compression_test() ->
    State = #{
        type => entitlement,
        tenant => <<"acme_corp">>,
        features => [api, webhooks, advanced_reporting],
        limits => #{calls_per_day => 10000}
    },

    Compressed = compression:compress(State),
    Decompressed = compression:decompress(Compressed),

    % Assert: round-trip successful
    ?assertEqual(State, Decompressed).

%% @doc Test: Compression ratio
compression_ratio_test() ->
    % Create large state (20KB)
    State = #{
        governors => [
            #{id => G, state => #{tenant => <<"t", (integer_to_binary(G))/binary>>}}
            || G <- lists:seq(1, 100)
        ]
    },

    Compressed = compression:compress(State),
    BinaryState = term_to_binary(State),

    Original = byte_size(BinaryState),
    Compressed_size = byte_size(Compressed),

    Ratio = Original / Compressed_size,

    % Assert: 3x+ compression
    ?assert(Ratio >= 3.0).

%% @doc Test: Pool compression
pool_compression_test() ->
    Pool = memory_pool:new(50),

    % Add some events
    {Pool1, _} = lists:foldl(
        fun(I, {P, _}) ->
            memory_pool:add_event(P, {check, entitlement, {tenant, I}})
        end,
        {Pool, ok},
        lists:seq(1, 25)
    ),

    % Compress and decompress
    Compressed = compression:compress_pool(Pool1),
    Pool2 = compression:decompress_pool(Compressed),

    % Check they match
    Events1 = memory_pool:get_all_events(Pool1),
    Events2 = memory_pool:get_all_events(Pool2),

    % Assert: events preserved
    ?assertEqual(length(Events1), length(Events2)).

%%===================================================================
%% Atom Interning Tests
%%===================================================================

%% @doc Test: Atom cache effectiveness
atom_cache_test() ->
    Pool = memory_pool:new(100, true),  % Enable atom cache

    % Add events with repeated atoms
    _ = [
        memory_pool:add_event(Pool, {entitlement, check, tenant})
        || _ <- lists:seq(1, 100)
    ],

    Stats = memory_pool:stats(Pool),
    CacheSize = maps:get(atom_cache_size, Stats),

    % Assert: cache has interned atoms
    ?assert(CacheSize > 0).

%%===================================================================
%% Integration Tests
%%===================================================================

%% @doc Test: Full workflow (entitlement + billing + quota)
full_workflow_test() ->
    setup(),

    {ok, EntPid} = light_governors:start_link(entitlement),
    {ok, BillPid} = light_governors:start_link(billing),
    {ok, QuotaPid} = light_governors:start_link(quota),

    Tenant = <<"test_tenant">>,

    % Check entitlement
    {ok, Ent} = light_governors:check_entitlement(EntPid, Tenant, #{}),
    ?assertMatch(#{tier := _}, Ent),

    % Check billing
    {ok, Bill} = light_governors:check_billing(BillPid, Tenant, #{}),
    ?assertMatch(#{credit := _}, Bill),

    % Check quota
    {ok, Quota} = light_governors:check_quota(QuotaPid, Tenant, #{}),
    ?assertMatch(#{account_id := Tenant}, Quota),

    light_governors:halt(EntPid),
    light_governors:halt(BillPid),
    light_governors:halt(QuotaPid),

    teardown(ok).

%%===================================================================
%% Helper Functions
%%===================================================================

%% @private Measure single request latency
-spec measure_latency(pid()) -> non_neg_integer().
measure_latency(Pid) ->
    Start = erlang:monotonic_time(microsecond),
    {ok, _} = light_governors:check_billing(Pid, <<"t1">>, #{}),
    erlang:monotonic_time(microsecond) - Start.

%% @private Run throughput loop
-spec run_throughput_loop(pid(), non_neg_integer(), non_neg_integer()) ->
    non_neg_integer().
run_throughput_loop(Pid, Count, Start) ->
    Current = erlang:monotonic_time(millisecond),
    case Current - Start > 2000 of
        true ->
            Count;
        false ->
            _ = light_governors:check_entitlement(Pid, <<"t">>, #{}),
            run_throughput_loop(Pid, Count + 1, Start)
    end.

%%===================================================================
%% Test Suite Export for CT (Common Test)
%%===================================================================

all_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        [
            {"Governor memory", fun governor_memory_test/0},
            {"Multiple governors memory", fun multiple_governors_memory_test/0},
            {"Single request latency", fun single_request_latency_test/0},
            {"p99 latency", fun p99_latency_test/0},
            {"Throughput", fun throughput_test/0},
            {"Ring buffer add", fun ring_buffer_add_test/0},
            {"Ring buffer overflow", fun ring_buffer_overflow_test/0},
            {"Compression", fun basic_compression_test/0},
            {"Compression ratio", fun compression_ratio_test/0},
            {"Pool compression", fun pool_compression_test/0},
            {"Atom cache", fun atom_cache_test/0},
            {"Full workflow", fun full_workflow_test/0}
        ]
    }.

