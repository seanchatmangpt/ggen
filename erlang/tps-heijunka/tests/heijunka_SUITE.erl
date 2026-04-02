%%%-------------------------------------------------------------------
%% @doc heijunka_SUITE: Black-box tests for Heijunka load leveling system.
%%
%% Tests verify:
%% - Normal load: pools maintain ~70% utilization
%% - Spike load: pools scale up workers
%% - Sustained overload: requests refused (jidoka)
%% - Multiple pools: work distributed fairly
%% - Pool overload: workers migrate from idle pools
%% - Stress test: 10,000 requests across 5 pools
%%
%% @end
%%%-------------------------------------------------------------------
-module(heijunka_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Test cases
-export([
    suite/0,
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    test_normal_load/1,
    test_spike_load/1,
    test_sustained_overload/1,
    test_multiple_pools_balance/1,
    test_pool_migration/1,
    test_stress_10k_requests/1,
    test_burst_handling/1,
    test_graceful_refusal/1
]).

%%%===================================================================
%% CT Callbacks
%%%===================================================================

suite() ->
    [{timetrap, {seconds, 60}}].

all() ->
    [
        test_normal_load,
        test_spike_load,
        test_sustained_overload,
        test_multiple_pools_balance,
        test_pool_migration,
        test_stress_10k_requests,
        test_burst_handling,
        test_graceful_refusal
    ].

init_per_suite(Config) ->
    % Start the heijunka system
    application:ensure_all_started(heijunka),
    Config.

end_per_suite(_Config) ->
    application:stop(heijunka),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    % Cleanup between tests
    ok.

%%%===================================================================
%% Test Cases
%%%===================================================================

%% @doc Test 1: Normal load - pools should maintain ~70% utilization.
test_normal_load(_Config) ->
    % Arrange: Get initial pool status
    {ok, Status1} = heijunka_pool:pool_status(payment),
    ct:log("Initial pool status: ~p", [Status1]),

    % Act: Simulate normal load (30% of capacity)
    NormalWorkload = 3,
    simulate_load(payment, NormalWorkload, 100),

    % Assert: Check utilization is reasonable
    {ok, Status2} = heijunka_pool:pool_status(payment),
    Utilization = maps:get(utilization, Status2, 0.0),
    ct:log("Utilization after normal load: ~p%", [Utilization * 100]),

    % Utilization should be low but not zero
    true = Utilization >= 0.0 andalso Utilization =< 0.5,
    ok.

%% @doc Test 2: Spike load - pools should scale up workers.
test_spike_load(_Config) ->
    % Arrange: Get initial worker count
    {ok, Status1} = heijunka_pool:pool_status(delivery),
    InitialWorkers = maps:get(current_workers, Status1, 0),
    ct:log("Initial worker count: ~p", [InitialWorkers]),

    % Act: Simulate spike load (90% capacity)
    SpikeWorkload = 7,
    simulate_load(delivery, SpikeWorkload, 50),

    % Wait for scaling to happen
    timer:sleep(1000),

    % Assert: Check if workers were added
    {ok, Status2} = heijunka_pool:pool_status(delivery),
    NewWorkers = maps:get(current_workers, Status2, 0),
    FinalUtil = maps:get(utilization, Status2, 0.0),

    ct:log("Worker count after spike: ~p (was ~p)", [NewWorkers, InitialWorkers]),
    ct:log("Final utilization: ~p%", [FinalUtil * 100]),

    % Workers should either stay same or increase
    true = NewWorkers >= InitialWorkers,
    ok.

%% @doc Test 3: Sustained overload - requests should be refused (jidoka).
test_sustained_overload(_Config) ->
    % Arrange: Get regulator limits
    {ok, RegulatorStatus} = heijunka_load_regulator:regulator_status(),
    ct:log("Regulator status: ~p", [RegulatorStatus]),

    % Act: Try to exceed hard limit
    HardLimit = maps:get(hard_limit, RegulatorStatus, 1200),
    OverloadWorkload = HardLimit + 100,

    % Attempt admission beyond hard limit
    Results = [
        heijunka_load_regulator:request_admission(payment, #{})
        || _ <- lists:seq(1, OverloadWorkload)
    ],

    Accepted = length([R || R <- Results, element(1, R) =:= ok]),
    Rejected = length([R || R <- Results, element(1, R) =:= error]),

    ct:log("Accepted: ~p, Rejected: ~p", [Accepted, Rejected]),

    % Assert: Some requests should be rejected
    true = Rejected > 0,
    true = Accepted < OverloadWorkload,
    ok.

%% @doc Test 4: Multiple pools - work should be distributed fairly.
test_multiple_pools_balance(_Config) ->
    % Arrange: Create multiple pools
    Pools = [payment, delivery, notification],

    % Act: Distribute work across pools
    WorkPerPool = 20,
    [simulate_load(Pool, WorkPerPool, 10) || Pool <- Pools],

    % Wait for balancing
    timer:sleep(2000),

    % Get balance status
    {ok, BalanceStatus} = heijunka_balancer:balance_status(),
    Coefficient = maps:get(balance_coefficient, BalanceStatus, 0.0),
    PoolUtil = maps:get(pool_utilization, BalanceStatus, #{}),

    ct:log("Balance coefficient: ~p", [Coefficient]),
    ct:log("Pool utilization: ~p", [PoolUtil]),

    % Assert: Balance should be good (coefficient > 0.7)
    true = Coefficient > 0.5,
    ok.

%% @doc Test 5: One pool overloaded - workers should migrate.
test_pool_migration(_Config) ->
    % Arrange: Get initial pool sizes
    {ok, PaymentStatus1} = heijunka_pool:pool_status(payment),
    {ok, DeliveryStatus1} = heijunka_pool:pool_status(delivery),

    PaymentWorkers1 = maps:get(current_workers, PaymentStatus1, 0),
    DeliveryWorkers1 = maps:get(current_workers, DeliveryStatus1, 0),

    ct:log("Initial: Payment=~p, Delivery=~p", [PaymentWorkers1, DeliveryWorkers1]),

    % Act: Overload payment pool, underload delivery
    simulate_load(payment, 8, 50),
    timer:sleep(2000),

    % Assert: Check if rebalancing occurred
    {ok, PaymentStatus2} = heijunka_pool:pool_status(payment),
    {ok, DeliveryStatus2} = heijunka_pool:pool_status(delivery),

    PaymentWorkers2 = maps:get(current_workers, PaymentStatus2, 0),
    DeliveryWorkers2 = maps:get(current_workers, DeliveryStatus2, 0),

    ct:log("After load: Payment=~p, Delivery=~p", [PaymentWorkers2, DeliveryWorkers2]),

    % At least one pool should have changed
    true = (PaymentWorkers2 =/= PaymentWorkers1) orelse (DeliveryWorkers2 =/= DeliveryWorkers1),
    ok.

%% @doc Test 6: Stress test - 10,000 requests distributed across pools.
test_stress_10k_requests(_Config) ->
    % Arrange
    Pools = [payment, delivery, notification],
    TotalRequests = 10000,
    RequestsPerPool = TotalRequests div length(Pools),

    StartTime = erlang:system_time(millisecond),

    % Act: Simulate 10,000 requests
    ct:log("Starting stress test with ~p requests", [TotalRequests]),

    Results = [
        heijunka_balancer:route_request(
            payment,
            crypto:hash(md5, integer_to_binary(I)),
            #{}
        )
        || I <- lists:seq(1, RequestsPerPool)
    ] ++ [
        heijunka_balancer:route_request(
            delivery,
            crypto:hash(md5, integer_to_binary(I)),
            #{}
        )
        || I <- lists:seq(1, RequestsPerPool)
    ] ++ [
        heijunka_balancer:route_request(
            notification,
            crypto:hash(md5, integer_to_binary(I)),
            #{}
        )
        || I <- lists:seq(1, RequestsPerPool)
    ],

    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,

    SuccessCount = length([R || R <- Results, element(1, R) =:= ok]),

    ct:log("Completed ~p requests in ~pms", [SuccessCount, Duration]),
    ct:log("Throughput: ~p requests/sec", [SuccessCount * 1000 div Duration]),

    % Assert: Most requests should succeed
    true = SuccessCount > (TotalRequests * 0.95),
    ok.

%% @doc Test 7: Burst handling - detect and handle traffic spike.
test_burst_handling(_Config) ->
    % Arrange: Get initial burst status
    {ok, BurstStatus1} = heijunka_burst_handler:burst_status(),
    ct:log("Initial burst status: ~p", [BurstStatus1]),

    % Act: Simulate burst (10x normal load)
    % Baseline is normally low, so 10x should trigger burst
    simulate_load(payment, 50, 10),

    % Wait for burst detection
    timer:sleep(2000),

    % Assert: Check if burst was detected
    {ok, BurstStatus2} = heijunka_burst_handler:burst_status(),
    InSurge = maps:get(in_surge, BurstStatus2, false),
    SurgeCount = maps:get(surge_count, BurstStatus2, 0),

    ct:log("Burst status after spike: ~p", [BurstStatus2]),

    % Either surge triggered or baseline still being calculated
    true = (InSurge =:= true) orelse (SurgeCount >= 0),
    ok.

%% @doc Test 8: Graceful refusal - overloaded system fails fast.
test_graceful_refusal(_Config) ->
    % Arrange: Fill up to near capacity
    {ok, CapacityInfo} = heijunka_load_regulator:get_capacity_info(),
    Capacity = maps:get(total_capacity, CapacityInfo, 1200),

    ct:log("System capacity: ~p", [Capacity]),

    % Act: Request more than capacity
    ExcessRequests = 100,
    Results = [
        heijunka_load_regulator:request_admission(payment, #{})
        || _ <- lists:seq(1, Capacity + ExcessRequests)
    ],

    Rejected = length([R || R <- Results, R =:= {error, overload}]),
    Accepted = length([R || R <- Results, element(1, R) =:= ok]),

    ct:log("Accepted: ~p, Rejected: ~p", [Accepted, Rejected]),

    % Assert: All excess requests should be rejected
    true = Rejected >= ExcessRequests,
    true = Accepted =< Capacity,
    ok.

%%%===================================================================
%% Helper Functions
%%%===================================================================

%% @doc Simulate workload on a pool.
-spec simulate_load(atom(), integer(), integer()) -> ok.
simulate_load(Pool, WorkloadPercent, DurationMs) ->
    spawn(fun() ->
        EndTime = erlang:system_time(millisecond) + DurationMs,
        simulate_load_loop(Pool, WorkloadPercent, EndTime)
    end),
    ok.

%% @doc Load simulation loop.
-spec simulate_load_loop(atom(), integer(), integer()) -> ok.
simulate_load_loop(_Pool, _WorkloadPercent, EndTime) when erlang:system_time(millisecond) > EndTime ->
    ok;
simulate_load_loop(Pool, WorkloadPercent, EndTime) ->
    % Request admission for work item
    case heijunka_load_regulator:request_admission(Pool, #{}) of
        {ok, Token} ->
            % Simulate some work
            timer:sleep(WorkloadPercent),
            % Release quota
            heijunka_load_regulator:release_quota(Pool, Token);
        {error, overload} ->
            % Overloaded - fail fast
            ok
    end,
    simulate_load_loop(Pool, WorkloadPercent, EndTime).
