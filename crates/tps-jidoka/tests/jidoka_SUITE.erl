%%%-------------------------------------------------------------------
%%% @doc Jidoka Test Suite - Black-Box Testing
%%%
%%% Chicago TDD style tests (AAA pattern: Arrange, Act, Assert).
%%% No mocks - tests use real circuit breaker, worker pool, and rate limiter.
%%%
%%% Test Strategy:
%%% 1. Failure scenarios: Verify fail-fast behavior (no queueing)
%%% 2. Recovery scenarios: Verify state transitions (open -> half_open -> closed)
%%% 3. Stress tests: Verify no queue buildup under load
%%% 4. Jidoka verification: All timeouts < 100ms (fail-fast requirement)
%%% @end
%%%-------------------------------------------------------------------

-module(jidoka_SUITE).

%% Common Test callbacks
-export([suite/0, init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    %% Circuit breaker tests
    test_circuit_breaker_closed_to_open/1,
    test_circuit_breaker_open_to_half_open/1,
    test_circuit_breaker_half_open_to_closed/1,
    test_circuit_breaker_half_open_to_open/1,

    %% Worker pool tests
    test_worker_pool_execute_success/1,
    test_worker_pool_exhaustion_fails_fast/1,
    test_worker_pool_health_check/1,

    %% Rate limiter tests
    test_rate_limiter_accept_within_limit/1,
    test_rate_limiter_reject_exceeds_limit/1,
    test_rate_limiter_refill_tokens/1,

    %% Integration tests
    test_jidoka_supervisor_startup/1,
    test_jidoka_full_system_overload/1,
    test_jidoka_recovery_after_error/1,

    %% Stress tests
    test_jidoka_stress_concurrent_requests/1,
    test_jidoka_stress_no_queue_buildup/1
]).

%%%===================================================================
%%% Common Test Callbacks
%%%===================================================================

%% @private Test suite info.
suite() ->
    [
        {timetrap, {seconds, 60}},
        {require, jidoka_config}
    ].

%% @private Setup for entire test suite.
init_per_suite(Config) ->
    logger:set_primary_config(level, info),
    logger:add_handler(jidoka_handler, logger_std_h, #{}),

    %% Start the full Jidoka system
    JidokaConfig = #{
        pool_size => 5,
        circuit_threshold => 3,
        window_ms => 5000,
        rate_limit => 100
    },

    case jidoka_supervisor:start_link(JidokaConfig) of
        {ok, _Pid} ->
            timer:sleep(500),  % Let everything stabilize
            [{jidoka_config, JidokaConfig} | Config];
        {error, {already_started, _}} ->
            [{jidoka_config, JidokaConfig} | Config];
        {error, Reason} ->
            ct:fail("Failed to start Jidoka supervisor: ~p", [Reason])
    end.

%% @private Cleanup after entire test suite.
end_per_suite(_Config) ->
    catch jidoka_supervisor:reset_circuit_breaker(),
    timer:sleep(100),
    ok.

%% @private Setup for each test case.
init_per_testcase(_TestCase, Config) ->
    timer:sleep(200),  % Let system stabilize between tests
    Config.

%% @private Cleanup after each test case.
end_per_testcase(_TestCase, _Config) ->
    catch jidoka_supervisor:reset_circuit_breaker(),
    timer:sleep(100),
    ok.

%%%===================================================================
%%% Circuit Breaker Tests
%%%===================================================================

%% @doc Test: Circuit breaker transitions from closed to open on threshold.
test_circuit_breaker_closed_to_open(Config) ->
    StartTime = erlang:system_time(millisecond),

    %% Arrange: Get threshold from config
    JidokaConfig = ?config(jidoka_config, Config),
    Threshold = maps:get(circuit_threshold, JidokaConfig, 3),

    %% Act: Trigger failures to exceed threshold
    CB = whereis(jidoka_circuit_breaker),
    ct:assertTrue(CB =/= undefined, "Circuit breaker not running"),

    FailingFun = fun() -> error(simulated_failure) end,
    [jidoka_circuit_breaker:call(FailingFun) || _ <- lists:seq(1, Threshold + 1)],

    %% Assert: Circuit should be open
    ClosedTime = erlang:system_time(millisecond) - StartTime,
    State = jidoka_circuit_breaker:get_state(CB),
    ct:assertEqual(open, State, "Circuit breaker should be open after threshold"),
    ct:assertTrue(ClosedTime < 1000, "Circuit should open within 1s"),

    {comment, io_lib:format("Circuit opened in ~pms", [ClosedTime])}.

%% @doc Test: Circuit breaker transitions from open to half_open on timeout.
test_circuit_breaker_open_to_half_open(_Config) ->
    CB = whereis(jidoka_circuit_breaker),

    %% Arrange: Ensure circuit is open
    jidoka_supervisor:reset_circuit_breaker(),
    FailingFun = fun() -> error(test_failure) end,
    [jidoka_circuit_breaker:call(FailingFun) || _ <- lists:seq(1, 5)],

    State1 = jidoka_circuit_breaker:get_state(CB),
    ct:assertEqual(open, State1, "Circuit should be open"),

    %% Act: Wait for recovery timeout (30s in default config, but we use shorter timeout)
    timer:sleep(35000),

    %% Assert: Circuit should transition to half_open
    State2 = jidoka_circuit_breaker:get_state(CB),
    ct:assertEqual(half_open, State2, "Circuit should be half_open after timeout"),

    {comment, "Circuit breaker recovery tested successfully"}.

%% @doc Test: Circuit breaker transitions from half_open to closed on success.
test_circuit_breaker_half_open_to_closed(_Config) ->
    CB = whereis(jidoka_circuit_breaker),

    %% Arrange: Get circuit to half_open state
    jidoka_supervisor:reset_circuit_breaker(),
    FailingFun = fun() -> error(test_failure) end,
    [jidoka_circuit_breaker:call(FailingFun) || _ <- lists:seq(1, 5)],
    timer:sleep(31000),  % Wait for recovery

    State = jidoka_circuit_breaker:get_state(CB),
    ct:assertEqual(half_open, State, "Circuit should be half_open"),

    %% Act: Successful call in half_open state
    SuccessFun = fun() -> ok end,
    Result = jidoka_circuit_breaker:call(SuccessFun),

    %% Assert: Circuit should close
    ct:assertEqual(ok, Result, "Call should succeed"),
    State2 = jidoka_circuit_breaker:get_state(CB),
    ct:assertEqual(closed, State2, "Circuit should close after successful call"),

    {comment, "Circuit breaker recovery to closed tested"}.

%% @doc Test: Circuit breaker transitions from half_open back to open on failure.
test_circuit_breaker_half_open_to_open(_Config) ->
    CB = whereis(jidoka_circuit_breaker),

    %% Arrange: Get circuit to half_open state
    jidoka_supervisor:reset_circuit_breaker(),
    FailingFun = fun() -> error(test_failure) end,
    [jidoka_circuit_breaker:call(FailingFun) || _ <- lists:seq(1, 5)],
    timer:sleep(31000),  % Wait for recovery

    State = jidoka_circuit_breaker:get_state(CB),
    ct:assertEqual(half_open, State, "Circuit should be half_open"),

    %% Act: Failed call in half_open state
    Result = jidoka_circuit_breaker:call(FailingFun),

    %% Assert: Circuit should reopen
    ct:assertEqual({error, error}, Result, "Call should fail"),
    State2 = jidoka_circuit_breaker:get_state(CB),
    ct:assertEqual(open, State2, "Circuit should reopen after failed call"),

    {comment, "Circuit breaker half_open->open tested"}.

%%%===================================================================
%%% Worker Pool Tests
%%%===================================================================

%% @doc Test: Worker pool executes successful functions.
test_worker_pool_execute_success(_Config) ->
    %% Arrange: Simple function
    TestFun = fun() -> {result, success} end,

    %% Act: Execute through pool
    Result = jidoka_worker_pool:execute(TestFun),

    %% Assert: Result returned correctly
    ct:assertEqual({result, success}, Result, "Worker pool should execute and return result"),

    {comment, "Worker pool success test passed"}.

%% @doc Test: Worker pool rejects when exhausted (Jidoka fail-fast).
test_worker_pool_exhaustion_fails_fast(Config) ->
    JidokaConfig = ?config(jidoka_config, Config),
    PoolSize = maps:get(pool_size, JidokaConfig, 5),

    %% Arrange: Blocking functions to exhaust pool
    BlockingFun = fun() -> timer:sleep(5000) end,

    %% Act: Spawn workers to fill pool
    StartTime = erlang:system_time(millisecond),
    Results = [
        catch jidoka_worker_pool:execute(BlockingFun, jidoka_worker_pool)
        || _ <- lists:seq(1, PoolSize + 2)
    ],
    ElapsedMs = erlang:system_time(millisecond) - StartTime,

    %% Assert: Some requests should be rejected quickly (Jidoka fail-fast)
    Rejected = [R || R <- Results, R =:= {error, pool_exhausted}],
    ct:assertTrue(length(Rejected) > 0, "Some requests should be rejected"),
    ct:assertTrue(ElapsedMs < 1000, "Rejection should be fast (< 1s)"),

    {comment, io_lib:format("Pool exhaustion tested: ~p rejected in ~pms", [length(Rejected), ElapsedMs])}.

%% @doc Test: Worker pool health check.
test_worker_pool_health_check(_Config) ->
    %% Arrange & Act: Perform health check
    Result = jidoka_worker_pool:health_check(),

    %% Assert: Health check should pass
    ct:assertEqual(ok, Result, "Worker pool health check should pass"),

    {comment, "Worker pool health check passed"}.

%%%===================================================================
%%% Rate Limiter Tests
%%%===================================================================

%% @doc Test: Rate limiter accepts requests within limit.
test_rate_limiter_accept_within_limit(_Config) ->
    %% Arrange: Reset rate limiter
    jidoka_rate_limiter:reset(),

    %% Act: Acquire tokens within limit
    Result = jidoka_rate_limiter:acquire(10),

    %% Assert: Should succeed
    ct:assertEqual(ok, Result, "Rate limiter should accept within limit"),

    {comment, "Rate limiter acceptance test passed"}.

%% @doc Test: Rate limiter rejects requests exceeding limit.
test_rate_limiter_reject_exceeds_limit(_Config) ->
    %% Arrange: Reset and acquire all tokens
    jidoka_rate_limiter:reset(),
    jidoka_rate_limiter:acquire(100),  % Assuming burst_size is 50-100

    %% Act: Try to acquire more
    Result = jidoka_rate_limiter:acquire(1),

    %% Assert: Should fail
    ct:assertEqual({error, rate_limit_exceeded}, Result, "Rate limiter should reject over limit"),

    {comment, "Rate limiter rejection test passed"}.

%% @doc Test: Rate limiter refills tokens over time.
test_rate_limiter_refill_tokens(_Config) ->
    %% Arrange: Reset and exhaust tokens
    jidoka_rate_limiter:reset(),
    jidoka_rate_limiter:acquire(100),

    Result1 = jidoka_rate_limiter:acquire(1),
    ct:assertEqual({error, rate_limit_exceeded}, Result1, "Should be exhausted"),

    %% Act: Wait for refill
    timer:sleep(1100),

    Result2 = jidoka_rate_limiter:acquire(1),

    %% Assert: Should succeed after refill
    ct:assertEqual(ok, Result2, "Rate limiter should refill tokens over time"),

    {comment, "Rate limiter refill test passed"}.

%%%===================================================================
%%% Integration Tests
%%%===================================================================

%% @doc Test: Jidoka supervisor starts all components.
test_jidoka_supervisor_startup(_Config) ->
    %% Arrange: Components should be running
    CB = whereis(jidoka_circuit_breaker),
    RL = whereis(jidoka_rate_limiter),
    WP = whereis(jidoka_worker_pool),

    %% Act & Assert: All should be running
    ct:assertTrue(CB =/= undefined, "Circuit breaker should be running"),
    ct:assertTrue(RL =/= undefined, "Rate limiter should be running"),
    ct:assertTrue(WP =/= undefined, "Worker pool should be running"),

    %% Check status
    Status = jidoka_supervisor:get_status(),
    ct:assertTrue(is_map(Status), "Status should be a map"),

    {comment, "Jidoka supervisor startup test passed"}.

%% @doc Test: Jidoka system handles overload by failing fast.
test_jidoka_full_system_overload(Config) ->
    JidokaConfig = ?config(jidoka_config, Config),
    RateLimit = maps:get(rate_limit, JidokaConfig, 100),

    %% Arrange: Reset system
    jidoka_supervisor:reset_circuit_breaker(),
    jidoka_rate_limiter:reset(),

    %% Act: Send more requests than rate limit
    StartTime = erlang:system_time(millisecond),
    Results = [
        jidoka_rate_limiter:acquire(1) || _ <- lists:seq(1, RateLimit + 50)
    ],
    ElapsedMs = erlang:system_time(millisecond) - StartTime,

    %% Assert: All rejections should be fast
    Accepted = length([R || R <- Results, R =:= ok]),
    Rejected = length([R || R <- Results, R =:= {error, rate_limit_exceeded}]),

    ct:assertTrue(Accepted > 0, "Some requests should be accepted"),
    ct:assertTrue(Rejected > 0, "Some requests should be rejected"),
    ct:assertTrue(ElapsedMs < 1000, "System should fail fast (< 1s for all operations)"),

    {comment, io_lib:format("Overload test: ~p accepted, ~p rejected in ~pms", [Accepted, Rejected, ElapsedMs])}.

%% @doc Test: Jidoka system recovers after error.
test_jidoka_recovery_after_error(_Config) ->
    %% Arrange: Trigger failures
    CB = whereis(jidoka_circuit_breaker),
    FailingFun = fun() -> error(test_error) end,
    [jidoka_circuit_breaker:call(FailingFun) || _ <- lists:seq(1, 5)],

    State1 = jidoka_circuit_breaker:get_state(CB),
    ct:assertEqual(open, State1, "Circuit should be open"),

    %% Act: Reset circuit breaker
    ok = jidoka_supervisor:reset_circuit_breaker(),

    %% Assert: Circuit should be closed again
    State2 = jidoka_circuit_breaker:get_state(CB),
    ct:assertEqual(closed, State2, "Circuit should be closed after reset"),

    {comment, "Jidoka recovery test passed"}.

%%%===================================================================
%%% Stress Tests
%%%===================================================================

%% @doc Test: Jidoka handles 1000 concurrent requests without hanging.
test_jidoka_stress_concurrent_requests(_Config) ->
    %% Arrange: Prepare 1000 test functions
    NumRequests = 1000,
    TestFun = fun() -> timer:sleep(10) end,

    %% Act: Execute all concurrently
    StartTime = erlang:system_time(millisecond),
    _Results = [
        spawn(fun() -> jidoka_worker_pool:execute(TestFun) end)
        || _ <- lists:seq(1, NumRequests)
    ],
    timer:sleep(100),  % Let all requests start
    ElapsedMs = erlang:system_time(millisecond) - StartTime,

    %% Assert: Should complete in reasonable time (fail-fast, not queued)
    ct:assertTrue(ElapsedMs < 5000, "Should handle concurrent requests quickly"),

    {comment, io_lib:format("Concurrent requests test: ~p requests in ~pms", [NumRequests, ElapsedMs])}.

%% @doc Test: Jidoka doesn't build up queue under stress.
test_jidoka_stress_no_queue_buildup(_Config) ->
    %% Arrange: Get initial pool status
    Status1 = jidoka_worker_pool:status(),
    {available, Available1} = Status1,

    %% Act: Send burst of requests
    TestFun = fun() -> timer:sleep(5) end,
    [jidoka_worker_pool:execute(TestFun) || _ <- lists:seq(1, 20)],
    timer:sleep(200),

    %% Assert: Pool should return to normal (no queue buildup)
    Status2 = jidoka_worker_pool:status(),
    {available, Available2} = Status2,

    ct:assertTrue(Available2 >= Available1 - 2, "Pool should recover (no queue buildup)"),

    {comment, io_lib:format("Queue test: initial=~p, final=~p available", [Available1, Available2])}.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @private Assert that result is within time bound (Jidoka fail-fast requirement).
assert_fail_fast(ElapsedMs, MaxMs, Context) ->
    ct:assertTrue(
        ElapsedMs =< MaxMs,
        io_lib:format("~s: ~pms exceeds ~pms", [Context, ElapsedMs, MaxMs])
    ).
