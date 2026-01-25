# TPS Jidoka: Practical Examples

Copy-paste ready examples for integrating Jidoka into your Erlang applications.

## Example 1: Simple HTTP Request Handler with Jidoka

```erlang
%% http_handler.erl
-module(http_handler).

-export([handle_request/2]).

%% Handle HTTP request with Jidoka protection
handle_request(Method, Path) ->
    %% Step 1: Check rate limit (fail-fast on overload)
    case jidoka_rate_limiter:acquire(1) of
        {error, rate_limit_exceeded} ->
            logger:warning("Rate limit exceeded: ~p ~p", [Method, Path]),
            {error, {429, "Too Many Requests"}};
        ok ->
            %% Step 2: Execute in worker pool (isolated)
            case jidoka_worker_pool:execute(fun() -> process_request(Method, Path) end) of
                {error, pool_exhausted} ->
                    logger:warning("Worker pool exhausted: ~p ~p", [Method, Path]),
                    {error, {503, "Service Unavailable"}};
                {error, Error} ->
                    logger:error("Request failed: ~p, error: ~p", [Path, Error]),
                    {error, {500, "Internal Server Error"}};
                Result ->
                    logger:info("Request succeeded: ~p ~p", [Method, Path]),
                    Result
            end
    end.

%% Process the actual request with circuit breaker for external service
process_request(<<"GET">>, Path) ->
    %% Call external service through circuit breaker
    case jidoka_circuit_breaker:call(fun() -> external_service:get(Path) end) of
        {error, circuit_open} ->
            logger:warning("External service unavailable, circuit open: ~p", [Path]),
            {error, {503, "Service Unavailable - Circuit Open"}};
        {error, Error} ->
            jidoka_circuit_breaker:record_failure(),
            logger:error("External service error: ~p", [Error]),
            {error, {502, "Bad Gateway"}};
        Result ->
            jidoka_circuit_breaker:record_success(),
            {ok, Result}
    end;

process_request(<<"POST">>, Path) ->
    %% Similar pattern for POST
    case jidoka_circuit_breaker:call(fun() -> external_service:post(Path) end) of
        {error, circuit_open} ->
            {error, {503, "Circuit Open"}};
        {error, Error} ->
            jidoka_circuit_breaker:record_failure(),
            {error, {502, "Bad Gateway"}};
        Result ->
            jidoka_circuit_breaker:record_success(),
            {ok, Result}
    end.
```

## Example 2: Database Connection Pool with Jidoka

```erlang
%% db_handler.erl
-module(db_handler).

-export([query/2, insert/2, health_check/0]).

%% Execute a SELECT query with Jidoka protection
query(Table, Conditions) ->
    case jidoka_worker_pool:execute(fun() -> do_query(Table, Conditions) end) of
        {error, pool_exhausted} ->
            logger:warning("DB pool exhausted: ~p", [Table]),
            {error, pool_exhausted};
        {error, Error} ->
            logger:error("Query failed: ~p, conditions: ~p", [Error, Conditions]),
            {error, Error};
        Result ->
            Result
    end.

%% Execute an INSERT with rate limiting
insert(Table, Data) ->
    %% Rate limit write operations (writes are more expensive)
    case jidoka_rate_limiter:acquire(2) of  % Writes cost 2 tokens
        {error, rate_limit_exceeded} ->
            logger:warning("Rate limit exceeded on insert: ~p", [Table]),
            {error, rate_limit_exceeded};
        ok ->
            case jidoka_worker_pool:execute(fun() -> do_insert(Table, Data) end) of
                {error, pool_exhausted} ->
                    {error, pool_exhausted};
                {error, Error} ->
                    logger:error("Insert failed: ~p", [Error]),
                    {error, Error};
                Result ->
                    Result
            end
    end.

%% Health check: database is responsive
health_check() ->
    case jidoka_worker_pool:health_check() of
        ok ->
            logger:info("Database pool healthy"),
            ok;
        {error, Reason} ->
            logger:error("Database pool unhealthy: ~p", [Reason]),
            {error, Reason}
    end.

%% Internal: Actual query execution
do_query(Table, Conditions) ->
    %% Call actual database library
    case db_client:query(Table, Conditions) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> error(Reason)
    end.

%% Internal: Actual insert execution
do_insert(Table, Data) ->
    case db_client:insert(Table, Data) of
        {ok, Id} -> {ok, Id};
        {error, Reason} -> error(Reason)
    end.
```

## Example 3: Async Task Queue with Jidoka

```erlang
%% task_queue.erl
-module(task_queue).

-export([enqueue_task/2, process_tasks/0, queue_status/0]).

%% Enqueue a task (rate limited to prevent queue explosion)
enqueue_task(TaskType, Payload) ->
    case jidoka_rate_limiter:acquire(1) of
        {error, rate_limit_exceeded} ->
            logger:warning("Task queue overloaded, rejecting: ~p", [TaskType]),
            {error, queue_overloaded};
        ok ->
            %% Task accepted, queue it
            Queue = get_queue(),
            NewQueue = queue:in({TaskType, Payload}, Queue),
            put_queue(NewQueue),
            {ok, queued}
    end.

%% Process tasks from queue using worker pool
process_tasks() ->
    Queue = get_queue(),
    case queue:out(Queue) of
        {empty, _} ->
            ok;
        {{value, {TaskType, Payload}}, NewQueue} ->
            put_queue(NewQueue),
            %% Execute task in worker pool (isolated)
            case jidoka_worker_pool:execute(fun() -> handle_task(TaskType, Payload) end) of
                {error, pool_exhausted} ->
                    %% Put task back if pool exhausted
                    put_queue(queue:in({TaskType, Payload}, NewQueue)),
                    logger:warning("Worker pool exhausted, task requeued"),
                    {error, pool_exhausted};
                {error, Error} ->
                    logger:error("Task failed: ~p, error: ~p", [TaskType, Error]),
                    {error, Error};
                Result ->
                    logger:info("Task completed: ~p", [TaskType]),
                    Result
            end
    end.

%% Get queue status
queue_status() ->
    Queue = get_queue(),
    #{
        queue_depth => queue:len(Queue),
        rate_limiter_status => jidoka_rate_limiter:status(),
        worker_pool_status => jidoka_worker_pool:status()
    }.

%% Internal: Handle specific task type
handle_task(email, {Address, Subject, Body}) ->
    email_service:send(Address, Subject, Body);

handle_task(notification, {UserId, Message}) ->
    notification_service:send(UserId, Message);

handle_task(report, {ReportType, Params}) ->
    report_generator:generate(ReportType, Params);

handle_task(Unknown, _) ->
    error({unknown_task, Unknown}).

%% Internal: Queue storage (simplified, use ETS or mnesia in production)
get_queue() ->
    case get(task_queue) of
        undefined -> queue:new();
        Q -> Q
    end.

put_queue(Queue) ->
    put(task_queue, Queue).
```

## Example 4: Integrating into Supervisor Tree

```erlang
%% my_app_sup.erl
-module(my_app_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    },

    ChildSpecs = [
        %% Jidoka: Fail-fast system with circuit breaker, rate limiting, worker pool
        #{
            id => jidoka,
            start => {jidoka_supervisor, start_link, [#{
                pool_size => 20,
                circuit_threshold => 5,
                window_ms => 10000,
                rate_limit => 5000
            }]},
            restart => permanent,
            shutdown => 10000,
            type => supervisor,
            modules => [jidoka_supervisor]
        },

        %% Your application logic
        #{
            id => my_app_handler,
            start => {my_app_handler, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [my_app_handler]
        },

        %% Monitoring process
        #{
            id => my_app_monitor,
            start => {my_app_monitor, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [my_app_monitor]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

## Example 5: Monitoring & Metrics

```erlang
%% jidoka_metrics.erl
-module(jidoka_metrics).

-export([export_metrics/0, report_status/0, alert_on_issues/0]).

%% Export Prometheus-style metrics
export_metrics() ->
    CB = jidoka_circuit_breaker:status(),
    {CBState, CBFailures, CBRecovery} = CB,
    RL = jidoka_rate_limiter:status(),
    {RLTokens, RLAccepted, RLRejected} = RL,
    WP = jidoka_worker_pool:status(),
    {available, Available} = WP,

    Metrics = [
        {circuit_breaker_state, state_to_int(CBState)},
        {circuit_breaker_failures, CBFailures},
        {rate_limiter_tokens, round(RLTokens)},
        {rate_limiter_accepted, RLAccepted},
        {rate_limiter_rejected, RLRejected},
        {worker_pool_available, Available}
    ],

    %% Export to metrics backend
    lists:foreach(fun({Name, Value}) ->
        logger:info("Metric: ~p=~p", [Name, Value])
    end, Metrics),

    Metrics.

%% Generate human-readable status report
report_status() ->
    Status = jidoka_supervisor:get_status(),
    PoolStatus = jidoka_supervisor:get_pool_status(),

    io:format("=== Jidoka Status Report ===~n"),
    io:format("Circuit Breaker: ~p~n", [maps:get(circuit_breaker_state, Status)]),
    io:format("Active Workers: ~p~n", [maps:get(active_workers, Status)]),
    io:format("Pool Utilization: ~p%~n", [maps:get(utilization_percent, PoolStatus)]),
    io:format("Timestamp: ~p~n", [maps:get(timestamp, Status)]),
    ok.

%% Alert if system health is degraded
alert_on_issues() ->
    Status = jidoka_supervisor:get_status(),
    PoolStatus = jidoka_supervisor:get_pool_status(),

    %% Check circuit breaker
    case maps:get(circuit_breaker_state, Status) of
        open ->
            logger:error("ALERT: Circuit breaker OPEN - external service failing");
        half_open ->
            logger:warning("ALERT: Circuit breaker HALF_OPEN - testing recovery");
        closed ->
            ok
    end,

    %% Check pool utilization
    Utilization = maps:get(utilization_percent, PoolStatus),
    case Utilization of
        U when U > 90 ->
            logger:error("ALERT: Worker pool ~p% utilized - consider scaling", [U]);
        U when U > 75 ->
            logger:warning("ALERT: Worker pool ~p% utilized - monitor closely", [U]);
        _ ->
            ok
    end,

    %% Check rate limiter
    {Tokens, Accepted, Rejected} = jidoka_rate_limiter:status(),
    RejectionRate = case Accepted + Rejected of
        0 -> 0;
        Total -> (Rejected / Total) * 100
    end,
    case RejectionRate of
        R when R > 10 ->
            logger:warning("ALERT: Rate limiter rejecting ~p% of requests", [round(R)]);
        _ ->
            ok
    end.

%% Helper: Convert circuit breaker state to integer for metrics
state_to_int(closed) -> 0;
state_to_int(open) -> 1;
state_to_int(half_open) -> 2.
```

## Example 6: Graceful Shutdown

```erlang
%% my_app.erl
-module(my_app).
-behaviour(application).

-export([start/2, stop/1]).

start(normal, []) ->
    logger:info("Starting my_app"),

    %% Start Jidoka first
    {ok, JidokaSup} = jidoka_supervisor:start_link(#{
        pool_size => 10,
        circuit_threshold => 5,
        window_ms => 10000,
        rate_limit => 1000
    }),

    %% Start app supervisor
    case my_app_sup:start_link() of
        {ok, AppSup} ->
            logger:info("my_app started successfully"),
            {ok, {AppSup, JidokaSup}};
        Error ->
            logger:error("Failed to start my_app: ~p", [Error]),
            Error
    end.

stop({AppSup, JidokaSup}) ->
    logger:info("Stopping my_app"),

    %% Wait for in-flight requests to complete (grace period)
    logger:info("Waiting for in-flight requests..."),
    timer:sleep(5000),

    %% Allow time for circuit breaker to recover
    logger:info("Allowing circuit breaker recovery time..."),
    timer:sleep(2000),

    %% Shutdown supervisors
    catch supervisor:terminate_child(AppSup, all),
    catch supervisor:terminate_child(JidokaSup, all),

    logger:info("my_app stopped"),
    ok.
```

## Example 7: Testing with Jidoka

```erlang
%% my_app_tests.erl
-module(my_app_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    {ok, _} = jidoka_supervisor:start_link(#{
        pool_size => 5,
        circuit_threshold => 3,
        rate_limit => 100
    }),
    ok.

cleanup(_) ->
    catch jidoka_supervisor:reset_circuit_breaker(),
    ok.

%% Test: Normal request succeeds
normal_request_test() ->
    setup(),
    Result = http_handler:handle_request(<<"GET">>, <<"/">]),
    ?assertMatch({ok, _}, Result),
    cleanup(ok).

%% Test: Rate limit exceeded
rate_limit_test() ->
    setup(),
    %% Exhaust rate limiter
    [jidoka_rate_limiter:acquire(100) || _ <- lists:seq(1, 2)],
    %% Next request should be rejected
    Result = jidoka_rate_limiter:acquire(1),
    ?assertEqual({error, rate_limit_exceeded}, Result),
    cleanup(ok).

%% Test: Circuit breaker opens on failures
circuit_breaker_test() ->
    setup(),
    CB = whereis(jidoka_circuit_breaker),
    %% Trigger failures
    [jidoka_circuit_breaker:call(fun() -> error(test) end) || _ <- lists:seq(1, 5)],
    %% Check state
    State = jidoka_circuit_breaker:get_state(CB),
    ?assertEqual(open, State),
    cleanup(ok).

%% Test: Pool exhaustion (fail-fast)
pool_exhaustion_test() ->
    setup(),
    %% Fill pool with blocking requests
    [spawn(fun() ->
        jidoka_worker_pool:execute(fun() -> timer:sleep(5000) end)
    end) || _ <- lists:seq(1, 6)],
    timer:sleep(100),
    %% Next request should be rejected
    Result = jidoka_worker_pool:execute(fun() -> ok end),
    ?assertEqual({error, pool_exhausted}, Result),
    cleanup(ok).
```

## Example 8: Operational Recovery Script

```erlang
%% jidoka_ops.erl - Operations/Recovery Module
-module(jidoka_ops).

-export([daily_check/0, recover_from_failure/0, scale_pool/1]).

%% Daily health check
daily_check() ->
    logger:info("=== Daily Jidoka Health Check ==="),

    Status = jidoka_supervisor:get_status(),
    CBState = maps:get(circuit_breaker_state, Status),

    case CBState of
        closed ->
            logger:info("✓ Circuit breaker: CLOSED (normal)");
        open ->
            logger:error("✗ Circuit breaker: OPEN (external service failing)"),
            logger:error("   Action: Investigate external service health");
        half_open ->
            logger:warning("⚠ Circuit breaker: HALF_OPEN (testing recovery)")
    end,

    PoolStatus = jidoka_supervisor:get_pool_status(),
    Utilization = maps:get(utilization_percent, PoolStatus),

    case Utilization of
        U when U > 90 ->
            logger:error("✗ Pool utilization: ~p% (CRITICAL)", [U]),
            logger:error("   Action: Consider scaling pool or investigating slow requests");
        U when U > 75 ->
            logger:warning("⚠ Pool utilization: ~p% (HIGH)", [U]),
            logger:warning("   Action: Monitor closely");
        U ->
            logger:info("✓ Pool utilization: ~p% (normal)", [U])
    end,

    {_Tokens, Accepted, Rejected} = jidoka_rate_limiter:status(),
    RejectionRate = case Accepted + Rejected of
        0 -> 0;
        Total -> (Rejected / Total) * 100
    end,

    case RejectionRate of
        R when R > 10 ->
            logger:warning("⚠ Rate limiter rejection: ~p% (ELEVATED)", [round(R)]);
        R ->
            logger:info("✓ Rate limiter rejection: ~p% (normal)", [round(R)])
    end,

    logger:info("=== Daily Check Complete ==="),
    ok.

%% Recover from external service failure
recover_from_failure() ->
    logger:info("=== Jidoka Recovery Procedure ==="),

    %% Step 1: Verify external service is healthy
    logger:info("Step 1: Checking external service health..."),
    case external_service:health_check() of
        ok ->
            logger:info("✓ External service is healthy");
        {error, Reason} ->
            logger:error("✗ External service still failing: ~p", [Reason]),
            logger:error("   Cannot recover until external service is fixed"),
            {error, service_still_failing}
    end,

    %% Step 2: Reset circuit breaker
    logger:info("Step 2: Resetting circuit breaker..."),
    ok = jidoka_supervisor:reset_circuit_breaker(),
    logger:info("✓ Circuit breaker reset to closed"),

    %% Step 3: Verify recovery
    logger:info("Step 3: Verifying recovery..."),
    Status = jidoka_supervisor:get_status(),
    CBState = maps:get(circuit_breaker_state, Status),

    case CBState of
        closed ->
            logger:info("✓ Recovery successful - circuit breaker is CLOSED"),
            logger:info("=== Recovery Complete ==="),
            {ok, recovered};
        _ ->
            logger:error("✗ Recovery failed - circuit breaker is still ~p", [CBState]),
            {error, recovery_failed}
    end.

%% Scale pool size (for operational adjustment)
scale_pool(NewSize) ->
    logger:info("Scaling worker pool to ~p workers", [NewSize]),
    % Note: This requires stopping and restarting Jidoka
    % In production, would use a more sophisticated scaling mechanism
    catch jidoka_supervisor:reset_circuit_breaker(),
    {ok, _} = jidoka_supervisor:start_link(#{pool_size => NewSize}),
    logger:info("Pool scaled to ~p workers", [NewSize]),
    {ok, scaled}.
```

## Summary

These examples show:
1. **HTTP Handling** - Protecting HTTP endpoints with rate limiting and circuit breaking
2. **Database Operations** - Managing database load with worker pools
3. **Task Queues** - Preventing queue explosion with rate limiting
4. **Supervisor Integration** - Adding Jidoka to your supervision tree
5. **Monitoring** - Exporting metrics and alerting on issues
6. **Graceful Shutdown** - Allowing in-flight requests to complete
7. **Testing** - Writing tests that verify Jidoka behavior
8. **Operations** - Daily checks and recovery procedures

All examples follow Jidoka principles:
- **Fail-fast**: Reject immediately when system overloaded
- **Don't amplify**: Circuit breaker stops calling failing services
- **Observable**: Every action is logged
- **Isolated**: Worker crashes don't affect other workers
