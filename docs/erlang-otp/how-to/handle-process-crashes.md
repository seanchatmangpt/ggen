# How to Handle Process Crashes

**Problem**: Your gen_server crashes intermittently under load, causing service disruption.

**Solution**: Implement layered supervision with exponential backoff and circuit breakers.

## When to Use This Guide

- ✅ Worker processes crash due to external failures (network, database)
- ✅ Need to prevent restart storms
- ✅ Want graceful degradation under failure
- ✅ Building production-grade fault tolerance

## Quick Solutions

### 1. Basic Supervision with Backoff

**Problem**: Worker restarts immediately, crashes again, enters restart storm.

**Solution**: Use exponential backoff to delay restarts.

```erlang
-module(backoff_worker).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_info/2]).

-record(state, {
    retry_count = 0 :: non_neg_integer(),
    max_retries = 5 :: non_neg_integer()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    % Try to connect with backoff
    {ok, retry_connect(#state{})}.

retry_connect(State = #state{retry_count = Count, max_retries = Max})
        when Count >= Max ->
    % Give up after max retries
    {stop, max_retries_exceeded};

retry_connect(State = #state{retry_count = Count}) ->
    case connect_to_external_service() of
        {ok, Connection} ->
            State#state{retry_count = 0};  % Reset on success
        {error, _Reason} ->
            % Exponential backoff: 100ms, 200ms, 400ms, 800ms, 1600ms
            Delay = 100 * trunc(math:pow(2, Count)),
            timer:send_after(Delay, retry_connect),
            State#state{retry_count = Count + 1}
    end.

handle_info(retry_connect, State) ->
    {noreply, retry_connect(State)}.

connect_to_external_service() ->
    % Your connection logic here
    {error, not_implemented}.
```

### 2. Transient Restart Policy

**Problem**: Worker crashes on normal shutdown, supervisor restarts unnecessarily.

**Solution**: Use `restart => transient` to only restart abnormal exits.

```erlang
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    ChildSpecs = [
        #{
            id => worker,
            start => {my_worker, start_link, []},
            restart => transient,    % Only restart if exit was abnormal
            shutdown => 5000,
            type => worker
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

**Restart policies**:
- `permanent` - Always restart (default for infrastructure)
- `transient` - Restart only if abnormal exit (good for workers)
- `temporary` - Never restart (one-shot tasks)

### 3. Circuit Breaker Pattern

**Problem**: Failing external service causes cascading failures.

**Solution**: Implement circuit breaker to fail fast and recover gracefully.

```erlang
-module(circuit_breaker).
-behaviour(gen_server).

-export([start_link/0, call/1]).
-export([init/1, handle_call/3, handle_info/2]).

-record(state, {
    status = closed :: closed | open | half_open,
    failure_count = 0 :: non_neg_integer(),
    failure_threshold = 5 :: non_neg_integer(),
    timeout = 60000 :: non_neg_integer()  % 60 seconds
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

call(Request) ->
    gen_server:call(?MODULE, {call, Request}).

init([]) ->
    {ok, #state{}}.

handle_call({call, Request}, _From, State = #state{status = open}) ->
    % Circuit is open - fail fast
    {reply, {error, circuit_open}, State};

handle_call({call, Request}, _From, State = #state{status = half_open}) ->
    % Test if service recovered
    case external_call(Request) of
        {ok, Response} ->
            % Success! Close circuit
            {reply, {ok, Response}, State#state{status = closed, failure_count = 0}};
        {error, Reason} ->
            % Still failing - reopen circuit
            erlang:send_after(State#state.timeout, self(), try_half_open),
            {reply, {error, Reason}, State#state{status = open}}
    end;

handle_call({call, Request}, _From, State = #state{status = closed}) ->
    case external_call(Request) of
        {ok, Response} ->
            {reply, {ok, Response}, State#state{failure_count = 0}};
        {error, Reason} ->
            NewCount = State#state.failure_count + 1,
            if
                NewCount >= State#state.failure_threshold ->
                    % Open circuit
                    erlang:send_after(State#state.timeout, self(), try_half_open),
                    {reply, {error, Reason}, State#state{status = open, failure_count = NewCount}};
                true ->
                    {reply, {error, Reason}, State#state{failure_count = NewCount}}
            end
    end.

handle_info(try_half_open, State) ->
    % Try to recover
    {noreply, State#state{status = half_open}}.

external_call(_Request) ->
    % Your external service call here
    {error, not_implemented}.
```

### 4. Bulkhead Pattern

**Problem**: One failing worker exhausts all resources, affecting other workers.

**Solution**: Isolate workers into separate pools with dedicated supervisors.

```erlang
-module(bulkhead_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    % Separate pools for different services
    ChildSpecs = [
        #{
            id => database_pool_sup,
            start => {pool_sup, start_link, [database_pool, 10]},
            restart => permanent,
            type => supervisor
        },
        #{
            id => api_pool_sup,
            start => {pool_sup, start_link, [api_pool, 20]},
            restart => permanent,
            type => supervisor
        },
        #{
            id => cache_pool_sup,
            start => {pool_sup, start_link, [cache_pool, 5]},
            restart => permanent,
            type => supervisor
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

**Benefits**:
- Database failures don't affect API calls
- API failures don't affect cache
- Independent scaling of each pool

### 5. Graceful Degradation

**Problem**: Critical service down, want to serve cached/stale data instead of crashing.

**Solution**: Implement fallback logic with stale data tolerance.

```erlang
-module(graceful_service).
-behaviour(gen_server).

-export([start_link/0, get_data/1]).
-export([init/1, handle_call/3]).

-record(state, {
    cache :: ets:tab(),
    cache_ttl = 300000 :: non_neg_integer()  % 5 minutes
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_data(Key) ->
    gen_server:call(?MODULE, {get, Key}, 10000).

init([]) ->
    Cache = ets:new(data_cache, [set, private]),
    {ok, #state{cache = Cache}}.

handle_call({get, Key}, _From, State) ->
    Now = erlang:system_time(millisecond),

    case external_service:fetch(Key) of
        {ok, FreshData} ->
            % Cache fresh data
            ets:insert(State#state.cache, {Key, FreshData, Now}),
            {reply, {ok, FreshData}, State};

        {error, _Reason} ->
            % Service failed - try cache
            case ets:lookup(State#state.cache, Key) of
                [{Key, CachedData, Timestamp}] ->
                    Age = Now - Timestamp,
                    if
                        Age < State#state.cache_ttl ->
                            % Cache is fresh enough
                            {reply, {ok, CachedData, stale}, State};
                        true ->
                            % Cache too old
                            {reply, {error, service_unavailable}, State}
                    end;
                [] ->
                    % No cache available
                    {reply, {error, service_unavailable}, State}
            end
    end.
```

## Advanced Patterns

### Health Check with Supervised Restart

Monitor worker health and proactively restart before failure.

```erlang
-module(health_checked_worker).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_info/2]).

-record(state, {
    health_check_interval = 10000 :: non_neg_integer(),
    unhealthy_count = 0 :: non_neg_integer(),
    max_unhealthy = 3 :: non_neg_integer()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    schedule_health_check(0),  % Immediate first check
    {ok, #state{}}.

handle_info(health_check, State) ->
    case check_health() of
        ok ->
            schedule_health_check(State#state.health_check_interval),
            {noreply, State#state{unhealthy_count = 0}};
        {error, _Reason} ->
            NewCount = State#state.unhealthy_count + 1,
            if
                NewCount >= State#state.max_unhealthy ->
                    % Proactively crash - supervisor will restart
                    {stop, unhealthy, State};
                true ->
                    schedule_health_check(State#state.health_check_interval),
                    {noreply, State#state{unhealthy_count = NewCount}}
            end
    end.

schedule_health_check(Delay) ->
    erlang:send_after(Delay, self(), health_check).

check_health() ->
    % Your health check logic
    ok.
```

### Supervisor Intensity Monitoring

Track supervisor restart rates to detect systemic issues.

```erlang
-module(monitored_supervisor).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    % Start metrics collector
    start_metrics_collector(),

    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpecs = [worker_spec()],

    {ok, {SupFlags, ChildSpecs}}.

start_metrics_collector() ->
    spawn_link(fun() -> collect_restart_metrics(?MODULE) end).

collect_restart_metrics(SupPid) ->
    timer:sleep(5000),  % Every 5 seconds

    % Count current children
    Children = supervisor:which_children(SupPid),
    ActiveCount = length([C || {_, Pid, _, _} <- Children, is_pid(Pid)]),

    % Log to metrics system
    logger:info("Supervisor ~p has ~p active children", [SupPid, ActiveCount]),

    collect_restart_metrics(SupPid).
```

## Production Checklist

When handling process crashes in production:

- ✅ Use appropriate restart policy (`permanent`/`transient`/`temporary`)
- ✅ Set reasonable intensity limits (prevent restart storms)
- ✅ Implement exponential backoff for external dependencies
- ✅ Add circuit breakers for failing services
- ✅ Use bulkheads to isolate failure domains
- ✅ Implement graceful degradation (serve stale data if needed)
- ✅ Monitor supervisor restart rates
- ✅ Log crash reasons for debugging
- ✅ Set up alerts for high restart rates
- ✅ Test failure scenarios in staging

## Testing Crash Handling

```erlang
% In your test suite
test_worker_restart() ->
    % Start supervisor
    {ok, SupPid} = my_supervisor:start_link(),

    % Get worker PID
    [{worker, WorkerPid, _, _}] = supervisor:which_children(SupPid),

    % Kill worker
    exit(WorkerPid, kill),

    % Wait for restart
    timer:sleep(100),

    % Verify new worker started
    [{worker, NewWorkerPid, _, _}] = supervisor:which_children(SupPid),
    ?assert(is_pid(NewWorkerPid)),
    ?assert(WorkerPid =/= NewWorkerPid).
```

## Related Guides

- [Building Supervision Trees](../tutorials/03-supervision-trees.md) - Foundational concepts
- [Optimize Message Passing](optimize-message-passing.md) - Prevent mailbox-related crashes
- [Hot Code Reloading](hot-code-reloading.md) - Update without downtime

## References

- [Supervisor API](../reference/supervisor-api.md)
- [gen_server API](../reference/gen-server-api.md)
- [Let It Crash Philosophy](../explanation/let-it-crash-philosophy.md)

---

**When to use**: Production systems with external dependencies
**Difficulty**: Intermediate
**See also**: Circuit Breaker pattern, Bulkhead pattern, Exponential backoff
