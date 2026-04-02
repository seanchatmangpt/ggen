# TPS Heijunka Architecture

## System Components

### 1. heijunka_sup (Supervisor)
**Purpose**: Root supervisor managing all Heijunka subsystems
**Supervision Strategy**: `one_for_all` (if one fails, restart all)
**Children**:
- heijunka_pool_sup (supervises worker pools)
- heijunka_load_regulator (admission control)
- heijunka_balancer (work distribution)
- heijunka_burst_handler (spike management)

```
heijunka_sup (root, one_for_all)
├── heijunka_pool_sup (one_for_one)
│   ├── payment_pool
│   ├── delivery_pool
│   └── notification_pool
├── heijunka_load_regulator (worker)
├── heijunka_balancer (worker)
└── heijunka_burst_handler (worker)
```

### 2. heijunka_pool.erl (Per-Domain Pool Manager)
**Responsibility**: Manage worker pool for a single domain (payment, delivery, etc.)

**Key Functions**:
```erlang
get_worker/1           % Check out worker (blocking)
return_worker/2        % Return worker to pool
pool_status/1          % Get current metrics
scale_up/1             % Add worker
scale_down/1           % Remove worker
health_check/1         % Verify worker health
```

**Data Structure**:
```erlang
#pool_state {
    pool_name,              % e.g., payment
    domain,                 % e.g., payment
    worker_count,           % Current count
    max_workers,            % Upper bound
    overflow,               % Overflow count
    queue_length,           % Waiting requests
    total_requests,         % Lifetime total
    failed_requests,        % Lifetime failures
    avg_latency,            % Average response time
    health_check_timer,     % Periodic check
    metrics_timer,          % Metric updates
    worker_latencies,       % Queue of latencies
    last_scaling_time       % When last scaled
}
```

**Scaling Algorithm**:
```
Every 5 seconds (update_metrics):
  Utilization = (Workers - Available) / Workers

  if Utilization > 80% AND Workers < MaxWorkers
    → add_worker()
    last_scaling_time = now

  else if Utilization < 50% AND Workers > MinWorkers
    → remove_worker()
    last_scaling_time = now
```

**Health Classification**:
```
healthy:    Util ≤ 80% AND ErrorRate ≤ 5%
degraded:   Util > 80% OR ErrorRate > 5%
critical:   Util > 95%
```

### 3. heijunka_load_regulator.erl (Admission Control)
**Responsibility**: Prevent overload by controlling request admission

**Key Functions**:
```erlang
request_admission/2         % Request quota token
release_quota/2             % Return quota
regulator_status/0          % Get stats
set_soft_limit/1            % Adjust soft limit
set_hard_limit/1            % Adjust hard limit
get_capacity_info/0         % Check headroom
```

**Design Pattern**: Token-based admission
```erlang
{ok, Token} = request_admission(Domain, Metadata)
try
    do_work()
finally
    release_quota(Domain, Token)
end
```

**Limits**:
```
Regular domains:
  effective_limit = hard_limit * (1 - reserved_capacity)
                  = 1200 * 0.8
                  = 960

Priority domains (payment, critical):
  effective_limit = hard_limit
                  = 1200

Rejection logic:
  if load > hard_limit
    → reject (absolute maximum exceeded)
  else if load > effective_limit AND not priority
    → reject (regular limit exceeded)
  else
    → accept (return token)
```

**Data Structure**:
```erlang
#regulator_state {
    soft_limit,             % 1000 (warning threshold)
    hard_limit,             % 1200 (rejection threshold)
    reserved_capacity,      % 0.20 (20%)
    current_load,           % Current admission count
    accepted_requests,      % Lifetime accepted
    rejected_requests,      % Lifetime rejected
    admitted_quota,         % Set of active tokens
    alert_threshold,        % Soft limit * (1 - reserved)
    capacity_check_timer,   % Every 1 second
    alert_check_timer       % Every 5 seconds
}
```

### 4. heijunka_balancer.erl (Work Distribution)
**Responsibility**: Route work fairly across pools

**Key Functions**:
```erlang
route_request/3            % Get pool for request
balance_status/0           % Check balance metrics
rebalance_pools/0          % Trigger rebalancing
set_affinity_groups/1      % Sticky routing
get_pool_metrics/0         % Pool stats
```

**Routing Algorithm**:
```erlang
select_pool(RoutingKey, Domain) →
  % Check affinity first
  if has_affinity(RoutingKey)
    PreferredPool = affinity_pool(RoutingKey)
  else
    % Hash-based consistent routing
    Hash = erlang:phash2(RoutingKey, PoolCount)
    PreferredPool = nth(Hash + 1, Pools)

  % Check if preferred is overloaded
  if utilization(PreferredPool) > 85%
    → return least_loaded_pool()
  else
    → return PreferredPool
```

**Affinity Benefits**:
- Same request type → same pool
- Better cache locality
- Reduced lock contention
- Predictable behavior

**Rebalancing**:
```erlang
Coefficient = calculate_balance_coefficient(Utilizations)

if Coefficient < 0.8 AND time_since_last_rebalance > 30s
  → find most and least loaded pools
  → migrate_worker(most_loaded, least_loaded)
  → increment rebalance_count
```

**Balance Coefficient**:
```
coefficient = 1.0 - (std_dev / max_std_dev)

Where:
  std_dev = standard deviation of pool utilizations
  max_std_dev ≈ 0.5 (empirical maximum)

Result:
  1.0 = all pools same utilization (perfect)
  0.8 = good (max diff ~0.1)
  0.5 = fair (max diff ~0.25)
  <0.5 = poor (max diff > 0.25, rebalance triggered)
```

### 5. heijunka_burst_handler.erl (Spike Management)
**Responsibility**: Detect and handle traffic bursts

**Key Functions**:
```erlang
burst_status/0             % Current burst state
get_surge_metrics/0        % Surge details
disable_surge/0            % Manual control
enable_surge/0             % Re-enable
```

**Burst Detection**:
```erlang
% Sample system load every 1 second
current_load = average_pool_utilizations()

% Calculate baseline from 60-second window
baseline = average(last_60_samples)

% Burst threshold
threshold = baseline * 10  % 10x multiplier

% Detection
if current_load > threshold
  → trigger_surge()
```

**Surge Activation**:
```erlang
trigger_surge() →
  surge_workers = 5 per pool
  surge_pools = [payment, delivery, notification]
  surge_duration = 5 minutes
  surge_cooldown = 30 seconds

  % Cost tracking
  surge_cost = surge_workers * pools * cost_factor(2.0)
             = 5 * 3 * 2.0
             = 30.0 per surge event
```

**Data Structure**:
```erlang
#burst_state {
    enabled,                % Surge enabled/disabled
    baseline_load,          % Rolling average (60s)
    current_load,           % Current utilization
    load_samples,           % Last 60 samples
    in_surge,               % Currently surging?
    surge_start_time,       % When surge started
    surge_pools,            % Pools with surge workers
    surge_count,            % Lifetime surge events
    total_surge_cost,       % Cumulative cost
    detection_timer,        % Every 1 second
    baseline_timer,         % Every 60 seconds
    cooldown_timer          % Surge duration (5 min)
}
```

### 6. heijunka_worker.erl (Worker Process)
**Responsibility**: Individual worker managed by Poolboy

**Key Functions**:
```erlang
execute/2                  % Perform work
ping/1                     % Health check
```

**Lifecycle**:
```
Poolboy creates N workers per pool
Workers are stored in LIFO queue
Request checkout → worker executes work → checkin

Worker state tracks:
  - ID (unique identifier)
  - Last activity time
  - Requests processed
  - Pool membership
```

---

## Message Flow

### Request Processing Flow

```
1. Request arrives
   ↓
2. Load Regulator
   Is capacity available?
   YES → Issue token + continue
   NO → Return {error, overload}, client fails fast
   ↓
3. Load Balancer
   Route to pool using:
   - Consistent hashing (same key → same pool)
   - Check if preferred pool overloaded
   - Route to least loaded if needed
   ↓
4. Pool Manager
   Get worker from pool
   - Worker available → return immediately
   - All workers busy → wait (configurable timeout)
   ↓
5. Worker Executes
   Process the actual request
   ↓
6. Release Resources
   Return worker to pool
   Release quota token
   Update metrics
```

### Metrics Collection Flow

```
Every 5 seconds (update_metrics):
  ├─ Pool Manager updates
  │  ├─ Calculate utilization
  │  ├─ Check health status
  │  └─ Adjust worker count if needed
  │
  ├─ Balancer updates
  │  ├─ Sample all pool utilizations
  │  ├─ Calculate balance coefficient
  │  └─ Queue rebalancing if needed
  │
  ├─ Burst Handler samples
  │  └─ Add to 60-second window
  │
  └─ Emit metrics for export
     ├─ Prometheus scrape (if enabled)
     ├─ Dashboard update
     └─ Alert evaluation
```

### Scaling Decision Flow

```
Utilization → 80%
   ↓
Pool detects threshold
   ↓
Check if time since last scale > 1 second
   ↓
If true: add_worker()
   Update worker_count
   Reschedule health check
   Update last_scaling_time
   ↓
Emit metric to monitoring
   ↓
Balancer sees new worker
   ↓
Next routing request may use new worker
   ↓
Utilization gradually decreases
```

---

## Concurrency Model

### Thread Model
- **Supervisors**: One per subsystem (heijunka_sup, heijunka_pool_sup)
- **Workers**: One per pool (heijunka_pool, heijunka_load_regulator, heijunka_balancer, heijunka_burst_handler)
- **Poolboy**: N workers per pool (e.g., 10 payment workers)
- **Timers**: Multiple per component (health check, metrics, detection)

### Concurrent Operations
```
Payment Pool:
  - Requests → get_worker (concurrent, multiple in parallel)
  - Worker processes → execute (true parallelism, one per worker)
  - Returns → return_worker (concurrent)
  - Metrics → pool_status (read-only, no lock)

Load Regulator:
  - Admission → request_admission (increments counter, locked)
  - Release → release_quota (decrements counter, locked)
  - Status → regulator_status (read-only snapshot)

Balancer:
  - Routing → route_request (hash lookup, no lock)
  - Rebalancing → migrate_worker (occasional, 30s cooldown)
  - Status → balance_status (read-only snapshot)

Burst Handler:
  - Detection → sample_load (read pool metrics, no lock)
  - Surge trigger → add_workers (rare, 10x threshold)
  - Status → burst_status (read-only snapshot)
```

### Synchronization
- **ETS** not used (keeping simple for now, could optimize)
- **Maps** used for state (immutable, new state on each update)
- **gb_sets** for token tracking (ordered set, efficient)
- **Timers** for periodic actions (erlang:send_after)

---

## Failure Modes & Recovery

### Scenario 1: Pool Worker Crashes
```
Worker process dies
  ↓
Poolboy detects dead worker (connection check)
  ↓
Removes from available pool
  ↓
Next request waits or times out
  ↓
After timeout, request fails
  ↓
Client retries (with backoff)
  ↓
System recovers (workers remain stable)
```

### Scenario 2: Pool Manager Crashes
```
Pool supervisor restart
  ↓
Pool manager gen_server restarted
  ↓
Timers are restarted (erlang:send_after)
  ↓
Worker pool remains running (managed by Poolboy)
  ↓
System recovers in <1 second
```

### Scenario 3: Entire Heijunka Stops
```
Heijunka supervisor detects failure
  ↓
one_for_all strategy → restart all children
  ↓
Load regulator resets metrics
  ↓
Balancer recalculates balance
  ↓
Pools rebuild from Poolboy state
  ↓
System recovers in 2-3 seconds
```

---

## Configuration Recommendations

### Small Deployments (<1000 req/s)
```erlang
#{
    pools => [payment, delivery],
    regulator => #{
        soft_limit => 500,
        hard_limit => 600,
        reserved_capacity => 0.20
    },
    pool_config => #{
        payment => #{initial_workers => 5, max_workers => 15},
        delivery => #{initial_workers => 4, max_workers => 12}
    }
}
```

### Medium Deployments (1000-5000 req/s)
```erlang
#{
    pools => [payment, delivery, notification],
    regulator => #{
        soft_limit => 1000,
        hard_limit => 1200,
        reserved_capacity => 0.20
    },
    pool_config => #{
        payment => #{initial_workers => 10, max_workers => 30},
        delivery => #{initial_workers => 8, max_workers => 25},
        notification => #{initial_workers => 5, max_workers => 20}
    }
}
```

### Large Deployments (>5000 req/s)
```erlang
#{
    pools => [payment, delivery, notification, batch_processing],
    regulator => #{
        soft_limit => 3000,
        hard_limit => 3600,
        reserved_capacity => 0.20
    },
    pool_config => #{
        payment => #{initial_workers => 25, max_workers => 100},
        delivery => #{initial_workers => 20, max_workers => 80},
        notification => #{initial_workers => 15, max_workers => 60},
        batch => #{initial_workers => 10, max_workers => 40}
    }
}
```

---

## Performance Characteristics

### Time Complexity
```
route_request/3        O(1)   Hash lookup + utilization check
request_admission/2    O(1)   Counter increment/check
pool_status/1          O(1)   Map lookups
balance_status/0       O(n)   Calculate variance (n = pool count, typically 3-4)
get_worker/1           O(1)   Poolboy LIFO queue
return_worker/2        O(1)   Poolboy queue
scale_up/1             O(1)   Counter increment
health_check/1         O(n)   Check n workers health (n = worker count)
```

### Space Complexity
```
Load regulator: O(1)           Fixed state
Balancer:       O(n)           n = pool count (3-4 typical)
Burst handler:  O(60)          60-second sample window
Pool manager:   O(w*l)         w = worker count, l = queue size
Total:          ~O(1)          Constant regardless of scale
```

### Latency
```
request_admission:    <1μs      Token generation + counter
route_request:        <10μs     Hash + utilization check
get_worker:           1ms-100ms Depends on availability
health_check:         <100ms    Parallel pings to workers
rebalancing:          <10ms     Worker movement
```

---

**Version**: 1.0.0
**Last Updated**: 2026-01-25
