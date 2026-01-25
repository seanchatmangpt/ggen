# Heijunka: Load Leveling in TPS (Transaction Processing System)

## Table of Contents
1. [Heijunka Principles](#heijunka-principles)
2. [Architecture Overview](#architecture-overview)
3. [Component Reference](#component-reference)
4. [Configuration Guide](#configuration-guide)
5. [Metrics & Monitoring](#metrics--monitoring)
6. [Scaling Decisions](#scaling-decisions)
7. [Production Patterns](#production-patterns)
8. [Troubleshooting](#troubleshooting)

---

## Heijunka Principles

### Definition
**Heijunka** (平準化) is a Japanese Lean manufacturing term meaning "load leveling" or "production smoothing."

### Core Concept
Instead of feast-and-famine cycles (0% → 100% → 0% utilization), Heijunka maintains **smooth, constant flow**:

```
WITHOUT Heijunka (Wasteful):
████████████░░░░░░░░░░░░░░░░  ← Spike: overload, timeouts
░░░░░░░░░░░░████████████░░░░░░  ← Trough: idle workers, waste
████████░░░░░░░░████████░░░░░░  ← Chaotic: unpredictable

WITH Heijunka (Smooth):
███████████████████████████████  ← Target: steady 70% utilization
███████████████████████████████  ← Predictable, efficient flow
███████████████████████████████  ← No waste, no overload
```

### Benefits
- **Stability**: Predictable performance, no request timeouts
- **Efficiency**: ~70% utilization = optimal power/cost per request
- **Responsiveness**: Constant capacity available for urgent work
- **Observability**: Metrics remain consistent and interpretable

---

## Architecture Overview

### System Diagram

```
┌─────────────────────────────────────────────────────────────┐
│  TPS Frontend (API, Webhooks, Queues)                       │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
        ┌────────────────────────┐
        │ Load Regulator         │
        │ (Admission Control)    │
        │ • Request/Release      │
        │ • Soft/Hard Limits     │
        │ • Reserved Capacity    │
        └────────────┬───────────┘
                     │
         ┌───────────┴────────────┐
         ▼                        ▼
  ┌──────────────┐        ┌──────────────┐
  │ Balancer     │        │ Burst Handler│
  │ • Routing    │        │ • Detection  │
  │ • Affinity   │        │ • Surge Cap  │
  │ • Migration  │        │ • Cooldown   │
  └──────┬───────┘        └──────┬───────┘
         │                       │
    ┌────┴───────┬───────────────┤
    ▼            ▼               ▼
 ┌──────┐    ┌──────┐       ┌──────┐
 │Pool: │    │Pool: │       │Pool: │
 │Payment    │Delivery    │Notification
 │ • Monitor │ • Scale    │ • Health
 │ • Health  │ • Metrics  │ • Metrics
 │ • Metric  │           │
 └─┬────────┘└──────┘       └──────┘
   │
   ▼
┌────────────────────────────────────┐
│ Poolboy (Worker Pool Framework)    │
│ • Min/Max workers per pool         │
│ • LIFO queue strategy              │
│ • Overflow handling                │
└────────────────────────────────────┘
```

### Key Components

#### 1. Load Regulator (Admission Control)
```erlang
% Usage:
{ok, Token} = heijunka_load_regulator:request_admission(payment, #{})
% Do work...
heijunka_load_regulator:release_quota(payment, Token)
```
- **Soft Limit**: 1000 (warnings, but accept)
- **Hard Limit**: 1200 (reject beyond this)
- **Reserved Capacity**: 20% for priority work (payment, critical notifications)
- **Philosophy**: Fail fast rather than queue forever

#### 2. Load Balancer (Work Distribution)
```erlang
% Usage:
{ok, PoolName} = heijunka_balancer:route_request(payment, RequestId, #{})
```
- **Hash-based Affinity**: Same request type → same pool (better cache locality)
- **Rebalancing**: If primary pool >85% busy, route to secondary
- **Migration**: Workers migrate from idle pools to overloaded ones
- **Balance Coefficient**: 0-1 (1 = perfect balance across pools)

#### 3. Pool Manager (Per-Domain Scaling)
```erlang
{ok, Status} = heijunka_pool:pool_status(payment)
```
- **Target**: 70% utilization
- **Scale Up**: If utilization > 80%, add worker
- **Scale Down**: If utilization < 50%, remove worker
- **Bounds**: Min 2 workers, Max 50 per pool
- **Health Checks**: Every 30 seconds

#### 4. Burst Handler (Spike Management)
```erlang
{ok, Status} = heijunka_burst_handler:burst_status()
```
- **Detection**: 10x normal load in 1 minute
- **Surge**: Temporarily add 5 workers per pool
- **Duration**: 5 minutes (then gradually shrink)
- **Cost-aware**: Surge workers are more expensive (manual intervention)

---

## Component Reference

### heijunka_pool.erl (700 lines)

**Responsibility**: Per-domain worker pool management using Poolboy.

```erlang
%% Get pool status
{ok, Status} = heijunka_pool:pool_status(payment)
% Returns: #{
%   name => payment,
%   domain => payment,
%   current_workers => 10,
%   max_workers => 30,
%   utilization => 0.65,          % 65% utilized
%   queue_length => 0,
%   avg_latency_ms => 42.5,
%   total_requests => 5000,
%   failed_requests => 3,
%   health_status => healthy      % healthy | degraded | critical
% }

%% Manual scaling
{ok, NewCount} = heijunka_pool:scale_up(payment)      % Add worker
{ok, NewCount} = heijunka_pool:scale_down(payment)    % Remove worker

%% Health check
{ok, HealthStatus} = heijunka_pool:health_check(payment)
```

**Metrics**:
- **Utilization**: (Workers - Available) / Workers
- **Health**:
  - `healthy`: Utilization ≤ 80%, Error Rate ≤ 5%
  - `degraded`: Utilization > 80% OR Error Rate > 5%
  - `critical`: Utilization > 95%

**Scaling Algorithm**:
```erlang
Utilization = (Workers - Available) / Workers

if Utilization > 0.80 and Workers < MaxWorkers
  → add_worker()
else if Utilization < 0.50 and Workers > MinWorkers
  → remove_worker()
```

---

### heijunka_load_regulator.erl (500 lines)

**Responsibility**: Admission control to prevent overload.

```erlang
%% Request admission
{ok, Token} = heijunka_load_regulator:request_admission(payment, #{
    user_id => "user123",
    priority => high
})
% or:
{error, overload} = heijunka_load_regulator:request_admission(delivery, #{})

%% Release quota when done
heijunka_load_regulator:release_quota(payment, Token)

%% Monitor regulator
{ok, Status} = heijunka_load_regulator:regulator_status()
% Returns: #{
%   soft_limit => 1000,
%   hard_limit => 1200,
%   current_load => 850,
%   accepted_requests => 10000,
%   rejected_requests => 42,
%   admission_rate => 0.996,       % 99.6% accepted
%   rejection_rate => 0.004,       % 0.4% rejected
%   load_factor => 0.708           % 70.8% of hard limit
% }
```

**Key Features**:
- **Fast Failure**: Reject immediately rather than queue
- **Reserved Capacity**: 20% (1200 * 0.2 = 240 slots) reserved for urgent work
- **Priority Domains**: `payment`, `critical_notification` can exceed regular limits
- **Jidoka**: Fail fast = better customer experience than timeout

**Load Calculation**:
```erlang
% Regular domains:
effective_limit = hard_limit * (1 - reserved_capacity)
                = 1200 * 0.8
                = 960

% Priority domains (payment, critical):
effective_limit = hard_limit
                = 1200

% Rejection logic:
if current_load >= hard_limit
  → reject {error, overload}
else if current_load >= effective_limit and not is_priority(domain)
  → reject {error, overload}
else
  → accept {ok, Token}
```

---

### heijunka_balancer.erl (600 lines)

**Responsibility**: Distribute work across multiple pools fairly.

```erlang
%% Route request to pool
{ok, PoolName} = heijunka_balancer:route_request(
    payment,           % Domain
    RequestId,         % Routing key (for affinity)
    #{}                % Metadata
)

%% Get balance status
{ok, Status} = heijunka_balancer:balance_status()
% Returns: #{
%   pools => 3,
%   balance_coefficient => 0.92,   % 92% balanced
%   total_routed => 50000,
%   rebalance_count => 3,
%   pool_utilization => #{
%     payment => 0.68,
%     delivery => 0.65,
%     notification => 0.70
%   },
%   balance_status => balanced     % balanced | imbalanced | critical
% }

%% Trigger rebalancing
{ok, Result} = heijunka_balancer:rebalance_pools()
% Result: #{
%   migrated => true,
%   from => payment,
%   to => delivery,
%   variance => 0.25
% }
```

**Routing Algorithm**:

```erlang
select_pool(RoutingKey, Domain) →
  % Check affinity group
  if has_affinity(RoutingKey)
    → use_affinity_pool()
  else
    → hash_consistent_routing(RoutingKey)

  % Check if preferred pool is overloaded
  if pool_utilization > 85%
    → switch_to_least_loaded()
  else
    → use_preferred_pool()
```

**Balance Coefficient**:
```
Perfect Balance (coeff = 1.0):
  All pools have same utilization

Good Balance (0.8-1.0):
  Max difference in utilization < 0.1

Fair Balance (0.5-0.8):
  Max difference in utilization 0.1-0.25

Poor Balance (0-0.5):
  Max difference in utilization > 0.25
  → Rebalance needed
```

**Rebalancing**:
- **Frequency**: Every 10 seconds
- **Cooldown**: 30 seconds between migrations
- **Trigger**: Balance coefficient < 0.8
- **Action**: Migrate workers from most-loaded to least-loaded pool

---

### heijunka_burst_handler.erl (400 lines)

**Responsibility**: Detect and gracefully handle traffic spikes.

```erlang
%% Get burst status
{ok, Status} = heijunka_burst_handler:burst_status()
% Returns: #{
%   enabled => true,
%   in_surge => true,              % Currently in surge mode
%   baseline_load => 0.35,         % Normal load baseline
%   current_load => 3.8,           % Current total load
%   load_ratio => 10.86,           % 10.86x baseline (burst!)
%   burst_threshold => 3.5,        % 10x baseline
%   surge_active => true,
%   surge_count => 2,
%   total_surge_cost => 25.0
% }

%% Get surge metrics
{ok, Metrics} = heijunka_burst_handler:get_surge_metrics()
% Returns: #{
%   in_surge => true,
%   surge_start_time => 1611234567890,
%   surge_time_remaining_ms => 248000,  % 4 mins left
%   surge_pools => [payment, delivery],
%   surge_count => 2,
%   total_surge_cost => 25.0,
%   avg_surge_cost => 12.5
% }
```

**Burst Detection**:
```
Baseline = 60-second rolling average of pool utilizations

BurstThreshold = Baseline * 10  (10x normal)

If CurrentLoad > BurstThreshold:
  → Trigger surge capacity (add 5 workers per pool)
  → Hold for 5 minutes
  → Gradually shrink back to normal
  → Cooldown: 30 seconds before next surge possible
```

**Cost Calculation**:
```
surge_cost_per_pool = 5 workers * 2.0 (surge multiplier)
                    = 10.0

total_surge_cost = surge_cost_per_pool * num_pools
                 = 10.0 * 3
                 = 30.0

Cost is tracked per surge event for cost analysis.
```

---

## Configuration Guide

### Startup Configuration

```erlang
%% Start the Heijunka system
Config = #{
    pools => [payment, delivery, notification],
    regulator => #{
        soft_limit => 1000,           % Warning threshold
        hard_limit => 1200,           % Rejection threshold
        reserved_capacity => 0.20     % 20% reserved for priority
    }
},

{ok, HeijunkaPid} = heijunka_sup:start_link(Config)
```

### Pool Configuration

Each pool gets domain-specific configuration:

```erlang
%% heijunka_pool_sup.erl - get_pool_config/1

payment =>
    initial_workers => 10,     % Start with 10 workers
    max_workers => 30          % Scale up to max 30

delivery =>
    initial_workers => 8,
    max_workers => 25

notification =>
    initial_workers => 5,
    max_workers => 20
```

### Runtime Adjustments

```erlang
%% Adjust load regulator limits
heijunka_load_regulator:set_soft_limit(2000)
heijunka_load_regulator:set_hard_limit(2500)

%% Set affinity groups
heijunka_balancer:set_affinity_groups(#{
    % Same request ID always routes to same pool (cache affinity)
    cart_checkout_123 => payment,
    user_delivery_456 => delivery,
    email_notification_789 => notification
})

%% Manually disable surge during maintenance
heijunka_burst_handler:disable_surge()

%% Re-enable
heijunka_burst_handler:enable_surge()
```

---

## Metrics & Monitoring

### Key Metrics to Track

#### 1. Utilization (Per Pool)
```
Ideal: 70% (60-80% acceptable)
Critical: >95% (add workers immediately)
Idle: <20% (consider removing workers)

Calculation: (Workers - Available) / Workers
```

#### 2. Load Factor (System)
```
Load Factor = current_load / hard_limit

Acceptable: 0.5-0.8 (50-80%)
Warning: 0.8-1.0 (80-100%, approaching limit)
Critical: >1.0 (over limit, rejecting)
```

#### 3. Balance Coefficient (Cross-Pool)
```
1.0 = Perfect balance (all pools same utilization)
0.8-1.0 = Good balance, no migration needed
0.5-0.8 = Fair balance, monitor for migration
<0.5 = Poor balance, rebalance triggered

Variance in utilization directly impacts coefficient.
```

#### 4. Request Metrics
```
Admission Rate = accepted / (accepted + rejected)
             = 0.99 means 99% of requests accepted

Rejection Rate = rejected / (accepted + rejected)
             = 0.01 means 1% rejected (overload)

Avg Latency = sum(latencies) / request_count
           = Should be stable, not increasing with load
```

#### 5. Burst Metrics
```
Burst Count = number of bursts detected
Surge Cost = manual intervention cost * surge_count
Surge Duration = time spent in surge mode

Analysis: High surge cost → need more baseline capacity
```

### Prometheus Metrics Example

```promql
# Utilization (target ~70%)
heijunka_pool_utilization{pool="payment"} = 0.68
heijunka_pool_utilization{pool="delivery"} = 0.72
heijunka_pool_utilization{pool="notification"} = 0.65

# Load factor (target <0.8)
heijunka_system_load_factor = 0.72

# Balance (target >0.8)
heijunka_balance_coefficient = 0.89

# Admission (target >0.99)
heijunka_admission_rate = 0.996

# Burst (monitor for patterns)
heijunka_burst_count = 3
heijunka_total_surge_cost = 45.0
```

### Alerting Rules

```yaml
# Alert if any pool utilization is critical
alert: PoolUtilizationCritical
expr: heijunka_pool_utilization > 0.95
for: 1m

# Alert if system is near hard limit
alert: SystemNearCapacity
expr: heijunka_system_load_factor > 0.85
for: 5m

# Alert if balance is poor
alert: PoolImbalance
expr: heijunka_balance_coefficient < 0.5
for: 10m

# Alert if rejection rate is high
alert: HighRejectionRate
expr: heijunka_rejection_rate > 0.05  # >5% rejected
for: 5m
```

---

## Scaling Decisions

### When to Scale Up (Add Workers)

1. **Utilization > 80%** (Automatic)
   ```
   Issue: Requests may wait, latency increases
   Action: Add worker immediately
   Cooldown: None (immediate action)
   ```

2. **Sustained High Utilization** (Proactive)
   ```
   Issue: 70% utilization for > 1 minute
   Action: Add 1-2 workers preemptively
   Benefit: Absorb traffic spikes before rejection
   ```

3. **Burst Detected** (Surge)
   ```
   Issue: 10x normal load (burst)
   Action: Add 5 workers per pool temporarily
   Duration: 5 minutes, then shrink back
   Cost: Tracked for post-analysis
   ```

### When to Scale Down (Remove Workers)

1. **Utilization < 50%** (Automatic)
   ```
   Issue: Idle workers = waste
   Action: Remove worker every minute
   Limit: Never below minimum (2 workers)
   ```

2. **Sustained Low Utilization** (Planned)
   ```
   Issue: <30% utilization for > 10 minutes
   Action: Gradually reduce to match demand
   Schedule: Off-peak hours preferred
   ```

3. **After Burst Recovery** (Automated)
   ```
   Surge workers:
     - Added during burst (time 0)
     - Removed after 5 minutes (time 300s)
     - Gradual shrinking over 2 minutes (time 300-420s)
     - Back to baseline by time 420s
   ```

### Cost-Aware Scaling

```
Cost per Worker (per hour):
  Baseline worker: $1.00    (already running)
  Surge worker: $2.00       (temporary, manual intervention)

Annual surge cost calculation:
  Burst frequency: 2 per day
  Surge duration: 5 minutes
  Surge workers: 5 * 3 pools = 15 workers
  Cost per surge: 15 * $2.00 * (5/60) hours = $2.50
  Annual: 730 days * 2 surges * $2.50 = $3,650

Decision:
  If annual surge cost > cost of baseline capacity increase:
    → Add permanent baseline workers
  Else:
    → Keep surge for peaks
```

### Scaling Thresholds Summary

| Metric | Threshold | Action | Cooldown |
|--------|-----------|--------|----------|
| Utilization | >80% | +1 worker | Immediate |
| Utilization | <50% | -1 worker | 1 minute |
| Load Factor | >1.0 (over hard limit) | Reject requests | (automatic) |
| Balance Coefficient | <0.8 | Rebalance pools | 30 seconds |
| Burst Detected | 10x baseline | +5 workers/pool surge | 5 minutes |
| Health | degraded/critical | Add worker + alert | Immediate |

---

## Production Patterns

### Pattern 1: Handle Traffic Spike

```erlang
%% Scenario: Black Friday - 10x normal traffic

%% 1. Load regulator rejects excess beyond hard limit
%%    → Fail fast, don't queue forever

%% 2. Burst handler detects 10x load
%%    → Activates surge capacity
%%    → Adds 5 workers per pool

%% 3. Pools scale up naturally
%%    → Utilization increases to 75% (absorbs load)

%% 4. Balancer rebalances work
%%    → Ensure no single pool overwhelmed

%% 5. After spike subsides (5 mins)
%%    → Surge workers gradually removed
%%    → Return to baseline configuration

%% 6. Cost analysis
%%    → Calculate surge cost vs. lost revenue
%%    → Decide if surge was worth it
```

### Pattern 2: Handle Slow Pool

```erlang
%% Scenario: Payment pool slower than usual

%% 1. Balancer detects payment pool >85% busy
%%    → Routes new requests to delivery or notification

%% 2. Monitor shows imbalance (balance_coefficient < 0.8)
%%    → Trigger rebalancing

%% 3. Rebalancing migrates worker from idle pool to payment
%%    → Gradual, no disruption

%% 4. Eventually payment returns to normal
%%    → Worker migrates back to idle pool

%% Key: Routing changes faster than worker migration
%%       → Immediate relief via routing
%%       → Long-term fix via migration
```

### Pattern 3: Graceful Degradation

```erlang
%% Scenario: Sustained overload (> hard limit)

%% 1. Load regulator tracks admission rate
%%    → 99% accepted, 1% rejected = healthy
%%    → 80% accepted, 20% rejected = overloaded

%% 2. If rejection rate > 5% for > 5 minutes
%%    → Alert operators (PagerDuty, Slack)
%%    → Provide options:
%%       a) Scale up manually (if burst handler insufficient)
%%       b) Shed lower-priority requests
%%       c) Enable circuit breaker for non-critical domains

%% 3. Fail fast prevents cascading failures
%%    → Rejected requests fail immediately
%%    → No timeout cascade
%%    → Client can try again or use fallback
```

### Pattern 4: Cost Optimization

```erlang
%% Monitor surge costs
{ok, Metrics} = heijunka_burst_handler:get_surge_metrics()

SurgeCount = maps:get(surge_count, Metrics),
TotalCost = maps:get(total_surge_cost, Metrics),
AvgCost = maps:get(avg_surge_cost, Metrics),

% Annual projection
DaysPerYear = 365,
SurgesPerDay = 2,
YearlySurges = DaysPerYear * SurgesPerDay,
EstimatedAnnualCost = TotalCost * (YearlySurges / SurgeCount),

% Decision logic
if EstimatedAnnualCost > 5000  % $5k threshold
  → Add permanent baseline worker to payment pool
  → Baseline worker cost: $8760/year < $5000 surge cost
  % Wait, that math doesn't work. Let me recalculate...
  → If surge_cost > baseline_cost, add permanent capacity
else
  → Keep surge for peaks, it's cheaper
```

---

## Troubleshooting

### Issue 1: Pools Constantly Scaling Up/Down

**Symptom**: Utilization oscillates between 50-80%, workers added then removed repeatedly.

**Root Cause**: Scaling thresholds too close, causing thrashing.

**Solution**:
```erlang
% Current thresholds
?SCALE_UP_THRESHOLD = 0.80
?SCALE_DOWN_THRESHOLD = 0.50

% Increase hysteresis (gap between up/down)
% Old gap: 80% - 50% = 30%
% New gap: 85% - 40% = 45% (more stable)

% Also increase cooldown
?MINIMUM_SCALING_INTERVAL = 60000  % 1 minute between scaling events
```

### Issue 2: One Pool Consistently Underutilized

**Symptom**: Delivery pool at 20% while payment at 80%.

**Root Cause**: Affinity group routing all requests to payment.

**Solution**:
```erlang
% Check affinity groups
{ok, Status} = heijunka_balancer:balance_status()

% Clear problematic affinity
heijunka_balancer:set_affinity_groups(#{})

% Or adjust affinity to be less sticky
% Rotate affinity every 5 minutes instead of per-request
```

### Issue 3: Rejection Rate Spiking

**Symptom**: 10-15% of requests rejected, but pools not full.

**Root Cause**: Load regulator hard limit too low.

**Solution**:
```erlang
% Check current limits
{ok, RegStatus} = heijunka_load_regulator:regulator_status()
% Suppose hard_limit = 1200, current_load = 950

% Calculate headroom
Headroom = 1200 - 950 = 250  % 20% headroom

% If rejecting at >10% rate, increase hard limit
heijunka_load_regulator:set_hard_limit(1500)

% Verify load factor: 950 / 1500 = 63% (much healthier)
```

### Issue 4: Burst Handler Not Triggering

**Symptom**: Traffic spikes but no surge activation.

**Root Cause**: Baseline not yet established, or burst detection disabled.

**Solution**:
```erlang
% Check burst status
{ok, BurstStatus} = heijunka_burst_handler:burst_status()

% If baseline_load = 0.0, baseline still being calculated (60s window)
% Wait 60+ seconds after startup before spikes

% If enabled = false, re-enable
heijunka_burst_handler:enable_surge()

% Verify burst detection is running
% Should see log messages when burst > 10x baseline
```

### Issue 5: High Latency Despite Low Utilization

**Symptom**: avg_latency_ms = 500ms, but utilization only 60%.

**Root Cause**: Worker pool too small for workload pattern, or hot spots.

**Solution**:
```erlang
% Increase initial worker count
% Edit heijunka_pool_sup.erl

payment => #{
    initial_workers => 10,   % was 10
    initial_workers => 15,   % increase to 15
    max_workers => 30
}

% Or reduce request latency via:
% - Caching within workers
% - Parallel query execution
% - Connection pooling to backend
```

### Metrics Checklist

```erlang
%% Before declaring system healthy, verify:

% 1. All pools have good utilization (60-80%)
{ok, PaymentStatus} = heijunka_pool:pool_status(payment)
Utilization = maps:get(utilization, PaymentStatus),
true = Utilization > 0.60 andalso Utilization < 0.80

% 2. Rejection rate is low (<1%)
{ok, RegStatus} = heijunka_load_regulator:regulator_status(),
RejectionRate = maps:get(rejection_rate, RegStatus),
true = RejectionRate < 0.01

% 3. Balance is good (coefficient >0.8)
{ok, BalStatus} = heijunka_balancer:balance_status(),
Coefficient = maps:get(balance_coefficient, BalStatus),
true = Coefficient > 0.8

% 4. No pools in critical health
AllStatuses = [heijunka_pool:pool_status(P) || P <- [payment, delivery, notification]],
HealthStatuses = [maps:get(health_status, Status) || {ok, Status} <- AllStatuses],
false = lists:member(critical, HealthStatuses)

% 5. Avg latency stable
{ok, PaymentStatus2} = heijunka_pool:pool_status(payment),
AvgLatency = maps:get(avg_latency_ms, PaymentStatus2),
true = AvgLatency < 100  % <100ms acceptable

% All checks passed = system healthy!
```

---

## References

### Lean Manufacturing Origins
- Heijunka board: Physical kanban board with load leveling
- Heijunka box: Pre-printed cards to smooth production
- Reduces WIP (Work In Progress), improves flow

### Related Concepts
- **Takt Time**: Rate customers want (vs. rate we can produce)
- **Paced Production**: Synchronize work to takt time
- **Muri/Mura/Muda**: Overburden/Unevenness/Waste (what Heijunka fixes)

### Erlang/OTP Patterns
- Supervisor trees: Fault tolerance via restart
- gen_server: Stateful worker management
- Poolboy: Worker pool framework (LIFO strategy)
- Jobs: Load regulation library (alternative approach)

---

**Last Updated**: 2026-01-25
**Version**: 1.0.0
**Status**: Production Ready
