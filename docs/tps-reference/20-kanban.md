# TPS Kanban: Pull-Based Work Queue System

## Executive Summary

**Kanban** is a pull-based work distribution system inspired by Toyota's manufacturing methodology. Instead of pushing work to workers (antipattern), workers *pull* work from a queue at their own pace. This naturally implements backpressure, preventing queue buildup and worker overload.

**ggen's TPS Kanban** implements enterprise-grade Kanban patterns using:
- **NATS**: Primary message broker (lightweight, fast, ephemeral)
- **RabbitMQ**: Fallback persistence layer (durable queues, reliable delivery)
- **Workers**: Pull N items, process, ACK → repeat
- **Scheduler**: Cloud Scheduler integration for periodic work
- **Coordinator**: Auto-scaling, health monitoring, dead letter management

---

## Part 1: Kanban Philosophy

### What Is Kanban?

**Kanban** (Japanese: 看板 - "visual card/sign") is a manufacturing methodology for controlling production flow without overproduction.

#### Key Principle: Pull, Not Push

```
❌ ANTIPATTERN (Push)          ✅ PATTERN (Pull - Kanban)
┌─────────────┐                ┌─────────────┐
│  Scheduler  │ PUSH (overload)│  Scheduler  │ publishes work
└──────┬──────┘                └──────┬──────┘
       │                              │
       ├→ Worker 1 (busy!)           │
       ├→ Worker 2 (busy!)           ├→ [Queue]
       ├→ Worker 3 (queue crash)     │      ↑
       └→ Worker 4 (dropped work)    ↓      │
                          Work lost  Worker 1 PULLS
                                     Worker 2 PULLS
                                     Worker 3 PULLS
                                     (only when ready)
```

#### Benefits of Pull-Based Kanban

| Benefit | Why It Matters |
|---------|---|
| **Backpressure** | Queue depth limits work intake; prevents overload |
| **Flow Control** | Workers control pace; no dropped work |
| **Visibility** | Queue depth signals bottlenecks (Andon) |
| **Efficiency** | No idle workers waiting for pushed work |
| **Stability** | System self-regulates; prevents cascade failures |
| **Scalability** | Add workers = automatic load balancing |

### Kanban in Manufacturing

Toyota's Kanban implementation:

```
Upstream Station          Kanban Card          Downstream Worker
┌──────────────┐          (Pull Signal)        ┌────────────┐
│ Produces     │ ← ← ← ← ← ← ← ← ← ← ← ← ←  │ Consumes   │
│ Only when    │          "Make more!"        │ Only what  │
│ downstream   │                             │ is needed  │
│ pulls        │                             │            │
└──────────────┘                             └────────────┘
     (WIP limited)                           (Controls pace)
```

**Result**:
- No overproduction (manufacturing waste)
- Just-in-time delivery
- Fast flow with low inventory (WIP)
- Quality improvements through visibility

---

## Part 2: TPS Kanban Architecture

### System Overview

```
Cloud Scheduler
      │
      ├─ Schedule periodic items (e.g., every minute)
      └─ Publish to Kanban queue
           │
           ├──────────────────────┐
           ↓                      ↓
      ┌─────────────┐      ┌──────────────┐
      │    NATS     │      │  RabbitMQ    │
      │ (Primary)   │      │ (Fallback)   │
      └──────┬──────┘      └──────┬───────┘
             │                   │
    ┌────────┴───────┐          │
    │                │          │
    ↓                ↓          ↓
  kanban.high.payment          (Persistence)
  kanban.normal.fraud
  kanban.low.account
    │         │       │
    ↓         ↓       ↓
  Worker1  Worker2  Worker3
  (Pull)    (Pull)    (Pull)
    │         │       │
    ├─ Process work
    ├─ ACK (on success)
    └─ NACK + Dead Letter (on failure)
           │
           ↓
    ┌─────────────────────┐
    │  Coordinator        │
    │  - Auto-scale       │
    │  - Health monitor   │
    │  - Drain queue      │
    └─────────────────────┘
           │
           ↓
    Prometheus Metrics
```

### Core Components

#### 1. **kanban_queue** - Work Item Storage

```erlang
%% Work item format (JSON)
{
  "id": "unique-work-id",
  "priority": "high|normal|low",
  "domain": "payment|fraud|account|billing",
  "payload": {...},
  "deadline": 1234567890000,
  "created_at": 1234567880000,
  "retry_count": 0
}

%% NATS subjects (routing by priority + domain)
kanban.high.payment    → Urgent payments
kanban.normal.fraud    → Regular fraud checks
kanban.low.account     → Low-priority account updates
```

**Key Behaviors**:
- Primary: NATS pub/sub (fast, ephemeral)
- Fallback: RabbitMQ (durable, persistent)
- Automatic failover if NATS unavailable
- Metrics: published, acked, nacked, queue_depth

#### 2. **kanban_worker** - Work Processor

```erlang
%% Worker lifecycle
1. start_link(Priority, Domain)    → Subscribe to queue
2. pull_work()                     → PULL N items (default 10)
3. process(Item)                   → Work on item (with timeout)
4. ack(WorkId)                     → Success → remove from queue
5. nack(WorkId, Reason)            → Failure → retry or dead letter
6. repeat from step 2              → Continue pulling

%% Backpressure mechanism
- Worker pulls only what it can process
- Default: 10 items per pull
- If processing slow, pulls slow down
- Queue naturally accumulates backlog
- Auto-scale watches queue depth
```

**Circuit Breaker** (Jidoka - Stop the Line):
- 3 consecutive failures → circuit opens
- Stop pulling until human intervention
- Alert raised for investigation

#### 3. **kanban_scheduler** - Periodic Work

```erlang
%% Schedule periodic work
kanban_scheduler:schedule_periodic(high, payment, 60000, 55000)
  │
  ├─ Priority: high
  ├─ Domain: payment
  ├─ Interval: 60 seconds
  └─ Deadline: 55 seconds (5s buffer for SLA)

%% Every 60 seconds:
publish({high, payment, Payload, Deadline})

%% Backpressure adjustment
- Monitor queue depth
- If depth > 100: reduce scheduling rate 2x
- If depth < 10: increase scheduling rate 2x
- Prevents scheduling hammer during congestion
```

#### 4. **kanban_coordinator** - Health & Auto-Scale

```erlang
%% Health checks (every 5 seconds)
- Queue depth sampling (rolling window of 10)
- Average queue depth calculation
- Stalled item detection (>5 minutes in queue)
- Dead letter queue monitoring

%% Auto-scaling algorithm
TargetWorkers = max(2, min(20, (AvgDepth ÷ 20) + 2))

  Example:
  ├─ Depth 0-20    → 2 workers
  ├─ Depth 20-40   → 3 workers
  ├─ Depth 40-60   → 4 workers
  └─ Depth 200+    → 12 workers (max 20)

%% Graceful shutdown
- Stop accepting new work
- Wait for all items to process
- Timeout: 60s (fail safe)
```

---

## Part 3: Implementation Patterns

### Pattern 1: Basic Work Publication

```erlang
% Publish high-priority payment work with 30s deadline
Priority = high,
Domain = payment,
Payload = #{amount => 100.50, account_id => "ACC123"},
Deadline = kanban_queue:now_ms() + 30000,

{ok, WorkId} = kanban_queue:publish({Priority, Domain, Payload, Deadline}).
```

### Pattern 2: Worker Pull Loop

```erlang
%% Worker pulls and processes
{ok, Items} = kanban_worker:pull_work(WorkerPid),

lists:foreach(fun(Item) ->
    WorkId = maps:get(id, Item),

    % Process work
    case process_payment(maps:get(payload, Item)) of
        ok ->
            kanban_queue:ack(WorkId, Domain);
        {error, Reason} ->
            kanban_queue:nack(WorkId, Domain, Reason)
    end
end, Items).
```

### Pattern 3: Periodic Scheduling

```erlang
%% Schedule health check every 5 minutes
{ok, JobId} = kanban_scheduler:schedule_periodic(
    normal,           % Priority
    health_check,     % Domain
    300000,          % Interval (5 minutes)
    290000           % Deadline (5.1 minutes, 1min processing window)
).

%% Later: cancel if needed
kanban_scheduler:cancel_schedule(JobId).
```

### Pattern 4: Circuit Breaker (Jidoka)

```erlang
%% Worker detects pattern of failures
%% Circuit breaker opens after 3 consecutive failures
%% Stops pulling work, raises alert

%% Alert: "Worker payment_high circuit open - investigate root cause"
%% Example: Database connection failure, external service timeout

%% Recovery: Manual intervention or automatic reset after cooldown
kanban_worker:stop(WorkerPid),
{ok, NewPid} = kanban_worker:start_link(high, payment).
```

### Pattern 5: Dead Letter Queue

```erlang
%% Unacknowledged items after timeout
%% Automatically moved to dead letter queue
%% Example: Item published at T=0, processed at T=30s, deadline was T=20s

%% Dead letter stores:
- Original item
- Reason (deadline_exceeded, processing_timeout, explicit_nack)
- Retry count
- First failure timestamp

%% Operations can inspect:
Metrics = kanban_queue:get_metrics(),
DeadLetteredCount = maps:get(dead_lettered, Metrics).
```

---

## Part 4: Operations Guide

### Starting the System

#### Local Development (NATS + RabbitMQ)

```bash
# Start with docker-compose
docker-compose -f docker-compose.test.yml up

# In Erlang shell
rebar3 shell
> application:ensure_all_started(tps_kanban).

# Verify operational
> kanban_queue:publish({high, payment, #{}, erlang:system_time(millisecond) + 60000}).
{ok, "work_id_12345"}

> kanban_coordinator:get_health().
#{
  current_workers => 5,
  target_workers => 5,
  queue_depth_samples => [0],
  queue_draining => false,
  timestamp => 1706035200000
}
```

#### Production Deployment

```bash
# Build release
rebar3 as prod release

# Run release
./_build/prod/rel/tps_kanban/bin/tps_kanban start

# Verify
> kanban_coordinator:get_health().
```

### Monitoring Queue Health

```erlang
%% Check every 5 seconds
Health = kanban_coordinator:get_health(),

Maps = Health#{
  current_workers => integer(),        % Active workers
  target_workers => integer(),         % Auto-scaled target
  queue_depth_samples => [integer()],  % Last 10 depth readings
  queue_draining => boolean()          % Graceful shutdown active
}.

%% Prometheus metrics
kanban_queue_depth              % Current depth
kanban_current_workers          % Active worker count
kanban_queue_depth_avg          % 10-reading rolling average
kanban_latency_ms              % Work item latency
kanban_published_total         % Total items published
kanban_dead_lettered_total     % Total dead-lettered items
```

### Auto-Scaling Behavior

```
Queue Depth    Target Workers    Action
────────────   ────────────────  ─────
0-20           2                 Minimal (cost optimization)
40-60          4                 Normal load
100-120        8                 Adding workers
200+           16                Heavy load (still max 20)
```

**Example**:
```erlang
% Publish 150 items
[kanban_queue:publish({high, payment, #{}, ...}) || _ <- lists:seq(1, 150)].

% Coordinator detects queue depth ≈ 150
% Calculates: target = max(2, min(20, (150 ÷ 20) + 2)) = 9 workers

% 5 seconds later: 4 new workers started
% Log: "Scaling up workers: 5 -> 9 (queue depth: 150)"

% Workers pull items, queue drains to 20
% Coordinator calculates: target = 3
% 5 seconds later: 6 workers stopped
% Log: "Scaling down workers: 9 -> 3 (queue depth: 20)"
```

### Graceful Shutdown

```erlang
%% Initiate drain
kanban_coordinator:drain_queue().

%% System behavior:
% 1. Stop accepting new work
% 2. Wait for all in-flight work to complete
% 3. Move unacknowledged items to dead letter
% 4. Return ok (or {error, timeout} after 60s)

%% Example:
ok = kanban_coordinator:drain_queue(),
ok = kanban_coordinator:adjust_workers(0),  % Stop all workers
application:stop(tps_kanban).
```

### Handling Dead Letter Queue

```erlang
%% Items moved to dead letter queue when:
% 1. Item deadline exceeded (created > 5 minutes ago)
% 2. Processing timeout exceeded (took > deadline - created)
% 3. Explicit NACK with no retry
% 4. Unacknowledged for > 5 minutes

%% Inspect dead letter metrics
Metrics = kanban_queue:get_metrics(),
DeadLetterCount = maps:get(dead_lettered, Metrics),

lager:alert("Found ~B dead-lettered items, investigate", [DeadLetterCount]).

%% Root cause analysis:
% - Are deadlines too aggressive?
% - Are workers timing out?
% - Is there external service latency?
% - Are worker counts sufficient?
```

---

## Part 5: Troubleshooting

### Problem: Queue Depth Growing (Not Draining)

**Symptoms**:
```
kanban_queue_depth gradually increasing
kanban_current_workers = target_workers (already scaled)
```

**Investigation**:
```erlang
% Check worker health
Health = kanban_coordinator:get_health(),
CurrentWorkers = maps:get(current_workers, Health),
AvgDepth = lists:sum(maps:get(queue_depth_samples, Health)) /
           length(maps:get(queue_depth_samples, Health)).

% Investigate:
% 1. Are workers processing slower than before?
%    - Check worker latency metrics
%    - Are external services slow?
%
% 2. Are there circuit breakers open?
%    - Check kanban_circuit_breaker_open_total
%    - Investigate root cause (DB failure? Service timeout?)
%
% 3. Is deadline too aggressive?
%    - Example: 5s deadline with 10s processing = always fail
%    - Adjust deadline in scheduler or queue publication
%
% 4. Is domain misrouted?
%    - Example: payment items published to fraud domain
%    - Verify priority/domain routing
```

**Solution**:
```erlang
% Quick: Increase workers manually
kanban_coordinator:adjust_workers(20).

% Proper: Fix root cause
% 1. Increase deadline (if too aggressive)
% 2. Add database connection pool (if bottleneck)
% 3. Implement external service retry (if timeouts)
% 4. Clear circuit breaker (if persistent failures resolved)
```

### Problem: Worker Circuit Breaker Open

**Symptoms**:
```
kanban_circuit_breaker_open_total incremented
Worker logs: "Circuit breaker opened for worker payment_high"
Workers not pulling (pulling returns {error, circuit_breaker_open})
```

**Investigation**:
```erlang
% Circuit breaker opens after 3 consecutive failures
% Investigate why failures:

% Check queue metrics for NACK reasons
Metrics = kanban_queue:get_metrics(),
NackCount = maps:get(nacked, Metrics),

% Root causes:
% 1. Database unavailable → connection errors
% 2. External API timeout → service latency
% 3. Invalid data → payload processing errors
% 4. Deadline exceeded → items arriving with no time to process

% Check application logs
tail -f log/tps_kanban.log | grep "NACK\|Failed\|Error"
```

**Solution**:
```erlang
% Fix root cause first
% Then recover worker:

WorkerPid = whereis(kanban_worker_sup),  % Get supervisor
supervisor:terminate_child(WorkerPid, WorkerId),  % Stop failing worker
{ok, _} = kanban_worker_sup:start_worker(high, payment).  % Start new

% Verify metrics show new worker starting
canary_id = kanban_scheduler:schedule_canary(payment, #{}).
```

### Problem: Messages Lost

**Symptoms**:
```
Published 1000 items, but only 950 ACKed + dead-lettered
```

**Root Cause**:
```
NATS is ephemeral - if no subscribers when message published,
message is lost (not persisted).

RabbitMQ fallback ensures persistence, but only if:
1. NATS unavailable (triggers fallback)
2. Item published to RabbitMQ

If NATS available but slow subscriber, items can be lost
if subscriber disconnects before ACK.
```

**Prevention**:
```erlang
% 1. Ensure RabbitMQ is running (persistence layer)
docker-compose up -d rabbitmq nats

% 2. Monitor NATS connection health
gnat_status = gnat:status(NatsConn),

% 3. Publish with verification
{ok, WorkId} = kanban_queue:publish({high, payment, Payload, Deadline}),
% Wait for ACK from worker (separate process)
ok = wait_for_ack(WorkId, 5000).

% 4. Use RabbitMQ explicitly if critical
kanban_rabbitmq_fallback:publish({high, payment, Payload, Deadline}).
```

### Problem: Why Are Workers Idle?

**Symptoms**:
```
Queue has 100 items, but workers not pulling
kanban_current_workers = 3, but processing = 0
```

**Investigation**:
```erlang
% Check worker status
{ok, Workers} = supervisor:which_children(kanban_worker_sup),
[kanban_worker:get_status(Pid) || {_Id, Pid, _, _} <- Workers].

% Possible reasons:
% 1. Workers connected to wrong domain
%    - Worker subscribed to kanban.normal.fraud
%    - But items published to kanban.high.payment
%    - Solution: Start workers for correct domains

% 2. NATS subscribers timed out
%    - Gnat connection dropped
%    - Solution: Restart worker, check network

% 3. Circuit breaker open
%    - Workers stopped pulling after failures
%    - Solution: Fix root cause, restart workers

% 4. Queue draining
%    - Graceful shutdown in progress
%    - Solution: Wait for drain to complete
```

**Solution**:
```erlang
% Verify correct domains are covered
Health = kanban_coordinator:get_health(),
% Should have workers for: payment, fraud, account, billing

% Start missing workers
kanban_worker_sup:start_worker(high, payment),
kanban_worker_sup:start_worker(normal, fraud),
kanban_worker_sup:start_worker(normal, account),
kanban_worker_sup:start_worker(low, billing).

% Verify pulling resumes
timer:sleep(2000),
Metrics = kanban_queue:get_metrics(),
% Should see Published increasing, Acked increasing
```

### Problem: Scheduler Not Publishing

**Symptoms**:
```
scheduled_jobs list = [job1, job2, job3]
But queue metrics published_total not increasing
```

**Investigation**:
```erlang
% Check scheduler status
Jobs = kanban_scheduler:get_scheduled_jobs(),
[#{
    id => "job_123",
    priority => high,
    domain => payment,
    interval_ms => 60000,
    next_scheduled => 1706035260000,  % Upcoming execution time
    active => true
}]

% Issue 1: next_scheduled in future
% Solution: Wait, or publish canary manually

% Issue 2: Job inactive
active => false,  % Was cancelled
% Solution: Schedule new job

% Issue 3: RabbitMQ/NATS unavailable
% Solution: Check connectivity
gnat_status = gnat:status(NatsConn).
```

**Solution**:
```erlang
% Schedule new periodic job
{ok, JobId} = kanban_scheduler:schedule_periodic(
    high, payment, 60000, 55000
).

% Test with canary
ok = kanban_scheduler:schedule_canary(payment, #{test => true}).

% Verify metrics
timer:sleep(500),
Metrics = kanban_queue:get_metrics(),
maps:get(published, Metrics).  % Should be >= 1
```

---

## Part 6: Testing

### Running Test Suite

```bash
# Run all tests with real NATS/RabbitMQ
make test

# Or with docker-compose
docker-compose -f docker-compose.test.yml up --build --abort-on-container-exit

# Unit tests only (no infrastructure)
rebar3 eunit

# Common test suite (with infrastructure)
rebar3 ct
```

### Test Coverage

The test suite (`kanban_SUITE.erl`) includes:

1. **Single Worker Pull** - Basic work processing
2. **Multiple Workers Load Distribution** - Parallel processing
3. **Backpressure** - Worker pull limiting
4. **Dead Letter Queue** - Unacknowledged handling
5. **Priority Routing** - High priority first
6. **Graceful Shutdown** - Drain without losing work
7. **Stress Test** - 10,000 items (real throughput validation)
8. **Metrics Tracking** - Prometheus metric validation
9. **Circuit Breaker** - Failure pattern detection
10. **Scheduler** - Periodic job execution

All tests use **Chicago TDD pattern**:
- Real NATS/RabbitMQ (no mocks)
- State-based assertions (verify output, not implementation)
- Arrange/Act/Assert structure

---

## Part 7: Performance Characteristics

### Throughput

| Configuration | Items/sec | Queue | Workers | Notes |
|---|---|---|---|---|
| Local (NATS) | 500-1000 | In-memory | 4 | Network latency: 1-5ms |
| RabbitMQ | 100-200 | Durable | 4 | Disk writes: 5-10ms/item |
| Docker (3 services) | 300-500 | Hybrid | 4 | Container overhead |
| Production (optimized) | 1000+ | NATS | 20 | High-performance hardware |

### Latency

```
Work Item Journey:
Publish → NATS → Worker pulls → Process → ACK
└─ 1ms ─→ 2ms ─→ 50-100ms ────→ 1ms ─→

Total: 54-104ms (P50)
       100-200ms (P95)
       200-500ms (P99)
```

### Memory

Per worker: ~5-10 MB
Per 10,000 items in queue: ~50-100 MB
Total (20 workers + queue): 150-300 MB

---

## Part 8: Reference

### Configuration Options

```erlang
% tps_kanban application environment variables

{nats_servers, ["nats://localhost:4222"]},
% NATS server URLs (list for clustering)

{rabbitmq_host, "localhost"},
{rabbitmq_port, 5672},
{rabbitmq_user, "guest"},
{rabbitmq_pass, "guest"},
% RabbitMQ connection parameters

{worker_pull_size, 10},
% Items per pull (backpressure mechanism)

{worker_process_timeout_ms, 30000},
% Max time for process (deadline buffer: 5s)

{dead_letter_timeout_ms, 300000},
% Items > 5 minutes unacked → dead letter

{coordinator_check_interval_ms, 5000},
% Health check frequency

{stalled_item_threshold_ms, 300000},
% Items > 5 minutes in queue considered stalled
```

### API Reference

```erlang
% Publishing
kanban_queue:publish({Priority, Domain, Payload, Deadline})
  → {ok, WorkId} | {error, Reason}

% Worker Management
kanban_worker:start_link(Priority, Domain)
  → {ok, Pid} | {error, Reason}

kanban_worker:pull_work(Pid)
  → {ok, [Item]} | {error, Reason}

kanban_worker:get_status(Pid)
  → #{worker_id => string(), ...}

kanban_worker:stop(Pid)
  → ok | {error, Reason}

% Acknowledgment
kanban_queue:ack(WorkId, Domain)
  → ok | {error, Reason}

kanban_queue:nack(WorkId, Domain, Reason)
  → ok | {error, Reason}

% Scheduling
kanban_scheduler:schedule_periodic(Priority, Domain, IntervalMs, DeadlineMs)
  → {ok, JobId} | {error, Reason}

kanban_scheduler:schedule_canary(Domain, Payload)
  → ok | {error, Reason}

kanban_scheduler:cancel_schedule(JobId)
  → ok | {error, Reason}

% Coordination
kanban_coordinator:get_health()
  → #{current_workers => int(), ...}

kanban_coordinator:drain_queue()
  → ok | {error, timeout}

kanban_coordinator:adjust_workers(Count)
  → ok | {error, Reason}

% Metrics
kanban_queue:get_metrics()
  → #{published => int(), acked => int(), ...}
```

---

## Conclusion

**Kanban** transforms work distribution from push (antipattern) to pull (pattern):

- ✅ Workers control pace (backpressure)
- ✅ Queue visibility signals bottlenecks (Andon)
- ✅ Auto-scaling based on demand (coordinator)
- ✅ Circuit breaker stops line on failures (Jidoka)
- ✅ Dead letter queue prevents work loss (reliability)
- ✅ Graceful shutdown drains queue (operational safety)

Implement Kanban in your work systems to achieve **flow, visibility, and stability**.
