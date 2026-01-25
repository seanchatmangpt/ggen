# TPS Kanban Architecture

## System Design Overview

```
┌─────────────────────────────────────────────────────────┐
│                  External Systems                        │
│  (Cloud Scheduler, Application Servers, Webhooks)       │
└────────────────────┬────────────────────────────────────┘
                     │ publish work items
                     ↓
        ┌────────────────────────────┐
        │   Kanban Queue Layer       │
        ├────────────────────────────┤
        │ Primary: NATS              │
        │ Fallback: RabbitMQ         │
        │ Subjects:                  │
        │  kanban.high.payment       │
        │  kanban.normal.fraud       │
        │  kanban.low.account        │
        └────────────┬───────────────┘
                     │
        ┌────────────┴──────────┬──────────────┐
        ↓                       ↓              ↓
    ┌────────┐            ┌────────┐     ┌────────┐
    │Worker1 │            │Worker2 │     │Worker3 │
    │┌──────┐│            │┌──────┐│     │┌──────┐│
    ││ Pull ││ ─Pull─→   ││ Pull ││ ──→ ││ Pull ││
    ││ Work ││ (N items) ││ Work ││     ││ Work ││
    ││ ACK  ││            ││ ACK  ││     ││ ACK  ││
    │└──────┘│            │└──────┘│     │└──────┘│
    └────┬───┘            └────┬───┘     └────┬───┘
         │                     │              │
         └─────────────┬───────┴──────────────┘
                       ↓
        ┌──────────────────────────┐
        │  Coordinator             │
        ├──────────────────────────┤
        │ • Health monitoring      │
        │ • Auto-scaling           │
        │ • Dead letter handling   │
        │ • Graceful shutdown      │
        └──────────────┬───────────┘
                       │
        ┌──────────────┴──────────────┐
        │                             │
        ↓                             ↓
    Prometheus Metrics           System Logs
    (published, acked,           (INFO, WARN,
     queue_depth, latency)       ERROR, ALERT)
```

## Component Architecture

### 1. Kanban Queue (kanban_queue.erl)

**Purpose**: Publish and subscribe work items with priority routing

**Architecture**:
```
Input:  {Priority, Domain, Payload, Deadline}
         ↓
      Format to JSON with metadata
         ↓
      NATS pub/sub
      Subject: kanban.{priority}.{domain}
         ↓
Output: {ok, WorkId} | {error, Reason}
```

**State**:
```erlang
#state{
  nats_conn: pid(),           % NATS connection
  rabbitmq_conn: pid(),       % RabbitMQ fallback
  use_nats: boolean(),        % Primary or fallback
  pending_acks: #{WorkId => {Ref, Pid}},
  metrics: #{
    published => integer(),
    acked => integer(),
    nacked => integer(),
    dead_lettered => integer()
  }
}
```

**Key Behaviors**:
- Publish: JSON encode → NATS pub → fallback to RabbitMQ
- ACK: Decrement pending count, update metrics
- NACK: Increment retry count, optionally move to dead letter
- Metrics: Real-time Prometheus push

**Synchronization**:
- NATS subscription is fire-and-forget
- RabbitMQ subscriptions are per-consumer with QoS
- No central state synchronization (stateless)

### 2. Kanban Worker (kanban_worker.erl)

**Purpose**: Pull and process work items with deadline awareness

**Lifecycle**:
```
init() → subscribe(queue) → pull_work loop
  ↓                             ↓
  │                         pull N items
  │                             ↓
  │                         process(item)
  │                             ↓
  │                         ACK/NACK
  │                             ↓
  └←←←←←←←←←←←← repeat ←←←←←←←←←
```

**State**:
```erlang
#state{
  worker_id: string(),
  priority: atom(),
  domain: atom(),
  pull_size: integer(),
  process_timeout: integer(),
  queue_server: atom(),
  processing_count: integer(),
  failed_count: integer(),
  success_count: integer(),
  circuit_breaker_open: boolean(),
  metrics: #{}
}
```

**Pull Mechanism**:
```
1. Pull up to N items from queue
2. For each item:
   - Check deadline (if exceeded, NACK immediately)
   - Spawn async processor with timeout
   - Wait for result
   - ACK on success, NACK on failure
3. Track failures → open circuit breaker on 3+ consecutive
4. Repeat
```

**Circuit Breaker**:
```
FailCount = 0
  ├─ Success → FailCount = 0
  └─ Failure → FailCount++
      ├─ FailCount < 3 → retry
      └─ FailCount >= 3 → circuit opens
          └─ Stop pulling, alert monitoring
          └─ (manual intervention required)
```

**Backpressure**:
- Worker pulls only what it can process
- If processing slow, pulls slow down
- Queue accumulates items (visible as queue depth)
- Coordinator detects and scales workers

### 3. Kanban Scheduler (kanban_scheduler.erl)

**Purpose**: Publish work items at regular intervals with backpressure awareness

**Architecture**:
```
Schedule periodic job
         ↓
  Timer fires (every interval)
         ↓
  Check queue depth
         ↓
  Adjust scheduling rate
  (reduce if queue > 100)
         ↓
  Publish work item
         ↓
  Next cycle
```

**State**:
```erlang
#state{
  scheduled_jobs: #{JobId => #scheduled_job{}},
  queue_server: atom(),
  rate_limiter: #{Domain => Interval}
}
```

**Scheduled Job Record**:
```erlang
#scheduled_job{
  id: string(),
  priority: atom(),
  domain: atom(),
  interval_ms: integer(),
  last_scheduled: integer(),
  next_scheduled: integer(),
  payload: map(),
  deadline_ms: integer(),
  active: boolean()
}
```

**Rate Limiting**:
```
Queue Depth > 100
  → Reduce rate (interval *= 2)
  → Prevent flooding during congestion

Queue Depth < 10
  → Increase rate (interval /= 2)
  → Resume normal operations
```

**Canary Deployment**:
```
schedule_canary(Domain, Payload)
  ↓
  Publish 1 high-priority test item
  ↓
  Monitor success
  ↓
  If success: continue scheduling
  If failure: pause scheduling (circuit breaker)
```

### 4. Kanban Coordinator (kanban_coordinator.erl)

**Purpose**: Monitor health, auto-scale workers, handle graceful shutdown

**Architecture**:
```
Every 5 seconds:

1. Sample queue depth
2. Calculate rolling average (window of 10)
3. Auto-scale workers:
   TargetWorkers = max(2, min(20, (AvgDepth÷20)+2))
4. Detect stalled items (>5 min in queue)
5. Check for dead letter items
6. Publish health metrics
```

**State**:
```erlang
#state{
  queue_server: atom(),
  worker_sup: atom(),
  current_workers: integer(),
  target_workers: integer(),
  queue_depth_samples: [integer()],  % Last 10
  last_health_check: integer(),
  queue_draining: boolean()
}
```

**Auto-Scaling Algorithm**:
```
AvgDepth = sum(samples) / len(samples)

TargetWorkers = max(
  2,                          % Minimum
  min(
    20,                       % Maximum
    (AvgDepth ÷ 20) + 2      % Scale formula
  )
)

Action:
  ├─ TargetWorkers > CurrentWorkers → Start workers
  ├─ TargetWorkers < CurrentWorkers → Stop workers
  └─ Otherwise → No change
```

**Graceful Shutdown**:
```
drain_queue()
  ↓
Set queue_draining = true
  ↓
Stop accepting new work
  ↓
Wait for all in-flight items to complete
  ↓
Timeout after 60 seconds
  ↓
Return ok | {error, timeout}
```

**Health Metrics**:
```
- kanban_queue_depth (gauge)
- kanban_current_workers (gauge)
- kanban_target_workers (gauge)
- kanban_queue_depth_avg (gauge)
- kanban_stalled_items (gauge)
- kanban_circuit_breaker_open (counter)
```

### 5. RabbitMQ Fallback (kanban_rabbitmq_fallback.erl)

**Purpose**: Provide durable persistence when NATS unavailable

**Architecture**:
```
NATS Primary
  ↓
Is NATS available?
  ├─ YES → Use NATS (fast, ephemeral)
  └─ NO → Fallback to RabbitMQ (durable, reliable)

RabbitMQ Setup:
  ├─ Exchange: kanban.{priority}.{domain}
  ├─ Queue: kanban_queue_{priority}_{domain}
  ├─ Binding: Exchange → Queue
  └─ Durability: true (survive server restart)
```

**State**:
```erlang
#state{
  connection: pid(),           % AMQP connection
  channel: pid(),              % AMQP channel
  subscribers: #{...},        % Active subscribers
  metrics: #{...}
}
```

**Message Properties**:
```
delivery_mode = 2  % Persistent (saved to disk)
expiration = deadline - now_ms()  % Auto-discard old messages
content_type = "application/json"
```

**Dead Letter Setup**:
```
Queue arguments:
  x-dead-letter-exchange: "kanban.dead_letter"

Effect:
  ├─ ACK → remove from queue
  ├─ NACK → send to dead letter exchange
  ├─ Timeout → auto-NACK → dead letter
  └─ Max retries → dead letter
```

## Data Flow

### Publishing Work

```
Application calls:
  kanban_queue:publish({high, payment, Payload, Deadline})
    ↓
  Generate WorkId (timestamp + random)
    ↓
  Encode payload to JSON:
    {
      "id": "work_123456",
      "priority": "high",
      "domain": "payment",
      "payload": {...},
      "deadline": 1706035200000,
      "created_at": 1706035170000,
      "retry_count": 0
    }
    ↓
  NATS pub to "kanban.high.payment"
    ↓
  Is publish successful?
    ├─ YES → Return {ok, WorkId}
    └─ NO → Fallback to RabbitMQ
           ├─ Declare exchange
           ├─ Declare queue
           ├─ Publish message
           └─ Return {ok, WorkId}
    ↓
  Update metrics (published++)
```

### Processing Work

```
Worker pulls:
  kanban_worker:pull_work(Pid)
    ↓
  Check circuit breaker
    ├─ OPEN → Return {error, circuit_breaker_open}
    └─ CLOSED → Continue
    ↓
  Subscribe to queue (first time)
  NATS sub "kanban.high.payment" → receive messages
    ↓
  Receive up to N messages (default 10)
    ↓
  For each item:
    ├─ Get deadline
    ├─ Calculate time remaining
    ├─ If expired → NACK immediately
    └─ Otherwise:
         ├─ Spawn async processor
         ├─ Set timeout to min(deadline, process_timeout)
         ├─ Wait for result
         ├─ If success → ACK
         └─ If failure → NACK
             └─ Increment failure counter
             └─ If failures >= 3 → Open circuit breaker
    ↓
  Return pulled items to caller
```

### Monitoring & Auto-Scale

```
Coordinator runs every 5 seconds:
    ↓
  Get queue depth for all domains
    ↓
  Add to rolling window (last 10 samples)
    ↓
  Calculate average depth
    ↓
  Calculate target workers:
    TargetWorkers = max(2, min(20, (AvgDepth÷20)+2))
    ↓
  Compare with current workers:
    ├─ Target > Current → Start new workers
    ├─ Target < Current → Stop workers (gracefully)
    └─ Same → No change
    ↓
  Update Prometheus metrics
    ├─ kanban_queue_depth_avg = avg(samples)
    ├─ kanban_current_workers = current
    ├─ kanban_target_workers = target
    ↓
  Check for stalled items (>5 min in queue)
    ├─ If found → Move to dead letter
    └─ Alert: "Found N stalled items"
    ↓
  Sleep 5 seconds, repeat
```

## Synchronization & Concurrency

### Work Item Lifecycle

```
Published (T=0)
  ├─ In queue
  ├─ Waiting for worker
  ├─ Deadline = T + 60000ms
  │
  └─ Worker pulls (T=2000ms)
      ├─ Processing
      ├─ Time remaining = Deadline - now
      ├─ If remaining < 0: NACK (expired)
      ├─ Otherwise: process
      │
      └─ Processed (T=2500ms)
          ├─ Success → ACK (removed from queue)
          ├─ Failure → NACK
          │  ├─ If retries < max: retry
          │  └─ Otherwise: dead letter
          │
          └─ Acknowledged (T=2510ms) → Complete
```

### Worker Scaling

```
Current Workers = 5
Queue Depth = 150

T=0:
  Coordinator samples depth: [0, 0, 0, 0, 0]
  Avg = 0, Target = 2, No action

T=5s: Items published to queue
  Depth = 100
  Samples = [0, 0, 0, 0, 100]
  Avg = 20, Target = 4
  Start 4-5 = -1 (no change yet)

T=10s: More items published
  Depth = 150
  Samples = [0, 0, 100, 150, 150]
  Avg = 80, Target = 6
  Start 6-5 = 1 new worker
  Current = 6

T=15s: Items draining
  Depth = 30
  Samples = [100, 150, 150, 30, 30]
  Avg = 92, Target = 6
  No change (already 6)

T=20s: Most items processed
  Depth = 5
  Samples = [150, 150, 30, 30, 5]
  Avg = 73, Target = 5
  No change

T=25s: Queue nearly empty
  Depth = 1
  Samples = [150, 30, 30, 5, 1]
  Avg = 43, Target = 4
  Stop 5-4 = 1 worker
  Current = 5
```

## Error Handling Strategy

### NATS Connection Failure

```
Publication fails (NATS unavailable)
  ↓
Fallback to RabbitMQ
  ├─ Connect to RabbitMQ
  ├─ Declare queue
  ├─ Publish message
  └─ Return {ok, WorkId}

If RabbitMQ also unavailable:
  ├─ Return {error, no_queue_available}
  ├─ Application retries with backoff
  └─ Alert: "Both NATS and RabbitMQ unavailable"
```

### Worker Processing Timeout

```
Worker processing takes too long
  ├─ Deadline exceeded
  ├─ Timeout timer fires
  ├─ Async processor killed
  ├─ NACK sent to queue
  ├─ Item moved to dead letter
  ├─ Alert: "Item exceeded deadline"
  └─ Investigate: deadline too aggressive? Processing slow?
```

### Circuit Breaker Trip

```
Worker fails 3 times in a row
  ├─ Circuit breaker opens
  ├─ Worker stops pulling
  ├─ Alert: "Circuit breaker open for worker_X"
  ├─ Operator investigates root cause
  │  (database down? API timeout? bad data?)
  ├─ Operator fixes issue
  ├─ Operator restarts worker
  └─ Circuit breaker closes, worker resumes
```

## Performance Characteristics

### Throughput

- **NATS**: 500-1000 items/sec (network dependent)
- **RabbitMQ**: 100-200 items/sec (disk writes slower)
- **Combined**: System throughput limited by slowest worker

### Latency

```
Publish → Queue → Worker Pull → Process → ACK

        NATS              Worker Process
        ↓                 ↓
    1-5ms           50-100ms typical
                    200-500ms P99

Total: 51-505ms typical
```

### Memory

- Per-worker: 5-10 MB
- Queue (10k items): 50-100 MB
- System (20 workers + queue): 150-300 MB

### CPU

- Queue operations: <1% per core (NATS is efficient)
- Worker processing: Depends on application logic
- Coordinator: <1% per core (minimal work)

## Production Considerations

### Deployment

1. **Start NATS** (or RabbitMQ as fallback)
2. **Start Kanban application**
3. **Configure workers** (priority/domain coverage)
4. **Monitor metrics** (Prometheus scrape interval 30s)
5. **Setup alerts** (queue_depth > 1000? circuit_breaker_open?)

### Scaling

```
Vertical (single machine):
  ├─ Increase workers (config: max 20 per Erlang node)
  ├─ Increase NATS/RabbitMQ resources
  └─ Increase memory/CPU

Horizontal (multiple machines):
  ├─ Run multiple Kanban instances
  ├─ Each instance: 20 workers per domain
  ├─ Shared NATS cluster (for load balancing)
  ├─ Shared RabbitMQ cluster (for failover)
  └─ Central monitoring (Prometheus)
```

### High Availability

```
NATS HA:
  ├─ NATS clustering (3+ nodes for quorum)
  └─ Each node independent

RabbitMQ HA:
  ├─ RabbitMQ clustering (3+ nodes)
  ├─ Mirrored queues for failover
  └─ Load balancing across nodes

Kanban HA:
  ├─ Multiple instances (load balanced)
  ├─ Shared NATS broker
  ├─ Shared RabbitMQ broker
  └─ Stateless (any instance can fail)
```

---

## Summary

TPS Kanban is a **production-grade, pull-based work distribution system** designed for:

- **Simplicity**: Pull model naturally prevents overload
- **Reliability**: NATS + RabbitMQ ensures no work loss
- **Scalability**: Auto-scaling adapts to demand
- **Observability**: Full metrics and visibility
- **Safety**: Circuit breakers, graceful shutdown, dead letters

The architecture emphasizes **statelessness**, **resilience**, and **operational clarity**.
