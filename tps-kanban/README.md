# TPS Kanban: Production-Grade Pull-Based Work Queue

**Kanban** = Pull-based work distribution inspired by Toyota manufacturing methodology.

Instead of pushing work to workers (causing overload), workers **pull** work from a queue at their own pace. This naturally implements backpressure, preventing queue buildup and worker exhaustion.

## Quick Start

### Prerequisites

- Erlang 26+
- Rebar3
- Docker & Docker Compose (for testing)

### Development Setup

```bash
# Clone/enter the project
cd /home/user/ggen/tps-kanban

# Build
rebar3 compile

# Run tests with real NATS/RabbitMQ
make test

# Or start locally
docker-compose -f docker-compose.test.yml up

# Connect to shell
rebar3 shell
```

### Hello Kanban

```erlang
% Start application
application:ensure_all_started(tps_kanban).

% Publish high-priority payment work
Deadline = erlang:system_time(millisecond) + 30000,
{ok, WorkId} = kanban_queue:publish({
    high,                           % Priority
    payment,                        % Domain
    #{amount => 100.50},           % Payload
    Deadline                        % Deadline (ms)
}).
% WorkId = "worker_123456..."

% Start worker (pulls from high.payment queue)
{ok, WorkerPid} = kanban_worker:start_link(high, payment).

% Worker pulls work
{ok, Items} = kanban_worker:pull_work(WorkerPid).
% Items = [#{id => "worker_123456...", ...}]

% Process and acknowledge
lists:foreach(fun(Item) ->
    WorkId = maps:get(id, Item),
    % Do real work here
    kanban_queue:ack(WorkId, payment)
end, Items).

% Check health
Health = kanban_coordinator:get_health().
% #{
%   current_workers => 5,
%   target_workers => 5,
%   queue_depth_samples => [0],
%   queue_draining => false,
%   timestamp => ...
% }
```

## Architecture

```
┌──────────────────────────────────┐
│     Cloud Scheduler / API        │
└──────────────┬───────────────────┘
               │ publish work
               ↓
         ┌─────────────┐
         │    NATS     │  (Primary)
         │  (Fast,     │
         │ Ephemeral)  │
         └──────┬──────┘
                │
        ┌───────┴────────────┐
        ↓                    ↓
    kanban.high.payment   kanban.normal.fraud
    kanban.low.account    kanban.normal.account
        │                    │
        ↓                    ↓
    ┌──────────────────────────┐
    │  Worker Pool (Pull-based) │
    │  ┌────┐ ┌────┐ ┌────┐    │
    │  │ W1 │ │ W2 │ │ W3 │    │
    │  └────┘ └────┘ └────┘    │
    └──────────────────────────┘
        │       │       │
        ├─ Process work
        ├─ ACK (success)
        └─ NACK + Dead Letter (failure)
             │
             ↓
    ┌──────────────────────┐
    │   Coordinator        │
    │ - Auto-scale workers │
    │ - Monitor health     │
    │ - Drain queue        │
    └──────────────────────┘
         │
         ↓
    Prometheus Metrics
```

## Key Concepts

### Pull vs Push

```
❌ PUSH (Antipattern)           ✅ PULL (Kanban)
┌─────────┐                    ┌─────────┐
│Scheduler│ → Force work       │Scheduler│
└─────────┘   to workers       └─────────┘
   ├→ W1                           │
   ├→ W2 (overload!)              [Queue]
   ├→ W3 (dropped!)                ↑
   └→ W4 (cascade fail)            │
                          ← Workers PULL

Workers control pace          System self-stabilizes
Queue crashes                Backpressure prevents overload
Work lost                     Full visibility
```

### Backpressure

```
Queue Depth    Worker Behavior         System Effect
────────────   ──────────────────      ─────────────
0-10           Pull immediately        Flow = production
20-50          Pull at steady pace     System in balance
100+           Pull slowed             Intake rate reduced
                                       Prevents cascade failure
```

### Circuit Breaker

```
Worker detects pattern of failures
├─ 1st failure: log, retry
├─ 2nd failure: log, retry
├─ 3rd consecutive failure: STOP
│  └─ Circuit breaker OPENS
│  └─ Stop pulling work (prevent cascade)
│  └─ Alert: "Worker circuit open - investigate"
│
Human intervention
├─ Fix root cause (DB? API?)
├─ Restart worker
└─ System resumes
```

### Dead Letter Queue

Items are moved to dead letter queue when:
- Unacknowledged for > 5 minutes
- Deadline exceeded when dequeued
- Processing timeout exceeded
- Explicit NACK with reason

Dead lettered items are preserved for investigation.

## Components

### 1. `kanban_queue` - Work Storage

- NATS primary (fast, ephemeral)
- RabbitMQ fallback (durable, persistent)
- Automatic failover if NATS unavailable
- Subjects: `kanban.{priority}.{domain}`

```erlang
kanban_queue:publish({high, payment, Payload, Deadline})
kanban_queue:ack(WorkId, Domain)
kanban_queue:nack(WorkId, Domain, Reason)
```

### 2. `kanban_worker` - Work Processor

- Pulls N items from queue (default 10)
- Processes with deadline awareness
- Acknowledges on success, nacks on failure
- Circuit breaker for failure patterns

```erlang
kanban_worker:start_link(Priority, Domain)
kanban_worker:pull_work(Pid)
kanban_worker:get_status(Pid)
kanban_worker:stop(Pid)
```

### 3. `kanban_scheduler` - Periodic Work

- Schedule work at regular intervals
- Adjust rate based on queue backpressure
- Canary testing for safe deployments

```erlang
kanban_scheduler:schedule_periodic(Priority, Domain, IntervalMs, DeadlineMs)
kanban_scheduler:schedule_canary(Domain, Payload)
kanban_scheduler:cancel_schedule(JobId)
```

### 4. `kanban_coordinator` - Health & Auto-Scale

- Auto-scale worker count based on queue depth
- Health checks every 5 seconds
- Detect stalled items and move to dead letter
- Graceful shutdown with queue draining

```erlang
kanban_coordinator:get_health()
kanban_coordinator:adjust_workers(Count)
kanban_coordinator:drain_queue()
```

## Configuration

See `config/sys.config`:

```erlang
{tps_kanban, [
    {nats_servers, ["nats://localhost:4222"]},
    {rabbitmq_host, "localhost"},
    {rabbitmq_port, 5672},
    {worker_pull_size, 10},
    {worker_process_timeout_ms, 30000},
    {dead_letter_timeout_ms, 300000},
    {coordinator_check_interval_ms, 5000}
]}
```

## Testing

### Run Full Test Suite

```bash
# With docker-compose (real NATS/RabbitMQ)
make test

# Manual test running
docker-compose -f docker-compose.test.yml up --build
```

### Test Coverage

The test suite (`test/kanban_SUITE.erl`) includes:

1. Single worker pulls and processes items
2. Multiple workers load distribution
3. Backpressure mechanism (worker pulls N items)
4. Dead letter queue handling
5. Priority-based routing
6. Graceful shutdown with queue draining
7. Stress test: 10,000 items
8. Metrics tracking
9. Circuit breaker on failures
10. Scheduler periodic jobs

All tests use **Chicago TDD**:
- Real services (no mocks)
- State-based assertions
- Arrange/Act/Assert structure

## Monitoring

### Health Check

```erlang
Health = kanban_coordinator:get_health().
#{
  current_workers => 5,              % Active workers
  target_workers => 5,               % Auto-scaled target
  queue_depth_samples => [0, 0, 0],  % Last 10 readings
  queue_draining => false,           % Graceful shutdown active
  timestamp => 1706035200000
}
```

### Metrics (Prometheus)

```
kanban_queue_depth              # Current queue depth
kanban_current_workers          # Active worker count
kanban_queue_depth_avg          # 10-reading rolling avg
kanban_latency_ms              # Work item latency
kanban_published_total         # Total published items
kanban_acked_total             # Total acknowledged items
kanban_dead_lettered_total     # Total dead-lettered items
kanban_circuit_breaker_open    # Circuit breaker status
```

### Logs

```bash
tail -f log/tps_kanban.log | grep "ERROR\|ALERT"
```

## Troubleshooting

### Queue Not Draining

```erlang
% Check health
Health = kanban_coordinator:get_health(),
AvgDepth = lists:sum(Health.queue_depth_samples) /
           length(Health.queue_depth_samples),

% If average depth > 100 and not increasing workers:
% 1. Check worker circuit breaker status
% 2. Check processing latency (deadline too aggressive?)
% 3. Verify domain routing (items going to wrong domain?)

% Quick fix: add more workers
kanban_coordinator:adjust_workers(20).
```

### Circuit Breaker Open

```erlang
% Workers stop pulling after 3 consecutive failures

% Investigate root cause
% 1. Database unavailable? → restore connection
% 2. External API timeout? → increase timeout
% 3. Invalid data? → fix upstream

% Recover
kanban_worker:stop(WorkerPid),
{ok, NewPid} = kanban_worker:start_link(high, payment).
```

### Workers Not Pulling

```erlang
% Check which domains have workers
supervisor:which_children(kanban_worker_sup).

% If missing domain coverage:
kanban_worker_sup:start_worker(high, payment),
kanban_worker_sup:start_worker(normal, fraud).

% Verify workers are pulling
Metrics = kanban_queue:get_metrics(),
maps:get(acked, Metrics).  % Should be increasing
```

## Documentation

See `/docs/tps-reference/20-kanban.md` for comprehensive guide:
- Kanban philosophy and history
- Architecture deep dive
- Implementation patterns
- Operations guide
- Performance characteristics
- Full API reference

## Production Deployment

```bash
# Build release
rebar3 as prod release

# Run
./_build/prod/rel/tps_kanban/bin/tps_kanban start

# Verify
./_build/prod/rel/tps_kanban/bin/tps_kanban eval "kanban_coordinator:get_health()."

# Stop gracefully
./_build/prod/rel/tps_kanban/bin/tps_kanban stop
```

## License

Part of the ggen project ecosystem. See main LICENSE.

## Support

- Report issues: See main ggen repository
- Kanban reference: `/docs/tps-reference/20-kanban.md`
- Architecture: See ARCHITECTURE.md (when created)
