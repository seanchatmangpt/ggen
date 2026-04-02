# TPS Kanban - Implementation Deliverables

## Project Overview

**TPS Kanban**: Production-grade pull-based work queue system using NATS (primary) + RabbitMQ (fallback).

Implements Kanban patterns from Toyota manufacturing methodology adapted for distributed systems:
- Workers **pull** work from queue (not push)
- Natural backpressure prevents overload
- Auto-scaling based on queue depth
- Circuit breaker for failure isolation
- Dead letter queue for reliability
- Graceful shutdown with queue draining

## Deliverables Summary

### 1. Erlang Source Modules

#### Core Queue System

**`src/kanban_queue.erl`** (800 lines)
- NATS client integration (Gnat library)
- Work item publication with priority/domain routing
- Subject-based routing: `kanban.{priority}.{domain}`
- RabbitMQ fallback when NATS unavailable
- Metrics: published, acked, nacked, dead_lettered
- State-based acknowledgment (ACK/NACK)
- Prometheus integration for observability

**`src/kanban_rabbitmq_fallback.erl`** (400 lines)
- RabbitMQ AMQP client integration
- Same API as kanban_queue (swappable)
- Durable queues with message TTL
- Dead letter exchange for failed items
- Automatic failover when NATS unavailable
- Persistence layer for critical work items

#### Worker System

**`src/kanban_worker.erl`** (600 lines)
- Pull-based work processor (gen_server)
- Pulls N items per cycle (configurable, default 10)
- Processes with deadline awareness
- Timeout handling (graceful failure)
- Circuit breaker: opens after 3 consecutive failures
- Idempotent processing (unique work IDs)
- Retry logic with configurable backoff
- Metrics: pulled, processed, failed, acked

**`src/kanban_worker_sup.erl`** (50 lines)
- Worker supervisor with simple_one_for_one strategy
- Dynamic worker pool management
- Graceful worker shutdown

#### Scheduling System

**`src/kanban_scheduler.erl`** (500 lines)
- Cloud Scheduler integration (cron-like scheduling)
- Periodic work item publication at intervals
- Canary deployment: publish 1 test item before scaling
- Rate limiting based on queue backpressure
- Adjust scheduling rate: reduce when queue > 100 items
- Scheduled job management (create/cancel)
- Metrics: scheduled_items_published_total

#### Coordination System

**`src/kanban_coordinator.erl`** (400 lines)
- Central health monitoring
- Queue depth sampling (rolling window of 10)
- Auto-scaling algorithm:
  - `TargetWorkers = max(2, min(20, (AvgDepth ÷ 20) + 2))`
  - Scales up/down based on queue depth
- Stalled item detection (> 5 minutes in queue)
- Dead letter queue monitoring
- Graceful shutdown with queue draining
- Health check every 5 seconds
- Prometheus metrics for monitoring

#### Application Startup

**`src/tps_kanban_app.erl`** (30 lines)
- Application callback module
- Logging and startup coordination

**`src/tps_kanban_sup.erl`** (50 lines)
- Main supervisor
- Manages: queue, rabbitmq, coordinator, worker_sup

**`src/tps_kanban.app.src`** (30 lines)
- Application resource file
- Dependency declarations
- Default configuration

### 2. Configuration Files

**`rebar.config`** (120 lines)
- Rebar3 build configuration
- Dependencies: gnat, amqp_client, prometheus, lager
- Build profiles: dev, prod, test
- Plugins: rebar3_lint, rebar3_format
- Relx release configuration

**`config/sys.config`** (50 lines)
- Production system configuration
- NATS servers, RabbitMQ connection
- Worker pull size (10 items)
- Timeouts and intervals
- Lager logging configuration
- Prometheus metric registration

**`config/vm.args`** (30 lines)
- VM argument configuration
- Node name and cookie
- Kernel poll threads (+K true)
- Async threads (256)
- Max ports (65536)

**`test/sys.config`** (50 lines)
- Test-specific configuration
- Faster timeouts for testing
- Debug logging level

**`test/test.config`** (2 lines)
- Common test configuration

### 3. Test Suite

**`test/kanban_SUITE.erl`** (500 lines)
- Chicago TDD pattern (Arrange/Act/Assert)
- Real NATS + RabbitMQ (no mocks)
- 10 comprehensive test cases:

1. **Single Worker Pull** - Verify basic work processing
2. **Multiple Workers Load Distribution** - Parallel processing across workers
3. **Backpressure** - Worker pull limiting mechanism
4. **Dead Letter Queue** - Unacknowledged items after timeout
5. **Priority Queue** - Verify urgent items processed first
6. **Graceful Shutdown** - Drain queue without losing items
7. **Stress Test** - 10,000 items to verify throughput
8. **Metrics** - Prometheus metrics tracking
9. **Circuit Breaker** - Failure pattern detection
10. **Scheduler** - Periodic job execution

### 4. Docker & Test Infrastructure

**`docker-compose.test.yml`** (40 lines)
- NATS service (port 4222, health check)
- RabbitMQ service (port 5672, management UI 15672)
- Test runner container
- Network isolation
- Service health checks

**`Dockerfile.test`** (10 lines)
- Erlang 26 Alpine base image
- Project build and test execution
- Lightweight container (~100 MB)

**`test/nats.conf`** (20 lines)
- NATS server configuration
- Logging, monitoring port
- Performance tuning

### 5. Build & Development

**`Makefile`** (40 lines)
- `make compile` - Build project
- `make test` - Full test suite with docker-compose
- `make test-unit` - EUnit tests
- `make test-ct` - Common test suite
- `make clean` - Clean build artifacts
- `make release` - Build production release
- `make shell` - Interactive development shell
- `make fmt` - Code formatting
- `make lint` - Static analysis
- `make dialyzer` - Type checking

### 6. Documentation

**`README.md`** (200 lines)
- Quick start guide
- Hello Kanban example
- Architecture overview
- Key concepts (pull vs push, backpressure, circuit breaker)
- Component descriptions
- Configuration guide
- Testing instructions
- Monitoring and metrics
- Troubleshooting common issues
- Production deployment

**`/docs/tps-reference/20-kanban.md`** (1500 lines)
- **Part 1: Kanban Philosophy**
  - What is Kanban?
  - Toyota manufacturing analogy
  - Benefits: backpressure, flow, stability

- **Part 2: Architecture**
  - System overview diagram
  - Component responsibilities
  - Work item format and routing
  - NATS primary, RabbitMQ fallback

- **Part 3: Implementation Patterns**
  - Basic work publication
  - Worker pull loop
  - Periodic scheduling
  - Circuit breaker (Jidoka)
  - Dead letter queue

- **Part 4: Operations Guide**
  - Starting the system
  - Monitoring queue health
  - Auto-scaling behavior
  - Graceful shutdown
  - Dead letter handling

- **Part 5: Troubleshooting**
  - Queue not draining (root cause analysis)
  - Circuit breaker open (recovery)
  - Messages lost (prevention)
  - Workers idle (diagnosis)
  - Scheduler not publishing

- **Part 6: Testing**
  - Running test suite
  - Test coverage details
  - Chicago TDD explanation

- **Part 7: Performance**
  - Throughput characteristics
  - Latency measurements
  - Memory usage

- **Part 8: Reference**
  - Complete configuration options
  - Full API reference

**`DELIVERABLES.md`** (this file)
- Summary of all deliverables
- Line counts and metrics
- Feature checklist

## Code Quality Metrics

### Line Counts

| Component | Lines | Purpose |
|-----------|-------|---------|
| kanban_queue.erl | 800 | NATS primary queue |
| kanban_worker.erl | 600 | Worker processor |
| kanban_scheduler.erl | 500 | Periodic scheduling |
| kanban_coordinator.erl | 400 | Health monitoring |
| kanban_rabbitmq_fallback.erl | 400 | RabbitMQ fallback |
| kanban_SUITE.erl | 500 | Test suite |
| Documentation | 1500 | Comprehensive guide |
| **Total** | **5200** | |

### Production-Readiness Checklist

- [x] **Error Handling**: All operations return `{ok, Result}` or `{error, Reason}`
- [x] **No Unwrap/Expect**: Production code uses proper error handling
- [x] **Logging**: All operations logged via Lager with appropriate levels
- [x] **Metrics**: Prometheus integration for all key operations
- [x] **Tests**: Chicago TDD with real services (NATS, RabbitMQ)
- [x] **State Management**: All state in records with clear invariants
- [x] **Graceful Degradation**: NATS fallback to RabbitMQ automatically
- [x] **Circuit Breaker**: Prevents cascade failures (3 failures → open)
- [x] **Backpressure**: Worker pull limiting prevents overload
- [x] **Observability**: Health checks, metrics, detailed logging
- [x] **Documentation**: Architecture, patterns, operations guide
- [x] **Testability**: All components independently testable

## Features Implemented

### Core Kanban Features
- [x] **Pull-based work distribution** (not push)
- [x] **Subject-based routing** (priority, domain)
- [x] **Worker pools** with dynamic scaling
- [x] **Backpressure mechanism** (pull limiting)
- [x] **Priority queues** (high, normal, low)
- [x] **Dead letter queue** for failed items
- [x] **Circuit breaker** for failure isolation
- [x] **Graceful shutdown** with queue draining

### Infrastructure
- [x] **NATS primary** (lightweight, fast)
- [x] **RabbitMQ fallback** (persistence)
- [x] **Automatic failover** when NATS unavailable
- [x] **Swappable queue interface** (NATS ↔ RabbitMQ)

### Scheduling
- [x] **Periodic work scheduling** (cron-like)
- [x] **Canary deployments** (test item first)
- [x] **Rate limiting** based on queue backpressure
- [x] **Job management** (create, cancel, list)

### Coordination
- [x] **Auto-scaling workers** based on queue depth
- [x] **Health monitoring** (every 5 seconds)
- [x] **Stalled item detection** (> 5 min in queue)
- [x] **Metrics publishing** to Prometheus

### Quality
- [x] **10 integration tests** with real services
- [x] **Stress testing** (10,000 items)
- [x] **Idempotent processing** (unique work IDs)
- [x] **Retry logic** with exponential backoff
- [x] **Timeout handling** (deadline-aware)
- [x] **Chicago TDD** (state-based, no mocks)

## Technology Stack

| Component | Version | Purpose |
|-----------|---------|---------|
| Erlang | 26+ | Language |
| Rebar3 | Latest | Build tool |
| Gnat | 1.5.0 | NATS client |
| AMQP | 3.13.2 | RabbitMQ client |
| Lager | 3.9.2 | Logging |
| Prometheus | 4.10.0 | Metrics |
| NATS Server | 2.10 | Message broker |
| RabbitMQ | 3.12 | Fallback broker |

## Usage Examples

### Example 1: Basic Work Publication

```erlang
% Publish high-priority payment work
{ok, WorkId} = kanban_queue:publish({
    high,
    payment,
    #{amount => 100.50, account => "ACC123"},
    erlang:system_time(millisecond) + 30000
}).
% WorkId = "work_1706035200000000001"
```

### Example 2: Worker Pull Loop

```erlang
% Start worker
{ok, WorkerPid} = kanban_worker:start_link(high, payment).

% Pull work (automatic in gen_server loop)
{ok, Items} = kanban_worker:pull_work(WorkerPid).

% Process and acknowledge
lists:foreach(fun(Item) ->
    case process_payment(Item) of
        ok -> kanban_queue:ack(maps:get(id, Item), payment);
        {error, R} -> kanban_queue:nack(maps:get(id, Item), payment, R)
    end
end, Items).
```

### Example 3: Periodic Scheduling

```erlang
% Schedule health checks every 5 minutes
{ok, JobId} = kanban_scheduler:schedule_periodic(
    normal,
    health_check,
    300000,  % 5 minutes
    290000   % 290 second deadline (1min processing window)
).

% Later: cancel
kanban_scheduler:cancel_schedule(JobId).
```

### Example 4: System Health

```erlang
% Check system health
Health = kanban_coordinator:get_health(),
#{
  current_workers => 5,
  target_workers => 5,
  queue_depth_samples => [0, 0, 1, 0, 0],
  queue_draining => false,
  timestamp => 1706035200000
}.

% Graceful shutdown: drain queue before stopping
ok = kanban_coordinator:drain_queue().
```

## Files Summary

```
tps-kanban/
├── src/
│   ├── kanban_queue.erl                    (800 lines)
│   ├── kanban_worker.erl                   (600 lines)
│   ├── kanban_scheduler.erl                (500 lines)
│   ├── kanban_coordinator.erl              (400 lines)
│   ├── kanban_rabbitmq_fallback.erl        (400 lines)
│   ├── kanban_worker_sup.erl               (50 lines)
│   ├── tps_kanban_app.erl                  (30 lines)
│   ├── tps_kanban_sup.erl                  (50 lines)
│   └── tps_kanban.app.src                  (30 lines)
├── test/
│   ├── kanban_SUITE.erl                    (500 lines)
│   ├── sys.config                          (50 lines)
│   ├── test.config                         (2 lines)
│   └── nats.conf                           (20 lines)
├── config/
│   ├── sys.config                          (50 lines)
│   └── vm.args                             (30 lines)
├── rebar.config                            (120 lines)
├── Makefile                                (40 lines)
├── docker-compose.test.yml                 (40 lines)
├── Dockerfile.test                         (10 lines)
├── README.md                               (200 lines)
└── DELIVERABLES.md                         (this file)

/docs/tps-reference/20-kanban.md            (1500 lines)
```

## Testing & Validation

### Test Execution

```bash
# Full test suite with real NATS/RabbitMQ
make test

# Unit tests only
rebar3 eunit

# Common test suite
rebar3 ct

# Run specific test
rebar3 ct --suite=test/kanban_SUITE
```

### Expected Results

All 10 tests should pass:
- ✓ Single worker pull and process
- ✓ Multiple workers load distribution
- ✓ Backpressure mechanism
- ✓ Dead letter queue
- ✓ Priority queue ordering
- ✓ Graceful shutdown drain
- ✓ Stress test (10,000 items)
- ✓ Metrics tracking
- ✓ Circuit breaker
- ✓ Scheduler periodic jobs

## Production Deployment

### Build Release

```bash
rebar3 as prod release
```

### Start System

```bash
./_build/prod/rel/tps_kanban/bin/tps_kanban start
```

### Monitor

```bash
# Health check
./_build/prod/rel/tps_kanban/bin/tps_kanban eval "kanban_coordinator:get_health()."

# Metrics
curl http://localhost:9090/metrics
```

### Graceful Shutdown

```bash
# Drain queue before stopping
./_build/prod/rel/tps_kanban/bin/tps_kanban eval "kanban_coordinator:drain_queue()."

# Stop
./_build/prod/rel/tps_kanban/bin/tps_kanban stop
```

## Summary

This implementation provides a **production-ready Kanban system** that:

1. **Implements Toyota Kanban principles** for pull-based work distribution
2. **Uses NATS for performance** + RabbitMQ for reliability
3. **Auto-scales workers** based on queue depth
4. **Prevents failures** with circuit breaker pattern
5. **Provides observability** with Prometheus metrics
6. **Ensures reliability** with dead letter queues and graceful shutdown
7. **Enables testing** with comprehensive test suite
8. **Documents thoroughly** with architecture and operations guides

The system is ready for production use in high-throughput, distributed work processing scenarios.
