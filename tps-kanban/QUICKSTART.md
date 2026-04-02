# TPS Kanban - Quick Start Guide

Get a Kanban system running in 5 minutes.

## Prerequisites

```bash
# Required
erlang --version         # Should be 26.0 or higher
rebar3 --version         # Should be 3.x

# For testing (optional)
docker --version
docker-compose --version
```

## 1. Clone & Setup (1 minute)

```bash
cd /home/user/ggen/tps-kanban

# Build the project
rebar3 compile

# You should see: ===> Compile succeeded
```

## 2. Start Services (2 minutes)

### Option A: Using Docker (Recommended)

```bash
# Start NATS and RabbitMQ
docker-compose -f docker-compose.test.yml up -d

# Verify
curl -s nats://localhost:4222 || echo "NATS running"
curl -s http://localhost:15672 || echo "RabbitMQ running"
```

### Option B: Manual Start

```bash
# Install and start NATS locally
# Install and start RabbitMQ locally
# (See your package manager)
```

## 3. Start Erlang Shell (1 minute)

```bash
# Start interactive shell
rebar3 shell

# In shell, start the application
> application:ensure_all_started(tps_kanban).
ok

# You should see logs:
# "Starting TPS Kanban application"
# "Initializing Kanban Queue"
# "Initialized RabbitMQ Fallback Queue"
# "Starting Kanban Scheduler"
# etc.
```

## 4. Say Hello to Kanban (1 minute)

```erlang
%% Publish a work item
{ok, WorkId} = kanban_queue:publish({
    high,
    payment,
    #{amount => 100.50, account => "ACC123"},
    erlang:system_time(millisecond) + 30000
}).

% You should see:
% WorkId = "work_1706035200000000..."

%% Start a worker
{ok, WorkerPid} = kanban_worker:start_link(high, payment).

% Worker is now pulling from the queue

%% Pull work items
{ok, Items} = kanban_worker:pull_work(WorkerPid).

% You should see a list of items:
% Items = [#{id => ..., priority => high, ...}]

%% Acknowledge work
lists:foreach(fun(Item) ->
    WorkId = maps:get(id, Item),
    kanban_queue:ack(WorkId, payment)
end, Items).

%% Check system health
Health = kanban_coordinator:get_health().

% You should see:
% #{
%   current_workers => 5,
%   target_workers => 5,
%   queue_depth_samples => [0, 0, 0],
%   queue_draining => false,
%   timestamp => ...
% }

%% Stop worker
kanban_worker:stop(WorkerPid).
```

## 5. Run Tests

### Run Full Test Suite

```bash
# Exit shell first (Ctrl+C)
> q().

# Run tests with docker-compose
make test

# Or run directly
rebar3 ct

# You should see 10 tests pass:
# test_single_worker_pulls_and_processes .... ok
# test_multiple_workers_load_distribution ... ok
# test_backpressure_worker_pulls_n ......... ok
# ... etc
```

## Common Commands

### Interactive Development

```bash
# Start shell
rebar3 shell

# Compile changes (in shell)
> c(kanban_queue).
{ok, kanban_queue}

# Get queue metrics
> kanban_queue:get_metrics().
#{published => 5, acked => 3, nacked => 0, ...}

# Get system health
> kanban_coordinator:get_health().

# Schedule periodic job
> kanban_scheduler:schedule_periodic(high, payment, 60000, 55000).
{ok, "job_..."}

# Exit shell
> q().
```

### Building & Testing

```bash
# Build
make compile
rebar3 compile

# Quick tests
make test-unit
rebar3 eunit

# Full test suite
make test
rebar3 ct

# Code formatting
make fmt
rebar3 fmt --write

# Linting
make lint
rebar3 lint

# Type checking
make dialyzer
rebar3 dialyzer

# Clean
make clean
rebar3 clean
```

### Production Release

```bash
# Build production release
make release
rebar3 as prod release

# Start release
./_build/prod/rel/tps_kanban/bin/tps_kanban start

# Stop release
./_build/prod/rel/tps_kanban/bin/tps_kanban stop

# Graceful shutdown
./_build/prod/rel/tps_kanban/bin/tps_kanban eval "kanban_coordinator:drain_queue()."
```

## Architecture at a Glance

```
┌─────────────────────────────┐
│   Your Application          │
│  publish({high, payment})   │
└──────────────┬──────────────┘
               │
               ↓
        ┌─────────────┐
        │  NATS/RMQ   │
        │  (Queue)    │
        └──────┬──────┘
               │
   ┌───────────┴───────────┐
   ↓                       ↓
┌─────────┐           ┌─────────┐
│Worker1  │           │Worker2  │
│ Pull N  │           │ Pull N  │
│ Process │           │ Process │
│ ACK     │           │ ACK     │
└──┬──────┘           └──┬──────┘
   │                     │
   └─────────┬───────────┘
             ↓
    ┌──────────────────┐
    │  Coordinator     │
    │  Auto-scale      │
    │  Health monitor  │
    └──────────────────┘
```

## Key Concepts

### Pull vs Push

- **Pull** (Kanban): Workers take work when ready
  - Prevents overload
  - Backpressure naturally limits intake
  - System self-regulates

- **Push** (Antipattern): System forces work on workers
  - Causes overload
  - Queue crashes
  - Workers drop items

### Work Item Lifetime

```
1. Publish (T=0)
   └─ Item in queue, deadline = T + 30000ms

2. Worker pulls (T=500ms)
   └─ Item dequeued, processing starts

3. Process (T=500-2500ms)
   └─ Application logic runs

4. ACK/NACK (T=2500ms)
   └─ Success: removed from queue
   └─ Failure: retry or dead letter

5. Complete (T=2510ms)
   └─ Item removed from system
```

### Backpressure

```
Queue Depth          Worker Behavior        System Effect
────────────────     ──────────────────     ────────────
0-10                 Pull immediately       Normal flow
50-100               Pull steadily          Balanced
100+                 Pull slowing down      Load limiting
                     (auto-adjusted)
```

## Troubleshooting

### "Connection refused" error

**Problem**: Can't connect to NATS/RabbitMQ

**Solution**:
```bash
# Ensure containers are running
docker-compose -f docker-compose.test.yml ps

# Should show:
# nats    : up
# rabbitmq: up

# If not running:
docker-compose -f docker-compose.test.yml up -d
```

### "Item not found" in tests

**Problem**: Pull returned empty list

**Solution**:
```erlang
% This is normal - queue might be empty
% Publish some items first:
lists:foreach(fun(_) ->
  kanban_queue:publish({high, payment, #{}, now_ms() + 30000})
end, lists:seq(1, 5)).

% Then pull again
{ok, Items} = kanban_worker:pull_work(WorkerPid).
```

### Circuit breaker open

**Problem**: Worker not pulling (circuit open)

**Solution**:
```erlang
% Check status
Status = kanban_worker:get_status(WorkerPid),
maps:get(circuit_breaker_open, Status).

% If true:
% 1. Fix the root cause (database? API?)
% 2. Stop and restart worker:
kanban_worker:stop(WorkerPid),
{ok, NewPid} = kanban_worker:start_link(high, payment).
```

## Next Steps

1. **Read the docs**:
   - `README.md` - Project overview
   - `ARCHITECTURE.md` - System design
   - `docs/tps-reference/20-kanban.md` - Complete guide

2. **Explore the code**:
   - `src/kanban_queue.erl` - Work queue
   - `src/kanban_worker.erl` - Worker processor
   - `src/kanban_coordinator.erl` - Health monitoring
   - `test/kanban_SUITE.erl` - Test examples

3. **Run examples**:
   - See test cases in `test/kanban_SUITE.erl`
   - Each test shows a different pattern

4. **Integrate with your app**:
   - Import `tps_kanban` application
   - Call `kanban_queue:publish()` to submit work
   - Start workers with `kanban_worker:start_link()`
   - Monitor with `kanban_coordinator:get_health()`

## API Quick Reference

### Publishing Work

```erlang
kanban_queue:publish({Priority, Domain, Payload, DeadlineMs})
→ {ok, WorkId} | {error, Reason}
```

### Worker Management

```erlang
kanban_worker:start_link(Priority, Domain)
→ {ok, Pid} | {error, Reason}

kanban_worker:pull_work(Pid)
→ {ok, [Item]} | {error, Reason}

kanban_worker:stop(Pid)
→ ok | {error, Reason}
```

### Acknowledgment

```erlang
kanban_queue:ack(WorkId, Domain)
→ ok | {error, Reason}

kanban_queue:nack(WorkId, Domain, Reason)
→ ok | {error, Reason}
```

### Scheduling

```erlang
kanban_scheduler:schedule_periodic(Priority, Domain, IntervalMs, DeadlineMs)
→ {ok, JobId} | {error, Reason}

kanban_scheduler:cancel_schedule(JobId)
→ ok | {error, Reason}
```

### Monitoring

```erlang
kanban_coordinator:get_health()
→ #{current_workers => int(), ...}

kanban_queue:get_metrics()
→ #{published => int(), acked => int(), ...}
```

## Performance Tips

1. **Tune pull size**: Default 10 items per pull
   - More items = higher throughput, higher memory
   - Fewer items = lower latency, more CPU

2. **Adjust deadlines**: Make realistic for your processing
   - Too aggressive → items timeout, move to dead letter
   - Too lenient → waste resources

3. **Monitor queue depth**: Should be < 100 normally
   - If > 100: add more workers or increase processing speed
   - If 0: reduce scheduling rate or increase deadline

4. **Use NATS for speed**: RabbitMQ fallback if needed
   - NATS: 500-1000 items/sec
   - RabbitMQ: 100-200 items/sec

## Getting Help

- **Docs**: See `/docs/tps-reference/20-kanban.md`
- **Code**: Review `test/kanban_SUITE.erl` for examples
- **Architecture**: See `ARCHITECTURE.md` for deep dive
- **Issues**: Check ggen project repository

---

**Happy Kanban-ing!** 🚀
