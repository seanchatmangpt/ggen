# API Reference: Generated Modules

**Complete API documentation for all generated Erlang modules.**

**Generated from:** `.specify/specs/001-job-queue/feature.ttl`

---

## Module: job_queue

Job queue management with NATS backend and RabbitMQ fallback.

### Types

```erlang
-type priority() :: high | normal | low.
-type domain() :: atom().
-type job_id() :: string().
-type payload() :: map().
-type deadline() :: integer().  % Unix timestamp (milliseconds)
```

### API Functions

#### start_link/0

```erlang
-spec start_link() -> {ok, pid()} | {error, term()}.
```

Start the job queue server.

**Returns:**
- `{ok, Pid}` on success
- `{error, Reason}` on failure

**Example:**
```erlang
{ok, Pid} = job_queue:start_link().
```

---

#### publish/4

```erlang
-spec publish(priority(), domain(), payload(), deadline()) ->
    {ok, job_id()} | {error, term()}.
```

Publish a job to the queue with priority, domain, payload, and deadline.

**Parameters:**
- `Priority` - Job priority (`high`, `normal`, `low`)
- `Domain` - Domain for job classification (atom, e.g., `payment`, `fraud`)
- `Payload` - Job data (map)
- `Deadline` - Unix timestamp in milliseconds when job expires

**Returns:**
- `{ok, JobId}` - Unique job identifier
- `{error, Reason}` - Publication failed

**Example:**
```erlang
Payload = #{transaction_id => <<"txn-123">>, amount => 100},
Deadline = erlang:system_time(millisecond) + 60000,  % 1 minute from now

{ok, JobId} = job_queue:publish(high, payment, Payload, Deadline).
```

---

#### subscribe/2

```erlang
-spec subscribe(priority(), domain()) -> ok | {error, term()}.
```

Subscribe the calling process to receive jobs for a specific priority/domain combination.

**Parameters:**
- `Priority` - Priority level to subscribe to
- `Domain` - Domain to subscribe to

**Returns:**
- `ok` on successful subscription
- `{error, Reason}` on failure

**Example:**
```erlang
ok = job_queue:subscribe(high, payment).

% Receive jobs in mailbox:
receive
    {job, JobId, Payload} ->
        process_job(JobId, Payload)
end.
```

---

#### get_metrics/0

```erlang
-spec get_metrics() -> #{atom() => integer()}.
```

Get queue metrics (published, processed, failed, retried).

**Returns:** Map of metric counters

**Example:**
```erlang
Metrics = job_queue:get_metrics().
% => #{published => 1234, processed => 1100, failed => 12, retried => 45}
```

---

#### get_job_status/1

```erlang
-spec get_job_status(job_id()) -> {ok, job_status()} | {error, not_found}.
```

Get the current status of a job.

**Parameters:**
- `JobId` - Job identifier

**Returns:**
- `{ok, Status}` where Status is `pending | processing | completed | failed | deadline_exceeded`
- `{error, not_found}` if job doesn't exist

**Example:**
```erlang
{ok, Status} = job_queue:get_job_status("job-123").
```

---

## Module: job_worker_pool

Poolboy-managed worker pool for job processing.

### API Functions

#### start_link/1

```erlang
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
```

Start the worker pool with configuration.

**Parameters:**
- `Config` - Map with keys:
  - `pool_size` (integer, default 10)
  - `max_overflow` (integer, default 5)
  - `pull_size` (integer, default 5)
  - `process_timeout` (integer, default 30000ms)

**Example:**
```erlang
Config = #{
    pool_size => 20,
    max_overflow => 10,
    pull_size => 10,
    process_timeout => 60000
},
{ok, Pid} = job_worker_pool:start_link(Config).
```

---

#### pool_status/0

```erlang
-spec pool_status() -> #{atom() => integer()}.
```

Get current pool status.

**Returns:** Map with:
- `available_workers` - Idle workers
- `overflow_workers` - Temporary overflow workers active
- `monitored_workers` - Total workers being monitored

**Example:**
```erlang
Status = job_worker_pool:pool_status().
% => #{available_workers => 8, overflow_workers => 2, monitored_workers => 20}
```

---

## Module: job_worker

Individual worker implementation (gen_server).

### API Functions

#### start_link/1

```erlang
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
```

Start a worker with configuration.

**Example:**
```erlang
Config = #{priority => high, domain => payment, pull_size => 5},
{ok, Pid} = job_worker:start_link(Config).
```

---

#### get_status/1

```erlang
-spec get_status(pid()) -> #{atom() => term()}.
```

Get worker status.

**Returns:** Map with worker ID, priority, domain, circuit breaker status, metrics.

**Example:**
```erlang
Status = job_worker:get_status(WorkerPid).
% => #{
%     worker_id => "worker-high-payment-123",
%     priority => high,
%     domain => payment,
%     circuit_breaker_open => false,
%     metrics => #{pulled => 100, processed => 95, failed => 5}
% }
```

---

## Module: job_scheduler

Periodic and delayed job scheduling.

### API Functions

#### schedule_periodic/4

```erlang
-spec schedule_periodic(priority(), domain(), integer(), integer()) ->
    {ok, job_id()} | {error, term()}.
```

Schedule a job to run periodically.

**Parameters:**
- `Priority` - Job priority
- `Domain` - Job domain
- `IntervalMs` - Interval in milliseconds
- `DeadlineMs` - Deadline for each job execution

**Example:**
```erlang
% Run every 5 minutes with 30s deadline
{ok, JobId} = job_scheduler:schedule_periodic(normal, billing, 300000, 30000).
```

---

#### schedule_delayed/5

```erlang
-spec schedule_delayed(priority(), domain(), payload(), integer(), deadline()) ->
    {ok, job_id()} | {error, term()}.
```

Schedule a one-time delayed job.

**Parameters:**
- `Priority` - Job priority
- `Domain` - Job domain
- `Payload` - Job data
- `DelayMs` - Delay in milliseconds
- `Deadline` - Job deadline

**Example:**
```erlang
% Run once after 1 hour
Payload = #{reminder => <<"Payment due">>},
{ok, JobId} = job_scheduler:schedule_delayed(high, notifications, Payload, 3600000, 60000).
```

---

## Error Handling

All functions return `{ok, Result}` or `{error, Reason}`.

**Common error reasons:**
- `pool_exhausted` - Worker pool is full (no available workers)
- `circuit_breaker_open` - Worker circuit breaker triggered
- `deadline_exceeded` - Job deadline passed before processing
- `max_retries_exceeded` - Job failed and exhausted retry attempts
- `nats_connection_failed` - NATS backend unavailable
- `timeout` - Operation timed out

**Example error handling:**
```erlang
case job_queue:publish(high, payment, Payload, Deadline) of
    {ok, JobId} ->
        logger:info("Published job ~s", [JobId]);
    {error, pool_exhausted} ->
        logger:warning("Worker pool exhausted, job not published");
    {error, Reason} ->
        logger:error("Failed to publish job: ~p", [Reason])
end.
```

---

## Prometheus Metrics

All modules export Prometheus metrics via `prometheus_text_format:format/0`.

**Available metrics:**
- `jobs_published_total` (counter)
- `jobs_processed_total` (counter)
- `jobs_failed_total` (counter)
- `jobs_deadline_exceeded_total` (counter)
- `jobs_retried_total` (counter)
- `job_processing_duration_ms` (histogram)
- `worker_pool_exhausted_total` (counter)
- `circuit_breaker_opened_total` (counter)
- `circuit_breaker_closed_total` (counter)

**Query metrics:**
```erlang
prometheus_text_format:format().
```

---

**See also:**
- [Configuration Options](04-configuration.md)
- [RDF Ontology Reference](02-rdf-ontology.md)
- [CLI Command Reference](05-cli-reference.md)

---

**Generated by ggen v6.0.0 | 2026-01-29**
