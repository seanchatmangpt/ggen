# TPS Distributed Tracing Reference (v1.0.0)

**Production-Grade Observability for Value Stream Mapping**

OpenTelemetry + Jaeger integration for visualizing request flow through autonomic systems.

## Table of Contents

- [Core Concepts](#core-concepts)
- [Architecture](#architecture)
- [Tracing System Components](#tracing-system-components)
- [Span Builders](#span-builders)
- [Value Stream Mapping](#value-stream-mapping)
- [Implementation Guide](#implementation-guide)
- [Jaeger Setup](#jaeger-setup)
- [Configuration](#configuration)
- [Examples](#examples)
- [Troubleshooting](#troubleshooting)
- [Performance Characteristics](#performance-characteristics)

## Core Concepts

### What is Distributed Tracing?

Distributed tracing follows a single request through an entire system, capturing how it moves between components. Each movement becomes a "span" with:

- **Timing**: When did this happen? (start time, duration)
- **Context**: What was happening? (operation name, component)
- **Attributes**: Why did it happen? (decision points, parameters)
- **Events**: Important milestones along the way

### Value Stream Mapping

In Lean Manufacturing, a "value stream" is the complete sequence of activities from raw material to finished product. In software:

```
Request → Component A → Component B → Component C → Response
  |         |           |           |         |
  +-- Transport: Moving data between processes (latency)
  +-- Processing: Doing actual work (computation, DB)
  +-- Waiting: Queueing for resources (thread pool, queue)
  +-- Defects: Failures requiring retry or escalation
```

Tracing reveals:
- **Value-added time**: Useful computation (Processing span latency)
- **Non-value-added time**: Waiting, transport, defects
- **Ratio**: Value / Total time (target: >80%)

### Trace vs Span

- **Trace**: Complete request journey (root span + all children)
- **Span**: Single operation within the trace (unit of work)
- **Parent-Child**: Spans form hierarchy showing work decomposition

Example trace structure:
```
Trace: process_order (trace_id=abc123)
├── Root Span: process_order (100ms)
│   ├── Child: authorize_payment (30ms) [jidoka]
│   ├── Child: pull_inventory (40ms) [kanban]
│   │   └── Child: db_query (25ms) [database]
│   ├── Child: queue_for_shipment (20ms) [heijunka]
│   └── Child: send_confirmation (10ms) [action]
```

## Architecture

### System Overview

```
┌─────────────────────────────────────────────────────────┐
│                    TPS Application                      │
│  (jidoka, kanban, andon, heijunka components)           │
└──────────┬──────────────────────────────────────────────┘
           │ tps_span_builder:*_span(...)
           ↓
┌─────────────────────────────────────────────────────────┐
│                   tps_tracing (gen_server)              │
│  - Root span creation                                    │
│  - Child span creation                                   │
│  - Attribute/event recording                             │
│  - Span latency measurement                              │
│  - Sampling decision                                     │
│  - ETS storage (spans, traces tables)                    │
│  - Batching (100 spans/5 seconds)                        │
└──────────┬──────────────────────────────────────────────┘
           │ Queue for export (ETS)
           ↓
┌─────────────────────────────────────────────────────────┐
│              tps_tracing_exporter (gen_server)           │
│  - Batch collection (100 spans)                          │
│  - Jaeger format conversion                              │
│  - HTTP POST to Jaeger collector                         │
│  - Retry with exponential backoff                        │
│  - Export metrics (stats, latency, errors)               │
│  - Graceful degradation (fail silently)                  │
└──────────┬──────────────────────────────────────────────┘
           │ HTTP (Thrift/Zipkin format)
           ↓
┌─────────────────────────────────────────────────────────┐
│              Jaeger Collector (Docker)                   │
│  - Receives spans                                        │
│  - Indexes by trace_id                                   │
│  - Stores in Elasticsearch/Cassandra                     │
└──────────┬──────────────────────────────────────────────┘
           │ Query API
           ↓
┌─────────────────────────────────────────────────────────┐
│              Jaeger UI (http://localhost:16686)          │
│  - Search traces by tag/service                          │
│  - Visualize span hierarchy                              │
│  - Analyze latency breakdown                             │
│  - Identify bottlenecks                                  │
└─────────────────────────────────────────────────────────┘

Local Analysis (without Jaeger):
           ↓
┌─────────────────────────────────────────────────────────┐
│            tps_tracing_analyzer (gen_server)             │
│  - Waste breakdown (transport, processing, waiting)      │
│  - Bottleneck detection (slowest component)              │
│  - Anomaly detection (P95 comparison)                    │
│  - Value stream metrics (value/total ratio)              │
└─────────────────────────────────────────────────────────┘
```

### Data Flow

1. **Generation** (Application)
   - Component calls `tps_span_builder:*_span(...)` to start span
   - Span created in `tps_tracing` gen_server
   - Stored in ETS table with unique span_id

2. **Recording** (Application)
   - Application records attributes: `tps_tracing:set_span_attribute(...)`
   - Application records events: `tps_tracing:add_span_event(...)`
   - Attributes/events added to ETS span record

3. **Completion** (Application)
   - Component calls `tps_span_builder:*_span_end(...)`
   - Span latency calculated: end_time - start_time
   - Span status set (ok/error)
   - Span marked for export

4. **Batching** (tps_tracing)
   - Every 5 seconds OR when 100 spans queued
   - Spans moved from main table to export queue
   - Sampled spans (based on sampling rate) included

5. **Export** (tps_tracing_exporter)
   - Batch collected from export queue
   - Converted to Jaeger wire format (JSON + Thrift metadata)
   - HTTP POST to Jaeger collector (localhost:6831)
   - Retry with exponential backoff (100ms, 200ms, 400ms)
   - Failed exports dropped (don't break request)

6. **Analysis** (tps_tracing_analyzer)
   - Traces analyzed for value stream mapping
   - Waste categorized: transport, processing, waiting, defects
   - Bottleneck identified (slowest component)
   - Anomaly detected (latency > P95 * 1.5)

## Tracing System Components

### tps_tracing (Main Tracer)

Central span management with:
- Root span creation (traces entered system)
- Child span creation (hierarchical decomposition)
- Span attribute recording (context, decisions)
- Span event recording (milestones)
- Span status tracking (ok, error, unset)
- Sampling enforcement (100% dev, 1% prod)
- ETS-based storage (concurrent access)
- Batch export coordination

**Key Functions:**

```erlang
%% Create root span when request enters system
{ok, TraceId, RootSpanId} = tps_tracing:start_root_span(
    "order-123",           % RequestId
    "order_service",       % Component
    #{"user_id" => "42"}   % Baggage (context)
).

%% Create child spans for sub-operations
{ok, AuthSpanId} = tps_tracing:start_child_span(
    TraceId,               % Link to root trace
    RootSpanId,            % Parent span
    "authorize_payment",   % Operation
    "jidoka"               % Component
).

%% Record attributes (add context)
ok = tps_tracing:set_span_attribute(AuthSpanId, "card_token", "tok_xxx").

%% Record events (mark milestones)
ok = tps_tracing:add_span_event(AuthSpanId, "authorization_received", [
    {"status", "approved"},
    {"amount", "99.99"}
]).

%% End span (with status)
ok = tps_tracing:end_span(AuthSpanId).
ok = tps_tracing:end_span_with_status(ErrorSpanId, {error, "timeout"}).

%% Sampling control
ok = tps_tracing:set_sampling_rate(0.01).  % 1% sampling for production

%% Introspection
{ok, ActiveTraces} = tps_tracing:get_active_traces().
```

### tps_span_builder (Component-Specific Helpers)

Pre-built span creators for each TPS component:

#### Jidoka (Circuit Breaker)

```erlang
%% Circuit check span (latency < 5ms)
{ok, SpanId} = tps_span_builder:jidoka_circuit_check_span(
    TraceId, RootSpanId,
    "payment-circuit",     % Circuit name
    closed                 % Circuit state: open/closed/half_open
).

%% Record circuit decision
ok = tps_span_builder:jidoka_circuit_check_end(
    SpanId,
    pass,                  % pass (allow traffic) or block (reject)
    undefined              % ErrorMessage or undefined
).

%% Attributes recorded automatically:
%% - circuit_name: "payment-circuit"
%% - circuit_state: "closed"
%% - result: "pass" or "block"
%% - error: "error message" (if failed)

%% Events generated:
%% - "circuit_state_checked": when decision made
```

#### Kanban (Queue Management)

```erlang
%% Queue pull span (measure queue wait time)
{ok, PullSpanId} = tps_span_builder:kanban_queue_pull_span(
    TraceId, RootSpanId,
    "work-queue",          % Queue name
    5                      % Current queue depth
).

%% Record work item acquisition
ok = tps_span_builder:kanban_queue_pull_end(
    PullSpanId,
    "work-item-123",       % Work item ID
    "P1",                  % Priority
    1500                   % Queue wait time (microseconds)
).

%% Queue acknowledge span
{ok, AckSpanId} = tps_span_builder:kanban_queue_ack_span(
    TraceId, RootSpanId,
    "work-item-123",
    "work-queue"
).

%% Record acknowledgement
ok = tps_span_builder:kanban_queue_ack_end(
    AckSpanId,
    success,               % success or failure
    undefined              % ErrorMessage or undefined
).

%% Attributes recorded:
%% - queue_depth: 5
%% - work_item_id: "work-item-123"
%% - priority: "P1"
%% - queue_wait_time_us: 1500
```

#### Andon (Alert System)

```erlang
%% Alert event span
{ok, SpanId} = tps_span_builder:andon_log_event_span(
    TraceId, RootSpanId,
    "latency_anomaly",     % Event type
    high                   % Severity: critical/high/medium/low
).

%% Record alert handling
ok = tps_span_builder:andon_log_event_end(
    SpanId,
    true,                  % Acknowledged by handler
    "ops-team",            % Handler name
    "Optimized DB query"   % Action taken
).

%% Attributes recorded:
%% - event_type: "latency_anomaly"
%% - severity: "high"
%% - acknowledged: true
%% - handler: "ops-team"
%% - message: "Optimized DB query"
```

#### Heijunka (Resource Leveling)

```erlang
%% Pool acquire span (measure resource acquisition)
{ok, SpanId} = tps_span_builder:heijunka_pool_acquire_span(
    TraceId, RootSpanId,
    "worker-pool",         % Pool name
    0.75                   % Current utilization (0.0-1.0)
).

%% Record resource acquisition
ok = tps_span_builder:heijunka_pool_acquire_end(
    SpanId,
    "worker-5",            % Acquired resource ID
    250,                   % Time to acquire (microseconds)
    true                   % Success or failure
).

%% Attributes recorded:
%% - pool_name: "worker-pool"
%% - pool_utilization: 0.75
%% - resource_id: "worker-5"
%% - acquire_time_us: 250
```

#### Action Execute

```erlang
%% Top-level action span
{ok, ActionSpanId} = tps_span_builder:action_execute_span(
    TraceId, RootSpanId,
    "process_payment",     % Action name
    "payment"              % Action type
).

%% Record action result
ok = tps_span_builder:action_execute_end(
    ActionSpanId,
    success,               % success or failure
    undefined,             % ErrorMessage or undefined
    #{
        "amount" => "99.99",
        "currency" => "USD",
        "processor" => "stripe"
    }                      % Additional metadata
).
```

### tps_tracing_exporter (Jaeger Integration)

Exports spans to Jaeger for visualization:

**Features:**
- Batching (100 spans per batch OR every 5 seconds)
- Format conversion (OpenTelemetry → Jaeger Thrift)
- HTTP transport (POST to localhost:6831)
- Retry logic (exponential backoff: 100ms → 200ms → 400ms)
- Graceful degradation (failed exports don't break requests)
- Export statistics (tracking, debugging)

**Configuration (Erlang app env):**

```erlang
%% In your app config or application start
application:set_env(ggen, jaeger_host, "localhost"),
application:set_env(ggen, jaeger_port, 6831),
application:set_env(ggen, jaeger_enabled, true),
```

**API:**

```erlang
%% Start exporter
{ok, Pid} = tps_tracing_exporter:start_link().

%% Queue span for export (called internally by tps_tracing)
ok = tps_tracing_exporter:export_span(SpanData).

%% Get export statistics
{ok, Stats} = tps_tracing_exporter:get_export_stats().
%% Returns: #{
%%     total_spans => 1250,
%%     successful_exports => 12,
%%     failed_exports => 1,
%%     retry_count => 2,
%%     last_export_time => 1642345612000000
%% }

%% Change Jaeger endpoint (for failover)
ok = tps_tracing_exporter:set_jaeger_endpoint("jaeger-prod.example.com", 6831).
```

### tps_tracing_analyzer (Value Stream Mapping)

Analyzes traces to identify waste and bottlenecks:

**Waste Categories:**
- **Transport**: Moving data between processes (latency between spans)
- **Processing**: Actual useful work (computation, DB operations)
- **Waiting**: Queueing for resources (queue time, pool wait)
- **Defects**: Failures (error spans)

**Metrics:**
- **Total Latency**: End-to-end request time
- **Value-Added Time**: Useful computation (Processing waste type)
- **Non-Value-Added Time**: Waiting + Transport + Defects
- **Ratio**: Value-Added / Total (target: >80%)
- **Bottleneck**: Slowest component
- **Anomaly**: Is this request slower than normal?

**API:**

```erlang
%% Start analyzer
{ok, Pid} = tps_tracing_analyzer:start_link().

%% Analyze single trace
{ok, Report} = tps_tracing_analyzer:analyze_trace(#{
    trace_id => TraceId,
    spans => [Span1, Span2, Span3, ...],
    expected_latency => 100000  % Optional baseline (microseconds)
}).
%% Returns: #{
%%     trace_id => TraceId,
%%     total_latency => 95000,
%%     component_breakdown => #{
%%         "jidoka" => 30000,
%%         "kanban" => 45000,
%%         "action" => 20000
%%     },
%%     value_added_time => 20000,       % Processing only
%%     non_value_added_time => 75000,   % Waiting + Transport
%%     ratio => 0.21,                    % Only 21% value-added (needs improvement!)
%%     waste_breakdown => #{
%%         processing => 20000,
%%         waiting => 60000,
%%         transport => 10000,
%%         defect => 5000
%%     },
%%     bottleneck_component => "kanban", % Slowest component
%%     anomaly_detected => true          % > P95 * 1.5
%% }

%% Analyze batch of traces
{ok, Reports} = tps_tracing_analyzer:analyze_trace_batch([Trace1, Trace2, ...]).

%% Get component latencies (average + P95)
{ok, Latencies} = tps_tracing_analyzer:get_component_latencies().
%% Returns: [
%%     {"database", 45000, 78000},    % {component, avg_latency, P95}
%%     {"jidoka", 5000, 12000},
%%     {"kanban", 25000, 42000}
%% ]

%% Identify bottleneck (slowest on average)
{ok, {Component, AvgLatency}} = tps_tracing_analyzer:get_bottleneck().
%% Returns: {"database", 45000}

%% Detect anomaly for current request
{ok, IsAnomaly, Reason} = tps_tracing_analyzer:detect_anomaly(Trace).
%% Returns: {ok, true, "Latency 150000 us exceeds P95 threshold 120000 us"}

%% Get aggregated waste report
{ok, AggregateReport} = tps_tracing_analyzer:get_waste_report().
%% Returns aggregated waste breakdown across all traces
```

## Value Stream Mapping

### Understanding Waste

In Lean Manufacturing, "muda" (waste) comes in 7 types. In software systems:

#### 1. Transport (Moving Data)

**What it is:** Time spent moving data between processes/services

**Examples:**
- Network latency between services
- Message queue transit time
- Database query network round-trip

**How to identify in traces:**
- Look for spans with component="jidoka" (circuit checks)
- Spans connecting components that are geographically distant
- Parent span latency > sum of child spans

**Optimization:**
```
BEFORE: Request → Service A → Service B → Service C (50ms latency)
         Network delay: 10ms between each service

AFTER:  Request → Service A+B+C combined (25ms latency)
        Collocate related services, use local APIs
```

**Measurement:**
```erlang
%% In tps_tracing_analyzer waste_breakdown:
%% transport => 15000  % microseconds of transport overhead
```

#### 2. Processing (Actual Work)

**What it is:** Time spent doing useful computation

**Examples:**
- Database queries (SELECT, UPDATE, INSERT)
- Business logic computation
- API calls to external services (when necessary)

**Characteristics:**
- High value-add (directly moves toward customer goal)
- Should be optimized but not eliminated
- Reduce via better algorithms, caching, parallelization

**Measurement:**
```erlang
%% In tps_tracing_analyzer:
%% value_added_time => 25000  % Only processing counts as value-added
%% ratio => 0.35  % 35% of request is actual value-added work
```

#### 3. Waiting (Queueing)

**What it is:** Time spent waiting for resources

**Examples:**
- Queue depth in kanban system
- Thread pool exhaustion (heijunka)
- Lock contention
- DNS resolution timeouts

**How to identify:**
- Spans with component="kanban" (queue wait)
- Spans with component="heijunka" (pool wait)
- High queue_depth attributes
- Large latency with no sub-spans

**Optimization:**
```
Root cause analysis:
1. Why is queue so deep?
   - Too many requests coming in too fast? → Rate limit
   - Not processing fast enough? → Improve processing speed
   - Spiky load? → Better load leveling (heijunka)

2. Why is thread pool exhausted?
   - Too few threads? → Increase pool size
   - Threads blocked? → Fix blocking operations
   - Cascading failures? → Add circuit breaker (jidoka)
```

**Measurement:**
```erlang
%% In tps_tracing_analyzer waste_breakdown:
%% waiting => 45000  % 45ms waiting for resources
```

#### 4. Defects (Failures)

**What it is:** Work that must be redone due to failures

**Examples:**
- Failed API calls (retry loop)
- Database constraint violations
- Timeout and retry cycles
- Exception handling overhead

**Impact:**
- Not just the error span latency, but retry latency
- Each retry re-executes all previous work
- Exponential cost: 1 retry = 2x latency, 2 retries = 3x latency

**Example:**
```
Single attempt: Request → [Process] → [API call] → Response (100ms)

With 1 retry:   Request → [Process] → [API call FAILED]
                         → [Process] → [API call OK] → Response (200ms)
                Latency doubled!

With 2 retries: Request → [Process] → [API call FAILED]
                         → [Process] → [API call FAILED]
                         → [Process] → [API call OK] → Response (300ms)
                Latency tripled!
```

**Prevention (Andon signals):**
```erlang
%% Track defect rate
error_count => 3,  % 3 errors in this trace

%% In tps_tracing_analyzer waste_breakdown:
%% defect => 150000  % 150ms of retry overhead
```

### Example: Order Processing Value Stream

```
Customer places order (trace_id=order-123)

Request comes in (root span)
├─ jidoka: Check circuit breaker (5ms) [TRANSPORT]
│  └ Attributes: circuit_name="order-processor", state="closed"
│
├─ kanban: Pull work from queue (50ms) [WAITING]
│  └ Attributes: queue_depth=12, wait_time=50ms
│
├─ Action: Authorize payment (30ms) [PROCESSING]
│  ├─ jidoka: Circuit check (2ms) [TRANSPORT]
│  └─ External API: Stripe (25ms) [PROCESSING]
│
├─ kanban: Fetch inventory (60ms) [WAITING + PROCESSING]
│  ├─ heijunka: Wait for DB connection (15ms) [WAITING]
│  └─ action: Query DB (40ms) [PROCESSING]
│
└─ heijunka: Queue for shipment service (5ms) [WAITING]

Total latency: 150ms
Value-added time: 30 (auth) + 40 (db) = 70ms
Non-value-added time: 5 + 50 + 2 + 15 + 5 = 77ms
Ratio: 70 / 150 = 47% (needs improvement!)

Waste breakdown:
- Transport (circuit checks): 7ms (5%)
- Processing (auth + db): 70ms (47%)
- Waiting (queue + pool): 65ms (43%)
- Defects: 8ms (5%) [1 payment retry]

Action items (in priority order):
1. Reduce queue wait (50ms) → Add more workers to queue processor
2. Reduce pool wait (15ms) → Increase DB connection pool
3. Reduce retry rate (8ms) → Improve payment API timeout handling
4. Profile auth API (25ms) → Consider async payment validation
```

### Identifying Your Bottleneck

**Using tps_tracing_analyzer:**

```erlang
%% Method 1: Get bottleneck component
{ok, {BottleneckComponent, AvgLatency}} =
    tps_tracing_analyzer:get_bottleneck().
%% Response: {"database", 45000}
%% → Database is slowest on average (45ms per request)

%% Method 2: Get latency breakdown by component
{ok, ComponentLatencies} =
    tps_tracing_analyzer:get_component_latencies().
%% Response: [
%%     {"database", 45000, 78000},  % Avg 45ms, P95 78ms
%%     {"kanban", 25000, 42000},
%%     {"jidoka", 5000, 12000}
%% ]
%% → Focus on database first (slowest average)

%% Method 3: Analyze single trace for waste
{ok, TraceReport} =
    tps_tracing_analyzer:analyze_trace(#{
        trace_id => TraceId,
        spans => Spans
    }).
%% Look at: bottleneck_component field
```

### Optimization Strategy (80/20)

**The 80/20 principle in value stream mapping:**
- 20% of components cause 80% of the latency
- Fix the slowest component first
- Measure before and after (confirm improvement)

**Optimization workflow:**

```
1. MEASURE (baseline)
   {ok, Report1} = tps_tracing_analyzer:get_waste_report().
   Baseline: 50% value-added ratio (100ms out of 200ms)

2. IDENTIFY (bottleneck)
   {ok, {"database", 65000}} = tps_tracing_analyzer:get_bottleneck().
   Database is slowest (65ms average)

3. HYPOTHESIZE
   Current DB query: SELECT * FROM orders (full table scan)
   Hypothesis: Add index on order_id, reduce from 65ms to 20ms

4. IMPLEMENT
   CREATE INDEX orders_id ON orders(order_id);

5. MEASURE (after)
   {ok, Report2} = tps_tracing_analyzer:get_waste_report().
   After: 75% value-added ratio (improvement!)
   Database latency: 20ms (was 65ms)

6. VALIDATE
   Total latency: 125ms (was 200ms)
   Improvement: 37.5% faster
   Target: Continue optimizing until ratio > 80%
```

## Implementation Guide

### Step 1: Enable Tracing in Your Application

In your application's start function:

```erlang
-module(my_app).
-behaviour(application).

start(_StartType, _StartArgs) ->
    %% Configure tracing
    application:set_env(ggen, tracing_sampling_rate, 1.0),  % 100% in dev
    application:set_env(ggen, jaeger_enabled, true),
    application:set_env(ggen, jaeger_host, "localhost"),
    application:set_env(ggen, jaeger_port, 6831),

    %% Start tracing infrastructure
    {ok, _} = tps_tracing:start_link(),
    {ok, _} = tps_tracing_exporter:start_link(),
    {ok, _} = tps_tracing_analyzer:start_link(),

    %% Start your supervisors
    my_app_sup:start_link().
```

### Step 2: Instrument Your Components

For each component, add tracing around the operation:

**Example: Jidoka Circuit Breaker**

```erlang
-module(my_jidoka).

check_circuit(TraceId, ParentSpanId, CircuitName) ->
    %% Create span
    {ok, SpanId} = tps_span_builder:jidoka_circuit_check_span(
        TraceId, ParentSpanId, CircuitName, closed
    ),

    %% Check circuit
    try
        case is_circuit_open(CircuitName) of
            true ->
                ok = tps_span_builder:jidoka_circuit_check_end(
                    SpanId, block, "Circuit is open"
                ),
                {error, circuit_open};
            false ->
                ok = tps_span_builder:jidoka_circuit_check_end(
                    SpanId, pass, undefined
                ),
                {ok, allow}
        end
    catch
        _:Error ->
            ok = tps_span_builder:jidoka_circuit_check_end(
                SpanId, block, io_lib:format("~p", [Error])
            ),
            {error, Error}
    end.
```

**Example: Kanban Queue**

```erlang
-module(my_kanban).

pull_work(TraceId, ParentSpanId, QueueName) ->
    StartTime = erlang:system_time(microsecond),

    %% Create span
    {ok, PullSpanId} = tps_span_builder:kanban_queue_pull_span(
        TraceId, ParentSpanId, QueueName, queue_depth(QueueName)
    ),

    %% Pull from queue
    case queue:get(QueueName) of
        {ok, WorkItem} ->
            WaitTime = erlang:system_time(microsecond) - StartTime,
            ok = tps_span_builder:kanban_queue_pull_end(
                PullSpanId,
                WorkItem#work_item.id,
                WorkItem#work_item.priority,
                WaitTime
            ),
            {ok, WorkItem};
        {error, empty} ->
            WaitTime = erlang:system_time(microsecond) - StartTime,
            ok = tps_span_builder:kanban_queue_pull_end(
                PullSpanId,
                "empty",
                "none",
                WaitTime
            ),
            {error, empty}
    end.
```

### Step 3: Add Root Span at Request Entry Point

When a request enters your system, create a root span:

```erlang
-module(my_request_handler).

handle_request(Request) ->
    RequestId = Request#request.id,
    ComponentName = "request_handler",
    Baggage = #{
        "request_id" => RequestId,
        "user_id" => Request#request.user_id,
        "method" => atom_to_list(Request#request.method),
        "path" => Request#request.path
    },

    %% Create root span
    {ok, TraceId, RootSpanId} = tps_tracing:start_root_span(
        RequestId, ComponentName, Baggage
    ),

    try
        %% Process request (pass trace IDs through your system)
        Result = process_request(Request, TraceId, RootSpanId),

        %% End root span
        ok = tps_tracing:end_span(RootSpanId),
        Result
    catch
        _:Error ->
            ok = tps_tracing:end_span_with_status(RootSpanId, {error, io_lib:format("~p", [Error])}),
            {error, Error}
    end.

process_request(Request, TraceId, RootSpanId) ->
    %% Create child spans for sub-operations
    {ok, AuthSpanId} = tps_tracing:start_child_span(
        TraceId, RootSpanId, "authorize", "auth"
    ),
    %% ... authentication logic ...
    ok = tps_tracing:end_span(AuthSpanId),

    %% Continue with other components
    {ok, ProcessSpanId} = tps_tracing:start_child_span(
        TraceId, RootSpanId, "process", "processor"
    ),
    %% ... processing logic ...
    ok = tps_tracing:end_span(ProcessSpanId),

    {ok, Result}.
```

### Step 4: Configure Sampling for Production

```erlang
%% Development (100% tracing - more data)
application:set_env(ggen, tracing_sampling_rate, 1.0).

%% Production (1% tracing - less overhead)
application:set_env(ggen, tracing_sampling_rate, 0.01).

%% Adjust dynamically
tps_tracing:set_sampling_rate(0.05).  % 5% sampling
```

### Step 5: Query Traces with Analysis

```erlang
%% After request completes, analyze the trace
{ok, TraceId, _} = tps_tracing:start_root_span(...),
%% ... request processing ...

%% Later: analyze
{ok, WasteReport} = tps_tracing_analyzer:analyze_trace(#{
    trace_id => TraceId,
    spans => get_trace_spans(TraceId),
    expected_latency => 100000  % Target: 100ms
}).

%% Check for anomalies
case maps:get(anomaly_detected, WasteReport) of
    true ->
        %% Send alert
        alert_ops("Slow request: " ++ atom_to_list(TraceId));
    false ->
        ok
end.

%% Check waste ratio
case maps:get(ratio, WasteReport) of
    Ratio when Ratio < 0.5 ->
        %% Send optimization opportunity
        log_improvement_needed(TraceId, Ratio);
    _ ->
        ok
end.
```

## Jaeger Setup

### Quick Start (Docker Compose)

```yaml
# docker-compose.yml
version: '3.8'

services:
  jaeger:
    image: jaegertracing/all-in-one:latest
    ports:
      - "6831:6831/udp"    # Thrift compact agent port
      - "16686:16686"      # Jaeger UI port
      - "14268:14268"      # HTTP collector port
    environment:
      - COLLECTOR_ZIPKIN_HTTP_PORT=9411
    container_name: jaeger
```

Start Jaeger:

```bash
docker-compose up -d jaeger

# Verify it's running
curl http://localhost:16686/api/services
```

### Configuration in Erlang

```erlang
%% sys.config
[
  {ggen, [
    {jaeger_enabled, true},
    {jaeger_host, "localhost"},
    {jaeger_port, 6831},
    {tracing_sampling_rate, 1.0}
  ]}
].
```

### Troubleshooting Jaeger Connection

```erlang
%% Check if Jaeger is reachable
{ok, _} = gen_tcp:connect("localhost", 6831, [], 1000).

%% Check export stats
{ok, Stats} = tps_tracing_exporter:get_export_stats().
maps:get(failed_exports, Stats).

%% Enable logging
application:set_env(kernel, logger_level, debug).
```

## Configuration

### Application Environment Variables

```erlang
%% In sys.config or at runtime
[
  {ggen, [
    % Tracing
    {tracing_sampling_rate, 1.0},      % 1.0 = 100%, 0.01 = 1%

    % Jaeger
    {jaeger_enabled, true},             % Enable/disable Jaeger export
    {jaeger_host, "localhost"},         % Jaeger collector hostname
    {jaeger_port, 6831},                % Jaeger collector port

    % Export batching
    {jaeger_batch_size, 100},           % Spans per batch
    {jaeger_export_interval, 5000}      % Export every 5 seconds (ms)
  ]}
].
```

### Runtime Configuration

```erlang
%% Change sampling rate
tps_tracing:set_sampling_rate(0.05).

%% Change Jaeger endpoint (for failover)
tps_tracing_exporter:set_jaeger_endpoint("jaeger-backup.example.com", 6831).

%% Get current settings
{ok, Stats} = tps_tracing_exporter:get_export_stats().
```

## Examples

### Complete Request Example

```erlang
%% Process an order with full tracing
process_order(Order) ->
    %% 1. Root span at entry
    {ok, TraceId, RootSpanId} = tps_tracing:start_root_span(
        Order#order.id,
        "order_service",
        #{
            "user_id" => Order#order.user_id,
            "total_amount" => Order#order.total
        }
    ),

    try
        %% 2. Authorization (with circuit breaker)
        {ok, AuthSpanId} = tps_span_builder:jidoka_circuit_check_span(
            TraceId, RootSpanId, "payment-processor", closed
        ),
        case process_payment(Order, TraceId, AuthSpanId) of
            {ok, PaymentId} ->
                ok = tps_span_builder:jidoka_circuit_check_end(
                    AuthSpanId, pass, undefined
                ),
                ok = tps_tracing:set_span_attribute(
                    AuthSpanId, "payment_id", PaymentId
                );
            {error, Reason} ->
                ok = tps_span_builder:jidoka_circuit_check_end(
                    AuthSpanId, block, io_lib:format("~p", [Reason])
                ),
                throw({payment_failed, Reason})
        end,

        %% 3. Inventory check (with queue)
        {ok, InventorySpanId} = tps_span_builder:kanban_queue_pull_span(
            TraceId, RootSpanId, "inventory-queue", 3
        ),
        case check_inventory(Order, TraceId, InventorySpanId) of
            {ok, ReservationId} ->
                ok = tps_tracing:set_span_attribute(
                    InventorySpanId, "reservation_id", ReservationId
                ),
                ok = tps_span_builder:kanban_queue_pull_end(
                    InventorySpanId,
                    Order#order.id,
                    "P1",
                    500
                );
            {error, OutOfStock} ->
                ok = tps_tracing:end_span_with_status(
                    InventorySpanId, {error, "out_of_stock"}
                ),
                throw({out_of_stock, OutOfStock})
        end,

        %% 4. Queue for shipment (with pool)
        {ok, ShipSpanId} = tps_span_builder:heijunka_pool_acquire_span(
            TraceId, RootSpanId, "shipment-workers", 0.65
        ),
        ShipmentId = queue_for_shipment(Order),
        ok = tps_span_builder:heijunka_pool_acquire_end(
            ShipSpanId, ShipmentId, 250, true
        ),

        %% 5. Root span success
        ok = tps_tracing:set_span_attribute(RootSpanId, "order_status", "completed"),
        ok = tps_tracing:end_span(RootSpanId),

        {ok, ShipmentId}

    catch
        _:Error:Stacktrace ->
            error_logger:error_msg("Order processing failed: ~p~n~p~n",
                [Error, Stacktrace]),
            ok = tps_tracing:end_span_with_status(
                RootSpanId,
                {error, io_lib:format("~p", [Error])}
            ),
            {error, Error}
    end.
```

### Analytics Query Example

```erlang
%% Find all slow requests in the last hour
{ok, ActiveTraces} = tps_tracing:get_active_traces(),
{ok, AllReports} = tps_tracing_analyzer:analyze_trace_batch([
    get_trace_data(TraceId) || TraceId <- ActiveTraces
]),

%% Filter for anomalies
SlowRequests = [R || R <- AllReports,
    maps:get(anomaly_detected, R) =:= true],

%% Group by bottleneck component
Bottlenecks = lists:foldl(fun(R, Acc) ->
    Component = maps:get(bottleneck_component, R),
    Count = maps:get(Component, Acc, 0),
    Acc#{Component => Count + 1}
end, #{}, SlowRequests),

%% Top bottleneck
[{TopComponent, _}|_] = lists:sort(fun({_,A}, {_,B}) -> A >= B end,
    maps:to_list(Bottlenecks)),

io:format("Top bottleneck in slow requests: ~s~n", [TopComponent]).
```

## Troubleshooting

### Problem: Spans Not Appearing in Jaeger UI

**Diagnosis:**

```erlang
%% 1. Check if exporter is running
{ok, Stats} = tps_tracing_exporter:get_export_stats().
maps:get(total_spans, Stats).  % Should be > 0

%% 2. Check failed exports
maps:get(failed_exports, Stats).  % Should be 0 or low

%% 3. Check Jaeger connectivity
gen_tcp:connect("localhost", 6831, [], 1000).

%% 4. Verify Jaeger is running
curl http://localhost:16686/api/services
```

**Solutions:**

- Ensure Jaeger is running: `docker ps | grep jaeger`
- Check configuration: `application:get_env(ggen, jaeger_host)`
- Verify network: `telnet localhost 6831`
- Check logs: `error_logger` for export errors
- Enable debug logging: `application:set_env(kernel, logger_level, debug)`

### Problem: Too Many Spans (Performance Issue)

**Solutions:**

```erlang
%% Reduce sampling rate
tps_tracing:set_sampling_rate(0.01).  % 1% instead of 100%

%% Disable Jaeger export (for local analysis only)
application:set_env(ggen, jaeger_enabled, false).

%% Batch size adjustment
application:set_env(ggen, jaeger_batch_size, 500).  % Export fewer, larger batches

%% Increase export interval
application:set_env(ggen, jaeger_export_interval, 10000).  % 10 seconds
```

### Problem: Latency Measurements Seem Wrong

**Debugging:**

```erlang
%% Check actual span latencies
[{SpanId, SpanData}] = ets:lookup(tps_spans, SpanId),
Latency = maps:get(latency, SpanData).

%% Verify start/end times
StartTime = maps:get(start_time, SpanData),
EndTime = maps:get(end_time, SpanData),
CalculatedLatency = EndTime - StartTime.

%% Check system time source
erlang:system_time(microsecond).
```

**Common causes:**

- System clock adjustment (NTP sync)
- Span not ended properly
- Time units confusion (microseconds vs milliseconds)

## Performance Characteristics

### Latency Overhead

| Operation | Overhead | Notes |
|-----------|----------|-------|
| Start span | <100 µs | ETS insert, ID generation |
| Set attribute | <50 µs | ETS update |
| Add event | <50 µs | ETS update |
| End span | <100 µs | Latency calculation, ETS update |
| Export batch | <50 ms | HTTP request to Jaeger |

### Memory Usage

- Per span: ~2-4 KB (attributes, events, metadata)
- Batching reduces memory: 100 spans → 1 HTTP request
- ETS tables are periodically cleaned up

### Sampling Impact

```
100% sampling:   1000 spans/sec → All exported → High Jaeger load
10% sampling:    1000 spans/sec → 100 exported → Manageable load
1% sampling:     1000 spans/sec → 10 exported → Low load
```

### Production Recommendations

```erlang
%% Development
{tracing_sampling_rate, 1.0},      % See all requests
{jaeger_enabled, true},

%% Production
{tracing_sampling_rate, 0.01},     % 1% sampling
{jaeger_enabled, true},
{jaeger_batch_size, 500},          % Larger batches
{jaeger_export_interval, 10000},   % Less frequent exports
```

## References

- [OpenTelemetry Specification](https://opentelemetry.io/docs/concepts/what-is-opentelemetry/)
- [Jaeger Documentation](https://www.jaegertracing.io/docs/)
- [Lean Manufacturing - Value Stream Mapping](https://en.wikipedia.org/wiki/Value_stream_mapping)
- [System Design: Distributed Tracing](https://en.wikipedia.org/wiki/Tracing_(software))

---

**Document Version**: 1.0.0
**Last Updated**: 2026-01-25
**Status**: Production-Ready
