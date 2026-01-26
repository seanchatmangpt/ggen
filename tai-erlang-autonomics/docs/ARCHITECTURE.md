# TAI Erlang Autonomics - System Architecture

## Overview

TAI Erlang Autonomics is a production-grade runtime for autonomous SKU management on Google Cloud Platform (GCP). The system implements a distributed, fault-tolerant architecture using Erlang/OTP principles for reliable autonomic control of entitlements and resources.

## Core Architecture

### Layered Design

```
┌─────────────────────────────────────────────────────────┐
│ HTTP Layer (Cowboy HTTP Server)                         │
│ ├─ /health - Readiness probe                           │
│ ├─ /pubsub - Pub/Sub event ingestion                   │
│ └─ /marketplace - Entitlement action handling          │
└────────────────┬────────────────────────────────────────┘
                 │
┌─────────────────┴────────────────────────────────────────┐
│ Request Handler Layer (tai_http_handler)                 │
│ ├─ Payload validation                                   │
│ ├─ Signature verification (JWT)                         │
│ └─ Request routing                                      │
└────────────────┬────────────────────────────────────────┘
                 │
┌─────────────────┴────────────────────────────────────────┐
│ Autonomic Governance Layer                               │
│ ├─ tai_governor (gen_statem)                            │
│ │  ├─ Entitlement state machine                         │
│ │  ├─ Action execution with bounded concurrency         │
│ │  └─ Receipt emission                                  │
│ ├─ Tenant Governors (per-tenant isolation)              │
│ └─ Resource Governors (quota, billing, compliance)      │
└────────────────┬────────────────────────────────────────┘
                 │
┌─────────────────┴────────────────────────────────────────┐
│ Persistence & State Layer                                │
│ ├─ Firestore (Receipt Ledger)                           │
│ ├─ Pub/Sub (Event Stream)                               │
│ ├─ gproc (Process Registry)                             │
│ └─ Poolboy (Worker Pool)                                │
└────────────────┬────────────────────────────────────────┘
                 │
┌─────────────────┴────────────────────────────────────────┐
│ Observability Layer                                      │
│ ├─ Prometheus (Metrics)                                 │
│ ├─ OpenTelemetry (Tracing)                              │
│ └─ Structured Logging (JSON)                            │
└─────────────────────────────────────────────────────────┘
```

## Component Details

### 1. HTTP Server (Cowboy)

**Module**: `tai_http` (gen_server)

**Responsibilities**:
- Listen on configurable port (default: 8080)
- Route requests to appropriate handlers
- Implement health check endpoint
- Handle graceful shutdown

**Endpoints**:
- `GET /health` - Readiness check, returns operational status
- `POST /pubsub` - Ingest Pub/Sub notifications
- `POST /marketplace` - Process entitlement actions

**Failure Modes**:
- Port binding failure → Fatal startup error
- Handler crash → Error response (500) to client, server continues
- Request timeout → HTTP 504

### 2. Request Handler (tai_http_handler)

**Module**: `tai_http_handler` (cowboy_handler)

**Responsibilities**:
- Parse incoming JSON requests
- Validate payload structure
- Verify JWT signatures
- Route to appropriate governor
- Generate response receipts

**Key Functions**:
- `handle_pubsub/2` - Validate and route Pub/Sub events
- `handle_marketplace/2` - Validate and route marketplace requests
- `verify_signature/2` - JWT signature validation

**Error Handling**:
- Malformed JSON → 400 Bad Request + refusal receipt
- Invalid signature → 403 Forbidden + refusal receipt
- Missing fields → 400 Bad Request + refusal receipt
- Rate limited → 429 Too Many Requests

### 3. Autonomic Governors

**Module**: `tai_governor` (gen_statem - State Machine)

**State Machine**:
```
                    ┌─────────────────┐
                    │   boot           │
                    └────────┬─────────┘
                             │
                    ┌────────▼─────────┐
                    │ active           │◄─┐
                    │ (entitlement ok) │  │
                    └────────┬─────────┘  │
                             │            │
                    ┌────────▼─────────┐  │
                    │ suspended        │──┤
                    │ (quota exceeded) │  │
                    └────────┬─────────┘  │
                             │            │
                    ┌────────▼─────────┐  │
                    │ complying        │──┘
                    │ (refunding)      │
                    └──────────────────┘
```

**Responsibilities**:
- Maintain entitlement state
- Execute bounded actions (limited concurrency)
- Emit cryptographic receipts
- Handle timeouts and failures
- Provide idempotent retry semantics

**Data Structure**:
```erlang
-record(governor_state, {
    tenant_id :: binary(),
    entitlement_id :: binary(),
    status :: active | suspended | complying,
    quota :: integer(),
    used :: integer(),
    actions_pending :: integer(),
    last_receipt_hash :: binary(),
    created_at :: integer(),
    updated_at :: integer()
}).
```

### 4. Receipt Ledger (tai_receipts)

**Module**: `tai_receipts` (gen_server)

**Responsibilities**:
- Emit cryptographic receipts for all state changes
- Store in Firestore
- Maintain hash chain for audit trail
- Provide receipt verification

**Receipt Structure**:
```json
{
  "receipt_id": "r_2024_01_25_abc123",
  "type": "action_success|action_failure|state_change",
  "timestamp": 1704067200000,
  "tenant_id": "tenant_xyz",
  "entitlement_id": "ent_abc",
  "body": {...},
  "previous_hash": "sha256_hash_of_previous_receipt",
  "receipt_hash": "sha256_hash_of_this_receipt",
  "signature": "JWS_signature"
}
```

**Hash Chain**:
- Each receipt includes hash of previous receipt
- Cryptographic proof of ordering and non-tampering
- Enables audit trail verification

### 5. Action Executor (tai_actions)

**Module**: `tai_actions` (gen_server)

**Responsibilities**:
- Execute actions with bounded concurrency (poolboy)
- Implement timeout policies
- Generate action receipts
- Handle failure recovery

**Bounded Concurrency**:
- Configurable worker pool size (default: 10)
- Prevents resource exhaustion
- Ensures fair queuing of requests
- Graceful degradation under load

**Action Flow**:
1. Receive action request
2. Check quota and entitlement status
3. Acquire worker from pool
4. Execute action
5. Emit result receipt
6. Release worker

### 6. Process Registry (gproc)

**Module**: gproc (external library)

**Responsibilities**:
- Global process registry for governors
- Named process discovery
- Pub/Sub capabilities

**Key Registry Entries**:
- `{n, l, {tai_governor, TenantId}}` - Tenant governor process
- `{n, l, {tai_receipt_ledger, tenant}}` - Receipt ledger process

### 7. Observability

#### Prometheus Metrics
- Request count/latency by endpoint
- Governor state transitions
- Action execution time/success rate
- Queue depth
- Memory/CPU usage

#### OpenTelemetry Tracing
- Span creation for each request
- Distributed trace context propagation
- Attributes for request/state data

#### Structured Logging
- JSON format for all logs
- Trace ID correlation
- Severity levels (debug, info, warn, error)

## Fault Tolerance

### Supervisor Hierarchy

```
tai_autonomics_sup (application supervisor)
├── tai_http (HTTP server)
├── governance_sup (Governors supervisor)
│   ├── (dynamic) Governor 1
│   ├── (dynamic) Governor 2
│   └── (dynamic) Governor N
├── receipt_ledger_sup (Receipts supervisor)
│   └── tai_receipts
├── cluster_sup (Cluster management)
│   └── tai_cluster
└── observability_sup (Observability)
    ├── prometheus_registry
    └── otel_tracer
```

### Restart Policies

| Component | Restart | Max Restarts | Time Window |
|-----------|---------|--------------|-------------|
| HTTP Server | permanent | 5 | 60s |
| Governor | permanent | 5 | 60s |
| Receipt Ledger | permanent | 3 | 60s |
| Cluster Manager | transient | N/A | - |

### Failure Scenarios

#### Scenario 1: Governor Crash
- Supervisor restarts governor
- New governor spawned with clean state
- New requests treated as new entitlements
- Previous actions stored in Firestore

#### Scenario 2: Receipt Ledger Unavailable
- Fallback to local receipt buffer
- Retry writes to Firestore periodically
- No requests rejected (buffered)

#### Scenario 3: HTTP Server Crash
- Supervisor restarts HTTP server
- Requests fail temporarily
- Health check fails, triggering Cloud Run restart

#### Scenario 4: Action Executor Timeout
- Worker released after timeout
- Action receipt marked as timeout
- Caller receives timeout error
- No resource leak

## Scalability Design

### Horizontal Scaling
- Stateless HTTP servers
- Tenant governors are per-instance (not shared)
- GCP Cloud Run auto-scaling on CPU/memory
- Load balancing via Cloud Load Balancer

### Vertical Scaling
- Configurable worker pool size
- Adjustable action timeout
- Tunable garbage collection
- Memory-efficient state representation

### Performance Characteristics
- Health check: <5ms (local response)
- Action execution: <100ms (p99)
- Receipt emission: <50ms
- Throughput: >100 requests/sec per instance

## Security Architecture

### Authentication
- JWT signature verification on all requests
- Configurable verification (can be disabled for testing)
- Async signature validation

### Authorization
- Tenant isolation via entitlement ownership
- Quota enforcement per entitlement
- No cross-tenant action execution

### Data Protection
- No secrets stored in code
- GCP credentials from environment
- TLS for external service calls
- Cryptographic receipts for audit trail

### Compliance
- Audit trail via receipt ledger
- Immutable action history
- Non-repudiation via signatures
- Compliance verification governor

## Deployment Architecture

### Local Development
```
macOS/Linux
├── Erlang/OTP 26
├── Rebar3
├── Docker (optional)
└── Firestore Emulator (optional)
```

### Container Runtime
```
Alpine Linux 3.19
├── Erlang/OTP 26
├── Health check script
└── Release artifact
```

### Cloud Run Deployment
```
GCP Cloud Run
├── Container Image
├── Service Configuration
├── Environment Variables
└── Health Check
```

### Infrastructure
```
GCP Resources
├── Cloud Run Service
├── Cloud Pub/Sub
│   ├── Topic: erlang-autonomics-events
│   └── Subscription: erlang-autonomics-signals
├── Firestore
│   └── Collection: receipts
├── Artifact Registry
└── Cloud Monitoring
```

## Performance Optimization

### Memory Optimization
- Lightweight process state
- Binary strings instead of atom lists
- Lazy evaluation of expensive operations

### CPU Optimization
- Asynchronous I/O via gen_server
- Non-blocking message passing
- Efficient hash chain computation

### I/O Optimization
- Connection pooling to Firestore
- Batch writes where possible
- Configurable retry policies

## Monitoring & Alerting

### Key Metrics
- Request rate and latency
- Governor state distribution
- Action execution rate
- Receipt emission rate
- Error rates by type

### Alert Conditions
- HTTP server restart frequency
- Governor crash rate
- Firestore connectivity issues
- Action queue depth
- Memory usage trend

## Extension Points

### Adding New Governors
1. Create new governor module
2. Implement gen_statem behavior
3. Register with supervisor
4. Update routing in handler

### Adding New Endpoints
1. Add route in HTTP server
2. Create handler function
3. Implement validation logic
4. Generate appropriate receipts

### Custom Observability
1. Register Prometheus metrics
2. Create OpenTelemetry spans
3. Emit structured logs
4. Export to monitoring system

## References

- [CONFIG.md](CONFIG.md) - Configuration guide
- [ENDPOINTS.md](ENDPOINTS.md) - API specification
- [RECEIPTS.md](RECEIPTS.md) - Receipt schema
- [RUNBOOK.md](RUNBOOK.md) - Operations guide
- [SECURITY_REQUIREMENTS.md](SECURITY_REQUIREMENTS.md) - Security specification
