# TAI Erlang Autonomics - System Architecture

**Status**: Phase 1 (Eval-only)
**Version**: 1.0.0
**Last Updated**: 2026-01-26

---

## Executive Summary

TAI Erlang Autonomics is a production-grade Erlang/OTP runtime implementing autonomous entitlement gating and bounded action control via the Model Context Protocol (MCP). The system uses a five-stage deterministic pipeline (normalize → extract → emit → canonicalize → receipt) to transform specification-driven configurations into cryptographically verifiable action sequences.

**Core Equation**: A = μ(O)

Where:
- **A** = executable actions (autonomic control)
- **O** = ontology (RDF specifications)
- **μ** = transformation pipeline (five stages)

---

## 1. System Architecture Overview

### High-Level Flow

```
┌──────────────────────────────────────────────────────────────┐
│ HTTP Client / MCP Client                                     │
└────────────────────┬─────────────────────────────────────────┘
                     │
                     ▼
┌──────────────────────────────────────────────────────────────┐
│ HTTP Server (Cowboy)                                         │
│ ├─ GET /health         → Health check                       │
│ ├─ POST /marketplace   → Entitlement actions                │
│ ├─ POST /pubsub        → Event ingestion                    │
│ └─ GET /mcp/tools      → Tool discovery                     │
└────────────────────┬─────────────────────────────────────────┘
                     │
                     ▼
┌──────────────────────────────────────────────────────────────┐
│ Request Validation Layer                                      │
│ ├─ JSON schema validation                                    │
│ ├─ JWT signature verification                               │
│ └─ Tenant isolation checks                                  │
└────────────────────┬─────────────────────────────────────────┘
                     │
                     ▼
┌──────────────────────────────────────────────────────────────┐
│ Autonomic Governance Layer                                    │
│ ├─ Gate Checking (3-stage sequence)                         │
│ │  ├─ Quota Gate      (resource limits)                    │
│ │  ├─ User Tier Gate  (entitlement level)                  │
│ │  └─ Compliance Gate (regulatory checks)                  │
│ ├─ Governor State Machine (gen_statem)                     │
│ │  ├─ boot → active (initialization)                       │
│ │  ├─ active → suspended (quota exceeded)                 │
│ │  ├─ suspended → active (quota reset)                    │
│ │  └─ suspended → complying (refunding)                   │
│ └─ Bounded Action Executor (poolboy)                        │
└────────────────────┬─────────────────────────────────────────┘
                     │
                     ▼
┌──────────────────────────────────────────────────────────────┐
│ Receipt & Persistence Layer                                   │
│ ├─ Receipt Emission (μ₅ Canonicalize)                       │
│ │  ├─ JSON envelope generation                             │
│ │  ├─ SHA-256 hash chain                                   │
│ │  └─ Cryptographic signature (JWS)                        │
│ ├─ Firestore Storage                                        │
│ ├─ gproc Process Registry                                  │
│ └─ Pub/Sub Event Stream                                    │
└────────────────────┬─────────────────────────────────────────┘
                     │
                     ▼
┌──────────────────────────────────────────────────────────────┐
│ Observability Layer                                           │
│ ├─ Prometheus Metrics                                        │
│ ├─ OpenTelemetry Tracing                                    │
│ └─ Structured Logging (JSON)                                │
└──────────────────────────────────────────────────────────────┘
```

---

## 2. Core Components

### 2.1 HTTP Server (taiea_http)

**Type**: `gen_server`
**Module**: `apps/taiea_core/src/taiea_http.erl`

**Responsibilities**:
- Listen on port 8080 (configurable)
- Route requests to appropriate handlers
- Implement health check endpoint
- Graceful shutdown on termination

**Endpoints**:

| Method | Path | Purpose |
|--------|------|---------|
| `GET` | `/health` | System readiness probe |
| `POST` | `/marketplace` | Entitlement action requests |
| `POST` | `/pubsub` | Pub/Sub event ingestion |
| `GET` | `/mcp/tools` | MCP tool discovery |

**Error Handling**:
- Port binding failure → Application crash (supervisor restarts)
- Handler crash → 500 error response, server continues
- Request timeout → 504 Gateway Timeout

### 2.2 Request Handler (taiea_http_handler)

**Type**: `cowboy_http_handler`
**Module**: `apps/taiea_core/src/taiea_http_handler.erl`

**Responsibilities**:
- Parse incoming JSON
- Validate payload schema
- Verify JWT signatures (phase 2+)
- Route to appropriate governor
- Generate response receipts

**Validation Pipeline**:

```erlang
validate_request(Request) ->
    case parse_json(Request) of
        {ok, Payload} ->
            case validate_schema(Payload) of
                ok ->
                    case verify_signature(Payload) of
                        ok -> route_request(Payload);
                        error -> {error, invalid_signature}
                    end;
                error -> {error, invalid_schema}
            end;
        error -> {error, malformed_json}
    end.
```

**Error Responses**:

| Status | Reason | Response |
|--------|--------|----------|
| 400 | Malformed JSON | `{status: "error", code: "invalid_json"}` |
| 400 | Schema violation | `{status: "error", code: "invalid_schema", details: [...]}` |
| 403 | Signature invalid | `{status: "denied", code: "invalid_signature"}` |
| 429 | Rate limited | `{status: "denied", code: "rate_limited"}` |
| 500 | Handler crash | `{status: "error", code: "internal_error"}` |

### 2.3 Gate Checking System (taiea_gates)

**Type**: Pure functions with side-effect receipts
**Module**: `apps/tai_autonomics/src/taiea_gates.erl`

**Three-Stage Sequential Gate Sequence**:

```
┌─────────────────────────────────────────┐
│ Request                                 │
└────────────────┬────────────────────────┘
                 │
                 ▼
        ┌────────────────────┐
        │ Quota Gate         │
        │ (resource limits)  │
        │ Checks: used < max │
        │ Action: deny/pass  │
        └────────┬───────────┘
                 │
        ┌────────▼───────────┐
        │ User Tier Gate     │
        │ (entitlement lvl)  │
        │ Checks: tier OK    │
        │ Action: deny/pass  │
        └────────┬───────────┘
                 │
        ┌────────▼───────────┐
        │ Compliance Gate    │
        │ (regulatory chks)  │
        │ Checks: audit OK   │
        │ Action: deny/pass  │
        └────────┬───────────┘
                 │
                 ▼
        ┌────────────────────┐
        │ Action Allowed or  │
        │ Denied with Reason │
        └────────────────────┘
```

**Gate Functions**:

```erlang
%% Quota Gate: Ensures used quota < max quota
-spec quota_gate(map()) -> {ok, map()} | {deny, atom()}.
quota_gate(#{used := Used, quota := Max} = State)
  when Used < Max ->
    {ok, State};
quota_gate(_) ->
    {deny, quota_exceeded}.

%% User Tier Gate: Ensures user tier meets requirement
-spec user_tier_gate(map()) -> {ok, map()} | {deny, atom()}.
user_tier_gate(#{tier := <<"premium">>} = State) ->
    {ok, State};
user_tier_gate(_) ->
    {deny, insufficient_tier}.

%% Compliance Gate: Ensures compliance audit passed
-spec compliance_gate(map()) -> {ok, map()} | {deny, atom()}.
compliance_gate(#{compliance_status := <<"passed">>} = State) ->
    {ok, State};
compliance_gate(_) ->
    {deny, compliance_failed}.
```

**Gate Execution**:

```erlang
execute_sequence(State) ->
    case quota_gate(State) of
        {ok, State1} ->
            case user_tier_gate(State1) of
                {ok, State2} ->
                    case compliance_gate(State2) of
                        {ok, Final} ->
                            emit_receipt(gate_passed, State),
                            {ok, Final};
                        {deny, Reason} ->
                            emit_receipt(gate_denied, State, Reason),
                            {deny, Reason}
                    end;
                {deny, Reason} ->
                    emit_receipt(gate_denied, State, Reason),
                    {deny, Reason}
            end;
        {deny, Reason} ->
            emit_receipt(gate_denied, State, Reason),
            {deny, Reason}
    end.
```

**Key Design Decisions**:
- **Sequential**: Gates evaluated in order, short-circuit on first failure
- **Pure Functions**: No side effects in gate logic itself
- **Receipt Emission**: Every gate decision recorded
- **Extensible**: Add new gates by implementing spec and adding to sequence

### 2.4 Governor State Machine (tai_governor)

**Type**: `gen_statem`
**Module**: `apps/tai_autonomics/src/tai_governor.erl`

**State Machine Diagram**:

```
                    start_link/1
                         │
                         ▼
                    ┌─────────────┐
                    │    boot     │
                    │             │
                    │ (inactive)  │
                    └──────┬──────┘
                           │
                      activate/1
                           │
                           ▼
                    ┌─────────────┐
                    │   active    │◄─────────┐
                    │             │          │
                    │ (ready)     │      quota_reset/1
                    └──────┬──────┘          │
                           │                 │
                    quota_exceeded/1         │
                           │                 │
                           ▼                 │
                    ┌─────────────┐          │
                    │  suspended  │          │
                    │             │──────────┘
                    │ (blocking)  │
                    └──────┬──────┘
                           │
                       refund/1
                           │
                           ▼
                    ┌─────────────┐
                    │  complying  │
                    │             │
                    │ (refunding) │
                    └─────────────┘
```

**State Definitions**:

```erlang
-record(governor_state, {
    tenant_id         :: binary(),      % Tenant identifier
    entitlement_id    :: binary(),      % Entitlement identifier
    status            :: boot | active | suspended | complying,  % Current state
    quota             :: integer(),     % Max actions allowed
    used              :: integer(),     % Actions consumed
    actions_pending   :: integer(),     % In-flight actions
    last_receipt_hash :: binary(),      % Hash of last receipt (for chain)
    created_at        :: integer(),     % Unix ms timestamp
    updated_at        :: integer()      % Last state change time
}).
```

**State Callbacks**:

| Event | From | To | Action |
|-------|------|-----|---------|
| `init` | - | `boot` | Initialize state |
| `activate` | `boot` | `active` | Enable actions |
| `quota_exceeded` | `active` | `suspended` | Pause on quota |
| `quota_reset` | `suspended` | `active` | Resume after reset |
| `refund` | `suspended` | `complying` | Initiate refund |
| `terminate` | any | - | Cleanup, final receipt |

**Event Handlers**:

```erlang
% Boot state: only allow activation
boot({call, From}, activate, StateData) ->
    NewState = StateData#governor_state{status = active, updated_at = os:system_time(millisecond)},
    {next_state, active, NewState, [{reply, From, {ok, NewState}}]};

% Active state: allow actions and transitions
active({call, From}, {execute_action, Action}, StateData) ->
    case quota_gate(StateData) of
        {ok, NewState} ->
            spawn_action(Action, NewState),
            {keep_state, NewState, [{reply, From, ok}]};
        {deny, Reason} ->
            {next_state, suspended, NewState, [{reply, From, {error, Reason}}]}
    end;

% Suspended state: block actions, allow reset
suspended({call, From}, quota_reset, StateData) ->
    NewState = StateData#governor_state{used = 0, status = active},
    {next_state, active, NewState, [{reply, From, ok}]};
suspended({call, From}, {execute_action, _}, _StateData) ->
    {keep_state_and_data, [{reply, From, {error, suspended}}]}.
```

**Action Execution Flow**:

```
governor (state machine)
    │
    ├─ Check gates
    │
    └─ If gates pass:
       │
       ├─ Acquire worker from poolboy
       │
       ├─ Execute action with timeout
       │
       ├─ Release worker
       │
       └─ Emit result receipt
```

### 2.5 Bounded Action Executor (tai_action_executor)

**Type**: `gen_server` + poolboy worker pool
**Module**: `apps/tai_autonomics/src/tai_action_executor.erl`

**Design**:
- **Concurrency Control**: Poolboy worker pool (default: 10 workers)
- **Timeout Protection**: 30-second action timeout
- **Memory Bounds**: Worker state < 10MB each
- **Graceful Degradation**: Queue requests when pool full

**Worker Pool Configuration**:

```erlang
{poolboy, [
    {name, {local, action_pool}},
    {worker_module, tai_action_worker},
    {size, 10},          % Number of workers
    {max_overflow, 5},   % Additional workers on overflow
    {worker_args, []}
]}
```

**Execution Sequence**:

```erlang
execute_action(Action) ->
    % 1. Acquire worker from pool (blocks if full)
    {ok, Worker} = poolboy:checkout(action_pool),

    % 2. Execute with timeout
    Result = tai_action_worker:execute(Worker, Action, 30000),

    % 3. Release worker
    poolboy:checkin(action_pool, Worker),

    % 4. Return result
    Result.
```

**Timeout Handling**:

```erlang
execute_with_timeout(Worker, Action, TimeoutMs) ->
    TRef = erlang:send_after(TimeoutMs, self(), {action_timeout, Worker}),

    receive
        {action_result, Result} ->
            erlang:cancel_timer(TRef),
            {ok, Result};
        {action_timeout, Worker} ->
            kill_worker(Worker),
            {error, timeout}
    after TimeoutMs + 1000 ->
        {error, timeout}
    end.
```

### 2.6 Receipt Ledger (taiea_receipts)

**Type**: `gen_server`
**Module**: `apps/tai_autonomics/src/taiea_receipts.erl`

**Five-Stage Emission Pipeline** (μ₁-μ₅):

```
┌─────────────────────────────────────────────────────────┐
│ μ₁ Normalize                                             │
│ - Validate receipt data structure                       │
│ - Ensure required fields present                        │
│ - Standardize timestamp format (Unix ms)               │
└────────────────┬────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────┐
│ μ₂ Extract                                               │
│ - SPARQL query on ontology                             │
│ - Infer entitlement properties                         │
│ - Extract compliance context                           │
└────────────────┬────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────┐
│ μ₃ Emit                                                  │
│ - Generate receipt JSON envelope                       │
│ - Assign unique receipt ID                             │
│ - Include action data                                  │
└────────────────┬────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────┐
│ μ₄ Canonicalize                                          │
│ - Deterministic JSON encoding (sorted keys)            │
│ - SHA-256 hash of canonical form                       │
│ - Include previous receipt hash (chain)                │
└────────────────┬────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────┐
│ μ₅ Receipt (Storage & Proof)                            │
│ - Sign with JWS (algorithm: RS256 or HS256)           │
│ - Write to Firestore (append-only collection)          │
│ - Return receipt to client                             │
└─────────────────────────────────────────────────────────┘
```

**Receipt Structure**:

```json
{
  "receipt_id": "r_2026_01_26_abc123def456",
  "type": "action_success | action_failure | gate_passed | gate_denied | state_change",
  "timestamp": 1704067200000,
  "tenant_id": "acme-corp",
  "entitlement_id": "ent_abc123",
  "body": {
    "action_type": "activate | execute | refund",
    "action_id": "act_xyz789",
    "result": "success | failure",
    "reason": "quota_exceeded | insufficient_tier | internal_error",
    "duration_ms": 47
  },
  "previous_hash": "sha256_hash_of_previous_receipt",
  "receipt_hash": "sha256_hash_of_this_receipt",
  "signature": "JWS_signature_bytes"
}
```

**Hash Chain**:

```
Receipt N-1: hash(receipt_N-1) = h1
         │
         ├─ Sign with private key → signature_N-1
         │
         └─ Store in Firestore

Receipt N: previous_hash = h1
         │
         ├─ hash(receipt_N) = h2
         │
         ├─ Sign with private key → signature_N
         │
         └─ Store in Firestore

Verification:
  - For each receipt: hash(receipt_N) == stored_receipt_hash
  - previous_hash == hash(receipt_N-1)
  - signature verifies with public key
  → Audit trail immutable and tamper-evident
```

### 2.7 Process Registry (gproc)

**Module**: External library `gproc`

**Purpose**: Global distributed process registry

**Key Registrations**:

```erlang
% Register governor processes
gproc:reg({n, l, {tai_governor, TenantId, EntitlementId}}, Pid),

% Register receipt ledger
gproc:reg({n, l, {tai_receipt_ledger, tenant}}, ReceiptPid),

% Lookup by name
Pids = gproc:lookup_pids({n, l, {tai_governor, <<"acme">>, <<"ent1">>}}),

% Pub/Sub capability (future)
gproc:subscribe(event, {tai_events, tenant_activated})
```

**Benefits**:
- Process discovery across cluster
- Pub/Sub for events
- Atomic registration/deregistration
- Scales to 1000s of processes

---

## 3. Data Flow Examples

### Example 1: Action Request Flow

```
1. Client sends POST /marketplace
   {
     "event": "sku_activate",
     "sku_id": "autonomic-v1",
     "tenant_id": "acme-corp",
     "data": {"feature": "report-gen", "tier": "professional"}
   }

2. HTTP handler validates
   - Parse JSON ✓
   - Check schema ✓
   - Verify signature (phase 2) ✓

3. Look up or create governor
   Governor = {tai_governor, <<"acme-corp">>, <<"ent_abc">>}

4. Execute gate sequence
   - Quota Gate: used (25) < max (100) ✓
   - Tier Gate: tier (professional) >= required ✓
   - Compliance Gate: audit_status (passed) ✓
   → All gates pass

5. Acquire worker from pool
   Worker = poolboy:checkout(action_pool)

6. Execute action
   Result = tai_action_worker:execute(Worker, {sku_activate, Data}, 30000)
   → Action executes: "Report generation feature activated for user"

7. Release worker
   poolboy:checkin(action_pool, Worker)

8. Emit receipt
   μ₁ Normalize: Validate action result
   μ₂ Extract: Query ontology for compliance context
   μ₃ Emit: Generate receipt JSON
   μ₄ Canonicalize: Compute hash and chain
   μ₅ Receipt: Sign and store in Firestore

   Receipt = {
     "receipt_id": "r_2026_01_26_xyz789",
     "type": "action_success",
     "timestamp": 1704067200000,
     "tenant_id": "acme-corp",
     "entitlement_id": "ent_abc",
     "body": {"action_type": "activate", "result": "success", "duration_ms": 47},
     "previous_hash": "hash(receipt_N-1)",
     "receipt_hash": "hash(this_receipt)",
     "signature": "JWS_signature"
   }

9. Return response to client
   {
     "status": "accepted",
     "event_id": "evt-uuid",
     "receipt": {
       "id": "r_2026_01_26_xyz789",
       "timestamp": "2026-01-26T14:30:45.123Z",
       "hash": "hash(receipt)"
     }
   }
```

### Example 2: Gate Denial Flow

```
1. Client sends POST /marketplace with tier: "basic"

2. Governor executes gates
   - Quota Gate: used (25) < max (100) ✓
   - Tier Gate: tier (basic) >= required (premium) ✗ DENY

3. Emit denial receipt
   μ₁ Normalize: Validate denial reason
   μ₂ Extract: Query ontology for audit context
   μ₃ Emit: Generate denial receipt
   μ₄ Canonicalize: Compute hash
   μ₅ Receipt: Sign and store

   Receipt = {
     "type": "gate_denied",
     "body": {"gate": "user_tier_gate", "reason": "insufficient_tier"},
     ...
   }

4. Return denial response
   {
     "status": "denied",
     "code": "insufficient_tier",
     "receipt": {...}
   }
```

---

## 4. Supervisor Hierarchy

```
tai_autonomics_sup (application supervisor)
│
├─ tai_http (gen_server)
│  │ Restarts: permanent, max_restarts=5 in 60s
│  └─ Cowboy HTTP listener
│
├─ governance_sup (supervisor)
│  │ Restarts: permanent
│  │ Strategy: one_for_one (restart individual governors)
│  │
│  ├─ (dynamic) tai_governor:{TenantId,EntId}
│  ├─ (dynamic) tai_governor:{TenantId,EntId}
│  └─ (dynamic) ...
│
├─ receipt_ledger_sup (supervisor)
│  │ Restarts: permanent
│  │
│  ├─ taiea_receipts (gen_server)
│  │  Restarts: permanent, max_restarts=3 in 60s
│  │
│  └─ taiea_firestore_writer (gen_server)
│     Restarts: permanent, max_restarts=3 in 60s
│
├─ action_executor_sup (supervisor)
│  │
│  ├─ poolboy:action_pool
│  │  └─ (pooled) tai_action_worker × N
│  │
│  └─ tai_action_executor (gen_server)
│     Manages pool health
│
├─ process_registry_sup (supervisor)
│  │
│  └─ gproc (dynamic process registry)
│
└─ observability_sup (supervisor)
   │
   ├─ prometheus_registry
   │  ├─ Metrics: http_requests_total
   │  ├─ Metrics: http_request_duration_seconds
   │  ├─ Metrics: governor_state
   │  └─ Metrics: action_execution_time
   │
   └─ otel_tracer
      └─ Distributed trace export
```

---

## 5. Failure Modes & Recovery

### Failure Mode 1: Governor Crash

**Scenario**: Governor process exits unexpectedly

**Detection**:
```
governor supervisor detects exit signal
```

**Recovery**:
```
supervisor automatically restarts governor with clean state
new governor initialized with boot state
state queries will return empty until re-activation
```

**Data Preservation**:
- Old state lost (ephemeral)
- Receipts preserved in Firestore (persistent)
- Clients can replay from receipt ledger

### Failure Mode 2: Receipt Storage Unavailable

**Scenario**: Firestore connection fails

**Detection**:
```
taiea_firestore_writer:write fails with timeout
```

**Recovery**:
```
1. Buffer receipt in memory
2. Retry writes periodically (exponential backoff)
3. Request still accepted (graceful degradation)
4. Client receives response with local receipt
```

**Guarantees**:
- No requests rejected
- Receipts eventually written when service recovers

### Failure Mode 3: Worker Pool Exhaustion

**Scenario**: All workers busy, new request arrives

**Detection**:
```
poolboy:checkout returns after timeout
```

**Recovery**:
```
1. Overflow workers spawned (up to max_overflow)
2. Request queued and executed when worker available
3. If queue full, return 429 Too Many Requests
```

**Client Handling**:
- Implement exponential backoff retry
- Configure max_overflow based on load

### Failure Mode 4: HTTP Server Crash

**Scenario**: Cowboy listener exits

**Detection**:
```
tai_http gen_server terminates
supervisor detects exit
```

**Recovery**:
```
supervisor restarts HTTP server
new listener bound to port
service recovers in <5 seconds
```

**Impact**:
- Requests fail for duration of restart
- Health check fails, triggering Cloud Run restart (if healthy_timeout exceeded)
- Multi-instance deployment absorbs failure

---

## 6. Entitlement Resolution

**Definition**: Process of determining if a tenant's action is authorized

**Three-Level Resolution**:

```
Level 1: Tenant Lookup
    tenant_id → {TenantId, EntitlementId, Tier, Quota}

Level 2: Entitlement Lookup
    EntitlementId → {Status, Quota, Used, ComplianceStatus}

Level 3: Gate Evaluation
    {Tenant, Entitlement} → {Quota, Tier, Compliance} → Allow/Deny
```

**Example Resolution Chain**:

```erlang
resolve_entitlement(TenantId, Action) ->
    % 1. Look up tenant
    {ok, Tenant} = load_tenant(TenantId),

    % 2. Extract entitlement ID
    EntitlementId = maps:get(entitlement_id, Tenant),

    % 3. Look up or create governor
    {ok, GovernorPid} = get_or_create_governor(TenantId, EntitlementId),

    % 4. Get current state
    {ok, State} = tai_governor:get_state(GovernorPid),

    % 5. Execute gates
    case taiea_gates:execute_sequence(State) of
        {ok, State} -> {allow, State};
        {deny, Reason} -> {deny, Reason}
    end.
```

---

## 7. Security Architecture

### Authentication

- **Phase 1**: No authentication (eval-only)
- **Phase 2**: JWT verification required
  ```erlang
  verify_jwt(Token, PublicKey) ->
      case jose:verify_compact(Token, PublicKey) of
          {true, Payload} -> {ok, Payload};
          false -> {error, invalid_signature}
      end.
  ```

### Authorization

- **Tenant Isolation**: Actions limited to tenant's entitlements
- **Quota Enforcement**: Actions count against tenant's quota
- **Role-Based Access**: (Phase 2+) Roles determine available actions

### Data Protection

- **Secrets**: Loaded from GCP Secret Manager, never hardcoded
- **Encryption**: TLS for external service calls
- **Audit Trail**: Every action recorded in immutable receipt ledger

### Compliance

- **Non-Repudiation**: Signatures prove action by authorized party
- **Audit Trail**: Receipt chain verifiable and tamper-evident
- **Data Retention**: Receipts retained per compliance requirements

---

## 8. Performance Characteristics

### Latency (p99)

| Operation | Latency |
|-----------|---------|
| Health check | <5ms |
| Gate checking | <20ms |
| Action execution | <100ms |
| Receipt emission | <50ms |
| **Total request** | **<200ms** |

### Throughput

| Metric | Capacity |
|--------|----------|
| Requests/second | 100+ |
| Concurrent actions | 10 (poolboy size) |
| Concurrent governors | 1000+ |
| Receipt writes/sec | 50+ |

### Resource Usage

| Resource | Limit |
|----------|-------|
| Memory per instance | 2 GB |
| CPU per instance | 2 vCPU |
| Connections to Firestore | 100 |
| Pub/Sub subscriptions | 10 |

---

## 9. Extension Points

### Adding New Governor Type

1. Create module: `tai_governor_custom.erl`
2. Implement `gen_statem` behavior
3. Define custom state machine
4. Register in supervision tree
5. Update routing in handler

### Adding New Gate

1. Implement gate function in `taiea_gates.erl`
2. Add to sequence in `execute_sequence/1`
3. Write unit tests
4. Emit receipt on pass/deny

### Adding New MCP Tool

1. Create tool module: `apps/taiea_mcp/src/tools/my_tool.erl`
2. Implement `spec/0` and `handle/2`
3. Register in `taiea_mcp_registry.erl`
4. Write tests

---

## 10. References

- **DEVELOPER_GUIDE.md**: Setup, building, testing
- **OPERATIONAL_GUIDE.md**: Deployment, scaling, troubleshooting
- **API.md**: HTTP endpoints and MCP tools
- **CONFIG.md**: Configuration reference
- **docs/**: Additional technical documentation

---

**Questions?** Contact the architecture team or file an issue in the repository.
