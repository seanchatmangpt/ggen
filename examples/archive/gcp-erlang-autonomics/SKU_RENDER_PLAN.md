# SKU Render Plan: GCP Erlang Autonomics Complete Generation Pipeline

## Overview

This document describes the complete 23-module generation pipeline for transforming a single Marketplace SKU specification (JSON or ggen.toml) into a production-ready Erlang Autonomics BEAM deployment on GCP.

**Core Equation**: One JSON spec + 23 Tera templates = Deterministic, reproducible Erlang system

---

## High-Level Architecture

```
ggen.toml (SKU Metadata)
    ↓
[μ₁ Normalize] - SHACL validation of spec
    ↓
[μ₂ Extract] - SPARQL queries extract template context
    ↓
[μ₃ Emit] - Tera template rendering (23 modules)
    ↓
[μ₄ Canonicalize] - Erlang formatter, deterministic output
    ↓
[μ₅ Receipt] - Cryptographic proof (SHA-256, audit trail)
    ↓
Deployable Erlang/BEAM System
```

---

## Module Generation Pipeline (23 Modules)

### Core Domain (5 modules)

**1. _types.hrl** (`_types.hrl.tera`)
- Generated type definitions and records
- Governor data structure: `governor_data{tenant_id, sku, profile, entitlement, in_flight_action}`
- Entitlement data: `ent_data{tenant_id, entitlement_id, sku, status}`
- Policy decision result: `{ok} | {warn, Reason, Actions} | {refuse, Reason}`
- **Key Field**: `in_flight_action` tracks current intervention (prevents action storms)
- **Determinism**: Fixed field order ensures bit-compatible records across builds
- **Usage**: Included by all modules via `-include("{{module_prefix}}_types.hrl")`

**2. _types.hrl** (header template)
- Standard Erlang header with license, module prefix, auto-generation notice
- Pattern: `{% include "_header.erl.tera" %}` at top of all modules
- **Determinism**: Header is identical every generation

**3. policy_pack.erl** (`policy_pack.erl.tera`)
- Core decision engine: `decide(Profile, SignalName, Payload, Data) → {ok} | {warn, ...} | {refuse, ...}`
- Pattern-matches profile (standard, premium, enterprise)
- Implements SKU-specific rules (pricing, throttling, degradation thresholds)
- **Example Policy**:
  ```erlang
  decide(standard, {quota_exceeded}, Payload, Data) ->
    {warn, quota_exceeded, [throttle_concurrency]};
  decide(enterprise, {quota_exceeded}, _Payload, _Data) ->
    ok.  %% Enterprise tier has unlimited quota
  ```
- **Determinism**: Profile + Signal → Decision is deterministic
- **Extension Point**: Template loop `{% for r in rules %}` allows arbitrary rule injection

---

### Governor FSM (3 modules)

**4. governor_statem.erl** (`governor_statem.erl.tera`)
- Core state machine: `boot → stable → warning → intervening → degraded | refusing`
- **Key Methods**:
  - `boot/3`: Initialization state, immediately transitions to stable with tick
  - `stable/3`: Normal operation, accepts signals and internal reconcile
  - `warning/3`: Policy decision returned warn, tracks in-flight action, intervenes
  - `intervening/3`: Action in flight, postpones new signals, clears on action_result
  - `degraded/3`: Action failed, refuses risky operations
  - `refusing/3`: Entitlement not active, refuses signals until reactivated
- **Event Smoothing**:
  - Uses `postpone` to queue signals during intervening state (prevents action storms)
  - Uses `next_event` to trigger internal reconcile when entitlement activates
  - Tracks `in_flight_action` to ensure one action at a time
- **Receipts**: Every state transition receipted with decision, action, invariants
- **Determinism**: Pattern match order on state + event type is deterministic

**5. entitlement_statem.erl** (`entitlement_statem.erl.tera`)
- Marketplace entitlement lifecycle: `installed → billing_pending → active | cancelled | suspended | expired`
- **Key Methods**:
  - `translate_marketplace_event/1`: Maps GCP Marketplace event names to internal FSM events
    - `entitlement_created` → `install_confirmed`
    - `entitlement_active` → `payment_confirmed`
    - `entitlement_cancelled` → `cancel`
  - State transitions receipt all Marketplace events for audit trail
  - On `payment_confirmed` in `billing_pending`: Ensures tenant governor and sends `{entitlement, active}` cast
- **Determinism**: Event translation is pure function (no side effects during translation)
- **Integration**: Routes via tenant registry to governor when entitlement state changes

**6. tenant_registry.erl** (`tenant_registry_dyn.erl.tera`)
- Dynamic tenant FSM registry: One FSM per tenant
- **Key Methods**:
  - `ensure_tenant/2`: Get or create tenant FSM (idempotent)
  - `whereis_tenant/1`: Lookup tenant FSM process by ID
  - `route_call/3`: Route sync call to tenant FSM
  - `route_cast/3`: Route async cast to tenant FSM
- **Process Management**:
  - Uses DynamicSupervisor (`tenant_dynsup`) to create child FSMs with proper OTP lifecycle
  - Monitors each tenant FSM via `erlang:monitor/2`
  - On 'DOWN' signal, auto-cleans up dead tenant from registry
  - Dual indexing: `tenants` (TenantId → {Pid, Ref}) and `monitors` (Ref → TenantId)
- **Determinism**: Registry consistency is maintained by OTP supervisor guarantees

---

### Supervision Tree (2 modules)

**7. tenant_dynsup.erl** (`tenant_dynsup.erl.tera`)
- DynamicSupervisor for tenant-scoped governor FSMs
- **Strategy**: `simple_one_for_one` (no restart on normal exit, enables clean lifecycle)
- **Child Spec**:
  - Module: `tenant_registry:governor_statem`
  - Restart: transient (only restart on abnormal exit)
  - Shutdown: 5000ms grace period
  - Intensity: 10 restarts per 10 seconds (prevents restart storms)
- **Purpose**: Enables proper OTP lifecycle management vs. manual spawn
- **Determinism**: Fixed strategy and intensity ensure predictable supervision behavior

**8. sku_sup.erl** (`sku_sup.erl.tera`)
- Root supervisor for entire SKU system
- **Supervision Tree**:
  1. `receipt_ledger` (permanent worker) - Audit trail ledger
  2. `tenant_dynsup` (permanent supervisor) - Dynamic tenant FSM creation
  3. `tenant_registry` (permanent worker) - Tenant FSM registry
  4. `entitlement_statem` (permanent worker) - Marketplace entitlement FSM
  5. `http_server` (permanent worker) - Signal ingestion HTTP listener
- **Strategy**: `one_for_one` (independent children, supervisor restarts failed child)
- **Intensity**: 5 restarts per 10 seconds
- **Determinism**: Fixed child order and restart strategy ensure reproducible supervision

---

### Receipt & Audit (2 modules)

**9. receipt_ledger.erl** (`receipt_ledger.erl.tera`)
- Append-only receipt store (audit trail)
- **Key Methods**:
  - `append(TenantId, Receipt) → ok`: Append receipt to ledger
  - `query(TenantId, TimeRange) → [Receipt]`: Query tenant receipts by timestamp
- **Receipt Structure**:
  ```erlang
  Receipt = #{
    kind => transition | refusal | gcp_action | gcp_request | gcp_request_result,
    timestamp => iso8601_datetime,
    tenant_id => TenantId,
    event => Event,
    decision => Decision,
    action => ActionName,
    reason => Reason (if refusal)
  }
  ```
- **Storage**: ETS table (transient, survives node restart via disk backup)
- **Determinism**: Append-only + timestamp ordering ensures reproducible audit trail

**10. receipt_hash.erl** (`receipt_hash.erl.tera`)
- Cryptographic receipt chaining (Merkle-linked proof)
- **Key Methods**:
  - `hash_receipt(Receipt, PrevHash) → SHA256Hash`: Chain receipts cryptographically
  - `verify_chain(Receipts) → ok | {error, TamperedReceipt}`: Verify receipt chain integrity
- **Chain Structure**: Each receipt hash includes hash of previous receipt (Merkle link)
- **Purpose**: Detect tampering, prove audit trail integrity for regulatory compliance
- **Determinism**: SHA-256 is deterministic; same receipts → same chain hash

---

### GCP Integration (5 modules)

**11. gcp_actions_cloudrun.erl** (`gcp_actions_cloudrun.erl.tera`)
- Cloud Run autonomic actions
- **Key Methods**:
  - `throttle_concurrency(TenantId, Payload) → ok | {error, Reason}`: Reduce concurrency
  - `rollback_revision(TenantId, Payload) → ok`: Revert to previous revision
  - `freeze_rollout(TenantId, Payload) → ok`: Halt new deployments
- **API Endpoints**:
  - `https://cloudrun.googleapis.com/v1/projects/{project}/locations/{region}/services/{service}:update`
  - `https://cloudrun.googleapis.com/v1/projects/{project}/locations/{region}/services/{service}:rollback`
  - `https://cloudrun.googleapis.com/v1/projects/{project}/locations/{region}/services/{service}:freeze`
- **Receipt**: Each action receipted before/after execution
- **Integration**: Called by `actuator_adapter:execute/3` when action group is `cloudrun`

**12. gcp_actions_scheduler.erl** (`gcp_actions_scheduler.erl.tera`)
- Cloud Scheduler autonomic actions (background job throttling)
- **Key Methods**:
  - `pause_job(TenantId, Payload) → ok`: Suspend scheduled job
  - `resume_job(TenantId, Payload) → ok`: Resume suspended job
- **API Endpoints**:
  - `https://cloudscheduler.googleapis.com/v1/projects/{project}/locations/{region}/jobs/{job}:pause`
  - `https://cloudscheduler.googleapis.com/v1/projects/{project}/locations/{region}/jobs/{job}:resume`
- **Integration**: Called by `actuator_adapter:execute/3` when action group is `scheduler`

**13. gcp_actions_pubsub.erl** (`gcp_actions_pubsub.erl.tera`)
- Pub/Sub autonomic actions (backlog pressure valve)
- **Key Methods**:
  - `pause_subscription(TenantId, Payload) → ok`: Halt message ingestion
  - `resume_subscription(TenantId, Payload) → ok`: Resume message consumption
- **API Endpoints**:
  - `https://pubsub.googleapis.com/v1/projects/{project}/subscriptions/{sub}:modifyPushConfig`
- **Integration**: Called by `actuator_adapter:execute/3` when action group is `pubsub`

**14. gcp_client_auth.erl** (`gcp_client_auth.erl.tera`)
- Centralized authenticated GCP client
- **Key Methods**:
  - `request(Method, Url, Headers, Body, TenantId) → {ok, Response} | {error, Reason}`
    - Adds OAuth2 Bearer token to Authorization header
    - Routes through receipt ledger for audit trail
  - `fetch_token/1`: Fetch OAuth2 token from GCP metadata server
- **Receipt Pattern**:
  - Before request: `#{kind => gcp_request, action => ..., decision => attempt}`
  - After response: `#{kind => gcp_request_result, action => ..., decision => accept|reject}`
- **Determinism**: Token caching ensures consistent request patterns within token lifetime

**15. signal_normalizer.erl** (`signal_normalizer.erl.tera`)
- Normalizes diverse GCP signals into standard SKU signal format
- **Input**: Raw GCP events (Cloud Monitoring, Cloud Logging, GCP Marketplace)
- **Output**: Normalized signals: `{signal_name, payload}`
  - Example: `{quota_exceeded, #{service => "cloudrun", quota_key => "concurrent_requests"}}`
- **Mapping Rules**: Template loop allows arbitrary signal translation rules
- **Determinism**: Signal normalization is pure function (input → output is deterministic)

---

### HTTP & Network (2 modules)

**16. http_server.erl** (`http_server.erl.tera`)
- Signal ingestion HTTP server (listens for GCP Pub/Sub push notifications)
- **Endpoints**:
  - `POST /signals/:tenant_id`: Ingest signal for tenant
  - `POST /marketplace/entitlements`: Ingest Marketplace entitlement events
  - `GET /health`: Health check
- **Key Methods**:
  - Receives HTTP POST → parses JSON → calls `signal_normalizer:normalize/1`
  - Routes normalized signal via `tenant_registry:route_call/3`
  - Translates marketplace events via `entitlement_statem:translate_marketplace_event/1`
- **Receipts**: Logs all ingested signals for audit trail
- **Determinism**: HTTP path routing is deterministic

**17. http_router.erl** (`http_router.erl.tera`)
- Route handler dispatcher (separates concerns: routing vs. business logic)
- **Routes**:
  - `/signals/:tenant_id` → Signal ingestion (calls governor FSM)
  - `/marketplace/entitlements/:ent_id` → Entitlement event ingestion (calls entitlement FSM)
  - `/health` → Health check response
  - `/_status/receipts` → Query receipt ledger
- **Response Format**: JSON `{ok: true, result: ...} | {error: "reason"}`
- **Determinism**: Route matching order is deterministic

---

### GCP-Specific Connectors (2 modules)

**18. gcp_monitoring_connector.erl** (`gcp_monitoring_connector.erl.tera`)
- Queries GCP Cloud Monitoring (metrics) to synthesize signals
- **Key Methods**:
  - `query_quota_usage(TenantId, Service) → Usage`: Query current quota usage
  - `query_error_rate(TenantId, Service) → ErrorRate`: Query error rate
  - `query_latency_p99(TenantId, Service) → Latency`: Query 99th percentile latency
- **Integration**: Called by governor FSM (or signal normalizer) to gather real-time metrics
- **Determinism**: Metric queries are deterministic (same query → same response)

**19. gcp_marketplace_connector.erl** (`gcp_marketplace_connector.erl.tera`)
- GCP Marketplace integration for entitlement lifecycle
- **Key Methods**:
  - `register_sku(SkuMetadata) → ok`: Register SKU with Marketplace
  - `provision_account(TenantId, EntitlementId) → ok`: Provision customer account
  - `deprovision_account(TenantId) → ok`: Clean up customer account
- **Integration**: Called by entitlement FSM on state transitions
- **Determinism**: Operations are idempotent (safe to retry)

---

### Observability (2 modules)

**20. metrics_exporter.erl** (`metrics_exporter.erl.tera`)
- Prometheus metrics export
- **Metrics**:
  - `sku_signal_count{signal_name}` - Count of signals by type
  - `sku_action_count{action_name}` - Count of autonomic actions
  - `sku_decision_latency_ms{decision}` - Latency of policy decisions
  - `sku_entitlement_state{state}` - Current entitlement FSM state
  - `sku_governor_state{state}` - Current governor FSM states (by tenant)
  - `sku_action_success_rate{action}` - Success rate of autonomic actions
- **Endpoint**: `GET /metrics` (Prometheus text format)
- **Determinism**: Metric values at time T are deterministic

**21. trace_collector.erl** (`trace_collector.erl.tera`)
- OpenTelemetry trace collection (optional)
- **Spans**:
  - `signal_ingestion` - Span for signal ingestion
  - `policy_decision` - Span for policy decision execution
  - `action_execution` - Span for autonomic action execution
  - `gcp_request` - Span for GCP API calls
- **Purpose**: Distributed tracing for debugging (optional, can be disabled)
- **Determinism**: Traces capture deterministic execution sequence

---

### Configuration & Deployment (2 modules)

**22. config_loader.erl** (`config_loader.erl.tera`)
- Loads configuration from environment, TOML, or YAML
- **Config Keys**:
  - `gcp.project_id` - GCP project ID
  - `gcp.service_account_json` - Service account JSON path
  - `sku.id` - SKU identifier
  - `sku.profile` - Default profile (standard|premium|enterprise)
  - `http.port` - HTTP server port (default 8080)
  - `monitoring.enabled` - Enable Cloud Monitoring integration
  - `tracing.enabled` - Enable OpenTelemetry tracing
- **Priority**: Environment variables > TOML file > Defaults
- **Determinism**: Config loading is deterministic (no randomness)

**23. health_check.erl** (`health_check.erl.tera`)
- Health check module (readiness & liveness probes)
- **Methods**:
  - `liveness/0 → ok | {error, Reason}`: Is the system alive?
  - `readiness/0 → ok | {error, Reason}`: Is the system ready to accept traffic?
  - `status/0 → #{state: ..., components: [...]}`
- **Checks**:
  - Receipt ledger accessible?
  - Tenant registry running?
  - Entitlement FSM running?
  - HTTP server listening?
  - GCP credentials valid?
- **Endpoint**: `GET /health` (HTTP 200 if ready, 503 if not)
- **Determinism**: Health checks return deterministic status

---

## Template Generation Order

The ggen pipeline generates modules in this deterministic order:

### Phase 1: Initialization (read-only)
1. Load ggen.toml spec
2. Validate spec against SHACL schema
3. Extract template context via SPARQL queries
4. Resolve imports and dependencies

### Phase 2: Header & Types (foundational)
1. Generate `_header.erl.tera` (constants)
2. Generate `_types.hrl.tera` (type definitions)
3. Validate Erlang syntax

### Phase 3: Core Domain (ordered for dependencies)
1. Generate `policy_pack.erl`
2. Generate `governor_statem.erl`
3. Generate `entitlement_statem.erl`
4. Generate `tenant_registry.erl`

### Phase 4: Supervision (depends on domain)
1. Generate `tenant_dynsup.erl`
2. Generate `sku_sup.erl`

### Phase 5: Audit Trail (independent)
1. Generate `receipt_ledger.erl`
2. Generate `receipt_hash.erl`

### Phase 6: GCP Integration (can be generated in parallel)
1. Generate `gcp_actions_cloudrun.erl`
2. Generate `gcp_actions_scheduler.erl`
3. Generate `gcp_actions_pubsub.erl`
4. Generate `gcp_client_auth.erl`
5. Generate `signal_normalizer.erl`

### Phase 7: HTTP & Network (depends on domain)
1. Generate `http_server.erl`
2. Generate `http_router.erl`

### Phase 8: GCP Connectors (can be generated in parallel)
1. Generate `gcp_monitoring_connector.erl`
2. Generate `gcp_marketplace_connector.erl`

### Phase 9: Observability (independent)
1. Generate `metrics_exporter.erl`
2. Generate `trace_collector.erl`

### Phase 10: Configuration & Health (final)
1. Generate `config_loader.erl`
2. Generate `health_check.erl`

### Phase 11: Finalization (receipts)
1. Canonicalize all Erlang modules (erlang formatter)
2. Generate SHA-256 content hashes per file
3. Generate receipt with audit trail (JSON)

---

## Generation Context (Template Variables)

All templates have access to these variables:

### SKU Metadata
- `module_prefix`: String (e.g., "autonomic_gcp_sku_001")
- `sku`: String (e.g., "gcp-erlang-autonomics-premium")
- `tenant_key`: String (e.g., "account_id")
- `gcp.project_id`: String
- `gcp.region`: String (default: "us-central1")

### Policy Rules
- `rules`: Array of policy rules
  - `signal_name`: String
  - `condition`: Expression
  - `decision`: ok | warn | refuse
  - `actions`: Array of action names

### Actions
- `actions`: Array of autonomic actions
  - `name`: String (e.g., "throttle_concurrency")
  - `group`: String (e.g., "cloudrun")
  - `adapter_fn`: String (module function name)

### States
- `states`: Array of FSM states (boot, stable, warning, intervening, degraded, refusing)

### Connectors
- `connectors`: Array of GCP connectors (cloudrun, scheduler, pubsub, monitoring)

---

## Determinism & Reproducibility

**Core Principle**: Same spec file + same templates = identical Erlang code every time

### Determinism Guarantees

1. **Type System**: Records have fixed field order (bit-compatible)
2. **Pattern Matching**: State/event handling order is deterministic
3. **Templates**: Tera loops iterate in consistent order
4. **Hashing**: SHA-256 per-file ensures content integrity
5. **Timestamps**: ISO 8601 UTC ensures consistent audit trail ordering
6. **Random Numbers**: None in deterministic path (entropy only in runtime metrics)

### Verification

```bash
# Generate twice, verify identical output
ggen sync --audit true
cp -r examples/gcp-erlang-autonomics/gen gen1
ggen sync --audit true --force true
diff -r gen1/ examples/gcp-erlang-autonomics/gen
# Expected: No differences (byte-for-byte identical)
```

---

## Deployment Integration

Once all 23 modules are generated:

1. **Compile**: `rebar3 compile` or `erlc +debug_info *.erl`
2. **Package**: `rebar3 release` creates BEAM release tarball
3. **Deploy**: Deploy to GCP Compute Engine, GKE, or Cloud Run
4. **Monitor**: Scrape Prometheus metrics from `/metrics` endpoint
5. **Audit**: Query `/health`, `/status/receipts` for operational status

---

## Extension Points (Template Hooks)

Generated system is extensible via template variable arrays:

- **`rules`**: Add policy decision rules
- **`actions`**: Add autonomic actions (routed by group)
- **`connectors`**: Add GCP connectors
- **`states`**: Add FSM states (advanced - requires template patches)

Add new rules/actions in ggen.toml, regenerate entire system:
```bash
ggen sync --audit true
```

---

## Maintenance & Versioning

**Single Source of Truth**: ggen.toml spec file

To update system behavior:
1. Edit `ggen.toml` (add rules, actions, or change thresholds)
2. Run `ggen sync --audit true`
3. Review diff: `git diff examples/gcp-erlang-autonomics/gen/`
4. Commit: `git commit -m "chore: Regenerate Erlang Autonomics from updated spec"`
5. Deploy: Release engineering picks up new module versions

---

## Example: Complete SKU Specification

```toml
# ggen.toml - Complete SKU specification

[sku]
name = "GCP Erlang Autonomics Premium"
id = "autonomic-gcp-premium"
module_prefix = "autonomic_gcp_premium"
tenant_key = "account_id"

[gcp]
project_id = "my-gcp-project"
region = "us-central1"

# Policy rules (template loop: {% for r in rules %})
[[rules]]
signal_name = "quota_exceeded"
profile = "standard"
decision = "warn"
actions = ["throttle_concurrency"]

[[rules]]
signal_name = "quota_exceeded"
profile = "enterprise"
decision = "ok"

# Autonomic actions (template loop: {% for a in actions %})
[[actions]]
name = "throttle_concurrency"
group = "cloudrun"
adapter_fn = "throttle_concurrency"

[[actions]]
name = "pause_job"
group = "scheduler"
adapter_fn = "pause_job"

# GCP connectors
[[connectors]]
name = "cloudrun"
service = "Cloud Run"

[[connectors]]
name = "scheduler"
service = "Cloud Scheduler"
```

One spec → 23 modules → reproducible Erlang system! 🚀

---

**Last Updated**: 2026-01-25 | **Version**: 1.0 (Production-Ready)
