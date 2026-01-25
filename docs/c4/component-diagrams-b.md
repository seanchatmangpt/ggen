# C4 Component Diagrams - GCP Marketplace Autonomics (Level 3)

**Version**: 1.0.0 | **Date**: January 2026 | **Status**: Production-Ready

## Overview

Level 3 (Component) diagrams decompose containers into their logical architectural components, showing internal structure, component responsibilities, and inter-component relationships. These diagrams provide architects and senior developers with implementation guidance for the GCP Marketplace Autonomics system.

**C4 Model Hierarchy**:
- **Level 1** (System Context): External systems and users
- **Level 2** (Container): Major architectural containers (Autonomics Engine, Signal Processor, Policy Store, etc.)
- **Level 3** (Component): Internal components within containers ← **YOU ARE HERE**
- **Level 4** (Code): Classes, modules, functions (generated from code)

---

## 1. Signal Normalization Components

**Container**: Signal Normalization & Validation Layer
**Responsibility**: Accept multi-source signals (billing events, logs, monitoring data, audit events) and normalize into canonical signal schema

```mermaid
C4Component
    title Signal Normalization Components (gcp-marketplace-autonomics)

    Container(signal_norm, "Signal Normalization Layer", "Async Rust + Tokio", "Accepts raw signals from multiple sources")

    Component(schema_validator, "Schema Validator", "RDF + SHACL", "Validates signal payload against canonical schema, type checking, required fields enforcement")
    Component(payload_enforcer, "Payload Key Enforcer", "Rust/Serde", "Ensures signal keys match allowed set, denies unknown keys, enforces field types")
    Component(adapter_factory, "Source Adapter Factory", "Factory Pattern", "Creates appropriate adapters for each signal source type")

    Component(billing_adapter, "Billing Event Adapter", "GCP Billing API", "Adapts GCP Billing export events (SKU_ID, region, commitment_term)")
    Component(logging_adapter, "Cloud Logging Adapter", "GCP Cloud Logging API", "Adapts structured logs from autonomic actions (policy changes, quota updates)")
    Component(monitoring_adapter, "Cloud Monitoring Adapter", "GCP Cloud Monitoring API", "Adapts metric events (CPU usage, traffic patterns, cost trends)")
    Component(audit_adapter, "Cloud Audit Adapter", "GCP Cloud Audit Logs", "Adapts administrative events (deployments, config changes, access events)")

    Component(validation_router, "Validation Router", "Async Channels", "Routes signals to appropriate validators based on signal type, batches for throughput")
    Component(canonical_emitter, "Canonical Signal Emitter", "Message Queue", "Emits normalized signals in canonical format (v1.0)")

    Rel(schema_validator, payload_enforcer, "enforces keys via")
    Rel(payload_enforcer, validation_router, "forwards valid signals")
    Rel(adapter_factory, billing_adapter, "creates")
    Rel(adapter_factory, logging_adapter, "creates")
    Rel(adapter_factory, monitoring_adapter, "creates")
    Rel(adapter_factory, audit_adapter, "creates")
    Rel(billing_adapter, schema_validator, "emits raw", "proto")
    Rel(logging_adapter, schema_validator, "emits raw", "json")
    Rel(monitoring_adapter, schema_validator, "emits raw", "proto")
    Rel(audit_adapter, schema_validator, "emits raw", "json")
    Rel(validation_router, canonical_emitter, "validated signals")
```

**Component Responsibilities**:

| Component | Responsibility | Tech Stack | Timeout SLO |
|-----------|-----------------|-----------|------------|
| Schema Validator | RDF+SHACL validation of signal shape, type safety | `oxigraph`, `rio`, `sparql` | <100ms |
| Payload Key Enforcer | Whitelist enforcement, unknown key rejection | `serde`, `thiserror` | <10ms |
| Source Adapter Factory | Factory for creating typed adapters per source | Rust Factory Pattern | <1ms |
| Billing/Logging/Monitoring/Audit Adapters | Convert external format → internal canonical form | GCP client libs | <50ms each |
| Validation Router | Async channel routing, signal batching, throughput | `tokio::mpsc`, `tokio::sync` | <5ms |
| Canonical Signal Emitter | Emit normalized signals to message queue | `tokio`, message queue | <10ms |

**Context Variables** (from `.specify/*.ttl`):
```sparql
?schemaName            # e.g., "signal-v1-schema"
?allowedSignalKeys     # RDF list of allowed keys
?sourceTypes           # billing, logging, monitoring, audit
?validationRules       # SPARQL CONSTRUCT queries
?timeoutMs             # Per-component timeout (100ms recommended)
?batchSize             # Signal batch size (1000 recommended)
?channelCapacity       # Tokio channel capacity (10000 recommended)
```

---

## 2. Entitlement FSM Components

**Container**: Entitlement Management & Lifecycle
**Responsibility**: Process marketplace events, maintain entitlement state machine, generate receipt evidence

```mermaid
C4Component
    title Entitlement FSM Components (gcp-marketplace-autonomics)

    Container(entitle_mgmt, "Entitlement Management", "Async Rust", "Processes marketplace events, manages subscription lifecycle")

    Component(event_processor, "Marketplace Event Processor", "GCP Pub/Sub", "Listens to marketplace account changes, subscription events, cancellations")
    Component(fsm_state_machine, "Entitlement State Machine", "State Pattern + Enum", "Drives state transitions (Init→Active→Suspended→Cancelled), idempotent")
    Component(state_validator, "State Validator", "RDF State Shapes", "Validates state transition legality using SHACL shapes")
    Component(receipt_generator, "Receipt Generator", "JSON + Crypto", "Generates cryptographic receipt for each state transition")
    Component(lifecycle_manager, "Subscription Lifecycle Manager", "Async Manager", "Coordinates renewal dates, quota updates, grace periods")
    Component(entitle_storage, "Entitlement Store", "Cloud Datastore", "Stores current entitlement state + audit trail")

    Rel(event_processor, fsm_state_machine, "delivers events")
    Rel(fsm_state_machine, state_validator, "requests validation")
    Rel(state_validator, fsm_state_machine, "validates/rejects")
    Rel(fsm_state_machine, receipt_generator, "triggers on state change")
    Rel(receipt_generator, entitle_storage, "persists receipt")
    Rel(fsm_state_machine, lifecycle_manager, "triggers lifecycle updates")
    Rel(lifecycle_manager, entitle_storage, "reads/writes state")
    Rel(entitle_storage, fsm_state_machine, "provides current state")
```

**Component State Diagram**:
```
┌─────────────────────────────────────────────────────┐
│ Entitlement FSM State Space                         │
│                                                      │
│  Init ─(activate)→ Active ─(suspend)→ Suspended   │
│       ←(cancel)─       ↓                            │
│                   ┌─(renew)─┐                       │
│                   └→ Active  │                       │
│                              │                       │
│       (cancel) ─────────────→ Cancelled ─(delete)→ │
└─────────────────────────────────────────────────────┘

State Transitions:
  Init → Active (receipt: account_activated)
  Active → Suspended (receipt: subscription_suspended)
  Suspended → Active (receipt: subscription_renewed)
  Active → Cancelled (receipt: account_deleted)
  Any → Init (reset only on error recovery)
```

**Component Responsibilities**:

| Component | Responsibility | Tech Stack | Idempotency |
|-----------|-----------------|-----------|------------|
| Marketplace Event Processor | Listen to PubSub events, deduplicate | `tokio`, `google-cloud-pubsub` | Event ID + timestamp |
| Entitlement FSM | Drive state transitions, enforce invariants | Rust enum + pattern matching | Transaction ID |
| State Validator | SHACL shape validation, transition legality | `oxigraph`, `sparql` | No side effects |
| Receipt Generator | Cryptographic proof (HMAC-SHA256, timestamp) | `sha2`, `hmac` | Hash is deterministic |
| Lifecycle Manager | Manage renewal dates, quota syncs, grace periods | `tokio`, `chrono` | Idempotent timers |
| Entitlement Store | Persist state transitions with full audit trail | Cloud Datastore + TTL index | Optimistic locking |

**Context Variables** (from `.specify/*.ttl`):
```sparql
?initialState          # "INIT"
?activeState           # "ACTIVE"
?suspendedState        # "SUSPENDED"
?cancelledState        # "CANCELLED"
?allowedTransitions    # RDF list of valid transitions
?receiptFormat         # "application/json+receipt"
?receiptTTL            # Receipt retention time (90 days)
?graceperiodDays       # Days before actual cancellation (7 days)
?renewalCheckInterval  # How often to check renewals (daily)
```

---

## 3. Policy Update Components

**Container**: Policy Propagation & Config Management
**Responsibility**: Load ontology-driven policies, generate config changes, safely hot-reload or reboot

```mermaid
C4Component
    title Policy Update Components (gcp-marketplace-autonomics)

    Container(policy_mgmt, "Policy Management", "Async Rust", "Loads ontologies, generates configs, manages rollouts")

    Component(ontology_handler, "Ontology-Driven Update Handler", "RDF + SPARQL", "Loads ontology from Cloud Storage, executes SPARQL queries, materializes config")
    Component(config_builder, "Config Builder", "Tera Templates", "Renders config files from SPARQL results using Tera templates")
    Component(change_receipt, "Config Change Receipt Generator", "JSON + Deterministic", "Generates cryptographic receipt for each config change (manifest hash + diff)")
    Component(hot_reload_mgr, "Hot Reload Manager", "Signal + FifoMutex", "Applies config changes without restart (signal handlers, graceful reload)")
    Component(reboot_coord, "Reboot Coordinator", "Orchestrator Pattern", "Orchestrates graceful reboot (drain connections, save state, restart)")
    Component(rollback_handler, "Rollback Handler", "State Snapshots", "Stores pre-change snapshots, reverts on validation failure")
    Component(policy_store, "Policy Store", "Cloud Storage + Cache", "Stores ontology files, config snapshots, change history")

    Rel(ontology_handler, config_builder, "SPARQL results")
    Rel(config_builder, change_receipt, "config content")
    Rel(change_receipt, policy_store, "persists receipt + manifest hash")
    Rel(config_builder, hot_reload_mgr, "triggers reload check")
    Rel(config_builder, reboot_coord, "triggers if reload unsafe")
    Rel(hot_reload_mgr, policy_store, "validates pre-change state")
    Rel(reboot_coord, rollback_handler, "coordinates with")
    Rel(rollback_handler, policy_store, "restores from snapshot")
```

**Hot Reload vs Reboot Decision Tree**:
```
┌─────────────────────────────────────────────────┐
│ Policy Change                                    │
└──────────────────┬──────────────────────────────┘
                   │
         ┌─────────▼──────────┐
         │ Can be hot-reloaded?
         │ (validators pass)   │
         └─────────┬──────────┬┘
                   │          │
              YES  │          │  NO
                   │          │
         ┌─────────▼──────┐  ┌┴──────────────────┐
         │ Hot Reload     │  │ Graceful Reboot   │
         │ • Signal SIGHUP│  │ • Drain conns     │
         │ • Apply config │  │ • Save state      │
         │ • Validate     │  │ • Restart         │
         │ • Emit receipt │  │ • Validate state  │
         └────────────────┘  │ • Emit receipt    │
                             └───────────────────┘
```

**Component Responsibilities**:

| Component | Responsibility | Tech Stack | Determinism |
|-----------|-----------------|-----------|------------|
| Ontology Update Handler | Load RDF ontology, execute SPARQL queries | `oxigraph`, `sparql` | Materialize all triples |
| Config Builder | Render Tera templates from SPARQL context | `tera`, `serde_json` | Deterministic output |
| Config Change Receipt | Generate manifest hash + diff + signature | `sha2`, `hmac` | Hash all content |
| Hot Reload Manager | Apply changes without restart via signals | `tokio::signal`, `FifoMutex` | Validate before apply |
| Reboot Coordinator | Orchestrate graceful reboot sequence | `tokio::task`, state mgmt | Idempotent restart |
| Rollback Handler | Store snapshots, revert on failure | state snapshots, storage | Transaction-like |
| Policy Store | Persist ontologies, configs, receipts | Cloud Storage + local cache | Immutable receipts |

**Context Variables** (from `.specify/*.ttl`):
```sparql
?ontologySource        # "gs://bucket/ontologies/policies.ttl"
?policyNamespace       # "https://gcp.marketplace/policies/v1#"
?configTemplate        # Tera template path + name
?hotReloadRules        # Which fields allow hot reload
?rebootFields          # Which fields require reboot
?receiptFormat         # manifest hash + diff format
?snapshotRetention     # How long to keep rollback snapshots (30 days)
?validationTimeout     # How long to wait before forcing reboot (5 minutes)
```

---

## 4. Tenant Isolation Components

**Container**: Multi-Tenant Isolation & Quota Enforcement
**Responsibility**: Maintain per-tenant governors, enforce quotas, prevent cross-tenant interference

```mermaid
C4Component
    title Tenant Isolation Components (gcp-marketplace-autonomics)

    Container(isolation, "Tenant Isolation", "Async Rust", "Per-tenant governors, action queues, quota enforcement")

    Component(governor_registry, "Per-Tenant Governor Registry", "Arc<RwLock<HashMap>>", "Registry of tenant governors, cached for fast lookup, lazy initialization")
    Component(action_queue, "Per-Tenant Action Queue", "Async MPSC + Priority", "Per-tenant queue for autonomic actions, ordered execution, priority levels")
    Component(quota_enforcer, "Quota Enforcer", "Token Bucket + Sliding Window", "Enforces per-tenant quotas (actions/min, policy changes/day, cost_limit)")
    Component(tenant_mapper, "Tenant Context Mapper", "Request Context", "Extracts tenant ID from request, validates ownership, maps to governor")
    Component(isolation_validator, "Isolation Validator", "SHACL + Ownership", "Validates action is authorized for tenant, no cross-tenant access")
    Component(governor_storage, "Governor Storage", "Cloud Datastore + Cache", "Persists governor state, quota usage, audit trail per tenant")
    Component(action_executor, "Action Executor", "Serial per Governor", "Executes queued actions serially per tenant (prevents race conditions)")

    Rel(tenant_mapper, governor_registry, "lookup by tenant_id")
    Rel(governor_registry, governor_storage, "loads state from")
    Rel(action_queue, quota_enforcer, "before dequeue")
    Rel(quota_enforcer, action_queue, "reject if quota exceeded")
    Rel(action_queue, isolation_validator, "validates authorization")
    Rel(isolation_validator, action_executor, "authorized actions")
    Rel(action_executor, governor_storage, "updates quota usage")
    Rel(tenant_mapper, isolation_validator, "provides tenant context")
```

**Quota Enforcement Algorithm**:
```
┌────────────────────────────────────────────────┐
│ Per-Tenant Quota Enforcement                   │
│                                                 │
│ Token Bucket: capacity=Q, refill_rate=R/min   │
│                                                 │
│ on_action_request(tenant_id, action):         │
│   governor = registry.get(tenant_id)          │
│   if governor.tokens >= cost(action):         │
│     governor.tokens -= cost(action)           │
│     storage.record_usage(tenant_id, action)  │
│     return OK                                  │
│   else:                                        │
│     emit_signal(quota_exceeded, tenant_id)   │
│     return RATE_LIMITED                       │
│                                                 │
│ on_timer(every 1 minute):                      │
│   governor.tokens = min(capacity,             │
│     governor.tokens + rate * time_elapsed)   │
└────────────────────────────────────────────────┘
```

**Component Responsibilities**:

| Component | Responsibility | Tech Stack | Tenant Isolation |
|-----------|-----------------|-----------|------------------|
| Governor Registry | Fast O(1) lookup of tenant governors | `Arc<RwLock<DashMap>>` | Per-tenant entry |
| Action Queue | Ordered queue per tenant with priority | `tokio::sync::mpsc` + priority | Per-tenant queue |
| Quota Enforcer | Token bucket + sliding window enforcement | Custom bucket implementation | Per-tenant quota |
| Tenant Mapper | Extract + validate tenant identity | Request headers/context | Rejects invalid |
| Isolation Validator | Ownership check + authorization | SHACL shapes + RDF rules | SPARQL SHACL shapes |
| Governor Storage | Persist governor state + audit trail | Cloud Datastore + TTL index | Encrypted per tenant |
| Action Executor | Serial execution per governor | `tokio::task` + mutex | Single-threaded per tenant |

**Context Variables** (from `.specify/*.ttl`):
```sparql
?tenantIdentifier      # Extract from header (e.g., X-Tenant-ID)
?quotaCapacity         # Token bucket capacity (e.g., 100 actions)
?quotaRefillRate       # Tokens per minute (e.g., 10/min)
?quotaPolicies         # RDF list of quota rules per action type
?actionCosts           # Cost of each action type (e.g., policy_change=5)
?maxConcurrentActions  # Per-tenant max concurrent (1 = serial)
?governorTTL           # Cache TTL for governors (15 minutes)
?auditRetention        # Quota usage audit retention (90 days)
```

---

## Template Framework Overview

Each component diagram is parameterized via **Tera templates** and populated from RDF specifications:

```tera
{%- macro component_diagram(domain, sku_id, region, action_groups) %}
C4Component
    title {{ domain | capitalize }} Components ({{ sku_id }}-{{ region }})

    {%- for component in components[domain] %}
    Component({{ component.id }}, "{{ component.name }}",
        "{{ component.tech }}", "{{ component.description }}")
    {%- endfor %}

    {%- for rel in relationships[domain] %}
    Rel({{ rel.from }}, {{ rel.to }}, "{{ rel.label }}")
    {%- endfor %}
{% endmacro %}
```

**Integration Points**:
1. **Specification file**: `.specify/specs/autonomics-marketplace/components.ttl`
2. **Template file**: `templates/c4/component-diagram.tera`
3. **Context variables**: SPARQL results from ontology
4. **Output**: Markdown with embedded Mermaid diagrams

---

## Production Validation Checklist

- [ ] All components have explicit responsibilities and tech stack
- [ ] All relationships have directional labels
- [ ] Timeout SLOs defined for each component
- [ ] Context variables documented from `.specify/*.ttl`
- [ ] State machines visualized for stateful components
- [ ] Quota/rate limiting algorithms shown explicitly
- [ ] Error paths documented (rollback, reboot scenarios)
- [ ] Tenant isolation boundaries clearly marked
- [ ] All diagrams render without syntax errors in Mermaid
- [ ] Cross-reference to Container diagrams (Level 2)
- [ ] File naming follows: `component-{domain}-{sku_id}.md`

---

## Integration with Container Diagrams (Level 2)

These Component diagrams expand the following containers from Level 2:

| Level 2 Container | Level 3 Components File | Scope |
|-------------------|------------------------|-------|
| Signal Normalization & Validation | `component-signals-autonomics.md` | Adapters, validation routers, schema enforcement |
| Entitlement Management | `component-entitlements-autonomics.md` | FSM, lifecycle, receipts |
| Policy Propagation | `component-policies-autonomics.md` | Ontology loading, config building, rollout |
| Tenant Isolation | `component-isolation-autonomics.md` | Governors, quotas, action queues |

---

## Next Steps (Level 4 - Code)

These Component diagrams decompose further into:
- **Rust modules**: `crates/ggen-autonomics/src/{signals,entitlements,policies,isolation}/`
- **Type definitions**: Strongly-typed RDF entities (PhantomData state machines)
- **Test suites**: Chicago TDD integration tests validating component contracts
- **Performance benchmarks**: Verify component SLOs (e.g., <100ms validation)

---

**Generated**: January 25, 2026
**Branch**: `claude/erlang-autonomic-c4-diagrams-V7Hpq`
**Status**: ✅ Production-Ready
