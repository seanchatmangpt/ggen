<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [C4 Level 3: Component Architecture - Governor Service Internals](#c4-level-3-component-architecture---governor-service-internals)
  - [Component Context: Inside the Governor](#component-context-inside-the-governor)
  - [C4 Diagram 1: Governor Service Components (Ingress Path)](#c4-diagram-1-governor-service-components-ingress-path)
  - [C4 Diagram 2: Governor Service Components (FSM & Policy Evaluation)](#c4-diagram-2-governor-service-components-fsm--policy-evaluation)
  - [C4 Diagram 3: Governor Service Components (Intervention & Observability)](#c4-diagram-3-governor-service-components-intervention--observability)
  - [C4 Diagram 4: Component Interactions (Full Sequence)](#c4-diagram-4-component-interactions-full-sequence)
  - [Component Interaction Sequence](#component-interaction-sequence)
  - [Glossary Cross-Reference](#glossary-cross-reference)
  - [Receipt Contract (Evidence Plane)](#receipt-contract-evidence-plane)
  - [Performance Characteristics](#performance-characteristics)
  - [Definition of Done](#definition-of-done)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# C4 Level 3: Component Architecture - Governor Service Internals

**Document Purpose**: Decompose the Governor Service into reusable components: gen_statem FSM, policy evaluators, invariant checkers, action routers, and observability exporters.

**Version**: 1.0 | **Date**: 2026-01-25

---

## Component Context: Inside the Governor

The **Governor Service** (Cloud Run container) contains 6 core components:

| Component | Purpose | Pattern |
|-----------|---------|---------|
| **Cowboy Ingress** | HTTP endpoint, request parsing, response formatting | Erlang cowboy_http_handler |
| **JSON Decoder** | Deserialize signal JSON, type validation | serde (Rust) or jiffy (Erlang) |
| **Rate Limiter** | Token bucket per principal, storm detection | Erlang `rate_limiter` (custom) |
| **gen_statem FSM** | Finite state machine, deterministic state transitions | Erlang OTP `gen_statem` behavior |
| **Policy Evaluator** | SPARQL SELECT queries, RDF inference, rule execution | Oxigraph SPARQL engine |
| **Action Router** | Capability dispatch, entitlement boundary checking | Pattern matching (Rust/Erlang) |
| **Receipt Emitter** | Signal emission to Evidence Sidecar | Message queue (local or Pub/Sub) |
| **Prometheus Exporter** | Metrics collection, histogram/counter families | Prometheus text format |
| **OpenTelemetry Tracer** | Distributed tracing, span generation, parent linking | OTEL SDK (optional feature) |

---

## C4 Diagram 1: Governor Service Components (Ingress Path)

```mermaid
C4Component
    title Governor Service - Ingress Request Flow

    Container(http_server, "Cowboy HTTP Server", "Erlang HTTP", "Listens port 8080, routes to ingress handler")

    Component(ingress_handler, "Ingress Handler", "HTTP Handler", "Parses request, extracts JWT, validates signature")
    Component(json_decoder, "JSON Decoder", "Parser", "Deserializes signal JSON, type validation")
    Component(rate_limiter, "Rate Limiter", "Policy Enforcer", "Token bucket per principal, storm detection (>50 signals/10s)")
    Component(signal_buffer, "Signal Buffer", "Queue", "Aggregates signals into 5s windows")
    Component(fsm_dispatcher, "FSM Dispatcher", "Router", "Routes aggregated signals to gen_statem")

    Rel(http_server, ingress_handler, "delegates HTTP request to handler")
    Rel(ingress_handler, json_decoder, "deserialize signal")
    Rel(json_decoder, rate_limiter, "check quota")
    Rel(rate_limiter, signal_buffer, "buffer signal if accepted")
    Rel(signal_buffer, fsm_dispatcher, "emit aggregated batch on window close")

    UpdateElementStyle(fsm_dispatcher, $bgColor=#FF6B6B, $fontColor=#FFFFFF)
    UpdateElementStyle(rate_limiter, $bgColor=#FFD700, $fontColor=#000000)
```

**Component Details**:

**Cowboy HTTP Server**:
- Port 8080 (configurable, default 8080)
- Routes: `POST /signal` → ingress_handler
- Routes: `GET /health` → health_check_handler
- SSL/TLS: Enforce HTTPS (via Cloud Load Balancer)

**Ingress Handler**:
- Extracts JWT from `Authorization: Bearer <token>` header
- Validates token signature against GCP public keys (cached, TTL 1h)
- Enriches request context: principal, sku_scope, entitlement_expiry
- Returns 401 if token invalid/expired
- Returns 429 if principal quota exceeded

**JSON Decoder**:
- Validates signal schema (required fields: `signal_type`, `resource_id`, `severity`)
- Type checks: signal_type ∈ {monitoring, alert, audit, compliance}
- Rejects malformed JSON (returns 400)
- Enriches decoded signal: timestamp_received, principal_id

**Rate Limiter**:
- Per-principal token bucket: `capacity = sku.signal_quota_per_minute`
- Token refill: 1 token per second (configurable)
- Storm detection: If tokens drained in <10s, post storm warning to Pub/Sub
- Returns 429 (too many requests) if bucket empty

**Signal Buffer**:
- Aggregation window: 5s (configurable per policy)
- Deduplication key: `(signal_type, resource_id, timestamp_bucket_5s)`
- On window close: Emit batch to FSM dispatcher
- Max batch size: 100 signals (prevents unbounded growth)

**FSM Dispatcher**:
- Routes aggregated signal batch to gen_statem
- Starts new gen_statem instance (per batch) or reuses warm instance
- Returns response to HTTP client (202 Accepted)

---

## C4 Diagram 2: Governor Service Components (FSM & Policy Evaluation)

```mermaid
C4Component
    title Governor Service - FSM & Policy Evaluation Path

    Container(fsm_process, "gen_statem FSM", "Erlang OTP", "Finite state machine, deterministic transitions")

    Component(state_evaluator, "State Evaluator", "Pattern Matcher", "Matches signal to current state, determines event handler")
    Component(policy_loader, "Policy Loader", "Cache", "Loads policy-pack.ttl from GCS (cached), versioned")
    Component(sparql_executor, "SPARQL Executor", "RDF Query Engine", "Executes SELECT queries against RDF graph (Oxigraph)")
    Component(rule_evaluator, "Rule Evaluator", "Interpreter", "Evaluates policy rules in sequence, builds decision path")
    Component(invariant_checker, "Invariant Checker", "Validator", "Checks type safety, business logic constraints")
    Component(action_router, "Action Router", "Dispatcher", "Routes decision to remediator or intervention handler")

    Rel(fsm_process, state_evaluator, "signal arrives, current state")
    Rel(state_evaluator, policy_loader, "request current policy version")
    Rel(policy_loader, sparql_executor, "policy rules (RDF)")
    Rel(sparql_executor, rule_evaluator, "SPARQL results")
    Rel(rule_evaluator, invariant_checker, "proposed action, signal context")
    Rel(invariant_checker, action_router, "validated action (or rejection)")

    UpdateElementStyle(fsm_process, $bgColor=#FF6B6B, $fontColor=#FFFFFF)
    UpdateElementStyle(sparql_executor, $bgColor=#4ECDC4, $fontColor=#FFFFFF)
    UpdateElementStyle(invariant_checker, $bgColor=#FFD700, $fontColor=#000000)
```

**Component Details**:

**State Evaluator**:
- Matches (current_state, event) pair to handler
- States: idle, evaluating, deciding, executing, emitting
- Events: signal_arrived, policy_result, decision_made, action_result, receipt_sent
- Returns: handler function (deterministic)

**Policy Loader**:
- Caches policy-pack.ttl in memory (TTL 5s, refreshed via Pub/Sub announcement)
- Policy file path: `gs://autonomics-policy-bucket/policy-pack/v{version}.ttl`
- Maintains version number (for receipt metadata)
- On cache miss: Fetches from GCS, parses Turtle RDF, caches

**SPARQL Executor**:
- Loads RDF graph (policy-pack.ttl) into Oxigraph
- Executes SELECT queries (policy rules are SPARQL queries)
- Example query:
  ```sparql
  PREFIX policy: <http://autonomics.ggen/policy/>
  SELECT ?action_type ?consent_required
  WHERE {
    ?rule a policy:SecurityThreatRule ;
      policy:matchesSignalType "security_threat" ;
      policy:proposedAction ?action_type ;
      policy:requiresHumanConsent ?consent_required .
  }
  ```
- Results: Bindings for {action_type, consent_required, ...}

**Rule Evaluator**:
- Iterates through policy rules sequentially
- For each rule: Evaluates SPARQL SELECT
- Builds decision path: List[(rule_name, result)]
- Stops at first matching rule (veto rule can block further evaluation)
- Example decision path:
  ```json
  [
    {"rule": "policy/security/threat-detect", "result": true, "matched": true},
    {"rule": "policy/scope/within-sku", "result": true, "matched": true},
    {"rule": "policy/consent/auto-approve", "result": false, "matched": false}
  ]
  ```

**Invariant Checker**:
- Type safety: Action type valid? Parameters have correct types?
- Resource constraints: Quota available? Service enabled?
- Contradiction checking: Can't update + delete same resource
- Entitlement boundary: Action within sku_scope?
- Returns: {valid: bool, reason: String (if invalid)}

**Action Router**:
- Routes action to one of 3 handlers:
  1. **Direct Execution**: Action allowed, no consent needed → Emit to remediator
  2. **Intervention (Warn)**: Action requires consent → Wait for human approval
  3. **Refusal**: Action policy-blocked or scope-violated → Return 403
- Emits action to Pub/Sub `action-emit` topic (for remediator)

---

## C4 Diagram 3: Governor Service Components (Intervention & Observability)

```mermaid
C4Component
    title Governor Service - Intervention Flow & Observability

    Component(intervention_handler, "Intervention Handler", "State Manager", "Manages warn→approval→execute flow")
    Component(consent_waiter, "Consent Waiter", "Async", "Waits for human approval via Pub/Sub")
    Component(metrics_collector, "Metrics Collector", "Instrumentation", "Counter (decisions/s), Histogram (latency), Gauge (queue depth)")
    Component(prometheus_exporter, "Prometheus Exporter", "Metrics", "Exposes /metrics endpoint, text format")
    Component(otel_tracer, "OpenTelemetry Tracer", "Tracing", "[Optional] Generates spans, parent linking")
    Component(span_exporter, "Span Exporter", "Export", "[Optional] Sends traces to Cloud Trace")

    Rel(action_router, intervention_handler, "if action needs consent")
    Rel(intervention_handler, consent_waiter, "wait for approval signal")
    Rel(consent_waiter, action_router, "approval received → route to executor")

    Rel(state_evaluator, metrics_collector, "increment state_transitions counter")
    Rel(rule_evaluator, metrics_collector, "record rule_evaluation_latency histogram")
    Rel(action_router, metrics_collector, "increment action_routed counter")

    Rel(metrics_collector, prometheus_exporter, "metrics snapshot")
    Rel(fsm_process, otel_tracer, "create span for FSM transition")
    Rel(otel_tracer, span_exporter, "export span")

    UpdateElementStyle(intervention_handler, $bgColor=#FFD700, $fontColor=#000000)
    UpdateElementStyle(prometheus_exporter, $bgColor=#4ECDC4, $fontColor=#FFFFFF)
    UpdateElementStyle(otel_tracer, $bgColor=#96CEB4, $fontColor=#FFFFFF)
```

**Component Details**:

**Intervention Handler**:
- When action requires human consent (policy: `requiresHumanConsent = true`)
- Waits for approval signal from Pub/Sub `approval-response` topic
- Timeout: 5 minutes (configurable)
- On approval: Continues FSM → executing state
- On timeout/rejection: Cancels action (receipt: cancelled)
- Emits alert to oncall (if system configured with alerting)

**Consent Waiter**:
- Subscribes to approval-response topic
- Filters by (action_id, principal_id)
- Waits up to 5s (blocks FSM progress)
- Returns: {approved: bool, approver_id: String, approval_timestamp: DateTime}

**Metrics Collector**:
- Counter: `gov_state_transitions_total{state_from,state_to}` (incremented per transition)
- Counter: `gov_decisions_total{decision_type,result}` (e.g., "direct" + "allowed")
- Histogram: `gov_policy_evaluation_seconds` (buckets: 10ms, 50ms, 100ms, 500ms, 1s, 5s)
- Histogram: `gov_action_routing_seconds` (same buckets)
- Gauge: `gov_signal_buffer_depth` (current queue depth)
- Gauge: `gov_policy_cache_age_seconds` (age of cached policy version)

**Prometheus Exporter**:
- Exposes `GET /metrics` endpoint (port 8081, internal only)
- Text format: Prometheus metric lines
- Aggregates metrics from Metrics Collector every 10s
- Example output:
  ```
  gov_state_transitions_total{state_from="idle",state_to="evaluating"} 12345
  gov_policy_evaluation_seconds_bucket{le="0.1"} 10000
  gov_policy_evaluation_seconds_sum 654.32
  gov_policy_evaluation_seconds_count 12345
  ```

**OpenTelemetry Tracer**:
- [Optional feature, enabled with `--features otel`]
- Creates span for each FSM transition
- Span attributes: signal_id, policy_version, action_type, result
- Parent/child relationships: signal → policy_eval → action_route
- Exports to Cloud Trace (GCP)

**Span Exporter**:
- Batch exporter (collects 100 spans or 5s timeout)
- Sends to Google Cloud Trace API
- Traces queryable in GCP Cloud Console
- Filter: By latency, error rate, principal_id, action_type

---

## C4 Diagram 4: Component Interactions (Full Sequence)

```mermaid
C4Component
    title Governor Service - Signal to Receipt (Full Component Sequence)

    Component(http_ingress, "HTTP Ingress", "Handler", "1. Receive signal, validate JWT")
    Component(decode, "Decode", "Parser", "2. Parse JSON, type check")
    Component(rate_limit, "Rate Limit", "Enforcer", "3. Token bucket check")
    Component(aggregate, "Aggregate", "Buffer", "4. Window signals 5s")
    Component(dispatch, "Dispatch", "Router", "5. Send batch to FSM")
    Component(fsm_eval, "FSM Eval", "State Machine", "6. Load policy, evaluate rules")
    Component(invariant, "Invariant", "Validator", "7. Check constraints")
    Component(route, "Route", "Dispatcher", "8. Route to executor or intervention")
    Component(emit, "Emit", "Publisher", "9. Publish action to remediator")
    Component(receipt, "Receipt", "Generator", "10. Generate signed receipt")

    Rel(http_ingress, decode, "signal JSON")
    Rel(decode, rate_limit, "decoded signal")
    Rel(rate_limit, aggregate, "rate-limited signal")
    Rel(aggregate, dispatch, "aggregated batch")
    Rel(dispatch, fsm_eval, "batch event")
    Rel(fsm_eval, invariant, "proposed action")
    Rel(invariant, route, "validated action")
    Rel(route, emit, "action to execute")
    Rel(emit, receipt, "action result")

    UpdateElementStyle(fsm_eval, $bgColor=#FF6B6B, $fontColor=#FFFFFF)
    UpdateElementStyle(receipt, $bgColor=#45B7D1, $fontColor=#FFFFFF)
```

---

## Component Interaction Sequence

**Happy Path** (All checks pass):
```
1. Signal arrives → HTTP Ingress
2. JWT validated ✓
3. JSON parsed ✓
4. Rate limit check ✓
5. Signal buffered
6. Aggregation window closes → Batch dispatched
7. FSM evaluates signal batch
8. Policy rules executed → Decision made
9. Invariant checks pass ✓
10. Action not requiring consent → Route to executor
11. Pub/Sub action-emit published
12. Remediator executes action
13. Action result returned
14. Receipt generated + hash-chained + signed
15. Firestore write + Cloud Logging emit
16. HTTP response: 202 Accepted
```

**Failure Path** (Refusal):
```
1-8. Same as above
9. Invariant check FAILS (e.g., action outside sku_scope)
10. Action Router returns Refusal
11. Receipt generated: result="refused", reason="outside_scope"
12. Firestore write + Cloud Logging emit
13. HTTP response: 200 OK (receipt acknowledges refusal)
```

**Intervention Path** (Consent required):
```
1-9. Same as above
10. Policy rule indicates consent required
11. Intervention Handler activated
12. Consent Waiter blocks, waiting for approval
13. Human approves via Pub/Sub approval-response
14. Consent Waiter returns approval
15. FSM transitions to executing → executor publishes action
16. Receipt includes: intervention_type="warn", consent_result="approved"
```

---

## Glossary Cross-Reference

- **gen_statem**: Erlang OTP finite state machine behavior (deterministic state transitions)
- **SPARQL**: Query language for RDF graphs (SELECT queries return bindings)
- **Invariant**: Constraint that must remain true (type safety, resource availability)
- **Jidoka**: Stop-the-line enforcement; refuse action if policy or evidence fails
- **Intervention**: Warn → human approval → execute flow (requires consent)
- **Hash-Chain**: Merkle-linked receipts; each receipt includes SHA256 of prior
- **Prometheus**: Metrics collection format (counter, histogram, gauge families)
- **OpenTelemetry**: Distributed tracing standard (optional feature, enabled via `--features otel`)

---

## Receipt Contract (Evidence Plane)

**Component Interactions Create Receipts**:

1. **After Signal Ingestion**: No receipt yet (signal processing, no action)
2. **After FSM Evaluation**: No receipt yet (decision made, not yet executed)
3. **After Action Routing**: Receipt generated IF:
   - Action executed: Receipt with result="success" or "failure"
   - Action refused: Receipt with result="refused"
   - Action postponed: Receipt with result="postponed"
4. **After Receipt Emission**: Firestore write + Cloud Logging mirror

**Receipt Contains**:
- Decision path: All policy rules evaluated + results (from Rule Evaluator)
- Invariant results: All constraints checked + results (from Invariant Checker)
- Action routed: Type, parameters, capability required (from Action Router)
- Intervention details: If consent required, consent result (from Intervention Handler)

---

## Performance Characteristics

**Component Latencies** (P99):

| Component | Latency |
|-----------|---------|
| HTTP Ingress (JWT validation) | <10ms |
| JSON Decoder | <5ms |
| Rate Limiter (token bucket) | <1ms |
| Signal Buffer (aggregation) | <5ms (per signal) |
| FSM Dispatcher | <1ms |
| Policy Loader (cached) | <20ms (miss: <500ms) |
| SPARQL Executor | <50ms (10-20 rules) |
| Rule Evaluator | <30ms (10 rules) |
| Invariant Checker | <10ms |
| Action Router | <5ms |
| Metrics Collector | <1ms |
| **Total Signal-to-Decision** | **<2s** (SLO target) |

---

## Definition of Done

- [ ] C4 component diagram 1: Ingress path (6 components, 5 relationships)
- [ ] C4 component diagram 2: FSM & policy evaluation (6 components, 6 relationships)
- [ ] C4 component diagram 3: Intervention & observability (6 components, 4 relationships)
- [ ] C4 component diagram 4: Full sequence (10 components, 9 relationships)
- [ ] All 4 diagrams render in Mermaid without syntax errors
- [ ] gen_statem FSM: 5 states (idle, evaluating, deciding, executing, emitting) documented
- [ ] Policy Evaluator: SPARQL query example provided
- [ ] Invariant Checker: Type safety + constraint examples listed
- [ ] Intervention Handler: Consent flow documented (warn → approval → execute)
- [ ] Metrics: Counter, histogram, gauge families defined with labels
- [ ] Performance SLOs: All component latencies documented (P99 <2s total)
- [ ] Receipt generation: All components that contribute to receipt defined
- [ ] Jidoka enforcement: Refusal path documented (policy block, scope violation)
- [ ] OpenTelemetry: Optional feature integration documented

---

**Next**: See [c4-runtime.md](c4-runtime.md) for Level 4 runtime behavior (signal sequences, state transitions, intervention flows, storm handling).
