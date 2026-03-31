# ggen v2026.3.30 Comprehensive Test Plan

## Executive Summary

81 crates. 6 semconv span groups. 3 crates with ZERO tests. 1 crate with zero tests on payment processing. 3 semconv span name mismatches in production code. No in-memory TestExporter utility anywhere. This plan addresses every gap with Chicago TDD rigor: RED (write failing test on observable output) -> GREEN (implement to make it pass) -> REFACTOR (clean up while green).

---

## 1. Current State

### Workspace Composition

| Category | Crates | Test Files | OTEL Instrumented |
|----------|--------|------------|-------------------|
| Core | ggen-core, ggen-domain, ggen-cli | 25+ (core), 15+ (domain), 20+ (cli) | core only |
| AI/LLM | ggen-ai, ggen-a2a-mcp, ggen-dspy | 30+ (a2a-mcp) | ai, a2a-mcp |
| OTEL | ggen-tps-andon, knhk-otel | 2 (andon) | andon |
| Transport | ggen-transport, a2a-generated | 1 (transport) | none |
| Domain logic | ggen-yawl, ggen-workflow, ggen-workflow-43 | 3 (yawl) | none |
| TPS | ggen-heijunka, ggen-kaizen, ggen-metrics-tps | 8 (heijunka 4, kaizen 4) | none |
| Consensus | ggen-consensus | 5 | none |
| Marketplace | ggen-marketplace, ggen-domain | 15+ (domain) | none |
| Payment | ggen-payments | **0** | none |
| SaaS | ggen-saas | 0 external | none |
| Utility | ggen-utils, ggen-canonical, ggen-receipt, ggen-codegen | 7 (utils) | none |
| Quality | ggen-jidoka, ggen-backpressure | 0 external (jidoka), 1 (backpressure) | none |
| Protocol | ggen-a2a, ggen-packet, ggen-prompt-mfg | 0 external | none |
| Testing | ggen-testing | N/A (test infra) | N/A |
| Excluded | ggen-workflow (erlang NIF), knhk-hot, tps-jidoka | - | - |

### 6 Semconv Span Groups

| Group | Span Name | Required Attributes | Source | Test Coverage |
|-------|-----------|--------------------|--------|---------------|
| ggen.a2a | `ggen.a2a.message` | `a2a.message_id`, `a2a.operation_name` | correlation.rs | Partial (4 spans in weaver) |
| ggen.mcp | `ggen.mcp.tool_call` | `mcp.tool_name` | ggen_server.rs | Good (10 spans) |
| ggen.llm | `ggen.llm.generation` | `llm.model`, `llm.prompt_tokens`, `llm.total_tokens` | client.rs | Good (real Groq calls) |
| ggen.yawl | `ggen.yawl.task_execution` | `yawl.task_id`, `yawl.task_name`, `yawl.state.from`, `yawl.state.to` | yawl_bridge/ | Good (31 spans) |
| ggen.pipeline | `ggen.pipeline.operation` | `pipeline.operation`, `pipeline.duration_ms` | handlers.rs | Partial (12 spans) |
| ggen.error | `ggen.error` | `error.type`, `error.message` | **NOWHERE** | **ZERO** |

### Live-Check Pipeline Status

```
Lib tests   : PASS (53 lib + 15 integration)
Groq tests  : PASS (14 real API tests)
Gate        : PASS (2823 entities, 5/6 groups, 0 violations)
```

Missing: `ggen.error` (zero instrumentation anywhere).

---

## 2. Critical Gaps (Must Fix First)

### Gap 1: ggen-payments — ZERO tests on payment processing

**Priority: CRITICAL**
**Why:** Revenue-critical crate making HTTP calls to Stripe with zero test coverage.

Chicago TDD approach:
1. RED: Write tests asserting on webhook signature verification output (not mock assertions)
2. RED: Write tests asserting on payment intent parsing return values
3. RED: Write tests asserting on error handling state transitions
4. GREEN: Implement to make tests pass
5. REFACTOR: Extract shared signature verification logic

### Gap 2: ggen.error span — ZERO instrumentation

**Priority: CRITICAL**
**Why:** The 6th semconv group has no code behind it. Error paths record `error=true` on existing spans but never create a dedicated `ggen.error` span.

Chicago TDD approach:
1. RED: Write test that calls a function producing an error, captures spans, asserts `ggen.error` span exists
2. GREEN: Add `#[tracing::instrument(name = "ggen.error")]` on error paths in ggen-a2a-mcp
3. GREEN: Add `ggen.error` spans in ggen-core error paths
4. Verify: Run live-check, assert 6/6 groups

### Gap 3: a2a.operation_name missing from spans

**Priority: HIGH**
**Why:** `span.ggen.a2a.message` requires `a2a.operation_name` per semconv, but `process_message()` and `correlation::span_from_a2a_context()` never record it.

Chicago TDD approach:
1. RED: Write test asserting `a2a.operation_name` attribute exists on captured span
2. GREEN: Add `a2a.operation_name` recording in `correlation::span_from_a2a_context()`
3. Verify: live-check passes with correct attributes

### Gap 4: Semconv span name mismatches in production code

**Priority: HIGH**
**Why:** Three production crates emit spans with names that don't match the semconv model. Weaver will flag these as violations.

| Crate | Current Span Name | Semconv Expected |
|-------|------------------|-----------------|
| ggen-ai (`client.rs`) | `llm.complete` | `ggen.llm.generation` |
| ggen-a2a-mcp (`correlation.rs`) | `a2a.message` | `ggen.a2a.message` |
| ggen-core (`registry.rs`) | `ggen.market.search` | No semconv definition |

Chicago TDD approach:
1. RED: Write test asserting span name matches semconv (e.g., `ggen.llm.generation`)
2. GREEN: Rename `info_span!("llm.complete", ...)` to `info_span!("ggen.llm.generation", ...)`
3. GREEN: Fix `correlation.rs` to use `"ggen.a2a.message"`
4. Verify: weaver live-check shows zero name violations

### Gap 5: ggen-transport — ZERO OTEL on inter-service communication

**Priority: HIGH**
**Why:** All MCP/A2A/gRPC calls go through transport. Without spans here, distributed tracing is blind.

Chicago TDD approach:
1. RED: Write test asserting RPC spans exist during gRPC transport
2. GREEN: Add `#[tracing::instrument]` to gRPC method handlers
3. GREEN: Add RPC semconv attributes (`rpc.system`, `rpc.service`, `rpc.method`)

### Gap 6: No in-memory TestExporter utility

**Priority: HIGH**
**Why:** Every OTEL test either requires a running collector, just verifies function completion (not attributes), or makes real API calls. There is no way to programmatically assert on span attributes in unit tests.

Chicago TDD approach:
1. RED: Write test that captures a span in memory and asserts on its attributes
2. GREEN: Create `TestSpanExporter` in `ggen-testing` crate implementing `SpanExporter` trait
3. GREEN: Add `SpanCapture` wrapper with `assert_span_exists()` and `assert_attribute()` methods
4. Verify: All OTEL tests can use it without external collector

---

## 3. Per-Crate Test Inventory

### CRITICAL Priority

| Crate | Current Tests | Current OTEL | Required Action |
|-------|--------------|--------------|-----------------|
| ggen-payments | **0** | none | Full Chicago TDD suite for Stripe webhook, payment intent, error handling |
| ggen.error spans | N/A | **zero** | Add instrumentation to ggen-a2a-mcp + ggen-core error paths |

### HIGH Priority

| Crate | Current Tests | Current OTEL | Required Action |
|-------|--------------|--------------|-----------------|
| ggen-core | 68+ inline, 25+ integration | telemetry.rs, registry spans | Add TestExporter for programmatic span assertion; align fields with semconv |
| ggen-a2a-mcp | 30+ test files | 4 span groups | Fix span name `a2a.message` -> `ggen.a2a.message`; add `a2a.operation_name`; add `ggen.error` spans |
| ggen-ai | 127+ inline, 15+ integration | Partial (wrong names) | Rename `llm.complete` -> `ggen.llm.generation`; add TestExporter-based span attribute tests |
| ggen-transport | 12 inline, 1 integration | none | Add gRPC RPC semconv spans; add trace context propagation |
| ggen-api | 68 inline, 2 integration | none | Add `#[tracing::instrument]` to all API handlers; add HTTP server semconv |
| ggen-auth | 97 inline, 0 external | none | Add `#[tracing::instrument]` to JWT/OAuth2 flows; add error spans for auth failures |
| ggen-dspy | 127+ inline, 0 external | none | Add pipeline spans for optimizer iterations; create external integration tests |
| ggen-saas | 10 inline, 0 external | none | Add integration tests for quota enforcement edge cases |
| ggen-domain | 80+ inline, 15+ integration | none | Add `#[tracing::instrument]` to domain operations |
| ggen-cli | 20+ test files | none | Add pipeline operation spans to CLI commands |

### MEDIUM Priority

| Crate | Current Tests | Current OTEL | Required Action |
|-------|--------------|--------------|-----------------|
| ggen-yawl | 53+ inline, 3 external | references only | Add pipeline spans for code generation rules |
| ggen-tps-andon | 25+ inline, 2 external | 5 instrumented | Expand instrumentation to all signal handlers |
| ggen-consensus | 44+ inline, 5 external | none | Add spans to PBFT phases |
| ggen-jidoka | 32+ inline, 0 external | none | Add error spans for quality gate failures |
| ggen-metrics-tps | 29+ inline, 2 external | Prometheus only | Evaluate OTEL metrics bridge |
| ggen-execution | 33+ inline, 0 external | none | Add pipeline spans |
| ggen-a2a | 29+ inline, 0 external | none | Add `ggen.a2a.message` spans to state transitions |
| ggen-marketplace | 94+ inline, 0 external | none | Add external integration tests |
| ggen-prompt-mfg | 29+ inline, 0 external | none | Add pipeline spans to compilation |

### LOW Priority

| Crate | Current Tests | Current OTEL | Required Action |
|-------|--------------|--------------|-----------------|
| ggen-heijunka | 65+ inline, 4 external | none | Optional: add scheduling decision spans |
| ggen-kaizen | 54+ inline, 4 external | none | Optional: add PDCA cycle spans |
| ggen-utils | 27+ inline, 7 external | none | No OTEL needed (utility) |
| ggen-codegen | 3 inline, 0 external | none | Add more unit tests |
| ggen-canonical | 27+ inline, 0 external | none | No OTEL needed (deterministic) |
| ggen-packet | 42+ inline, 0 external | none | Optional: add routing spans |
| ggen-receipt | 16+ inline, 0 external | none | Optional: add audit trail spans |
| ggen-backpressure | 5+ inline, 1 external | none | Optional: add admission control spans |
| ggen-workflow-43 | inline only, 0 external | none | Add integration tests |

### Excluded (from workspace)

| Crate | Reason | Notes |
|-------|--------|-------|
| ggen-workflow | Erlang NIF dependency | Has `otel` feature stub |
| knhk-hot | Standalone | Hot path optimization |
| tps-jidoka | Standalone | TPS jidoka integration |

---

## 4. Implementation Phases

### Phase 1: Critical Infrastructure (Week 1)

**Goal:** Fix CRITICAL gaps + SpanCapture infrastructure + span name alignment

| Step | Action | Chicago TDD | Verification |
|------|--------|-------------|--------------|
| 1.1 | Create `TestSpanExporter` + `SpanCapture` in ggen-testing | RED: test that captures a span in memory | Test passes |
| 1.2 | Fix span name `llm.complete` -> `ggen.llm.generation` in ggen-ai | RED: test asserts correct span name | Test passes |
| 1.3 | Fix span name `a2a.message` -> `ggen.a2a.message` in correlation.rs | RED: test asserts correct span name | Test passes |
| 1.4 | Add `ggen.error` spans to ggen-a2a-mcp error paths | RED: test asserts `ggen.error` exists after error | Test passes |
| 1.5 | Add `ggen.error` spans to ggen-core error paths | RED: test asserts error span on failed pipeline | Test passes |
| 1.6 | Add `a2a.operation_name` to correlation spans | RED: test asserts attribute present | Test passes |
| 1.7 | Run live-check | Verify 6/6 groups, 0 violations, 0 name mismatches | Gate passes |

**SpanCapture design:**
```rust
// In ggen-testing/src/span_capture.rs
pub struct SpanCapture {
    spans: Arc<Mutex<Vec<CapturedSpan>>>,
}

pub struct CapturedSpan {
    pub name: String,
    pub attributes: HashMap<String, Value>,
    pub duration: Duration,
}

impl SpanCapture {
    pub fn assert_span_exists(&self, name: &str) -> &CapturedSpan;
    pub fn assert_attribute(&self, span_name: &str, key: &str, value: &str);
    pub fn get_spans_by_name(&self, name: &str) -> Vec<&CapturedSpan>;
}
```

### Phase 2: HIGH Priority Crates (Week 2-3)

**Goal:** Add OTEL to all HIGH priority crates + zero-test crates

| Step | Action | Verification |
|------|--------|--------------|
| 2.1 | ggen-payments: Chicago TDD suite (ZERO tests currently) | Tests pass, no unwrap |
| 2.2 | ggen-transport: gRPC spans | Live-check shows RPC spans |
| 2.3 | ggen-api: instrument API handlers | HTTP server semconv spans |
| 2.4 | ggen-auth: instrument JWT/OAuth2 flows | Auth spans + error spans |
| 2.5 | ggen-dspy: pipeline spans for optimizer iterations | External integration tests |
| 2.6 | ggen-saas: integration tests | Quota edge cases covered |
| 2.7 | ggen-domain: instrument domain ops | Pipeline spans on domain operations |
| 2.8 | ggen-cli: instrument CLI commands | Pipeline spans on all commands |
| 2.9 | ggen-core: semconv attribute alignment | Registry spans match semconv attributes |

### Phase 3: MEDIUM Priority Crates (Week 3)

**Goal:** Expand OTEL to all MEDIUM priority crates

| Step | Action | Verification |
|------|--------|--------------|
| 3.1 | ggen-yawl: pipeline spans | Code generation emits pipeline spans |
| 3.2 | ggen-tps-andon: expand instrumentation | All signal handlers instrumented |
| 3.3 | ggen-consensus: PBFT spans | Consensus phases traced |
| 3.4 | ggen-jidoka: error spans | Quality failures create ggen.error spans |
| 3.5 | ggen-a2a: state machine spans | A2A transitions create ggen.a2a.message spans |
| 3.6 | ggen-marketplace: external tests | Integration tests under tests/ |

### Phase 4: CI Integration + Live-Check (Week 4)

**Goal:** Live-check runs in CI with 6/6 groups required

| Step | Action | Verification |
|------|--------|--------------|
| 4.1 | Add live-check to `.github/workflows/` | CI runs weaver receiver + tests + gate |
| 4.2 | Live-check requires 6/6 groups in CI mode | `--ci` flag enforces ggen.error |
| 4.3 | Live-check requires GROQ_API_KEY in CI | Secret configured in GitHub Actions |
| 4.4 | Live-check produces evidence artifacts | `live_check.json` uploaded as artifact |
| 4.5 | Gate fails CI on violations | Non-zero exit blocks merge |

### Phase 5: LOW Priority + Polish (Week 5)

**Goal:** Complete remaining crates

| Step | Action | Verification |
|------|--------|--------------|
| 5.1 | ggen-codegen: expand from 3 tests | 15+ tests |
| 5.2 | ggen-workflow-43: add external tests | Integration tests under tests/ |
| 5.3 | Optional: ggen-heijunka, ggen-kaizen spans | Nice-to-have |
| 5.4 | Delete broken test files | Remove multi_mcp_otel_self_play.rs, multi_a2a_otel_chain.rs |
| 5.5 | Full regression run | All tests pass, live-check 6/6 |

---

## 5. Test Patterns

### Pattern A: Span Assertion with SpanCapture (new)

```rust
#[test]
fn test_llm_generation_emits_correct_spans() {
    // ARRANGE
    let capture = SpanCapture::new();
    let client = LlmClient::with_exporter(capture.exporter());

    // ACT
    let result = client.complete("test prompt").unwrap();

    // ASSERT on observable output
    assert!(!result.is_empty());

    // ASSERT on spans (proves real API call happened)
    capture.assert_span_exists("ggen.llm.generation");
    capture.assert_attribute("ggen.llm.generation", "llm.model", "groq::llama-3.3-70b-versatile");
    capture.assert_attribute("ggen.llm.generation", "llm.total_tokens");
    assert!(capture.get_attribute::<u64>("ggen.llm.generation", "llm.total_tokens").unwrap() > 0);
}
```

### Pattern B: Chicago TDD for MCP Tool Chains

```rust
#[test]
fn test_multi_mcp_tool_chain_produces_spans() {
    // ARRANGE
    let capture = SpanCapture::new();
    let server = GgenServer::with_exporter(capture.exporter());

    // ACT - chain multiple MCP tools
    let r1 = server.call_tool("validate_pipeline", "{}").unwrap();
    let r2 = server.call_tool("validate_sparql", "{}").unwrap();
    let r3 = server.call_tool("validate_templates", "{}").unwrap();

    // ASSERT on observable output
    assert!(r1.is_success());
    assert!(r2.is_success());
    assert!(r3.is_success());

    // ASSERT on spans
    let spans = capture.get_spans_by_name("ggen.mcp.tool_call");
    assert_eq!(spans.len(), 3);
    assert_eq!(spans[0].attributes["mcp.tool_name"], "validate_pipeline");
    assert_eq!(spans[1].attributes["mcp.tool_name"], "validate_sparql");
    assert_eq!(spans[2].attributes["mcp.tool_name"], "validate_templates");
}
```

### Pattern C: Chicago TDD for A2A Message Chains

```rust
#[test]
fn test_a2a_message_chain_produces_spans() {
    // ARRANGE
    let capture = SpanCapture::new();
    let client = A2aLlmClient::with_exporter(capture.exporter());

    // ACT - send multiple A2A messages
    let msg1 = A2aMessage::new("task-1", "generate", payload.clone());
    let msg2 = A2aMessage::new("task-2", "validate", payload.clone());
    let r1 = client.process_message(msg1).unwrap();
    let r2 = client.process_message(msg2).unwrap();

    // ASSERT on observable output
    assert!(!r1.is_empty());
    assert!(!r2.is_empty());

    // ASSERT on spans
    let spans = capture.get_spans_by_name("ggen.a2a.message");
    assert!(spans.len() >= 2);
    // Verify required semconv attributes
    for span in &spans {
        assert!(span.attributes.contains_key("a2a.message_id"));
        assert!(span.attributes.contains_key("a2a.operation_name"));
    }
}
```

### Pattern D: Error Span Assertion

```rust
#[test]
fn test_error_path_creates_ggen_error_span() {
    // ARRANGE
    let capture = SpanCapture::new();
    let server = GgenServer::with_exporter(capture.exporter());

    // ACT - trigger an error
    let result = server.call_tool("validate_pipeline", "{invalid_json}");

    // ASSERT on error state
    assert!(result.is_error());

    // ASSERT on error span
    capture.assert_span_exists("ggen.error");
    capture.assert_attribute("ggen.error", "error.type", "json_parse_error");
    assert!(capture.get_attribute::<String>("ggen.error", "error.message").unwrap().len() > 0);
}
```

---

## 6. Live-Check CI Integration

### Current State
- Script exists: `semconv/live-check/run-ggen-live-check.sh`
- Gate exists: `semconv/live-check/ggen_gate.py`
- No CI workflow calls it yet

### Target CI Workflow

```yaml
# .github/workflows/live-check.yml
name: OTEL Live-Check
on:
  push:
    branches: [master]
  pull_request:
    branches: [master]

jobs:
  live-check:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      - run: curl -LsSf https://github.com/open-telemetry/weaver/releases/latest/download/weaver-installer.sh | sh
      - run: pip install pyyaml
      - name: Run live-check
        env:
          GROQ_API_KEY: ${{ secrets.GROQ_API_KEY }}
        run: ./semconv/live-check/run-ggen-live-check.sh --ci
      - uses: actions/upload-artifact@v4
        if: always()
        with:
          name: live-check-report
          path: /tmp/ggen-weaver-live-check/
```

### Gate Requirements

| Mode | Required Groups | Blocking Violations |
|------|----------------|-------------------|
| Local (`./run-ggen-live-check.sh`) | 5/6 (ggen.error optional) | 0 |
| CI (`./run-ggen-live-check.sh --ci`) | 6/6 (all required) | 0 |

---

## 7. Definition of Done

A crate is considered "tested" when:

1. **Chicago TDD compliance** - Test commit preceded or accompanied implementation
2. **All tests pass** - `cargo make test` succeeds
3. **OTEL spans verified** - For instrumented features, spans exist with correct attributes
4. **No unwrap** - Production code uses `Result<T,E>`, not `unwrap()/expect()`
5. **80%+ coverage** - Measured by `cargo-tarpaulin` or equivalent

The full workspace is considered "tested" when:

1. All 3 ZERO-test crates have test suites (ggen-payments, knhk-hot, tps-jidoka)
2. All 6 semconv groups have instrumentation AND test coverage
3. Live-check passes with 6/6 groups in CI mode
4. Zero `unwrap()` violations in production code
5. All existing tests still pass (no regressions)

---

## 8. Metrics to Track

| Metric | Current | Target | Measurement |
|--------|---------|--------|-------------|
| Crates with tests | 78/81 | 81/81 | `cargo test --workspace` |
| Semconv groups covered | 5/6 | 6/6 | weaver live-check |
| Span name mismatches | 3 | 0 | weaver violations |
| Crates with OTEL | 4 | 20+ | grep `tracing::instrument` |
| Live-check entities | 2823 | 5000+ | live_check.json |
| Zero-test crates | 3 | 0 | manual audit |
| `unwrap()` violations | unknown | 0 | clippy lint |

---

## 9. Cross-Cutting Gaps

### Span Name Mismatches (Production Code)

| Crate | File | Current Name | Semconv Expected | Status |
|-------|------|-------------|-----------------|--------|
| ggen-ai | `client.rs` | `llm.complete` | `ggen.llm.generation` | Fix in Phase 1 |
| ggen-a2a-mcp | `correlation.rs` | `a2a.message` | `ggen.a2a.message` | Fix in Phase 1 |
| ggen-core | `registry.rs` | `ggen.market.search` | No semconv definition | Create or remove |

### Missing Semconv Span Models

No semconv model exists for:
- `http.server` / `http.client` (needed by ggen-api, ggen-transport)
- `rpc.server` / `rpc.client` (needed by ggen-transport gRPC)
- `db.query` (if any crate does database operations)

### Feature-Gated Test Hiding

Many integration tests are behind `required-features = ["integration"]` which means `cargo test` alone doesn't run them. This hides test coverage gaps. Consider running integration tests in CI even if they require specific features.

### ggen-metrics-tps Architecture Mismatch

Uses Prometheus crate, not OpenTelemetry, for metrics. Consider `opentelemetry-prometheus` exporter bridge.

---

*Generated: 2026-03-30*
*Version: v2026.3.30*
