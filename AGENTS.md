# AGENTS.md: Verification Constitution

**Non-negotiable laws for agent-generated code in this repository.**

All claims of correctness must be **provable, not asserted**.

---

## Forbidden

The following constructs are **defects** and must not appear in the codebase:

### Testing & Verification
- **Unit tests** without real boundary crossing
- **Mocks** of primary evidence paths (tracer, meter, OTel client, MCP server) via `mockall` or manual stubs
- **Stubs** that simulate behavior without execution
- **Fixtures** that fabricate traces, spans, OCEL events, or receipts
- **Synthetic telemetry** builders (fake span structs, fabricated events)
- **Test-only fake clients** for LLM, MCP, observability, or state

### Code Quality
- **TODO, FIXME** placeholders (use real implementation or block work)
- **Monkeypatching** of code paths (including `mockall::mock!`, `#[automock]`, or manual trait implementation for testing only)
- **Hardcoded returns** that simulate success
- **Documentation-only contracts** (code must enforce, not docs)

### Evidence Falsification
- **Fake receipt builders** (receipts must be cryptographically derived)
- **Fabricated BLAKE3 hashes** (use real computation)
- **Synthetic MCP/LLM boundaries** (must be real calls or abstain)
- **Empty or placeholder proofs** (proof gates must validate real evidence)

### Configuration & Policy
- **Weakening of hooks** or enforcement settings
- **Bypass clauses** in policy (policy must be hard)
- **Shadow configuration files** that contradict main policy
- **Disablement of structural checks** without revision control

---

## Required

All code must satisfy:

### Execution & Boundaries
- **Real boundary crossing** — code must invoke real LLM, MCP, OTel, subprocess, or state transition
- **No shortcuts** — if a boundary exists, it must be crossed in tests
- **Evidence at crossing** — each boundary crossing must produce externalizable evidence

### Observability & Evidence
- **Real OTel traces** from actual execution (via Tempo/Jaeger)
- **Real OCEL derivation** from traces (never hand-constructed)
- **Real BLAKE3 receipts** from cryptographic computation
- **Externalizable evidence** in files, OTel, DB, logs, or process

### Verification & Proof
- **Causality chains** — tests must prove X caused Y (not just assert Y exists)
- **Multi-surface corroboration** — claims must appear in ≥3 of: execution, telemetry, state, process, causality
- **Falsifiability** — tests must fail if core behavior is broken or faked
- **Anti-cheating resilience** — faking the test must require more work than real execution

---

## Enforcement

These laws are enforced by **Claude Code hooks** backed by **Rust structural analysis**:

- `PreToolUse` on `Edit|Write` — blocks introduction of forbidden constructs
- `PostToolUse` on `Edit|Write` — re-scans written content for violations
- `ConfigChange` — blocks weakening of hooks or policy settings
- `FileChanged` on policy/test/config paths — audits and enforces consistency

Violations are **denied**, not warned. There are no bypasses.

---

## Rationale

### Why No Mocks?

Mocking the observability layer (tracer, meter, OTel client) creates fake evidence. Fake evidence proves nothing. A test that passes with a mocked tracer proves only that the mock was called, not that the system works.

### Why No Unit Tests Without Boundaries?

A unit test that asserts internal state without crossing a boundary tests nothing real. It tests the test. Real boundaries (LLM, MCP, OTel, state) force the code to do actual work.

### Why No TODOs?

TODO is a **loophole**. It says "I'm not done, but the code is in the repo." This lets agents defer problems instead of solving them. If work is incomplete, block it.

### Why Rust Enforcement?

Structural checks (AST, symbol analysis, type inspection) are hard to game. Prompt or agent hooks reintroduce human judgment and can be reasoned around. Rust checks are compiled rules that don't negotiate.

### Why Multi-Surface Corroboration?

A single surface (e.g., "OTel span exists") can be faked. Multiple surfaces (execution + observability + state + process + causality) require real work. This is Van der Aalst process mining thinking applied to tests.

---

## Examples

### ❌ Forbidden: Mock Testing (London TDD)

```rust
#[test]
fn test_operator_forbidden() {
    let mut mock = MockTracer::new();
    mock.expect_export().times(1).returning(|_| Ok(()));
    
    let result = operator.execute(artifact, &mock);
    assert_eq!(result.state, ArtifactState::Bred);
}
```

**Why:** This test passes whether or not tracing actually happened in a real environment. The mock accepts any call. This is fake evidence.

### ✅ Required: Real-Boundary Testing (Chicago TDD)

```rust
#[tokio::test]
async fn test_operator_real() {
    let mut pipeline = Pipeline::new(config);
    let receipt = pipeline.run().await.unwrap();
    
    // 1. Evidence: BuildReceipt exists and is cryptographically valid ✓
    assert!(receipt.verify().is_ok());
    
    // 2. Proof Gates: Interrogate the receipt against 8 canonical gates ✓
    let validator = ProofGateValidator::new();
    let reports = validator.validate(&receipt);
    
    // 3. Multi-surface corroboration ✓
    // Gate 7: Observability Present
    assert!(reports.iter().any(|r| r.gate_type == ProofGateType::ObservabilityPresent && r.pass));
    
    // Gate 8: Causal Consistency (Receipt chain)
    assert!(reports.iter().any(|r| r.gate_type == ProofGateType::CausalConsistent && r.pass));
    
    // 4. Execution boundary crossed (Physical output verification) ✓
    assert!(Path::new("generated/output.rs").exists());
}
```

This test requires real execution across 4 surfaces. Faking any one fails the entire test.

---

## For AI Agents

When you write code in this repository:

### Do
- Write code that works on the first try (no TODOs)
- Use real boundaries (real LLM, MCP, OTel, state)
- Produce externalizable evidence (OTel, files, receipts)
- Test causality, not assertions
- Make faking harder than real execution

### Don't
- Add mocks, stubs, monkeypatching
- Create synthetic telemetry or fixtures
- Use TODO as a placeholder for incomplete work
- Test internal state without evidence
- Assume causality without proving it
- Game policy names or labels

If a violation is introduced, the hook will block it. This is not a guideline. It is a law.

---

## Policy Enforcement

This constitution is enforced by:

1. **Rust policy checker** — inspects for forbidden constructs
2. **Claude Code hooks** — blocks violations before they land
3. **Structural analysis** — uses AST, symbols, and semantic rules, not regex

Enforcement is **structural, not interpretive**. There are no gray areas.
