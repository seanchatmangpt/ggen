---
name: proof-stack
description: Execute the full three-layer proof: Execution + Semantics + Evaluation
user-invocable: true
---

# Proof Stack

## Purpose
Run the complete three-layer verification required before any feature merge.

## Procedure

### Layer 1: Execution Proof
Run the feature with OTEL tracing enabled:
```bash
RUST_LOG=trace,ggen_ai=trace,ggen_core=trace cargo make test 2>&1 | tee /tmp/proof-stack-otel.txt
```
Verify spans exist: grep for `llm.complete`, `pipeline.*`, `mcp.tool.*`

### Layer 2: Semantic Proof
Verify test assertions check domain invariants, not type wrappers:
- Every `assert!` tests business logic
- No `assert!(result.is_ok())` as the sole assertion
- Test name describes the claim being verified

### Layer 3: Evaluation Proof
Run schema conformance if applicable:
```bash
weaver registry check -r ./semconv/model -p ./semconv/policies/ --quiet
```

## Output
Write receipt to memory:
- Timestamp
- Feature name
- All three layers: PASS/FAIL
- Evidence tier: PROVEN only if all three pass

## Failure Modes
- Skipping any layer = UNVERIFIED
- Self-certifying without running = SELF-CERT violation
- Weakening a test to make a layer pass = TEST MURDER violation
