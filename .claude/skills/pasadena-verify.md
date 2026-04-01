---
name: pasadena-verify
description: Hermetic verification of feature claims. Runs in isolated context to prevent builder-judge contamination.
user-invocable: false
context: fork
---

# Pasadena Verification

## Purpose
Run hermetic verification on feature claims before they are made. This skill runs in a forked context -- the verifier has no access to the builder's reasoning, only the code and test results.

## When to Invoke
- Before any "done", "complete", "ready" claim
- After implementation phase of any feature
- Before committing code

## Procedure
1. Read the changed files (`git diff --name-only`)
2. Run `cargo make check` -- compiler must pass
3. Run `cargo make test` -- all tests must pass
4. Run `cargo make lint` -- no warnings
5. If LLM features involved: capture OTEL spans with `RUST_LOG=trace,ggen_ai=trace`
6. Classify result: PASS or FAIL with evidence tier

## Output Format
Return structured assessment:
- Feature: [name]
- Evidence tier: PROVEN / OBSERVED / INFERRED / UNVERIFIED
- Compiler: PASS/FAIL
- Tests: PASS/FAIL (count)
- Lint: PASS/FAIL
- OTEL spans: CAPTURED / MISSING / N/A
- Verdict: APPROVED / REJECTED

## Failure Modes to Watch For
- NARRATION: Claude describes what should happen instead of running verification
- SELF-CERT: Claude says "this looks correct" without running gates
- LAZY JUDGE: Claude evaluates its own output instead of using mechanical gates
