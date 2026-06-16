<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen Definition of Done — 5 Core Quality Gates](#ggen-definition-of-done--5-core-quality-gates)
  - [Quick Reference](#quick-reference)
    - [Combined Validation](#combined-validation)
  - [Gate 1: timeout-check (Prerequisite)](#gate-1-timeout-check-prerequisite)
    - [Success Criteria](#success-criteria)
    - [If It Fails](#if-it-fails)
    - [Andon Signal](#andon-signal)
  - [Gate 2: check (Compilation)](#gate-2-check-compilation)
    - [What It Does](#what-it-does)
    - [Success Criteria](#success-criteria-1)
    - [If It Fails](#if-it-fails-1)
    - [Common Causes](#common-causes)
    - [Andon Signal](#andon-signal-1)
  - [Gate 3: lint (Code Quality)](#gate-3-lint-code-quality)
    - [What It Does](#what-it-does-1)
    - [Success Criteria](#success-criteria-2)
    - [If It Fails](#if-it-fails-2)
    - [Forbidden Responses](#forbidden-responses)
    - [Common Causes](#common-causes-1)
    - [Andon Signal](#andon-signal-2)
  - [Gate 4: test (Functional Verification)](#gate-4-test-functional-verification)
    - [What It Does](#what-it-does-2)
    - [Success Criteria](#success-criteria-3)
    - [If It Fails](#if-it-fails-3)
    - [Chicago TDD Requirement (NO MOCKS)](#chicago-tdd-requirement-no-mocks)
    - [Common Causes](#common-causes-2)
    - [Andon Signal](#andon-signal-3)
  - [Gate 5: slo-check (Performance Validation)](#gate-5-slo-check-performance-validation)
    - [What It Does](#what-it-does-3)
    - [SLO Targets (HARD LIMITS)](#slo-targets-hard-limits)
    - [Success Criteria](#success-criteria-4)
    - [If It Fails](#if-it-fails-4)
    - [Common Causes](#common-causes-3)
    - [Andon Signal](#andon-signal-4)
  - [Pre-Commit Gate (4 of 5)](#pre-commit-gate-4-of-5)
    - [Gates Included](#gates-included)
    - [Use Cases](#use-cases)
    - [Success Criteria](#success-criteria-5)
  - [Complete Definition of Done](#complete-definition-of-done)
    - [ALL FIVE GATES REQUIRED](#all-five-gates-required)
    - [Success Condition](#success-condition)
    - [Failure Condition](#failure-condition)
  - [Andon Protocol (Stop the Line)](#andon-protocol-stop-the-line)
    - [Critical Stops](#critical-stops)
    - [High Priority Stops](#high-priority-stops)
    - [Forbidden Behaviors](#forbidden-behaviors)
  - [Evidence & Proof](#evidence--proof)
    - [No Fabrication Rule](#no-fabrication-rule)
    - [OTEL Validation (LLM/External Services)](#otel-validation-llmexternal-services)
  - [Workspace Context](#workspace-context)
  - [Troubleshooting Matrix](#troubleshooting-matrix)
  - [Definition of Done Checklist](#definition-of-done-checklist)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen Definition of Done — 5 Core Quality Gates

**Version:** 26.5.28 | **Last Updated:** 2026-06-14

> **GOLDEN RULE:** All five gates must pass. A partial pass is a failure. This is non-negotiable.

---

## Quick Reference

| Gate | Command | Timeout | Typical Duration | Purpose |
|------|---------|---------|------------------|---------|
| **timeout-check** | `just timeout-check` | 5s | 1s | Verify `timeout` command exists (prerequisite) |
| **check** | `just check` | 60s | 5-15s | Compilation check (no errors) |
| **lint** | `just lint` | 90s | 45-90s | Code quality + formatting (zero warnings) |
| **test** | `just test` | 120s | 25-120s | Full test suite (all tests pass) |
| **slo-check** | `just slo-check` | 120s | 60s | Performance SLOs met |

### Combined Validation

```bash
# Fast pre-commit validation (4 of 5 gates)
just pre-commit        # fmt-check + check + lint + test-lib

# Full Definition of Done (all 5 gates)
just timeout-check && just check && just lint && just test && just slo-check
```

---

## Gate 1: timeout-check (Prerequisite)

**Command:** `just timeout-check`  
**Timeout:** 5 seconds  
**Purpose:** Verify the system has the `timeout` command (required for all timed gates)

### Success Criteria
```
Exit code: 0
Output: ✅ timeout command verified
```

### If It Fails
```bash
# Check if timeout exists
command -v timeout

# Install:
# macOS:   brew install coreutils
# Linux:   sudo apt install coreutils
# WSL:     Ensure WSL tools in PATH
```

### Andon Signal
- **Level:** HIGH
- **Message:** STOP. Cannot proceed without timeout command.
- **Blocks:** All other timed gates (check, test, lint, slo-check)

---

## Gate 2: check (Compilation)

**Command:** `just check`  
**Timeout:** 60 seconds  
**Typical Duration (hot cache):** 5-15 seconds  
**Purpose:** Verify all 15 workspace crates compile without errors

### What It Does
```
timeout 60s cargo check --workspace
```

### Success Criteria
```
Exit code: 0
Output: Finished [debug] profile [unoptimized + debuginfo] target(s) in X.XXs
No "error[E...]" messages
```

### If It Fails

**Root Cause Diagnosis:**
1. Read the compiler error output (error[E...] with line numbers)
2. Open the source file at the reported location
3. Understand the error (type mismatch? lifetime? missing impl?)
4. Do NOT suppress with `#[allow(...)]`

**Recovery:**
```bash
# Read the full error
just check 2>&1 | head -50

# Open the file at the error line
$EDITOR crates/ggen-core/src/lib.rs:123

# Apply the fix
# Re-run
just check
```

### Common Causes
- Type mismatch: function expects `T`, received `U`
- Missing trait impl: type doesn't implement required trait
- Lifetime mismatch: borrowed value doesn't live long enough
- Missing dependency in `Cargo.toml`
- Workspace-wide cascade: change in one crate breaks dependents

### Andon Signal
- **Level:** CRITICAL
- **Message:** STOP THE LINE. Compiler errors block all work.
- **Blocks:** All subsequent gates (lint, test, slo-check)
- **Action:** Fix the error. Do not proceed. Do not commit.

---

## Gate 3: lint (Code Quality)

**Command:** `just lint`  
**Timeout:** 90 seconds  
**Typical Duration (hot cache):** 45-90 seconds  
**Purpose:** Verify code formatting and catch real bugs via Clippy

### What It Does
```
# Step 1: Code formatting check
cargo fmt --all -- --check

# Step 2: Clippy analysis with -D warnings
timeout 90s cargo clippy --all-targets -- -D warnings
```

### Success Criteria
```
Exit code: 0
Formatting: All files match rustfmt rules
Clippy: Zero warnings (treated as errors with -D)
Output: "Finished check [unoptimized + debuginfo]"
```

### If It Fails

**Formatting Issue:**
```bash
# Auto-fix formatting
cargo fmt --all

# Verify changes
git diff

# Re-run
just lint
```

**Clippy Warning:**
1. Read the warning message carefully
2. Understand the suggestion (often includes code example)
3. Apply the fix (refactor code, don't suppress)
4. Re-run

**Example Recovery:**
```bash
# Clippy says: "use of `map(f).unwrap_or_else(g)` can be simplified"
# Current code:
let x = values.iter().map(|v| v.as_str()).unwrap_or_else(|| "default");

# Fix: Use and_then or try more idiomatic approach
let x = values.iter().next().map(|v| v.as_str()).unwrap_or("default");

# Re-run
just lint
```

### Forbidden Responses
- ❌ Do NOT add `#[allow(clippy::...)]` to suppress
- ❌ Do NOT leave `#[allow(...)]` from previous debugging
- ❌ Do NOT commit code with warnings
- ❌ Do NOT use `//! allow-warnings` in module headers

### Common Causes
- Unused imports or variables: Clippy detects dead code
- Unnecessary clone: remove `.clone()` calls
- Type conversion inefficiency: use `.into()` instead
- Formatting mismatch: run `cargo fmt --all`
- Inefficient patterns: Clippy suggests more idiomatic Rust

### Andon Signal
- **Level:** HIGH
- **Message:** STOP before release. Warnings unacceptable in production.
- **Blocks:** Release/merge (not blocking to commit locally)
- **Action:** Fix all warnings before shipping.

---

## Gate 4: test (Functional Verification)

**Command:** `just test`  
**Timeout:** 120 seconds  
**Typical Duration (hot cache):** 25-30 seconds  
**Typical Duration (cold cache, first build):** 120 seconds  
**Purpose:** Run full test suite (347+ tests) to verify correctness

### What It Does
```
# Primary gate (with escalation logic):
timeout 30s cargo test --workspace --tests
# If timeout, escalates to:
timeout 120s cargo test --workspace --tests
```

### Success Criteria
```
Exit code: 0
Output: "test result: ok. NNN passed; 0 failed"
Test count: 347+ tests across workspace
No FAILED, panicked, or timeout messages
```

### If It Fails

**Root Cause Diagnosis:**
1. Read the FAILED test output carefully
2. Note the test name and assertion
3. Understand: is this a test bug or a code bug?
4. Read the assertion message and actual vs expected values
5. Trace backwards to the code path

**Recovery:**
```bash
# Run the specific failing test with output
cargo test <test_name> -- --nocapture --exact

# Read the full output
# Find the code being tested
# Apply the fix to the code (NOT the test)
# Re-run
just test
```

### Chicago TDD Requirement (NO MOCKS)
Tests must verify **real behavior** against **real collaborators**, NOT mocks:

```rust
// ❌ FORBIDDEN: Mock HTTP client
use mockall::mock!;
mock! { HttpClient { ... } }

// ✅ REQUIRED: Real HTTP client
let client = reqwest::Client::new();
let response = client.get("https://example.com").await?;
assert_eq!(response.status(), 200);
```

### Common Causes
- Off-by-one error in loops or slicing
- State not initialized in test setup
- Async function not awaited
- Error handling path not implemented
- Thread safety issue under concurrent load
- Floating-point precision issue

### Andon Signal
- **Level:** CRITICAL
- **Message:** STOP THE LINE. Failing tests = code is broken.
- **Blocks:** All subsequent gates (slo-check)
- **Action:** Fix the code. Do not proceed. Do not commit.

---

## Gate 5: slo-check (Performance Validation)

**Command:** `just slo-check`  
**Timeout:** 120 seconds  
**Typical Duration:** 60 seconds  
**Purpose:** Verify build times, runtime, and memory stay within SLO targets

### What It Does
```
cargo bench --bench cli_startup_performance -- --test
```

### SLO Targets (HARD LIMITS)

| Metric | Target | Verification |
|--------|--------|-------------|
| First build | ≤15 seconds | Initial `cargo build` |
| Incremental build | ≤2 seconds | After minor edit |
| Test suite | ≤30 seconds | `just test` with cache |
| RDF processing | ≤5 seconds per 1k triples | `ggen sync` |
| Generation memory | ≤100 MB | Runtime profiling |
| CLI scaffolding | ≤3 seconds end-to-end | Integration test |
| Reproducibility | 100% | Hash verification across runs |

### Success Criteria
```
Exit code: 0
Output: "test result: ok" or benchmark metrics within SLO thresholds
All metrics: ≤ target threshold
```

### If It Fails

**Root Cause Diagnosis:**
1. Identify which SLO was exceeded
2. Measure current performance: `cargo bench --bench cli_startup_performance`
3. Compare to SLO threshold
4. Identify what changed (new deps? expensive algorithm? allocation?)
5. Profile the bottleneck (flamegraph, perf)

**Recovery:**
```bash
# Benchmark current state
cargo bench --bench cli_startup_performance

# Locate hot code path (where time is spent)
# Optimize: reduce allocations, cache results, parallelize
cargo bench --bench cli_startup_performance   # measure after

# Re-run gate
just slo-check
```

### Common Causes
- New dependency added (invalidates Cargo.lock)
- Algorithm complexity: O(n²) instead of O(n)
- Memory allocation in tight loop
- Unnecessary cloning
- Blocking I/O in async context
- Redundant recompilation

### Andon Signal
- **Level:** HIGH
- **Message:** STOP before release. Performance regressions block deployment.
- **Blocks:** Release/merge
- **Action:** Fix bottleneck. Do not ship with SLO violations.

---

## Pre-Commit Gate (4 of 5)

**Command:** `just pre-commit`  
**Timeout:** 120 seconds  
**Typical Duration:** 90 seconds  
**Sequence:** fmt-check → check → lint → test-lib (fails fast at first failure)

### Gates Included
1. **fmt-check** (3s) — Code formatting validation
2. **check** (60s) — Compilation check
3. **lint** (90s) — Clippy + rustfmt
4. **test-lib** (30s) — Unit tests only (faster than full `just test`)

### Use Cases
- Local validation before committing
- Pre-commit hook (prevents commit on failure)
- CI gating (prevents merge on failure)
- Developer feedback loop (fast iteration)

### Success Criteria
```
Exit code: 0
All four sub-gates pass
```

---

## Complete Definition of Done

### ALL FIVE GATES REQUIRED

```bash
# Run all five gates in sequence (total ~180s typical)
just timeout-check && just check && just lint && just test && just slo-check
```

### Success Condition
```
timeout-check: ✅ Exit 0
check:         ✅ Exit 0
lint:          ✅ Exit 0
test:          ✅ Exit 0
slo-check:     ✅ Exit 0
```

### Failure Condition
Any gate returns non-zero exit code = **NOT DONE**.

---

## Andon Protocol (Stop the Line)

When a gate fails, you **STOP** immediately.

### Critical Stops
**Compiler errors** and **test failures** = HALT all work.
```
Actions:
1. Read the error/failure output carefully
2. Identify root cause (not symptom)
3. Fix the code
4. Re-run the gate
5. Repeat until pass
6. Do NOT proceed to next gate
7. Do NOT commit
```

### High Priority Stops
**Clippy warnings** and **SLO violations** = STOP before release.
```
Actions:
1. Fix the violation
2. Re-run the gate
3. Repeat until pass
4. Do NOT merge to main
```

### Forbidden Behaviors
- ❌ Silencing with `#[allow(...)]`
- ❌ Ignoring tests with `#[ignore]`
- ❌ Using `|| true` to hide failures
- ❌ Leaving `todo!()` in production
- ❌ Suppressing warnings without understanding
- ❌ Committing with known failures

---

## Evidence & Proof

### No Fabrication Rule
Never claim completion without evidence:

- ✅ **check pass:** Exit code 0 from `cargo check --workspace`
- ✅ **test pass:** Exit code 0 + test count from `cargo test --workspace --tests`
- ✅ **lint pass:** Exit code 0 + zero clippy warnings
- ✅ **slo-check pass:** Benchmark output showing all metrics ≤ thresholds

### OTEL Validation (LLM/External Services)
For any feature involving LLM calls, external APIs, or pipeline stages, verify OTEL spans:

```bash
# Enable trace logging
export RUST_LOG=trace,ggen_ai=trace

# Run test and capture output
cargo test -p ggen-cli-lib --test llm_e2e_test -- --nocapture 2>&1 | tee otel.txt

# Verify spans exist
grep -E "llm\.complete|llm\.model|llm\.total_tokens" otel.txt
```

**Required spans for LLM features:**
- `llm.complete` or `llm.complete_stream`
- Attributes: `llm.model`, `llm.prompt_tokens`, `llm.completion_tokens`, `llm.total_tokens`

**Rule:** No OTEL spans = feature not working, even if tests pass.

---

## Workspace Context

| Aspect | Value |
|--------|-------|
| Crates | 15 active (ggen-*, genesis-*, cpmp, stpnt) |
| Testing | Chicago TDD ONLY (no mocks, real collaborators) |
| Coverage | 80%+ of public APIs |
| Mutation score | ≥60% |
| Edition | Rust 2021 |
| Version | 26.5.28 |

---

## Troubleshooting Matrix

| Symptom | Root Cause | Fix |
|---------|-----------|-----|
| `timeout-check fails` | `timeout` command missing | `brew install coreutils` (macOS) or `apt install coreutils` (Linux) |
| `check fails` | Compiler error | Read error, fix code, re-run `just check` |
| `lint fails` | Formatting or clippy warning | Run `cargo fmt --all`, then apply clippy suggestion, re-run `just lint` |
| `test fails` | Test assertion failure | Read failing test, fix code (not test), re-run `just test` |
| `slo-check fails` | Performance exceeds target | Profile bottleneck, optimize, re-run `just slo-check` |
| `first run >120s` | Cold cache (deps compile) | Wait for build, subsequent runs ~25s |
| Multiple gates fail | Cascading errors from early gate | Fix earliest gate first, re-run in sequence |

---

## Definition of Done Checklist

Before claiming completion:

- [ ] `just timeout-check` passes (exit 0)
- [ ] `just check` passes (exit 0, no compiler errors)
- [ ] `just lint` passes (exit 0, zero warnings)
- [ ] `just test` passes (exit 0, all 347+ tests pass)
- [ ] `just slo-check` passes (exit 0, all SLOs met)
- [ ] For LLM features: OTEL spans verified (captured real traces)
- [ ] No `#[allow(...)]` suppressing real issues
- [ ] No `#[ignore]` on tests
- [ ] No `todo!()` in production code
- [ ] Ready to commit/merge

---

## References

- **JSON spec:** `/docs/DEFINITION_OF_DONE.json` (machine-readable, all details)
- **Commands:** `justfile` in project root
- **Andon protocol:** `.claude/rules/andon/signals.md`
- **Chicago TDD:** `.claude/rules/rust/testing.md`
- **Performance SLOs:** `.claude/rules/rust/performance.md`
- **OTEL validation:** `.claude/rules/otel-validation.md`

---

**Last Updated:** 2026-06-14 | **Project:** ggen v26.5.28 | **Repository:** https://github.com/seanchatmangpt/ggen
