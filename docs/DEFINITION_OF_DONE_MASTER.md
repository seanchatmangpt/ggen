<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen Definition of Done (Master Specification)](#ggen-definition-of-done-master-specification)
  - [Overview: 10 Dimensions of Completeness](#overview-10-dimensions-of-completeness)
  - [1️⃣ Core Quality Gates (5 Mandatory Gates)](#-core-quality-gates-5-mandatory-gates)
    - [Success Criteria](#success-criteria)
  - [2️⃣ Testing Completeness (Chicago TDD + 80%+ Coverage)](#-testing-completeness-chicago-tdd--80-coverage)
    - [8 Testing Dimensions](#8-testing-dimensions)
    - [Commands](#commands)
    - [Forbidden Patterns](#forbidden-patterns)
    - [Required Patterns](#required-patterns)
  - [3️⃣ OTEL/Observability Validation (For LLM + External Services)](#-otelobservability-validation-for-llm--external-services)
    - [8 Observability Dimensions](#8-observability-dimensions)
    - [Verification Commands](#verification-commands)
    - [Success Criteria](#success-criteria-1)
  - [4️⃣ Performance SLOs (Build, Test, Runtime, Memory)](#-performance-slos-build-test-runtime-memory)
    - [8 Performance Dimensions](#8-performance-dimensions)
    - [Commands](#commands-1)
  - [5️⃣ Validation Gates (RDF, SPARQL, Profiles, Determinism)](#-validation-gates-rdf-sparql-profiles-determinism)
    - [7 Validation Dimensions](#7-validation-dimensions)
    - [Commands](#commands-2)
  - [6️⃣ Documentation (8 Surfaces)](#-documentation-8-surfaces)
    - [8 Documentation Surfaces](#8-documentation-surfaces)
    - [Commands](#commands-3)
  - [7️⃣ Process Automation (10 Dimensions)](#-process-automation-10-dimensions)
    - [10 Automation Dimensions](#10-automation-dimensions)
    - [Key Files](#key-files)
  - [8️⃣ Security & Signing (8 Dimensions)](#-security--signing-8-dimensions)
    - [8 Security Dimensions](#8-security-dimensions)
    - [Commands](#commands-4)
    - [Sabotage Tests (Must Fail Loudly)](#sabotage-tests-must-fail-loudly)
  - [9️⃣ Developer Experience (7 Dimensions)](#-developer-experience-7-dimensions)
    - [7 DX Dimensions](#7-dx-dimensions)
    - [Commands](#commands-5)
  - [🔟 Release Readiness (10 Dimensions)](#-release-readiness-10-dimensions)
    - [10 Release Dimensions](#10-release-dimensions)
    - [Pre-Release Checklist](#pre-release-checklist)
  - [Master Definition of Done Validation](#master-definition-of-done-validation)
  - [The Golden Rule](#the-golden-rule)
  - [Files Reference](#files-reference)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen Definition of Done (Master Specification)

**Version:** 26.5.29 | **Date:** 2026-06-15 | **Status:** Production-Ready

> **CARDINAL RULE:** All dimensions must be satisfied before any work is considered complete. Partial completion is failure.

---

## Overview: 10 Dimensions of Completeness

Every task in ggen must satisfy ALL 10 Definition of Done dimensions. These are organized by phase: validation, implementation, observability, and release.

| # | Dimension | Priority | Quick Command | Status |
|---|-----------|----------|--------------|--------|
| 1️⃣ | **Core Quality Gates** | CRITICAL | `just pre-commit` | ✅ 5 gates |
| 2️⃣ | **Testing Completeness** | CRITICAL | `cargo make test-mutation` | ✅ 8 dimensions |
| 3️⃣ | **OTEL/Observability** | CRITICAL (LLM/external) | `RUST_LOG=trace cargo test` | ✅ 8 dimensions |
| 4️⃣ | **Performance SLOs** | HIGH | `just slo-check` | ✅ 8 dimensions |
| 5️⃣ | **Validation Gates** | HIGH | `ggen validate *.ttl` | ✅ 7 dimensions |
| 6️⃣ | **Documentation** | HIGH | `cargo doc && grep -r TODO docs/` | ✅ 8 surfaces |
| 7️⃣ | **Process Automation** | HIGH | `git push` (hooks run) | ✅ 10 dimensions |
| 8️⃣ | **Security & Signing** | HIGH | `ggen receipt verify` | ✅ 8 dimensions |
| 9️⃣ | **Developer Experience** | MEDIUM | `just --list` | ✅ 7 dimensions |
| 🔟 | **Release Readiness** | CRITICAL | `./RELEASE_QUICK_CHECK.sh` | ✅ 10 dimensions |

---

## 1️⃣ Core Quality Gates (5 Mandatory Gates)

**Files:** `docs/DEFINITION_OF_DONE.json`, `docs/DEFINITION_OF_DONE.md`

All five must pass, zero exceptions. Andon signal = STOP THE LINE.

```bash
# Fast pre-commit (4 gates)
just pre-commit

# Full Definition of Done (5 gates)
just timeout-check && just check && just lint && just test && just slo-check
```

| Gate | Timeout | Duration | Andon | Recovery |
|------|---------|----------|-------|----------|
| `timeout-check` | 5s | 1s | HIGH | Install coreutils |
| `check` | 60s | 5-15s | **CRITICAL** | Fix compiler errors |
| `lint` | 90s | 45-90s | HIGH | Run `cargo fmt` + fix clippy |
| `test` | 120s | 25-120s | **CRITICAL** | Fix failing tests (Chicago TDD) |
| `slo-check` | 120s | 60s | HIGH | Profile and optimize |

### Success Criteria
- ✅ Exit code 0 for all 5 gates
- ✅ No compiler errors (check passes)
- ✅ All 347+ tests pass (Chicago TDD: no mocks)
- ✅ Zero clippy warnings
- ✅ SLOs met: build <15s, test <30s, memory <100MB

---

## 2️⃣ Testing Completeness (Chicago TDD + 80%+ Coverage)

**Files:** `docs/TESTING_DEFINITION_OF_DONE.json`, `.claude/rules/rust/testing.md`

Mandatory: **Zero mocks, zero test doubles, real collaborators only.**

### 8 Testing Dimensions

1. **Coverage Gates** — Line ≥80%, branch ≥75%, mutation ≥60%
2. **Chicago TDD Enforcement** — Zero mockall, zero Mock* structs, zero behavior verification
3. **Test Categorization** — Unit, integration, property, snapshot, security, determinism, perf, sabotage
4. **Error Path Testing** — All Result variants covered, cleanup verified, no panics
5. **Concurrency Testing** — RNG_SEED=42 determinism, TSAN, deadlock detection
6. **Sabotage Tests** — Negative paths prove system fails loudly (not silently)
7. **AAA Pattern** — Every test: Arrange (real) → Act (call) → Assert (state)
8. **Cross-Crate Testing** — Integration across all 15 workspace members validated

### Commands

```bash
# Coverage validation
cargo tarpaulin --workspace --out Html

# Mutation testing
cargo mutants --workspace --baseline 60

# Full test suite
just test

# Fast unit tests only
just test-unit
```

### Forbidden Patterns
❌ `mockall::mock!`  
❌ `#[automock]` traits  
❌ Behavior verification (`.expect_x().times(1)`)  
❌ Test doubles simulating real behavior  
❌ Dependency injection for testability  

### Required Patterns
✅ Real `reqwest::Client` with actual HTTP calls  
✅ Real `SqlitePool(:memory:)` with actual DB ops  
✅ Real `TempDir` with actual filesystem I/O  
✅ Real `GenAiClient` with actual LLM calls  
✅ OTEL spans verified for external services  

---

## 3️⃣ OTEL/Observability Validation (For LLM + External Services)

**Files:** `docs/observability/OTEL_DEFINITION_OF_DONE.json`, `.claude/rules/otel-validation.md`

**Doctrine:** Tests passing ≠ Feature working. OTEL spans prove real execution.

### 8 Observability Dimensions

1. **Required Spans** — LLM (llm.complete, llm.complete_stream), MCP (mcp.tool.*), Pipeline (pipeline.*), Gates (quality_gate.*)
2. **Span Attributes** — Non-zero tokens, real timestamps, network latency ≥100ms
3. **Span Validation** — Real vs. synthetic detection (prompt + completion = total, latency > 100ms)
4. **Multi-Surface Corroboration** — Proof in ≥3 of 5: execution + observability + state + process + causality
5. **Trace Extraction** — Real sources (Tempo, Jaeger) NOT mocked spans
6. **OCEL Derivation** — Event logs from real traces, validated lifecycle, no orphans
7. **Process Mining** — pm4py discovery + conformance checking (fitness ≥0.9)
8. **Negative Proof** — No spans = feature didn't run (even if test passed)

### Verification Commands

```bash
# Enable trace logging
export RUST_LOG=trace,ggen_ai=trace,ggen_core=trace,genai=trace

# Run and capture OTEL output
cargo test -p ggen-cli-lib --test llm_e2e_test -- --nocapture 2>&1 | tee otel_output.txt

# Verify required spans exist
grep -E "llm\.complete|llm\.complete_stream" otel_output.txt
grep -E "llm\.model.*groq|llm\.model.*gpt" otel_output.txt
grep -E "llm\.(prompt_tokens|completion_tokens|total_tokens)" otel_output.txt

# If no spans: feature did not run
```

### Success Criteria
✅ Required spans exist for the operation  
✅ All required attributes populated with real values  
✅ Token counts non-zero and sum correctly  
✅ Network latency ≥100ms (not mock response)  
✅ Error spans if operation failed  

---

## 4️⃣ Performance SLOs (Build, Test, Runtime, Memory)

**Files:** `docs/PERFORMANCE_DEFINITION_OF_DONE.json`, `.claude/rules/rust/performance.md`

All SLOs must be met. No regressions allowed.

### 8 Performance Dimensions

1. **Build Time** — First ≤15s, incremental ≤2s, check ≤5s
2. **Test Execution** — Unit ≤10s, full ≤30s, mutation ≤5min
3. **Runtime Performance** — RDF ≤5s/1k triples, pipeline ≤3s, CLI ≤3s
4. **Memory** — Generation ≤100MB, CLI ≤50MB, zero unbounded allocations
5. **Benchmarks** — ≥80% runtime covered, 6+ Criterion files
6. **Profiling** — Flamegraph + perf required for optimization claims
7. **Regression Detection** — >5% slower blocks merge
8. **Resource Limits** — API calls ≤100/sync, timeouts ≤30s, connections ≤32

### Commands

```bash
just slo-check                                    # Validate all SLOs
cargo bench                                       # Run benchmarks
cargo flamegraph --release --bin ggen -- sync    # Profile hot paths
/usr/bin/time -v ggen sync                        # Memory check
```

---

## 5️⃣ Validation Gates (RDF, SPARQL, Profiles, Determinism)

**Files:** `docs/validation/VALIDATION_GATES_DEFINITION_OF_DONE.json`

All validation layers must pass before artifacts are emitted.

### 7 Validation Dimensions

1. **SHACL Validation** — `ggen validate *.ttl` passes with no errors
2. **Profile Enforcement** — Strict mode constraints block artifact emission
3. **SPARQL Conformance** — Provision checks (GGEN-OUT-001, GGEN-TPL-001, etc.)
4. **Type-Level** — Rust compiler (no unsafe, no unwrap)
5. **Diagnostic Codes** — All GGEN-* codes resolved before sync
6. **Output Layer** — Files don't escape project root (GGEN-YIELD-001)
7. **Determinism** — Reproducible builds (hash verification)

### Commands

```bash
ggen validate .specify/specs/*/feature.ttl                # SHACL validation
ggen sync --dry_run true                                  # Preview validation
ggen receipt verify .ggen/receipts/latest.json            # Determinism check
```

---

## 6️⃣ Documentation (8 Surfaces)

**Files:** `docs/DOCUMENTATION_DEFINITION_OF_DONE.json`, `docs/DOCUMENTATION_DEFINITION_OF_DONE.md`

All docs must reference **real code, real OTEL output, actual file paths.**

### 8 Documentation Surfaces

1. **Architecture** — COMPRESSED_REFERENCE.md, crate map, cross-cutting patterns (LSP-verified)
2. **Rules & Policy** — .claude/rules/ modular reference with enforcement
3. **API Docs** — /// doc comments on public types/functions
4. **Examples** — Real Rust code validated by `cargo test --doc`
5. **Specifications** — .specify/specs/*.ttl (source), generated .md
6. **Process** — Development workflow, gates, release process (this DoD!)
7. **Evidence-First** — Real paths, real OTEL output, actual execution proof
8. **Navigation** — Links in CLAUDE.md, docs/INDEX.md, no broken references

### Commands

```bash
cargo doc --no-deps              # Generate API docs
cargo test --doc --all           # Validate doc examples compile
ggen validate .specify/specs/*.ttl  # Validate specifications
```

---

## 7️⃣ Process Automation (10 Dimensions)

**Files:** `docs/automation/DEFINITION_OF_DONE.md`, `scripts/hooks/`

Automation prevents manual errors and enforces gates at commit/push time.

### 10 Automation Dimensions

1. **Git Hooks** — pre-commit + pre-push validation (no --no-verify bypass)
2. **CI/CD Gates** — GitHub Actions checks (check, test, lint)
3. **Just Recipes** — Cargo Make tasks coordinating all validation
4. **MCP Servers** — ggen-lsp-mcp, ggen-a2a-mcp health checks
5. **Andon Signals** — Stop-the-line detection (compiler errors, test failures)
6. **LSP Diagnostics** — GGEN-* codes block sync
7. **Sync Validation** — Lockfile, digests, profiles checked before emit
8. **Receipt Signing** — Ed25519 signatures on all artifacts
9. **Determinism** — Build reproducibility (hash verification)
10. **Auto-Merge** — Gates that enable merge to main

### Key Files

- `.git/hooks/pre-commit` → `scripts/hooks/pre-commit.sh` (builds workspace)
- `.git/hooks/pre-push` → `scripts/hooks/pre-push.sh` (runs tests)
- `justfile` — Entry point for all validation

---

## 8️⃣ Security & Signing (8 Dimensions)

**Files:** `docs/security/SECURITY_DEFINITION_OF_DONE.json`, `crates/ggen-receipt/`

All artifacts must be cryptographically signed and verifiable.

### 8 Security Dimensions

1. **Receipt Generation** — `.ggen/receipts/latest.json` emitted on every sync
2. **Signature Validation** — Ed25519 with `.ggen/keys/signing.key` and `.ggen/keys/verifying.key`
3. **Lockfile Digests** — SHA-256 per pack, verified at sync time
4. **Input/Output Hashing** — BLAKE3 of all inputs and outputs in receipt
5. **Chain Validation** — Receipt `previous_hash` links to prior receipt (unbroken)
6. **Tampering Detection** — Signature verification fails if receipt/lockfile modified
7. **Key Management** — Private keys never in repo, public keys verified
8. **Audit Trail** — All receipts immutable proof of what ran

### Commands

```bash
ggen receipt verify .ggen/receipts/latest.json --public-key .ggen/keys/public.pem
ggen packs verify --lockfile .ggen/packs.lock
```

### Sabotage Tests (Must Fail Loudly)

```bash
rm .ggen/packs.lock && ggen sync --locked          # Must exit non-zero
echo 'garbage' > .ggen/packs.lock && ggen sync     # Must exit non-zero
echo '{}' > .ggen/receipts/latest.json && ggen receipt verify  # is_valid: false
```

---

## 9️⃣ Developer Experience (7 Dimensions)

**Files:** `docs/dx/DEFINITION_OF_DONE.json`

Development must be frictionless, errors actionable, iteration fast.

### 7 DX Dimensions

1. **Command Ergonomics** — All critical commands via `just <task>`
2. **Error Messages** — Clear, actionable, recovery paths
3. **Workflow Efficiency** — Keyboard shortcuts, aliases, automation
4. **IDE Integration** — LSP, debugging, editor support
5. **Documentation Surface** — Quick references in-editor (CLAUDE.md, rules/)
6. **Hot Reload** — Fast iteration loops (cargo watch, test-unit)
7. **Sensible Defaults** — Reduce cognitive load, prevent foot-guns

### Commands

```bash
just --list              # Discover all tasks
just pre-commit          # Fast gate loop (90s)
just test-unit           # Unit tests only (10s)
```

---

## 🔟 Release Readiness (10 Dimensions)

**Files:** `docs/DEFINITION_OF_DONE_RELEASE.json`, `RELEASE_QUICK_CHECK.sh`

All gates must pass before shipping. No exceptions.

### 10 Release Dimensions

1. **Core Gates** — All 5 gates pass (check, lint, test, slo-check)
2. **Artifact Emission** — Code generation outputs with correct content
3. **Checksum Validation** — SHA-256 checksums match expected
4. **Signature Verification** — Receipt signatures validate
5. **Lockfile Finality** — `.ggen/packs.lock` frozen, no changes
6. **Version Tagging** — Semantic versioning enforced
7. **Changelog** — All changes documented
8. **Smoke Testing** — End-to-end validation of generated code
9. **Deployment Checklist** — No uncommitted changes, branch clean, CI green
10. **Rollback Plan** — How to revert if needed (use prior receipt hash)

### Pre-Release Checklist

```bash
# Run comprehensive gate
./RELEASE_QUICK_CHECK.sh

# Verify all artifacts exist and are signed
ggen receipt verify .ggen/receipts/latest.json
ggen packs verify --lockfile .ggen/packs.lock

# Create version tag
git tag -s v26.5.29

# Push to origin
git push -u origin claude/amazing-euler-z33vha
git push --tags
```

---

## Master Definition of Done Validation

**All work is DONE only when this passes:**

```bash
# Phase 1: Validation
timeout-check ✅ && \
check ✅ && \
lint ✅ && \
test ✅ && \
slo-check ✅ && \

# Phase 2: Testing
test-mutation (score ≥60%) ✅ && \
RUST_LOG=trace cargo test (OTEL spans verified) ✅ && \

# Phase 3: Performance
cargo bench (no regressions) ✅ && \

# Phase 4: Security
ggen receipt verify ✅ && \
ggen packs verify ✅ && \

# Phase 5: Documentation
cargo doc ✅ && \
cargo test --doc ✅ && \
ggen validate *.ttl ✅ && \

# Phase 6: Release
./RELEASE_QUICK_CHECK.sh ✅
```

---

## The Golden Rule

> **All 10 dimensions must be satisfied. Partial completion is failure. Work until all gates are green.**

This is not a suggestion. This is production discipline.

---

## Files Reference

| Dimension | File | Type |
|-----------|------|------|
| 1 | `docs/DEFINITION_OF_DONE.json` | JSON spec |
| 2 | `docs/TESTING_DEFINITION_OF_DONE.json` | JSON spec |
| 3 | `docs/observability/OTEL_DEFINITION_OF_DONE.json` | JSON spec |
| 4 | `docs/PERFORMANCE_DEFINITION_OF_DONE.json` | JSON spec |
| 5 | `docs/validation/VALIDATION_GATES_DEFINITION_OF_DONE.json` | JSON spec |
| 6 | `docs/DOCUMENTATION_DEFINITION_OF_DONE.json` | JSON spec |
| 7 | `docs/automation/DEFINITION_OF_DONE.md` | Markdown |
| 8 | `docs/security/SECURITY_DEFINITION_OF_DONE.json` | JSON spec |
| 9 | `docs/dx/DEFINITION_OF_DONE.json` | JSON spec |
| 10 | `docs/DEFINITION_OF_DONE_RELEASE.json` | JSON spec |

---

**Status:** ✅ All 10 dimensions production-ready | **Next:** Commit and push to branch
