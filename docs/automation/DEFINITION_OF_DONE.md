<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen v26.5.28 — Process Automation Definition of Done](#ggen-v26528--process-automation-definition-of-done)
  - [Overview](#overview)
  - [10 Automation Dimensions](#10-automation-dimensions)
    - [Dimension 1: Git Hooks (Pre-Commit & Pre-Push Enforcement)](#dimension-1-git-hooks-pre-commit--pre-push-enforcement)
    - [Dimension 2: CI/CD Gates (GitHub Actions Quality-Gate Pipeline)](#dimension-2-cicd-gates-github-actions-quality-gate-pipeline)
    - [Dimension 3: Just Recipes (Cargo Make Task Coordination)](#dimension-3-just-recipes-cargo-make-task-coordination)
    - [Dimension 4: MCP Servers (ggen-lsp-mcp & ggen-a2a-mcp Health)](#dimension-4-mcp-servers-ggen-lsp-mcp--ggen-a2a-mcp-health)
    - [Dimension 5: Andon Signals (Stop-the-Line Protocol)](#dimension-5-andon-signals-stop-the-line-protocol)
    - [Dimension 6: LSP Diagnostics (GGEN-* Codes Block Sync)](#dimension-6-lsp-diagnostics-ggen--codes-block-sync)
    - [Dimension 7: Sync Validation Gates (Lockfile, Digests, Profiles)](#dimension-7-sync-validation-gates-lockfile-digests-profiles)
    - [Dimension 8: Receipt Signing (Cryptographic Ed25519 Signatures)](#dimension-8-receipt-signing-cryptographic-ed25519-signatures)
    - [Dimension 9: Determinism Checks (Build Reproducibility)](#dimension-9-determinism-checks-build-reproducibility)
    - [Dimension 10: Auto-Merge Gates (Conditions Enabling Merge)](#dimension-10-auto-merge-gates-conditions-enabling-merge)
  - [Cross-Dimensional Flow](#cross-dimensional-flow)
  - [Gate Summary](#gate-summary)
  - [Definition of Done Checklist](#definition-of-done-checklist)
    - [Before every commit](#before-every-commit)
    - [Before every push](#before-every-push)
    - [Before every merge](#before-every-merge)
    - [Before every release](#before-every-release)
  - [Failure Scenarios & Responses](#failure-scenarios--responses)
    - [Scenario 1: Compilation error in pre-commit](#scenario-1-compilation-error-in-pre-commit)
    - [Scenario 2: Test failure in pre-push](#scenario-2-test-failure-in-pre-push)
    - [Scenario 3: Clippy warning in CI](#scenario-3-clippy-warning-in-ci)
    - [Scenario 4: GGEN-TPL-001 diagnostic blocks sync](#scenario-4-ggen-tpl-001-diagnostic-blocks-sync)
    - [Scenario 5: Non-deterministic output detected](#scenario-5-non-deterministic-output-detected)
  - [What Makes Bypass Impossible](#what-makes-bypass-impossible)
  - [Related Documents](#related-documents)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen v26.5.28 — Process Automation Definition of Done

**Version**: 26.5.28 | **Last Updated**: 2026-06-14 | **Authority**: `.claude/rules/coding-agent-mistakes.md`, `andon/signals.md`, git hooks, CI/CD workflows

---

## Overview

This document specifies the complete automation infrastructure preventing manual errors and enforcing gates across 10 dimensions. It defines what automation must exist, why it matters, when it runs, what it validates, how to recover from failures, and what prevents bypass.

**Core Principle**: Automation prevents bad code from entering the repository. Every gate has no manual bypass option.

---

## 10 Automation Dimensions

### Dimension 1: Git Hooks (Pre-Commit & Pre-Push Enforcement)

**Files**: `scripts/hooks/pre-commit.sh`, `scripts/hooks/pre-push.sh`

**What it is**: Bash scripts that run locally before committing/pushing to main.

**Why it matters**: 
- Pre-commit catches 62% of defects in <10 seconds
- Pre-push catches 97% of defects in <90 seconds
- Prevents broken code from entering the repository

**Trigger points**:
- Pre-commit: After `git commit` on main branch
- Pre-push: Before `git push origin main`

**What it validates**:

| Tier | Gates | Defect Coverage | Timeout |
|------|-------|-----------------|---------|
| **Pre-Commit (Fast)** | `just check`, `just fmt-check` | 62% | <10s |
| **Pre-Push (Full)** | `just check`, `just lint`, `just fmt-check`, `just test-lib` | 97% | <90s |

**Recovery**:
1. Pre-commit fails → Run `just check` locally, fix errors, retry commit
2. Pre-push fails → Run each gate individually, fix, retry push
3. Format errors → Run `just fmt` to auto-fix, re-commit

**Bypass prevention**:
- FORBIDDEN: `git commit --no-verify` (CI will catch broken code anyway)
- FORBIDDEN: `git push --no-verify` (CI quality gates are authoritative)
- Enforcement: `.git/hooks/pre-commit` and `.git/hooks/pre-push` are symlinked to scripts/hooks/
- Rationale: Commits to main that bypass the local hook will still fail CI checks, so bypassing is pointless

---

### Dimension 2: CI/CD Gates (GitHub Actions Quality-Gate Pipeline)

**Files**: `.github/workflows/ci.yml`, `.github/workflows/quality-gates.yml`

**What it is**: Automated GitHub Actions workflows triggered on push/PR. Gates prevent merge until all checks pass.

**Why it matters**:
- CI is the final authoritative boundary before code reaches main
- Runs on GitHub infrastructure with consistent Rust toolchain (stable + beta)
- Catches platform-specific issues, flaky tests, code organization violations

**Trigger points**:
- On push to main (immediate execution)
- On PR open, synchronize, ready_for_review (blocks merge)

**What it validates**:

| Gate | Check | Severity |
|------|-------|----------|
| **File Organization** | No .rs/.ttl/.tmpl in root; no temp files | GATE 0 |
| **Comprehensive Tests** | `cargo test --workspace --tests` (stable + beta) | GATE 1 |
| **Quality Gates** | Panic point detection; code organization | GATE 2 |

**Recovery**:
1. Read GitHub Actions log (click red X on commit)
2. Panic points detected → Add `// SAFE: reason` comment or remove the call
3. File organization error → Move file to correct subdirectory
4. Test failure → Investigate test logic, ensure real collaborators, rerun
5. Re-trigger: Push new commit (CI runs automatically)

**Bypass prevention**:
- FORBIDDEN: Merge PR without CI status check passing (GitHub enforces)
- FORBIDDEN: Push directly to main (branch protection requires CI + code review)
- Enforcement: GitHub branch protection rules (seanchatmangpt/ggen settings)
- Requirement: All workflows must report success before 'Merge' button is enabled

---

### Dimension 3: Just Recipes (Cargo Make Task Coordination)

**File**: `justfile`

**What it is**: Single entry point for all development tasks. `just` ensures consistent execution across contributors.

**Why it matters**:
- One recipe runs the right tool with the right timeout
- Avoids skipped steps and incomplete workflows
- Pre-commit, CI, and agent automation all delegate to just recipes

**Key recipes**:

```bash
just check          # Compilation check (60s) — VITAL for pre-commit
just fmt-check      # Format check without modifying (2m)
just lint           # Clippy with -D warnings (90s) — VITAL for pre-push
just test           # Full test suite (<30s hot cache, 120s escalation)
just test-lib       # Unit tests only (<30s) — VITAL for pre-push
just pre-commit     # Full gate: fmt-check → check → lint → test-lib
just sync           # Full μ₁-μ₅ pipeline with audit
just sync-dry       # Preview sync without writing
```

**Recovery**:
1. Recipe timeout → Just automatically escalates to longer timeout on first run
2. Compilation error → Fix source, rerun `just check`
3. Lint failure → Run `just fmt` first, then `just lint`
4. Test failure → Run `just test-unit`, identify issue, fix, rerun

**Bypass prevention**:
- FORBIDDEN: Direct `cargo check` instead of `just check` (bypasses timeout)
- FORBIDDEN: Direct `cargo test` instead of `just test` (loses escalation logic)
- Enforcement: `.claude/rules/_core/absolute.md` Rule 4: "ALWAYS just <task>"
- Rationale: Just recipes encode timeouts, escalation logic, and dependency ordering

---

### Dimension 4: MCP Servers (ggen-lsp-mcp & ggen-a2a-mcp Health)

**Files**: `crates/ggen-lsp-mcp/src/`, `crates/ggen-a2a-mcp/src/a2a_registry/health.rs`

**What it is**: Two MCP (Model Context Protocol) servers exposing ggen-lsp repair routes and A2A agent coordination as tools.

**Why it matters**:
- MCP servers are how agents execute repairs and fix GGEN-* diagnostics
- If unavailable or stuck, agent automation cannot proceed
- Health monitor prevents agents from being called when endpoints are down

**Servers**:

| Server | Purpose | Exposes |
|--------|---------|---------|
| **ggen-lsp-mcp** | Repair routes for GGEN-* diagnostics | source_law_repair, security_repair, proof_topology_repair tools |
| **ggen-a2a-mcp** | Agent coordination and health monitoring | Agent registry, health check loop (pings agents every 5s) |

**Recovery**:
1. ggen-lsp-mcp stuck → Kill process, restart Claude Code
2. Health check failing → Check agent endpoint URL reachable (`curl -I <url>`)
3. MCP won't start → Check tracing_subscriber initialization, review logs for panic

**Bypass prevention**:
- FORBIDDEN: Running ggen sync while GGEN-* diagnostics are active
- FORBIDDEN: Ignoring health monitor alerts (agents marked :Unreachable)
- Enforcement: ggen-cli sync checks for active diagnostics before pipeline
- Rationale: MCP tools are the only way to repair law-surface violations

---

### Dimension 5: Andon Signals (Stop-the-Line Protocol)

**File**: `.claude/rules/andon/signals.md`

**What it is**: Signal detection protocol that halts all work when critical failures appear.

**Why it matters**:
- Signals indicate system is in invalid state
- Continuing work while signals active results in cascading failures
- Protocol enforces immediate fix-and-retry

**Signal levels**:

| Level | Pattern | Action |
|-------|---------|--------|
| **CRITICAL** | `error[E...]` | HALT. Compiler errors. Nothing else exists until resolved. |
| **CRITICAL** | `test ... FAILED` | HALT. Test failures. Read failure, identify cause, fix. |
| **HIGH** | `warning:` | STOP before release. Unacceptable in committed code. |
| **HIGH** | Clippy errors | STOP before release. Catches real bugs. |
| **CLEAR** | All checks pass | Proceed. Only when every gate is green. |

**Recovery protocol**:
```
Step 1: Read the signal (file:line, assertion, compiler rule)
Step 2: Identify root cause (not symptom, the actual cause)
Step 3: Fix the cause (modify source code)
Step 4: Re-run the check (recompile, retest)
Step 5: Repeat until all signals clear
```

**Bypass prevention**:
- FORBIDDEN: `#[allow(...)]` to silence warnings
- FORBIDDEN: `#[ignore]` on failing tests
- FORBIDDEN: `|| true` to mask failures
- FORBIDDEN: `unwrap()` to bypass error handling
- FORBIDDEN: `todo!()` in committed code
- Enforcement: Clippy lints, pre-push hook, CI checks reject these
- Rationale: Suppressing signals makes problems invisible and worse

---

### Dimension 6: LSP Diagnostics (GGEN-* Codes Block Sync)

**Files**: `crates/ggen-lsp/src/route/diagnostic_species.rs`, `crates/ggen-lsp/src/check.rs`

**What it is**: Language server diagnostic codes (GGEN-TPL-001, GGEN-OUT-001, etc.) that indicate law-surface violations.

**Why it matters**:
- GGEN-* codes are the interface between ontology and execution engine
- ERROR severity means sync pipeline will fail anyway
- Blocking sync prevents broken artifacts from being written

**Key diagnostic codes**:

| Code | Trigger | Severity | Repair |
|------|---------|----------|--------|
| **GGEN-TPL-001** | Template uses `{{ var }}` that SELECT doesn't produce | ERROR | source_law_repair |
| **GGEN-OUT-001** | `output_file` pattern uses unbound variable | ERROR | source_law_repair |
| **GGEN-RULE-001** | `{file = ...}` references missing file | ERROR | source_law_repair |
| **GGEN-YIELD-001** | `output_file` escapes project root | ERROR | security_repair |
| **GGEN-HARNESS-001** | Cargo.toml [[test]] path mismatch | RELEASE_BLOCKING | proof_topology_repair |
| **E0011 / E0013** | SPARQL lacks ORDER BY | WARNING / ERROR (strict) | — |

**Recovery**:
1. Diagnostic appears → Read the code (GGEN-TPL-001, etc.)
2. Open ggen-lsp repair route → Accept the repair
3. Verify diagnostic clears → Save file
4. Retry ggen sync

**Bypass prevention**:
- FORBIDDEN: Running `ggen sync` with active GGEN-* ERROR diagnostics
- Enforcement: ggen-cli sync checks for diagnostics before starting pipeline
- Exit code: sync returns 1 if diagnostics block execution
- Rationale: GGEN-* errors indicate invariant violations; sync would fail anyway

---

### Dimension 7: Sync Validation Gates (Lockfile, Digests, Profiles)

**Files**: `crates/ggen-cli/src/cmds/sync.rs`, `crates/ggen-core/src/lockfile.rs`, `crates/ggen-marketplace/src/rdf/control.rs`

**What it is**: Validation checkpoints before artifact emission: lockfile presence, digest verification, profile enforcement.

**Why it matters**:
- Prevents pipeline from producing artifacts under invalid conditions
- Without these gates, sync would silently write broken artifacts

**Gates (in order)**:

| Gate | Check | Failure Mode |
|------|-------|--------------|
| **A: Lockfile** | `.ggen/packs.lock` exists and is parseable | Err(MissingLockfile), exit 1 |
| **B: Digests** | Each pack digest matches file on disk | Err(DigestMismatch), exit 1 |
| **C: Profile** | Profile constraints are satisfied | Err(ProfileViolation), exit 1 |
| **D: Diagnostics** | No GGEN-* ERROR codes active | Err(DiagnosticsBlocking), exit 1 |

**Recovery**:
1. Lockfile missing → Run `ggen packs add <pack_id>`
2. Digest mismatch → Reinstall pack or restore TOML
3. Profile violation → Update pack trust tier or relax constraints
4. GGEN-* blocking → Use ggen-lsp repair route, retry sync

**Bypass prevention**:
- FORBIDDEN: Skipping digest verification with a flag (no `--skip-digests` exists)
- FORBIDDEN: Emitting artifacts when profile is violated (ProfileEnforcer enforces)
- FORBIDDEN: Continuing with active GGEN-* ERROR diagnostics
- Enforcement: All gates are hard-coded, no environment variables
- Rationale: Skipping leads to decorative completion (success reported, artifact invalid)

---

### Dimension 8: Receipt Signing (Cryptographic Ed25519 Signatures)

**Files**: `crates/ggen-core/src/receipt.rs`, `crates/ggen-config/src/receipt/mod.rs`

**What it is**: Every ggen sync produces a signed receipt (.ggen/receipts/latest.json) binding input hashes to output hashes.

**Why it matters**:
- Receipts are the proof object connecting source-of-truth to artifacts
- Ed25519 signature proves: (1) exact input was processed, (2) exact output was generated, (3) process was authorized

**Receipt structure**:

```json
{
  "operation_id": "<UUID v4, non-zero>",
  "timestamp": "<RFC-3339>",
  "input_hashes": {
    "<pack_id@version>": "<SHA-256>",
    "ontology": "<SHA-256>",
    "ggen_toml": "<SHA-256>"
  },
  "output_hashes": {
    "path/to/artifact.rs": "<SHA-256>",
    ...
  },
  "signature": "<base64 Ed25519 (non-empty!)>"
}
```

**Invariants**:
- `operation_id`: Real Uuid::new_v4(), never hardcoded
- `timestamp`: Current RFC-3339, not stale
- `input_hashes`: Every pack consumed at locked version
- `output_hashes`: Every file written during emit
- `signature`: Non-empty base64, signed with private key

**Recovery**:
1. Empty signature → Fix receipt-generation code to call sign() method
2. Invalid signature → Regenerate receipt by running sync again
3. Missing receipt → Run sync to create one

**Bypass prevention**:
- FORBIDDEN: Emitting artifacts without signed receipt (μ₅ always generates)
- FORBIDDEN: Hardcoding operation_id or timestamp (must be real values)
- FORBIDDEN: Skipping signature generation (always sign)
- Enforcement: sync.rs enforces no artifact emission until receipt is signed
- Test: crates/ggen-config/tests/ includes sabotage tests for empty signature

---

### Dimension 9: Determinism Checks (Build Reproducibility)

**What it is**: Validation that ggen is deterministic: same inputs → bit-identical outputs → identical receipt hashes.

**Why it matters**:
- Non-determinism indicates hidden state, timing-dependent behavior
- Required for reproducible builds, audit trails, receipt verification

**What to prevent**:
- HashMap iteration order (use BTreeMap)
- UUID generation in output (use deterministic IDs)
- Timestamps in generated code (derive from receipt, not current time)
- Random numbers without seed (use RNG_SEED env var)
- File modification times (use consistent mtime)

**Validation**:
```bash
# Run 1: Execute ggen sync → receipt_1.json
# Run 2: Execute ggen sync again → receipt_2.json
# Check: receipt_1.input_hashes == receipt_2.input_hashes
# Check: receipt_1.output_hashes == receipt_2.output_hashes
# Check: All files are byte-identical
```

**Recovery**:
1. Non-deterministic output → Diff receipt hashes to find which file differs
2. Random behavior → Search for Uuid::new_v4() or rand:: calls
3. HashMap ordering → Replace with BTreeMap, add sort()
4. Timing-dependent → Remove DateTime::now() from artifacts

**Bypass prevention**:
- FORBIDDEN: Using HashMap for iteration in μ₃-μ₅ stages (use BTreeMap)
- FORBIDDEN: Uuid::new_v4() in generated artifact content
- FORBIDDEN: Current time in generated code (should be in receipt only)
- Test: `tests/determinism_test.rs` verifies two runs produce identical artifacts

---

### Dimension 10: Auto-Merge Gates (Conditions Enabling Merge)

**What it is**: Conditions that must be satisfied before GitHub auto-merge is enabled.

**Why it matters**:
- Auto-merge gates ensure only code passing all quality checks reaches main
- Manual merge could bypass critical gates

**Required status checks** (ALL must pass):
1. File Organization Check
2. Comprehensive Tests (stable)
3. Comprehensive Tests (beta) — future-proofing
4. Quality Gates / Poka-Yoke

**Branch protection rules** (GitHub settings for seanchatmangpt/ggen):
- Required status checks: All above must be green
- Require branches up to date: PR must be rebased on main
- Require code reviews: Minimum 1 approval (can be 2 for stricter gates)
- Dismiss stale reviews: Old approvals invalidated if PR updates
- Allow auto-merge: Can enable per-PR, but only if all checks pass

**No direct push to main**:
- GitHub rule prevents direct push to main (must use PR)
- Exception: github-actions bot can push (semantic versioning, releases)

**Merge strategy**: Squash and merge (one commit per PR, cleaner history)

**Recovery**:
1. If CI fails → Fix locally, push new commit, CI re-runs
2. If PR behind main → Click 'Update branch', CI re-runs

**Bypass prevention**:
- FORBIDDEN: Direct push to main (branch protection prevents)
- FORBIDDEN: Merge if required check failing (GitHub enforces)
- FORBIDDEN: Force-push to main to bypass checks (GitHub hook prevents)
- Enforcement: GitHub repository settings (Branch protection rules)
- Rationale: All gates must pass; no 'quick merge' option exists

---

## Cross-Dimensional Flow

```
Developer edits code
  ↓
Pre-commit hook: just check, just fmt-check (Dimension 3)
  → If fails: Stop, fix, retry commit (Dimension 5 Andon)
  ↓
Commit accepted locally
  ↓
Developer pushes to feature branch
  ↓
Pre-push hook: just check, just lint, just test-lib (Dimension 3)
  → If fails: Stop, fix, retry push (Dimension 5 Andon)
  ↓
Push accepted, PR updated
  ↓
CI/CD workflows trigger (Dimension 2)
  - File Organization Check
  - Comprehensive Tests (stable + beta)
  - Quality Gates / Poka-Yoke
  → If any fails: PR is blocked (cannot merge)
  → Developer fixes, pushes new commit, CI re-runs
  ↓
All CI checks pass: Merge button enabled (Dimension 10 gates)
  ↓
Code review approval (if required)
  ↓
Merge to main (auto-merge enabled or manual click)
  ↓
Main branch CI runs (final verification)
  ↓
Release tags created (semantic versioning)
  ↓
Release workflow builds artifacts (Dimension 10)
  ↓
Release published
```

---

## Gate Summary

| Gate | Tier | Checks | Coverage | Timeout |
|------|------|--------|----------|---------|
| Pre-commit | Local | `check`, `fmt-check` | 62% | <10s |
| Pre-push | Local | `check`, `lint`, `fmt-check`, `test-lib` | 97% | <90s |
| CI file-org | GitHub | File organization | 100% | 5m |
| CI tests | GitHub | Tests (stable + beta) | Varies | 30m |
| CI quality | GitHub | Panic detection, code org | ~5% | 10m |
| Branch protection | GitHub | Status checks + review | 100% | Instant |

---

## Definition of Done Checklist

### Before every commit
- [ ] Run `just pre-commit` (fast tier)
- [ ] Read error output
- [ ] Fix all issues locally
- [ ] Rerun `just pre-commit` until passing

### Before every push
- [ ] Run `just pre-commit` (fast tier)
- [ ] `git push` (pre-push hook runs full tier)
- [ ] Read hook output
- [ ] Fix issues, rerun `just test-lib`, retry push

### Before every merge
- [ ] All CI checks pass (file org, tests, quality gates)
- [ ] Code review approved (if required)
- [ ] PR up to date with main
- [ ] No GGEN-* ERROR diagnostics blocking
- [ ] Click 'Merge' (or enable auto-merge)

### Before every release
- [ ] Main branch CI is green
- [ ] Create release tag (`git tag -a v<version>`)
- [ ] GitHub Actions release workflow creates artifacts
- [ ] Verify artifacts downloadable
- [ ] Update Homebrew formula (manual step)

---

## Failure Scenarios & Responses

### Scenario 1: Compilation error in pre-commit
**Signal**: `error[E0425]: cannot find value 'foo'`

**Response**:
1. Pre-commit hook exits 1, commit rejected
2. Read error (file:line where undefined)
3. Fix the code (define `foo` or use correct identifier)
4. Rerun `just check` locally
5. Retry `git commit`

### Scenario 2: Test failure in pre-push
**Signal**: `test foo::bar::test_baz ... FAILED`

**Response**:
1. Pre-push hook exits 1, push rejected
2. Run `just test-lib` locally
3. Read test failure output
4. Identify root cause (logic bug, missing setup)
5. Fix code or test
6. Rerun `just test-lib`
7. Retry `git push`

### Scenario 3: Clippy warning in CI
**Signal**: `warning: unused variable: 'x'`

**Response**:
1. CI check reports warning, merge blocked
2. Read GitHub Actions log
3. Fix warning (remove variable or use it)
4. Verify with `just lint` locally
5. Commit and push fix
6. CI re-runs and passes

### Scenario 4: GGEN-TPL-001 diagnostic blocks sync
**Signal**: `GGEN-TPL-001: Template uses {{ var }} that SELECT doesn't produce`

**Response**:
1. `ggen check` reports ERROR diagnostic
2. Open ggen-lsp repair route (CodeAction in editor)
3. Accept the repair
4. Rerun ggen sync

### Scenario 5: Non-deterministic output detected
**Signal**: `Receipt hash differs on second sync run`

**Response**:
1. Determinism test fails
2. Diff receipts to find which file differs
3. Search for random sources (HashMap, Uuid::new_v4(), DateTime::now())
4. Replace non-deterministic code
5. Verify second sync produces identical artifacts

---

## What Makes Bypass Impossible

**Prevents Bypass**:
- Git hooks are locally enforced (CI gates catch broken code anyway)
- CI gates are enforced by GitHub branch protection (cannot merge if failing)
- Just recipes encode timeouts (cannot be bypassed without losing safety)
- MCP servers are required for LSP repair routes (cannot fix GGEN-* without them)
- Andon signals enforced by pre-commit/pre-push and CI (must fix, cannot suppress)
- Sync gates are hard-coded (no environment variables to skip)
- Receipt signatures are Ed25519 (cannot forge without private key)
- Branch protection rules are GitHub-enforced (cannot push directly to main)

**Makes Faking Impossible**:
- Tests must use real collaborators (Chicago TDD only, no mocks)
- Receipts must have non-empty signature (assertion checks this)
- Artifacts must be written to disk (observable, verifiable with `stat`)
- Digest verification happens before sync (pack TOML hash must match)
- GGEN-* diagnostics checked before pipeline (cannot emit with active errors)
- Determinism checks automated (two runs must produce identical outputs)
- Andon signals non-suppressible (compiler errors cannot be ignored)

---

## Related Documents

- `.claude/rules/coding-agent-mistakes.md` — 6-question patch contract, 5 mistake classes, 4 invariants, sabotage tests
- `.claude/rules/andon/signals.md` — Stop-the-line protocol
- `.claude/rules/validation-persistence.md` — Keep working until all checks pass
- `scripts/hooks/pre-commit.sh`, `pre-push.sh` — Implementation
- `.github/workflows/ci.yml`, `quality-gates.yml` — CI implementation
- `justfile` — Just recipes
- `Cargo.toml` — 15-crate workspace definition

---

**Next Review**: 2026-07-14 (assess completeness of MCP health monitoring, auto-merge coverage)
