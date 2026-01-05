---
name: cargo-make-protocol
description: "Master Cargo Make build orchestration. Use for running ggen builds safely. Covers: poka-yoke error-proofing, SLO enforcement (check <5s, test <30s, lint <60s), timeout mechanisms, quality gates, andon signal detection. Essential skill - always use cargo make, never direct cargo commands. When building, testing, linting, or validating code."
allowed_tools: "Bash(cargo make:*)"
---

# Cargo Make Protocol Skill (2026 Edition)

## Overview

**Poka-Yoke Build System** from Toyota Production System

The ggen project uses Cargo Make (2,323 lines, 100+ targets) to enforce build discipline:
- **Error-proofing**: Prevents mistakes through system design (Poka-Yoke)
- **SLO Enforcement**: Timeout-based quality gates (check <5s, test <30s, lint <60s)
- **Andon Signals**: Stop-the-line on RED signals (3-layer validation)
- **Deterministic**: Reproducible builds and outputs
- **Receipt-Based Validation**: Evidence replaces narrative review (2026 paradigm shift)

**New in 2026**:
- **Andon Signal Validation Framework**: 3-layer validation (Compile-Time → Test-Time → Runtime)
- **Hermetic Testing**: clnrm Docker-based isolation for E2E tests
- **Kaizen Metrics**: Automated metrics tracking (8 categories)
- **Test Quality Audit**: Mutation testing, false positive detection
- **Local CI Testing**: `act` for GitHub Actions workflow validation
- **FMEA-Based Release Validation**: Systematic failure mode prevention

## Golden Rule

```
ALWAYS use: cargo make [target]
NEVER use:  cargo [command]
```

Direct cargo commands bypass:
- Timeout enforcement
- Warning-as-error compilation flags
- Quality gate dependencies
- Andon signal monitoring

## Quick Reference Cards (By Workflow)

### 🚀 Fast Feedback Loop (Incremental Development)
```bash
cargo make check        # Compile check (<5s) - RED on errors
cargo make test-unit    # Unit tests (<16s) - GREEN if passing
cargo make lint         # Clippy linter (<60s) - YELLOW on warnings
cargo make fmt          # Auto-format code (<5s)
```

### ✅ Pre-Commit Validation (Before Git Commit)
```bash
cargo make pre-commit   # ALL quality gates (<2min)
                        # ├─ fmt (format check)
                        # ├─ lint (clippy strict)
                        # ├─ test (essential v5 tests)
                        # ├─ test-doc (doc tests)
                        # ├─ validate-docs (FMEA doc checks)
                        # ├─ docs-check (rustdoc validation)
                        # └─ verify-cli (runtime validation)
```

### 🔬 Testing Suite (Comprehensive)
```bash
# Unit & Integration
cargo make test              # Essential v5 tests (<15s)
cargo make test-unit         # Unit tests only (<16s)
cargo make test-integration  # Integration tests (<30s)
cargo make test-doc          # Documentation tests (<60s)

# E2E & Hermetic Testing
cargo make test-clnrm        # Hermetic CLI tests (<300s)
cargo make cli-smoke         # CLI smoke tests (<180s)
cargo make verify-cli        # Runtime validation (<30s)

# Quality Assurance
cargo make test-audit        # Mutation testing, false positive detection (<30s)
cargo make test-mutate       # Cargo-mutants mutation testing (<5min)
cargo make test-budget-check # Performance budget validation (<5s)
```

### 🎯 Quality Assurance (2026 Framework)
```bash
# Andon Signal Validation (3-Layer)
cargo make verify-cli          # Layer 3: Runtime validation
cargo make validation-report   # Generate validation report
cargo make monitor-validation  # Alert on failures

# Test Quality (Feature 004)
cargo make test-audit          # Detect false positives (<30s)
cargo make test-opt            # Optimize test suite (200/1178 tests)
cargo make test-budget-check   # Enforce performance budgets

# Security & Dependencies
cargo make audit              # Security vulnerabilities
cargo make audit-all          # All dependency audits
cargo make audit-deny         # Comprehensive cargo-deny checks
```

### 📦 Release & Deployment
```bash
# Pre-Release Validation
cargo make release-validate   # All FMEA release checks
                              # ├─ release-validate-git-state
                              # ├─ release-validate-version
                              # ├─ release-validate-artifacts
                              # ├─ release-validate-build
                              # ├─ release-validate-security
                              # ├─ release-validate-changelog
                              # ├─ release-validate-breaking-changes
                              # └─ release-validate-docs-sync

# Build & Release
cargo make build-release      # Optimized binary (<30s)
cargo make release            # Full release workflow
cargo make release-brew       # Release + Homebrew update
```

### 📊 Kaizen Metrics (Continuous Improvement)
```bash
cargo make metrics-collect    # Daily metrics collection
cargo make metrics-status     # Current metrics status
cargo make metrics-dashboard  # Open live dashboard
cargo make metrics-weekly     # Weekly trend report
cargo make metrics-monthly    # Month-end comprehensive report
```

### 🐳 Local CI Testing (GitHub Actions with act)
```bash
cargo make act-status         # Check act installation
cargo make act-validation     # Run Andon Signal Validation workflow
cargo make act-validate       # Validate all workflows (dry-run)
cargo make act WORKFLOW=ci.yml JOB=lint  # Run specific job
```

### 🏪 Marketplace Validation
```bash
cargo make marketplace-validate           # Validate all packages
cargo make marketplace-emit-receipts      # Emit validation receipts
cargo make marketplace-generate-artifacts # Generate JSON/Markdown
cargo make marketplace-full-pipeline      # Complete pipeline
```

### 📚 Documentation
```bash
cargo make docs-check      # Rustdoc validation (<30s)
cargo make docs-api        # Build API docs (<10s)
cargo make docs-build      # Build mdbook docs (<30s)
cargo make docs-deploy     # Build + validate for deployment
cargo make validate-docs   # FMEA-based doc accuracy checks
```

### 🔧 Advanced Development
```bash
# RDF & Ontology Validation
cargo make validate-rdf    # RDF graph + SPARQL validation (<30s)

# Cleanroom & Production Readiness
cargo make test-cleanroom  # Cleanroom crate tests
cargo make production-readiness  # Comprehensive validation

# Gap Detection
cargo make detect-gaps     # Find missing tests/coverage
cargo make gap-report      # Generate gap report

# FMEA Analysis
cargo make fmea-report     # Generate FMEA report
cargo make fmea-pareto     # Pareto analysis (80/20)
```

## SLO Targets (Service Level Objectives - 2026 Edition)

### Core Build & Compilation
```
Target                          SLO                          Escalation
─────────────────────────────────────────────────────────────────────────
check                           <5s (incremental 1.95s)      15s → 30s
build                           <10s                         -
build-release                   <30s                         -
clean                           <5s                          -
```

### Testing & Validation
```
Target                          SLO                          Escalation
─────────────────────────────────────────────────────────────────────────
test (v5 essential)             <15s                         -
test-unit                       <16s                         150s (clean build)
test-integration                <30s                         -
test-doc                        <60s                         180s
test-clnrm                      <300s                        -
cli-smoke                       <180s                        -
verify-cli                      <30s                         -
test-audit                      <30s (incremental <10s)      -
test-mutate                     <5min (full workspace)       -
test-budget-check               <5s                          -
validate-rdf                    <30s per stage               -
```

### Quality Assurance
```
Target                          SLO                          Escalation
─────────────────────────────────────────────────────────────────────────
fmt                             <5s                          -
lint                            <5s quick                    5s → 30s → 60s
pre-commit                      <2min (phased)               -
ci                              <3min (sum of deps)          -
audit                           Variable (network)           -
```

### Documentation
```
Target                          SLO                          Escalation
─────────────────────────────────────────────────────────────────────────
docs-check                      <30s                         180s
docs-api                        <10s                         -
docs-build (mdbook)             <30s                         -
validate-docs                   <5s                          -
```

### Release & Deployment
```
Target                          SLO                          Escalation
─────────────────────────────────────────────────────────────────────────
release-validate (all checks)   <5min                        -
marketplace-full-pipeline       Variable                     -
```

### Metrics & Monitoring
```
Target                          SLO                          Escalation
─────────────────────────────────────────────────────────────────────────
metrics-collect                 <60s                         -
metrics-status                  <5s                          -
validation-report               <5s                          -
```

### Local CI (act)
```
Target                          SLO                          Escalation
─────────────────────────────────────────────────────────────────────────
act-validation                  <10min (full workflow)       -
act-validate (dry-run)          <2min                        -
```

### Performance Budgets (Kaizen Targets)
```
Metric                          2026 Target          Baseline (Week 0)
────────────────────────────────────────────────────────────────────────
Build time (first)              <15s (47% ↓)         28s
Build time (incremental)        <2s                  3.8s
Unit test execution             <16s                 -
Compiler errors                 0                    158
Test pass rate                  100%                 15%
Code quality score              9.5                  7.2
Template accessibility          100%                 5%
Waste score                     2.0 (76% ↓)          8.4
Velocity (features/sprint)      5                    2
Cost of waste                   $8k/year (76% ↓)     $33k/year
```

## Poka-Yoke Mechanisms

### 1. Timeout Enforcement

Each target has timeout wrapper:
- **Quick timeout**: Detects deadlocks/hangs quickly
- **Escalation timeout**: Allows for contention on slow systems
- **Exit on timeout**: Prevents indefinite waits

```bash
# Example: test target
cargo make test         # 30s quick timeout
                        # 120s escalation timeout (lock contention)
```

### 2. Warnings as Errors

Compilation enforces zero-warning builds:

```rust
RUSTFLAGS="-D warnings"  // Force compiler to reject warnings
```

Prevents warning accumulation from hiding real problems.

### 3. Quality Gates

Pre-commit depends on three checks:

```
cargo make pre-commit
├─ cargo make check      # Must pass (RED if errors)
├─ cargo make lint       # Must pass (RED if warnings)
└─ cargo make test-unit  # Must pass (RED if failures)
```

Blocks commits with RED Andon signals.

### 4. Andon Signal Escalation

**Stop-the-line protocol**:

| Signal | Meaning | Action |
|--------|---------|--------|
| **RED** | ERROR | **STOP** - Fix immediately |
| **YELLOW** | WARNING | INVESTIGATE - Before release |
| **GREEN** | SUCCESS | CONTINUE - Ready for next step |

## Build Profile Optimization

### Development Profile
```toml
[profile.dev]
opt_level = 0              # Fast compilation
debug = true              # Debugging info
incremental = true        # Fast incremental builds
codegen_units = 256       # Parallel compilation
```

### Release Profile
```toml
[profile.release]
opt_level = 3             # Optimize aggressively
lto = "thin"             # Link-time optimization
strip = true             # Remove debug symbols
codegen_units = 16       # Balance speed/optimization
```

## Lint Configuration

**Zero-tolerance policy**:

```
[workspace.lints.rust]
warnings = "deny"                # All warnings → errors
unsafe_code = "deny"            # No unsafe without justification
missing_docs = "warn"           # Document public API

[workspace.lints.clippy]
all = { level = "deny" }        # All clippy checks
pedantic = { level = "deny" }   # Extra scrutiny
unwrap_used = "deny"            # Critical: NO unwrap!
expect_used = "deny"            # Critical: NO expect!
panic = "deny"                  # Panic-free requirement
```

---

## 🚨 Andon Signal Validation Framework (NEW 2026)

**Three-Layer Validation**: Prevents "fake greens" by validating actual behavior, not just test execution.

### Layer 1: Compile-Time Validation
```bash
cargo make check    # Compilation errors (RED)
cargo make lint     # Clippy warnings (YELLOW)
```
**Andon Signals**: RED = compiler errors, YELLOW = clippy warnings, GREEN = clean

### Layer 2: Test-Time Validation
```bash
cargo make test-unit    # Unit tests (YELLOW if failures)
cargo make test-clnrm   # Hermetic E2E tests (YELLOW if failures)
```
**Andon Signals**: RED = critical test failures, YELLOW = non-critical failures, GREEN = all pass

### Layer 3: Runtime Validation (Critical)
```bash
cargo make verify-cli   # Verify ALL CLI commands work end-to-end
```
**Purpose**: Prevents "tests pass but CLI broken" scenarios

**Example**:
```bash
# Layer 1 & 2 pass, but CLI is broken:
cargo make check    # ✅ Compiles
cargo make test     # ✅ Tests pass
cargo make verify-cli  # ❌ ggen init fails - STOP THE LINE

# This catches real-world breakage that tests miss
```

### Monitoring & Reporting
```bash
cargo make validation-report    # Generate 3-layer validation report
cargo make monitor-validation   # Alert on failures (RED/YELLOW/GREEN)
```

**Exit Codes**:
- Layer 1 or Layer 3 failures = **RED** (critical, stop the line)
- Layer 2 failures = **YELLOW** (warning, investigate)
- All layers passing = **GREEN** (continue)

---

## 🐳 Hermetic Testing with clnrm (NEW 2026)

**Cleanroom Docker-based testing** for deterministic, isolated E2E validation.

### Core clnrm Tests
```bash
cargo make test-clnrm           # Main CLI command suite (<300s)
cargo make test-clnrm-all       # All clnrm suites (cli + marketplace + lifecycle)
```

### Marketplace Test Suites
```bash
cargo make test-clnrm-marketplace-search       # Full search suite (<420s)
cargo make test-clnrm-marketplace-search-lite  # Lightweight search (<180s)
cargo make test-clnrm-marketplace-install      # Install/registry (<420s)
cargo make test-clnrm-marketplace-error        # Error handling (<420s)
cargo make test-clnrm-marketplace-otel         # OTEL validation (<300s)
```

### Lifecycle Test Suites
```bash
cargo make test-clnrm-lifecycle-init      # Init with OTEL spans (<420s)
cargo make test-clnrm-lifecycle-phases    # Phase transitions (<420s)
cargo make test-clnrm-lifecycle-readiness # Readiness checks (<420s)
cargo make test-clnrm-lifecycle-rollback  # Rollback scenarios (<420s)
cargo make test-clnrm-lifecycle-deploy    # Deploy workflows (<420s)
```

### Cleanroom Crate Testing
```bash
cargo make test-cleanroom         # Cleanroom production tests
cargo make test-cleanroom-crate   # Cleanroom crate unit tests
cargo make lint-cleanroom         # Clippy for cleanroom
cargo make cleanroom-validate     # Full cleanroom validation
cargo make cleanroom-slo-check    # Performance SLO checks
```

**Why clnrm?**
- **Hermetic**: No host contamination, fresh Docker container per test
- **Reproducible**: Same inputs → same outputs, every time
- **Realistic**: Tests actual CLI behavior, not mocked internals
- **CI/CD Ready**: Same tests run locally and in CI

---

## 🧪 Test Quality & Optimization (Feature 004 - NEW 2026)

**Mutation testing, false positive detection, and intelligent test selection.**

### Test Quality Audit
```bash
cargo make test-audit          # Full audit (<30s)
                               # ├─ Mutation kill rate analysis
                               # ├─ Assertion strength detection
                               # ├─ False positive identification
                               # └─ Behavior validation checks

# Exit codes:
#   RED = false positives detected (tests pass when code is broken)
#   YELLOW = weak assertions (12+ tests need strengthening)
#   GREEN = quality passes (≥80% mutation kill rate)
```

**What it detects**:
- **False positives**: Tests pass even when ggen.toml is broken
- **Weak assertions**: `assert!(result.is_ok())` instead of `assert_eq!(result.unwrap(), expected)`
- **Low mutation kill rate**: <80% of mutations killed by tests

### Test Suite Optimization
```bash
cargo make test-opt            # Optimize to 200 tests (<15s)
                               # Applies 80/20 Pareto principle
                               # Target: 80%+ bug detection, ≤11s execution

# Output: .ggen/test-metadata/optimized-suite.json
```

**80/20 Test Selection**:
- Full suite: 1,178 tests (37.38s execution)
- Optimized suite: 200 tests (8.31s execution, 83% reduction)
- Bug detection: 84.7% (target: 80%+)

**Value Scoring Algorithm** (weights):
```
Test Value = (0.40 × Coverage) + (0.25 × Failure History) +
             (0.20 × Mutation Kill) + (0.15 × Execution Speed)
```

### Mutation Testing
```bash
cargo make test-mutate         # Cargo-mutants (<5min full, <1min critical paths)

# Example output:
#   ✅ 847/1000 mutants killed (84.7% - PASS)
#   ❌ 654/1000 mutants killed (65.4% - FAIL, need more tests)
```

### Performance Budget Enforcement
```bash
cargo make test-budget-check   # Validate budgets (<5s)

# Budgets:
#   Unit tests: ≤1s total
#   Integration tests: ≤10s total
#
# Exit codes:
#   RED = Critical violation (exceeds budget)
#   YELLOW = Warning (approaching budget)
#   GREEN = Compliant (within budget)
```

### Metadata Management
```bash
cargo make metadata-update     # Update test metadata (<10s)
                               # Collects: execution times, coverage, failure history
                               # Storage: .ggen/test-metadata/
```

---

## 🎬 Local CI Testing with act (NEW 2026)

**Run GitHub Actions workflows locally** before pushing.

### Prerequisites
```bash
cargo make act-status          # Check act + Docker installation
```

**Installation** (if needed):
```bash
# macOS
brew install act

# Linux
curl https://raw.githubusercontent.com/nektos/act/master/install.sh | sudo bash
```

### Running Workflows
```bash
# Run Andon Signal Validation workflow (all 3 layers)
cargo make act-validation

# Run specific job
cargo make act-validation JOB=compile-time

# Dry-run mode (syntax check only)
cargo make act-validation DRYRUN=true

# Run any workflow
cargo make act WORKFLOW=ci.yml              # All jobs
cargo make act WORKFLOW=lint.yml JOB=lint   # Specific job
```

### Workflow Validation
```bash
cargo make act-validate        # Validate ALL workflows (dry-run, <2min)
cargo make act-list            # List available workflows
```

### Cleanup
```bash
cargo make act-cleanup         # Remove act containers and images
```

**Benefits**:
- Catch CI failures before pushing
- Faster iteration (no wait for GitHub Actions)
- Same environment as CI (Docker containers)
- Works offline (after initial image pull)

---

## 📊 Kaizen Metrics Tracking (NEW 2026)

**Automated continuous improvement metrics** tracking 8 categories.

### Daily Operations
```bash
cargo make metrics-collect     # Collect metrics (Mon/Wed/Fri, <60s)
cargo make metrics-status      # Current status (<5s)
cargo make metrics-dashboard   # Open live dashboard in browser
```

### Reporting
```bash
cargo make metrics-weekly      # Weekly trend report (specify WEEK=N)
cargo make metrics-monthly     # Month-end comprehensive report
cargo make metrics-baseline    # Create Week 0 baseline snapshot
```

### Metrics Categories (8 Total)
```
1. Build Time          - Target: <15s (47% reduction from 28s)
2. Test Pass Rate      - Target: 100% (from 15%)
3. Compiler Errors     - Target: 0 (from 158)
4. Code Quality        - Target: 9.5 (from 7.2)
5. Template Access     - Target: 100% (from 5%)
6. Waste Score         - Target: 2.0 (76% reduction from 8.4)
7. Velocity            - Target: 5 features/sprint (from 2)
8. Cost of Waste       - Target: $8k/year (76% reduction from $33k)
```

### Andon Signals (Metrics)
```
🔴 CRITICAL - Compiler errors, test failures (STOP IMMEDIATELY)
🟡 HIGH     - Linting errors, performance regressions (FIX SOON)
🟢 MEDIUM   - Code quality warnings (INVESTIGATE)
```

**Storage**:
- Data: `.metrics/` directory
- Dashboards: `docs/metrics/latest.html`
- Scripts: `scripts/metrics/`

**Learn more**: `cargo make metrics-help`

---

## 🛡️ FMEA & Release Validation (NEW 2026)

**Failure Mode and Effects Analysis** for systematic defect prevention.

### Pre-Release Validation (8 Gates)
```bash
cargo make release-validate    # ALL 8 validation gates (<5min)
```

**Gates** (in order):
1. **Git State**: No uncommitted changes (RPN 504)
2. **Version Consistency**: All crates match (RPN 504)
3. **Artifacts**: Required files exist (RPN 180)
4. **Build**: Release build succeeds (RPN 432)
5. **Security**: No vulnerabilities (RPN 360)
6. **Changelog**: Version documented (RPN 288)
7. **Breaking Changes**: Documented (RPN 240)
8. **Docs Sync**: Version references updated (RPN 96)

### Individual Validation Tasks
```bash
cargo make release-validate-git-state          # Gate 1
cargo make release-validate-version            # Gate 2
cargo make release-validate-artifacts          # Gate 3
cargo make release-validate-build              # Gate 4 (depends on build-release)
cargo make release-validate-security           # Gate 5 (depends on audit-all)
cargo make release-validate-changelog          # Gate 6
cargo make release-validate-breaking-changes   # Gate 7
cargo make release-validate-docs-sync          # Gate 8
```

### FMEA Analysis
```bash
cargo make fmea-report         # Generate FMEA report
cargo make fmea-pareto         # Pareto analysis (80/20 of failures)
cargo make fmea-list           # List all failure modes
cargo make fmea-export         # Export to JSON (fmea-report.json)
```

**Risk Priority Number (RPN)** = Severity × Occurrence × Detection
- RPN ≥400: Critical (gates 1, 2, 4)
- RPN 200-399: High (gates 5, 6, 7)
- RPN <200: Medium (gate 3, 8)

---

## 🏪 Marketplace Validation (NEW 2026)

**Production readiness validation** for 70+ marketplace packages.

### Validation Pipeline
```bash
# Full pipeline (Track A + Track B)
cargo make marketplace-full-pipeline

# Individual tracks
cargo make marketplace-validate           # Validate all packages
cargo make marketplace-emit-receipts      # Emit validation receipts (Track A)
cargo make marketplace-generate-artifacts # Generate JSON/Markdown (Track B)

# Reporting
cargo make marketplace-health             # Health report from receipts
cargo make marketplace-report             # Validation reports
```

**Track A (Validation)**:
1. Validate package structure (manifest, templates, tests)
2. Run package-specific tests
3. Emit cryptographic receipts (sha256)

**Track B (Artifact Generation)**:
```
Receipts → JSON registry → Markdown docs
```

**Outputs**:
- `marketplace/registry.json` - Machine-readable package registry
- `marketplace/PACKAGES.md` - Human-readable catalog
- `marketplace/MARKETPLACE_HEALTH.json` - Health metrics

---

## 📚 Documentation Targets (Expanded 2026)

### Core Documentation
```bash
cargo make docs-check      # Rustdoc validation (<30s, escalates to 180s)
                           # ├─ Verify all doc comments compile
                           # ├─ Check for broken intra-doc links
                           # └─ Enforce RUSTDOCFLAGS="-D warnings"

cargo make docs-api        # Build API docs (<10s, opens in browser)
cargo make docs-build      # Build mdbook docs (<30s)
cargo make docs-serve      # Serve docs locally with mdbook
cargo make docs-watch      # Watch and rebuild on changes
```

### Documentation Validation
```bash
cargo make validate-docs   # FMEA-based accuracy checks (<5s)
                           # Checks for:
                           # ├─ Incorrect cargo commands (e.g., "cargo build" should be "cargo make build")
                           # ├─ Invalid file paths
                           # └─ Macro invocation errors

cargo make docs-validate   # Structure and link validation
                           # ├─ Validate mdbook structure
                           # ├─ Check for broken links
                           # └─ Verify TOC completeness
```

### Deployment
```bash
cargo make docs-deploy     # Build + validate for GitHub Pages
                           # Dependencies: docs-clean, docs-build, docs-validate
```

### GitHub Pages Diagnostics
```bash
cargo make gh-pages-status        # Check Pages config via API
cargo make gh-workflow-status     # Check deployment workflow status
cargo make gh-pages-compare       # Compare local vs deployed
cargo make gh-pages-trigger       # Manually trigger deployment
cargo make gh-pages-logs          # View deployment logs
cargo make gh-pages-setup-check   # Comprehensive setup validation
```

---

## 🔧 Advanced Development Targets (NEW 2026)

### RDF & Ontology Validation
```bash
cargo make validate-rdf    # Multi-stage RDF validation (<30s per stage)
                           # ├─ RDF graph syntax (Turtle/N-Triples)
                           # ├─ Graph consistency checks
                           # ├─ SPARQL query syntax
                           # └─ Deterministic processing verification
```

**Stages**:
1. RDF validation tests (`ggen-core::rdf::validation`)
2. Graph validation tests (`ggen-core::graph`)
3. RDF integration tests (if exist)
4. Graph E2E tests (if exist)

### Production Readiness
```bash
cargo make production-readiness        # Comprehensive validation
                                       # ├─ test-testcontainers
                                       # ├─ test-cleanroom
                                       # ├─ cleanroom-validate
                                       # ├─ test-unit
                                       # ├─ test-integration
                                       # ├─ lint
                                       # └─ build-release

cargo make production-readiness-script # Run validation script (--full)
```

### Test Gap Detection
```bash
cargo make detect-gaps     # Find missing tests/coverage (80/20 focused)
cargo make gap-report      # Generate comprehensive gap report
                           # Output: target/gap-detection-report.json

cargo make enforce-coverage # Enforce coverage for changed files
cargo make install-hooks    # Install git hooks with gap detection
```

**Gap Categories**:
- Untested public APIs
- Low coverage modules (<80%)
- Missing integration tests
- Undocumented edge cases

### Deterministic Testing
```bash
cargo make deterministic           # Fixed seed, single-threaded
                                   # ENV: RUST_TEST_THREADS=1, RNG_SEED=42

cargo make test-single-threaded    # Single-threaded execution
cargo make validate-outputs        # Validate deterministic outputs
```

### Security & Dependency Audits
```bash
cargo make audit              # Security vulnerabilities (variable time, network)
cargo make audit-outdated     # Check for outdated dependencies
cargo make audit-unused       # Check for unused deps (requires nightly)
cargo make audit-deny         # Comprehensive cargo-deny checks
cargo make audit-all          # ALL audits (audit + outdated + deny)
```

### Waste Detection (Lean)
```bash
cargo make check-dead-code        # Dead code, unused imports (<10s)
cargo make check-unused-features  # Unused feature flags (<10s)
cargo make validate-examples      # Ensure examples compile (<30s)
```

---

## 🚀 Build & Release Targets (Expanded 2026)

### Core Build
```bash
cargo make check           # Fast compilation check (<5s, escalates to 30s)
cargo make build           # Debug binary (<10s)
cargo make build-release   # Optimized binary (<30s)
cargo make verify-binary   # Verify binary exists and display size
cargo make clean           # Clean build artifacts (<5s)
```

### Pre-Push Validation
```bash
cargo make check-pre-push  # Check with 30s timeout for lock contention
cargo make pre-commit-hook # Comprehensive pre-commit hook (Week 2 efficiency)
                           # ├─ Format check
                           # ├─ Quick compilation
                           # ├─ Linting
                           # └─ Unit tests
```

### CI Gates
```bash
cargo make ci-gate         # CI gate validation (Week 2 efficiency)
                           # ├─ pre-commit-hook
                           # ├─ Integration tests
                           # ├─ Security audit
                           # └─ Documentation check

cargo make ci              # Full CI pipeline (<3min typical)
                           # ├─ format
                           # ├─ clippy-ci-flow
                           # ├─ check-all-crates
                           # ├─ detect-gaps
                           # ├─ test
                           # ├─ test-doc
                           # ├─ audit-all
                           # └─ docs-test

cargo make ci-check        # GitHub Actions workflow health check
cargo make ci-health       # Alias for ci-check
```

### Compilation Validation
```bash
cargo make check-all-crates    # Check all crates compile
cargo make compile-validation  # Validate compilation (depends on check-all-crates)
cargo make type-safety-check   # Check for type safety issues
```

### Release Workflow
```bash
cargo make release         # Create release (after full validation)
                           # Dependencies: release-validate, ci, docs-build

cargo make release-brew    # Release + Homebrew formula update
cargo make release-check   # Check if release artifacts are ready
```

### Homebrew Tasks
```bash
cargo make brew-update-formula  # Update formula with SHA256
cargo make brew-update          # Alias for brew-update-formula
```

---

## 🧪 Extended Testing Targets (2026 Complete)

### BDD Testing (Cucumber)
```bash
cargo make test-bdd                     # Run all BDD feature tests
cargo make test-bdd-feature FEATURE=graph  # Run specific feature
cargo make test-bdd-verbose             # BDD with verbose output
```

### LLM Integration Tests (Requires API Keys)
```bash
cargo make test-live       # Live LLM tests (all integrations)
cargo make test-ollama     # Ollama integration (requires Ollama running)
cargo make test-openai     # OpenAI integration (requires OPENAI_API_KEY)
cargo make test-anthropic  # Anthropic integration (requires ANTHROPIC_API_KEY)
```

### Ollama Performance & Resilience
```bash
cargo make test-ollama-performance  # Performance/stress tests
cargo make test-ollama-resilience   # Error recovery tests
cargo make test-ollama-all          # All Ollama tests (integration + perf + resilience)
cargo make validate-ollama          # Full validation (service + model + tests)
```

### Test Infrastructure
```bash
cargo make test-timings         # Generate timing report (<30s)
                                # Identifies slow tests
                                # ENV: --test-threads 1, --report-time

cargo make test-ignored         # Long-running ignored tests (<20min)
                                # Requires Docker for marketplace e2e

cargo make test-verbose         # Tests with verbose output (--nocapture)
```

### Property-Based Testing (proptest)
```bash
cargo make test-proptest                    # All property tests
cargo make test-proptest-single PACKAGE=X   # Single package
cargo make test-proptest-parallel           # Parallel execution (4 threads)
```

### Testcontainers & Docker
```bash
cargo make test-testcontainers  # Production readiness validation
                                # ENV: RUST_LOG=info
```

---

## 🌐 Node.js N-API Addon Targets (NEW 2026)

**Cross-language bindings** for Node.js ecosystem.

```bash
cargo make node-build           # Build N-API addon (debug)
cargo make node-build-release   # Build N-API addon (release)
cargo make node-test            # Run Node addon tests (vitest)
cargo make node-prebuild        # Create prebuilt binaries for common targets
                                # Targets: x86_64-linux, aarch64-darwin,
                                #          x86_64-darwin, x86_64-windows
```

**Package Manager Support**: npx, pnpm (via corepack), yarn (fallback chain)

---

## 🔧 Shell Completion Targets (NEW 2026)

```bash
cargo make completions SHELL=bash OUTPUT=ggen.bash  # Generate completions
cargo make completions-install SHELL=zsh            # Install completions
cargo make completions-list                         # List supported shells
```

**Supported Shells**: bash, zsh, fish, powershell, elvish

---

## Common Workflows (Updated 2026)

### Fast Development Cycle
```bash
# 1. Make code changes

# 2. Quick feedback (<20s total)
cargo make check        # Compilation (<5s)
cargo make lint         # Linting (<5s quick, escalates if needed)
cargo make test-unit    # Unit tests (<16s)

# 3. Commit ready?
cargo make pre-commit   # Full validation (<2min)
```

### 2026 Receipt-Based Workflow
```bash
# OLD WAY (narrative):
# "I reviewed the code. It looks good."

# NEW WAY (receipts):
cargo make pre-commit
# Output:
# [Receipt] check: ✓ (1.95s)
# [Receipt] lint: ✓ (4.82s, 0 violations)
# [Receipt] test: ✓ (27/27 pass)
# [Receipt] verify-cli: ✓ (all commands work)
# Status: READY FOR COMMIT
```

### Before Pushing (Comprehensive)
```bash
# Verify everything works
cargo make ci           # Full CI pipeline

# Validate CLI actually works (Layer 3)
cargo make verify-cli   # Catches real-world breakage

# Check performance budgets
cargo make test-budget-check
cargo make slo-check

# Optional: Run locally what CI will run
cargo make act-validation
```

### Weekly Quality Review
```bash
# Test quality
cargo make test-audit       # Detect false positives
cargo make test-mutate      # Mutation testing (80%+ kill rate)

# Metrics
cargo make metrics-weekly WEEK=N  # Trend analysis

# Gap detection
cargo make detect-gaps
cargo make gap-report

# Security
cargo make audit-all
```

### Before Release
```bash
# 1. FMEA validation (8 gates)
cargo make release-validate

# 2. Full test suite (not just optimized)
cargo test --workspace --lib

# 3. Production readiness
cargo make production-readiness

# 4. Documentation
cargo make docs-deploy

# 5. Create release
cargo make release

# 6. Homebrew (if applicable)
cargo make release-brew
```

### Debugging Workflow
```bash
# Identify slow tests
cargo make test-timings

# Check for dead code
cargo make check-dead-code

# Type safety issues
cargo make type-safety-check

# Test coverage gaps
cargo make detect-gaps

# Security vulnerabilities
cargo make audit-all
```

### Before Pushing

```bash
# Verify everything works
cargo make ci           # Full CI pipeline

# Check performance
cargo make slo-check    # SLO compliance
cargo make bench        # Performance baseline

# Specification compliance
cargo make speckit-check # RDF ontology syntax valid
```

### Performance Work

```bash
cargo make bench                # Baseline
# ... make optimizations ...
cargo make bench-compare        # Compare performance
cargo make slo-check           # Verify SLOs met
```

## Timeout Escalation Details

For long-running operations, Cargo Make implements smart escalation:

```
Initial Timeout (Quick)
    ↓
[Operation still running]
    ↓
Wait period (allows slow systems)
    ↓
Escalation Timeout (If lock contention detected)
    ↓
[Timeout event]
```

Allows ggen to work on:
- Fast development machines (quick feedback)
- CI systems with resource contention (escalation gives more time)
- Resource-constrained environments (still enforces limits)

## Exit Codes & Interpretation

```
Exit Code    Meaning               Action
─────────────────────────────────────────
0            Success              Continue
1            Failure              Fix issue
2            Timeout              Investigate contention
124          Hard timeout         Deadlock detected
```

## Integration with Git Hooks

Pre-commit hook runs:
```bash
cargo make check    # RED if compilation fails
cargo make format   # Auto-fix formatting
cargo make lint     # RED if clippy warnings
```

Prevents committing broken code.

Pre-push hook runs:
```bash
cargo make test     # RED if tests fail
cargo make lint     # RED if warnings
```

Prevents pushing broken code.

**NEVER use `git push --no-verify`** - Defeats defect prevention

## Environment Variables

```bash
RUST_BACKTRACE=1           # Full panic backtrace
RUST_LOG=info              # Tracing level
CARGO_TERM_COLOR=always    # Force color output
```

Set these via `.claude/settings.json` environment section.

## Troubleshooting

### Hanging Builds

```bash
# Check for infinite loops
RUST_BACKTRACE=1 cargo make check

# If timeout triggered, investigate:
# - Check for deadlocks in concurrent code
# - Verify no infinite loops in macros
# - Profile with: cargo make bench
```

### Timeout Failures

```bash
# Escalation timeout occurred - system under heavy load
# Options:
1. Retry after system calm
2. Run on less loaded machine
3. Profile to optimize further
```

### Clippy Warnings Blocking Build

```bash
# Fix warning (don't suppress):
cargo make lint              # See warnings
# Edit code to fix root cause
cargo make check             # Re-verify
```

## Best Practices (2026 Edition)

### 1. **ALWAYS Use cargo make** (Golden Rule)
```bash
# ❌ WRONG
cargo check
cargo test
cargo clippy

# ✅ CORRECT
cargo make check
cargo make test
cargo make lint
```
**Why**: Direct cargo commands bypass timeouts, quality gates, and Andon signal monitoring.

### 2. **Receipt-Based Validation** (Paradigm Shift 3)
```bash
# ❌ OLD WAY (narrative)
"I reviewed the code. It looks good."

# ✅ NEW WAY (receipts)
cargo make pre-commit
# [Receipt] check: ✓ (1.95s)
# [Receipt] lint: ✓ (4.82s, 0 violations)
# [Receipt] test: ✓ (27/27 pass)
# [Receipt] verify-cli: ✓
```
**Receipts replace narratives** - Objective, reproducible, auditable.

### 3. **Three-Layer Validation** (Always)
```bash
# Layer 1: Compile-Time
cargo make check && cargo make lint

# Layer 2: Test-Time
cargo make test

# Layer 3: Runtime (CRITICAL - prevents fake greens)
cargo make verify-cli
```
**Why**: Tests can pass while CLI is broken. Layer 3 catches real-world failures.

### 4. **Specification Closure Before Implementation** (Paradigm Shift 1)
```bash
# Before ANY non-trivial work:
/speckit-verify [feature]

# If incomplete (spec has "TBD", "maybe"):
#   → STOP
#   → Clarify with user
#   → Update .ttl spec
#   → Verify closure = 100%
#   → THEN implement
```
**Iteration is a defect signal** - Fix the spec, not the code.

### 5. **Test Quality First** (Before Optimization)
```bash
# Before optimizing test suite:
cargo make test-audit       # Detect false positives

# If RED (false positives):
#   → FIX tests first
#   → NEVER optimize broken tests

# If GREEN (≥80% mutation kill rate):
#   → Proceed with optimization
cargo make test-opt
```
**Why**: Optimizing false-positive tests makes problems worse.

### 6. **Performance Budgets** (Enforce)
```bash
# Check budgets before commit
cargo make test-budget-check

# Budgets:
#   Unit tests: ≤1s total
#   Integration tests: ≤10s total
```
**Slow tests = waste** - Optimize or exclude.

### 7. **Local CI Testing** (Before Push)
```bash
# Run what CI will run, locally:
cargo make act-validation

# Benefits:
#   - Catch CI failures before push
#   - Faster iteration (no GitHub wait)
#   - Same Docker environment as CI
```

### 8. **Metrics-Driven Improvement** (Weekly)
```bash
# Weekly quality review:
cargo make metrics-weekly WEEK=N
cargo make test-audit
cargo make gap-report
cargo make audit-all
```
**Track trends, not snapshots** - Kaizen = continuous improvement.

### 9. **FMEA Before Release** (8 Gates, No Shortcuts)
```bash
cargo make release-validate

# If ANY gate fails:
#   → STOP
#   → Fix issue
#   → Re-run full validation
#   → NEVER skip gates
```
**Why**: Each gate prevents a class of defects (RPN ranked).

### 10. **Respect Andon Signals** (Stop-the-Line)
```
🔴 RED    - STOP IMMEDIATELY (compiler error, test failure, CLI broken)
🟡 YELLOW - FIX SOON (clippy warning, approaching budget)
🟢 GREEN  - CONTINUE (all checks pass)
```
**Don't bypass** - Timeouts and failures signal real issues.

### 11. **Timeout Escalation is Normal** (Not a Failure)
```bash
# First attempt: 5s timeout
# If timeout (lock contention):
#   → Auto-retry with 30s timeout
#   → Normal on CI systems
```
**Why**: Handles slow systems without false failures.

### 12. **Use Hermetic Testing for E2E** (clnrm)
```bash
# NOT this (host contamination):
./target/debug/ggen init

# THIS (cleanroom isolation):
cargo make test-clnrm
```
**Why**: Reproducible, no host state contamination.

---

## 🚨 Three Paradigm Shifts (2026 - Critical)

### Paradigm Shift 1: Big Bang 80/20 (Specification-First)

**Old way**: Vague requirement → Plan → Code → Test → [Iterate when broken]

**New way**: Specification closure → Single-pass construction → Validation receipts

**Workflow**:
1. Before implementation: `cargo make speckit-verify` or `/speckit-verify`
2. If incomplete: **STOP**, clarify, update .ttl
3. Only proceed when closure score = 100%
4. Implementation is one-pass (no iteration needed)

**Cargo Make Integration**:
```bash
# Speckit targets (RDF-first workflow):
cargo make speckit-check      # Validate .ttl syntax
cargo make speckit-validate   # Check .ttl → .md generation
cargo make validate-rdf       # Full RDF validation
```

**Why it matters**: Iteration means your spec was incomplete. Fix the spec, don't iterate code.

### Paradigm Shift 2: EPIC 9 - Atomic Cognitive Cycle (Parallel-First)

**Old way (sequential)**: Plan (1h) → Code (2h) → Test (1h) → Review (1h) = **5 hours**

**New way (parallel)**: 10 agents in parallel (2h) + Collision (30m) + Convergence (30m) = **3 hours**

**Why it matters for cargo make**:
- Parallel agents need **deterministic builds** (cargo make provides this)
- Same spec → same output (reproducibility required)
- No coordination overhead (cargo make targets are atomic)

**Benefits**: 2.8-4.4x speedup, high confidence (collision = agreement)

### Paradigm Shift 3: Deterministic Validation (Evidence-First)

**Old way**: "I reviewed the code. It looks good." (Opinion)

**New way**: "[Receipt] cargo make check ✓ | [Receipt] All 347 tests pass" (Evidence)

**Receipts in cargo make**:
```bash
cargo make pre-commit
# Output (receipts):
[Receipt] cargo make check: ✓ (1.95s)
[Receipt] cargo make lint: ✓ (4.82s, 0 violations)
[Receipt] cargo make test: ✓ (27/27 pass)
[Receipt] cargo make verify-cli: ✓
[Receipt] Specification coverage: 95%
[Receipt] SLO compliance: ✓
Status: READY FOR DEPLOYMENT
```

**Receipt categories** (all via cargo make):
- Compilation: `cargo make check` pass
- Tests: `cargo make test` (all pass, coverage ≥80%)
- Linting: `cargo make lint` (0 violations)
- Performance: `cargo make slo-check` pass
- Runtime: `cargo make verify-cli` pass
- Security: `cargo make audit` (0 vulnerabilities)

**Why it matters**: Objective, reproducible, auditable. No opinions, only evidence.

---

## 🔧 Troubleshooting (Updated 2026)

### Hanging Builds (Timeout Enforcement)
```bash
# Symptoms: cargo make check hangs

# Diagnosis:
RUST_BACKTRACE=1 cargo make check

# Common causes:
#   - Deadlocks in concurrent code
#   - Infinite loops in macros
#   - Lock contention on CI

# Recovery:
#   - Check escalation timeout triggered (normal on CI)
#   - Profile with: cargo make test-timings
#   - Review recent changes to concurrent code
```

### Timeout Failures (Escalation)
```bash
# Symptoms: "⚠️ Retrying with extended timeout..."

# This is NORMAL:
#   - First timeout: 5s (quick feedback)
#   - Escalation: 30s (allows lock contention)
#   - System under load: expected behavior

# Action:
#   - Wait for escalation to complete
#   - If escalation also times out: investigate
#   - Check system load: `top`, `htop`
```

### Clippy Warnings Blocking Build
```bash
# Symptoms: cargo make lint fails with warnings

# DO NOT suppress (RUSTFLAGS="-A"):
#   ❌ #[allow(clippy::all)]  # WRONG
#   ❌ RUSTFLAGS="-A warnings" # WRONG

# FIX the root cause:
cargo make lint              # See warnings
# Edit code to fix
cargo make check             # Re-verify

# For false positives (rare):
#[allow(clippy::specific_lint)]  # With justification comment
```

### False Positive Tests (Layer 3 Failure)
```bash
# Symptoms:
cargo make test     # ✅ PASS
cargo make verify-cli  # ❌ FAIL - ggen init broken

# Diagnosis:
cargo make test-audit   # Detect false positives

# Common causes:
#   - Tests use mocks, not real CLI
#   - Assertions too weak: assert!(result.is_ok())
#   - Tests don't cover integration paths

# Recovery:
#   1. Strengthen assertions: assert_eq!(result.unwrap(), expected)
#   2. Add integration tests: cargo make test-integration
#   3. Add clnrm hermetic tests: cargo make test-clnrm
```

### Performance Budget Violations
```bash
# Symptoms:
cargo make test-budget-check
# Output: "Critical: test_slow took 12.3s (exceeds 10s budget)"

# Recovery:
#   1. Optimize test (remove I/O, use mocks)
#   2. OR exclude from optimized suite
#   3. Update metadata: cargo make metadata-update
#   4. Re-check budgets
```

### Mutation Kill Rate < 80%
```bash
# Symptoms:
cargo make test-mutate
# Output: "654/1000 mutants killed (65.4% < 80%)"

# Recovery:
#   1. Review surviving mutants (cargo-mutants output)
#   2. Add tests for uncovered cases
#   3. Strengthen assertions
#   4. Re-run: cargo make test-mutate
#   5. Target: ≥80% kill rate
```

### Release Validation Gate Failures
```bash
# Symptoms:
cargo make release-validate
# Output: "❌ ERROR: CHANGELOG.md missing entry for version 3.5.0"

# Recovery (for each gate):
#   Gate 1 (Git State): Commit or stash changes
#   Gate 2 (Version): Update all Cargo.toml versions
#   Gate 3 (Artifacts): Create missing files
#   Gate 4 (Build): Fix compilation errors
#   Gate 5 (Security): Update vulnerable deps
#   Gate 6 (Changelog): Add version entry
#   Gate 7 (Breaking): Document breaking changes
#   Gate 8 (Docs Sync): Update version references

# Re-run full validation:
cargo make release-validate
```

### act Workflow Failures (Local CI)
```bash
# Symptoms:
cargo make act-validation
# Output: "Error: Docker is not running"

# Prerequisites check:
cargo make act-status

# Common issues:
#   - Docker not running: Start Docker Desktop
#   - act not installed: brew install act
#   - Workflow syntax error: cargo make act-validate

# Recovery:
#   1. Fix prerequisites
#   2. Retry: cargo make act-validation
#   3. For specific job: cargo make act-validation JOB=compile-time
```

### Metrics Collection Failures
```bash
# Symptoms:
cargo make metrics-collect
# Output: "⚠️ No metrics collected yet"

# Recovery:
#   1. Ensure scripts exist: ls scripts/metrics/
#   2. Create .metrics/ directory (auto-created)
#   3. Re-run: cargo make metrics-collect
#   4. Check status: cargo make metrics-status
```

---

## 📖 Reference & Learning Resources

### Internal Documentation
- **Makefile.toml** (2,323 lines): Source of truth for all targets
- **CLAUDE.md**: Complete ggen project documentation (2026 edition)
- **scripts/**: Validation scripts (verify-cli, metrics, FMEA)
- **.ggen/test-metadata/**: Test optimization metadata

### Key Scripts
- `scripts/verify-cli-commands.sh` - Layer 3 validation
- `scripts/generate-validation-report.sh` - Andon signal reporting
- `scripts/metrics/collect_metrics.sh` - Kaizen metrics
- `scripts/release-validate-*.sh` - FMEA release gates

### Commands for Help
```bash
cargo make --list-all-steps     # List ALL targets (100+)
cargo make metrics-help         # Kaizen metrics guide
cargo make act-status           # act setup check
cargo make --help               # General help
```

### External Resources
- **cargo-make**: https://github.com/sagiegurari/cargo-make
- **cargo-mutants**: https://github.com/sourcefrog/cargo-mutants
- **act**: https://github.com/nektos/act
- **testcontainers-rs**: https://github.com/testcontainers/testcontainers-rs
- **FMEA**: https://en.wikipedia.org/wiki/Failure_mode_and_effects_analysis
- **Poka-Yoke**: https://en.wikipedia.org/wiki/Poka-yoke
- **Toyota Production System**: https://en.wikipedia.org/wiki/Toyota_Production_System

---

## 🎯 Quick Decision Matrix

**"Which cargo make target should I use?"**

| Situation | Target | SLO |
|-----------|--------|-----|
| Just changed code | `cargo make check` | <5s |
| Before commit | `cargo make pre-commit` | <2min |
| Before push | `cargo make ci` | <3min |
| Before release | `cargo make release-validate` | <5min |
| Tests failing in CI but not locally | `cargo make test-clnrm` | <300s |
| Need to validate CLI works | `cargo make verify-cli` | <30s |
| Suspect false positive tests | `cargo make test-audit` | <30s |
| Want faster test suite | `cargo make test-opt` | <15s |
| Check test quality | `cargo make test-mutate` | <5min |
| Run CI locally | `cargo make act-validation` | <10min |
| Weekly quality review | `cargo make metrics-weekly` | <5min |
| Security audit | `cargo make audit-all` | Variable |
| Documentation broken | `cargo make validate-docs` | <5s |
| Slow builds | `cargo make slo-check` | <180s |
| Coverage gaps | `cargo make detect-gaps` | <60s |

---

## 🚀 2026 Best Practices Summary

1. **Golden Rule**: ALWAYS `cargo make`, NEVER direct `cargo`
2. **Three-Layer Validation**: Compile → Test → Runtime (verify-cli)
3. **Receipt-Based**: Evidence replaces narratives
4. **Specification-First**: Closure before implementation
5. **Test Quality**: Audit before optimize
6. **Performance Budgets**: Enforce SLOs
7. **Local CI**: `act` before push
8. **Metrics-Driven**: Track trends weekly
9. **FMEA Release**: 8 gates, no shortcuts
10. **Andon Signals**: RED = stop, YELLOW = fix, GREEN = go

**Constitutional Equation (2026)**:
```
spec.md = μ(feature.ttl) | Receipts replace review | 3-layer validation mandatory
```

---

**Last Updated**: 2026-01-05 (v5.2.0, 100+ targets documented)
