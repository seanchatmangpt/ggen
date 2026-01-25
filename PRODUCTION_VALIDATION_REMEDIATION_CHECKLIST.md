# Production Validation Remediation Checklist
**Date Created**: 2026-01-25
**Target Status**: Production Ready ✅
**Current Status**: 🔴 CRITICAL ISSUES BLOCKING - 0% Ready

---

## PHASE 1: CRITICAL BLOCKERS (MUST COMPLETE FIRST)

These items block all other work. Complete in order.

### Task 1.1: Fix Cargo.toml - Remove Duplicate ggen-ai Declaration

**File**: `/home/user/ggen/Cargo.toml`
**Problem**: `ggen-ai` declared twice - once required (line 151), once optional (line 294)
**Status**: 🔴 NOT STARTED

- [ ] **1.1.1**: Locate and remove duplicate at line 294
  ```toml
  # REMOVE THIS:
  ggen-ai = { path = "crates/ggen-ai", version = "0.2.0", optional = true }
  ```

- [ ] **1.1.2**: Ensure line 151 keeps optional declaration
  ```toml
  # SHOULD BE (after fix):
  ggen-ai = { path = "crates/ggen-ai", version = "0.2.0", optional = true }
  ```

- [ ] **1.1.3**: Update line 310 to reference optional (if currently duplicated)

- [ ] **1.1.4**: Verify no other ggen-ai declarations exist
  ```bash
  grep -n "ggen-ai\s*=" /home/user/ggen/Cargo.toml
  # Should show only ONE declaration with optional = true
  ```

**Acceptance Criteria**:
- [x] Grep returns exactly 1 result: `ggen-ai = { ... optional = true }`
- [x] No parse errors: `cargo metadata --format-version 1` succeeds

**Estimated Time**: 5 minutes

---

### Task 1.2: Fix Cargo.toml - Remove Duplicate ggen-dspy Declaration

**File**: `/home/user/ggen/Cargo.toml`
**Problem**: `ggen-dspy` declared twice - once required (line 151), once optional (line 383)
**Status**: 🔴 NOT STARTED

- [ ] **1.2.1**: Locate line 383 and remove duplicate optional declaration
  ```toml
  # REMOVE THIS:
  ggen-dspy = { path = "crates/ggen-dspy", version = "0.2.0", optional = true }
  ```

- [ ] **1.2.2**: Keep line 151 with optional declaration
  ```toml
  # SHOULD BE (after fix):
  ggen-dspy = { path = "crates/ggen-dspy", version = "0.2.0", optional = true }
  ```

- [ ] **1.2.3**: Verify feature flag `ai = ["ggen-ai", "ggen-dspy"]` references optional

- [ ] **1.2.4**: Verify no other ggen-dspy declarations exist
  ```bash
  grep -n "ggen-dspy\s*=" /home/user/ggen/Cargo.toml
  # Should show only ONE declaration with optional = true
  ```

**Acceptance Criteria**:
- [x] Grep returns exactly 1 result: `ggen-dspy = { ... optional = true }`
- [x] No parse errors: `cargo metadata --format-version 1` succeeds

**Estimated Time**: 5 minutes

---

### Task 1.3: Fix Cargo.toml - Validate Optional Dependencies

**File**: `/home/user/ggen/Cargo.toml`
**Problem**: Some feature flags reference dependencies not marked optional
**Status**: 🔴 NOT STARTED

- [ ] **1.3.1**: Check `ggen-marketplace` optional status
  ```bash
  grep -A 2 "ggen-marketplace" /home/user/ggen/Cargo.toml | grep optional
  # Should show: optional = true
  ```

- [ ] **1.3.2**: If not optional, add `optional = true` to declaration
  ```toml
  # BEFORE:
  ggen-marketplace = { path = "crates/ggen-marketplace", version = "0.2.0" }

  # AFTER:
  ggen-marketplace = { path = "crates/ggen-marketplace", version = "0.2.0", optional = true }
  ```

- [ ] **1.3.3**: Verify all feature flag dependencies are optional
  ```bash
  # Feature definitions (lines 295-320 approx)
  # Each feature should reference only optional dependencies or other features
  ```

- [ ] **1.3.4**: Run cargo metadata validation
  ```bash
  timeout 15s cargo metadata --format-version 1 2>&1 | head -20
  # Should have no "feature includes" errors
  ```

**Acceptance Criteria**:
- [x] No "feature includes which is neither a dependency nor another feature" errors
- [x] `cargo metadata` completes successfully

**Estimated Time**: 10 minutes

---

### Task 1.4: Install cargo-make

**Tool**: `cargo-make` CLI utility
**Status**: 🔴 NOT INSTALLED
**Reason**: Makefile.toml requires `cargo make` command which dispatches to actual tasks

- [ ] **1.4.1**: Install cargo-make
  ```bash
  cargo install cargo-make --locked
  ```

- [ ] **1.4.2**: Verify installation and PATH
  ```bash
  which cargo-make  # Should show: /home/user/.cargo/bin/cargo-make
  cargo-make --version  # Should show version info
  ```

- [ ] **1.4.3**: Test basic cargo-make command
  ```bash
  cargo make --list | head -20
  # Should show available tasks from Makefile.toml
  ```

**Acceptance Criteria**:
- [x] `which cargo-make` returns a path
- [x] `cargo-make --version` succeeds
- [x] `cargo make --list` shows tasks

**Estimated Time**: 5 minutes

---

### Task 1.5: Verify Basic Compilation (Cargo.toml Fixed)

**Goal**: Verify that Cargo.toml fixes allow project to parse
**Status**: 🔴 NOT STARTED

- [ ] **1.5.1**: Run cargo metadata (no actual compilation)
  ```bash
  timeout 15s cargo metadata --format-version 1 > /tmp/metadata.json 2>&1
  echo "Exit code: $?"
  # Should be 0 (success)
  ```

- [ ] **1.5.2**: Check for parse errors
  ```bash
  cat /tmp/metadata.json | grep -i "error" || echo "No errors"
  ```

- [ ] **1.5.3**: Verify workspace resolution
  ```bash
  cargo metadata --format-version 1 | jq '.workspace_members | length'
  # Should show: 40 (number of workspace members)
  ```

- [ ] **1.5.4**: Run syntax check only (no build)
  ```bash
  timeout 20s cargo check --lib --no-default-features 2>&1 | head -50
  # May time out but should not have parse errors
  ```

**Acceptance Criteria**:
- [x] No Cargo.toml parse errors
- [x] Workspace metadata loads successfully
- [x] Feature flags validate without conflicts

**Estimated Time**: 10 minutes

---

### PHASE 1 SIGN-OFF

After completing tasks 1.1-1.5:

- [ ] All Cargo.toml errors resolved
- [ ] cargo-make installed and working
- [ ] Project parses successfully (metadata succeeds)
- [ ] No compilation errors blocking progress

**Gate**: Before proceeding to Phase 2, verify:
```bash
timeout 60s cargo make check 2>&1 | tail -20
# Should not show parse errors, may show compilation ongoing/timeout
```

---

## PHASE 2: PERFORMANCE REMEDIATION (After Phase 1)

### Task 2.1: Diagnose File Lock Contention

**Goal**: Understand why compilation takes 120s+ instead of 5-10s
**Status**: 🟡 PENDING (blocked by Phase 1)

- [ ] **2.1.1**: Clean all artifacts
  ```bash
  cargo clean
  ```

- [ ] **2.1.2**: Time a single check operation with verbose output
  ```bash
  time cargo check --lib 2>&1 | tee /tmp/check_output.log
  ```

- [ ] **2.1.3**: Analyze lock contention messages
  ```bash
  grep -i "waiting for file lock" /tmp/check_output.log
  # Note: If found, this is root cause of slowness
  ```

- [ ] **2.1.4**: Check for rustc parallelization
  ```bash
  # During build, in separate terminal:
  watch -n 1 'ps aux | grep rustc | wc -l'
  # Should show 15-20 rustc processes if parallelization working
  ```

**Findings to Document**:
- [ ] How many lock wait messages observed?
- [ ] Total build time on clean checkout?
- [ ] Peak memory usage (from `ps` output)?
- [ ] CPU utilization (from `top` during build)?

**Estimated Time**: 20 minutes

---

### Task 2.2: Measure Actual vs Target Performance

**Goal**: Get baseline measurements for all SLOs
**Status**: 🟡 PENDING (blocked by Phase 1)

**Measurements to Take** (use `time` command):

- [ ] **2.2.1**: Full check timeout
  ```bash
  time timeout 90s cargo make check 2>&1 | tail -10
  ```
  **Expected**: Should complete within 60s (current SLO), or show file lock issues

- [ ] **2.2.2**: Quick unit tests
  ```bash
  time cargo make test-unit 2>&1 | tail -10
  ```
  **Expected**: <150s per SLO documentation

- [ ] **2.2.3**: Linting pass
  ```bash
  time cargo make lint 2>&1 | tail -10
  ```
  **Expected**: <90s per Makefile.toml

- [ ] **2.2.4**: Pre-commit gate
  ```bash
  time cargo make pre-commit 2>&1 | tail -10
  ```
  **Expected**: <150s actual vs 395s sequential

**Create Performance Baseline Document**:
```markdown
# Performance Baseline (2026-01-25)

| Operation | Target | Actual | Status |
|-----------|--------|--------|--------|
| cargo make check | 10s | ??? | 🟡 PENDING |
| cargo make test-unit | 150s | ??? | 🟡 PENDING |
| cargo make lint | 90s | ??? | 🟡 PENDING |
| cargo make pre-commit | 150s | ??? | 🟡 PENDING |
```

**Estimated Time**: 30 minutes (includes waiting for builds)

---

### Task 2.3: Identify Performance Root Causes

**Goal**: Determine why SLOs are exceeded
**Status**: 🟡 PENDING (blocked by Phase 1 + 2.1-2.2)

Based on findings from 2.1-2.2, identify:

- [ ] **2.3.1**: Is bottleneck file locks?
  - [ ] If YES → Enable sccache or mold linker
  - [ ] If NO → Check CPU/memory bottleneck

- [ ] **2.3.2**: Is bottleneck memory?
  - [ ] If YES → Feature-gate large optional crates (ggen-ai: 2.6M, ggen-dspy: 439K)
  - [ ] If NO → Check CPU or I/O bottleneck

- [ ] **2.3.3**: Is bottleneck CPU?
  - [ ] If YES → Reduce `codegen-units` to 16-32
  - [ ] If NO → Check compilation dependency chain

- [ ] **2.3.4**: Root cause analysis (5 Whys)
  ```
  Why is cargo check taking 120s?
  → Because of file lock contention

  Why file lock contention?
  → 30 workspace crates competing for package cache

  Why multiple crates competing?
  → Parallel cargo builds but serialized by lock

  Why serialized?
  → Cargo lock implementation, not our issue

  Why accept this for production?
  → We can't - need feature-gating or sccache
  ```

**Estimated Time**: 15 minutes (analysis only)

---

### PHASE 2 SIGN-OFF

After completing tasks 2.1-2.3:

- [ ] Performance baseline measured
- [ ] Root causes identified
- [ ] Remediation strategy defined

**Gate**: Before Phase 3, decide on performance fixes:
- Option A: Enable sccache (simple, external dependency)
- Option B: Feature-gate optional crates (more complex, cleaner)
- Option C: Accept current performance with documentation

---

## PHASE 3: CODE QUALITY & SECURITY

### Task 3.1: Fix Unwrap/Expect Violations

**Goal**: Remove unguarded `expect()` calls from production code
**Status**: 🟡 PENDING

**Violations to Fix**:

1. **`crates/ggen-core/src/audit/mod.rs`**
   ```rust
   // BEFORE:
   let json = audit.to_json().expect("Failed to serialize");

   // AFTER (option 1 - propagate error):
   let json = audit.to_json()?;

   // AFTER (option 2 - handle error):
   let json = audit.to_json()
     .map_err(|e| AuditError::SerializationFailed(e.to_string()))?;
   ```

2. **`crates/ggen-core/src/audit/writer.rs` (3 violations)**
   ```rust
   // BEFORE:
   let temp_dir = TempDir::new().expect("Failed to create temp dir");

   // AFTER:
   let temp_dir = TempDir::new()
     .map_err(|e| AuditError::IoFailed(e.to_string()))?;
   ```

- [ ] **3.1.1**: Fix `audit/mod.rs` serialization
- [ ] **3.1.2**: Fix `audit/writer.rs` temp dir creation
- [ ] **3.1.3**: Fix `audit/writer.rs` write operation
- [ ] **3.1.4**: Fix `audit/writer.rs` read operation
- [ ] **3.1.5**: Verify no new violations introduced
  ```bash
  grep -r "\.expect(" crates/ggen-core/src --include="*.rs" | \
    grep -v "test\|#\[allow(clippy::expect_used)\]" | wc -l
  # Should be: 0 (no violations)
  ```

**Acceptance Criteria**:
- [x] All 4 violations fixed with proper error handling
- [x] `cargo make lint` passes (no clippy warnings)
- [x] `cargo make test` passes (all tests green)

**Estimated Time**: 30 minutes

---

### Task 3.2: Run Security Audit

**Goal**: Check for known vulnerabilities in dependencies
**Status**: 🔴 NOT STARTED

- [ ] **3.2.1**: Install cargo-audit
  ```bash
  cargo install cargo-audit
  ```

- [ ] **3.2.2**: Run full audit
  ```bash
  cargo audit 2>&1 | tee /tmp/audit_report.txt
  ```

- [ ] **3.2.3**: Check for vulnerabilities
  ```bash
  grep -i "vulnerability\|error" /tmp/audit_report.txt || echo "No issues"
  ```

- [ ] **3.2.4**: If vulnerabilities found:
  - [ ] Update dependency versions
  - [ ] Re-run audit
  - [ ] Repeat until clean

**Acceptance Criteria**:
- [x] `cargo audit` completes successfully
- [x] No vulnerabilities reported (or documented exceptions)

**Estimated Time**: 10-30 minutes (depending on vulnerabilities)

---

### Task 3.3: Update CLAUDE.md Documentation

**Goal**: Fix inaccurate information in project instructions
**Status**: 🟡 PENDING

- [ ] **3.3.1**: Update workspace member count
  ```
  FROM: "30 total crates"
  TO: "51 crate directories"
  ```

- [ ] **3.3.2**: Update CI workflow count
  ```
  FROM: "18 GitHub Actions workflows"
  TO: "34+ active workflows"
  ```

- [ ] **3.3.3**: Add actual vs target SLO callout
  ```markdown
  ### Current Performance Status (2026-01-25)
  ⚠️ Note: SLOs currently exceeded. See PRODUCTION_VALIDATION_REPORT_2026-01-25.md for details.
  ```

- [ ] **3.3.4**: Document known issues section
  ```markdown
  ### Known Issues
  - File lock contention during parallel builds
  - Performance SLOs exceeded 12-24x (remediation in progress)
  - See PRODUCTION_VALIDATION_REPORT_2026-01-25.md for details
  ```

**Estimated Time**: 15 minutes

---

### PHASE 3 SIGN-OFF

After completing tasks 3.1-3.3:

- [ ] All unwrap/expect violations fixed
- [ ] Security audit passed
- [ ] Documentation updated and accurate

**Gate**: Verify:
```bash
cargo make pre-commit 2>&1 | tail -20
# Should show: check ✓, lint ✓, test-unit ✓, all green
```

---

## PHASE 4: VALIDATION & TESTING

### Task 4.1: Comprehensive Build Validation

**Goal**: Verify complete build process works end-to-end
**Status**: 🟡 PENDING (blocked by Phase 1-3)

- [ ] **4.1.1**: Clean build from scratch
  ```bash
  cargo clean
  timeout 120s cargo build --lib 2>&1 | tail -30
  # Should complete successfully
  ```

- [ ] **4.1.2**: Incremental rebuild
  ```bash
  # Modify one file in ggen-core
  touch crates/ggen-core/src/lib.rs

  time cargo build --lib 2>&1 | tail -5
  # Should be faster than clean build
  ```

- [ ] **4.1.3**: Full test suite
  ```bash
  timeout 150s cargo make test 2>&1 | tail -20
  # All tests should pass
  ```

- [ ] **4.1.4**: Release build
  ```bash
  timeout 180s cargo build --release --bin ggen 2>&1 | tail -20
  # Should complete and create binary
  ```

**Acceptance Criteria**:
- [x] Clean build completes without errors
- [x] Incremental build faster than clean
- [x] All tests pass
- [x] Release binary created successfully

**Estimated Time**: 45 minutes

---

### Task 4.2: Platform Compatibility Testing

**Goal**: Verify build works across platforms
**Status**: 🟡 PENDING (blocked by Phase 1-3)

**On Linux (current)**:
- [ ] **4.2.1**: Verify target triplet
  ```bash
  rustc --version --verbose | grep host
  # Example: x86_64-unknown-linux-gnu
  ```

- [ ] **4.2.2**: Build for Linux
  ```bash
  cargo build --release --target x86_64-unknown-linux-gnu
  ```

**On macOS** (if available):
- [ ] **4.2.3**: Build for macOS (x86_64)
  ```bash
  cargo build --release --target x86_64-apple-darwin
  ```

- [ ] **4.2.4**: Build for macOS (Apple Silicon)
  ```bash
  cargo build --release --target aarch64-apple-darwin
  ```

**On Windows** (if available):
- [ ] **4.2.5**: Build for Windows
  ```bash
  cargo build --release --target x86_64-pc-windows-msvc
  ```

**Acceptance Criteria**:
- [x] At least Linux target builds successfully
- [x] Other targets build without platform-specific errors

**Estimated Time**: 30 minutes (may vary by platform availability)

---

### Task 4.3: CI Workflow Verification

**Goal**: Run all GitHub Actions workflows locally to verify compatibility
**Status**: 🟡 PENDING (blocked by Phase 1-3)

- [ ] **4.3.1**: List all workflows
  ```bash
  ls .github/workflows/*.yml | wc -l
  # Should show: 34+ workflows
  ```

- [ ] **4.3.2**: Test critical workflows locally (using `act`)
  ```bash
  # Install act if needed
  act --list | head -20
  # Shows available workflows
  ```

- [ ] **4.3.3**: Run core workflows
  ```bash
  act --job build         # Run build job
  act --job test          # Run test job
  act --job security      # Run security job
  ```

- [ ] **4.3.4**: Document any failures
  - [ ] If failures found, create issues to track
  - [ ] If fixable, apply fixes
  - [ ] Re-run until passing

**Acceptance Criteria**:
- [x] All critical workflows pass locally
- [x] No platform-specific failures
- [x] Workflows complete within reasonable time

**Estimated Time**: 45 minutes

---

### Task 4.4: Performance Receipt Generation

**Goal**: Create documented evidence of SLO compliance
**Status**: 🟡 PENDING (blocked by Phase 1-3)

- [ ] **4.4.1**: Run benchmark suite
  ```bash
  timeout 300s cargo make bench 2>&1 | tee /tmp/bench_results.txt
  ```

- [ ] **4.4.2**: Extract performance metrics
  ```bash
  # Generate report with:
  # - Build times (check, test, lint, pre-commit)
  # - Memory usage
  # - Test execution times
  # - Benchmark results
  ```

- [ ] **4.4.3**: Create performance receipt document
  ```markdown
  # Performance Receipt (2026-01-25)

  ## Build Times
  - First build: XXs
  - Incremental: XXs
  - check: XXs
  - test: XXs
  - lint: XXs
  - pre-commit: XXs

  ## SLO Compliance
  - First build ≤ 15s: ❌/✅
  - Incremental ≤ 2s: ❌/✅
  - Memory ≤ 100MB: ❌/✅

  ## SHA256 Hashes
  - Cargo.lock: XXXXX
  - Source trees: XXXXX
  ```

- [ ] **4.4.4**: Archive receipt in project
  ```bash
  cp /tmp/PERFORMANCE_RECEIPT_2026-01-25.md docs/receipts/
  ```

**Acceptance Criteria**:
- [x] Performance metrics measured and documented
- [x] SLO compliance status clear (met or not met)
- [x] Receipts archived for production record

**Estimated Time**: 30 minutes

---

### PHASE 4 SIGN-OFF

After completing tasks 4.1-4.4:

- [ ] Complete build validation passed
- [ ] Platform compatibility verified
- [ ] CI workflows passing
- [ ] Performance receipts documented

**Final Gate - Production Readiness Checklist**:

```
✅ PHASE 1: CRITICAL BLOCKERS
  ✅ Cargo.toml errors fixed
  ✅ cargo-make installed
  ✅ Project compiles

✅ PHASE 2: PERFORMANCE (Optional if blockers resolved)
  ✅ File lock contention diagnosed
  ✅ Performance baseline measured
  ✅ Root causes identified

✅ PHASE 3: CODE QUALITY & SECURITY
  ✅ Unwrap/expect violations fixed
  ✅ Security audit passed
  ✅ Documentation updated

✅ PHASE 4: VALIDATION & TESTING
  ✅ Complete build validation passed
  ✅ Platform compatibility verified
  ✅ CI workflows passing
  ✅ Performance receipts documented

🟢 READY FOR PRODUCTION DEPLOYMENT
```

---

## Final Completion Sign-Off

**When all 4 phases complete**:

1. Create summary commit message:
```
feat(build): Production validation remediation complete

PHASE 1: Fixed Cargo.toml feature flag errors + cargo-make install
PHASE 2: [Performance improvements] (if applicable)
PHASE 3: Fixed production code violations + security audit passed
PHASE 4: Full validation and testing completed

All Andon signals clear. Performance SLOs [met/documented/in-remediation].
Ready for production deployment.

Validation Report: PRODUCTION_VALIDATION_REPORT_2026-01-25.md
Checklist: PRODUCTION_VALIDATION_REMEDIATION_CHECKLIST.md
```

2. Tag commit with validation evidence:
```bash
git tag -a v0.2.0-prod-validated-2026-01-25 -m "Production validation complete"
```

3. Merge to main with evidence-based PR

---

**Document Version**: 1.0
**Created**: 2026-01-25
**Status**: IN PROGRESS - Phase 1 Starting
**Next Review**: After Phase 1 completion
