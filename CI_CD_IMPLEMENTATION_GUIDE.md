# CI/CD Bulletproof Implementation Guide

**Quick Start**: How to make ggen's CI/CD bulletproof in 5 days

---

## 🚨 DAY 1: FIX CRITICAL BLOCKER (MUST DO FIRST)

### Problem: Project Doesn't Compile

**File**: `Cargo.toml:79`
```toml
chicago-tdd-tools = { path = "/Users/sac/chicago-tdd-tools", version = "1.1.0" }
```

### Solution Options (Choose One)

#### Option A: Publish to crates.io (RECOMMENDED)

```bash
# 1. Navigate to chicago-tdd-tools directory
cd /path/to/chicago-tdd-tools

# 2. Ensure Cargo.toml is ready for publishing
cargo publish --dry-run

# 3. Publish to crates.io
cargo publish

# 4. Update ggen's Cargo.toml
# Replace line 79 with:
chicago-tdd-tools = "1.1.0"

# 5. Verify it works
cargo clean
cargo build --workspace
```

**Verification**:
```bash
# Fresh clone should work
git clone https://github.com/jmanhype/ggen /tmp/test-ggen
cd /tmp/test-ggen
cargo build  # Should succeed
```

#### Option B: Use Git Dependency

```toml
# In Cargo.toml, replace line 79 with:
chicago-tdd-tools = { git = "https://github.com/seanchatmangpt/chicago-tdd-tools", tag = "v1.1.0" }
```

**Pros**: Quick fix, no publishing needed
**Cons**: Slower builds, dependency on GitHub

#### Option C: Make It Optional

```toml
[dependencies]
chicago-tdd-tools = { version = "1.1.0", optional = true }

[features]
default = []
chicago_tdd = ["chicago-tdd-tools"]
```

Then update all usage to check feature flag.

---

## 📋 DAY 2: ENABLE NEW QUALITY GATES

### Step 1: Add New Workflow

The `quality-gates.yml` workflow has been created. Enable it:

```bash
# 1. Review the workflow
cat .github/workflows/quality-gates.yml

# 2. Commit and push
git add .github/workflows/quality-gates.yml
git commit -m "ci: add comprehensive quality gates workflow"
git push origin your-branch
```

### Step 2: Configure Branch Protection

Go to GitHub Settings → Branches → Add Rule for `main`:

```yaml
Branch name pattern: main

Require a pull request before merging:
  ☑ Require approvals: 2
  ☑ Dismiss stale reviews
  ☑ Require review from Code Owners

Require status checks to pass:
  ☑ Require branches to be up to date before merging

  Required checks:
    ☑ GATE 1: No Panic Points
    ☑ GATE 2: Clippy Strict
    ☑ GATE 3: Code Coverage ≥80%
    ☑ GATE 4: No Hardcoded Paths
    ☑ GATE 5: All Tests Pass
    ☑ GATE 6: Build All Platforms
    ☑ Quality Gates Summary

Require signed commits:
  ☑ Enabled

Do not allow bypassing the above settings:
  ☑ Enabled
```

### Step 3: Test the Gates

Create a test PR with intentional violations:

```bash
# Create test branch
git checkout -b test/quality-gates

# Add a panic point to production code
echo "panic!(\"test\");" >> crates/ggen-core/src/lib.rs

# Commit and push
git add .
git commit -m "test: trigger quality gate failure"
git push origin test/quality-gates
```

**Expected**: PR should FAIL Gate 1 (Panic Point Detection)

Fix it:
```bash
# Remove the panic
git checkout crates/ggen-core/src/lib.rs
git commit -am "fix: remove panic point"
git push
```

**Expected**: PR should now PASS all gates

---

## 🧹 DAY 3: CLEANUP & OPTIMIZATION

### Delete Obsolete Workflows

```bash
# P2P was removed in v2.6.0, so delete P2P workflows
git rm .github/workflows/p2p-marketplace-ci.yml
git rm .github/workflows/p2p-release.yml

# Commit
git commit -m "ci: remove obsolete P2P workflows (P2P removed in v2.6.0)"
```

### Consolidate Overlapping Workflows

Current state:
- `ci.yml` - Main CI
- `test.yml` - Testing
- `build.yml` - Build testing

These overlap significantly. Choose one approach:

#### Option A: Keep ci.yml, enhance it

```bash
# Delete redundant workflows
git rm .github/workflows/test.yml
git rm .github/workflows/build.yml

# Update ci.yml to include all checks
# (Edit .github/workflows/ci.yml to merge functionality)
```

#### Option B: Use quality-gates.yml as the main CI

```bash
# Rename quality-gates.yml to ci.yml
mv .github/workflows/quality-gates.yml .github/workflows/ci-main.yml

# Archive old workflows
mkdir .github/workflows/archived
git mv .github/workflows/ci.yml .github/workflows/archived/
git mv .github/workflows/test.yml .github/workflows/archived/
git mv .github/workflows/build.yml .github/workflows/archived/
```

---

## 🐛 DAY 4: FIX PRODUCTION PANIC POINTS

### Current State

11 panic points found in production code:
1. `crates/ggen-core/src/graph/types.rs` - 8 panics
2. `crates/ggen-core/src/template.rs` - 2 panics
3. `crates/ggen-ai/src/governance/mod.rs` - 1 panic

### Automated Fix

```bash
# Run the panic point fixer script (if it exists)
cargo run --example fix-panic-points -- --dry-run

# Review the changes
cargo run --example fix-panic-points

# Or manually fix (see guide below)
```

### Manual Fix Guide

#### File 1: `crates/ggen-core/src/graph/types.rs`

**Before** (lines 109, 131, 151, etc.):
```rust
match result {
    CachedResult::Boolean(b) => assert!(b),
    _ => panic!("Expected Boolean variant"),
}
```

**After**:
```rust
match result {
    CachedResult::Boolean(b) => Ok(b),
    other => Err(anyhow::anyhow!(
        "Expected Boolean variant, got {:?}",
        std::mem::discriminant(&other)
    )),
}
```

#### File 2: `crates/ggen-core/src/template.rs`

**Before** (line 837):
```rust
panic!("Template parsing is not idempotent");
```

**After**:
```rust
return Err(TemplateError::NonIdempotentParsing {
    first: template_first.clone(),
    second: template_second.clone(),
}.into());
```

**Before** (line 879):
```rust
panic!("Expected path to be preserved, but got None");
```

**After**:
```rust
return Err(TemplateError::PathNotPreserved {
    template_name: template.name.clone(),
}.into());
```

#### File 3: `crates/ggen-ai/src/governance/mod.rs`

**Before** (line 230):
```rust
panic!("Governance decision failed: {:?}", error);
```

**After**:
```rust
return Err(GovernanceError::DecisionFailed {
    reason: error.to_string(),
}.into());
```

### Add Error Types

Create error types for each module:

```rust
// In crates/ggen-core/src/graph/types.rs
#[derive(Debug, thiserror::Error)]
pub enum CachedResultError {
    #[error("Expected {expected} variant, got {actual}")]
    VariantMismatch {
        expected: &'static str,
        actual: String,
    },
}

// In crates/ggen-core/src/template.rs
#[derive(Debug, thiserror::Error)]
pub enum TemplateError {
    #[error("Template parsing is not idempotent")]
    NonIdempotentParsing {
        first: String,
        second: String,
    },

    #[error("Template path not preserved for {template_name}")]
    PathNotPreserved {
        template_name: String,
    },
}
```

### Test the Fixes

```bash
# Build to ensure no compilation errors
cargo build --workspace

# Run tests to ensure no breakage
cargo test --workspace

# Run quality gates
cargo clippy --workspace --all-targets -- \
  -D clippy::unwrap_used \
  -D clippy::expect_used \
  -D clippy::panic

# Should pass with 0 warnings
```

---

## 📊 DAY 5: ENABLE COVERAGE ENFORCEMENT

### Step 1: Generate Baseline Coverage

```bash
# Install tarpaulin
cargo install cargo-tarpaulin

# Generate coverage report
cargo tarpaulin --workspace --out Html --output-dir coverage

# Open coverage/index.html to see current coverage
```

### Step 2: Identify Low Coverage Areas

Look for files with <80% coverage:
- Red: 0-50% coverage (critical)
- Yellow: 50-80% coverage (needs work)
- Green: 80-100% coverage (good)

### Step 3: Add Tests for Critical Paths

Focus on the 11 untested panic paths from the analysis:

```rust
// Example test for graph/types.rs
#[test]
fn test_cached_result_variant_mismatch_returns_error() {
    let result = CachedResult::Boolean(true);

    // Try to convert to wrong variant - should return Error, not panic
    match try_as_solutions(&result) {
        Ok(_) => panic!("Should have returned error"),
        Err(e) => {
            assert!(e.to_string().contains("Expected Solutions variant"));
        }
    }
}
```

### Step 4: Configure Codecov

Create `.codecov.yml`:
```yaml
coverage:
  status:
    project:
      default:
        target: 80%
        threshold: 0%  # No coverage decrease allowed
    patch:
      default:
        target: 80%
        threshold: 0%

comment:
  layout: "header, diff, files"
  behavior: default

ignore:
  - "tests/"
  - "examples/"
  - "**/tests.rs"
```

### Step 5: Test Coverage Enforcement

```bash
# This should fail if coverage < 80%
cargo tarpaulin --workspace --fail-under 80

# Fix by adding tests until coverage ≥ 80%
```

---

## 🎯 VERIFICATION CHECKLIST

After completing all 5 days, verify:

### Day 1: Critical Blocker Fixed
- [ ] Fresh clone builds: `git clone <repo> /tmp/test && cd /tmp/test && cargo build`
- [ ] CI builds pass: Check GitHub Actions
- [ ] No hardcoded paths: `grep -r "path = \"/Users/" Cargo.toml`

### Day 2: Quality Gates Enabled
- [ ] `quality-gates.yml` workflow exists
- [ ] Branch protection configured
- [ ] Test PR fails with panic point
- [ ] Test PR passes after fixing panic

### Day 3: Workflows Cleaned
- [ ] Obsolete workflows deleted
- [ ] No duplicate CI jobs
- [ ] All workflows documented

### Day 4: Panic Points Fixed
- [ ] Zero panics in production: `cargo clippy -- -D clippy::panic`
- [ ] Zero unwraps in production: `cargo clippy -- -D clippy::unwrap_used`
- [ ] Zero expects in production: `cargo clippy -- -D clippy::expect_used`
- [ ] All tests pass: `cargo test --workspace`

### Day 5: Coverage Enforced
- [ ] Coverage ≥ 80%: `cargo tarpaulin --fail-under 80`
- [ ] Codecov configured
- [ ] Coverage badge in README
- [ ] Coverage trends visible

---

## 🚀 POST-IMPLEMENTATION

### Week 2: Monitor & Tune

1. **Watch for False Positives**
   - Quality gates too strict?
   - Flaky tests?
   - Legitimate SAFE patterns flagged?

2. **Adjust Thresholds**
   - Coverage target (80% → 85%?)
   - Benchmark regression (10% → 5%?)
   - Timeout values

3. **Add Performance Monitoring**
   - Track CI duration
   - Optimize slow jobs
   - Parallelize where possible

### Week 3: Advanced Features

1. **Add Benchmarking CI**
2. **Add Chaos Engineering**
3. **Add Canary Deployments**
4. **Add Observability Dashboards**

---

## 📚 Troubleshooting

### Problem: Quality Gates Fail on Main

**Cause**: Existing code has issues
**Solution**:
```bash
# Create remediation branch
git checkout -b fix/quality-gates-main

# Fix all issues (Day 4 guide)
# Test locally
cargo clippy -- -D warnings
cargo test --workspace

# Push and merge
git push origin fix/quality-gates-main
# Merge via PR
```

### Problem: Coverage Calculation Fails

**Cause**: Tarpaulin timeout or OOM
**Solution**:
```bash
# Increase timeout
cargo tarpaulin --timeout 600

# Reduce scope
cargo tarpaulin --lib  # Only library code

# Exclude heavy tests
cargo tarpaulin --exclude-files "**/integration/*"
```

### Problem: Windows Build Fails

**Cause**: Platform-specific issues
**Solution**:
```bash
# Test on Windows locally with cross
cargo install cross
cross build --target x86_64-pc-windows-msvc

# Or use Windows runner
# Update .github/workflows/quality-gates.yml matrix
```

---

## 🎓 Success Metrics

After implementation, you should see:

| Metric | Before | After | Target |
|--------|--------|-------|--------|
| Build Success Rate | Unknown | 95%+ | 98%+ |
| Code Coverage | 54% | 80%+ | 85%+ |
| Panic Points | 11 | 0 | 0 |
| CI Duration | ~30min | ~20min | <15min |
| False Positive Rate | N/A | <5% | <2% |
| Deployment Confidence | Low | High | Very High |

---

## 🔗 Next Steps

1. **Review** this guide with the team
2. **Schedule** 5-day implementation sprint
3. **Assign** tasks to team members
4. **Execute** day by day
5. **Monitor** and iterate

**When done**: You can deploy on Friday at 5pm with confidence! 🚀
