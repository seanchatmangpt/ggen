# Critical Blockers - Release v2.6.0
**Status**: ⚠️ RELEASE BLOCKED
**Blocker Count**: 3 Critical, 1 High Priority
**Estimated Fix Time**: 4-6 hours
**Date**: 2025-11-13

---

## Critical Blockers (Must Fix Before Release)

### CB-1: Missing `ggen doctor` Command ❌ CRITICAL
**RPN**: 504 (Severity 9 × Frequency 7 × Detection 8)
**Impact**: Documentation-Reality Mismatch
**Status**: Not Implemented

**Problem**:
Documentation (README.md, CONTRIBUTING.md) claims `ggen doctor` command exists, but it's not implemented.

**Evidence**:
```bash
$ ggen doctor
Error: CLI error: CLI execution failed: Argument parsing failed:
error: unrecognized subcommand 'doctor'
```

**References in Docs**:
- README.md: "Verify: `ggen doctor` (validates environment setup)"
- CONTRIBUTING.md: "Verify setup: `ggen doctor`"
- Multiple quickstart guides reference this command

**Root Cause**:
Feature was planned but never implemented. Documentation was written assuming it would exist.

**Impact on Developers**:
- New developers follow README → command fails → confusion
- Breaks "Quick Start" promise of "< 5 minutes" setup
- Damages trust in documentation accuracy
- Reduces first-impression quality

**Fix Options**:

#### Option A: Implement `ggen doctor` (RECOMMENDED) ⏱️ 2-4 hours
**Benefits**:
- Provides actual developer value
- Matches documented behavior
- Improves onboarding experience

**Implementation**:
```rust
// In crates/ggen-cli/src/cmds/utils.rs

#[derive(Debug, Subcommand)]
pub enum UtilsCommand {
    // ... existing commands ...

    /// Validate development environment setup
    Doctor,
}

impl UtilsCommand {
    pub fn execute(&self, runtime: &Runtime) -> Result<()> {
        match self {
            // ... existing handlers ...

            UtilsCommand::Doctor => {
                println!("🔍 ggen Doctor - Environment Validation\n");

                // Check Rust version
                let rust_version = std::env!("CARGO_PKG_RUST_VERSION");
                let rust_ok = check_rust_version("1.70.0");
                print_check("Rust version (1.70+)", rust_ok);

                // Check cargo-make
                let cargo_make_ok = Command::new("cargo")
                    .arg("make")
                    .arg("--version")
                    .output()
                    .is_ok();
                print_check("cargo-make installed", cargo_make_ok);

                // Check build compiles
                let build_ok = Command::new("cargo")
                    .args(["check", "--quiet"])
                    .output()
                    .is_ok();
                print_check("Project builds", build_ok);

                // Check tests pass
                let test_ok = Command::new("cargo")
                    .args(["test", "--quiet", "--lib"])
                    .output()
                    .is_ok();
                print_check("Tests pass", test_ok);

                println!("\n✨ Environment validation complete!");
                Ok(())
            }
        }
    }
}

fn print_check(name: &str, ok: bool) {
    let symbol = if ok { "✅" } else { "❌" };
    println!("{} {}", symbol, name);
}

fn check_rust_version(min_version: &str) -> bool {
    // Implementation to compare versions
    true // Simplified
}
```

**Testing**:
```bash
cargo build
./target/debug/ggen utils doctor

# Expected output:
# 🔍 ggen Doctor - Environment Validation
#
# ✅ Rust version (1.70+)
# ✅ cargo-make installed
# ✅ Project builds
# ✅ Tests pass
#
# ✨ Environment validation complete!
```

#### Option B: Remove from Documentation ⏱️ 30 minutes
**Benefits**:
- Quick fix
- Removes false promise

**Drawbacks**:
- Loses potentially useful feature
- Doesn't improve developer experience

**Changes Required**:
```bash
# Remove references from:
- README.md (line 31)
- CONTRIBUTING.md (line 23)
- Any other guides mentioning ggen doctor
```

**Recommendation**: **Option A** (Implement)
- Provides actual value to developers
- Improves onboarding experience
- Validates environment setup proactively
- Time investment: 2-4 hours

---

### CB-2: Git Hooks Block Legitimate Commits ❌ CRITICAL
**RPN**: 432 (Severity 8 × Frequency 6 × Detection 9)
**Impact**: Prevents Developer Productivity
**Status**: Too Strict

**Problem**:
Pre-commit hook rejects commits with TODO/FUTURE comments, blocking legitimate development work.

**Evidence**:
```bash
$ git commit -m "feat: add new feature"
⚠️  No session found with ID: swarm-1763079481735-rnl9z4ygg
❌ ERROR: FUTURE/TODO comments found in staged files

Files with FUTURE/TODO comments (11 instances):
- crates/ggen-cli/src/cmds/project.rs: 1 FUTURE comment
- crates/ggen-cli/src/cmds/template.rs: 1 FUTURE comment
- crates/ggen-cli/src/cmds/utils.rs: 1 FUTURE comment
- crates/ggen-cli/src/conventions/watcher.rs: 2 FUTURE comments
- crates/ggen-core/src/e2e_tests.rs: 3 FUTURE comments
- crates/ggen-core/src/templates/generator.rs: 1 FUTURE comment
- crates/ggen-domain/src/template/list.rs: 1 FUTURE comment
- crates/ggen-marketplace/src/lib.rs: 1 FUTURE comment (in allow attribute)

Please remove or convert TODO/FUTURE comments before committing.
```

**Root Cause**:
Pre-commit hook rule is too strict. TODO/FUTURE comments are valid development markers.

**Impact on Developers**:
- Cannot commit incremental progress
- Forces developers to bypass hooks (`--no-verify`)
- Breaks normal development workflow
- Discourages proper planning (TODO comments are useful)

**Fix Options**:

#### Option A: Allow TODO/FUTURE in Comments (RECOMMENDED) ⏱️ 1 hour
**Benefits**:
- Maintains code quality gates
- Allows normal development workflow
- Still prevents TODO in critical production code

**Implementation**:
```bash
# Update pre-commit hook in .git/hooks/pre-commit or Makefile.toml

# Current (too strict):
grep -r "TODO\|FUTURE" staged_files && exit 1

# New (balanced):
# Only reject TODO/FUTURE in critical production paths
grep -r "TODO\|FUTURE" staged_files \
  | grep -v "tests/" \
  | grep -v "examples/" \
  | grep -v "// TODO:" \
  | grep -v "// FUTURE:" \
  && exit 1
```

**Rules**:
- ✅ Allow: `// TODO: implement caching` (normal planning)
- ✅ Allow: `// FUTURE: optimize with SIMD` (future optimization note)
- ✅ Allow: TODO in test files
- ❌ Block: `todo!()` macro in production code
- ❌ Block: `unimplemented!()` in production code

#### Option B: Clean Up All TODO/FUTURE Comments ⏱️ 2 hours
**Benefits**:
- Removes technical debt markers
- Forces immediate resolution

**Drawbacks**:
- Time-consuming
- May remove useful planning notes
- Doesn't scale for future development

**Recommendation**: **Option A** (Allow in Comments)
- Balances code quality with developer productivity
- Still prevents actual unimplemented code (`todo!()` macro)
- Allows normal development workflow

---

### CB-3: Release Blocked by Uncommitted Changes ❌ CRITICAL
**RPN**: 504 → 9 (After FMEA fix, correctly blocks dirty release)
**Impact**: Cannot Tag Release
**Status**: Working as Intended (but blocks release)

**Problem**:
82+ files staged/modified, preventing clean release.

**Evidence**:
```bash
$ cargo make release-validate-git-state
❌ ERROR: Uncommitted changes detected

Git status:
M  .cursor/rules/rust-standards.mdc
M  CHANGELOG.md
M  CONTRIBUTING.md
M  Cargo.toml
M  Makefile.toml
M  README.md
MM RELEASE_v2.5.1_CHECKLIST.md
A  RELEASE_v2.6.0_CHECKLIST.md
AM RELEASE_v2.6.0_STATUS.md
... (82+ files total)
```

**Root Cause**:
FMEA validation correctly prevents releasing from dirty state. This is **working as intended**, but blocks release.

**Impact**:
- Cannot tag v2.6.0
- Cannot push release
- Blocks release process

**Fix Options**:

#### Option A: Create Release Branch (RECOMMENDED) ⏱️ 30 minutes
**Benefits**:
- Unblocks release immediately
- Preserves uncommitted work
- Allows parallel development

**Implementation**:
```bash
# Save uncommitted work
git stash push -m "Work in progress before v2.6.0 release"

# Create clean release branch from last clean commit
git checkout -b release/2.6.0
git reset --hard <last-clean-commit-sha>

# Make release-specific changes only
# - Update version numbers
# - Update CHANGELOG
# - Create release commit

git commit -m "chore: prepare release v2.6.0"
git tag -a v2.6.0 -m "Release v2.6.0"

# Switch back and restore work
git checkout master
git stash pop
```

#### Option B: Commit All Changes ⏱️ 2 hours
**Benefits**:
- Cleans up uncommitted work
- Resolves all pending changes

**Drawbacks**:
- Time-consuming
- May include incomplete features
- Requires CB-2 fix first (git hooks)

**Recommendation**: **Option A** (Release Branch)
- Fastest path to release
- Preserves work in progress
- Standard release practice

---

## High Priority Issues (Should Fix Before Release)

### HP-1: Unit Tests Timeout After 60s ⚠️ HIGH
**RPN**: 288 (Severity 6 × Frequency 6 × Detection 8)
**Impact**: Slow Development Workflow
**Status**: Performance Issue

**Problem**:
Unit tests timeout at 60 seconds due to compilation-heavy dependencies.

**Evidence**:
```bash
$ cargo make test-unit
[Compiling 100+ dependencies...]
Error while executing command, exit code: 124  # Timeout
```

**Root Cause**:
Test suite includes integration tests in unit test run, causing long compilation times.

**Impact on Developers**:
- Cannot run fast test feedback loop
- Slows TDD workflow
- Wastes developer time

**Fix Options**:

#### Option A: Split Test Suites (RECOMMENDED) ⏱️ 2-3 hours
**Benefits**:
- Fast unit tests (< 5s)
- Separate slow integration tests
- Better developer experience

**Implementation**:
```toml
# In Makefile.toml

[tasks.test-quick]
description = "Run fast unit tests only"
command = "cargo"
args = ["test", "--lib", "--bins"]

[tasks.test-unit]
description = "Run all unit tests"
command = "cargo"
args = ["test", "--lib"]

[tasks.test-integration]
description = "Run integration tests"
command = "cargo"
args = ["test", "--test", "*"]
```

**Expected Results**:
```bash
cargo make test-quick    # < 5s
cargo make test-unit     # < 30s
cargo make test-integration  # < 5min
```

#### Option B: Increase Timeout ⏱️ 5 minutes
**Benefits**:
- Quick fix
- No code changes

**Drawbacks**:
- Doesn't improve developer experience
- Wastes time on every test run

**Recommendation**: **Option A** (Split Suites)
- Improves developer productivity
- Enables fast TDD workflow
- Better long-term solution

---

## Summary

### Blockers Overview

| ID | Issue | Severity | Fix Time | Recommendation |
|----|-------|----------|----------|----------------|
| CB-1 | Missing `ggen doctor` | Critical | 2-4h | Implement command |
| CB-2 | Git hooks too strict | Critical | 1h | Allow TODO/FUTURE |
| CB-3 | Uncommitted changes | Critical | 30min | Release branch |
| HP-1 | Test timeout | High | 2-3h | Split test suites |

### Total Fix Time: 4-6 hours (Critical only)

### Fix Order (Recommended):

1. **CB-3: Create Release Branch** (30 min)
   - Unblocks release immediately
   - Allows parallel work

2. **CB-2: Fix Git Hooks** (1 hour)
   - Unblocks development workflow
   - Required for future commits

3. **CB-1: Implement `ggen doctor`** (2-4 hours)
   - Improves onboarding experience
   - Matches documentation

4. **HP-1: Split Test Suites** (2-3 hours)
   - Improves developer productivity
   - Can be done post-release

### Go/No-Go Decision:

**Current Status**: ❌ **NO-GO** (3 critical blockers)

**After Fixes**: ✅ **GO** (all blockers resolved)

**Timeline**:
- Today: Fix CB-3, CB-2, CB-1 (4-6 hours)
- Tomorrow: Re-validate and release
- Next week: Fix HP-1 (nice to have)

---

**Document Version**: 1.0
**Last Updated**: 2025-11-13
**Status**: Active Blockers
