<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [🐝 SWARM PHASE 7: NEXT STEPS](#-swarm-phase-7-next-steps)
  - [Executive Summary](#executive-summary)
  - [Quick Validation](#quick-validation)
  - [Critical Path to 100% (Prioritized)](#critical-path-to-100-prioritized)
    - [Priority 1: Fix chicago_tdd_tools API Incompatibility (4-6 hours)](#priority-1-fix-chicago_tdd_tools-api-incompatibility-4-6-hours)
    - [Priority 2: Remove Tests from src/ Directories (2-3 hours)](#priority-2-remove-tests-from-src-directories-2-3-hours)
    - [Priority 3: Fix Integration Test Compilation (3-4 hours)](#priority-3-fix-integration-test-compilation-3-4-hours)
    - [Priority 4: Achieve 100% Pass Rate (2-4 hours)](#priority-4-achieve-100-pass-rate-2-4-hours)
  - [Effort Estimate](#effort-estimate)
  - [Success Criteria](#success-criteria)
  - [Alternative: Quick Win Strategy](#alternative-quick-win-strategy)
    - [Phase A: Fix ONE Module Completely (2 hours)](#phase-a-fix-one-module-completely-2-hours)
    - [Phase B: Apply Pattern to 5 More Modules (3 hours)](#phase-b-apply-pattern-to-5-more-modules-3-hours)
    - [Phase C: Declare "Critical 20%" Complete (15 min)](#phase-c-declare-critical-20-complete-15-min)
  - [Recommended Approach](#recommended-approach)
  - [Automation Scripts](#automation-scripts)
    - [Script 1: Count Remaining Work](#script-1-count-remaining-work)
    - [Script 2: Fix One File](#script-2-fix-one-file)
  - [Monitoring Progress](#monitoring-progress)
  - [Support](#support)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# 🐝 SWARM PHASE 7: NEXT STEPS

## Executive Summary

**Current Status: 50% Complete (33% Pass Rate)**

The ultrathink hive queen swarm created 8,130 lines of integration test code across 22 files, but critical gaps prevent execution:

- ✅ Library compilation: PASSING
- ❌ Test compilation: FAILING (600+ errors)
- ❌ Production code separation: FAILING (70 chicago_tdd_tools references)
- ⚠️ Integration tests: CREATED but don't compile
- ❌ Test execution: BLOCKED

**Recommendation: Complete the remaining 50% before deploying.**

---

## Quick Validation

Run this script anytime to check status:

```bash
./scripts/validate-swarm-status.sh
```

---

## Critical Path to 100% (Prioritized)

### Priority 1: Fix chicago_tdd_tools API Incompatibility (4-6 hours)

**Problem**: chicago_tdd_tools::test! macro expects `Result<(), Box<dyn Error>>` but code returns `Result<(), ggen_utils::Error>`.

**Solution**: Replace chicago_tdd_tools with standard Rust tests.

**Example Fix**:
```rust
// BEFORE (broken):
use chicago_tdd_tools::test;
test!(test_name, {
    // ... test code
    Ok(())  // Returns ggen_utils::Error
});

// AFTER (working):
#[test]
fn test_name() -> Result<(), Box<dyn std::error::Error>> {
    // ... test code
    Ok(())
}
```

**Automation Script**:
```bash
# Create script: scripts/fix-chicago-tdd-tools.sh
#!/usr/bin/env bash
# Convert chicago_tdd_tools tests to standard Rust tests

for file in $(grep -rl "chicago_tdd_tools::" crates/*/src/); do
    echo "Processing: $file"
    # Replace test! macro with #[test]
    sed -i '' 's/use chicago_tdd_tools::test;//g' "$file"
    sed -i '' 's/test!(\([^,]*\), {/#[test]\nfn \1() -> Result<(), Box<dyn std::error::Error>> {/g' "$file"
    sed -i '' 's/});/}/g' "$file"
done
```

---

### Priority 2: Remove Tests from src/ Directories (2-3 hours)

**Problem**: Tests were COPIED to tests/integration/ but NOT removed from src/.

**Solution**: Delete #[cfg(test)] modules from production code.

**Safe Deletion Process**:
```bash
# For each file with chicago_tdd_tools:
1. Verify equivalent test exists in tests/integration/
2. Delete #[cfg(test)] module from src/ file
3. Run: cargo check --lib --workspace
4. If passes, commit the change
5. If fails, restore and investigate
```

**Example**:
```rust
// File: crates/ggen-core/src/preprocessor.rs

// DELETE THIS ENTIRE SECTION:
#[cfg(test)]
mod tests {
    use chicago_tdd_tools::test;

    test!(test_preprocessor, {
        // ... 50 lines of test code
        Ok(())
    });
}
// END DELETE

// Keep only production code
```

---

### Priority 3: Fix Integration Test Compilation (3-4 hours)

**Problem**: Integration tests in tests/integration/ have import and type errors.

**Common Fixes**:

1. **Import Paths**:
```rust
// WRONG:
use crate::generator::Generator;

// RIGHT:
use ggen_core::generator::Generator;
```

2. **Error Types**:
```rust
// Add to top of each test file:
use std::error::Error;

#[test]
fn test_name() -> Result<(), Box<dyn Error>> {
    // ...
    Ok(())
}
```

3. **Test Utilities**:
```rust
// Add to tests/common/mod.rs:
use tempfile::TempDir;
use std::fs;
use std::path::PathBuf;

pub fn create_temp_dir() -> Result<TempDir, Box<dyn Error>> {
    Ok(TempDir::new()?)
}

pub fn write_file_in_temp(dir: &TempDir, path: &str, content: &str) -> Result<(), Box<dyn Error>> {
    let file_path = dir.path().join(path);
    fs::write(file_path, content)?;
    Ok(())
}
```

**Validation**:
```bash
# Compile each test file individually:
for test in tests/integration/*.rs; do
    echo "Compiling: $test"
    cargo test --test $(basename "$test" .rs) --no-run
done
```

---

### Priority 4: Achieve 100% Pass Rate (2-4 hours)

**Only after all tests compile:**

```bash
# Run all tests:
cargo test --lib --workspace

# Target: 100% pass rate
# If failures occur:
1. Read failure message carefully
2. Fix root cause (not symptom)
3. Re-run test
4. Repeat until green
```

**Common Test Failures**:
- Missing test data files → Add to tests/fixtures/
- Hardcoded paths → Use TempDir
- Concurrency issues → Add `-- --test-threads=1`
- Flaky tests → Add retries or fix timing

---

## Effort Estimate

| Priority | Task | Hours | Blockers |
|----------|------|-------|----------|
| 1 | Fix chicago_tdd_tools API | 4-6 | None |
| 2 | Remove tests from src/ | 2-3 | Priority 1 |
| 3 | Fix integration tests | 3-4 | Priority 1 |
| 4 | Achieve 100% pass rate | 2-4 | Priorities 1-3 |
| **TOTAL** | **Complete remaining 50%** | **11-17** | - |

---

## Success Criteria

```bash
# All checks must pass:
cargo check --lib --workspace          # ✅ Libraries compile
cargo check --tests                    # ✅ Tests compile
grep -r "chicago_tdd_tools" crates/*/src/  # 🚫 No results
cargo test --lib --workspace           # ✅ 100% tests passing
cargo test --test integration          # ✅ Integration tests passing
```

---

## Alternative: Quick Win Strategy

If 11-17 hours is too much, consider this **80/20 quick win**:

### Phase A: Fix ONE Module Completely (2 hours)

Pick the smallest, most critical module (e.g., `crates/ggen-core/src/preprocessor.rs`):

1. Remove chicago_tdd_tools from that file (30 min)
2. Fix integration test for that module (30 min)
3. Get test compiling and passing (1 hour)
4. **Success metric**: 1 module, 100% working

### Phase B: Apply Pattern to 5 More Modules (3 hours)

Use the working pattern from Phase A:
1. Apply to 5 more critical modules
2. **Success metric**: 6 modules, 100% working

### Phase C: Declare "Critical 20%" Complete (15 min)

Document:
- 6 modules tested (the critical 20%)
- 100% pass rate on critical functionality
- Remaining 80% can be migrated over time
- **Deliverable**: Production-ready core modules

**Total Time: 5-6 hours** (vs 11-17 hours for 100%)

---

## Recommended Approach

**For Production Systems**: Do ALL priorities (11-17 hours)
- Ensures complete coverage
- No technical debt
- Full regression protection

**For Rapid Iteration**: Do Quick Win Strategy (5-6 hours)
- Gets core modules to 100%
- Demonstrates pattern works
- Can scale to remaining modules later

---

## Automation Scripts

### Script 1: Count Remaining Work

```bash
#!/usr/bin/env bash
# scripts/count-remaining-work.sh

echo "Remaining chicago_tdd_tools references:"
grep -r "chicago_tdd_tools::" crates/*/src/ 2>/dev/null | wc -l

echo ""
echo "Files that need fixing:"
grep -rl "chicago_tdd_tools::" crates/*/src/ 2>/dev/null | wc -l

echo ""
echo "Integration tests to fix:"
find tests/integration -name "*.rs" | wc -l
```

### Script 2: Fix One File

```bash
#!/usr/bin/env bash
# scripts/fix-one-file.sh <filename>

FILE=$1
if [ -z "$FILE" ]; then
    echo "Usage: $0 <path/to/file.rs>"
    exit 1
fi

echo "Fixing: $FILE"

# Backup
cp "$FILE" "$FILE.backup"

# Remove chicago_tdd_tools import
sed -i '' '/use chicago_tdd_tools/d' "$FILE"

# Convert test! macros to #[test]
# (This is simplified - may need manual review)
sed -i '' 's/test!(\([^,]*\), {/#[test]\nfn \1() -> Result<(), Box<dyn std::error::Error>> {/g' "$FILE"

# Verify compilation
if cargo check --lib --quiet 2>/dev/null; then
    echo "✅ Success - library still compiles"
    rm "$FILE.backup"
else
    echo "❌ Failed - restoring backup"
    mv "$FILE.backup" "$FILE"
    exit 1
fi
```

---

## Monitoring Progress

Track progress with this dashboard:

```bash
#!/usr/bin/env bash
# scripts/progress-dashboard.sh

echo "╔════════════════════════════════════════════════════════╗"
echo "║         SWARM COMPLETION DASHBOARD                    ║"
echo "╚════════════════════════════════════════════════════════╝"
echo ""

# Progress Metrics
TOTAL_FILES=$(grep -rl "chicago_tdd_tools::" crates/*/src/ 2>/dev/null | wc -l | tr -d ' ')
TOTAL_REFS=$(grep -r "chicago_tdd_tools::" crates/*/src/ 2>/dev/null | wc -l | tr -d ' ')
INITIAL_FILES=67
INITIAL_REFS=70

FILES_FIXED=$((INITIAL_FILES - TOTAL_FILES))
REFS_FIXED=$((INITIAL_REFS - TOTAL_REFS))

PERCENT_FILES=$((FILES_FIXED * 100 / INITIAL_FILES))
PERCENT_REFS=$((REFS_FIXED * 100 / INITIAL_REFS))

echo "Files Fixed:      $FILES_FIXED / $INITIAL_FILES ($PERCENT_FILES%)"
echo "References Fixed: $REFS_FIXED / $INITIAL_REFS ($PERCENT_REFS%)"
echo ""

# Compilation Status
if cargo check --lib --workspace --quiet 2>/dev/null; then
    echo "Library Compilation: ✅ PASS"
else
    echo "Library Compilation: ❌ FAIL"
fi

if cargo check --tests --quiet 2>/dev/null; then
    echo "Test Compilation:    ✅ PASS"
else
    echo "Test Compilation:    ❌ FAIL"
fi

# Progress Bar
PROGRESS=$((PERCENT_FILES))
BAR_LENGTH=50
FILLED=$((PROGRESS * BAR_LENGTH / 100))
EMPTY=$((BAR_LENGTH - FILLED))

echo ""
echo -n "Progress: ["
printf '%*s' "$FILLED" | tr ' ' '█'
printf '%*s' "$EMPTY" | tr ' ' '░'
echo "] $PROGRESS%"
```

---

## Support

For questions or issues:

1. **Check validation script**: `./scripts/validate-swarm-status.sh`
2. **Review honest assessment**: `docs/SWARM_PHASE7_HONEST_ASSESSMENT.md`
3. **Monitor progress**: `./scripts/progress-dashboard.sh`
4. **Read detailed findings**: All findings documented in assessment

---

## Summary

The swarm made significant progress (8,130 LOC integration tests created) but needs 11-17 more hours to achieve production-ready status.

**Two paths forward:**

1. **Complete All Work** (11-17 hours): Full production readiness
2. **Quick Win** (5-6 hours): Critical 20% to 100%, scale later

**Recommendation**: Start with Quick Win to prove pattern, then scale to 100%.

---

*Generated: 2025-11-16*
*Status: 50% Complete*
*Next Milestone: Fix chicago_tdd_tools API*
