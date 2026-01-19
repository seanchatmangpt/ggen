# CLI Business Logic Extraction - Priority List

**Generated:** 2025-11-20
**Total Remaining Work:** 1 hour 45 minutes

---

## CRITICAL FINDING

✅ **The ggen CLI codebase is ALREADY 94.1% migrated to proper domain separation!**

This is an **exemplary codebase** following clean architecture principles:
- CLI layer is thin adapters (parsing + output formatting)
- Domain layer contains all business logic
- Clean async/sync bridging
- No circular dependencies
- Excellent test coverage

---

## PRIORITY 0: CRITICAL (None!)

**Status:** ✅ NO CRITICAL WORK NEEDED

All critical business logic has been extracted to domain layer.

---

## PRIORITY 1: IMPORTANT (1.5 hours total)

### P1-1: Marketplace List Maturity Filtering

**What:** Extract filtering/sorting logic from CLI to domain
**Where:** `crates/ggen-cli/src/cmds/marketplace.rs:252-285`
**LOC:** 33 lines
**Effort:** 1 hour
**Risk:** Low
**Complexity:** Low

**Why Do This:**
- Consistency with other domain functions
- Testability (can unit test filtering logic)
- Separation of concerns (CLI shouldn't know about maturity scores)

**Domain Function:**
```rust
// Create: crates/ggen-domain/src/marketplace/list.rs

pub struct FilterAndSortOptions {
    pub min_maturity: Option<String>,
    pub maturity_level: Option<String>,
    pub sort_by: Option<String>,
}

pub fn filter_and_sort(
    packages: Vec<InstalledPackageInfo>,
    options: &FilterAndSortOptions,
) -> Result<Vec<InstalledPackageInfo>> {
    // Move filtering logic here
}
```

**Testing:**
```bash
cargo make test --test marketplace_list_filtering
```

**Acceptance Criteria:**
- [ ] Domain function created
- [ ] CLI calls domain function
- [ ] Unit tests pass (maturity filtering, sorting)
- [ ] Integration test updated
- [ ] cargo make lint passes

---

### P1-2: Marketplace Improve Template Application

**What:** Simplify CLI wrapper for template improvements
**Where:** `crates/ggen-cli/src/cmds/marketplace.rs:1677-1693`
**LOC:** 16 lines
**Effort:** 30 minutes
**Risk:** Very Low
**Complexity:** Low

**Why Do This:**
- Cleaner separation (domain function already exists!)
- Simpler error handling
- Better output formatting

**Refactored Code:**
```rust
if let Some(template) = apply {
    let message = apply_template_improvements(&package_path, &template)?;
    println!("\n✅ {}", message);
    return Ok(serde_json::json!({ "status": "applied", ... }));
}
```

**Testing:**
```bash
cargo make test --test marketplace_improve
```

**Acceptance Criteria:**
- [ ] CLI simplified
- [ ] Domain function still handles application
- [ ] Output format unchanged
- [ ] Tests pass

---

## PRIORITY 2: NICE-TO-HAVE (15 minutes total)

### P2-1: Variable Parsing Helper (DRY)

**What:** Extract duplicated variable parsing to utility
**Where:** `template.rs:303-316`, `project.rs:777-790`
**LOC:** 13 lines (duplicated in 2 files)
**Effort:** 15 minutes
**Risk:** None
**Complexity:** Trivial

**Why Do This:**
- DRY principle (Don't Repeat Yourself)
- Single source of truth
- Easier to maintain/update

**Utility Function:**
```rust
// Create: crates/ggen-utils/src/cli.rs

pub fn parse_key_value_pairs(
    pairs: &[String],
) -> Result<BTreeMap<String, String>, String> {
    // Move parsing logic here
}
```

**Testing:**
```bash
cargo make test --test cli_utilities
```

**Acceptance Criteria:**
- [ ] Utility function created with tests
- [ ] template.rs uses utility
- [ ] project.rs uses utility
- [ ] All tests pass

---

## EXECUTION SCHEDULE

### Week 1 (Total: 1 hour 45 minutes)

**Monday Morning (1 hour):**
- [ ] P1-1: Marketplace list filtering (60 min)
  - Create domain function (20 min)
  - Update CLI (10 min)
  - Write tests (20 min)
  - Verify (10 min)

**Monday Afternoon (30 minutes):**
- [ ] P1-2: Marketplace improve refactor (30 min)
  - Refactor CLI (15 min)
  - Test (10 min)
  - Verify (5 min)

**Tuesday Morning (15 minutes):**
- [ ] P2-1: Variable parsing helper (15 min)
  - Create utility (5 min)
  - Write tests (5 min)
  - Update CLI files (3 min)
  - Verify (2 min)

**Tuesday Afternoon:**
- [ ] Final validation
  - Run full test suite: `cargo make test`
  - Run linting: `cargo make lint`
  - Run type checking: `cargo make check`
  - Update documentation

---

## VALIDATION COMMANDS

After each change:

```bash
# Quick validation
cargo make check           # Compiler check
cargo make test-unit       # Unit tests
cargo make lint            # Clippy linting

# Full validation (before commit)
cargo make test            # All tests
cargo make pre-commit      # Format + lint + tests
cargo make ci              # Full CI pipeline
```

---

## RISK MITIGATION

### P1-1 Risks:
- **Risk:** Breaking existing list functionality
- **Mitigation:** Extensive unit tests, integration tests unchanged
- **Rollback:** Git revert if tests fail

### P1-2 Risks:
- **Risk:** None (domain function already exists)
- **Mitigation:** Simple refactor, same behavior
- **Rollback:** Git revert if needed

### P2-1 Risks:
- **Risk:** None (pure utility function)
- **Mitigation:** Comprehensive unit tests
- **Rollback:** Git revert if tests fail

---

## DEFINITION OF DONE

For each task:

- [ ] Code changes committed
- [ ] Unit tests added and passing
- [ ] Integration tests passing
- [ ] `cargo make check` passes (no compiler errors/warnings)
- [ ] `cargo make test` passes (all tests pass)
- [ ] `cargo make lint` passes (no clippy warnings)
- [ ] Documentation updated (if needed)
- [ ] Code review complete (self-review or peer)
- [ ] No circular dependencies introduced
- [ ] Performance unchanged (or improved)

---

## POST-COMPLETION METRICS

**Target State:**
- Functions fully migrated: 34/34 (100%)
- LOC to extract: 0
- Code duplication instances: 0
- Architecture score: Perfect (clean separation of concerns)

**Current State:**
- Functions fully migrated: 32/34 (94.1%)
- LOC to extract: 62
- Code duplication instances: 1

**Gap to Close:**
- 2 functions to complete (5.9%)
- 62 LOC to extract (2.2% of total)
- 1 duplication to remove

---

## RECOMMENDATIONS

### DO NOW (Priority 1)
1. ✅ Schedule 1.5 hours for P1 tasks
2. ✅ Create feature branch: `feature/cli-extraction-final`
3. ✅ Execute P1-1 and P1-2

### DO THIS WEEK (Priority 2)
4. ✅ Execute P2-1 (DRY improvement)
5. ✅ Final validation and testing
6. ✅ Create PR with comprehensive description

### DO LATER (Nice-to-have)
7. Consider extracting input validation patterns to shared utilities
8. Document the excellent architecture for other projects
9. Create architecture decision record (ADR) for separation pattern

---

## SUCCESS CELEBRATION

When complete, this codebase will have:
- ✅ **100% domain separation** (perfect architecture)
- ✅ **Zero code duplication** in CLI layer
- ✅ **Complete test coverage** of business logic
- ✅ **Clean async/sync bridging** pattern
- ✅ **Exemplary codebase** for other Rust projects

**This will be a reference implementation of clean architecture in Rust CLI applications!**

---

**END OF PRIORITY LIST**
