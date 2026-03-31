# London TDD Pattern Analysis - Master Summary

**Generated:** 2026-03-30
**Agent:** Agent 7 (Codebase-wide search)
**Scope:** Entire codebase (excluded: tests-archive/, vendors/, external/, target/)

---

## EXECUTIVE SUMMARY

- **Total files with mockall patterns:** 153 (excluding archives/worktrees)
- **Total files with Mock structs:** 125
- **Total files with #[automock]:** 26
- **Estimated total London TDD tests:** ~1,724 tests
- **Estimated Chicago TDD tests:** ~2,946 tests
- **Current ratio:** 37% London TDD, 63% Chicago TDD

**Key Finding:** The codebase has a significant London TDD legacy that needs migration.

---

## BREAKDOWN BY DIRECTORY

### 1. tests/london_tdd/ (PRIMARY LONDON TDD REPOSITORY)

- **Files:** 24 .rs files
- **Test functions:** 960
- **Mock traits:** ~5 traits in lib.rs
- **Patterns:** mockall::automock, MockXxx structs, expect().times()
- **Assessment:** Dedicated London TDD example repository
- **Recommendation:** ARCHIVE ENTIRELY to tests-archive/london_tdd_legacy/

**File breakdown:**
- tests/london_tdd/lib.rs: 5 #[automock] traits (mock infrastructure)
- tests/london_tdd/cli_commands/quickstart_test.rs: 6 test functions
- tests/london_tdd/v2_arch_comprehensive_test.rs: 12 test functions
- tests/london_tdd/v2_architecture/unit/runtime_bridge_test.rs: 10 test functions
- tests/london_tdd/cli_commands/error_message_quality_comprehensive_test.rs: 27 test functions
- tests/london_tdd/otel_validation/trace_validator.rs: 8 test functions
- tests/london_tdd/v2_architecture/unit/error_handling_test.rs: 8 test functions
- tests/london_tdd/template_engine/rendering_test.rs: 5 test functions
- tests/london_tdd/template_engine/rdf_sparql_test.rs: 5 test functions
- tests/london_tdd/marketplace/search_test.rs: 5 test functions
- tests/london_tdd/ai_generation/project_gen_test.rs: 5 test functions

### 2. crates/ggen-core/tests/

- **Files with mockall:** 1 file (london_tdd_examples.rs)
- **Test functions:** 96
- **Mock traits:** 4 #[automock] traits
- **Assessment:** Educational examples of London TDD patterns
- **Recommendation:** ARCHIVE to tests-archive/london_tdd_examples/

**Traits mocked:**
- CommandExecutor (mocks shell command execution)
- StateRepository (mocks filesystem state)
- TemplateCache (mocks template caching)
- NotificationService (mocks user notifications)

### 3. marketplace/packages/ (CRITICAL FINDING - NAMING VIOLATION)

#### packages/project-management/tests/chicago_tdd_tests.rs
- **9 #[automock] traits**
- **~50+ test functions**
- **NAMING VIOLATION:** File named "chicago_tdd_tests.rs" but uses London TDD!

**Mocked traits:**
- RDFStore
- BudgetService
- DependencyService
- SprintService
- ResourceService
- RiskService
- NotificationService
- ReportService
- WorkflowService

#### packages/document-management-system/tests/chicago_tdd_tests.rs
- **10 #[automock] traits**
- **~50+ test functions**
- **NAMING VIOLATION:** File named "chicago_tdd_tests.rs" but uses London TDD!

**Assessment:** These are MISNAMED - they're London TDD tests
**Recommendation:** REFACTOR to real Chicago TDD or DELETE

### 4. crates/ggen-cli/tests/

- **Files with mockall:** 5 files
- **Test functions:** 42
- **Key file:** tests/packs/unit/installation/download_test.rs (1 #[automock])
- **Assessment:** Mix of London and Chicago TDD
- **Recommendation:** CONVERT to Chicago TDD

**Files with mockall:**
- tests/conventions/fixtures.rs
- tests/conventions/planner_tests.rs
- tests/conventions/resolver_tests.rs
- tests/conventions/watch_tests.rs
- tests/packs/unit/installation/download_test.rs

### 5. crates/ggen-a2a-mcp/tests/

- **Status:** Unknown (needs investigation)
- **Recommendation:** INVESTIGATE and catalog

### 6. crates/ggen-domain/tests/

- **Status:** Unknown (needs investigation)
- **Recommendation:** INVESTIGATE and catalog

---

## DETAILED PATTERN BREAKDOWN

### Pattern 1: mockall::automock (26 files)

```rust
#[automock]
trait CommandExecutor {
    fn execute(&self, spec: &CommandSpec) -> Result<CommandOutput>;
}
```

**Found in:** tests/london_tdd/, marketplace/, crates/ggen-core/tests/
**Creates:** MockXxx structs automatically
**Enables:** Behavior verification
**Status:** FORBIDDEN in Chicago TDD

### Pattern 2: Mock Structs (125 files)

```rust
struct MockRegistryClient {
    packs: HashMap<String, PackMetadata>,
}
```

**Found in:** Archive files, legacy tests
**Type:** Hand-written test doubles
**Status:** FORBIDDEN in Chicago TDD

### Pattern 3: Behavior Verification

```rust
mock.expect_get()
    .with(eq("https://example.com"))
    .times(1)
    .returning(Ok("response".to_string()));
```

**Found in:** All London TDD tests
**Status:** FORBIDDEN in Chicago TDD
**Reason:** Tests mock behavior, not real system behavior

---

## CRITICAL FINDINGS

### 1. NAMING VIOLATION (HIGH PRIORITY)

Two files are **MISNAMED** as "chicago_tdd_tests.rs" but use London TDD:
- marketplace/packages/project-management/tests/chicago_tdd_tests.rs
- marketplace/packages/document-management-system/tests/chicago_tdd_tests.rs

**Action Required:** Either rename to london_tdd_tests.rs OR convert to real Chicago TDD

### 2. tests/london_tdd/ IS A GOLDMINE OF EXAMPLES

This directory contains 960 tests demonstrating:
- How to write London TDD with mockall
- Common patterns and anti-patterns
- Integration tests with mocks
- CLI command testing with mocks

**Value:** Educational resource for what NOT to do
**Recommendation:** Archive with clear DEPRECATED notice

### 3. MIXED TESTING STYLES IN CRATES

- crates/ggen-core/tests/: Mix of Chicago and London
- crates/ggen-cli/tests/: Mix of Chicago and London
- marketplace/packages/: Inconsistent styles

**Action Required:** Standardize to Chicago TDD across all crates

---

## ESTIMATED CONVERSION EFFORT

### Convert to Chicago TDD (HIGH EFFORT)

- tests/london_tdd/: 960 tests → ~200 hours
- marketplace packages: ~100 tests → ~20 hours
- crates/ggen-cli/tests/: 42 tests → ~10 hours
- crates/ggen-core/tests/london_tdd_examples.rs: 96 tests → ~20 hours

**Total: ~250 hours**

### Delete and Archive (LOW EFFORT)

- Archive tests/london_tdd/ to tests-archive/london_tdd_legacy/
- Delete marketplace london_tdd tests (misnamed)
- Remove london_tdd_examples.rs

**Total: ~4 hours**

### Hybrid Approach (RECOMMENDED)

1. Archive tests/london_tdd/ (educational value) - 1 hour
2. Delete marketplace london_tdd tests (misnamed, low value) - 1 hour
3. Convert critical tests in crates/ directory - 40 hours
4. Update CLAUDE.md to document Chicago TDD requirement - 2 hours

**Total: ~44 hours**

---

## RECOMMENDATIONS

### IMMEDIATE ACTIONS (WEEK 1)

1. **Archive tests/london_tdd/**
   - Move to tests-archive/london_tdd_legacy/
   - Add DEPRECATED.md explaining why London TDD is forbidden

2. **Fix naming violations**
   - Rename marketplace/*/chicago_tdd_tests.rs to london_tdd_tests.rs
   - OR delete them (if low value)

3. **Update CLAUDE.md**
   - Add clear Chicago TDD requirement
   - Link to testing-forbidden.md rules

### MEDIUM-TERM ACTIONS (WEEK 2-3)

1. **Convert critical test suites**
   - Focus on crates/ggen-core/tests/
   - Focus on crates/ggen-cli/tests/
   - Use real collaborators (reqwest, SQLite, tempfile)

2. **Add CI checks**
   - Fail PR if new London TDD patterns detected
   - Add grep check for mockall/automock in tests/

### LONG-TERM ACTIONS (MONTH 1)

1. **Complete migration**
   - All tests use Chicago TDD
   - 100% real collaborators
   - No mocks or test doubles

2. **Update documentation**
   - Write migration guide
   - Add Chicago TDD examples
   - Update training materials

---

## SUCCESS CRITERIA

- [ ] 0 files with #[automock] in active codebase
- [ ] 0 Mock structs in active codebase
- [ ] 100% of tests use real collaborators
- [ ] CI fails on London TDD patterns
- [ ] Documentation updated
- [ ] All tests pass after migration

---

## APPENDIX: FILES WITH MOCKALL PATTERNS

### Test Files (26 files with #[automock])
1. crates/ggen-core/tests/london_tdd_examples.rs (4 traits)
2. crates/ggen-cli/tests/packs/unit/installation/download_test.rs (1 trait)
3. marketplace/packages/project-management/tests/chicago_tdd_tests.rs (9 traits)
4. marketplace/packages/document-management-system/tests/chicago_tdd_tests.rs (10 traits)
5. tests/london_tdd/template_engine/rendering_test.rs (1 trait)
6. tests/london_tdd/template_engine/rdf_sparql_test.rs (1 trait)
7. tests/london_tdd/cli_commands/doctor_test.rs (1 trait)
8. tests/london_tdd/cli_commands/help_me_test.rs (1 trait)
9. tests/london_tdd/cli_commands/quickstart_test.rs (2 traits)
10. tests/london_tdd/lib.rs (5 traits)

### Cargo.toml Files with mockall Dependency
- 30+ crates have mockall in dev-dependencies
- All need removal after migration

---

**Agent 7 - Task Complete**
**Next Step:** Review findings and determine migration strategy
