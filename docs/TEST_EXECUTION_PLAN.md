# Refactoring Validation Test Execution Plan

## Overview

This document outlines how to execute the refactoring validation test suite once compilation errors are resolved.

## Pre-Execution Checklist

- [ ] Fix compilation errors in `cli/src/cmds/graph.rs` (missing `GraphCmd`)
- [ ] Fix compilation errors in `cli/src/domain/utils/doctor.rs` (missing `run_doctor`)
- [ ] Fix compilation errors in `cli/src/domain/utils/env.rs` (missing imports)
- [ ] Verify `cargo build` succeeds
- [ ] Verify `cargo test` compiles

## Test Execution Sequence

### Phase 1: Smoke Test (Quick Validation)

Run a single test to verify the test harness works:

```bash
cargo test --test refactoring_validation regression::rdf_parsing::test_parse_simple_rdf -- --nocapture
```

**Expected:** ✅ Test passes, "Parse simple RDF: PASSED" message

**Duration:** < 5 seconds

### Phase 2: Regression Tests (v1 Functionality)

Verify all v1 functionality still works:

```bash
cargo test --test refactoring_validation regression -- --nocapture
```

**Tests:**
- ✅ RDF parsing (3 tests)
- ✅ SPARQL queries (3 tests)
- ✅ Template rendering (3 tests)

**Expected:** All 9 tests pass

**Duration:** < 20 seconds

### Phase 3: Migration Tests (v2 Architecture)

Verify v2 architecture works correctly:

```bash
cargo test --test refactoring_validation migration -- --nocapture
```

**Tests:**
- ✅ Three-layer architecture (3 tests)
- ✅ Async/sync bridge (3 tests)
- ✅ Error handling (4 tests)
- ✅ Domain modules (3 tests)

**Expected:** All 13 tests pass

**Duration:** < 30 seconds

### Phase 4: Integration Tests (E2E Workflows)

Verify complete workflows work:

```bash
cargo test --test refactoring_validation integration -- --nocapture
```

**Tests:**
- ✅ RDF-to-code workflows (2 tests)
- ✅ Multi-step workflows (3 tests)
- ✅ V1 workflows in v2 (3 tests)
- ✅ New v2 workflows (3 tests)

**Expected:** All 11 tests pass

**Duration:** < 40 seconds

### Phase 5: Performance Tests (Regression Detection)

Verify no performance regressions:

```bash
cargo test --test refactoring_validation performance -- --nocapture
```

**Tests:**
- ✅ RDF parsing performance (2 tests)
- ✅ SPARQL query performance (2 tests)
- ✅ Template rendering performance (2 tests)
- ✅ E2E workflow performance (1 test)

**Expected:** All 7 tests pass, metrics < thresholds

**Duration:** < 60 seconds (5 iterations per test)

### Phase 6: Full Suite (Complete Validation)

Run all tests together:

```bash
cargo test --test refactoring_validation -- --nocapture
```

**Expected:** All 40+ tests pass

**Duration:** < 2 minutes

## Troubleshooting Guide

### Test Failure Scenarios

#### Scenario 1: RDF Parsing Fails

```
❌ test regression::rdf_parsing::test_parse_simple_rdf ... FAILED
```

**Diagnosis:**
- Check if `ggen graph validate` command exists
- Verify RDF parser is correctly wired in v2 architecture
- Check domain/graph module is accessible

**Fix:**
1. Run `cargo run -- graph validate --help`
2. If command not found, check `cli/src/cmds/graph.rs`
3. Verify `domain::graph::validate` exists

#### Scenario 2: Runtime Bridge Fails

```
❌ test migration::async_sync_bridge::test_sync_wrapper_for_async_domain ... FAILED
```

**Diagnosis:**
- Check if `runtime::execute()` bridge works
- Verify async domain functions can be called from sync CLI

**Fix:**
1. Check `cli/src/runtime.rs` exists and exports `execute()`
2. Verify tokio runtime is created correctly
3. Test async function execution manually

#### Scenario 3: Performance Regression

```
❌ test performance::rdf_parsing_performance::test_rdf_parse_performance ... FAILED
assertion failed: avg_duration < Duration::from_secs(1)
```

**Diagnosis:**
- RDF parsing taking > 1 second
- Possible performance regression

**Fix:**
1. Run performance test multiple times to confirm
2. Check if Debug build (slower) or Release build
3. Profile RDF parsing code if confirmed regression

#### Scenario 4: Template Rendering Fails

```
❌ test regression::template_rendering::test_render_simple_template ... FAILED
```

**Diagnosis:**
- Template engine not working
- Tera integration broken

**Fix:**
1. Check `domain::template::render` function
2. Verify Tera dependency is correctly included
3. Test template rendering manually

## Performance Metrics to Collect

After all tests pass, collect these metrics:

```bash
# Run performance tests and capture output
cargo test --test refactoring_validation performance -- --nocapture > performance_results.txt 2>&1
```

**Metrics to extract:**
- RDF parse time (simple): ______ ms
- RDF parse time (large): ______ ms
- SPARQL query time (simple): ______ ms
- SPARQL query time (complex): ______ ms
- Template render time (simple): ______ ms
- Template render time (complex): ______ ms
- E2E workflow time: ______ ms

**Thresholds:**
- ✅ Simple RDF parse: < 1,000 ms
- ✅ Large RDF parse: < 2,000 ms
- ✅ SPARQL queries: < 1,000 ms
- ✅ Template rendering: < 500 ms (simple), < 1,000 ms (complex)
- ✅ E2E workflow: < 2,000 ms

## Success Criteria

### All Tests Must Pass

```
test result: ok. 40 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

### Performance Within Thresholds

All performance tests must complete within expected time limits (< 20% variance from baseline).

### Output Validation

All integration tests must produce expected output files with correct content.

### Workflow Validation

All E2E workflows must complete successfully from start to finish.

## Reporting

After execution, generate validation report:

```bash
# Run all tests and save results
cargo test --test refactoring_validation -- --nocapture > test_results.txt 2>&1

# Check results
grep "PASSED" test_results.txt | wc -l  # Should be 40+
grep "FAILED" test_results.txt | wc -l  # Should be 0
```

## Final Checklist

- [ ] All 40+ tests pass
- [ ] Performance metrics < thresholds
- [ ] No compilation warnings
- [ ] Output files generated correctly
- [ ] Workflows complete end-to-end
- [ ] Test execution < 2 minutes
- [ ] Validation report generated
- [ ] Results documented

## Post-Execution

### Update Documentation

1. Update `REFACTORING_VALIDATION_REPORT.md` with results
2. Add performance metrics to comparison table
3. Document any issues found and fixed
4. Mark refactoring as validated

### Commit Test Results

```bash
git add tests/refactoring_validation/
git add docs/REFACTORING_VALIDATION_*.md
git add docs/TEST_EXECUTION_PLAN.md
git commit -m "test: Add comprehensive refactoring validation suite

- 40+ tests covering regression, migration, integration, performance
- Chicago TDD approach testing real code
- Performance benchmarking with automated regression detection
- Complete v1→v2 refactoring validation

All tests pass with no regressions detected."
```

---

**Framework:** Rust + Cargo
**Test Count:** 40+ tests
**Duration:** < 2 minutes
**Approach:** Chicago TDD
**Status:** Ready to execute after compilation fixes
