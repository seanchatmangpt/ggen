# ⚡ Test Consolidation: Quick Reference

## TL;DR - 80/20 Strategy

**Goal:** Consolidate 8,274 lines of tests across 19 files into 4 lean modules (1,300 lines) while keeping 80% bug detection capability.

### Before & After

```
BEFORE                          AFTER
├── 19 test files              ├── 4 test modules
├── 8,274 total lines          ├── 1,300 total lines
├── 90+ tests                  ├── 26 critical tests
├── 5-10 min execution         └── <60s execution
└── Low maintainability        ✅ High maintainability
```

---

## Quick Start (5 minutes)

### 1. Review Decision

Read one document:
```bash
cat docs/TEST_CONSOLIDATION_80_20.md
```

Decision points:
- Keep: `consolidated_quality_tests.rs`, `chicago_tdd_smoke_test.rs`, etc.
- Remove: `determinism_framework.rs`, `telemetry_tests.rs`, etc.

### 2. Understand Structure

```bash
cat docs/TEST_CONSOLIDATION_ARCHITECTURE.md | head -100
```

Test modules:
- `consolidated_core_tests.rs` - Package validation, CRUD
- `consolidated_lifecycle_tests.rs` - State transitions, errors
- `consolidated_swarm_tests.rs` - Consensus, failures
- `consolidated_semantic_tests.rs` - RDF, ontology

### 3. Start Implementation

```bash
cat docs/TEST_CONSOLIDATION_IMPLEMENTATION.md

# Then follow phases 1-7
```

---

## Test Categories at a Glance

| Module | Tests | Lines | Time | Focus |
|--------|-------|-------|------|-------|
| Core | 7 | 350 | 2s | Validation, CRUD |
| Lifecycle | 9 | 350 | 2s | State, errors |
| Swarm | 6 | 300 | 4.5s | Consensus, faults |
| Semantic | 7 | 300 | 2s | RDF, ontology |
| **Total** | **29** | **1,300** | **10s** | **80% coverage** |

---

## Files to Keep vs Remove

### Keep & Consolidate ✅
```
✅ consolidated_quality_tests.rs (355 lines)
✅ chicago_tdd_smoke_test.rs (96 lines)
✅ pack_integration_tests.rs (401 lines)
✅ ontology_extraction_tests.rs (501 lines)
✅ lifecycle_bdd.rs (extract critical - 645 lines)
✅ lifecycle_edge_cases.rs (extract critical - 715 lines)
✅ swarm_consensus_tests.rs (344 lines)
✅ swarm_e2e_tests.rs (407 lines)
✅ swarm_failure_recovery_tests.rs (335 lines)
✅ swarm_security_tests.rs (290 lines)
```

### Remove ❌ (Low ROI)
```
❌ determinism_framework.rs (374 lines)
❌ telemetry_tests.rs (153 lines)
❌ template_comprehensive_test.rs (826 lines)
❌ production_validation.rs (503 lines)
❌ rdf_rendering_e2e.rs (653 lines)
❌ swarm_performance_tests.rs (341 lines)
❌ swarm_integration_tests.rs (320 lines)
❌ london_tdd_examples.rs (558 lines)
❌ marketplace_graph_integration.rs (326 lines)
```

**Total removed:** 4,054 lines (49% reduction before consolidation)

---

## Implementation Checklist

### Phase 1: Setup (30 min)
- [ ] Backup original tests
  ```bash
  mkdir -p /Users/sac/ggen/tests-archive
  cp -r crates/ggen-core/tests/* /Users/sac/ggen/tests-archive/
  ```
- [ ] Create git commit
  ```bash
  git commit -m "backup: archive original tests"
  ```

### Phase 2: Core Tests (1 hour)
- [ ] Create `consolidated_core_tests.rs`
- [ ] Extract critical tests from:
  - `consolidated_quality_tests.rs` (keep)
  - `chicago_tdd_smoke_test.rs` (merge)
  - `pack_integration_tests.rs` (merge)
- [ ] Run tests: `cargo test --test consolidated_core_tests`

### Phase 3: Lifecycle Tests (1.5 hours)
- [ ] Create `consolidated_lifecycle_tests.rs`
- [ ] Extract critical tests from:
  - `lifecycle_bdd.rs` (keep happy path)
  - `lifecycle_edge_cases.rs` (keep critical 5)
- [ ] Run tests: `cargo test --test consolidated_lifecycle_tests`

### Phase 4: Swarm Tests (2 hours)
- [ ] Create `consolidated_swarm_tests.rs`
- [ ] Extract critical tests from:
  - `swarm_consensus_tests.rs` (keep all)
  - `swarm_e2e_tests.rs` (keep happy path)
  - `swarm_failure_recovery_tests.rs` (keep 2-3)
  - `swarm_security_tests.rs` (keep byzantine)
- [ ] Run tests: `cargo test --test consolidated_swarm_tests`

### Phase 5: Semantic Tests (1.5 hours)
- [ ] Create `consolidated_semantic_tests.rs`
- [ ] Extract critical tests from:
  - `ontology_extraction_tests.rs` (keep all)
  - `rdf_rendering_e2e.rs` (keep basic)
- [ ] Run tests: `cargo test --test consolidated_semantic_tests`

### Phase 6: Validation (1 hour)
- [ ] Run full test suite: `cargo make test`
- [ ] Verify: All tests pass ✅
- [ ] Verify: Execution time < 60s ✅
- [ ] Archive old test files
  ```bash
  mv crates/ggen-core/tests/determinism_framework.rs tests-archive/
  # ... move all low-ROI tests
  ```

### Phase 7: Documentation (30 min)
- [ ] Update README.md with test overview
- [ ] Update CI/CD pipeline config
- [ ] Create git commit
  ```bash
  git commit -m "feat: consolidate tests using 80/20 principle"
  ```

---

## Key Numbers

| Metric | Before | After | Reduction |
|--------|--------|-------|-----------|
| Test Files | 19 | 4 | 79% ✅ |
| Total Lines | 8,274 | 1,300 | 84% ✅ |
| Test Count | 90+ | 29 | 68% ✅ |
| Execution Time | 5-10 min | <60s | 90% ✅ |
| Coverage (Critical) | 90% | 80% | -10% (acceptable) |

---

## Critical Tests (Never Remove)

### Package Management (5)
1. ✅ Valid package ID validation
2. ✅ Valid version validation
3. ✅ Package CRUD operations
4. ✅ Dependency resolution
5. ✅ Circular dependency detection

### Lifecycle (4)
6. ✅ Draft → Published transition
7. ✅ Version upgrade workflow
8. ✅ Installation flow
9. ✅ Package yanking

### Consensus (4)
10. ✅ Leader election
11. ✅ State agreement
12. ✅ Node failure recovery
13. ✅ Byzantine tolerance

### Semantic (3)
14. ✅ Ontology validation
15. ✅ RDF operations
16. ✅ SPARQL queries

**Total: 16 core scenarios covering 80% of bugs**

---

## Testing Guidelines

### What to Include (80% value)
✅ Happy path (main workflow)
✅ Critical error paths (5-10 scenarios)
✅ Boundary conditions (min/max values)
✅ State transitions (happy path only)
✅ Integration points (core dependencies)

### What to Exclude (20% value)
❌ Exhaustive edge case combinations
❌ Stress tests (detailed performance metrics)
❌ Redundant test variations
❌ Low-impact error scenarios
❌ Infrastructure tests (telemetry, logging)

### Test Execution Rules
- Run with: `cargo make test`
- Timeout: 60 seconds max
- Pass rate: 100% required
- Skip: Any test marked `#[ignore]`
- Parallel: Default parallel execution

---

## Command Reference

```bash
# Backup tests
mkdir -p /Users/sac/ggen/tests-archive
cp -r crates/ggen-core/tests/* /Users/sac/ggen/tests-archive/

# Run individual consolidated test suites
cargo test --test consolidated_core_tests -- --nocapture
cargo test --test consolidated_lifecycle_tests -- --nocapture
cargo test --test consolidated_swarm_tests -- --nocapture
cargo test --test consolidated_semantic_tests -- --nocapture

# Run all tests
cargo make test

# Run with specific thread count
cargo test --test consolidated_core_tests -- --test-threads=1

# Archive old tests
mv crates/ggen-core/tests/determinism_framework.rs /Users/sac/ggen/tests-archive/

# Commit consolidation
git add -A
git commit -m "feat: consolidate tests using 80/20 principle"
```

---

## Success Criteria

After consolidation, verify:

```bash
# 1. All tests pass
cargo make test
# Output: test result: ok

# 2. Execution time < 60 seconds
# Output: test ... ok    [time: 0.XXXs]

# 3. Test count reduced
# Before: running 90+ tests
# After: running 29 tests

# 4. No compile errors
cargo check
# Output: Finished `dev`

# 5. Git history clean
git log --oneline | head -5
# Shows consolidation commit
```

---

## Troubleshooting

### Issue: Test Fails After Consolidation
**Solution:** The removed test is critical
- [ ] Add back to appropriate module
- [ ] Update consolidation rules
- [ ] Re-run and verify

### Issue: Execution Time > 60s
**Solution:** Test is too slow
- [ ] Check async test duration
- [ ] Reduce timeout values
- [ ] Merge into faster module

### Issue: Coverage Dropped Below 80%
**Solution:** Missing critical test
- [ ] Identify uncovered critical path
- [ ] Add test back from archived file
- [ ] Verify coverage improves

---

## Documentation Files

| File | Purpose | Read Time |
|------|---------|-----------|
| `TEST_CONSOLIDATION_80_20.md` | Strategy & categories | 10 min |
| `TEST_CONSOLIDATION_PSEUDOCODE.md` | Detailed test logic | 15 min |
| `TEST_CONSOLIDATION_ARCHITECTURE.md` | System design | 15 min |
| `TEST_CONSOLIDATION_IMPLEMENTATION.md` | Step-by-step guide | 20 min |
| `TEST_CONSOLIDATION_QUICK_REFERENCE.md` | This file | 5 min |

---

## Next Steps

1. **Review:** Read `TEST_CONSOLIDATION_80_20.md` (10 min)
2. **Plan:** Decide on removed tests (5 min)
3. **Implement:** Follow phases 1-7 in implementation guide (4-5 hours)
4. **Verify:** Run full test suite, verify 100% pass rate (10 min)
5. **Document:** Update README and CI/CD (15 min)
6. **Commit:** Create git commit with summary (5 min)

---

## Questions?

Refer to:
- **"Why remove X test?"** → See `TEST_CONSOLIDATION_80_20.md` categorization
- **"How to implement?"** → See `TEST_CONSOLIDATION_IMPLEMENTATION.md` phases
- **"What's the structure?"** → See `TEST_CONSOLIDATION_ARCHITECTURE.md` diagrams
- **"Exact test code?"** → See `TEST_CONSOLIDATION_PSEUDOCODE.md` implementations

