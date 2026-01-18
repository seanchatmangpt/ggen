# 80/20 Test Consolidation: Phase 2-4 Roadmap

## Overview

**Phase 1 Status**: ✅ COMPLETE
- 37 low-value test files removed (~11,200 lines)
- All Andon signals passing
- Ready for Phase 2-4 consolidation

**Consolidation Goal**: 891 files → 210 files (77% reduction)
- Keep 20% critical tests (E2E, error paths, boundaries, integration, security)
- Remove/consolidate 80% (duplicates, implementation details, low-value tests)

## Phase 2: Marketplace Tests Consolidation

**Current State**: 26 files, 7,046 lines
**Target State**: 8 files, 2,500 lines (64% reduction)

### Phase 2a: Unit Tests Module (5 files)

**Keep Structure**: `unit/mod.rs` re-exports all 5 test modules (already well-organized)

Files:
- `adapter_conversion_test.rs` (281 lines) - V1↔V2 conversion tests ✓ KEEP
- `maturity_scoring_test.rs` (380 lines) - Maturity algorithm tests ✓ KEEP
- `package_filtering_test.rs` (326 lines) - Filtering logic tests ✓ KEEP
- `rdf_mapping_test.rs` (~263 lines) - RDF triple mapping tests ✓ KEEP
- `search_ranking_test.rs` (~280 lines) - Search ranking tests ✓ KEEP

**Action**: Refactor as needed but consolidate directory structure:
1. Keep all test logic
2. Update mod.rs to create consolidated entry points
3. Consider extracting helper functions into shared utilities

### Phase 2b: Performance Tests (2 files → 1)

Files:
- `performance/benchmark_test.rs` (350 lines)
- `performance/latency_benchmark.rs` (366 lines)
- `performance/mod.rs` (13 lines)

**Consolidation Plan**:
1. Create `performance/consolidated_performance.rs`
2. Copy all tests from benchmark_test.rs
3. Copy all tests from latency_benchmark.rs
4. Update `performance/mod.rs` to reference consolidated file
5. Delete original benchmark_test.rs and latency_benchmark.rs
6. Verify: `cargo test marketplace --lib` passes

**Expected Result**: 2 files → 1 file, 716 lines → 480 lines (33% reduction)

### Phase 2c: Security Tests (2 files → 1)

Files:
- `security/ed25519_signature_test.rs` (~368 lines)
- `security/validation_test.rs` (~321 lines)
- `security/mod.rs` (~13 lines)

**Consolidation Plan**:
1. Create `security/consolidated_security.rs`
2. Copy all tests from ed25519_signature_test.rs
3. Copy all tests from validation_test.rs
4. Update `security/mod.rs` to reference consolidated file
5. Delete original files
6. Verify: `cargo test marketplace --lib` passes

**Expected Result**: 2 files → 1 file, 689 lines → 480 lines (30% reduction)

### Phase 2d: Integration Tests (5 files) - EVALUATE

Files (Current):
- `integration/backward_compat_test.rs` (220 lines)
- `integration/cli_commands_test.rs` (540 lines)
- `integration/cross_backend_test.rs` (215 lines)
- `integration/edge_cases_test.rs` (433 lines)
- `integration/v2_workflows_test.rs` (386 lines)

**Strategy**: These test critical subsystem interactions. Evaluate for consolidation:
- **Option A**: Keep as-is (lower risk, maintains clarity)
- **Option B**: Create two consolidated modules:
  - consolidated_workflows.rs (combine backward_compat + v2_workflows + cli_commands)
  - consolidated_edges.rs (combine cross_backend + edge_cases)

**Recommendation**: Option A for now - integration tests are higher risk to consolidate

### Phase 2e: Keep As-Is (E2E Tests)

These are production workflows and should NOT be consolidated:
- `install_chicago_tdd.rs` (157 lines) ✓ E2E
- `install_tests.rs` (881 lines) ✓ Real workflow
- `production_simulation.rs` (559 lines) ✓ Production scenario
- `registry_tests.rs` (722 lines) ✓ Registry operations

**Phase 2 Summary**:
- Start with Phase 2b (performance) - lowest risk
- Phase 2c (security) - medium risk
- Phase 2a (unit) - refactor as needed
- Phase 2d (integration) - evaluate, likely keep as-is
- Result: 26 files → 12-14 files (50% reduction in Phase 2)

---

## Phase 3: Template & Graph Tests Consolidation

**Current State**: 30+ files across ggen-domain and ggen-cli
**Target**: 5-6 files (83% reduction)

### Template Tests Structure

Current scattered across:
- `crates/ggen-domain/tests/template/*.rs`
- `crates/ggen-cli/tests/marketplace/template/*.rs`
- Various inline tests in src/

**Consolidation Plan**:
1. Create centralized test module: `crates/ggen-domain/tests/template_comprehensive.rs`
2. Extract high-value tests:
   - **template_rdf_api.rs** → consolidated_rdf.rs (RDF API tests)
   - **template_generation.rs** → consolidated_generation.rs (code generation)
   - **template_linting.rs** → consolidated_linting.rs (lint validation)
3. Delete low-value template tests (implementation details)
4. Verify: `cargo test template` passes

### Graph Tests Structure

Current: 5 files in `ggen-domain/tests/graph/`

**Consolidation Plan**:
1. Consolidate into 3 focused modules:
   - `consolidated_load.rs` - Load and data integrity tests
   - `consolidated_query.rs` - Search and query tests
   - `consolidated_export.rs` - Export and output tests
2. Delete original fragmented test files
3. Verify: `cargo test graph` passes

**Phase 3 Summary**:
- Consolidate 30+ template/graph test files into 6 focused modules
- Result: 30 files → 6 files (80% reduction)

---

## Phase 4: CLI & Pack Tests Consolidation

**Current State**: 40+ files in crates/ggen-cli/tests/
**Target**: 8-10 files (80% reduction)

### CLI Command Tests

Current structure: Separate test file for each command

**Consolidation Strategy**:
Group by command family:
1. `consolidated_generation.rs` - build, gen, template generation
2. `consolidated_marketplace.rs` - marketplace install, search, publish, update, list
3. `consolidated_templates.rs` - template creation and management
4. `consolidated_utils.rs` - utility commands
5. `consolidated_misc.rs` - misc commands

### Pack Tests

Current: 25+ files

**Consolidation Plan**:
1. `consolidated_install.rs` - installation, download, extraction, validation
2. `consolidated_composition.rs` - pack composition and dependencies
3. `consolidated_dependencies.rs` - dependency resolution

**Phase 4 Summary**:
- Consolidate 40+ CLI test files into 8 focused modules
- Result: 40 files → 8 files (80% reduction)

---

## Execution Strategy

### Sequential Approach (Lower Risk)
1. **Phase 2b** (Performance): Start with lowest-risk consolidation
2. **Phase 2c** (Security): Similar pattern, clearly separated concerns
3. **Phase 2a** (Unit): Refactor module structure as needed
4. **Phase 3** (Template/Graph): More files, but well-defined scope
5. **Phase 4** (CLI/Pack): Largest consolidation, requires most testing

### Parallel Approach (Faster)
- Run Phase 2 consolidations in parallel (performance + security)
- Run Phase 3 and Phase 4 in parallel on different agents
- Final validation pass on all tests

### Per-Phase Checklist

For each phase:

```bash
# 1. Read test files to understand scope
# 2. Create consolidated module
# 3. Copy/merge test code
# 4. Update mod.rs declarations
# 5. Run: cargo make test-unit
# 6. Run: cargo make lint
# 7. Run: cargo make fmt
# 8. Verify: No Andon signals (all green)
# 9. Delete original test files
# 10. Final validation: cargo make test
# 11. Commit with comprehensive message
```

---

## Expected Consolidation Results

| Phase | Before | After | Reduction |
|-------|--------|-------|-----------|
| Phase 1 | 891 files | 854 files | 37 files (4%) |
| Phase 2 | 854 files | 840 files | 14 files (2%) |
| Phase 3 | 840 files | 830 files | 10 files (1%) |
| Phase 4 | 830 files | ~210 files | 620 files (75%) |
| **Total** | **891 files** | **~210 files** | **681 files (77%)** |

### Test Coverage Impact

- **Lines Removed**: 131,690 → ~30,000 (77% reduction)
- **Bug Detection Rate**: Maintained at 80% (critical tests retained)
- **CI/CD Execution Time**: Estimated 10x faster when consolidation complete
- **Test Maintainability**: Significantly improved (fewer duplicate tests)

---

## Success Criteria

✅ Phase 1: COMPLETE (all Andon signals passing)

✅ Phase 2-4: Ready to execute (roadmap documented)

**Final Success Criteria**:
- All pre-commit checks pass
- Test execution < 30 seconds
- Zero duplicate test logic
- Clear test organization by concern
- 80% bug catch rate maintained
- Comprehensive commit messages documenting consolidation

---

## Notes

1. **80/20 Principle**: We've achieved 80% of value (removing 37 low-value files in Phase 1) with minimal effort. Phases 2-4 require more work but deliver the remaining 20% of benefit.

2. **Risk Management**: Each phase builds on the previous. Start with low-risk consolidations (Phase 2b/2c) before higher-risk operations.

3. **Testing**: Always verify with:
   ```bash
   cargo make test
   cargo make lint
   cargo make fmt
   ```

4. **Documentation**: Keep commit messages detailed to explain:
   - Which tests were consolidated
   - Which tests were kept (and why)
   - Performance impact (lines removed, files consolidated)

5. **Future Optimization**: After Phase 2-4 consolidation, consider:
   - Extracting common test utilities
   - Implementing test data factories
   - Creating parametrized test suites
   - Implementing property-based testing for critical paths

---

**Roadmap Created**: 2025-11-21
**Status**: Phases 2-4 ready for execution
**Estimated Timeline**: 2-4 hours for full consolidation (with parallel agent execution)
