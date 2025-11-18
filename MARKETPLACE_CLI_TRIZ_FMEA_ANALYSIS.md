# Marketplace CLI Refactoring: TRIZ & FMEA Analysis

**Date**: 2025-11-17
**Scope**: Refactor 20 CLI commands from legacy marketplace to v2/v3 RDF-backed implementation
**Methodology**: TRIZ (Theory of Inventive Problem Solving) + FMEA (Failure Mode & Effects Analysis)

---

## 1. TRIZ Analysis: Contradictions & Innovative Solutions

### Problem Definition

**Technical Contradiction**: We need to:
- ✓ Maintain backward compatibility with existing CLI commands
- ✓ Leverage new v2/v3 RDF architecture with SPARQL queries
- ✓ Improve performance (SPARQL indexing, caching)
- ✗ Without duplicating business logic or creating complex translation layers

**Resource Contradiction**:
- ✓ Minimize refactoring effort (time/cost)
- ✓ Maximize performance gains from v2/v3
- ✓ Maintain code quality and test coverage
- ✗ Without adding complexity or dependencies

---

## 2. TRIZ Solution Patterns Applied

### Pattern 1: **Segmentation** (Divide into parts)
**Problem**: All 20 commands depend on different marketplace subsystems
**Solution**: Categorize commands by dependency type and refactor in priority order

```
Priority 1 (Decoupled, safe to refactor):
  ✓ search, search-by-id, list (direct RDF queries via SPARQL)
  ✓ publish, validate (use v2 RdfRegistry)
  ✓ install, install-bundle (use AsyncRepository trait)

Priority 2 (Moderate coupling, needs adapter):
  ◐ recommend, compare, export (map legacy inputs to SPARQL)

Priority 3 (Tightly coupled, requires redesign):
  ○ maturity*, dashboard (redesign with v3 OptimizedRegistry caching)
```

### Pattern 2: **Feedback** (Use output data to drive process)
**Problem**: Don't know if new implementation breaks existing behavior
**Solution**: Create integration tests that validate both old→new output equivalence

```rust
// Test that old CLI output ≈ new CLI output for same inputs
#[test]
fn test_search_backward_compatibility() {
    let old_results = legacy_search("database");
    let new_results = sparql_search("database");
    assert!(results_equivalent(&old_results, &new_results));
}
```

### Pattern 3: **Universality** (Create multi-functional system)
**Problem**: Maintain separate code paths for v1 and v2 marketplace
**Solution**: Design adapter layer that works with both, enables gradual migration

```rust
// Adapter trait allows both old and new implementations
pub trait MarketplaceBackend: AsyncRepository {
    async fn search_semantic(&self, query: &str) -> Result<Vec<Package>>;
    async fn get_recommendations(&self, id: &PackageId) -> Result<Vec<Package>>;
}

// Implement for both legacy and v2
impl MarketplaceBackend for LegacyRegistry { /* ... */ }
impl MarketplaceBackend for RdfRegistry { /* ... */ }
```

### Pattern 4: **Preliminary Action** (Do preparatory work)
**Problem**: Can't refactor all 20 commands at once without breaking CI
**Solution**: Create feature flags for gradual rollout

```toml
# Cargo.toml
[features]
marketplace-v2 = ["ggen-marketplace-v2"]
marketplace-legacy = []
default = ["marketplace-legacy"]

# Later: default = ["marketplace-v2"]
```

### Pattern 4b: **Transformation** (Redirect harmful functions)
**Problem**: Legacy maturity assessment is expensive, slow on large datasets
**Solution**: Use v3 OptimizedRegistry caching to accelerate maturity queries

```rust
// v3 caching transforms O(n) legacy assessment into O(1) lookups
let maturity_cache = V3OptimizedRegistry::new(store)
    .with_metadata_cache_for_maturity()
    .with_ttl(Duration::from_secs(3600))
    .build();

// Lookups now <100ms instead of ~500ms
```

### Pattern 5: **System Transition** (Evolve gradually)
**Problem**: Can't flip switch from v1 to v2 in production
**Solution**: Implement parallel execution, compare outputs, then switch

```
Phase 1 (Weeks 1-2): Implement v2 adapters, hide behind feature flag
Phase 2 (Weeks 3-4): Run v1 and v2 in parallel, log differences
Phase 3 (Weeks 5-6): Route 50% traffic to v2, monitor for issues
Phase 4 (Weeks 7-8): Route 100% traffic to v2, keep v1 as fallback
Phase 5 (Weeks 9-10): Remove v1 code, celebrate migration victory
```

---

## 3. FMEA: Failure Mode & Effects Analysis

### FMEA Process
For each CLI command category, identify:
- **Failure Mode**: What could go wrong?
- **Effects**: What happens if it fails?
- **Severity** (1-10): How bad is the impact?
- **Occurrence** (1-10): How likely is it?
- **Detection** (1-10): How easily can we catch it?
- **RPN** (Risk Priority Number) = S × O × D
- **Mitigation**: How do we prevent/detect it?

---

## 4. FMEA Results by Command Category

### Category 1: Search Commands (RPN Ranking)

#### Failure: SPARQL query syntax error
- **Severity**: 7 | **Occurrence**: 3 | **Detection**: 2 | **RPN**: 42
- **Effect**: Search returns empty results unexpectedly
- **Mitigation**:
  - ✓ Unit tests for each Queries template
  - ✓ Validate SPARQL syntax at startup (not runtime)
  - ✓ Fallback to legacy search if SPARQL query fails
  - ✓ Integration tests with real oxigraph Store

#### Failure: RDF triple encoding mismatch
- **Severity**: 6 | **Occurrence**: 4 | **Detection**: 3 | **RPN**: 72
- **Effect**: Search finds packages but filters them incorrectly
- **Mitigation**:
  - ✓ Validate RDF encoding in tests (UTF-8, URI encoding)
  - ✓ Test with package names containing special characters
  - ✓ Compare old vs new results for equivalence

#### Failure: Performance regression in large datasets
- **Severity**: 5 | **Occurrence**: 2 | **Detection**: 2 | **RPN**: 20
- **Effect**: Search >200ms on 5,000+ packages (breaks SLO)
- **Mitigation**:
  - ✓ Benchmark v1 vs v2 search on reference dataset (1K-10K packages)
  - ✓ Use v3 search indexing for packages >5K
  - ✓ Monitor latency in staging before production

---

### Category 2: Install Commands (RPN Ranking)

#### Failure: Dependency resolution incompleteness
- **Severity**: 8 | **Occurrence**: 3 | **Detection**: 2 | **RPN**: 48
- **Effect**: Installation succeeds but misses dependencies, breakage in user code
- **Mitigation**:
  - ✓ Run full resolver test suite (recursive dependencies, circular deps, etc.)
  - ✓ Compare install manifests: old vs new
  - ✓ Create test packages with complex dependency graphs
  - ✓ Validate against npm/cargo real-world datasets

#### Failure: Version constraint mismatch
- **Severity**: 7 | **Occurrence**: 2 | **Detection**: 3 | **RPN**: 42
- **Effect**: Wrong version installed, breaks downstream
- **Mitigation**:
  - ✓ Unit tests for PackageVersion validation
  - ✓ Test semver constraints (^1.2.3, ~2.0, >=1.0 <3.0, etc.)
  - ✓ Edge case: pre-releases (1.0.0-beta)

#### Failure: RDF store doesn't contain expected packages
- **Severity**: 6 | **Occurrence**: 4 | **Detection**: 2 | **RPN**: 48
- **Effect**: Install command fails with "package not found" when it exists
- **Mitigation**:
  - ✓ Seed RDF store with all existing packages during migration
  - ✓ Verify migration: count(RDF packages) == count(legacy packages)
  - ✓ Create migration validation script

---

### Category 3: Validation Commands (RPN Ranking)

#### Failure: Quality scoring divergence
- **Severity**: 5 | **Occurrence**: 5 | **Detection**: 3 | **RPN**: 75
- **Effect**: Same package gets different quality score in v1 vs v2
- **Mitigation**:
  - ✓ Audit scoring algorithm (metadata validator, README, tests, etc.)
  - ✓ Create test dataset with known quality scores
  - ✓ Score 100 packages in both v1 and v2, compare distributions
  - ✓ Adjust v2 weights if variance >5%

#### Failure: Validation framework doesn't find newly added validators
- **Severity**: 4 | **Occurrence**: 2 | **Detection**: 4 | **RPN**: 32
- **Effect**: Some packages pass validation when they shouldn't
- **Mitigation**:
  - ✓ Dynamic validator loading tests
  - ✓ Ensure validators implement Validator trait
  - ✓ Integration test: add new validator, verify it's used

---

### Category 4: Maturity Assessment (RPN Ranking - HIGHEST RISK)

#### Failure: Maturity dimension calculation mismatch
- **Severity**: 8 | **Occurrence**: 6 | **Detection**: 2 | **RPN**: 96 ⚠️ CRITICAL
- **Effect**: Package appears "ready for production" when it's not (backward compatibility, security risk)
- **Mitigation**:
  - ✓ **Redesign maturity scoring using v3 caching**: Move away from legacy dimensions
  - ✓ Create dual-mode validator: old scoring for backward compat + new scoring
  - ✓ A/B test maturity scores on reference dataset (100 packages)
  - ✓ Phase out legacy maturity gradually (6-month deprecation)

#### Failure: Caching stale maturity data
- **Severity**: 6 | **Occurrence**: 3 | **Detection**: 3 | **RPN**: 54
- **Effect**: Recently-improved package still shows old (low) maturity score
- **Mitigation**:
  - ✓ v3 caching TTL: 1 hour for maturity (not 24 hours)
  - ✓ Manual cache invalidation on package update
  - ✓ Test: update package, verify score refreshes within 1 hour

#### Failure: Missing dependency graph in RDF
- **Severity**: 7 | **Occurrence**: 4 | **Detection**: 2 | **RPN**: 56
- **Effect**: Maturity assessment can't traverse dependency tree
- **Mitigation**:
  - ✓ Ensure hasDepedency triples created for all versions
  - ✓ Test: build dependency graph from RDF, validate completeness
  - ✓ Migration: verify all legacy deps migrated to RDF

---

## 5. Risk Priority Summary

| Category | Highest RPN | Risk Level | Mitigation Phase |
|----------|-------------|-----------|------------------|
| Maturity Assessment | 96 (Critical) | 🔴 HIGH | Phase 1 (design new scoring) |
| Quality Scoring | 75 (High) | 🟠 MEDIUM | Phase 2 (validation testing) |
| Install: Dependencies | 48 | 🟠 MEDIUM | Phase 1 (resolver tests) |
| Install: RDF Incompleteness | 48 | 🟠 MEDIUM | Phase 1 (migration validation) |
| Maturity: Stale Cache | 54 | 🟠 MEDIUM | Phase 2 (caching tuning) |
| Maturity: Deps Missing | 56 | 🟠 MEDIUM | Phase 1 (RDF migration) |
| Search: Version Mismatch | 42 | 🟡 LOW | Phase 1 (unit tests) |
| Search: SPARQL Errors | 42 | 🟡 LOW | Phase 0 (schema validation) |

---

## 6. Refactoring Strategy: TRIZ-FMEA Integrated Approach

### Phase 0: Preparation (Weeks 1)
**TRIZ Principle**: Preliminary Action
**FMEA Focus**: Prevention (detect issues before they happen)

**Tasks**:
- [ ] Validate all SPARQL query templates at startup (catch syntax errors early)
- [ ] Create reference dataset (100-1,000 packages for testing)
- [ ] Build migration validation script (counts, checksums, sampling)
- [ ] Set up feature flags for gradual rollout

**Deliverable**: `MARKETPLACE_CLI_REFACTOR_PLAN.md` with detailed timeline

---

### Phase 1: Priority 1 Commands (Weeks 2-3)
**TRIZ Principle**: Segmentation (start with decoupled commands)
**FMEA Focus**: Detection (test thoroughly before going live)

**Target Commands**:
- `search` → Use SparqlSearchEngine
- `search-by-id` → Query by RDF URI
- `list` → SPARQL all_packages()
- `publish` → Use RdfRegistry.insert_package_rdf()
- `validate` → Use PackageValidator on v2 models

**Implementation**:
```rust
// New adapter in ggen-domain/marketplace/
pub trait MarketplaceRegistry: AsyncRepository + Send + Sync {
    async fn search_semantic(&self, query: &str) -> Result<Vec<String>>;
    async fn insert_package_rdf(&self, package: &Package) -> Result<()>;
}

// Implement for v2
impl MarketplaceRegistry for RdfRegistry { /* ... */ }

// Update CLI commands to use trait
#[verb]
async fn search(
    #[noun] query: String,
    marketplace: &dyn MarketplaceRegistry,
) -> Result<()> {
    let results = marketplace.search_semantic(&query).await?;
    // Display results
}
```

**Mitigation (from FMEA)**:
- [ ] SPARQL query validation tests (RPN 42)
- [ ] RDF encoding tests with special chars (RPN 72)
- [ ] Performance benchmarks: v1 vs v2 (RPN 20)
- [ ] Integration tests: old results ≈ new results

---

### Phase 2: Priority 2 Commands (Weeks 4-5)
**TRIZ Principle**: Feedback (validate before expanding)
**FMEA Focus**: Adaptation (learn from Phase 1 tests)

**Target Commands**:
- `recommend` → Map legacy logic to SPARQL queries
- `compare` → Compare packages via RDF properties
- `export` → Export RDF data in multiple formats

**Implementation**:
```rust
// Use v3 OptimizedRegistry for caching expensive operations
let registry = V3OptimizedRegistry::new(store).await;

// recommend now uses cached queries
let recommendations = registry.get_recommendations(&package_id).await?;
```

**Mitigation (from FMEA)**:
- [ ] A/B test recommend outputs (validate equivalence)
- [ ] Cache hit rate monitoring for export operations

---

### Phase 3: Priority 3 Commands - Maturity (Weeks 6-8)
**TRIZ Principle**: System Transition + Transformation (major redesign)
**FMEA Focus**: Redesign (RPN 96 requires fundamental change)

**Target Commands**:
- `maturity` → New scoring using v2 validators
- `dashboard` → Use v3 caching for performance
- `maturity-batch` → Process via v3 parallel caching

**NEW MATURITY DESIGN** (addresses RPN 96):
```rust
// Old (6 dimensions, slow, complex weights)
// → New (quantitative, validator-driven, cached)

pub struct MaturityAssessment {
    // Derived from PackageValidator results
    pub has_readme: bool,
    pub has_tests: bool,
    pub has_license: bool,
    pub has_repository: bool,
    pub documentation_completeness: f64,  // 0.0-1.0
    pub test_coverage: f64,                 // 0.0-1.0
    pub security_issues_found: usize,
    pub dependency_quality_avg: f64,

    // Composite score (cached, TTL 1 hour)
    pub quality_score: QualityScore,  // 1-100
    pub readiness_tier: ProductionTier,  // Alpha/Beta/Stable/Production
    pub cached_at: DateTime<Utc>,
    pub cache_ttl: Duration,
}

impl MaturityAssessment {
    /// Score is calculated once, cached in v3, expires after TTL
    pub async fn calculate(
        package: &Package,
        registry: &V3OptimizedRegistry,
    ) -> Result<Self> {
        // Use v3 metadata cache for repeated assessments
        // RPN 54 mitigated: 1-hour TTL keeps data fresh
        // RPN 96 mitigated: quantitative scoring, not fuzzy
    }
}
```

**Mitigation (from FMEA - all CRITICAL/HIGH items)**:
- [ ] A/B test maturity scores (100 packages, compare distributions)
- [ ] Ensure all dependency RDF triples exist (migration validation)
- [ ] Test cache TTL behavior (RPN 54)
- [ ] 6-month deprecation path for old maturity assessment

---

### Phase 4: Parallel Execution & Validation (Weeks 9-10)
**TRIZ Principle**: System Transition (gradual switchover)
**FMEA Focus**: Verification (run both systems, detect divergence)

**Implementation**:
```rust
// Feature flag controls which implementation is used
#[cfg(feature = "marketplace-v2")]
pub use new_marketplace::*;

#[cfg(not(feature = "marketplace-v2"))]
pub use legacy_marketplace::*;

// In tests: run both, compare outputs
#[cfg(test)]
mod compatibility_tests {
    #[tokio::test]
    async fn search_v1_vs_v2_equivalence() {
        let v1_results = legacy_search("database").await?;
        let v2_results = new_search("database").await?;
        assert!(results_equivalent(&v1_results, &v2_results));
    }
}
```

**Validation**:
- [ ] Run v1 and v2 in parallel for 2 weeks
- [ ] Log all differences to file
- [ ] Analyze differences: expected variations vs bugs
- [ ] Fix bugs found in v2 before switchover

---

### Phase 5: Cutover & Legacy Removal (Weeks 11+)
**TRIZ Principle**: System Transition (complete switchover)

**Steps**:
1. Switch default feature: `marketplace-v2` enabled
2. Run for 2 weeks with fallback enabled
3. Monitor production metrics (latency, errors, cache hit rates)
4. Remove legacy code, deprecation warnings

---

## 7. Testing Strategy (FMEA-Driven)

### Test Categories (Ordered by RPN)

| RPN | Category | Test Type | Scope |
|-----|----------|-----------|-------|
| 96 | Maturity Scoring | A/B comparison | 100 reference packages |
| 75 | Quality Scoring | Distribution analysis | 100 packages |
| 56 | RDF Dependency Graph | Migration validation | 100% coverage check |
| 54 | Maturity Cache Staleness | TTL verification | 10 cache updates |
| 48 | Install Dependencies | Resolver test suite | 50 complex graphs |
| 48 | RDF Migration | Completeness check | All packages |
| 42 | SPARQL Syntax | Query validation | All Queries templates |
| 42 | Version Constraints | Semver tests | 20 constraint patterns |
| 20 | Performance Regression | Latency benchmarks | 1K-10K datasets |

### Test Infrastructure

```rust
// tests/fmea_validation.rs
#[tokio::test]
async fn fmea_riskpriority_96_maturity_scoring() {
    // RPN 96: Maturity dimension calculation mismatch
    let reference_packages = load_reference_dataset(100);

    for pkg in reference_packages {
        let v1_maturity = legacy_assessment(&pkg).await?;
        let v2_maturity = new_assessment(&pkg).await?;

        // Allow 5% variance in quality scores
        assert_quality_score_equivalent(
            v1_maturity.quality_score,
            v2_maturity.quality_score,
            tolerance = 0.05,
        );
    }
}

#[tokio::test]
async fn fmea_riskpriority_75_quality_scoring() {
    // RPN 75: Quality scoring divergence
    let packages = load_reference_dataset(100);
    let v1_scores: Vec<u32> = packages.iter()
        .map(|p| legacy_quality_score(p))
        .collect();
    let v2_scores: Vec<u32> = packages.iter()
        .map(|p| new_quality_score(p))
        .collect();

    let v1_distribution = calculate_distribution(&v1_scores);
    let v2_distribution = calculate_distribution(&v2_scores);

    // Distributions should be similar (chi-square test)
    assert!(chi_square_test(&v1_distribution, &v2_distribution) > 0.05);
}
```

---

## 8. Success Criteria

### TRIZ Principles Achieved
- ✓ **Segmentation**: Commands categorized and refactored in priority order (reduced risk)
- ✓ **Feedback**: Integration tests validate equivalence (early detection)
- ✓ **Universality**: Adapter trait enables both v1 and v2 (smooth transition)
- ✓ **Preliminary Action**: Feature flags enable gradual rollout (reduced blast radius)
- ✓ **System Transition**: 5-phase approach minimizes disruption (managed risk)

### FMEA Mitigations Implemented
- ✓ All RPN >50 have explicit detection/prevention tests
- ✓ Critical items (RPN 96) redesigned, not patched
- ✓ High-risk items (RPN >70) have A/B validation
- ✓ Performance regressions detected via benchmarks

### Delivery Goals
- [ ] Phase 0-1: 2 weeks (search, list, publish commands)
- [ ] Phase 2: 1 week (recommend, compare, export)
- [ ] Phase 3: 2 weeks (maturity redesign)
- [ ] Phase 4-5: 2 weeks (validation, cutover)
- [ ] **Total: ~7 weeks** to complete migration

### Quality Metrics
- Test coverage: >90% for migrated commands
- Performance: v2 search <200ms (SLO met)
- Backward compatibility: >95% result equivalence (old vs new)
- Cache hit rate: >75% for v3 (target met)

---

## 9. Decision Records

### DR-1: Why not just refactor everything at once?
**Decision**: Use phased approach with feature flags
**Rationale**: FMEA identified RPN 96 critical risk. Phased approach allows testing, validation, and rollback at each stage.

### DR-2: Why redesign maturity scoring?
**Decision**: New quantitative model based on validators, cached via v3
**Rationale**: FMEA RPN 96 indicates current fuzzy 6-dimension scoring is broken. New model is more testable, measurable, and faster.

### DR-3: Why not migrate all RDF at once?
**Decision**: Gradual migration with verification at each stage
**Rationale**: FMEA RPN 48 (RDF incompleteness). Validation script ensures no packages lost in translation.

---

## Appendix: TRIZ Contradiction Matrix Reference

| Contradiction | TRIZ Principles | Applied |
|---|---|---|
| Backward compat vs new features | 15 (Feedback), 8 (Universality) | ✓ Feature flags + adapter trait |
| Performance vs complexity | 35 (Parameter transformation), 19 (Transition) | ✓ v3 caching + phased rollout |
| Testing effort vs coverage | 2 (Taking out), 10 (Preliminary action) | ✓ FMEA-driven tests, focus on high-RPN items |
| Code quality vs speed | 16 (Partial/Excessive action), 30 (Flexible shells) | ✓ Strict code review phase 1-2, flexible phase 3 |

---

**Document Status**: READY FOR IMPLEMENTATION
**Next Step**: Begin Phase 0 (Preparation)
**Approval**: Awaiting user confirmation to proceed with Phase 0 tasks

