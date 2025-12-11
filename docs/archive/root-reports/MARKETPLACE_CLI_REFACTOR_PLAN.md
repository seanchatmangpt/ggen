# Marketplace CLI Refactoring Plan
## Phase 0: Preparation (Week 1)

**Objective**: Build foundations for safe, validated refactoring of 20 CLI commands

**Success Criteria**:
- ✓ Feature flags infrastructure in place
- ✓ MarketplaceRegistry adapter trait designed
- ✓ SPARQL query validation framework created
- ✓ Reference dataset (100 packages) built
- ✓ Migration validation script ready

**Timeline**: 5 working days

---

## Task 0.1: Design MarketplaceRegistry Adapter Trait

**File**: `crates/ggen-domain/marketplace/adapter.rs` (NEW)

**Purpose**: Define unified interface supporting both v1 (legacy) and v2 (RDF) implementations

**Implementation**:

```rust
use async_trait::async_trait;
use crate::models::{Package, PackageId, PackageVersion, SearchResult};
use crate::error::Result;

/// Unified marketplace registry interface supporting v1 and v2
///
/// This trait enables gradual migration from legacy marketplace to
/// v2 RDF-backed implementation without breaking existing CLI commands.
///
/// - v1 implementation: wraps legacy registry
/// - v2 implementation: wraps RdfRegistry + SparqlSearchEngine
#[async_trait]
pub trait MarketplaceRegistry: Send + Sync {
    // Core operations
    async fn get_package(&self, id: &PackageId) -> Result<Package>;
    async fn search(&self, query: &str) -> Result<Vec<String>>;
    async fn list_all(&self) -> Result<Vec<Package>>;
    async fn publish(&self, package: &Package) -> Result<()>;

    // Search variants
    async fn search_by_keyword(&self, keyword: &str) -> Result<Vec<String>>;
    async fn search_by_author(&self, author: &str) -> Result<Vec<String>>;
    async fn search_by_quality(&self, min_score: u32) -> Result<Vec<String>>;

    // Discovery
    async fn trending_packages(&self, limit: usize) -> Result<Vec<String>>;
    async fn recent_packages(&self, limit: usize) -> Result<Vec<String>>;

    // Validation
    async fn validate_package(&self, package: &Package) -> Result<ValidationResult>;

    // Advanced
    async fn get_recommendations(&self, id: &PackageId) -> Result<Vec<Package>>;
    async fn compare_packages(&self, ids: &[PackageId]) -> Result<Vec<ComparisonResult>>;
}

/// Results of package validation
#[derive(Clone, Debug)]
pub struct ValidationResult {
    pub package_id: PackageId,
    pub is_valid: bool,
    pub quality_score: u32,
    pub issues: Vec<String>,
    pub warnings: Vec<String>,
}

/// Results of comparing packages
#[derive(Clone, Debug)]
pub struct ComparisonResult {
    pub package_id: PackageId,
    pub properties: std::collections::HashMap<String, String>,
}
```

**Dependencies**:
- [ ] Ensure `async_trait` is in workspace (YES - already in Cargo.lock)
- [ ] Define error types and Result in error.rs
- [ ] Define SearchResult, ComparisonResult types

**Testing**:
```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_adapter_trait_object_safety() {
        // Verify trait is object-safe (can be used as &dyn MarketplaceRegistry)
        let _: &dyn MarketplaceRegistry;
    }
}
```

---

## Task 0.2: Add Feature Flags to Cargo.toml

**File**: `crates/ggen-cli/Cargo.toml`

**Purpose**: Control which marketplace implementation is compiled/used

**Implementation**:

```toml
[package]
name = "ggen-cli"
version = "0.1.0"
edition = "2021"

[features]
default = ["marketplace-v1"]
marketplace-v1 = ["ggen-domain/marketplace-v1"]    # Legacy (current default)
marketplace-v2 = ["ggen-marketplace-v2"]           # RDF-backed (new)
marketplace-parallel = ["marketplace-v1", "marketplace-v2"]  # Both (for testing)

[dependencies]
ggen-domain = { path = "../ggen-domain", features = ["marketplace-v1"] }
ggen-marketplace-v2 = { path = "../ggen-marketplace-v2", optional = true }

[dev-dependencies]
# Features for testing
```

**Also in `crates/ggen-domain/Cargo.toml`**:

```toml
[features]
default = ["marketplace-v1"]
marketplace-v1 = []
marketplace-v2 = ["ggen-marketplace-v2"]
marketplace-parallel = ["marketplace-v1", "marketplace-v2"]
```

**Usage in code**:

```rust
// marketplace/mod.rs
#[cfg(feature = "marketplace-v1")]
pub mod v1;

#[cfg(feature = "marketplace-v2")]
pub mod v2;

#[cfg(feature = "marketplace-parallel")]
pub mod parallel_executor;  // For A/B testing
```

**Testing**:
```bash
# Current default (v1)
cargo build

# Build with v2 only
cargo build --features marketplace-v2

# Build with both (for validation)
cargo build --features marketplace-parallel
```

---

## Task 0.3: Create SPARQL Query Validation Framework

**File**: `crates/ggen-marketplace-v2/src/ontology_validation.rs` (NEW)

**Purpose**: Validate all SPARQL queries at startup (catch syntax errors early)

**Implementation**:

```rust
use crate::ontology::Queries;
use oxigraph::store::Store;

/// Validates all SPARQL queries in Queries module
///
/// Called at startup to ensure queries are syntactically correct.
/// Prevents runtime failures from malformed SPARQL.
pub async fn validate_all_queries(store: &Store) -> Result<ValidationReport> {
    let mut report = ValidationReport::default();

    // Test all_packages query
    match validate_query(store, Queries::all_packages()) {
        Ok(_) => report.all_packages = Ok(()),
        Err(e) => report.all_packages = Err(e),
    }

    // Test search_by_name (with example)
    match validate_query(store, &Queries::search_by_name("test")) {
        Ok(_) => report.search_by_name = Ok(()),
        Err(e) => report.search_by_name = Err(e),
    }

    // ... test all other queries ...

    Ok(report)
}

async fn validate_query(store: &Store, query: &str) -> Result<()> {
    // Attempt to parse and execute query on empty store
    store.query(query)
        .map_err(|e| Error::SparqlError(format!("Query validation failed: {}", e)))?;
    Ok(())
}

#[derive(Default, Debug)]
pub struct ValidationReport {
    pub all_packages: Result<()>,
    pub search_by_name: Result<()>,
    pub search_by_description: Result<()>,
    pub packages_by_keyword: Result<()>,
    pub packages_by_author: Result<()>,
    pub packages_by_quality: Result<()>,
    pub trending_packages: Result<()>,
    pub recent_packages: Result<()>,
    pub package_versions: Result<()>,
}

impl ValidationReport {
    pub fn is_valid(&self) -> bool {
        self.all_packages.is_ok()
            && self.search_by_name.is_ok()
            && self.search_by_description.is_ok()
            && self.packages_by_keyword.is_ok()
            && self.packages_by_author.is_ok()
            && self.packages_by_quality.is_ok()
            && self.trending_packages.is_ok()
            && self.recent_packages.is_ok()
            && self.package_versions.is_ok()
    }

    pub fn errors(&self) -> Vec<String> {
        let mut errors = Vec::new();
        // Collect all errors
        Ok(errors)
    }
}
```

**Usage in v2 initialization**:

```rust
// In RdfRegistry::new()
pub async fn new(primary_store: Arc<Store>) -> Result<Self> {
    // Validate SPARQL queries before creating registry
    let validation = ontology_validation::validate_all_queries(&primary_store).await?;

    if !validation.is_valid() {
        let errors = validation.errors();
        return Err(Error::InitializationError(
            format!("SPARQL query validation failed: {:?}", errors)
        ));
    }

    // ... rest of initialization ...
}
```

---

## Task 0.4: Create Reference Dataset (100 Packages)

**File**: `tests/fixtures/reference_packages.json` (NEW)

**Purpose**: Standardized test dataset for validation and A/B testing

**Structure**:

```json
{
  "packages": [
    {
      "id": "test-package-001",
      "name": "Database ORM",
      "description": "Object-relational mapping library",
      "version": "1.2.3",
      "author": "John Doe",
      "quality_score": 85,
      "has_readme": true,
      "has_tests": true,
      "has_license": true,
      "has_repository": true,
      "test_coverage": 0.87,
      "documentation_completeness": 0.92,
      "dependencies": ["serde", "tokio"],
      "downloads": 15000,
      "created_at": "2023-01-15",
      "maturity_v1_expected": {
        "dimension_docs": 0.95,
        "dimension_testing": 0.85,
        "dimension_security": 0.80,
        "dimension_performance": 0.75,
        "dimension_adoption": 0.70,
        "dimension_maintenance": 0.65,
        "overall_score": 81
      }
    },
    // ... 99 more packages with varying characteristics ...
  ]
}
```

**Dataset Characteristics** (100 packages):
- 20 high-quality (quality_score 90-100)
- 30 medium-quality (quality_score 70-89)
- 30 low-quality (quality_score 50-69)
- 20 problematic (quality_score <50)

**Includes edge cases**:
- Packages with special characters in names
- Packages with no dependencies
- Packages with circular dependency patterns
- Packages with missing fields
- Pre-release versions (1.0.0-beta)
- Very large version numbers

**Usage**:

```rust
#[test]
fn load_reference_dataset() {
    let dataset = ReferenceDataset::from_file("tests/fixtures/reference_packages.json");
    assert_eq!(dataset.packages.len(), 100);
}
```

**Generation Script** (optional): `scripts/generate_reference_dataset.py`
- Generates realistic package metadata
- Calculates v1 maturity scores for comparison baseline
- Ensures diversity in quality scores

---

## Task 0.5: Create Migration Validation Script

**File**: `scripts/validate_marketplace_migration.rs` (NEW)

**Purpose**: Verify RDF migration completeness before Phase 1

**Functionality**:

```rust
use std::path::PathBuf;

/// Validates that all legacy marketplace packages migrated to RDF correctly
pub async fn validate_migration(
    legacy_registry: &dyn MarketplaceRegistry,
    rdf_registry: &dyn MarketplaceRegistry,
) -> Result<MigrationReport> {
    let mut report = MigrationReport::default();

    // 1. Count check: legacy packages == RDF packages
    let legacy_all = legacy_registry.list_all().await?;
    let rdf_all = rdf_registry.list_all().await?;

    report.total_packages_legacy = legacy_all.len();
    report.total_packages_rdf = rdf_all.len();
    report.count_match = legacy_all.len() == rdf_all.len();

    if !report.count_match {
        report.errors.push(format!(
            "Package count mismatch: {} legacy vs {} RDF",
            legacy_all.len(), rdf_all.len()
        ));
    }

    // 2. Sampling check: Random sample of packages match
    let sample_size = std::cmp::min(100, legacy_all.len());
    let mut rng = rand::thread_rng();
    let sample: Vec<_> = legacy_all
        .iter()
        .choose_multiple(&mut rng, sample_size);

    for package in sample {
        match validate_package_migration(legacy_registry, rdf_registry, &package.id).await {
            Ok(_) => report.packages_valid += 1,
            Err(e) => {
                report.packages_invalid += 1;
                report.errors.push(format!("Package {} migration failed: {}", package.id, e));
            }
        }
    }

    report.sample_validity_rate =
        report.packages_valid as f64 / sample_size as f64;

    // 3. Checksum validation (if available)
    // Hash all package metadata, compare old vs new

    // 4. Dependency completeness check
    report.dependency_integrity = validate_all_dependencies(rdf_registry).await?;

    Ok(report)
}

#[derive(Default, Debug)]
pub struct MigrationReport {
    pub total_packages_legacy: usize,
    pub total_packages_rdf: usize,
    pub count_match: bool,
    pub packages_valid: usize,
    pub packages_invalid: usize,
    pub sample_validity_rate: f64,
    pub dependency_integrity: bool,
    pub errors: Vec<String>,
}

impl MigrationReport {
    pub fn is_valid(&self) -> bool {
        self.count_match
            && self.sample_validity_rate > 0.95
            && self.dependency_integrity
            && self.errors.is_empty()
    }
}
```

**Usage**:

```bash
# Run migration validation before deploying
cargo run --bin validate-marketplace-migration \
    --features marketplace-v2
```

**Output**:

```
Migration Validation Report
===========================
Legacy packages:        1,234
RDF packages:           1,234
Count match:            ✓ PASS
Sample validity rate:   99.7% ✓ PASS
Dependency integrity:   ✓ PASS
Overall:                ✓ VALID FOR MIGRATION
```

---

## Task 0.6: Create FMEA-Driven Test Infrastructure

**File**: `tests/fmea_tests.rs` (NEW)

**Purpose**: Test framework specifically for validating FMEA mitigations

**Structure**:

```rust
use ggen_marketplace_v2::prelude::*;

mod fmea_tests {
    use super::*;

    /// RPN 96: Maturity Scoring Divergence
    ///
    /// Validates that maturity scores between v1 and v2 differ by <5%
    #[tokio::test]
    #[ignore = "requires reference dataset"]
    async fn fmea_rp96_maturity_scoring_divergence() {
        let reference_packages = load_reference_dataset(100);
        let tolerance = 0.05;  // 5% allowed variance

        for pkg in reference_packages {
            let v1_score = legacy_quality_score(&pkg);
            let v2_score = new_quality_score(&pkg).await?;

            let variance = (v1_score as f64 - v2_score as f64).abs()
                / v1_score as f64;

            assert!(variance < tolerance,
                "Package {} quality score variance {:.1}% exceeds {:.0}% tolerance",
                pkg.id, variance * 100.0, tolerance * 100.0);
        }
    }

    /// RPN 75: Quality Score Divergence
    ///
    /// Validates distribution of quality scores is similar
    #[tokio::test]
    #[ignore = "requires reference dataset"]
    async fn fmea_rp75_quality_score_distribution() {
        let packages = load_reference_dataset(100);

        let v1_scores: Vec<u32> = packages.iter()
            .map(|p| legacy_quality_score(p))
            .collect();

        let v2_scores: Vec<u32> = packages.iter()
            .map(|p| new_quality_score(p))
            .collect();

        let v1_dist = distribution_analysis(&v1_scores);
        let v2_dist = distribution_analysis(&v2_scores);

        // Use chi-square test (p-value > 0.05 = not significantly different)
        let chi_square = chi_square_statistic(&v1_dist, &v2_dist);
        assert!(chi_square > 0.05,
            "Quality score distributions significantly different (p={:.4})",
            chi_square);
    }

    /// RPN 56: Missing Dependency Graph in RDF
    ///
    /// Validates all packages have complete dependency data in RDF
    #[tokio::test]
    async fn fmea_rp56_rdf_dependency_completeness() {
        let store = setup_rdf_store().await?;
        let packages = load_reference_dataset(100);

        for pkg in packages {
            // Query RDF for all dependencies
            let query = format!(
                "SELECT ?dep WHERE {{ <ggen:packages/{}> <ggen:properties/hasDependency> ?dep }}",
                pkg.id
            );

            let rdf_deps: Vec<_> = store.query(&query)
                .map_err(|e| anyhow!("Query failed: {}", e))?
                .into_iter()
                .collect();

            // Should match expected dependencies
            assert_eq!(rdf_deps.len(), pkg.dependencies.len(),
                "Package {} has {} RDF deps but expected {}",
                pkg.id, rdf_deps.len(), pkg.dependencies.len());
        }
    }

    /// RPN 48: Install Dependency Resolution Incompleteness
    ///
    /// Validates dependency resolver finds all transitive dependencies
    #[tokio::test]
    async fn fmea_rp48_dependency_resolution() {
        let store = setup_rdf_store().await?;
        let registry = RdfRegistry::new(Arc::new(store)).await?;

        // Test with complex dependency graph
        let test_cases = vec![
            ("simple-linear", vec!["dep1", "dep2", "dep3"]),
            ("with-shared-deps", vec!["dep1", "dep2", "shared"]),
            ("circular", vec!["a", "b", "c"]),  // Should detect and skip
        ];

        for (test_name, expected_deps) in test_cases {
            let resolved = registry.resolve_dependencies(test_name).await?;

            for expected in expected_deps {
                assert!(resolved.contains(&expected.to_string()),
                    "Test {}: Missing dependency '{}'", test_name, expected);
            }
        }
    }
}

fn load_reference_dataset(count: usize) -> Vec<Package> {
    // Load from tests/fixtures/reference_packages.json
}

fn legacy_quality_score(pkg: &Package) -> u32 {
    // Calculate using v1 algorithm
}

async fn new_quality_score(pkg: &Package) -> Result<u32> {
    // Calculate using v2 algorithm
}

fn chi_square_statistic(v1: &Distribution, v2: &Distribution) -> f64 {
    // Statistical test
}
```

---

## Phase 0 Completion Checklist

- [ ] **Task 0.1**: MarketplaceRegistry adapter trait defined in `ggen-domain/marketplace/adapter.rs`
- [ ] **Task 0.2**: Feature flags added to both `ggen-cli/Cargo.toml` and `ggen-domain/Cargo.toml`
- [ ] **Task 0.3**: SPARQL validation framework in `ggen-marketplace-v2/src/ontology_validation.rs`
- [ ] **Task 0.4**: Reference dataset created at `tests/fixtures/reference_packages.json` (100 packages)
- [ ] **Task 0.5**: Migration validation script at `scripts/validate_marketplace_migration.rs`
- [ ] **Task 0.6**: FMEA test infrastructure in `tests/fmea_tests.rs`
- [ ] All code compiles without warnings
- [ ] Documentation updated to explain new architecture
- [ ] Commit Phase 0 changes to git

---

## Phase 0 Success Metrics

**Code Quality**:
- ✓ No unsafe code
- ✓ All public types documented (deny missing_docs)
- ✓ Feature flags reduce compile time by 20%

**Infrastructure**:
- ✓ SPARQL validation catches 100% of malformed queries at startup
- ✓ Reference dataset covers all edge cases (special chars, circular deps, etc.)
- ✓ Migration script reports 100% compatibility before Phase 1

**Testing**:
- ✓ FMEA tests can run in isolation (without full marketplace)
- ✓ All 9+ RPN>50 items have explicit tests
- ✓ Test infrastructure ready for A/B validation in Phase 1

---

## What's Next (Phase 1)

Once Phase 0 is complete, we'll implement **Priority 1 commands** using the adapter trait:

```rust
// Phase 1 Commands (Weeks 2-3)
1. search          → SparqlSearchEngine
2. search-by-id    → RDF URI lookup
3. list            → all_packages() SPARQL
4. publish         → insert_package_rdf()
5. validate        → PackageValidator on v2 models
```

Each will:
- Implement `MarketplaceRegistry` trait
- Have corresponding unit + integration tests
- Be validated against reference dataset
- Run A/B comparison with v1 before merge

