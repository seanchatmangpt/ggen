# ggen Marketplace Data Structures Reference

**Version**: 1.0
**Date**: 2025-01-16
**Purpose**: Rust struct definitions and implementation patterns for marketplace domain layer

---

## Table of Contents

1. [Core Domain Types](#core-domain-types)
2. [Input/Output Structures](#inputoutput-structures)
3. [Maturity Assessment Types](#maturity-assessment-types)
4. [Search & Filter Types](#search--filter-types)
5. [Validation & Guards](#validation--guards)
6. [Test Fixtures](#test-fixtures)

---

## Core Domain Types

### PackageMetadata (Infrastructure Layer)

**Location**: `crates/ggen-domain/src/marketplace/registry.rs`

```rust
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Package metadata from registry index
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PackageMetadata {
    /// Package name (validated)
    pub name: String,

    /// All available versions
    pub versions: Vec<VersionMetadata>,

    /// Package description
    pub description: String,

    /// Package author
    pub author: Option<String>,

    /// Package category (e.g., "ai-agents", "microservices")
    pub category: Option<String>,

    /// Search tags
    pub tags: Vec<String>,

    /// Repository URL
    pub repository: Option<String>,

    /// License identifier (e.g., "MIT", "Apache-2.0")
    pub license: Option<String>,

    /// Homepage URL
    pub homepage: Option<String>,

    /// 8020 Innovation: Critical 20% bundle marker
    #[serde(default)]
    pub is_8020: bool,

    /// 8020 Innovation: Passed all Guard8020Coverage checks
    #[serde(default)]
    pub is_8020_certified: bool,

    /// 8020 Innovation: Dark matter reduction claim
    /// Example: "Eliminates 70% of observability setup work"
    #[serde(default)]
    pub dark_matter_reduction_target: Option<String>,

    /// 8020 Innovation: Sector classification
    /// Examples: "observability", "paper", "healthcare"
    #[serde(default)]
    pub sector: Option<String>,
}

/// Version-specific metadata
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct VersionMetadata {
    /// Semantic version (e.g., "1.2.0")
    pub version: String,

    /// Download URL for this version
    pub download_url: String,

    /// SHA-256 checksum
    pub checksum: String,

    /// Package dependencies
    pub dependencies: Vec<Dependency>,

    /// Publication timestamp (RFC3339 format)
    pub published_at: String,

    /// Package size in bytes
    pub size_bytes: u64,
}

/// Package dependency specification
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Dependency {
    /// Dependency package name
    pub name: String,

    /// Version requirement (e.g., "^1.0.0", ">=2.0.0")
    pub version_req: String,

    /// Whether this dependency is optional
    pub optional: bool,
}

impl PackageMetadata {
    /// Get the latest version
    pub fn latest_version(&self) -> Option<&VersionMetadata> {
        self.versions.last()
    }

    /// Check if package is production-ready
    pub fn is_production_ready(&self) -> bool {
        self.is_8020_certified
    }

    /// Get package identifier
    pub fn package_id(&self) -> String {
        format!("io.ggen.{}", self.name)
    }
}
```

---

## Input/Output Structures

### Search Operations

**Location**: `crates/ggen-domain/src/marketplace/search.rs`

```rust
use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};

/// Search input parameters
#[derive(Debug, Clone, Default)]
pub struct SearchInput {
    /// Search query (required, non-empty)
    pub query: String,

    /// Maximum number of results
    pub limit: usize,

    /// Filter by category
    pub category: Option<String>,

    /// Filter by minimum maturity level
    pub min_maturity: Option<String>,

    /// Filter by tags (match any)
    pub tags: Vec<String>,

    /// Sort field
    pub sort_by: Option<SortField>,
}

/// Sort options for search results
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum SortField {
    /// Sort by package name (alphabetical)
    Name,
    /// Sort by maturity score (descending)
    MaturityScore,
    /// Sort by download count (descending)
    Downloads,
    /// Sort by last update time (newest first)
    UpdatedAt,
}

/// Search result entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchResult {
    /// Package name
    pub name: String,

    /// Latest version
    pub version: String,

    /// Description
    pub description: String,

    /// Maturity score (0-100)
    pub maturity_score: u32,

    /// Maturity level
    pub maturity_level: String,

    /// Total downloads
    pub downloads: u64,

    /// GitHub stars (if available)
    pub stars: u32,

    /// Package author
    pub author: Option<String>,

    /// Package category
    pub category: Option<String>,

    /// Tags
    pub tags: Vec<String>,
}

/// Search domain function
pub async fn execute_search(input: SearchInput) -> Result<Vec<SearchResult>> {
    // Implementation loads registry, filters, and enriches results
    // See marketplace_architecture.md for detailed flow
    todo!("Implementation in domain layer")
}
```

### Install Operations

**Location**: `crates/ggen-domain/src/marketplace/install.rs`

```rust
use std::path::PathBuf;
use ggen_utils::error::Result;

/// Installation input parameters
#[derive(Debug, Clone)]
pub struct InstallInput {
    /// Package name to install
    pub package: String,

    /// Target directory (defaults to ~/.ggen/packages/<name>)
    pub target: Option<PathBuf>,

    /// Force reinstall even if already installed
    pub force: bool,

    /// Skip dependency installation
    pub no_dependencies: bool,

    /// Dry run (don't actually install)
    pub dry_run: bool,
}

/// Installation result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstallResult {
    /// Package name
    pub package_name: String,

    /// Installed version
    pub version: String,

    /// Installation path
    pub install_path: PathBuf,

    /// Dependencies that were installed
    pub dependencies_installed: Vec<String>,

    /// Installation status
    pub status: InstallStatus,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum InstallStatus {
    /// Successfully installed
    Installed,
    /// Already installed (and not forced)
    AlreadyInstalled,
    /// Dry run completed
    DryRun,
    /// Installation failed
    Failed { reason: String },
}

/// Install domain function
pub async fn execute_install(input: InstallInput) -> Result<InstallResult> {
    // Implementation:
    // 1. Validate package name
    // 2. Load registry and resolve version
    // 3. Check if already installed
    // 4. Download package archive
    // 5. Verify checksum
    // 6. Extract to target directory
    // 7. Resolve and install dependencies
    todo!("Implementation in domain layer")
}
```

### List Operations

**Location**: `crates/ggen-domain/src/marketplace/list.rs`

```rust
use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};

/// List input parameters
#[derive(Debug, Clone, Default)]
pub struct ListInput {
    /// Show detailed information
    pub detailed: bool,

    /// Output as JSON
    pub json: bool,

    /// Filter by minimum maturity level
    pub min_maturity: Option<String>,

    /// Filter by specific maturity level
    pub maturity_level: Option<String>,

    /// Sort by field
    pub sort: Option<String>,
}

/// List output
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListOutput {
    /// Installed packages
    pub packages: Vec<InstalledPackage>,

    /// Total count
    pub total: usize,
}

/// Installed package information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstalledPackage {
    /// Package name
    pub name: String,

    /// Installed version
    pub version: String,

    /// Package title
    pub title: String,

    /// Short description
    pub description: String,

    /// Installation path
    pub path: Option<PathBuf>,

    /// Maturity score (if assessed)
    pub maturity_score: Option<u32>,

    /// Last update timestamp
    pub updated_at: Option<String>,
}

/// List domain function
pub async fn execute_list(input: ListInput) -> Result<ListOutput> {
    // Implementation:
    // 1. Scan ~/.ggen/packages/ directory
    // 2. Load metadata for each package
    // 3. Apply filters (maturity level, category)
    // 4. Sort results
    // 5. Return ListOutput
    todo!("Implementation in domain layer")
}
```

---

## Maturity Assessment Types

### MaturityAssessment

**Location**: `crates/ggen-marketplace/src/maturity.rs`

```rust
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Complete maturity assessment for a package
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct MaturityAssessment {
    /// Package ID (e.g., "io.ggen.agent-editor")
    pub package_id: String,

    /// Human-readable package name
    pub package_name: String,

    /// Assessment timestamp
    pub assessed_at: DateTime<Utc>,

    /// Documentation score (0-20 points)
    pub documentation: DocumentationScore,

    /// Testing score (0-20 points)
    pub testing: TestingScore,

    /// Security score (0-20 points)
    pub security: SecurityScore,

    /// Performance score (0-15 points)
    pub performance: PerformanceScore,

    /// Adoption score (0-15 points)
    pub adoption: AdoptionScore,

    /// Maintenance score (0-10 points)
    pub maintenance: MaintenanceScore,
}

impl MaturityAssessment {
    /// Calculate total maturity score (0-100)
    pub fn total_score(&self) -> u32 {
        self.documentation.total()
            + self.testing.total()
            + self.security.total()
            + self.performance.total()
            + self.adoption.total()
            + self.maintenance.total()
    }

    /// Get maturity level based on total score
    pub fn level(&self) -> MaturityLevel {
        MaturityLevel::from_score(self.total_score())
    }

    /// Get percentage breakdown by dimension
    pub fn score_breakdown(&self) -> HashMap<String, f32> {
        let mut breakdown = HashMap::new();

        breakdown.insert(
            "documentation".to_string(),
            (self.documentation.total() as f32 / 20.0) * 100.0,
        );
        breakdown.insert(
            "testing".to_string(),
            (self.testing.total() as f32 / 20.0) * 100.0,
        );
        breakdown.insert(
            "security".to_string(),
            (self.security.total() as f32 / 20.0) * 100.0,
        );
        breakdown.insert(
            "performance".to_string(),
            (self.performance.total() as f32 / 15.0) * 100.0,
        );
        breakdown.insert(
            "adoption".to_string(),
            (self.adoption.total() as f32 / 15.0) * 100.0,
        );
        breakdown.insert(
            "maintenance".to_string(),
            (self.maintenance.total() as f32 / 10.0) * 100.0,
        );

        breakdown
    }

    /// Get all improvement feedback
    pub fn all_feedback(&self) -> Vec<(String, Vec<String>)> {
        let mut feedback = Vec::new();

        if !self.documentation.feedback().is_empty() {
            feedback.push(("documentation".to_string(), self.documentation.feedback()));
        }
        if !self.testing.feedback().is_empty() {
            feedback.push(("testing".to_string(), self.testing.feedback()));
        }
        if !self.security.feedback().is_empty() {
            feedback.push(("security".to_string(), self.security.feedback()));
        }
        if !self.performance.feedback().is_empty() {
            feedback.push(("performance".to_string(), self.performance.feedback()));
        }
        if !self.adoption.feedback().is_empty() {
            feedback.push(("adoption".to_string(), self.adoption.feedback()));
        }
        if !self.maintenance.feedback().is_empty() {
            feedback.push(("maintenance".to_string(), self.maintenance.feedback()));
        }

        feedback
    }
}

/// Maturity level classification
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum MaturityLevel {
    /// 0-40 points: Early-stage, not production-ready
    Experimental,
    /// 41-60 points: Functional but incomplete
    Beta,
    /// 61-80 points: Stable and reliable
    Production,
    /// 81-100 points: Fully mature and recommended
    Enterprise,
}

impl MaturityLevel {
    /// Get maturity level from score
    pub fn from_score(score: u32) -> Self {
        match score {
            0..=40 => MaturityLevel::Experimental,
            41..=60 => MaturityLevel::Beta,
            61..=80 => MaturityLevel::Production,
            81..=100 => MaturityLevel::Enterprise,
            _ => MaturityLevel::Enterprise,
        }
    }

    /// Get human-readable description
    pub fn description(&self) -> &'static str {
        match self {
            MaturityLevel::Experimental => {
                "Early-stage package, not recommended for production use"
            }
            MaturityLevel::Beta => "Functional but incomplete, suitable for testing and feedback",
            MaturityLevel::Production => "Stable and reliable, suitable for production use",
            MaturityLevel::Enterprise => "Fully mature, recommended for mission-critical systems",
        }
    }

    /// Get recommended next steps
    pub fn recommendations(&self) -> Vec<&'static str> {
        match self {
            MaturityLevel::Experimental => vec![
                "Add comprehensive documentation (README, examples, API docs)",
                "Implement unit and integration tests",
                "Set up security scanning and vulnerability audits",
                "Create benchmarks and performance baselines",
            ],
            MaturityLevel::Beta => vec![
                "Expand test coverage to reach 80%+ (currently under 80%)",
                "Add E2E tests and integration tests",
                "Implement security audit automation",
                "Create performance optimization targets",
            ],
            MaturityLevel::Production => vec![
                "Maintain test coverage above 85%",
                "Establish regular release cadence",
                "Monitor and respond to community issues",
                "Plan for enterprise features and SLAs",
            ],
            MaturityLevel::Enterprise => vec![
                "Maintain comprehensive documentation",
                "Sustain test coverage above 90%",
                "Monitor adoption metrics and community health",
                "Plan for long-term maintenance and evolution",
            ],
        }
    }
}

/// Documentation dimension score (0-20 points)
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct DocumentationScore {
    /// README exists and is comprehensive (0-5 points)
    pub readme: u32,
    /// API documentation present (0-5 points)
    pub api_docs: u32,
    /// Examples and tutorials (0-5 points)
    pub examples: u32,
    /// Changelog and release notes (0-5 points)
    pub changelog: u32,
}

impl DocumentationScore {
    pub fn total(&self) -> u32 {
        (self.readme + self.api_docs + self.examples + self.changelog).min(20)
    }

    pub fn feedback(&self) -> Vec<String> {
        let mut feedback = Vec::new();
        if self.readme < 5 {
            feedback.push(
                "Enhance README with more detailed descriptions and usage examples".to_string(),
            );
        }
        if self.api_docs < 5 {
            feedback.push(
                "Add comprehensive API documentation with parameter descriptions".to_string(),
            );
        }
        if self.examples < 5 {
            feedback.push("Include more examples covering different use cases".to_string());
        }
        if self.changelog < 5 {
            feedback.push("Maintain a detailed changelog documenting all versions".to_string());
        }
        feedback
    }
}

/// Testing dimension score (0-20 points)
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct TestingScore {
    /// Unit test coverage (0-8 points)
    pub unit_tests: u32,
    /// Integration tests (0-6 points)
    pub integration_tests: u32,
    /// E2E tests (0-4 points)
    pub e2e_tests: u32,
    /// Test coverage percentage
    pub coverage_percent: f32,
    /// Additional: Chicago TDD compliance (0-2 points)
    #[serde(default)]
    pub chicago_tdd: u32,
}

impl TestingScore {
    pub fn total(&self) -> u32 {
        (self.unit_tests + self.integration_tests + self.e2e_tests + self.chicago_tdd).min(20)
    }

    pub fn feedback(&self) -> Vec<String> {
        let mut feedback = Vec::new();
        if self.coverage_percent < 80.0 {
            feedback.push(format!(
                "Increase test coverage from {:.0}% to at least 80%",
                self.coverage_percent
            ));
        }
        if self.unit_tests < 8 {
            feedback.push("Expand unit tests to cover more edge cases and error paths".to_string());
        }
        if self.integration_tests < 6 {
            feedback.push("Add integration tests covering component interactions".to_string());
        }
        if self.e2e_tests < 4 {
            feedback.push("Implement E2E tests covering complete workflows".to_string());
        }
        if self.chicago_tdd < 2 {
            feedback.push("Adopt Chicago TDD practices with sociable tests".to_string());
        }
        feedback
    }
}

/// Security dimension score (0-20 points)
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct SecurityScore {
    /// Vulnerability scan results (0-10 points)
    pub vulnerability_scan: u32,
    /// Dependency audit (0-5 points)
    pub dependency_audit: u32,
    /// Safe code practices (0-5 points)
    pub safe_code: u32,
}

impl SecurityScore {
    pub fn total(&self) -> u32 {
        (self.vulnerability_scan + self.dependency_audit + self.safe_code).min(20)
    }

    pub fn feedback(&self) -> Vec<String> {
        let mut feedback = Vec::new();
        if self.vulnerability_scan < 10 {
            feedback.push("Run security vulnerability scans and address findings".to_string());
        }
        if self.dependency_audit < 5 {
            feedback.push("Audit dependencies for known vulnerabilities (cargo audit)".to_string());
        }
        if self.safe_code < 5 {
            feedback.push(
                "Eliminate unsafe code and replace unwrap/expect with proper error handling"
                    .to_string(),
            );
        }
        feedback
    }
}

/// Performance dimension score (0-15 points)
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct PerformanceScore {
    /// Benchmarks present (0-8 points)
    pub benchmarks: u32,
    /// Optimization work (0-4 points)
    pub optimization: u32,
    /// Determinism verification (0-3 points)
    pub determinism: u32,
}

impl PerformanceScore {
    pub fn total(&self) -> u32 {
        (self.benchmarks + self.optimization + self.determinism).min(15)
    }

    pub fn feedback(&self) -> Vec<String> {
        let mut feedback = Vec::new();
        if self.benchmarks < 8 {
            feedback.push(
                "Create comprehensive benchmarks measuring generation time and memory usage"
                    .to_string(),
            );
        }
        if self.optimization < 4 {
            feedback.push("Profile and optimize critical paths for better performance".to_string());
        }
        if self.determinism < 3 {
            feedback.push(
                "Verify deterministic output (byte-identical generation with same inputs)"
                    .to_string(),
            );
        }
        feedback
    }
}

/// Adoption dimension score (0-15 points)
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct AdoptionScore {
    /// Downloads/usage (0-6 points)
    pub downloads: u32,
    /// Academic citations (0-5 points)
    pub citations: u32,
    /// Community activity (0-4 points)
    pub community: u32,
}

impl AdoptionScore {
    pub fn total(&self) -> u32 {
        (self.downloads + self.citations + self.community).min(15)
    }

    pub fn feedback(&self) -> Vec<String> {
        let mut feedback = Vec::new();
        if self.downloads < 6 {
            feedback.push(
                "Promote package through blog posts, conferences, and community channels"
                    .to_string(),
            );
        }
        if self.citations < 5 {
            feedback.push(
                "Publish research papers or case studies demonstrating real-world impact"
                    .to_string(),
            );
        }
        if self.community < 4 {
            feedback.push(
                "Build community through documentation, examples, and engagement".to_string(),
            );
        }
        feedback
    }
}

/// Maintenance dimension score (0-10 points)
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct MaintenanceScore {
    /// Release frequency (0-5 points)
    pub release_cadence: u32,
    /// Issue response time (0-3 points)
    pub responsiveness: u32,
    /// Active maintenance (0-2 points)
    pub active_maintenance: u32,
}

impl MaintenanceScore {
    pub fn total(&self) -> u32 {
        (self.release_cadence + self.responsiveness + self.active_maintenance).min(10)
    }

    pub fn feedback(&self) -> Vec<String> {
        let mut feedback = Vec::new();
        if self.release_cadence < 5 {
            feedback.push(
                "Establish and maintain a regular release schedule (monthly or quarterly)"
                    .to_string(),
            );
        }
        if self.responsiveness < 3 {
            feedback.push("Improve issue and PR response time (target: 48 hours)".to_string());
        }
        if self.active_maintenance < 2 {
            feedback.push("Commit to ongoing maintenance and support".to_string());
        }
        feedback
    }
}
```

---

## Search & Filter Types

### Advanced Search

**Location**: `crates/ggen-domain/src/marketplace/search_advanced.rs`

```rust
use serde::{Deserialize, Serialize};

/// Advanced search query builder
#[derive(Debug, Clone, Default)]
pub struct AdvancedSearchQuery {
    /// Full-text search query
    pub query: String,

    /// Filter by category (exact match)
    pub category: Option<String>,

    /// Filter by minimum maturity score (0-100)
    pub min_score: Option<u32>,

    /// Filter by maturity level
    pub min_level: Option<MaturityLevel>,

    /// Filter by tags (match any)
    pub tags: Vec<String>,

    /// Filter by sector
    pub sector: Option<String>,

    /// Only show 8020-certified packages
    pub only_8020_certified: bool,

    /// Minimum dimension scores (optional filters)
    pub min_documentation: Option<u32>,
    pub min_testing: Option<u32>,
    pub min_security: Option<u32>,
    pub min_performance: Option<u32>,
    pub min_adoption: Option<u32>,
    pub min_maintenance: Option<u32>,

    /// Result limit
    pub limit: usize,

    /// Result offset (for pagination)
    pub offset: usize,

    /// Sort field
    pub sort_by: SortField,

    /// Sort order
    pub sort_order: SortOrder,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum SortOrder {
    Ascending,
    Descending,
}

/// Advanced search results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AdvancedSearchResults {
    /// Matching packages
    pub results: Vec<SearchResultEntry>,

    /// Total matches (before pagination)
    pub total_matches: usize,

    /// Query execution time (milliseconds)
    pub query_time_ms: u64,

    /// Search statistics
    pub statistics: SearchStatistics,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchResultEntry {
    /// Package metadata
    pub package: PackageMetadata,

    /// Maturity assessment
    pub maturity: MaturityAssessment,

    /// Relevance score (0-1)
    pub relevance: f32,

    /// Highlighted snippets (query terms in context)
    pub highlights: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct SearchStatistics {
    /// Total packages in index
    pub total_packages: usize,

    /// Packages matching query
    pub matching_packages: usize,

    /// Average maturity score of results
    pub average_score: f32,

    /// Distribution by maturity level
    pub level_distribution: HashMap<MaturityLevel, usize>,
}
```

---

## Validation & Guards

### Production Readiness Checks

**Location**: `crates/ggen-domain/src/marketplace/production_readiness.rs`

```rust
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Production readiness assessment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReadinessAssessment {
    /// Package being assessed
    pub package_name: String,

    /// Overall readiness status
    pub is_ready: bool,

    /// Individual checks
    pub checks: Vec<ReadinessCheck>,

    /// Deployment guide
    pub deployment_guide: Option<DeploymentGuide>,

    /// Blocking issues
    pub blocking_issues: Vec<String>,

    /// Warnings (non-blocking)
    pub warnings: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReadinessCheck {
    /// Check name
    pub name: String,

    /// Check category
    pub category: CheckCategory,

    /// Check status
    pub status: CheckStatus,

    /// Description
    pub description: String,

    /// Recommendation (if failed/warning)
    pub recommendation: Option<String>,

    /// Required for production?
    pub required: bool,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum CheckCategory {
    Documentation,
    Testing,
    Security,
    Performance,
    Deployment,
    Observability,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum CheckStatus {
    Pass,
    Warning,
    Fail,
    NotApplicable,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeploymentGuide {
    /// Prerequisites
    pub prerequisites: Vec<String>,

    /// Step-by-step deployment steps
    pub steps: Vec<DeploymentStep>,

    /// Post-deployment validation
    pub validation_steps: Vec<String>,

    /// Rollback procedure
    pub rollback_procedure: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeploymentStep {
    /// Step number
    pub number: usize,

    /// Step title
    pub title: String,

    /// Detailed instructions
    pub instructions: String,

    /// Example command (if applicable)
    pub command: Option<String>,

    /// Expected outcome
    pub expected_outcome: String,
}
```

### Quality Guards

**Location**: `crates/ggen-domain/src/marketplace/guards.rs`

```rust
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Guard check result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GuardCheckResult {
    /// Guard name
    pub guard_name: String,

    /// Check passed?
    pub passed: bool,

    /// Severity level
    pub severity: Severity,

    /// Detailed message
    pub message: String,

    /// Suggested fix
    pub suggestion: Option<String>,

    /// Auto-fixable?
    pub auto_fixable: bool,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum Severity {
    /// Informational only
    Info,
    /// Warning (non-blocking)
    Warning,
    /// Error (blocking for production)
    Error,
    /// Critical (security/safety issue)
    Critical,
}

/// Validation receipt (proof of quality checks)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationReceipt {
    /// Package ID
    pub package_id: String,

    /// Package version
    pub version: String,

    /// Validation timestamp
    pub validated_at: String,

    /// Total score
    pub total_score: u32,

    /// Maturity level
    pub maturity_level: String,

    /// All guard results
    pub guard_results: Vec<GuardCheckResult>,

    /// Passed all critical checks?
    pub production_ready: bool,

    /// Checksum of validated content
    pub content_checksum: String,
}

impl ValidationReceipt {
    /// Save receipt to file
    pub fn save(&self, path: &PathBuf) -> Result<()> {
        let json = serde_json::to_string_pretty(self)?;
        std::fs::write(path, json)?;
        Ok(())
    }

    /// Load receipt from file
    pub fn load(path: &PathBuf) -> Result<Self> {
        let content = std::fs::read_to_string(path)?;
        let receipt = serde_json::from_str(&content)?;
        Ok(receipt)
    }
}
```

---

## Test Fixtures

### Mock Package Builder

**Location**: `tests/fixtures/marketplace/mod.rs`

```rust
use ggen_marketplace::maturity::*;
use ggen_domain::marketplace::registry::*;

/// Test fixture builder for package metadata
pub struct PackageFixtureBuilder {
    name: String,
    version: String,
    description: String,
    category: Option<String>,
    maturity_score: u32,
    downloads: u64,
}

impl PackageFixtureBuilder {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            version: "1.0.0".to_string(),
            description: format!("Test package: {}", name),
            category: None,
            maturity_score: 60,
            downloads: 100,
        }
    }

    pub fn version(mut self, version: &str) -> Self {
        self.version = version.to_string();
        self
    }

    pub fn category(mut self, category: &str) -> Self {
        self.category = Some(category.to_string());
        self
    }

    pub fn maturity_score(mut self, score: u32) -> Self {
        self.maturity_score = score.min(100);
        self
    }

    pub fn downloads(mut self, count: u64) -> Self {
        self.downloads = count;
        self
    }

    pub fn build(self) -> PackageMetadata {
        PackageMetadata {
            name: self.name.clone(),
            versions: vec![VersionMetadata {
                version: self.version.clone(),
                download_url: format!("https://example.com/{}-{}.tar.gz", self.name, self.version),
                checksum: "a".repeat(64),
                dependencies: vec![],
                published_at: chrono::Utc::now().to_rfc3339(),
                size_bytes: 102400,
            }],
            description: self.description,
            author: Some("Test Author".to_string()),
            category: self.category,
            tags: vec!["test".to_string()],
            repository: Some(format!("https://github.com/test/{}", self.name)),
            license: Some("MIT".to_string()),
            homepage: Some("https://example.com".to_string()),
            is_8020: self.maturity_score >= 80,
            is_8020_certified: self.maturity_score >= 85,
            dark_matter_reduction_target: None,
            sector: None,
        }
    }

    pub fn build_with_assessment(self) -> (PackageMetadata, MaturityAssessment) {
        let pkg = self.build();
        let assessment = create_assessment_from_score(&pkg.name, self.maturity_score);
        (pkg, assessment)
    }
}

/// Create maturity assessment from score
fn create_assessment_from_score(name: &str, score: u32) -> MaturityAssessment {
    let mut assessment = MaturityAssessment::new(
        format!("io.ggen.{}", name),
        name,
    );

    // Distribute score across dimensions (simplified)
    let score_per_dimension = score / 6;
    assessment.documentation.readme = (score_per_dimension / 5).min(5);
    assessment.documentation.api_docs = (score_per_dimension / 5).min(5);
    assessment.documentation.examples = (score_per_dimension / 5).min(5);
    assessment.documentation.changelog = (score_per_dimension / 5).min(5);

    assessment.testing.unit_tests = (score_per_dimension * 8 / 20).min(8);
    assessment.testing.integration_tests = (score_per_dimension * 6 / 20).min(6);
    assessment.testing.coverage_percent = score as f32;

    assessment.security.vulnerability_scan = (score_per_dimension / 2).min(10);
    assessment.security.dependency_audit = (score_per_dimension / 4).min(5);

    assessment.performance.benchmarks = (score_per_dimension * 8 / 15).min(8);
    assessment.adoption.downloads = (score_per_dimension * 6 / 15).min(6);
    assessment.maintenance.release_cadence = (score_per_dimension / 2).min(5);

    assessment
}

// Convenience functions for common test scenarios
pub fn enterprise_package() -> (PackageMetadata, MaturityAssessment) {
    PackageFixtureBuilder::new("enterprise-pkg")
        .maturity_score(92)
        .downloads(5000)
        .category("enterprise")
        .build_with_assessment()
}

pub fn production_package() -> (PackageMetadata, MaturityAssessment) {
    PackageFixtureBuilder::new("production-pkg")
        .maturity_score(72)
        .downloads(1000)
        .category("production")
        .build_with_assessment()
}

pub fn beta_package() -> (PackageMetadata, MaturityAssessment) {
    PackageFixtureBuilder::new("beta-pkg")
        .maturity_score(55)
        .downloads(250)
        .category("experimental")
        .build_with_assessment()
}

pub fn experimental_package() -> (PackageMetadata, MaturityAssessment) {
    PackageFixtureBuilder::new("experimental-pkg")
        .maturity_score(35)
        .downloads(50)
        .category("experimental")
        .build_with_assessment()
}
```

---

**End of Data Structures Reference**
