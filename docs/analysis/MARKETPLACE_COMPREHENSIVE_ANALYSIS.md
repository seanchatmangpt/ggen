<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Comprehensive Marketplace Analysis for Packs Redesign](#comprehensive-marketplace-analysis-for-packs-redesign)
  - [Executive Summary](#executive-summary)
  - [Part 1: Complete Marketplace Capabilities (27 Operations)](#part-1-complete-marketplace-capabilities-27-operations)
    - [1.1 Package Discovery & Search (7 operations)](#11-package-discovery--search-7-operations)
      - [Basic Search (`search.rs`)](#basic-search-searchrs)
      - [Advanced Search (`search_advanced.rs`)](#advanced-search-search_advancedrs)
    - [1.2 Package Installation (6 operations)](#12-package-installation-6-operations)
      - [Install (`install.rs`)](#install-installrs)
    - [1.3 Package Publishing (4 operations)](#13-package-publishing-4-operations)
      - [Publish (`publish.rs`)](#publish-publishrs)
    - [1.4 Package Validation (5 operations)](#14-package-validation-5-operations)
      - [Validate (`validate.rs`)](#validate-validaters)
    - [1.5 Sector Bundles (5 operations)](#15-sector-bundles-5-operations)
      - [Bundles (`bundles.rs`)](#bundles-bundlesrs)
    - [1.6 Recommendations (Collaborative Filtering)](#16-recommendations-collaborative-filtering)
      - [Recommender (`recommender.rs`)](#recommender-recommenderrs)
  - [Part 2: Package Structure & Metadata](#part-2-package-structure--metadata)
    - [2.1 Package.toml Format](#21-packagetoml-format)
    - [2.2 Registry Index Format](#22-registry-index-format)
    - [2.3 Package Directory Structure](#23-package-directory-structure)
  - [Part 3: SPARQL & RDF Integration](#part-3-sparql--rdf-integration)
    - [3.1 SPARQL Query Examples](#31-sparql-query-examples)
    - [3.2 RDF Ontology Usage](#32-rdf-ontology-usage)
  - [Part 4: Advanced Features](#part-4-advanced-features)
    - [4.1 80/20 Innovation (3 features)](#41-8020-innovation-3-features)
    - [4.2 Quality Autopilot](#42-quality-autopilot)
    - [4.3 Production Readiness Checker](#43-production-readiness-checker)
    - [4.4 MAPE-K Integration (Autonomic Systems)](#44-mape-k-integration-autonomic-systems)
    - [4.5 Receipt Emitter (Audit Trail)](#45-receipt-emitter-audit-trail)
  - [Part 5: What Users DO With Packages (The 80/20)](#part-5-what-users-do-with-packages-the-8020)
    - [5.1 Critical User Workflows](#51-critical-user-workflows)
      - [Workflow 1: Discover & Install Package](#workflow-1-discover--install-package)
      - [Workflow 2: Compose Multi-Package Project](#workflow-2-compose-multi-package-project)
      - [Workflow 3: Validate & Publish Package](#workflow-3-validate--publish-package)
      - [Workflow 4: Get Recommendations](#workflow-4-get-recommendations)
    - [5.2 The 20% That Matters (Missing Commands)](#52-the-20-that-matters-missing-commands)
  - [Part 6: Recommendations for Packs Redesign](#part-6-recommendations-for-packs-redesign)
    - [6.1 Core Commands (Must Have)](#61-core-commands-must-have)
      - [Package Discovery](#package-discovery)
      - [Package Installation](#package-installation)
      - [Bundles (Composition)](#bundles-composition)
      - [Recommendations (Autonomous)](#recommendations-autonomous)
      - [Validation & Quality](#validation--quality)
      - [Publishing](#publishing)
    - [6.2 Advanced Commands (Nice to Have)](#62-advanced-commands-nice-to-have)
      - [Ontology & SPARQL](#ontology--sparql)
      - [Dependency Management](#dependency-management)
      - [Registry Management](#registry-management)
    - [6.3 Design Principles](#63-design-principles)
  - [Part 7: Data Structures & Algorithms](#part-7-data-structures--algorithms)
    - [7.1 Key Algorithms](#71-key-algorithms)
      - [Relevance Scoring (search.rs:447-531)](#relevance-scoring-searchrs447-531)
      - [Circular Dependency Detection (install.rs:189-236)](#circular-dependency-detection-installrs189-236)
      - [Topological Sort (install.rs:270-339)](#topological-sort-installrs270-339)
    - [7.2 Performance Characteristics](#72-performance-characteristics)
  - [Part 8: Edge Cases & Constraints](#part-8-edge-cases--constraints)
    - [8.1 Installation Constraints](#81-installation-constraints)
    - [8.2 Validation Constraints](#82-validation-constraints)
    - [8.3 Bundle Constraints](#83-bundle-constraints)
  - [Part 9: Key Insights for Packs Redesign](#part-9-key-insights-for-packs-redesign)
    - [9.1 What Packs Should Prioritize](#91-what-packs-should-prioritize)
    - [9.2 Anti-Patterns to Avoid](#92-anti-patterns-to-avoid)
    - [9.3 Command Structure Proposal](#93-command-structure-proposal)
  - [Conclusion](#conclusion)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Comprehensive Marketplace Analysis for Packs Redesign

**Date**: 2025-01-17
**Purpose**: Understand the complete marketplace implementation to inform packs command design
**Focus**: What users DO with packages, not just list/search

---

## Executive Summary

The ggen marketplace is a **production-grade package distribution system** with:
- **27+ capabilities** beyond basic list/search
- **RDF ontology + SPARQL** for semantic package discovery
- **Sector bundles** for vertical-stack composition
- **8020 certification** with automated quality scoring
- **Production-readiness validation** with 6 guards
- **Dependency resolution** with circular detection
- **Autonomous recommendations** using collaborative filtering

**Critical Insight**: Packs commands should expose **composition, validation, and recommendations**, not just CRUD operations.

---

## Part 1: Complete Marketplace Capabilities (27 Operations)

### 1.1 Package Discovery & Search (7 operations)

#### Basic Search (`search.rs`)
```rust
// File: crates/ggen-domain/src/marketplace/search.rs
pub async fn search_packages(query: &str, filters: &SearchFilters) -> Result<Vec<SearchResult>>
```

**Capabilities**:
- **Fuzzy search** with Levenshtein distance (70% similarity threshold)
- **Relevance scoring** (exact: 100, name-contains: 50, fuzzy: 30, desc: 20, tags: 10)
- **Popularity signals** (downloads/1000 + stars/10, max boost: 15 points)
- **80/20 filtering** (`only_8020`, `sector`, `is_8020_certified` flags)
- **Cache-first** with in-memory index (376ms → <50ms)

**Search Filters**:
```rust
pub struct SearchFilters {
    pub category: Option<String>,      // "rust", "web", "cli"
    pub keyword: Option<String>,       // "observability", "testing"
    pub author: Option<String>,
    pub license: Option<String>,
    pub min_stars: Option<u32>,
    pub min_downloads: Option<u32>,
    pub only_8020: bool,               // 80/20 certified only
    pub sector: Option<String>,        // "observability", "fintech", "healthcare"
    pub fuzzy: bool,
    pub sort: String,                  // "relevance", "stars", "downloads"
    pub limit: usize,
}
```

#### Advanced Search (`search_advanced.rs`)
```rust
// File: crates/ggen-domain/src/marketplace/search_advanced.rs
pub struct AdvancedSearchQuery {
    pub query: String,
    pub score_min: Option<f64>,        // Quality score 0-100
    pub score_max: Option<f64>,
    pub production_ready: Option<bool>, // Validated packages only
    pub bundles: Vec<String>,          // "sector-academic-papers"
    pub categories: Vec<String>,
    pub tags: Vec<String>,
    pub sort_by: SortField,            // Score, Name, UpdatedAt, Downloads, Relevance
    pub limit: usize,
}
```

**Returns analytics**:
- Average score
- Production-ready count
- Score distribution (excellent: 95-100, good: 80-94, fair: 60-79, poor: <60)
- Top 3 categories/bundles

### 1.2 Package Installation (6 operations)

#### Install (`install.rs`)
```rust
// File: crates/ggen-domain/src/marketplace/install.rs (1650 lines)
pub async fn install_package(options: &InstallOptions) -> Result<InstallResult>
```

**Features**:
1. **Version resolution**:
   - Semver ranges: `^1.2.3` (>=1.2.3 <2.0.0), `~1.2.3` (>=1.2.3 <1.3.0)
   - Latest version selection
   - Version conflict detection

2. **Dependency resolution**:
   - Topological sort for install order (Kahn's algorithm)
   - Circular dependency detection (DFS with recursion stack, lines 189-236)
   - Missing dependency warnings

3. **Download with retry**:
   - Exponential backoff (1s, 2s, 4s, 8s) + jitter
   - Rate limit detection (HTTP 429 → 30s+ backoff)
   - Transient error retry (5xx, timeouts, connection)
   - Permanent error fail-fast (4xx except 429)
   - 120s timeout

4. **Security**:
   - SHA256 checksum verification (mandatory, lines 562-578)
   - Zip bomb protection (100MB limit, 10K file limit)
   - Path traversal prevention (lines 980-1006)
   - Package name validation (no path separators, no control chars)

5. **Cache management**:
   - `~/.ggen/cache/downloads/` for archives
   - Cache corruption detection (checksum mismatch → re-download)

6. **Atomic operations**:
   - Lockfile with file locking (concurrent write protection, lines 1268-1312)
   - Rollback on failure (partial install cleanup, lines 1465-1508)
   - Backup before lockfile write

**Installation Targets**:
- Local filesystem (dev mode: `GGEN_DEV_MODE=1`)
- GitHub archives (default)
- Custom registry (`GGEN_REGISTRY_URL`)

### 1.3 Package Publishing (4 operations)

#### Publish (`publish.rs`)
```rust
// File: crates/ggen-domain/src/marketplace/publish.rs
pub async fn execute_publish(input: PublishInput) -> Result<PublishOutput>
```

**Validation (lines 299-440)**:
- **Name**: 1-100 chars, alphanumeric + `-_/`, no path traversal
- **Version**: Semver X.Y.Z (3+ numeric components)
- **Title**: 1-200 chars
- **Description**: 10-2000 chars
- **Categories**: Max 10, 1-50 chars each
- **Tags**: Max 20, 1-30 chars each, alphanumeric + `-_`

**Publishing Flow**:
1. Create tarball (gzip, tar format)
2. Calculate SHA256 checksum
3. Build `Package` with ggen-marketplace models
4. Validate with `ValidatedPackage` (Poka-yoke)
5. Update registry index (atomic write with backup)

### 1.4 Package Validation (5 operations)

#### Validate (`validate.rs`)
```rust
// File: crates/ggen-domain/src/marketplace/validate.rs (1107 lines)
pub fn validate_package(package_path: &Path) -> Result<PackageValidation>
```

**Score Calculation**:
- **Required checks (60% weight)**:
  - `package.toml` (TOML parsing + required fields: name, version, description)
  - `README.md` (>100 chars)
  - Source code (`src/main.rs` or `src/lib.rs` or `templates/`)
  - License (`LICENSE`, `LICENSE-MIT`, `LICENSE-APACHE`)

- **Quality checks (40% weight)**:
  - RDF ontology (`rdf/ontology.ttl`, >200 lines, >1 triple)
  - SPARQL queries (`sparql/*.rq`, `queries/*.sparql`)
  - Examples (`examples/*.rs`, `.py`, `.ts`, `.js`)
  - Tests (`tests/*.rs`, `.py`, `.ts`)
  - Documentation (`docs/*.md`)

**Production Readiness**:
- Score >= 95%
- All required checks pass (warnings fail by default)

**Output**:
```rust
pub struct PackageValidation {
    pub score: f64,                    // 0-100
    pub production_ready: bool,
    pub required_checks: Vec<(RequiredCheck, CheckResult)>,
    pub quality_checks: Vec<(QualityCheck, CheckResult)>,
    pub errors: Vec<String>,
    pub warnings: Vec<String>,
}
```

### 1.5 Sector Bundles (5 operations)

#### Bundles (`bundles.rs`)
```rust
// File: crates/ggen-domain/src/marketplace/bundles.rs
pub struct SectorBundle {
    pub id: String,                    // "sector-academic-papers"
    pub version: String,
    pub description: String,
    pub domain: String,                // "academic", "enterprise", "fintech"
    pub minimum_score: f64,            // Quality threshold
    pub packages: Vec<String>,         // Package names in bundle
    pub features: Vec<String>,
}
```

**Built-in Bundles (5 sectors)**:
1. **Academic Papers** (3 packages):
   - `academic-paper-lifecycle`, `academic-bibliography-manager`, `academic-peer-review-workflow`
   - Minimum score: 80%

2. **Enterprise SaaS** (4 packages):
   - `multi-tenant-saas`, `crm-customer-management`, `enterprise-erp-core`, `human-resources-management`
   - Minimum score: 80%

3. **Data Pipelines** (3 packages):
   - `data-pipeline-cli`, `database-schema-generator`, `business-intelligence-reporting`
   - Minimum score: 80%

4. **Healthcare** (4 packages):
   - `healthcare-analytics`, `laboratory-information-system`, `medical-billing`, `ehr-integration`
   - Minimum score: 85%

5. **FinTech** (4 packages):
   - `banking-core`, `iso-20022-payments`, `kyc-aml-compliance`, `cryptocurrency-exchange`
   - Minimum score: 90%

**Bundle Operations**:
- `BundleRegistry::list_bundles()` - Get all bundles
- `BundleRegistry::get_bundle(id)` - Get specific bundle
- `generate_bundle_docs(bundle)` - Generate markdown docs
- Install bundle (validates compatibility, checks minimum score, installs in dependency order)

### 1.6 Recommendations (Collaborative Filtering)

#### Recommender (`recommender.rs`)
```rust
// File: crates/ggen-domain/src/marketplace/recommender.rs
pub struct Recommender;

impl Recommender {
    pub fn recommend(
        config: &RecommenderConfig,
        installed: Vec<String>,
        preferred_bundles: Vec<String>,
        all_packages: Vec<PackageInfo>,
    ) -> RecommendationSet
}
```

**Recommendation Algorithm**:
- **Bundle affinity** (30% weight): Matching preferred bundles
- **Complementarity** (25% weight): Related to installed packages
- **Quality** (30% weight): Package score 0-100
- **Popularity** (15% weight): Downloads normalized

**Recommendation Reasons**:
```rust
pub enum RecommendationReason {
    SameBundleAffinity { bundle: String },
    Complementary { package: String },
    CategorySimilarity { category: String, avg_score: f64 },
    PopularInDomain { domain: String },
    Trending { improvement_rate: f64 },
}
```

**Configuration**:
```rust
pub struct RecommenderConfig {
    pub min_quality_score: f64,        // Default: 75%
    pub max_recommendations: usize,    // Default: 10
    pub production_ready_only: bool,   // Default: false
    pub bundle_affinity_weight: f64,   // Default: 30%
    pub complementarity_weight: f64,   // Default: 25%
    pub quality_weight: f64,           // Default: 30%
    pub popularity_weight: f64,        // Default: 15%
}
```

**Output**:
```rust
pub struct Recommendation {
    pub package_id: String,
    pub confidence: f64,               // 0-100
    pub reason: RecommendationReason,
    pub quality_score: f64,
    pub bundles: Vec<String>,
    pub complements: Vec<String>,      // Related packages
}
```

---

## Part 2: Package Structure & Metadata

### 2.1 Package.toml Format

**Example** (`hello-world/package.toml`):
```toml
[package]
name = "hello-world"
full_name = "hello-world-starter"
version = "1.0.0"
description = "Simple Hello World starter template"
category = "starter"
author = "ggen-team"
repository = "https://github.com/seanchatmangpt/ggen"
path = "marketplace/packages/hello-world"
license = "MIT"
dependencies = []
features = [
    "Basic Rust project structure",
    "Simple main.rs template",
]
tags = ["starter", "hello-world", "beginner"]
keywords = ["hello-world", "starter", "rust"]

[install]
type = "template"
template_path = "marketplace/packages/hello-world"

[files]
"Cargo.toml" = "Cargo.toml"
"src/" = "src/"

[variables]
project_name = { type = "string", default = "hello-world" }
author = { type = "string", default = "ggen-user" }

[examples]
basic = """
ggen market install hello-world
ggen template generate hello-world --vars '{"project_name":"my-first-project"}'
"""

[docs]
quick_start = "Simple Hello World starter"
```

### 2.2 Registry Index Format

**File**: `~/.ggen/registry/index.json` or `marketplace/registry/index.json`

```json
{
  "version": "1.0.0",
  "updated_at": "2024-01-17T00:00:00Z",
  "packages": [
    {
      "name": "agent-editor",
      "version": "1.0.0",
      "category": "agents",
      "description": "AI-powered code editor agent",
      "tags": ["agent", "editor", "ai"],
      "keywords": ["code", "editor", "ai"],
      "author": "ggen-team",
      "license": "MIT",
      "downloads": 1234,
      "stars": 56,
      "production_ready": true,
      "dependencies": [],
      "path": "marketplace/packages/agent-editor",
      "download_url": "https://github.com/seanchatmangpt/ggen/archive/master.zip",
      "checksum": "abcd1234...",
      "is_8020_certified": true,
      "sector": "observability",
      "dark_matter_reduction_target": "Eliminates 70% of manual editing"
    }
  ],
  "search_index": {
    "agent": ["agent-editor", "agent-cli-copilot"],
    "editor": ["agent-editor"]
  }
}
```

### 2.3 Package Directory Structure

**Standard Layout**:
```
agent-editor/
├── package.toml          # Metadata (required)
├── README.md            # Documentation (required, >100 chars)
├── LICENSE              # License file (required)
├── Cargo.toml           # Build config
├── src/                 # Source code (required unless template-only)
│   ├── lib.rs
│   └── main.rs
├── rdf/                 # RDF ontology (quality check)
│   └── ontology.ttl     # >200 lines, >1 triple
├── sparql/              # SPARQL queries (quality check)
│   ├── query1.sparql
│   └── query2.rq
├── examples/            # Example code (quality check)
│   ├── basic.rs
│   └── advanced.rs
├── tests/               # Test suite (quality check)
│   ├── unit_test.rs
│   └── integration_test.rs
└── docs/                # Extra docs (quality check)
    ├── GUIDE.md
    └── API.md
```

---

## Part 3: SPARQL & RDF Integration

### 3.1 SPARQL Query Examples

**File**: `notification-messaging-hub/sparql/templates.sparql` (326 lines, 13 queries)

**Query 1: Get Pending Messages**
```sparql
PREFIX nmh: <http://ggen.ai/ontology/notification-messaging-hub#>

SELECT ?message ?subject ?priority ?scheduledAt ?channel
WHERE {
    ?message a nmh:Message ;
             nmh:subject ?subject ;
             nmh:priority ?priority ;
             nmh:deliveredVia ?channel .

    OPTIONAL { ?message nmh:scheduledAt ?scheduledAt }

    ?message nmh:hasDeliveryStatus ?status .
    ?status nmh:deliveryStatus "pending" .
}
ORDER BY DESC(?priority) ?scheduledAt
LIMIT 1000
```

**Query 3: Channel Performance**
```sparql
SELECT ?channel ?channelType
       (COUNT(?message) AS ?totalMessages)
       (SUM(IF(?status = "delivered", 1, 0)) AS ?successCount)
       (SUM(IF(?status = "failed", 1, 0)) AS ?failureCount)
       (AVG(?attemptCount) AS ?avgAttempts)
WHERE {
    ?message a nmh:Message ;
             nmh:deliveredVia ?channel ;
             nmh:hasDeliveryStatus ?statusObj .

    ?channel nmh:channelType ?channelType .
    ?statusObj nmh:deliveryStatus ?status .

    OPTIONAL {
        ?message nmh:hasDeliveryStatus/nmh:attemptCount ?attemptCount
    }
}
GROUP BY ?channel ?channelType
ORDER BY DESC(?totalMessages)
```

### 3.2 RDF Ontology Usage

**Purpose**:
- Define package relationships
- Enable semantic search
- Support advanced filtering
- Power recommendation engine

**Validation**:
- `Graph::new()` - Create RDF graph
- `graph.load_path(&ontology_ttl)` - Parse Turtle format
- `graph.len()` - Count triples (must be >1 for quality check)

---

## Part 4: Advanced Features

### 4.1 80/20 Innovation (3 features)

**Fields in Package Metadata**:
```rust
pub struct PackageMetadata {
    pub is_8020: bool,                           // Critical 20% that covers 80% use cases
    pub is_8020_certified: bool,                 // Passed Guard8020Coverage checks
    pub dark_matter_reduction_target: Option<String>, // "Eliminates 70% of X work"
    pub sector: Option<String>,                  // "observability", "fintech"
}
```

**Search Integration**:
```rust
// Filter to only 80/20 packages
let filters = SearchFilters::new().only_8020();
let results = search_packages("observability", &filters).await?;
```

### 4.2 Quality Autopilot

**File**: `quality_autopilot.rs`

```rust
pub fn generate_improvement_plan(package_path: &Path) -> Result<ImprovementPlan>
pub fn apply_template_improvements(package_path: &Path, plan: &ImprovementPlan) -> Result<()>
```

**Generates**:
- Missing file templates (README, LICENSE, tests)
- Quality improvement suggestions
- Automated fixes for common issues

### 4.3 Production Readiness Checker

**File**: `production_readiness.rs`

```rust
pub struct ReadinessChecker;

pub struct ReadinessAssessment {
    pub overall_ready: bool,
    pub checks: Vec<ReadinessCheck>,
    pub deployment_guide: DeploymentGuide,
}
```

**Checks**:
- Dependencies resolved
- Security vulnerabilities
- Performance baselines
- Observability integration
- Deployment configuration
- Documentation completeness

### 4.4 MAPE-K Integration (Autonomic Systems)

**File**: `mape_k_integration.rs`

```rust
pub struct AutonomicMarketplace {
    pub status: AutonomicStatus,
    pub health: MarketplaceHealth,
    pub observations: Vec<MarketplaceObservation>,
}

pub enum MarketplaceObservationType {
    PackageInstallFailure,
    ValidationScoreDrop,
    DependencyConflict,
    SecurityVulnerability,
    PerformanceDegradation,
}
```

**Self-healing capabilities**:
- Monitor marketplace health
- Analyze anomalies
- Plan corrective actions
- Execute automated fixes
- Learn from patterns

### 4.5 Receipt Emitter (Audit Trail)

**File**: `receipt_emitter.rs`

```rust
pub fn emit_receipt_for_package(package: &PackageValidation) -> Result<ValidationReceipt>
pub fn generate_validation_report(packages: &[PackageValidation]) -> Result<ValidationReport>
```

**Tracks**:
- Validation timestamps
- Check results
- Production-ready status updates
- Quality score changes

---

## Part 5: What Users DO With Packages (The 80/20)

### 5.1 Critical User Workflows

#### Workflow 1: Discover & Install Package
```bash
# 1. Search by capability
ggen market search "observability" --sector=observability --only-8020

# 2. View package details (MISSING IN CURRENT CLI)
ggen market info agent-editor

# 3. Install with dependencies
ggen market install agent-editor
```

#### Workflow 2: Compose Multi-Package Project
```bash
# 1. Install bundle
ggen market install-bundle sector-enterprise-saas

# 2. Verify compatibility (MISSING IN CURRENT CLI)
ggen market verify-bundle sector-enterprise-saas

# 3. Generate project from templates
ggen template generate crm-system
```

#### Workflow 3: Validate & Publish Package
```bash
# 1. Validate package
ggen market validate ./my-package

# 2. Check production readiness
ggen market readiness ./my-package

# 3. Publish to registry
ggen market publish ./my-package --tag=v1.0.0
```

#### Workflow 4: Get Recommendations
```bash
# 1. Get recommendations based on installed (MISSING)
ggen market recommend

# 2. Find complementary packages (MISSING)
ggen market complements agent-editor

# 3. Discover bundle peers (MISSING)
ggen market bundle-peers crm-customer-management
```

### 5.2 The 20% That Matters (Missing Commands)

**Currently MISSING from CLI**:
1. **Package info/details** - Show full package metadata
2. **Bundle operations** - Install, verify, list bundles
3. **Recommendations** - Get personalized package suggestions
4. **Complementary discovery** - Find related packages
5. **Readiness checks** - Validate production deployment
6. **Quality reports** - Generate validation reports
7. **SPARQL queries** - Execute ontology queries
8. **Dependency graph** - Visualize package relationships

---

## Part 6: Recommendations for Packs Redesign

### 6.1 Core Commands (Must Have)

#### Package Discovery
```bash
ggen packs search <query> [OPTIONS]
  --sector <sector>           # "observability", "fintech"
  --only-8020                 # 80/20 certified only
  --production-ready          # Validated packages only
  --score-min <score>         # Minimum quality score
  --category <category>
  --fuzzy                     # Typo-tolerant search

ggen packs info <package>     # Show full package details
  --versions                  # List all versions
  --dependencies              # Show dependency tree
  --quality                   # Show validation scores

ggen packs list [OPTIONS]     # List available packages
  --installed                 # Show installed packages
  --sector <sector>
  --category <category>
```

#### Package Installation
```bash
ggen packs install <package>[@version] [OPTIONS]
  --force                     # Overwrite existing
  --no-dependencies           # Skip dependencies
  --dry-run                   # Simulate installation
  --target <path>             # Install to custom path

ggen packs uninstall <package>
ggen packs update <package>   # Update to latest version
ggen packs upgrade            # Upgrade all packages
```

#### Bundles (Composition)
```bash
ggen packs bundles list       # List all sector bundles
ggen packs bundles info <bundle-id>
ggen packs bundles install <bundle-id>
  --verify                    # Check compatibility before install
ggen packs bundles verify <bundle-id>
```

#### Recommendations (Autonomous)
```bash
ggen packs recommend          # Get personalized recommendations
  --based-on <package>        # Recommend based on package
ggen packs complements <package>  # Find complementary packages
ggen packs bundle-peers <package> # Find packages in same bundle
```

#### Validation & Quality
```bash
ggen packs validate <path>    # Validate package structure
  --config <config-toml>      # Custom validation config
  --report <output-file>      # Generate report
ggen packs readiness <path>   # Check production readiness
ggen packs score <path>       # Calculate quality score
```

#### Publishing
```bash
ggen packs publish <path>     # Publish to registry
  --tag <version>
  --force                     # Overwrite existing version
  --dry-run
```

### 6.2 Advanced Commands (Nice to Have)

#### Ontology & SPARQL
```bash
ggen packs sparql <query-file>  # Execute SPARQL query
  --package <package>           # Query specific package ontology
  --format <format>             # json, csv, table

ggen packs ontology <package>   # View package ontology
  --validate                    # Validate RDF syntax
```

#### Dependency Management
```bash
ggen packs deps tree <package>  # Show dependency tree
ggen packs deps graph <package> # Generate dependency graph
  --format <svg|dot|json>
ggen packs deps check           # Check for circular dependencies
```

#### Registry Management
```bash
ggen packs registry sync        # Update local registry cache
ggen packs registry status      # Show registry health
ggen packs registry config      # Show/set registry URL
```

### 6.3 Design Principles

1. **Composition over CRUD**:
   - Focus on bundles, recommendations, and multi-package workflows
   - Not just install/uninstall/list

2. **Quality-first**:
   - Expose validation scores, production readiness, 80/20 certification
   - Guide users to high-quality packages

3. **Autonomous assistance**:
   - Recommendations based on installed packages
   - Auto-detect missing dependencies
   - Suggest complementary packages

4. **Semantic discovery**:
   - Sector-based filtering
   - SPARQL query support
   - Ontology-powered search

5. **Production-ready**:
   - Readiness checks
   - Deployment guides
   - Quality reports

---

## Part 7: Data Structures & Algorithms

### 7.1 Key Algorithms

#### Relevance Scoring (search.rs:447-531)
```rust
fn calculate_relevance(pkg: &PackageMetadata, query: &str, fuzzy: bool) -> f64 {
    let mut score = 0.0;

    // Exact name match: 100 points
    if name_lower == query_lower {
        score += 100.0;
    }
    // Name contains: 50 points
    else if name_lower.contains(&query_lower) {
        score += 50.0;
    }
    // Fuzzy match: 30 points * similarity
    else if fuzzy {
        let distance = levenshtein_distance(&name_lower, &query_lower);
        let similarity = 1.0 - (distance / max_len);
        if similarity > 0.7 {
            score += similarity * 30.0;
        }
    }

    // Description: 20 points
    if desc_lower.contains(&query_lower) {
        score += 20.0;
    }

    // Tags/keywords: 10 points each
    for tag in &pkg.tags {
        if tag.contains(&query_lower) {
            score += 10.0;
        }
    }

    // Quality bonus: 0-1 points
    let desc_quality = if desc.len() > 200 { 1.0 } else { 0.0 };
    score += desc_quality;

    // Popularity boost (only if relevant)
    if score > 0.0 {
        score += (downloads / 1000.0).min(10.0);
        score += (stars / 10.0).min(5.0);
    }

    score
}
```

#### Circular Dependency Detection (install.rs:189-236)
```rust
fn dfs_cycle_check(
    &self,
    node: &str,
    visited: &mut HashSet<String>,
    rec_stack: &mut HashSet<String>,
) -> Result<()> {
    visited.insert(node.to_string());
    rec_stack.insert(node.to_string());

    if let Some(pkg) = self.nodes.get(node) {
        for (dep_name, dep_version) in &pkg.dependencies {
            let dep_key = format!("{}@{}", dep_name, dep_version);

            if !visited.contains(&dep_key) {
                self.dfs_cycle_check(&dep_key, visited, rec_stack)?;
            } else if rec_stack.contains(&dep_key) {
                // Back edge detected = cycle
                return Err(Error::new(&format!(
                    "Circular dependency: {} -> {}",
                    node, dep_key
                )));
            }
        }
    }

    rec_stack.remove(node);
    Ok(())
}
```

#### Topological Sort (install.rs:270-339)
```rust
fn topological_sort(&self) -> Result<Vec<String>> {
    let mut in_degree: HashMap<String, usize> = HashMap::new();
    let mut adj_list: HashMap<String, Vec<String>> = HashMap::new();

    // Initialize
    for key in self.nodes.keys() {
        in_degree.insert(key.clone(), 0);
        adj_list.insert(key.clone(), Vec::new());
    }

    // Build adjacency list and in-degree count
    for (key, node) in &self.nodes {
        for (dep_name, dep_version) in &node.dependencies {
            let dep_key = format!("{}@{}", dep_name, dep_version);
            if self.nodes.contains_key(&dep_key) {
                adj_list.get_mut(&dep_key)?.push(key.clone());
                *in_degree.get_mut(key)? += 1;
            }
        }
    }

    // Kahn's algorithm
    let mut queue: VecDeque<String> = in_degree
        .iter()
        .filter(|(_, &degree)| degree == 0)
        .map(|(k, _)| k.clone())
        .collect();

    let mut result = Vec::new();

    while let Some(node) = queue.pop_front() {
        result.push(node.clone());

        if let Some(neighbors) = adj_list.get(&node) {
            for neighbor in neighbors {
                *in_degree.get_mut(neighbor)? -= 1;
                if *in_degree.get(neighbor)? == 0 {
                    queue.push_back(neighbor.clone());
                }
            }
        }
    }

    if result.len() != self.nodes.len() {
        return Err(Error::new("Cycle detected during topological sort"));
    }

    Ok(result)
}
```

### 7.2 Performance Characteristics

**Search Performance**:
- **Without cache**: 376ms (JSON parse + scoring)
- **With cache**: <50ms (in-memory lookup)
- **Cache invalidation**: On registry sync

**Installation Performance**:
- **Download**: 120s timeout, 3 retries with exponential backoff
- **Extract**: Streaming (no memory limits)
- **Checksum**: SHA256 (mandatory for security)

**Validation Performance**:
- **File checks**: O(n) where n = files
- **RDF parsing**: O(t) where t = triples
- **SPARQL**: O(q * t) where q = queries, t = triples

---

## Part 8: Edge Cases & Constraints

### 8.1 Installation Constraints

**File System**:
- Package name validation (no path traversal, no control chars)
- Max name length: 100 chars
- Max archive size: 100MB (zip bomb protection)
- Max files in archive: 10,000

**Version Constraints**:
- Semver required (X.Y.Z format, numeric)
- No version overwrite (unless --force)
- Circular dependency detection (fails fast)

**Network Constraints**:
- 120s download timeout
- 3 retry attempts
- Rate limiting (HTTP 429 → extended backoff)
- Checksum verification (mandatory)

### 8.2 Validation Constraints

**Required Checks (must pass for production-ready)**:
- `package.toml` exists with valid TOML
- `README.md` >100 chars
- Source code OR templates directory
- License file

**Quality Checks (bonus points)**:
- RDF ontology >200 lines with >1 triple
- SPARQL queries (.rq or .sparql files)
- Examples directory with code files
- Tests directory with test files
- Docs directory with markdown

**Scoring**:
- Required: 60% weight
- Quality: 40% weight
- Production-ready threshold: 95%

### 8.3 Bundle Constraints

**Bundle Requirements**:
- All packages must exist in registry
- All packages must meet minimum score
- Dependency order must be resolvable
- No circular dependencies between bundle packages

**Sector Constraints**:
- Academic: 80% minimum score
- Enterprise: 80% minimum score
- Data: 80% minimum score
- Healthcare: 85% minimum score
- FinTech: 90% minimum score (highest)

---

## Part 9: Key Insights for Packs Redesign

### 9.1 What Packs Should Prioritize

**Top 5 Missing Capabilities (80/20 rule)**:
1. **Bundle operations** - Users compose multi-package stacks
2. **Recommendations** - Users discover related/complementary packages
3. **Package info** - Users need full metadata before install
4. **Readiness checks** - Users validate production deployment
5. **Dependency visualization** - Users understand package relationships

### 9.2 Anti-Patterns to Avoid

**Don't**:
- ❌ Make packs just a CRUD wrapper for marketplace
- ❌ Hide quality scores and production-ready status
- ❌ Ignore sector bundles and 80/20 certification
- ❌ Skip recommendations and complementary packages
- ❌ Neglect SPARQL and ontology capabilities

**Do**:
- ✅ Expose composition (bundles, recommendations)
- ✅ Surface quality signals (scores, readiness, 80/20)
- ✅ Enable semantic discovery (sectors, ontology, SPARQL)
- ✅ Provide autonomous assistance (recommendations, complements)
- ✅ Support production workflows (validation, readiness, reports)

### 9.3 Command Structure Proposal

**Primary Commands (verb-first)**:
```
ggen packs search <query>        # Discover packages
ggen packs info <package>        # View package details
ggen packs install <package>     # Install package
ggen packs validate <path>       # Validate package structure
ggen packs publish <path>        # Publish to registry
```

**Composition Commands (noun-first)**:
```
ggen packs bundles list          # List sector bundles
ggen packs bundles install <id>  # Install bundle
ggen packs recommend             # Get recommendations
ggen packs complements <pkg>     # Find complementary packages
```

**Advanced Commands (specialized)**:
```
ggen packs sparql <query>        # Execute SPARQL query
ggen packs ontology <pkg>        # View package ontology
ggen packs deps tree <pkg>       # Dependency tree
ggen packs readiness <path>      # Production readiness
```

---

## Conclusion

The marketplace is **far more than a package manager**. It's a **semantic discovery system** with:
- Ontology-powered search
- Quality-driven recommendations
- Production-ready validation
- Sector-based composition

**Packs should expose these capabilities**, not just provide CRUD operations. The 20% that delivers 80% value is:
1. Bundles (vertical stacks)
2. Recommendations (autonomous discovery)
3. Quality signals (scores, readiness, 80/20)
4. Semantic search (sectors, ontology, SPARQL)
5. Production workflows (validation, deployment)

**Next Steps**:
1. Review this analysis with the team
2. Design packs command taxonomy (verb vs noun)
3. Prioritize 20% features (bundles, recommendations, info)
4. Implement with marketplace domain layer (already exists!)
5. Add CLI glue code (thin layer over domain)
