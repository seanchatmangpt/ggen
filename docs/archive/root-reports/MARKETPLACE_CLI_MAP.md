# GGen Marketplace CLI Commands - Comprehensive Map

## Executive Summary

The ggen marketplace CLI is a multi-layered system with 20 implemented commands across search, install, publish, validation, and advanced maturity assessment. It bridges between three layers:
1. **CLI Layer** (`ggen-cli`): Command routing with clap-noun-verb v3.4.0 auto-discovery
2. **Domain Layer** (`ggen-domain`): Marketplace business logic (12.4K LOC)
3. **Legacy Layer** (`ggen-marketplace`): Maturity evaluation and assessment helpers

The system is undergoing a generational transition toward v2/v3 with RDF/SPARQL backends.

---

## File Structure & Locations

### Primary CLI Command Implementation
**File**: `/home/user/ggen/crates/ggen-cli/src/cmds/marketplace.rs`
- **Size**: 1,746 lines
- **Pattern**: clap-noun-verb `#[verb]` macro-based commands
- **Architecture**: Sync verb functions → async domain functions via `execute_async_verb()`
- **Entry Point**: Auto-discovered by clap-noun-verb::run()

### Domain Logic Layer
**Directory**: `/home/user/ggen/crates/ggen-domain/src/marketplace/`
- **Total Size**: 12.4K LOC across 22 modules
- **Key Modules**:
  - `install.rs` (1,649 LOC) - Package installation logic
  - `search.rs` (1,335 LOC) - Search and relevance scoring
  - `validate.rs` (1,106 LOC) - Package validation logic
  - `registry.rs` (1,103 LOC) - Package registry management
  - `publish.rs` (630 LOC) - Publishing logic
  - `guards.rs` (703 LOC) - Guard-based validation
  - And 16 supporting modules

### Legacy Marketplace
**Directory**: `/home/user/ggen/crates/ggen-marketplace/src/`
- **Purpose**: Maturity evaluation, assessment helpers, trait definitions
- **Key Exports**: MaturityEvaluator, MaturityAssessment, assessment_helpers
- **Status**: Being phased out in favor of v2/v3

### New Marketplace v2/v3
**Directory**: `/home/user/ggen/crates/ggen-marketplace-v2/src/`
- **Key Files**: 
  - `registry_rdf.rs` (12.3K LOC) - RDF/oxigraph backend
  - `v3.rs` (9.8K LOC) - V3 optimized implementation
  - `ontology.rs` (11K LOC) - RDF ontology definitions
- **Status**: Active development, not yet integrated into CLI

---

## 20 CLI Marketplace Commands

### 1. Search Commands (5 commands)

#### `ggen marketplace search`
```
fn search(query: String, limit: Option<usize>, category: Option<String>) -> Result<SearchOutput>
```
- **Lines**: 153-182
- **Input**: 
  - `query` (required): Search string
  - `limit` (optional): Result count (default 10)
  - `category` (optional): Category filter
- **Flow**: SearchInput → execute_search() → SearchResult[]
- **Domain Function**: `ggen_domain::marketplace::execute_search()`
- **Output Type**: SearchOutput { packages: Vec<PackageInfo>, total: usize }

#### `ggen marketplace search-maturity`
```
fn search_maturity(
    min_level: Option<String>,
    min_documentation: Option<u32>,
    min_testing: Option<u32>,
    min_security: Option<u32>,
    min_performance: Option<u32>,
    min_adoption: Option<u32>,
    min_maintenance: Option<u32>,
    exclude_maintenance_low: bool
) -> Result<serde_json::Value>
```
- **Lines**: 1035-1115
- **Purpose**: Search by maturity dimensions across 6 scoring axes
- **Implementation**: Generates assessments, applies dimension filters
- **Output**: JSON with search criteria, results, total_matches

#### `ggen marketplace compare`
```
fn compare(
    package_a: String,
    package_b: String,
    detailed: bool,
    output: Option<PathBuf>
) -> Result<serde_json::Value>
```
- **Lines**: 888-1015
- **Purpose**: Side-by-side maturity comparison of two packages
- **Dimensions Compared**: Documentation, testing, security, performance, adoption, maintenance
- **Output**: JSON comparison with winner analysis

### 2. Installation Commands (2 commands)

#### `ggen marketplace install`
```
fn install(
    package: String,
    target: Option<String>,
    force: bool,
    no_dependencies: bool,
    dry_run: bool
) -> Result<InstallOutput>
```
- **Lines**: 186-209
- **Input Type**: InstallInput
- **Domain Function**: `execute_install()`
- **Output Type**: InstallOutput {
  - package: String
  - version: String
  - path: String
  - dependencies: Vec<String>
}
- **Security**: Package name validated against injection attacks in domain layer

#### `ggen marketplace install-bundle`
```
fn install_bundle(bundle_id: String, dry_run: bool) -> Result<serde_json::Value>
```
- **Lines**: 1357-1416
- **Purpose**: Install complete sector bundles (academic papers, microservices, etc.)
- **Domain Function**: BundleRegistry::get_bundle()
- **Creates**: `.ggen-bundle-{id}.json` manifest file
- **Output**: Installation status and package list

### 3. Publishing Commands (1 command)

#### `ggen marketplace publish`
```
fn publish(
    path: PathBuf,
    tag: Option<String>,
    dry_run: bool,
    force: bool
) -> Result<PublishOutput>
```
- **Lines**: 295-315
- **Input Type**: PublishInput
- **Domain Function**: `execute_publish()`
- **Output Type**: PublishOutput {
  - package: String
  - version: String
}
- **Dry-run**: Simulates publishing without committing changes

### 4. Package List Commands (1 command)

#### `ggen marketplace list`
```
fn list(
    detailed: bool,
    json: bool,
    min_maturity: Option<String>,
    maturity_level: Option<String>,
    sort: Option<String>
) -> Result<ListOutput>
```
- **Lines**: 229-291
- **Input Type**: ListInput
- **Domain Function**: `execute_list()`
- **Output Type**: ListOutput {
  - packages: Vec<InstalledPackage>
  - total: usize
}
- **Filtering**: By maturity levels (experimental, beta, production, enterprise)
- **Sorting**: By maturity, downloads, or update time

### 5. Validation Commands (2 commands)

#### `ggen marketplace validate`
```
fn validate(
    package: Option<String>,
    packages_dir: Option<PathBuf>,
    update: bool,
    require_level: Option<String>,
    improvement_plan: bool
) -> Result<ValidateOutput>
```
- **Lines**: 335-452
- **Purpose**: Validate single or all packages for production readiness
- **Domain Functions**: 
  - `validate_package()` (single)
  - `validate_all_packages()` (batch)
- **Maturity Gating**: Enforces minimum scores (0/41/61/81 for experimental/beta/production/enterprise)
- **Output Type**: ValidateOutput {
  - score: f64
  - production_ready: bool
  - total_packages: usize
  - ready_count, needs_improvement_count, not_ready_count
  - details: Option<PackageValidation>
  - all_results: Option<Vec<PackageValidation>>
}

#### `ggen marketplace emit-receipts`
```
fn emit_receipts(report: bool) -> Result<serde_json::Value>
```
- **Lines**: 1430-1489
- **Purpose**: Generate validation receipts for all marketplace packages
- **Domain Function**: `emit_receipts_for_marketplace()`
- **Report**: Optional validation report generation
- **Output**: Results map with success/error per package

### 6. Maturity Assessment Commands (3 commands)

#### `ggen marketplace maturity`
```
fn maturity(
    package_id: String,
    detailed: bool,
    verify: Option<String>
) -> Result<MaturityOutput>
```
- **Lines**: 469-549
- **Purpose**: Assess single package across 6 maturity dimensions
- **Dimensions**:
  1. Documentation
  2. Testing
  3. Security
  4. Performance
  5. Adoption
  6. Maintenance
- **Domain Type**: Uses MaturityEvaluator from ggen-marketplace (legacy)
- **Verification**: Can require minimum level (experimental/beta/production/enterprise)
- **Output Type**: MaturityOutput {
  - total_score: u32
  - maturity_level: String
  - scores: MaturityScores (6 dimensions)
  - percentages: HashMap<String, f32>
  - feedback: Vec<(String, Vec<String>)>
  - next_steps: Vec<&'static str>
}

#### `ggen marketplace maturity-batch`
```
fn maturity_batch(
    packages_dir: Option<PathBuf>,
    _format: Option<String>,
    output: Option<PathBuf>
) -> Result<DashboardOutput>
```
- **Lines**: 685-771
- **Purpose**: Assess multiple packages in batch
- **Output**: DashboardOutput with statistics and assessments

#### `ggen marketplace dashboard`
```
fn dashboard(
    packages_dir: Option<PathBuf>,
    _format: Option<String>,
    output: Option<PathBuf>,
    min_maturity: Option<String>
) -> Result<DashboardOutput>
```
- **Lines**: 566-671
- **Purpose**: Generate marketplace maturity dashboard with statistics
- **Filtering**: By minimum maturity level
- **Statistics**: Level distribution, average scores by dimension
- **Export**: JSON format to file

### 7. Recommendation Commands (1 command)

#### `ggen marketplace recommend`
```
fn recommend(
    use_case: String,
    priority: Option<String>,
    min_score: Option<u32>,
    min_dimension_score: Option<u32>
) -> Result<serde_json::Value>
```
- **Lines**: 788-871
- **Purpose**: Recommend packages based on use cases
- **Use Cases**: production, research, enterprise, startup
- **Priority Filters**: security focus with dimension-level gating
- **Min Scores**: Contextual defaults (production: 65, research: 40, enterprise: 85, startup: 50)
- **Output**: Ranked recommendations with rationale

### 8. Data Export Commands (1 command)

#### `ggen marketplace export`
```
fn export(
    format: Option<String>,
    output: Option<PathBuf>,
    detailed: bool,
    min_maturity: Option<String>
) -> Result<serde_json::Value>
```
- **Lines**: 1135-1225
- **Formats**: CSV, JSON, HTML
- **Domain Function**: Uses ggen_marketplace::prelude exports
- **Filtering**: By maturity level
- **Output**: File written to disk + status JSON

### 9. Bundle Management Commands (2 commands)

#### `ggen marketplace list-bundles`
```
fn list_bundles(detailed: bool) -> Result<serde_json::Value>
```
- **Lines**: 1239-1280
- **Purpose**: List all marketplace sector bundles
- **Domain Type**: BundleRegistry from ggen-domain
- **Output**: Bundle metadata (id, version, domain, package count, features)

#### `ggen marketplace bundle-info`
```
fn bundle_info(bundle_id: String, docs: bool) -> Result<serde_json::Value>
```
- **Lines**: 1294-1343
- **Purpose**: Show details for specific bundle
- **Includes**: Packages, features, minimum score requirements
- **Docs**: Optional auto-generated documentation

### 10. Artifact Generation Commands (2 commands)

#### `ggen marketplace generate-artifacts`
```
fn generate_artifacts(
    json_output: Option<PathBuf>,
    md_output: Option<PathBuf>
) -> Result<serde_json::Value>
```
- **Lines**: 1560-1600
- **Purpose**: Generate marketplace artifacts from receipts
- **Outputs**:
  - JSON registry index
  - Markdown package documentation
- **Domain Functions**:
  - `generate_registry_index()`
  - `generate_packages_markdown()`
  - `write_registry_index()`
  - `write_packages_markdown()`

#### `ggen marketplace report`
```
fn report(output: Option<PathBuf>) -> Result<serde_json::Value>
```
- **Lines**: 1503-1546
- **Purpose**: Generate marketplace validation report
- **Metrics**: 
  - Total packages
  - Production ready count
  - Average/median scores
  - Score distribution buckets (95+, 80-94, <80)
- **Domain Function**: `generate_validation_report()`
- **Output**: File + JSON summary

### 11. Improvement Planning Commands (1 command)

#### `ggen marketplace improve`
```
fn improve(
    package_id: String,
    apply: Option<String>
) -> Result<serde_json::Value>
```
- **Lines**: 1614-1702
- **Purpose**: Generate improvement suggestions and optionally apply templates
- **Domain Functions**:
  - `generate_improvement_plan()`
  - `apply_template_improvements()`
- **Output**: Improvement plan with suggestions and template availability

---

## Architecture: Three-Layer Communication

### Layer 1: CLI Command Functions (sync)
```
marketplace.rs verb functions
├─ Takes String/PathBuf/bool arguments
├─ Uses execute_async_verb() wrapper
└─ Returns clap_noun_verb::Result<T>
```

### Layer 2: Runtime Bridge
```
runtime_helper.rs execute_async_verb()
├─ Detects existing tokio runtime
├─ Creates new runtime if needed
├─ Handles thread spawning for nested runtimes
└─ Converts anyhow::Error → NounVerbError
```

### Layer 3: Async Domain Functions (async)
```
ggen-domain/marketplace/ async functions
├─ Search, Install, Publish, Validate, List
├─ Accept domain input types (SearchInput, InstallInput, etc.)
├─ Use legacy ggen-marketplace types for maturity (transitional)
└─ Return Result<DomainOutputType>
```

---

## Input/Output Type Mapping

### Core Input Types (ggen-domain)
```rust
SearchInput {
    query: String,
    limit: usize,
    category: Option<String>,
    keyword: Option<String>,
    author: Option<String>,
    fuzzy: bool,
    only_8020: bool,
    ..
}

InstallInput {
    package: String,
    target: Option<String>,
    force: bool,
    no_dependencies: bool,
    dry_run: bool,
}

PublishInput {
    path: PathBuf,
    tag: Option<String>,
    dry_run: bool,
    force: bool,
}

ListInput {
    detailed: bool,
    json: bool,
}

ValidateInput {
    package: Option<String>,
    packages_dir: Option<PathBuf>,
    update: bool,
    require_level: Option<String>,
    improvement_plan: bool,
}
```

### Core Output Types (CLI)
```rust
SearchOutput {
    packages: Vec<PackageInfo>,
    total: usize,
}

InstallOutput {
    package: String,
    version: String,
    path: String,
    dependencies: Vec<String>,
}

PublishOutput {
    package: String,
    version: String,
}

ListOutput {
    packages: Vec<InstalledPackage>,
    total: usize,
}

ValidateOutput {
    score: f64,
    production_ready: bool,
    total_packages: usize,
    ready_count: usize,
    needs_improvement_count: usize,
    not_ready_count: usize,
    details: Option<PackageValidation>,
    all_results: Option<Vec<PackageValidation>>,
}

MaturityOutput {
    total_score: u32,
    maturity_level: String,
    scores: MaturityScores,
    percentages: HashMap<String, f32>,
    feedback: Vec<(String, Vec<String>)>,
    next_steps: Vec<&'static str>,
}

DashboardOutput {
    generated_at: String,
    statistics: DashboardStats,
    assessments: Vec<MaturityOutput>,
}
```

---

## Dependency Analysis: Old vs New Marketplace

### Current Integration (Transitional)

**CLI Layer** uses imports from BOTH:

1. **ggen-domain** (Primary, modern)
   ```rust
   use ggen_domain::marketplace::{
       execute_install, execute_list, execute_publish, execute_search,
       validate_all_packages, validate_package,
       InstallInput, ListInput, PublishInput, SearchInput, PackageValidation,
   };
   ```
   - Located: `/home/user/ggen/crates/ggen-domain/src/marketplace/`
   - Modules: search, install, publish, list, validate, registry, guards, bundles, etc.

2. **ggen-marketplace** (Legacy, being phased out)
   ```rust
   use ggen_marketplace::prelude::*;
   ```
   - Used for: MaturityEvaluator, MaturityAssessment, assessment_helpers
   - Located: `/home/user/ggen/crates/ggen-marketplace/src/`
   - Status: Providing maturity evaluation; core search/install/publish moved to ggen-domain

### Legacy Marketplace Exports Used in CLI
```rust
// From ggen-marketplace crate
MaturityEvaluator::evaluate(input: EvaluationInput)
MaturityAssessment::new(id, name)
MaturityDashboard::new(assessments)
MaturityLevel { Experimental, Beta, Production, Enterprise }
EvaluationInput { has_readme, test_coverage, unsafe_code_percent, ... }

// Helper types
assessment_helpers exports
generate_all_assessments()
find_for_use_case()
export_as_csv()
export_as_json()
```

### New Marketplace v2/v3 (Not Yet Integrated)
- Located: `/home/user/ggen/crates/ggen-marketplace-v2/src/`
- Status: **Parallel implementation**
- Key Components:
  - RDF registry with oxigraph
  - SPARQL query engine
  - Advanced ontology system
  - V3OptimizedRegistry
- Integration Status: **Awaiting refactoring** - currently separate crate

---

## Key Integration Points

### 1. Cargo.toml Dependencies
**File**: `/home/user/ggen/crates/ggen-cli/Cargo.toml`

```toml
[dependencies]
# Local crates
ggen-domain = { path = "../ggen-domain", version = "3.0.0" }
ggen-marketplace = { path = "../ggen-marketplace", version = "3.0.0" }

# Command routing
clap-noun-verb.workspace = true
clap-noun-verb-macros = "3.4.0"

# Async runtime
tokio.workspace = true

# I/O and utilities
anyhow.workspace = true
serde.workspace = true
serde_json.workspace = true
toml.workspace = true
```

### 2. Command Routing Pipeline
```
1. ggen binary (main.rs)
   └─> ggen_cli_lib::cli_match()
       └─> cmds::run_cli()
           └─> clap_noun_verb::run() [auto-discovery]
               └─> Finds all #[verb] functions
                   └─> marketplace.rs verb functions
                       └─> execute_async_verb()
                           └─> ggen-domain marketplace:: functions
```

### 3. Marketplace Command Auto-Discovery
**Mechanism**: clap-noun-verb v3.4.0 macro auto-discovery
- All `#[verb]` functions in `cmds/marketplace.rs` automatically registered
- Module declared in `cmds/mod.rs`:
  ```rust
  pub mod marketplace;  // Lines 15 in mod.rs
  ```
- Auto-discovered on startup with no manual routing code needed

---

## Security & Error Handling

### Input Validation
**Location**: `ggen-domain/src/marketplace/install.rs`
```rust
fn validate_package_name(name: &str) -> Result<()> {
    // Checks:
    // - Non-empty
    // - Max 100 chars
    // - No path separators (.., /, \)
    // - No control characters
}
```

### Type-Level Security (Poka-Yoke)
**File**: `ggen-domain/src/marketplace/types.rs`
```rust
pub struct ValidatedPackageName(String);
// Validated at construction, impossible to create invalid state
```

### Error Handling Chain
```
Verb function error (clap_noun_verb::NounVerbError)
    ↓ (wrapped by execute_async_verb)
Anyhow error (anyhow::Error)
    ↓ (propagated from domain function)
Result<T> in domain layer (ggen_utils::error::Result)
```

---

## Current State & Migration Path

### What's Complete (Production)
- [x] Search functionality (1,335 LOC)
- [x] Install functionality (1,649 LOC)
- [x] Publish functionality (630 LOC)
- [x] List functionality (357 LOC)
- [x] Validate functionality (1,106 LOC)
- [x] Maturity assessment (from ggen-marketplace legacy)
- [x] Bundle management (271 LOC)
- [x] Artifact generation (294 LOC)
- [x] CLI command routing (20 commands, 1,746 LOC total)

### What's In Progress (Parallel Implementation)
- [ ] ggen-marketplace-v2 with RDF backend
  - registry_rdf.rs (12.3K)
  - ontology.rs (11K)
  - v3.rs (9.8K)
  - search_sparql.rs
- [ ] Integration into CLI (awaiting refactoring)

### Dependencies on Old Marketplace
```
CLI (marketplace.rs)
├─> ggen-domain (primary - 12.4K)
│   ├─> search, install, publish, list, validate, registry
│   └─> No external marketplace dependency
└─> ggen-marketplace (legacy - for MaturityEvaluator only)
    └─> maturity_evaluator, assessment_helpers
```

---

## File Summary Table

| File | Lines | Purpose | Entry Points |
|------|-------|---------|--------------|
| `ggen-cli/src/cmds/marketplace.rs` | 1,746 | 20 verb commands | search, install, list, publish, validate, maturity, dashboard, recommend, compare, search-maturity, export, list-bundles, bundle-info, install-bundle, emit-receipts, report, generate-artifacts, improve |
| `ggen-domain/src/marketplace/mod.rs` | 92 | Module router | Re-exports |
| `ggen-domain/src/marketplace/search.rs` | 1,335 | Search logic | execute_search() |
| `ggen-domain/src/marketplace/install.rs` | 1,649 | Install logic | execute_install() |
| `ggen-domain/src/marketplace/validate.rs` | 1,106 | Validation logic | validate_package(), validate_all_packages() |
| `ggen-domain/src/marketplace/registry.rs` | 1,103 | Registry ops | Registry trait + impl |
| `ggen-domain/src/marketplace/publish.rs` | 630 | Publish logic | execute_publish() |
| `ggen-domain/src/marketplace/guards.rs` | 703 | Guard-based validation | Guard implementation |
| `ggen-domain/src/marketplace/bundles.rs` | 271 | Bundle management | BundleRegistry |
| `ggen-domain/src/marketplace/artifact_generator.rs` | 294 | Artifact generation | generate_registry_index(), generate_packages_markdown() |
| `ggen-cli/src/runtime_helper.rs` | 198 | Async bridge | execute_async_verb() |
| `ggen-marketplace-v2/src/registry_rdf.rs` | 12,297 | RDF backend (new) | RdfRegistry |
| `ggen-marketplace-v2/src/v3.rs` | 9,876 | V3 optimization (new) | V3OptimizedRegistry |

---

## Testing Infrastructure

**Test Files**:
1. `ggen-cli/tests/marketplace_concurrent_test.rs`
   - Concurrent read/write patterns
   - Race condition testing
   - Permutation testing

2. `ggen-domain/src/marketplace/integration_tests.rs` (502 LOC)
   - End-to-end CLI flow tests

3. `ggen-domain/src/marketplace/expert_tests.rs` (417 LOC)
   - Expert system validation

4. `ggen-domain/src/marketplace/types_tests.rs` (596 LOC)
   - Type validation tests

**Benchmarks**:
- `ggen-cli/benches/marketplace_benchmark.rs`
- `ggen-cli/benches/marketplace_search_benchmark.rs`

---

## Command Invocation Examples

```bash
# Search
ggen marketplace search --query "rust web" --limit 10

# Install
ggen marketplace install "my-package@1.0.0" --no-dependencies

# Publish
ggen marketplace publish ./my-package --tag v1.0.0

# List
ggen marketplace list --min-maturity production

# Validate
ggen marketplace validate --package "my-package"
ggen marketplace validate --update  # Validate all

# Maturity
ggen marketplace maturity "io.ggen.rust.microservice" --detailed

# Dashboard
ggen marketplace dashboard --min-maturity production

# Recommend
ggen marketplace recommend --use-case production --priority security

# Bundle
ggen marketplace install-bundle sector-academic-papers --dry-run

# Export
ggen marketplace export --format csv --output packages.csv

# Improve
ggen marketplace improve "my-package" --apply license-mit
```

