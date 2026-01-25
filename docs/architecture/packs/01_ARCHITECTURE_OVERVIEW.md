<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Packs System Architecture Overview](#packs-system-architecture-overview)
  - [Executive Summary](#executive-summary)
  - [Current State Analysis](#current-state-analysis)
    - [Existing Implementation](#existing-implementation)
    - [Root Causes](#root-causes)
  - [Architecture Principles](#architecture-principles)
    - [1. Layered Architecture](#1-layered-architecture)
    - [2. Integration Points](#2-integration-points)
      - [Marketplace Integration](#marketplace-integration)
      - [Template Integration](#template-integration)
      - [Graph Integration](#graph-integration)
  - [Core Components](#core-components)
    - [1. Pack Manifest (types.rs)](#1-pack-manifest-typesrs)
    - [2. Service Layer (services/)](#2-service-layer-services)
    - [3. Integration Adapters (adapters/)](#3-integration-adapters-adapters)
  - [Data Flow Diagrams](#data-flow-diagrams)
    - [Complete Workflow: Install Pack + Generate Project](#complete-workflow-install-pack--generate-project)
    - [Dependency Resolution Flow](#dependency-resolution-flow)
  - [Failure Modes and Mitigations (FMEA-Ready)](#failure-modes-and-mitigations-fmea-ready)
    - [FM1: Pack Manifest Not Found](#fm1-pack-manifest-not-found)
    - [FM2: Marketplace Package Install Fails](#fm2-marketplace-package-install-fails)
    - [FM3: Template Rendering Fails](#fm3-template-rendering-fails)
    - [FM4: Circular Dependency Detected](#fm4-circular-dependency-detected)
    - [FM5: Version Conflict](#fm5-version-conflict)
    - [FM6: Partial Installation State](#fm6-partial-installation-state)
    - [FM7: Template Variable Missing](#fm7-template-variable-missing)
    - [FM8: SPARQL Query Fails](#fm8-sparql-query-fails)
    - [FM9: Network Timeout During Download](#fm9-network-timeout-during-download)
    - [FM10: Disk Space Exhausted](#fm10-disk-space-exhausted)
  - [Performance Considerations](#performance-considerations)
    - [Caching Strategy](#caching-strategy)
    - [Parallel Execution](#parallel-execution)
    - [Batch Operations](#batch-operations)
  - [Security Considerations](#security-considerations)
    - [Input Validation](#input-validation)
    - [Checksum Verification](#checksum-verification)
    - [Sandboxing](#sandboxing)
  - [Testing Strategy](#testing-strategy)
    - [Unit Tests (per module)](#unit-tests-per-module)
    - [Integration Tests (cross-module)](#integration-tests-cross-module)
    - [E2E Tests (complete workflows)](#e2e-tests-complete-workflows)
    - [Performance Tests](#performance-tests)
  - [Success Metrics](#success-metrics)
    - [Health Score Target: 90%+](#health-score-target-90)
    - [Key Performance Indicators](#key-performance-indicators)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Packs System Architecture Overview

**Version:** 3.2.0
**Status:** Production Design
**Target Health Score:** 90%+ (from current 30.75%)

## Executive Summary

The packs system enables complete project workflows by providing curated collections of packages, templates, and configurations. This redesign transforms packs from a stub implementation (health 30.75/100) into a production-ready system that integrates deeply with marketplace, templates, and ggen-core.

## Current State Analysis

### Existing Implementation
- **Health Score:** 30.75/100
- **Issues:**
  - No real marketplace integration (stubs only)
  - No template generation (commented out)
  - No dependency resolution
  - Mock SPARQL queries
  - All 6 user workflows fail
  - Incomplete command implementation

### Root Causes
1. Packs domain isolated from marketplace/template domains
2. No bidirectional data flow between layers
3. Missing service layer for orchestration
4. No real package installation
5. Template engine not connected

## Architecture Principles

### 1. Layered Architecture
```
┌──────────────────────────────────────┐
│   CLI Layer (ggen-cli)               │
│   - Command handlers                 │
│   - User interaction                 │
└────────────┬─────────────────────────┘
             │
┌────────────▼─────────────────────────┐
│   Domain Layer (ggen-domain/packs)   │
│   - Business logic                   │
│   - Orchestration services           │
│   - Trait definitions                │
└────────────┬─────────────────────────┘
             │
┌────────────▼─────────────────────────┐
│   Infrastructure Layer (ggen-core)   │
│   - Template engine                  │
│   - RDF graph                        │
│   - File operations                  │
└──────────────────────────────────────┘
             │
┌────────────▼─────────────────────────┐
│   External Systems                   │
│   - Marketplace registry             │
│   - GitHub repositories              │
│   - File system                      │
└──────────────────────────────────────┘
```

### 2. Integration Points

#### Marketplace Integration
```rust
// Packs -> Marketplace
packs::install() -> marketplace::install_package()
packs::validate() -> marketplace::validate_package()
packs::list() -> marketplace::search()

// Data Flow
Pack Manifest -> Package Names -> Marketplace Install -> Local Packages
```

#### Template Integration
```rust
// Packs -> Template Engine
packs::generate() -> template::generate_file()
packs::render_templates() -> generator::generate()

// Data Flow
Pack Templates -> Variable Substitution -> Template Rendering -> Generated Files
```

#### Graph Integration
```rust
// Packs -> RDF Graph
packs::sparql() -> graph::query()
packs::metadata() -> graph::insert_turtle()

// Data Flow
Pack Metadata -> RDF Triples -> SPARQL Queries -> Query Results
```

## Core Components

### 1. Pack Manifest (types.rs)
Central data structure representing a pack:

```rust
pub struct Pack {
    // Identity
    pub id: String,
    pub name: String,
    pub version: String,

    // Metadata
    pub description: String,
    pub category: String,
    pub author: Option<String>,
    pub tags: Vec<String>,

    // Content
    pub packages: Vec<PackageRef>,      // Real packages to install
    pub templates: Vec<PackTemplate>,   // Templates to render
    pub sparql_queries: HashMap<String, String>,

    // Dependencies
    pub dependencies: Vec<PackDependency>,

    // Validation
    pub metadata: PackMetadata,
}
```

### 2. Service Layer (services/)
Orchestrates complex workflows:

```rust
pub struct PackInstallationService {
    marketplace_client: Arc<dyn MarketplaceClient>,
    template_engine: Arc<dyn TemplateEngine>,
    dependency_resolver: Arc<DependencyResolver>,
}

pub struct PackGenerationService {
    template_engine: Arc<dyn TemplateEngine>,
    variable_resolver: Arc<VariableResolver>,
    file_writer: Arc<FileWriter>,
}

pub struct PackValidationService {
    marketplace_validator: Arc<dyn Validator>,
    template_validator: Arc<dyn TemplateValidator>,
    dependency_validator: Arc<DependencyValidator>,
}
```

### 3. Integration Adapters (adapters/)
Bridge to external systems:

```rust
pub trait MarketplaceClient {
    async fn install_package(&self, name: &str, version: &str) -> Result<InstallResult>;
    async fn search_packages(&self, query: &str) -> Result<Vec<PackageMetadata>>;
    async fn validate_package(&self, name: &str) -> Result<ValidationResult>;
}

pub trait TemplateEngine {
    fn render(&self, template: &Path, vars: &Variables) -> Result<String>;
    fn render_file_tree(&self, templates: &[Template]) -> Result<GenerationResult>;
}

pub trait DependencyResolver {
    fn resolve(&self, deps: &[Dependency]) -> Result<DependencyGraph>;
    fn detect_conflicts(&self, graph: &DependencyGraph) -> Result<Vec<Conflict>>;
}
```

## Data Flow Diagrams

### Complete Workflow: Install Pack + Generate Project

```
User: ggen packs install web-api-starter
│
├──> PackInstallationService::install()
│    │
│    ├──> Load Pack Manifest (metadata.rs)
│    │    └──> Parse TOML -> Pack struct
│    │
│    ├──> Resolve Dependencies (dependency_resolver.rs)
│    │    ├──> Build dependency graph
│    │    ├──> Detect circular dependencies
│    │    └──> Topological sort for install order
│    │
│    ├──> Install Packages (marketplace adapter)
│    │    ├──> For each package in pack.packages:
│    │    │    └──> marketplace::install_package()
│    │    │         ├──> Download from registry
│    │    │         ├──> Verify checksums
│    │    │         └──> Extract to ~/.ggen/packages
│    │    │
│    │    └──> Update lockfile
│    │
│    └──> Return InstallResult

User: ggen packs generate web-api-starter my-api
│
├──> PackGenerationService::generate()
│    │
│    ├──> Load Pack Templates (metadata.rs)
│    │    └──> Read pack.templates
│    │
│    ├──> Resolve Variables (variable_resolver.rs)
│    │    ├──> User-provided: project_name="my-api"
│    │    ├──> Auto-detected: author, timestamp
│    │    └──> Template defaults
│    │
│    ├──> Render Templates (template adapter)
│    │    ├──> For each template in pack.templates:
│    │    │    └──> template::generate_file()
│    │    │         ├──> Load template file
│    │    │         ├──> Apply variable substitution
│    │    │         ├──> Process Tera filters
│    │    │         └──> Write output file
│    │    │
│    │    └──> Generate file tree
│    │
│    └──> Return GenerateResult
```

### Dependency Resolution Flow

```
Input: Pack with dependencies
│
├──> DependencyResolver::resolve()
│    │
│    ├──> Build Dependency Graph
│    │    ├──> Add pack's direct dependencies
│    │    ├──> Recursively load transitive dependencies
│    │    └──> Create nodes and edges
│    │
│    ├──> Detect Circular Dependencies
│    │    ├──> DFS with recursion stack
│    │    └──> Return error if cycle detected
│    │
│    ├──> Check Version Conflicts
│    │    ├──> Group dependencies by name
│    │    ├──> Check version compatibility
│    │    └──> Resolve using semver rules
│    │
│    ├──> Topological Sort
│    │    ├──> Kahn's algorithm
│    │    └──> Return install order
│    │
│    └──> Return DependencyGraph
```

## Failure Modes and Mitigations (FMEA-Ready)

### FM1: Pack Manifest Not Found
- **Detection:** File existence check before parse
- **Mitigation:** Clear error with pack search suggestions
- **Recovery:** Offer to download from marketplace

### FM2: Marketplace Package Install Fails
- **Detection:** Install result status check
- **Mitigation:** Retry with exponential backoff (3 attempts)
- **Recovery:** Rollback partial installation, restore lockfile

### FM3: Template Rendering Fails
- **Detection:** Template engine error codes
- **Mitigation:** Validate variables before rendering
- **Recovery:** Skip failed template, log error, continue with others

### FM4: Circular Dependency Detected
- **Detection:** DFS cycle detection in dependency graph
- **Mitigation:** Fail fast with clear cycle path
- **Recovery:** User must modify pack manifest to break cycle

### FM5: Version Conflict
- **Detection:** Semver compatibility check
- **Mitigation:** Attempt automatic resolution to highest compatible version
- **Recovery:** If unresolvable, fail with conflict report

### FM6: Partial Installation State
- **Detection:** Installation interrupted (signal, crash)
- **Mitigation:** Atomic operations, lockfile state tracking
- **Recovery:** Detect partial state on next run, offer cleanup or resume

### FM7: Template Variable Missing
- **Detection:** Tera rendering error on undefined variable
- **Mitigation:** Pre-validate required variables before rendering
- **Recovery:** Prompt user for missing variables or use defaults

### FM8: SPARQL Query Fails
- **Detection:** Graph query error
- **Mitigation:** Validate SPARQL syntax in pack manifest
- **Recovery:** Return empty results, log warning

### FM9: Network Timeout During Download
- **Detection:** HTTP request timeout
- **Mitigation:** Increase timeout, retry with backoff
- **Recovery:** Use cached version if available

### FM10: Disk Space Exhausted
- **Detection:** File write error (ENOSPC)
- **Mitigation:** Check available space before installation
- **Recovery:** Clean up partial files, report required space

## Performance Considerations

### Caching Strategy
```rust
// Multi-level cache
1. Pack Manifest Cache (in-memory)
   - TTL: 5 minutes
   - Invalidation: On manifest modification

2. Template Cache (filesystem)
   - Location: ~/.ggen/cache/templates
   - Invalidation: On version change

3. Package Download Cache (filesystem)
   - Location: ~/.ggen/cache/downloads
   - Integrity: SHA256 verification
   - Cleanup: LRU, max 1GB
```

### Parallel Execution
```rust
// Independent packages install in parallel
for packages in dependency_levels {
    // All packages in same level have no dependencies on each other
    tokio::join!(
        install_package(packages[0]),
        install_package(packages[1]),
        install_package(packages[2]),
    );
}
```

### Batch Operations
```rust
// Batch template rendering
let rendered: Vec<_> = templates
    .par_iter()  // Rayon parallel iterator
    .map(|tmpl| render_template(tmpl, &vars))
    .collect();
```

## Security Considerations

### Input Validation
- Package names: Alphanumeric + hyphens, max 100 chars
- Versions: Semver format validation
- Paths: No directory traversal (../)
- SPARQL: Query size limits, timeout enforcement

### Checksum Verification
- All downloaded packages: SHA256
- Pack manifests: Signed with PQC signatures
- Template files: Integrity check on cache hit

### Sandboxing
- Template rendering: Tera sandbox mode (no file access)
- SPARQL queries: Read-only graph access
- Package installation: Restricted to ~/.ggen/packages

## Testing Strategy

### Unit Tests (per module)
- Pack manifest parsing/validation
- Dependency resolution algorithms
- Template variable substitution
- SPARQL query execution

### Integration Tests (cross-module)
- Pack install -> marketplace install
- Pack generate -> template render
- Dependency resolution -> package install order

### E2E Tests (complete workflows)
- All 6 user workflows (see USER_WORKFLOWS.md)
- Error scenarios (network failure, disk full)
- Concurrent operations (multiple installs)

### Performance Tests
- Large pack installation (50+ packages)
- Complex dependency graphs (20+ levels)
- Template rendering throughput (100+ files)
- SPARQL query performance (1000+ triples)

## Success Metrics

### Health Score Target: 90%+
- Test coverage: 90%+
- All 6 workflows pass: 100%
- Error handling: All failure modes covered
- Documentation: Complete API docs + user guides
- Performance: <5s for typical pack install

### Key Performance Indicators
- Installation success rate: >99%
- Template rendering errors: <1%
- Dependency resolution time: <1s for typical packs
- User satisfaction: Workflows intuitive and reliable

## Next Steps

1. Implement service layer (priority: HIGH)
2. Create marketplace/template adapters (priority: HIGH)
3. Build dependency resolver (priority: HIGH)
4. Add comprehensive error handling (priority: HIGH)
5. Implement caching strategy (priority: MEDIUM)
6. Add telemetry and monitoring (priority: MEDIUM)
7. Performance optimization (priority: LOW)

---

**Related Documents:**
- [Command Specification](02_COMMAND_SPECIFICATION.md)
- [Data Model](03_DATA_MODEL.md)
- [Integration Layer](04_INTEGRATION_LAYER.md)
- [User Workflows](05_USER_WORKFLOWS.md)
- [Implementation Guide](06_IMPLEMENTATION_GUIDE.md)
