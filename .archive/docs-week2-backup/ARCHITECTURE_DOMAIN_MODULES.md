<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Domain Module Architecture - Marketplace, Template, Project](#domain-module-architecture---marketplace-template-project)
  - [Executive Summary](#executive-summary)
    - [Key Principles](#key-principles)
  - [Current Architecture Analysis](#current-architecture-analysis)
    - [Existing Module Organization (ggen-domain)](#existing-module-organization-ggen-domain)
    - [Best Practice Example: `ai/mod.rs`](#best-practice-example-aimodrs)
  - [Proposed Architecture](#proposed-architecture)
    - [1. Marketplace Domain Module](#1-marketplace-domain-module)
      - [Public API Functions](#public-api-functions)
      - [Type Definitions](#type-definitions)
      - [Module Organization](#module-organization)
    - [2. Template Domain Module](#2-template-domain-module)
      - [Public API Functions](#public-api-functions-1)
      - [Type Definitions](#type-definitions-1)
      - [Module Organization](#module-organization-1)
    - [3. Project Domain Module](#3-project-domain-module)
      - [Public API Functions](#public-api-functions-2)
      - [Type Definitions](#type-definitions-2)
      - [Module Organization](#module-organization-2)
  - [Cross-Module Design](#cross-module-design)
    - [Dependency Graph](#dependency-graph)
    - [Shared Types](#shared-types)
    - [Error Handling Strategy](#error-handling-strategy)
  - [API Design Principles](#api-design-principles)
    - [1. All Functions Are Async](#1-all-functions-are-async)
    - [2. All Output Types Derive Serialize](#2-all-output-types-derive-serialize)
    - [3. All Functions Return Result<T>](#3-all-functions-return-resultt)
    - [4. No CLI Dependencies](#4-no-cli-dependencies)
  - [Implementation Checklist](#implementation-checklist)
    - [Marketplace Module](#marketplace-module)
    - [Template Module](#template-module)
    - [Project Module](#project-module)
  - [Testing Strategy (Chicago TDD)](#testing-strategy-chicago-tdd)
    - [Unit Tests (State-Based)](#unit-tests-state-based)
    - [Integration Tests](#integration-tests)
  - [CLI Integration Pattern](#cli-integration-pattern)
    - [Before (CLI has domain logic)](#before-cli-has-domain-logic)
    - [After (CLI calls domain)](#after-cli-calls-domain)
  - [Web API Integration Example](#web-api-integration-example)
  - [Agent Integration Example](#agent-integration-example)
  - [Diagram: Architecture Layers](#diagram-architecture-layers)
  - [Performance SLOs](#performance-slos)
  - [Memory Storage Strategy](#memory-storage-strategy)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Domain Module Architecture - Marketplace, Template, Project

**Date**: 2025-11-20
**Status**: Design Phase
**System Architect**: Claude (Sonnet 4.5)

---

## Executive Summary

This document defines the architecture for extracting business logic from CLI layer (`ggen-cli`) into pure domain modules (`ggen-domain`). The goal is **zero clap dependencies** in domain logic, enabling reusability across CLI, web APIs, agents, and other interfaces.

### Key Principles

1. **CLI-Agnostic**: Domain modules have ZERO dependencies on clap/clap-noun-verb
2. **Async by Default**: All operations are async for non-blocking execution
3. **Serializable Outputs**: All output types derive `Serialize` for JSON support
4. **Result-Based**: All functions return `Result<T>` for proper error handling
5. **Type-Safe**: Use PhantomData and const generics where applicable
6. **Chicago TDD**: State-based testing with real collaborators

---

## Current Architecture Analysis

### Existing Module Organization (ggen-domain)

```
ggen-domain/src/
├── lib.rs                    # Module exports
├── ai/                       # AI operations (GOOD REFERENCE)
│   ├── mod.rs                # Clean module structure
│   ├── analyze.rs            # async fn analyze_code/project
│   └── generate.rs           # async fn generate_code
├── marketplace/              # NEEDS REFACTORING
│   ├── mod.rs                # 119 lines of re-exports
│   ├── adapter.rs            # Trait abstraction (GOOD)
│   ├── install.rs            # execute_install (GOOD)
│   ├── list.rs               # execute_list (GOOD)
│   ├── publish.rs            # execute_publish (GOOD)
│   ├── search.rs             # execute_search (GOOD)
│   └── ...30+ other files
├── template/                 # NEEDS REFACTORING
│   ├── mod.rs                # TemplateService + re-exports
│   ├── generate.rs           # execute_generate (GOOD)
│   ├── list.rs               # execute_list (GOOD)
│   └── ...
└── project/                  # MINIMAL - NEEDS EXPANSION
    ├── mod.rs                # Just re-exports
    ├── new.rs
    ├── gen.rs
    ├── plan.rs
    └── ...
```

### Best Practice Example: `ai/mod.rs`

```rust
//! AI domain layer - Pure business logic for AI operations
//!
//! ## Architecture
//! - **No CLI dependencies**: Pure domain logic
//! - **Async by default**: All operations are async
//! - **Error handling**: Uses `ggen_utils::error::Result`

pub mod analyze;
pub mod generate;

pub use analyze::*;
pub use generate::*;
```

**Why this is GOOD**:
- Clear documentation
- Minimal re-exports (only what's needed)
- Sub-modules are organized by operation type

---

## Proposed Architecture

### 1. Marketplace Domain Module

**Location**: `crates/ggen-domain/src/marketplace/`

#### Public API Functions

```rust
//! Marketplace domain layer - Pure business logic
//!
//! ## Operations
//! - search: Find packages in marketplace
//! - install: Install packages with dependencies
//! - publish: Publish packages to marketplace
//! - uninstall: Remove installed packages

/// Search for packages in marketplace
pub async fn search(query: SearchQuery) -> Result<SearchResults>

/// Install a package with dependencies
pub async fn install(input: InstallInput) -> Result<InstallResult>

/// Publish a package to marketplace
pub async fn publish(input: PublishInput) -> Result<PublishResult>

/// Uninstall a package
pub async fn uninstall(input: UninstallInput) -> Result<UninstallResult>

/// List installed packages
pub async fn list(input: ListInput) -> Result<ListOutput>

/// Validate package for production readiness
pub async fn validate(input: ValidateInput) -> Result<ValidationResult>
```

#### Type Definitions

```rust
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

// ============================================================================
// Search Types
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchQuery {
    pub query: String,
    pub limit: usize,
    pub category: Option<String>,
    pub min_score: Option<f64>,
}

impl Default for SearchQuery {
    fn default() -> Self {
        Self {
            query: String::new(),
            limit: 10,
            category: None,
            min_score: None,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchResults {
    pub packages: Vec<PackageInfo>,
    pub total: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageInfo {
    pub name: String,
    pub version: String,
    pub description: String,
    pub author: Option<String>,
    pub downloads: u32,
    pub stars: u32,
}

// ============================================================================
// Install Types
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstallInput {
    pub package: String,
    pub target: Option<PathBuf>,
    pub force: bool,
    pub no_dependencies: bool,
    pub dry_run: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstallResult {
    pub package_name: String,
    pub version: String,
    pub install_path: PathBuf,
    pub dependencies_installed: Vec<String>,
}

// ============================================================================
// Publish Types
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PublishInput {
    pub path: PathBuf,
    pub tag: Option<String>,
    pub dry_run: bool,
    pub force: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PublishResult {
    pub package_name: String,
    pub version: String,
    pub published_at: String,
}

// ============================================================================
// Uninstall Types
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UninstallInput {
    pub package: String,
    pub remove_dependencies: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UninstallResult {
    pub package_name: String,
    pub removed_dependencies: Vec<String>,
}

// ============================================================================
// Validation Types
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidateInput {
    pub package: Option<String>,
    pub packages_dir: Option<PathBuf>,
    pub update: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationResult {
    pub package: Option<String>,
    pub score: f64,
    pub production_ready: bool,
    pub total_packages: usize,
    pub ready_count: usize,
    pub needs_improvement_count: usize,
    pub not_ready_count: usize,
}

// ============================================================================
// Error Types
// ============================================================================

use thiserror::Error;

#[derive(Debug, Error, Serialize)]
pub enum MarketplaceError {
    #[error("Package not found: {0}")]
    PackageNotFound(String),

    #[error("Package already installed: {0}")]
    AlreadyInstalled(String),

    #[error("Invalid package metadata: {0}")]
    InvalidMetadata(String),

    #[error("Network error: {0}")]
    NetworkError(String),

    #[error("IO error: {0}")]
    IoError(String),
}
```

#### Module Organization

```
marketplace/
├── mod.rs              # Public API + re-exports
├── search.rs           # search() implementation
├── install.rs          # install() implementation
├── publish.rs          # publish() implementation
├── uninstall.rs        # uninstall() implementation
├── list.rs             # list() implementation
├── validate.rs         # validate() implementation
├── types.rs            # All type definitions
├── errors.rs           # MarketplaceError enum
└── registry.rs         # Internal registry logic
```

---

### 2. Template Domain Module

**Location**: `crates/ggen-domain/src/template/`

#### Public API Functions

```rust
//! Template domain layer - Pure business logic
//!
//! ## Operations
//! - generate: Render templates with context
//! - validate: Validate template syntax
//! - discover: Find available templates
//! - list: List all templates

/// Generate code from template
pub async fn generate(input: GenerateInput) -> Result<GenerateResult>

/// Validate template syntax
pub async fn validate(input: ValidateInput) -> Result<ValidationResult>

/// Discover templates in directories
pub async fn discover(input: DiscoverInput) -> Result<DiscoveryResult>

/// List all available templates
pub async fn list(input: ListInput) -> Result<ListOutput>

/// Show template metadata
pub async fn show(input: ShowInput) -> Result<ShowOutput>

/// Create new template
pub async fn create(input: CreateInput) -> Result<CreateResult>
```

#### Type Definitions

```rust
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::PathBuf;

// ============================================================================
// Generate Types
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerateInput {
    pub template: String,
    pub output_dir: PathBuf,
    pub vars: BTreeMap<String, String>,
    pub force: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerateResult {
    pub files_generated: Vec<PathBuf>,
    pub template_used: String,
    pub total_files: usize,
}

// ============================================================================
// Validate Types
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidateInput {
    pub template: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationResult {
    pub valid: bool,
    pub errors: Vec<ValidationError>,
    pub warnings: Vec<ValidationWarning>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationError {
    pub line: usize,
    pub column: usize,
    pub message: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationWarning {
    pub line: usize,
    pub message: String,
}

// ============================================================================
// Discover Types
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiscoverInput {
    pub search_dirs: Vec<PathBuf>,
    pub pattern: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiscoveryResult {
    pub templates_found: Vec<TemplateInfo>,
    pub total: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateInfo {
    pub name: String,
    pub path: PathBuf,
    pub source: TemplateSource,
    pub metadata: Option<TemplateMetadata>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TemplateSource {
    Local,
    Package(String),
    Remote(String),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateMetadata {
    pub description: Option<String>,
    pub author: Option<String>,
    pub version: Option<String>,
}

// ============================================================================
// List Types
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListInput {
    pub detailed: bool,
    pub filter: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListOutput {
    pub templates: Vec<TemplateInfo>,
    pub total: usize,
}

// ============================================================================
// Show Types
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ShowInput {
    pub template: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ShowOutput {
    pub name: String,
    pub path: PathBuf,
    pub content: String,
    pub metadata: TemplateMetadata,
}

// ============================================================================
// Create Types
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreateInput {
    pub name: String,
    pub content: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreateResult {
    pub template_name: String,
    pub path: PathBuf,
}

// ============================================================================
// Error Types
// ============================================================================

use thiserror::Error;

#[derive(Debug, Error, Serialize)]
pub enum TemplateError {
    #[error("Template not found: {0}")]
    NotFound(String),

    #[error("Invalid template syntax: {0}")]
    InvalidSyntax(String),

    #[error("Template already exists: {0}")]
    AlreadyExists(String),

    #[error("Rendering failed: {0}")]
    RenderingFailed(String),

    #[error("IO error: {0}")]
    IoError(String),
}
```

#### Module Organization

```
template/
├── mod.rs              # Public API + re-exports
├── generate.rs         # generate() implementation
├── validate.rs         # validate() implementation
├── discover.rs         # discover() implementation
├── list.rs             # list() implementation
├── show.rs             # show() implementation
├── create.rs           # create() implementation
├── types.rs            # All type definitions
├── errors.rs           # TemplateError enum
└── service.rs          # TemplateService (internal)
```

---

### 3. Project Domain Module

**Location**: `crates/ggen-domain/src/project/`

#### Public API Functions

```rust
//! Project domain layer - Pure business logic
//!
//! ## Operations
//! - create: Create new project from template
//! - configure: Configure project settings
//! - generate: Generate project files
//! - list: List project configurations

/// Create a new project from template
pub async fn create(input: CreateInput) -> Result<CreateResult>

/// Configure project settings
pub async fn configure(input: ConfigureInput) -> Result<ConfigureResult>

/// Generate project files from plan
pub async fn generate(input: GenerateInput) -> Result<GenerateResult>

/// List available project configurations
pub async fn list(input: ListInput) -> Result<ListOutput>

/// Initialize project structure
pub async fn init(input: InitInput) -> Result<InitResult>

/// Build project plan
pub async fn plan(input: PlanInput) -> Result<PlanResult>

/// Apply project changes
pub async fn apply(input: ApplyInput) -> Result<ApplyResult>
```

#### Type Definitions

```rust
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::PathBuf;

// ============================================================================
// Create Types
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreateInput {
    pub name: String,
    pub template: Option<String>,
    pub output_dir: PathBuf,
    pub vars: BTreeMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreateResult {
    pub project_name: String,
    pub project_path: PathBuf,
    pub files_created: Vec<PathBuf>,
}

// ============================================================================
// Configure Types
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConfigureInput {
    pub project_path: PathBuf,
    pub config: ProjectConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectConfig {
    pub name: String,
    pub version: String,
    pub description: Option<String>,
    pub author: Option<String>,
    pub settings: BTreeMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConfigureResult {
    pub project_path: PathBuf,
    pub config_updated: bool,
}

// ============================================================================
// Generate Types
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerateInput {
    pub project_path: PathBuf,
    pub plan: Option<GenerationPlan>,
    pub force: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerationPlan {
    pub files: Vec<FileSpec>,
    pub templates: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileSpec {
    pub path: PathBuf,
    pub template: String,
    pub vars: BTreeMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerateResult {
    pub project_path: PathBuf,
    pub files_generated: Vec<PathBuf>,
    pub total_files: usize,
}

// ============================================================================
// List Types
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListInput {
    pub search_dir: Option<PathBuf>,
    pub detailed: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListOutput {
    pub projects: Vec<ProjectInfo>,
    pub total: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectInfo {
    pub name: String,
    pub path: PathBuf,
    pub config: Option<ProjectConfig>,
}

// ============================================================================
// Init Types
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InitInput {
    pub path: PathBuf,
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InitResult {
    pub project_path: PathBuf,
    pub config_created: bool,
}

// ============================================================================
// Plan Types
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlanInput {
    pub project_path: PathBuf,
    pub templates: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlanResult {
    pub plan: GenerationPlan,
    pub estimated_files: usize,
}

// ============================================================================
// Apply Types
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApplyInput {
    pub project_path: PathBuf,
    pub plan: GenerationPlan,
    pub dry_run: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApplyResult {
    pub files_applied: Vec<PathBuf>,
    pub dry_run: bool,
}

// ============================================================================
// Error Types
// ============================================================================

use thiserror::Error;

#[derive(Debug, Error, Serialize)]
pub enum ProjectError {
    #[error("Project not found: {0}")]
    NotFound(String),

    #[error("Project already exists: {0}")]
    AlreadyExists(String),

    #[error("Invalid configuration: {0}")]
    InvalidConfig(String),

    #[error("Template error: {0}")]
    TemplateError(String),

    #[error("IO error: {0}")]
    IoError(String),
}
```

#### Module Organization

```
project/
├── mod.rs              # Public API + re-exports
├── create.rs           # create() implementation
├── configure.rs        # configure() implementation
├── generate.rs         # generate() implementation
├── list.rs             # list() implementation
├── init.rs             # init() implementation
├── plan.rs             # plan() implementation
├── apply.rs            # apply() implementation
├── types.rs            # All type definitions
└── errors.rs           # ProjectError enum
```

---

## Cross-Module Design

### Dependency Graph

```
┌────────────────┐
│  ggen-domain   │
└────────────────┘
        │
        ├─── marketplace/
        │    ├─ Depends on: ggen-core, ggen-marketplace, ggen-utils
        │    └─ Used by: CLI, Web API, Agents
        │
        ├─── template/
        │    ├─ Depends on: ggen-core, ggen-utils
        │    └─ Used by: CLI, Web API, Agents, marketplace, project
        │
        └─── project/
             ├─ Depends on: ggen-core, ggen-utils, template
             └─ Used by: CLI, Web API, Agents
```

**Dependency Rules**:
1. **marketplace** → Can use `template` for template-based operations
2. **project** → Can use `template` for project generation
3. **template** → Pure, no dependency on marketplace or project
4. All modules → Can use `ggen-utils::error::Result` for error handling

### Shared Types

**Location**: `crates/ggen-domain/src/common/`

```rust
//! Common types shared across domain modules

use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Metadata {
    pub description: Option<String>,
    pub author: Option<String>,
    pub version: Option<String>,
    pub tags: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileRef {
    pub path: PathBuf,
    pub content_type: String,
    pub size: u64,
}
```

### Error Handling Strategy

All modules use consistent error handling:

```rust
// Each module has its own error type
pub enum MarketplaceError { ... }
pub enum TemplateError { ... }
pub enum ProjectError { ... }

// All errors implement thiserror::Error
#[derive(Debug, Error, Serialize)]

// All functions return Result<T>
pub async fn search(query: SearchQuery) -> Result<SearchResults, MarketplaceError>

// CLI layer converts to clap_noun_verb::NounVerbError
// Web layer converts to HTTP status codes
```

---

## API Design Principles

### 1. All Functions Are Async

```rust
// ✅ CORRECT
pub async fn search(query: SearchQuery) -> Result<SearchResults>

// ❌ WRONG
pub fn search(query: SearchQuery) -> Result<SearchResults>
```

### 2. All Output Types Derive Serialize

```rust
// ✅ CORRECT
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchResults {
    pub packages: Vec<PackageInfo>,
    pub total: usize,
}

// ❌ WRONG
pub struct SearchResults {  // No Serialize
    pub packages: Vec<PackageInfo>,
    pub total: usize,
}
```

### 3. All Functions Return Result<T>

```rust
// ✅ CORRECT
pub async fn install(input: InstallInput) -> Result<InstallResult, MarketplaceError>

// ❌ WRONG
pub async fn install(input: InstallInput) -> InstallResult
```

### 4. No CLI Dependencies

```rust
// ✅ CORRECT - Pure domain logic
use serde::{Deserialize, Serialize};
use ggen_utils::error::Result;

// ❌ WRONG - CLI dependency
use clap::Parser;
use clap_noun_verb::Result;
```

---

## Implementation Checklist

### Marketplace Module
- [ ] Define all input/output types in `types.rs`
- [ ] Define `MarketplaceError` in `errors.rs`
- [ ] Implement `search()` in `search.rs`
- [ ] Implement `install()` in `install.rs`
- [ ] Implement `publish()` in `publish.rs`
- [ ] Implement `uninstall()` in `uninstall.rs`
- [ ] Implement `list()` in `list.rs`
- [ ] Implement `validate()` in `validate.rs`
- [ ] Update `mod.rs` with clean re-exports
- [ ] Create Chicago TDD tests for each function
- [ ] Verify ZERO clap dependencies

### Template Module
- [ ] Define all input/output types in `types.rs`
- [ ] Define `TemplateError` in `errors.rs`
- [ ] Implement `generate()` in `generate.rs`
- [ ] Implement `validate()` in `validate.rs`
- [ ] Implement `discover()` in `discover.rs`
- [ ] Implement `list()` in `list.rs`
- [ ] Implement `show()` in `show.rs`
- [ ] Implement `create()` in `create.rs`
- [ ] Update `mod.rs` with clean re-exports
- [ ] Create Chicago TDD tests for each function
- [ ] Verify ZERO clap dependencies

### Project Module
- [ ] Define all input/output types in `types.rs`
- [ ] Define `ProjectError` in `errors.rs`
- [ ] Implement `create()` in `create.rs`
- [ ] Implement `configure()` in `configure.rs`
- [ ] Implement `generate()` in `generate.rs`
- [ ] Implement `list()` in `list.rs`
- [ ] Implement `init()` in `init.rs`
- [ ] Implement `plan()` in `plan.rs`
- [ ] Implement `apply()` in `apply.rs`
- [ ] Update `mod.rs` with clean re-exports
- [ ] Create Chicago TDD tests for each function
- [ ] Verify ZERO clap dependencies

---

## Testing Strategy (Chicago TDD)

### Unit Tests (State-Based)

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_search_returns_results() {
        // ARRANGE
        let query = SearchQuery {
            query: "rust".to_string(),
            limit: 5,
            category: None,
            min_score: None,
        };

        // ACT
        let results = search(query).await.unwrap();

        // ASSERT - Verify observable outputs
        assert!(results.total > 0);
        assert!(results.packages.len() <= 5);
        assert!(results.packages.iter().all(|p| p.name.contains("rust")));
    }
}
```

### Integration Tests

```rust
#[tokio::test]
async fn test_install_creates_files() {
    // ARRANGE
    let temp_dir = TempDir::new().unwrap();
    let input = InstallInput {
        package: "io.ggen.test".to_string(),
        target: Some(temp_dir.path().to_path_buf()),
        force: false,
        no_dependencies: false,
        dry_run: false,
    };

    // ACT
    let result = install(input).await.unwrap();

    // ASSERT - Verify state changes
    assert!(result.install_path.exists());
    assert_eq!(result.package_name, "io.ggen.test");
    assert!(result.dependencies_installed.len() >= 0);
}
```

---

## CLI Integration Pattern

### Before (CLI has domain logic)

```rust
// marketplace.rs - CLI layer
#[verb]
fn search(query: String, limit: Option<usize>) -> Result<SearchOutput> {
    // ❌ Domain logic mixed with CLI
    let packages = registry.search(&query)?;
    let filtered = packages.into_iter().take(limit.unwrap_or(10)).collect();
    Ok(SearchOutput { packages: filtered })
}
```

### After (CLI calls domain)

```rust
// marketplace.rs - CLI layer
#[verb]
fn search(query: String, limit: Option<usize>) -> Result<SearchOutput> {
    let input = SearchQuery {
        query,
        limit: limit.unwrap_or(10),
        category: None,
        min_score: None,
    };

    execute_async_verb(async move {
        let results = ggen_domain::marketplace::search(input)
            .await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

        Ok(SearchOutput {
            packages: results.packages,
            total: results.total,
        })
    })
}
```

---

## Web API Integration Example

```rust
// web/api/marketplace.rs
use axum::{Json, extract::Query};
use ggen_domain::marketplace::{search, SearchQuery};

async fn search_endpoint(
    Query(params): Query<SearchQuery>
) -> Result<Json<SearchResults>, StatusCode> {
    search(params)
        .await
        .map(Json)
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)
}
```

---

## Agent Integration Example

```rust
// agents/marketplace_agent.rs
use ggen_domain::marketplace::{search, SearchQuery};

pub async fn find_best_packages(use_case: &str) -> Vec<PackageInfo> {
    let query = SearchQuery {
        query: use_case.to_string(),
        limit: 10,
        category: None,
        min_score: Some(80.0),
    };

    search(query)
        .await
        .ok()
        .map(|r| r.packages)
        .unwrap_or_default()
}
```

---

## Diagram: Architecture Layers

```
┌─────────────────────────────────────────────────────────┐
│                     INTERFACE LAYER                      │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐│
│  │   CLI    │  │ Web API  │  │  Agents  │  │  Python  ││
│  │  (clap)  │  │  (axum)  │  │ (swarm)  │  │ Bindings ││
│  └──────────┘  └──────────┘  └──────────┘  └──────────┘│
└────────────────────────┬────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────┐
│                    DOMAIN LAYER                          │
│  ┌────────────────┐  ┌────────────────┐  ┌────────────┐│
│  │  marketplace   │  │   template     │  │  project   ││
│  │  - search()    │  │  - generate()  │  │  - create()││
│  │  - install()   │  │  - validate()  │  │  - config()││
│  │  - publish()   │  │  - discover()  │  │  - gen()   ││
│  └────────────────┘  └────────────────┘  └────────────┘│
│                                                          │
│  All functions:                                          │
│  ✓ async                                                │
│  ✓ Result<T>                                            │
│  ✓ Serialize outputs                                    │
│  ✓ ZERO clap deps                                       │
└────────────────────────┬────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────┐
│               INFRASTRUCTURE LAYER                       │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐              │
│  │ggen-core │  │ggen-utils│  │ggen-ai   │              │
│  │ (engine) │  │ (errors) │  │ (LLM)    │              │
│  └──────────┘  └──────────┘  └──────────┘              │
└─────────────────────────────────────────────────────────┘
```

---

## Performance SLOs

All domain operations must meet:

- **Search**: ≤ 100ms for in-memory, ≤ 500ms for disk
- **Install**: ≤ 3s for small packages (< 10MB)
- **Generate**: ≤ 2s for templates with < 100 files
- **Validate**: ≤ 500ms for single package
- **List**: ≤ 200ms for < 1000 items

---

## Memory Storage Strategy

Store design decisions in Claude Flow memory:

```bash
npx claude-flow@alpha hooks post-task \
  --memory-key "swarm/architecture/modules" \
  --description "Marketplace, Template, Project domain module design"

npx claude-flow@alpha hooks post-task \
  --memory-key "swarm/architecture/dependencies" \
  --description "Cross-module dependency graph and shared types"

npx claude-flow@alpha hooks post-task \
  --memory-key "swarm/architecture/api-patterns" \
  --description "Async, Serialize, Result<T> API design patterns"
```

---

## Next Steps

1. **Review and Approval**: Review this architecture design
2. **Implementation**: Begin implementing modules following checklist
3. **Testing**: Create Chicago TDD tests for each function
4. **CLI Migration**: Update CLI layer to call domain functions
5. **Validation**: Run `cargo make check`, `cargo make test`, `cargo make lint`

---

**Architecture Design Complete** ✅
**Ready for Implementation** ⚡
