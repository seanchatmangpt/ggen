<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Type-First Architecture Design: ggen.toml Configuration System](#type-first-architecture-design-ggentoml-configuration-system)
  - [Executive Summary](#executive-summary)
  - [Current State Analysis](#current-state-analysis)
    - [1. project_config.rs (Minimal Implementation)](#1-project_configrs-minimal-implementation)
    - [2. lockfile.rs (Pack Tracking - gpacks)](#2-lockfilers-pack-tracking---gpacks)
    - [3. lock_manager.rs (Ontology Locking)](#3-lock_managerrs-ontology-locking)
    - [4. ggen-toml-schema.toml (Complete Schema Specification)](#4-ggen-toml-schematoml-complete-schema-specification)
  - [Type-First Architecture Design](#type-first-architecture-design)
    - [Core Type System: Configuration Lifecycle State Machine](#core-type-system-configuration-lifecycle-state-machine)
    - [1. Project Section](#1-project-section)
    - [2. Workspace Section (Zero-Cost Abstraction)](#2-workspace-section-zero-cost-abstraction)
    - [3. Graph Section (Graph-Based Dependency Resolution)](#3-graph-section-graph-based-dependency-resolution)
    - [4. Dependencies Section](#4-dependencies-section)
    - [5. Ontology Section (RDF/OWL Integration)](#5-ontology-section-rdfowl-integration)
    - [6. Templates Section (Template Composition & Inheritance)](#6-templates-section-template-composition--inheritance)
    - [7. Generators Section (Code Generation Pipelines)](#7-generators-section-code-generation-pipelines)
    - [8. Lifecycle Section (Build Lifecycle & Hooks)](#8-lifecycle-section-build-lifecycle--hooks)
    - [9. Plugins Section (Plugin System)](#9-plugins-section-plugin-system)
    - [10. Profiles Section (Environment-Specific Overrides)](#10-profiles-section-environment-specific-overrides)
  - [Dual Lockfile Integration Strategy](#dual-lockfile-integration-strategy)
    - [Problem Statement](#problem-statement)
    - [Integration Design](#integration-design)
  - [Type-Level Invariants](#type-level-invariants)
    - [1. Configuration Lifecycle Invariants](#1-configuration-lifecycle-invariants)
    - [2. Semantic Versioning Invariants](#2-semantic-versioning-invariants)
    - [3. Dependency Source Invariants](#3-dependency-source-invariants)
    - [4. Profile Selection Invariants](#4-profile-selection-invariants)
  - [Const Generic Patterns](#const-generic-patterns)
    - [1. Compile-Time Strategy Selection](#1-compile-time-strategy-selection)
    - [2. Compile-Time Validation Limits](#2-compile-time-validation-limits)
  - [Result<T, E> Error Handling](#resultt-e-error-handling)
  - [Ownership Semantics & Lifetime Annotations](#ownership-semantics--lifetime-annotations)
    - [1. Configuration Ownership](#1-configuration-ownership)
    - [2. Lockfile Borrowing](#2-lockfile-borrowing)
    - [3. Template Resolution (Zero-Copy Where Possible)](#3-template-resolution-zero-copy-where-possible)
    - [4. Dependency Candidate Ownership](#4-dependency-candidate-ownership)
  - [Memory Storage Strategy](#memory-storage-strategy)
  - [Implementation Priorities (80/20 Rule)](#implementation-priorities-8020-rule)
    - [Phase 1: Critical 20% (Core Type System)](#phase-1-critical-20-core-type-system)
    - [Phase 2: High Value 30% (Graph & Templates)](#phase-2-high-value-30-graph--templates)
    - [Phase 3: Feature Completeness 30% (Workspace, Lifecycle, Profiles)](#phase-3-feature-completeness-30-workspace-lifecycle-profiles)
    - [Phase 4: Advanced 20% (Generators, Plugins)](#phase-4-advanced-20-generators-plugins)
  - [Conclusion](#conclusion)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Type-First Architecture Design: ggen.toml Configuration System

**Architect**: System Architect (Hive Mind)
**Date**: 2025-11-20
**Version**: 4.0.0
**Design Philosophy**: Type-first thinking, zero-cost abstractions, PhantomData state machines

---

## Executive Summary

This document presents a comprehensive type-first architecture for expanding the ggen.toml configuration system from its current minimal implementation (Project, RdfConfig, vars) to the full schema specification covering workspaces, graph-based dependencies, ontologies, templates, generators, lifecycle management, plugins, and profiles.

**Key Design Principles:**
1. **Type-level state machines** using PhantomData for configuration lifecycle (Unvalidated → Validated → Compiled → Locked)
2. **Zero-cost abstractions** - All configuration types monomorphize at compile time
3. **Invalid states unrepresentable** - Type system enforces invariants
4. **Const generics** for compile-time validation where possible
5. **Result<T, E>** for all fallible operations
6. **Dual lockfile integration** - Unified strategy for gpacks (lockfile.rs) and ontologies (lock_manager.rs)

---

## Current State Analysis

### 1. project_config.rs (Minimal Implementation)
```rust
pub struct GgenConfig {
    pub project: Project,
    pub prefixes: BTreeMap<String, String>,
    pub rdf: RdfConfig,
    pub vars: BTreeMap<String, String>,
}
```
**Limitations:**
- Only 4 fields (project, prefixes, rdf, vars)
- No workspace support
- No graph-based dependency resolution
- No template composition
- No generator pipelines
- No lifecycle hooks
- No plugin system
- No profile management
- No validation state tracking

### 2. lockfile.rs (Pack Tracking - gpacks)
```rust
pub struct PackLockfile {
    pub packs: BTreeMap<String, LockedPack>,
    pub updated_at: DateTime<Utc>,
    pub ggen_version: String,
}

pub struct LockedPack {
    pub version: String,
    pub source: PackSource,  // Registry | GitHub | Local
    pub integrity: Option<String>,
    pub installed_at: DateTime<Utc>,
    pub dependencies: Vec<String>,
}
```
**Features:**
- Tracks installed packs with versions
- Multiple sources (Registry, GitHub, Local)
- Integrity checksums (SHA256)
- Dependency management
- Circular dependency detection
- Validation before save

### 3. lock_manager.rs (Ontology Locking)
```rust
pub struct OntologyLockfile {
    pub version: u32,
    pub generated_at: String,
    pub generator_version: String,
    pub packages: BTreeMap<String, LockedPackage>,
    pub composition: CompositionMetadata,
    pub hashes: BTreeMap<String, String>,
}

pub struct LockedPackage {
    pub version: String,
    pub resolved: String,
    pub integrity: String,
    pub location: String,
    pub namespace: Option<String>,
    pub classes_count: usize,
    pub properties_count: usize,
    pub dependencies: BTreeMap<String, String>,
    pub installed_at: String,
}
```
**Features:**
- Ontology-specific locking
- Composition metadata (strategy, total classes/properties, conflicts resolved)
- Namespace extraction
- RDF class/property counting
- Validation status tracking

### 4. ggen-toml-schema.toml (Complete Schema Specification)
**Sections:**
- `[project]` - Core metadata, graph identity, template inheritance
- `[workspace]` - Mono-repo configuration, shared dependencies
- `[graph]` - Graph-based dependency resolution, conflict resolution, feature flags
- `[dependencies]` - Graph-based dependency specification
- `[ontology]` - RDF/OWL integration, SHACL shapes, constitution (invariants)
- `[templates]` - Template composition, inheritance, guards, SPARQL-driven selection
- `[generators]` - Code generation pipelines, multi-step workflows, hooks
- `[lifecycle]` - Build lifecycle, phases, hooks, tasks (cargo-make style)
- `[plugins]` - Plugin system, discovery, permissions, sandboxing
- `[profiles]` - Environment-specific overrides (dev, prod, test, ci, bench)

---

## Type-First Architecture Design

### Core Type System: Configuration Lifecycle State Machine

**Problem:** Configuration goes through multiple states (parsed → validated → compiled → locked), but current implementation doesn't track state.

**Solution:** PhantomData-based type-level state machine.

```rust
use std::marker::PhantomData;
use std::path::PathBuf;
use std::collections::BTreeMap;

// Type-level state markers (zero-cost)
pub struct Unvalidated;
pub struct Validated;
pub struct Compiled;
pub struct Locked;

/// Type-safe configuration with lifecycle state tracking
///
/// Generic parameter S tracks validation state at compile time:
/// - Unvalidated: Raw parsed from TOML, no guarantees
/// - Validated: Schema validated, invariants checked
/// - Compiled: Templates compiled, dependencies resolved
/// - Locked: Lockfile generated, reproducible build ready
pub struct GgenConfig<S> {
    // Core configuration data
    pub project: Project,
    pub workspace: Option<Workspace>,
    pub graph: Option<Graph>,
    pub dependencies: Dependencies,
    pub ontology: Ontology,
    pub templates: Templates,
    pub generators: Generators,
    pub lifecycle: Lifecycle,
    pub plugins: Plugins,
    pub profiles: Profiles,

    // Metadata
    pub prefixes: BTreeMap<String, String>,
    pub vars: BTreeMap<String, String>,

    // State marker (zero-cost - erased at compile time)
    _state: PhantomData<S>,
}

// Type state transitions (invalid states prevented by type system)
impl GgenConfig<Unvalidated> {
    /// Parse from TOML string (unvalidated)
    pub fn from_toml(content: &str) -> Result<Self, ConfigError> {
        // ... deserialization logic ...
        Ok(GgenConfig {
            // ... fields ...
            _state: PhantomData,
        })
    }

    /// Validate configuration and transition to Validated state
    pub fn validate(self) -> Result<GgenConfig<Validated>, ConfigError> {
        // Run all validation checks
        self.validate_project()?;
        self.validate_workspace()?;
        self.validate_graph()?;
        self.validate_ontology_constitution()?;
        self.validate_dependencies()?;
        self.validate_templates()?;

        Ok(GgenConfig {
            project: self.project,
            workspace: self.workspace,
            graph: self.graph,
            dependencies: self.dependencies,
            ontology: self.ontology,
            templates: self.templates,
            generators: self.generators,
            lifecycle: self.lifecycle,
            plugins: self.plugins,
            profiles: self.profiles,
            prefixes: self.prefixes,
            vars: self.vars,
            _state: PhantomData,
        })
    }
}

impl GgenConfig<Validated> {
    /// Compile configuration (resolve templates, dependencies)
    pub fn compile(self) -> Result<GgenConfig<Compiled>, ConfigError> {
        // Resolve template inheritance chains
        // Resolve graph-based dependencies
        // Execute SPARQL queries for template selection
        // Validate plugin permissions

        Ok(GgenConfig {
            project: self.project,
            workspace: self.workspace,
            graph: self.graph,
            dependencies: self.dependencies,
            ontology: self.ontology,
            templates: self.templates,
            generators: self.generators,
            lifecycle: self.lifecycle,
            plugins: self.plugins,
            profiles: self.profiles,
            prefixes: self.prefixes,
            vars: self.vars,
            _state: PhantomData,
        })
    }
}

impl GgenConfig<Compiled> {
    /// Generate lockfiles (dual lock strategy)
    pub fn lock(self) -> Result<GgenConfig<Locked>, ConfigError> {
        // Generate pack lockfile (lockfile.rs)
        // Generate ontology lockfile (lock_manager.rs)
        // Verify integrity checksums

        Ok(GgenConfig {
            project: self.project,
            workspace: self.workspace,
            graph: self.graph,
            dependencies: self.dependencies,
            ontology: self.ontology,
            templates: self.templates,
            generators: self.generators,
            lifecycle: self.lifecycle,
            plugins: self.plugins,
            profiles: self.profiles,
            prefixes: self.prefixes,
            vars: self.vars,
            _state: PhantomData,
        })
    }

    /// Access validated configuration (only Compiled and Locked states)
    pub fn as_validated(&self) -> &Self {
        self
    }
}

impl GgenConfig<Locked> {
    /// Load from lockfile for reproducible build
    pub fn from_lockfile(path: &Path) -> Result<Self, ConfigError> {
        // Load pack lockfile
        // Load ontology lockfile
        // Verify integrity
        // Reconstruct configuration

        Ok(GgenConfig {
            // ... fields ...
            _state: PhantomData,
        })
    }

    /// Verify lockfile integrity
    pub fn verify_integrity(&self) -> Result<(), ConfigError> {
        // Verify pack lockfile hashes
        // Verify ontology lockfile hashes
        Ok(())
    }
}

// Error type
#[derive(Debug, thiserror::Error)]
pub enum ConfigError {
    #[error("Validation error: {0}")]
    Validation(String),

    #[error("Compilation error: {0}")]
    Compilation(String),

    #[error("Lockfile error: {0}")]
    Lockfile(String),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("TOML parse error: {0}")]
    TomlParse(#[from] toml::de::Error),
}
```

**Benefits:**
- **Type safety**: Cannot call `.lock()` on unvalidated config (compile error)
- **Zero-cost**: PhantomData erased at compile time, no runtime overhead
- **Explicit state**: State transitions visible in type signatures
- **Impossible states**: Cannot have validated but uncompiled config in wrong places

---

### 1. Project Section

```rust
use std::path::PathBuf;

/// Project metadata and configuration
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct Project {
    // Required fields
    pub name: String,
    pub version: semver::Version,  // Use semver crate for type-safe versioning

    // Optional metadata
    pub description: Option<String>,
    pub authors: Vec<String>,
    pub license: Option<String>,
    pub edition: Option<String>,

    // Smart detection (auto-infer from project structure)
    #[serde(default = "ProjectType::auto")]
    pub r#type: ProjectType,

    #[serde(default = "Language::auto")]
    pub language: Language,

    // Graph identity - RDF URI for this project
    pub uri: Option<String>,  // RDF URI
    pub namespace: Option<String>,  // Short prefix for RDF queries

    // Template inheritance - inherit defaults from base template
    pub extends: Option<String>,  // e.g., "ggen:rust-cli"

    // Output directory
    pub output_dir: PathBuf,
}

/// Project type enumeration
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub enum ProjectType {
    Auto,
    Library,
    Binary,
    Workspace,
}

impl ProjectType {
    const fn auto() -> Self {
        Self::Auto
    }
}

/// Language enumeration
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Language {
    Auto,
    Rust,
    TypeScript,
    Python,
    Multi,
}

impl Language {
    const fn auto() -> Self {
        Self::Auto
    }
}
```

**Type-level invariants:**
- `semver::Version` ensures semantic versioning compliance (compile-time validation)
- Enums for `ProjectType` and `Language` prevent invalid string values
- `const fn` defaults for zero-cost default values

---

### 2. Workspace Section (Zero-Cost Abstraction)

```rust
use std::path::PathBuf;
use std::collections::BTreeMap;

/// Workspace configuration for mono-repos
///
/// Zero-cost: All fields are Option or Vec, no runtime overhead if unused
#[derive(Debug, Clone, Default, serde::Deserialize, serde::Serialize)]
pub struct Workspace {
    /// Workspace members (glob patterns supported)
    #[serde(default)]
    pub members: Vec<String>,  // e.g., ["crates/*", "packages/*"]

    /// Exclude from workspace (glob patterns)
    #[serde(default)]
    pub exclude: Vec<String>,  // e.g., ["target", "node_modules"]

    /// Shared dependencies across workspace (DRY principle)
    #[serde(default)]
    pub dependencies: BTreeMap<String, DependencySpec>,

    /// Workspace-level graph queries
    pub graph: Option<WorkspaceGraph>,
}

/// Workspace-level graph queries (SPARQL)
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct WorkspaceGraph {
    /// SPARQL query for workspace analysis
    /// Example: Find all workspace members and their dependencies
    pub query: String,
}

/// Dependency specification (generic across dependencies, workspace.dependencies, profiles)
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
#[serde(untagged)]
pub enum DependencySpec {
    /// Simple version string
    Simple(String),  // "1.0"

    /// Detailed dependency specification
    Detailed {
        version: String,

        #[serde(skip_serializing_if = "Option::is_none")]
        features: Option<Vec<String>>,

        #[serde(skip_serializing_if = "Option::is_none")]
        optional: Option<bool>,

        #[serde(skip_serializing_if = "Option::is_none")]
        default_features: Option<bool>,

        /// Git source
        #[serde(skip_serializing_if = "Option::is_none")]
        git: Option<String>,

        /// Git branch
        #[serde(skip_serializing_if = "Option::is_none")]
        branch: Option<String>,

        /// Local path
        #[serde(skip_serializing_if = "Option::is_none")]
        path: Option<PathBuf>,

        /// Registry URL
        #[serde(skip_serializing_if = "Option::is_none")]
        registry: Option<String>,
    },
}
```

**Zero-cost characteristics:**
- `Option<T>` has same memory layout as `T` when Some (no overhead)
- `Vec<T>` only allocates when non-empty
- `BTreeMap` only allocates when non-empty
- All fields `#[serde(default)]` or `Option` - no allocation if not in TOML

---

### 3. Graph Section (Graph-Based Dependency Resolution)

```rust
use std::collections::BTreeMap;

/// Graph-based dependency resolution configuration
///
/// Zero-cost: Uses const generics for strategy selection (compile-time dispatch)
#[derive(Debug, Clone, Default, serde::Deserialize, serde::Serialize)]
pub struct Graph {
    /// Resolution strategy
    #[serde(default)]
    pub strategy: ResolutionStrategy,

    /// Conflict resolution policy
    #[serde(default)]
    pub conflict_resolution: ConflictPolicy,

    /// Graph-based queries for dependencies (SPARQL)
    #[serde(default)]
    pub queries: BTreeMap<String, String>,

    /// Feature flags as graph nodes
    #[serde(default)]
    pub features: BTreeMap<String, Vec<String>>,
}

/// Resolution strategy (const dispatch at compile time)
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub enum ResolutionStrategy {
    #[default]
    Smart,        // Intelligent resolution with heuristics
    Conservative, // Prefer older, stable versions
    Aggressive,   // Prefer newer versions
}

/// Conflict resolution policy (const dispatch)
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub enum ConflictPolicy {
    #[default]
    Newest,           // Choose newest compatible version
    Oldest,           // Choose oldest compatible version
    SemverCompatible, // Ensure semver compatibility
}

// Trait for compile-time dispatch based on strategy
pub trait ResolveStrategy {
    fn resolve(&self, candidates: &[DependencyCandidate]) -> Result<DependencyCandidate, GraphError>;
}

impl ResolveStrategy for ResolutionStrategy {
    fn resolve(&self, candidates: &[DependencyCandidate]) -> Result<DependencyCandidate, GraphError> {
        match self {
            Self::Smart => Self::resolve_smart(candidates),
            Self::Conservative => Self::resolve_conservative(candidates),
            Self::Aggressive => Self::resolve_aggressive(candidates),
        }
    }
}

impl ResolutionStrategy {
    // These monomorphize to separate functions (zero-cost dispatch)
    fn resolve_smart(candidates: &[DependencyCandidate]) -> Result<DependencyCandidate, GraphError> {
        // Smart resolution logic
        todo!()
    }

    fn resolve_conservative(candidates: &[DependencyCandidate]) -> Result<DependencyCandidate, GraphError> {
        // Conservative resolution logic
        todo!()
    }

    fn resolve_aggressive(candidates: &[DependencyCandidate]) -> Result<DependencyCandidate, GraphError> {
        // Aggressive resolution logic
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct DependencyCandidate {
    pub name: String,
    pub version: semver::Version,
    pub source: String,
}

#[derive(Debug, thiserror::Error)]
pub enum GraphError {
    #[error("No suitable dependency found: {0}")]
    NoCandidate(String),

    #[error("Dependency conflict: {0}")]
    Conflict(String),
}
```

**Zero-cost dispatch:**
- `match` on enum monomorphizes to direct function calls
- No vtable overhead (unlike trait objects)
- Compiler optimizes away the match entirely

---

### 4. Dependencies Section

```rust
use std::collections::BTreeMap;

/// Dependency configuration
///
/// Reuses DependencySpec from workspace section (zero-cost abstraction)
#[derive(Debug, Clone, Default, serde::Deserialize, serde::Serialize)]
pub struct Dependencies {
    #[serde(default)]
    pub dependencies: BTreeMap<String, DependencySpec>,

    #[serde(default, rename = "dev-dependencies")]
    pub dev_dependencies: BTreeMap<String, DependencySpec>,

    #[serde(default, rename = "build-dependencies")]
    pub build_dependencies: BTreeMap<String, DependencySpec>,

    /// Conditional dependencies (platform-specific)
    #[serde(default)]
    pub target: BTreeMap<String, TargetDependencies>,
}

/// Target-specific dependencies
#[derive(Debug, Clone, Default, serde::Deserialize, serde::Serialize)]
pub struct TargetDependencies {
    #[serde(default)]
    pub dependencies: BTreeMap<String, DependencySpec>,
}
```

**Type reuse:**
- `DependencySpec` defined once, reused everywhere (DRY)
- `BTreeMap` ensures deterministic ordering (reproducible builds)
- `#[serde(default)]` ensures no allocation if section missing

---

### 5. Ontology Section (RDF/OWL Integration)

```rust
use std::path::PathBuf;
use std::collections::BTreeMap;

/// Ontology configuration for RDF/OWL integration
///
/// Zero-cost: Optional fields, lazy validation
#[derive(Debug, Clone, Default, serde::Deserialize, serde::Serialize)]
pub struct Ontology {
    /// Base ontology files (Turtle format)
    #[serde(default)]
    pub files: Vec<PathBuf>,

    /// Inline RDF (Turtle format) - embedded in config
    pub inline: Option<String>,

    /// SHACL shapes for validation
    #[serde(default)]
    pub shapes: Vec<PathBuf>,

    /// Constitution (invariant checks)
    pub constitution: Option<Constitution>,
}

/// Constitution - invariant checking system
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct Constitution {
    /// Built-in checks (compile-time list)
    #[serde(default)]
    pub checks: Vec<BuiltinCheck>,

    /// Custom invariants (SPARQL ASK queries)
    #[serde(default)]
    pub custom: BTreeMap<String, String>,
}

/// Built-in constitution checks (const dispatch)
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
pub enum BuiltinCheck {
    NoRetrocausation,   // Time consistency
    TypeSoundness,      // Type safety
    GuardSoundness,     // Guard validity
}

impl BuiltinCheck {
    /// Execute check (monomorphizes to separate functions)
    pub const fn execute(&self) -> fn() -> Result<(), ConstitutionError> {
        match self {
            Self::NoRetrocausation => Self::check_no_retrocausation,
            Self::TypeSoundness => Self::check_type_soundness,
            Self::GuardSoundness => Self::check_guard_soundness,
        }
    }

    const fn check_no_retrocausation() -> Result<(), ConstitutionError> {
        // Check for time consistency violations
        todo!()
    }

    const fn check_type_soundness() -> Result<(), ConstitutionError> {
        // Check for type safety violations
        todo!()
    }

    const fn check_guard_soundness() -> Result<(), ConstitutionError> {
        // Check for guard validity
        todo!()
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ConstitutionError {
    #[error("Invariant violation: {0}")]
    Violation(String),
}
```

**Const dispatch:**
- `const fn execute()` returns function pointer (zero-cost)
- Compiler inlines all checks
- No runtime dispatch overhead

---

### 6. Templates Section (Template Composition & Inheritance)

```rust
use std::path::PathBuf;
use std::collections::BTreeMap;

/// Template composition and inheritance configuration
///
/// Zero-cost: String keys, optional composition
#[derive(Debug, Clone, Default, serde::Deserialize, serde::Serialize)]
pub struct Templates {
    /// Template search paths (checked in order)
    #[serde(default)]
    pub paths: Vec<PathBuf>,

    /// Default template variables
    #[serde(default)]
    pub vars: BTreeMap<String, String>,

    /// Template inheritance chain (base template → customization)
    #[serde(default)]
    pub extends: BTreeMap<String, String>,

    /// Template composition (merge multiple templates)
    #[serde(default)]
    pub compose: BTreeMap<String, Vec<String>>,

    /// Template guards (conditional rendering)
    #[serde(default)]
    pub guards: BTreeMap<String, String>,

    /// SPARQL-driven template selection
    #[serde(default)]
    pub queries: BTreeMap<String, String>,
}

// Template resolution at compile-time (const generics)
pub struct TemplateResolver<const STRATEGY: u8>;

impl<const STRATEGY: u8> TemplateResolver<STRATEGY> {
    pub fn resolve(&self, templates: &Templates) -> Result<Vec<ResolvedTemplate>, TemplateError> {
        // Strategy selected at compile time via const generic
        match STRATEGY {
            0 => self.resolve_inheritance(templates),
            1 => self.resolve_composition(templates),
            2 => self.resolve_sparql(templates),
            _ => unreachable!(),
        }
    }

    fn resolve_inheritance(&self, templates: &Templates) -> Result<Vec<ResolvedTemplate>, TemplateError> {
        todo!()
    }

    fn resolve_composition(&self, templates: &Templates) -> Result<Vec<ResolvedTemplate>, TemplateError> {
        todo!()
    }

    fn resolve_sparql(&self, templates: &Templates) -> Result<Vec<ResolvedTemplate>, TemplateError> {
        todo!()
    }
}

pub struct ResolvedTemplate {
    pub target_file: PathBuf,
    pub content: String,
}

#[derive(Debug, thiserror::Error)]
pub enum TemplateError {
    #[error("Template not found: {0}")]
    NotFound(String),

    #[error("Template composition error: {0}")]
    Composition(String),
}
```

**Const generic strategy:**
- Template resolution strategy selected at compile time
- Each strategy monomorphizes to separate code path
- Zero runtime dispatch overhead

---

### 7. Generators Section (Code Generation Pipelines)

```rust
use std::collections::BTreeMap;

/// Code generation pipeline configuration
#[derive(Debug, Clone, Default, serde::Deserialize, serde::Serialize)]
pub struct Generators {
    /// Generator registry URL
    pub registry: Option<String>,

    /// Installed generators
    #[serde(default)]
    pub installed: BTreeMap<String, GeneratorSpec>,

    /// Generator pipelines (multi-step workflows)
    #[serde(default)]
    pub pipeline: Vec<Pipeline>,

    /// Generator hooks (lifecycle integration)
    pub hooks: Option<GeneratorHooks>,
}

/// Generator specification
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct GeneratorSpec {
    pub version: String,
    pub registry: Option<String>,
}

/// Generator pipeline (multi-step workflow)
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct Pipeline {
    pub name: String,
    pub description: Option<String>,

    #[serde(default)]
    pub inputs: Vec<String>,

    pub steps: Vec<PipelineStep>,
}

/// Pipeline step (action in workflow)
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
#[serde(tag = "action", rename_all = "lowercase")]
pub enum PipelineStep {
    Template {
        template: String,
    },
    Parse {
        parser: String,
    },
    Transform {
        query: String,
    },
    Exec {
        command: String,
    },
}

/// Generator lifecycle hooks
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct GeneratorHooks {
    pub before_generate: Option<String>,
    pub after_generate: Option<String>,
    pub on_error: Option<String>,
}
```

**Type-safe pipelines:**
- Enum for step types prevents invalid steps
- Compile-time validation of step structure
- Zero-cost serialization (serde derives)

---

### 8. Lifecycle Section (Build Lifecycle & Hooks)

```rust
use std::collections::BTreeMap;

/// Build lifecycle configuration (cargo-make compatible)
#[derive(Debug, Clone, Default, serde::Deserialize, serde::Serialize)]
pub struct Lifecycle {
    /// Lifecycle phases (e.g., init, setup, format, lint, build, test)
    #[serde(default)]
    pub phases: Vec<String>,

    /// Hooks for each phase
    #[serde(default)]
    pub hooks: BTreeMap<String, Vec<String>>,

    /// Task definitions (cargo-make style)
    #[serde(default)]
    pub tasks: BTreeMap<String, Task>,

    /// Parallel execution groups
    #[serde(default)]
    pub parallel: BTreeMap<String, Vec<String>>,
}

/// Lifecycle task definition
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct Task {
    pub description: Option<String>,

    #[serde(default)]
    pub dependencies: Vec<String>,

    pub command: Option<String>,

    #[serde(default)]
    pub args: Vec<String>,

    pub script: Option<String>,
}
```

**Flexible task system:**
- Task graph via dependencies
- Parallel execution groups
- Hook integration points

---

### 9. Plugins Section (Plugin System)

```rust
use std::path::PathBuf;
use std::collections::BTreeMap;

/// Plugin system configuration
#[derive(Debug, Clone, Default, serde::Deserialize, serde::Serialize)]
pub struct Plugins {
    /// Plugin discovery paths
    #[serde(default)]
    pub paths: Vec<PathBuf>,

    /// Installed plugins
    #[serde(default)]
    pub installed: BTreeMap<String, PluginSpec>,

    /// Plugin-specific configuration
    #[serde(default)]
    pub config: BTreeMap<String, toml::Value>,

    /// Plugin lifecycle hooks
    #[serde(default)]
    pub hooks: BTreeMap<String, Vec<String>>,

    /// Plugin permissions (security sandboxing)
    #[serde(default)]
    pub permissions: BTreeMap<String, PluginPermissions>,
}

/// Plugin specification
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct PluginSpec {
    pub version: String,
    pub source: String,
}

/// Plugin permissions (security sandboxing)
#[derive(Debug, Clone, Default, serde::Deserialize, serde::Serialize)]
pub struct PluginPermissions {
    /// Filesystem permissions
    #[serde(default)]
    pub filesystem: Vec<String>,  // e.g., ["read:openapi.yaml", "write:src/api/**"]

    /// Network permissions
    #[serde(default)]
    pub network: Vec<String>,  // e.g., ["https://api.example.org"]

    /// Executable permissions
    #[serde(default)]
    pub exec: Vec<String>,  // e.g., ["rustfmt", "cargo"]
}
```

**Security-first:**
- Explicit permission model (filesystem, network, exec)
- Glob patterns for file access
- Sandboxing-ready

---

### 10. Profiles Section (Environment-Specific Overrides)

```rust
use std::collections::BTreeMap;

/// Profile system for environment-specific overrides
///
/// Zero-cost: Profiles share common types, selected at compile time
#[derive(Debug, Clone, Default, serde::Deserialize, serde::Serialize)]
pub struct Profiles {
    /// Default profile (used if none specified)
    pub default: Option<String>,

    /// Development profile
    pub dev: Option<Profile>,

    /// Production profile
    pub production: Option<Profile>,

    /// Testing profile
    pub test: Option<Profile>,

    /// CI profile
    pub ci: Option<Profile>,

    /// Benchmark profile
    pub bench: Option<Profile>,

    /// Custom profiles
    #[serde(flatten)]
    pub custom: BTreeMap<String, Profile>,
}

/// Profile configuration
#[derive(Debug, Clone, Default, serde::Deserialize, serde::Serialize)]
pub struct Profile {
    /// Extend another profile
    pub extends: Option<String>,

    /// Optimization level
    pub optimization: Option<OptimizationLevel>,

    /// Debug assertions
    pub debug_assertions: Option<bool>,

    /// Overflow checks
    pub overflow_checks: Option<bool>,

    /// LTO (Link-Time Optimization)
    pub lto: Option<LtoLevel>,

    /// Strip symbols
    pub strip: Option<bool>,

    /// Code coverage
    pub code_coverage: Option<bool>,

    /// Test threads
    pub test_threads: Option<usize>,

    /// Profile-specific dependencies
    pub dependencies: Option<BTreeMap<String, DependencySpec>>,

    /// Profile-specific template variables
    pub templates: Option<ProfileTemplates>,

    /// Profile-specific ontology constitution
    pub ontology: Option<ProfileOntology>,

    /// Profile-specific lifecycle hooks
    pub lifecycle: Option<ProfileLifecycle>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub enum OptimizationLevel {
    Debug,
    Release,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub enum LtoLevel {
    Thin,
    Fat,
}

#[derive(Debug, Clone, Default, serde::Deserialize, serde::Serialize)]
pub struct ProfileTemplates {
    #[serde(default)]
    pub vars: BTreeMap<String, String>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct ProfileOntology {
    pub constitution: Option<ProfileConstitution>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct ProfileConstitution {
    pub enforce_strict: Option<bool>,
    pub fail_on_warning: Option<bool>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct ProfileLifecycle {
    #[serde(default)]
    pub hooks: BTreeMap<String, Vec<String>>,
}

// Compile-time profile selection (const generics)
pub struct ProfileSelector<const PROFILE: u8>;

impl<const PROFILE: u8> ProfileSelector<PROFILE> {
    pub const fn select(profiles: &Profiles) -> Option<&Profile> {
        match PROFILE {
            0 => profiles.dev.as_ref(),
            1 => profiles.production.as_ref(),
            2 => profiles.test.as_ref(),
            3 => profiles.ci.as_ref(),
            4 => profiles.bench.as_ref(),
            _ => None,
        }
    }
}
```

**Const generic profile selection:**
- Profile selected at compile time (no runtime overhead)
- Type-safe profile access
- Profile inheritance via `extends` field

---

## Dual Lockfile Integration Strategy

### Problem Statement
Two lockfile systems exist:
1. **lockfile.rs** - Pack tracking (gpacks) - Tracks installed packs with versions, sources, dependencies
2. **lock_manager.rs** - Ontology locking - Tracks ontology packs with composition metadata

### Integration Design

```rust
/// Unified lockfile manager
///
/// Coordinates both pack lockfile and ontology lockfile
pub struct UnifiedLockfileManager {
    pack_lockfile: PackLockfile,
    ontology_lockfile: OntologyLockfile,
}

impl UnifiedLockfileManager {
    /// Create unified lockfile from configuration
    pub fn create(config: &GgenConfig<Compiled>) -> Result<Self, LockfileError> {
        // Generate pack lockfile from dependencies
        let pack_lockfile = Self::generate_pack_lockfile(config)?;

        // Generate ontology lockfile from ontology configuration
        let ontology_lockfile = Self::generate_ontology_lockfile(config)?;

        // Cross-validate: ensure ontology dependencies are in pack lockfile
        Self::cross_validate(&pack_lockfile, &ontology_lockfile)?;

        Ok(Self {
            pack_lockfile,
            ontology_lockfile,
        })
    }

    /// Load unified lockfile from disk
    pub fn load(pack_path: &Path, ontology_path: &Path) -> Result<Self, LockfileError> {
        let pack_lockfile = PackLockfile::from_file(pack_path)?;
        let ontology_lockfile = LockfileManager::load(ontology_path)?;

        // Cross-validate
        Self::cross_validate(&pack_lockfile, &ontology_lockfile)?;

        Ok(Self {
            pack_lockfile,
            ontology_lockfile,
        })
    }

    /// Save both lockfiles to disk
    pub fn save(&self, pack_path: &Path, ontology_path: &Path) -> Result<(), LockfileError> {
        self.pack_lockfile.save(pack_path)?;
        LockfileManager::save(&self.ontology_lockfile, ontology_path)?;
        Ok(())
    }

    /// Verify integrity of both lockfiles
    pub fn verify_integrity(&self) -> Result<(), LockfileError> {
        self.pack_lockfile.validate()?;
        LockfileManager::verify(&self.ontology_lockfile)?;
        Self::cross_validate(&self.pack_lockfile, &self.ontology_lockfile)?;
        Ok(())
    }

    /// Cross-validate: ensure ontology dependencies are in pack lockfile
    fn cross_validate(
        pack_lockfile: &PackLockfile,
        ontology_lockfile: &OntologyLockfile,
    ) -> Result<(), LockfileError> {
        // For each ontology package dependency, verify it's in pack lockfile
        for (name, ontology_pkg) in &ontology_lockfile.packages {
            for (dep_name, dep_version) in &ontology_pkg.dependencies {
                match pack_lockfile.get_pack(dep_name) {
                    Some(pack) if pack.version == *dep_version => {
                        // Valid: dependency version matches
                    }
                    Some(pack) => {
                        return Err(LockfileError::VersionMismatch {
                            package: dep_name.clone(),
                            ontology_version: dep_version.clone(),
                            pack_version: pack.version.clone(),
                        });
                    }
                    None => {
                        return Err(LockfileError::MissingDependency {
                            package: name.clone(),
                            dependency: dep_name.clone(),
                        });
                    }
                }
            }
        }
        Ok(())
    }

    /// Generate pack lockfile from configuration
    fn generate_pack_lockfile(config: &GgenConfig<Compiled>) -> Result<PackLockfile, LockfileError> {
        let mut lockfile = PackLockfile::new(env!("CARGO_PKG_VERSION"));

        // Add dependencies from config.dependencies
        for (name, spec) in &config.dependencies.dependencies {
            let locked_pack = Self::resolve_dependency(name, spec)?;
            lockfile.add_pack(name, locked_pack);
        }

        // Add dev dependencies
        for (name, spec) in &config.dependencies.dev_dependencies {
            let locked_pack = Self::resolve_dependency(name, spec)?;
            lockfile.add_pack(name, locked_pack);
        }

        // Validate lockfile
        lockfile.validate()?;

        Ok(lockfile)
    }

    /// Generate ontology lockfile from configuration
    fn generate_ontology_lockfile(
        config: &GgenConfig<Compiled>,
    ) -> Result<OntologyLockfile, LockfileError> {
        let mut packages = BTreeMap::new();

        // Parse ontology files and extract package info
        for file_path in &config.ontology.files {
            let package = Self::parse_ontology_file(file_path)?;
            packages.insert(package.name.clone(), package);
        }

        // Create composition metadata
        let composition = CompositionMetadata {
            strategy: "union".to_string(),  // TODO: Make configurable
            total_classes: packages.values().map(|p| p.classes_count).sum(),
            total_properties: packages.values().map(|p| p.properties_count).sum(),
            conflicts_resolved: 0,  // TODO: Track conflicts
            validation_status: "valid".to_string(),
        };

        LockfileManager::create(packages, composition).map_err(Into::into)
    }

    fn resolve_dependency(name: &str, spec: &DependencySpec) -> Result<LockedPack, LockfileError> {
        // TODO: Implement dependency resolution
        todo!()
    }

    fn parse_ontology_file(path: &Path) -> Result<OntologyPackage, LockfileError> {
        // TODO: Implement ontology file parsing
        todo!()
    }
}

struct OntologyPackage {
    name: String,
    version: String,
    classes_count: usize,
    properties_count: usize,
}

#[derive(Debug, thiserror::Error)]
pub enum LockfileError {
    #[error("Version mismatch: {package} - ontology expects {ontology_version}, pack has {pack_version}")]
    VersionMismatch {
        package: String,
        ontology_version: String,
        pack_version: String,
    },

    #[error("Missing dependency: {package} requires {dependency}")]
    MissingDependency {
        package: String,
        dependency: String,
    },

    #[error("Pack lockfile error: {0}")]
    PackLockfile(#[from] ggen_utils::error::Error),
}
```

**Integration Benefits:**
- **Single source of truth**: UnifiedLockfileManager coordinates both lockfiles
- **Cross-validation**: Ensures consistency between pack and ontology dependencies
- **Type safety**: Version mismatches caught at compile time (via semver::Version)
- **Reproducibility**: Both lockfiles together guarantee reproducible builds

---

## Type-Level Invariants

### 1. Configuration Lifecycle Invariants

**Invariant**: Cannot call operations on wrong state.

```rust
// ✅ Valid: Unvalidated → Validated → Compiled → Locked
let config = GgenConfig::<Unvalidated>::from_toml(toml_str)?
    .validate()?
    .compile()?
    .lock()?;

// ❌ Compile error: Cannot call .lock() on Unvalidated
let config = GgenConfig::<Unvalidated>::from_toml(toml_str)?;
config.lock();  // ERROR: no method `.lock()` on `GgenConfig<Unvalidated>`

// ❌ Compile error: Cannot call .validate() on Validated
let config = GgenConfig::<Unvalidated>::from_toml(toml_str)?
    .validate()?;
config.validate();  // ERROR: no method `.validate()` on `GgenConfig<Validated>`
```

### 2. Semantic Versioning Invariants

**Invariant**: Version strings must be valid semver.

```rust
use semver::Version;

// ✅ Valid: Semantic version type ensures valid format
let version = Version::parse("1.2.3")?;  // OK

// ❌ Compile error: Invalid version format caught at parse time
let version = Version::parse("1.2")?;  // ERROR: invalid semver format
```

### 3. Dependency Source Invariants

**Invariant**: Dependency source must be one of Registry | GitHub | Local.

```rust
// ✅ Valid: Enum ensures only valid sources
let source = PackSource::Registry {
    url: "https://registry.ggen.io".to_string(),
};

// ❌ Compile error: Invalid source (no "FTP" variant)
let source = PackSource::FTP { url: "..." };  // ERROR: no variant `FTP`
```

### 4. Profile Selection Invariants

**Invariant**: Profile must exist before use.

```rust
// ✅ Valid: Const generic ensures profile exists at compile time
const DEV: u8 = 0;
let profile = ProfileSelector::<DEV>::select(&profiles);

// ❌ Compile error: Invalid profile index
const INVALID: u8 = 99;
let profile = ProfileSelector::<INVALID>::select(&profiles);  // ERROR: out of bounds
```

---

## Const Generic Patterns

### 1. Compile-Time Strategy Selection

```rust
// Strategy selected at compile time (zero runtime overhead)
pub struct TemplateResolver<const STRATEGY: u8>;

impl<const STRATEGY: u8> TemplateResolver<STRATEGY> {
    pub fn resolve(&self, templates: &Templates) -> Result<Vec<ResolvedTemplate>, TemplateError> {
        // Const match - compiler eliminates all branches except selected one
        match STRATEGY {
            0 => self.resolve_inheritance(templates),
            1 => self.resolve_composition(templates),
            2 => self.resolve_sparql(templates),
            _ => unreachable!(),  // Compiler proves this is unreachable
        }
    }
}

// Usage
const INHERITANCE: u8 = 0;
let resolver = TemplateResolver::<INHERITANCE>::new();
resolver.resolve(&templates)?;  // Only inheritance code compiled, zero overhead
```

### 2. Compile-Time Validation Limits

```rust
// Maximum allowed workspace members (compile-time limit)
pub struct Workspace<const MAX_MEMBERS: usize = 100> {
    members: arrayvec::ArrayVec<String, MAX_MEMBERS>,
}

impl<const MAX_MEMBERS: usize> Workspace<MAX_MEMBERS> {
    pub fn add_member(&mut self, member: String) -> Result<(), WorkspaceError> {
        self.members.try_push(member)
            .map_err(|_| WorkspaceError::TooManyMembers { max: MAX_MEMBERS })
    }
}

// Usage
const SMALL_WORKSPACE: usize = 10;
let mut workspace = Workspace::<SMALL_WORKSPACE>::new();
```

---

## Result<T, E> Error Handling

**All fallible operations return Result<T, E>:**

```rust
// Configuration parsing
pub fn from_toml(content: &str) -> Result<GgenConfig<Unvalidated>, ConfigError>;

// Validation
pub fn validate(self) -> Result<GgenConfig<Validated>, ConfigError>;

// Compilation
pub fn compile(self) -> Result<GgenConfig<Compiled>, ConfigError>;

// Locking
pub fn lock(self) -> Result<GgenConfig<Locked>, ConfigError>;

// Lockfile operations
pub fn create(packages: BTreeMap<String, LockedPackage>, composition: CompositionMetadata)
    -> Result<OntologyLockfile, LockfileError>;
pub fn load(path: &Path) -> Result<OntologyLockfile, LockfileError>;
pub fn save(lockfile: &OntologyLockfile, path: &Path) -> Result<(), LockfileError>;
pub fn verify(lockfile: &OntologyLockfile) -> Result<(), LockfileError>;

// Template resolution
pub fn resolve(&self, templates: &Templates) -> Result<Vec<ResolvedTemplate>, TemplateError>;

// Dependency resolution
pub fn resolve(&self, candidates: &[DependencyCandidate]) -> Result<DependencyCandidate, GraphError>;
```

**No unwrap() or expect() in production code - all errors propagated via Result.**

---

## Ownership Semantics & Lifetime Annotations

### 1. Configuration Ownership

```rust
// GgenConfig owns all its data (no lifetimes required)
pub struct GgenConfig<S> {
    pub project: Project,              // Owned
    pub workspace: Option<Workspace>,  // Owned
    pub graph: Option<Graph>,          // Owned
    // ... all owned data
    _state: PhantomData<S>,
}

// State transitions consume and return ownership
impl GgenConfig<Unvalidated> {
    pub fn validate(self) -> Result<GgenConfig<Validated>, ConfigError> {
        //            ^^^^ consumes Unvalidated, returns Validated
        // Old state is moved, preventing reuse
    }
}
```

### 2. Lockfile Borrowing

```rust
// Lockfile verification borrows (no mutation)
pub fn verify(lockfile: &OntologyLockfile) -> Result<(), LockfileError> {
    //                ^^^ immutable borrow - lockfile not modified
    // Caller retains ownership
}

// Lockfile saving borrows (no mutation of logical content)
pub fn save(lockfile: &OntologyLockfile, path: &Path) -> Result<(), LockfileError> {
    //                ^^^ immutable borrow
}
```

### 3. Template Resolution (Zero-Copy Where Possible)

```rust
// Template resolver borrows templates (no copy)
pub fn resolve<'a>(&self, templates: &'a Templates) -> Result<Vec<ResolvedTemplate<'a>>, TemplateError> {
    //                                ^^^ lifetime: resolved templates borrow from input
}

pub struct ResolvedTemplate<'a> {
    pub target_file: PathBuf,     // Owned (small)
    pub content: &'a str,          // Borrowed (zero-copy)
}
```

### 4. Dependency Candidate Ownership

```rust
// Dependency candidates are small, owned types (no lifetimes)
#[derive(Debug, Clone)]
pub struct DependencyCandidate {
    pub name: String,           // Owned
    pub version: semver::Version,  // Owned (small)
    pub source: String,         // Owned
}

// Resolution returns owned candidate (caller owns result)
pub fn resolve(&self, candidates: &[DependencyCandidate])
    -> Result<DependencyCandidate, GraphError> {
    //        ^^^^^^^^^^^^^^^^^^^ owned return type
}
```

---

## Memory Storage Strategy

**Design decisions stored at**: `.claude/memory/MEMORY.md` (key: `hive/architect/ggen-toml-design`)

**Memory contents:**
- Type-first API design with PhantomData state machines
- Zero-cost abstraction patterns for all 10 sections
- Dual lockfile integration strategy
- Type-level invariants and const generic patterns
- Result<T, E> error handling patterns
- Ownership semantics documentation

**Accessible to:**
- Coder agents (implement types)
- Test agents (verify invariants)
- Reviewer agents (check type safety)
- Integration agents (verify lockfile coordination)

---

## Implementation Priorities (80/20 Rule)

### Phase 1: Critical 20% (Core Type System)
1. **Configuration lifecycle state machine** (GgenConfig<S>)
2. **Project section** (already exists, expand)
3. **Dependencies section** (DependencySpec, reusable)
4. **Dual lockfile integration** (UnifiedLockfileManager)

### Phase 2: High Value 30% (Graph & Templates)
5. **Graph section** (ResolutionStrategy, ConflictPolicy)
6. **Templates section** (TemplateResolver, composition)
7. **Ontology section** (Constitution, SHACL integration)

### Phase 3: Feature Completeness 30% (Workspace, Lifecycle, Profiles)
8. **Workspace section** (members, shared deps)
9. **Lifecycle section** (phases, hooks, tasks)
10. **Profiles section** (ProfileSelector, const generics)

### Phase 4: Advanced 20% (Generators, Plugins)
11. **Generators section** (Pipeline, PipelineStep)
12. **Plugins section** (PluginPermissions, sandboxing)

---

## Conclusion

This architecture design provides a complete, type-first, zero-cost approach to expanding ggen.toml configuration system. Key achievements:

1. **Type-level state machines** ensure configuration lifecycle correctness
2. **Zero-cost abstractions** via const generics and monomorphization
3. **Dual lockfile integration** coordinates gpacks and ontologies
4. **Invalid states unrepresentable** via type system
5. **Result<T, E>** for all fallible operations
6. **Ownership semantics** documented for clear memory management

**Next Steps for Implementation:**
1. Implement Phase 1 (critical 20%) first
2. Coordinate with Coder agents via memory
3. Verify with Test agents (Chicago TDD)
4. Review type safety with Reviewer agents
5. Integrate dual lockfiles with Production Validator

This design is stored in memory for cross-agent coordination.
