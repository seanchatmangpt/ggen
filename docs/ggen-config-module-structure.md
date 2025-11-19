# ggen-config Module Structure

## Overview

This document outlines the implementation structure for the `ggen-config` crate, which provides the hyper-advanced configuration system for ggen projects via `ggen.toml`.

## Crate Structure

```
crates/ggen-config/
├── Cargo.toml
├── README.md
├── src/
│   ├── lib.rs                      # Public API and re-exports
│   ├── types.rs                    # Core data structures
│   ├── parser.rs                   # TOML parsing
│   ├── validator.rs                # Configuration validation
│   ├── normalizer.rs               # Smart defaults & inference
│   ├── graph_integration.rs        # RDF graph conversion
│   ├── template_integration.rs     # Template resolution
│   ├── ontology_integration.rs     # Ontology validation
│   ├── plugin_integration.rs       # Plugin system
│   ├── profile_resolver.rs         # Profile merging
│   ├── error.rs                    # Error types
│   └── utils.rs                    # Helper functions
├── tests/
│   ├── parser_tests.rs             # TOML parsing tests
│   ├── validator_tests.rs          # Validation tests
│   ├── integration_tests.rs        # End-to-end tests
│   └── fixtures/
│       ├── valid/                  # Valid ggen.toml examples
│       ├── invalid/                # Invalid configs for error tests
│       └── complex/                # Complex real-world examples
└── examples/
    ├── basic.rs                    # Basic usage example
    ├── workspace.rs                # Workspace configuration
    └── advanced.rs                 # Advanced features demo
```

## Module Responsibilities

### lib.rs - Public API

Entry point for the crate. Exposes the main API and re-exports types.

```rust
//! ggen-config: Hyper-advanced configuration system for ggen
//!
//! # Features
//! - Graph-based dependency resolution
//! - Ontology-aware validation
//! - Template composition and inheritance
//! - Plugin system
//! - Profile management
//!
//! # Example
//! ```rust
//! use ggen_config::{GgenConfig, load_config};
//!
//! # async fn example() -> Result<()> {
//! // Load configuration
//! let config = load_config("ggen.toml").await?;
//!
//! // Validate against ontology
//! config.validate().await?;
//!
//! // Convert to RDF graph
//! let graph = config.to_graph().await?;
//! # Ok(())
//! # }
//! ```

pub mod error;
pub mod graph_integration;
pub mod normalizer;
pub mod ontology_integration;
pub mod parser;
pub mod plugin_integration;
pub mod profile_resolver;
pub mod template_integration;
pub mod types;
pub mod validator;
mod utils;

// Re-exports
pub use error::{Error, Result};
pub use types::*;

/// Load and parse ggen.toml configuration file
pub async fn load_config(path: &str) -> Result<GgenConfig> {
    let config = parser::parse_file(path).await?;
    let normalized = normalizer::normalize(config).await?;
    validator::validate(&normalized).await?;
    Ok(normalized)
}

/// Load configuration with specific profile
pub async fn load_config_with_profile(
    path: &str,
    profile: &str
) -> Result<GgenConfig> {
    let config = load_config(path).await?;
    profile_resolver::apply_profile(config, profile).await
}
```

### types.rs - Data Structures

Complete type definitions matching the TOML schema.

Key exports:
- `GgenConfig` - Root configuration
- `ProjectConfig` - Project metadata
- `WorkspaceConfig` - Workspace settings
- `GraphConfig` - Dependency resolution
- `OntologyConfig` - RDF/OWL settings
- `TemplateConfig` - Template system
- `GeneratorConfig` - Code generation
- `LifecycleConfig` - Build lifecycle
- `PluginConfig` - Plugin system
- `ProfileConfig` - Environment profiles
- `Dependency` - Dependency specification

### parser.rs - TOML Parsing

Parse TOML files into Rust data structures.

```rust
use crate::types::GgenConfig;
use crate::error::Result;
use std::path::Path;

/// Parse ggen.toml file
pub async fn parse_file(path: impl AsRef<Path>) -> Result<GgenConfig> {
    let content = tokio::fs::read_to_string(path).await?;
    parse_str(&content)
}

/// Parse TOML string
pub fn parse_str(content: &str) -> Result<GgenConfig> {
    toml::from_str(content)
        .map_err(|e| Error::ParseError(format!("Invalid TOML: {}", e)))
}

/// Parse TOML with custom deserializers
pub fn parse_with_defaults(content: &str) -> Result<GgenConfig> {
    // Parse with serde
    let mut config: GgenConfig = toml::from_str(content)?;

    // Apply defaults
    if config.project.r#type == "auto" {
        config.project.r#type = detect_project_type()?;
    }

    if config.project.language == "auto" {
        config.project.language = detect_language()?;
    }

    Ok(config)
}
```

### validator.rs - Validation

Validate configuration against rules and schemas.

```rust
use crate::types::GgenConfig;
use crate::error::Result;

/// Validation context
pub struct ValidationContext {
    pub errors: Vec<String>,
    pub warnings: Vec<String>,
}

/// Validate complete configuration
pub async fn validate(config: &GgenConfig) -> Result<()> {
    let mut ctx = ValidationContext::new();

    validate_project(&config.project, &mut ctx)?;
    validate_dependencies(&config.dependencies, &mut ctx)?;
    validate_graph(&config.graph, &mut ctx)?;
    validate_templates(&config.templates, &mut ctx)?;
    validate_plugins(&config.plugins, &mut ctx)?;

    if !ctx.errors.is_empty() {
        return Err(Error::ValidationError(ctx.errors));
    }

    Ok(())
}

/// Validate project metadata
fn validate_project(
    project: &ProjectConfig,
    ctx: &mut ValidationContext
) -> Result<()> {
    // Name validation
    if project.name.is_empty() {
        ctx.errors.push("Project name is required".to_string());
    }

    // Version validation (semantic versioning)
    if !is_semver(&project.version) {
        ctx.errors.push(format!(
            "Invalid version '{}': must follow semantic versioning",
            project.version
        ));
    }

    // URI validation
    if let Some(uri) = &project.uri {
        if !is_valid_uri(uri) {
            ctx.errors.push(format!("Invalid URI: {}", uri));
        }
    }

    Ok(())
}
```

### normalizer.rs - Smart Defaults

Apply intelligent defaults and inference.

```rust
use crate::types::GgenConfig;
use crate::error::Result;
use std::path::Path;

/// Normalize configuration with smart defaults
pub async fn normalize(mut config: GgenConfig) -> Result<GgenConfig> {
    // Auto-detect project type
    if config.project.r#type == "auto" {
        config.project.r#type = detect_project_type().await?;
    }

    // Auto-detect language
    if config.project.language == "auto" {
        config.project.language = detect_language().await?;
    }

    // Generate URI if missing
    if config.project.uri.is_none() {
        config.project.uri = Some(generate_uri(&config.project)?);
    }

    // Generate namespace if missing
    if config.project.namespace.is_none() {
        config.project.namespace = Some(
            config.project.name.to_lowercase().replace("-", "")
        );
    }

    // Apply template path defaults
    if config.templates.paths.is_empty() {
        config.templates.paths = default_template_paths()?;
    }

    Ok(config)
}

/// Detect project type from filesystem
async fn detect_project_type() -> Result<String> {
    if Path::new("Cargo.toml").exists() {
        if Path::new("src/main.rs").exists() {
            Ok("binary".to_string())
        } else if Path::new("src/lib.rs").exists() {
            Ok("library".to_string())
        } else {
            Ok("workspace".to_string())
        }
    } else if Path::new("package.json").exists() {
        Ok("library".to_string())
    } else {
        Ok("unknown".to_string())
    }
}

/// Detect programming language
async fn detect_language() -> Result<String> {
    if Path::new("Cargo.toml").exists() {
        Ok("rust".to_string())
    } else if Path::new("package.json").exists() {
        Ok("typescript".to_string())
    } else if Path::new("pyproject.toml").exists() {
        Ok("python".to_string())
    } else {
        Ok("unknown".to_string())
    }
}
```

### graph_integration.rs - RDF Graph

Convert configuration to RDF graph for SPARQL queries.

```rust
use crate::types::GgenConfig;
use crate::error::Result;
use ggen_core::graph::Graph;

/// Convert configuration to RDF graph
pub async fn config_to_graph(config: &GgenConfig) -> Result<Graph> {
    let graph = Graph::new()?;

    // Add project metadata
    add_project_triples(&graph, &config.project).await?;

    // Add dependencies
    add_dependency_triples(&graph, &config.dependencies).await?;

    // Add features
    add_feature_triples(&graph, &config.graph.features).await?;

    // Add ontology
    if let Some(inline) = &config.ontology.inline {
        graph.insert_turtle(inline)?;
    }

    for file in &config.ontology.files {
        graph.insert_from_file(file, RdfFormat::Turtle)?;
    }

    Ok(graph)
}

/// Query configuration graph
pub async fn query_config(
    config: &GgenConfig,
    sparql: &str
) -> Result<Vec<BTreeMap<String, String>>> {
    let graph = config_to_graph(config).await?;
    graph.query_to_map(sparql)
}
```

### template_integration.rs - Templates

Resolve templates with inheritance and composition.

```rust
use crate::types::{GgenConfig, TemplateConfig};
use crate::error::Result;
use ggen_core::template::Template;

/// Resolve template with inheritance
pub async fn resolve_template(
    config: &GgenConfig,
    target_file: &str
) -> Result<Template> {
    // Check composition first
    if let Some(sources) = config.templates.compose.get(target_file) {
        return compose_templates(config, sources).await;
    }

    // Check inheritance
    if let Some(base) = config.templates.extends.get(target_file) {
        return extend_template(config, base, target_file).await;
    }

    // Direct load
    load_template(config, target_file).await
}

/// Compose multiple templates
async fn compose_templates(
    config: &GgenConfig,
    sources: &[String]
) -> Result<Template> {
    let mut composed = Template::new();

    for source in sources {
        let template = load_template(config, source).await?;
        composed = composed.merge(template)?;
    }

    Ok(composed)
}
```

### ontology_integration.rs - Ontology Validation

Validate against OWL ontologies and run constitution checks.

```rust
use crate::types::GgenConfig;
use crate::error::Result;
use ggen_core::ontology::sigma_runtime::SigmaRuntime;

/// Validate configuration against ontology
pub async fn validate_ontology(config: &GgenConfig) -> Result<()> {
    let graph = crate::graph_integration::config_to_graph(config).await?;
    let runtime = SigmaRuntime::new(graph);

    // Run built-in constitution checks
    for check_name in &config.ontology.constitution.checks {
        runtime.run_check(check_name).await?;
    }

    // Run custom SPARQL checks
    for (name, query) in &config.ontology.constitution.custom {
        let result = runtime.graph().query_ask(query)?;
        if result {
            return Err(Error::ConstitutionViolation(name.clone()));
        }
    }

    Ok(())
}
```

### plugin_integration.rs - Plugin System

Discover, load, and execute plugins.

```rust
use crate::types::{GgenConfig, PluginConfig};
use crate::error::Result;

#[async_trait]
pub trait Plugin: Send + Sync {
    fn name(&self) -> &str;
    fn version(&self) -> &str;
    async fn init(&mut self, config: PluginConfig) -> Result<()>;
    fn hooks(&self) -> Vec<LifecycleHook>;
    async fn execute(
        &self,
        hook: LifecycleHook,
        context: &Context
    ) -> Result<()>;
}

pub struct PluginManager {
    plugins: Vec<Box<dyn Plugin>>,
}

impl PluginManager {
    pub async fn discover(config: &GgenConfig) -> Result<Self> {
        let mut plugins = Vec::new();

        for (name, plugin_config) in &config.plugins.installed {
            let plugin = load_plugin(name, plugin_config).await?;
            plugins.push(plugin);
        }

        Ok(Self { plugins })
    }

    pub async fn execute_hook(
        &self,
        hook: LifecycleHook,
        context: &Context
    ) -> Result<()> {
        for plugin in &self.plugins {
            if plugin.hooks().contains(&hook) {
                plugin.execute(hook, context).await?;
            }
        }
        Ok(())
    }
}
```

### profile_resolver.rs - Profile Management

Merge profiles with inheritance.

```rust
use crate::types::{GgenConfig, ProfileConfig};
use crate::error::Result;

/// Apply profile to configuration
pub async fn apply_profile(
    mut config: GgenConfig,
    profile_name: &str
) -> Result<GgenConfig> {
    let profile = config.profiles
        .get(profile_name)
        .ok_or_else(|| Error::ProfileNotFound(profile_name.to_string()))?
        .clone();

    // Handle profile inheritance
    let resolved_profile = if let Some(extends) = &profile.extends {
        merge_profiles(&profile, extends, &config.profiles)?
    } else {
        profile
    };

    // Apply profile overrides
    config = apply_profile_overrides(config, &resolved_profile)?;

    Ok(config)
}

/// Merge profile with parent profile
fn merge_profiles(
    profile: &ProfileConfig,
    extends: &str,
    all_profiles: &BTreeMap<String, ProfileConfig>
) -> Result<ProfileConfig> {
    let parent = all_profiles
        .get(extends)
        .ok_or_else(|| Error::ProfileNotFound(extends.to_string()))?;

    // Recursively merge if parent extends another profile
    let parent = if let Some(parent_extends) = &parent.extends {
        merge_profiles(parent, parent_extends, all_profiles)?
    } else {
        parent.clone()
    };

    // Merge child into parent
    Ok(ProfileConfig {
        extends: profile.extends.clone(),
        optimization: profile.optimization.or(parent.optimization),
        debug_assertions: profile.debug_assertions.or(parent.debug_assertions),
        // ... merge all fields
    })
}
```

### error.rs - Error Types

Comprehensive error handling.

```rust
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Parse error: {0}")]
    ParseError(String),

    #[error("Validation error: {0:?}")]
    ValidationError(Vec<String>),

    #[error("Constitution violation: {0}")]
    ConstitutionViolation(String),

    #[error("Profile not found: {0}")]
    ProfileNotFound(String),

    #[error("Template not found: {0}")]
    TemplateNotFound(String),

    #[error("Plugin error: {0}")]
    PluginError(String),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("TOML error: {0}")]
    Toml(#[from] toml::de::Error),

    #[error("Graph error: {0}")]
    Graph(#[from] ggen_utils::error::Error),
}

pub type Result<T> = std::result::Result<T, Error>;
```

## Cargo.toml

```toml
[package]
name = "ggen-config"
version = "3.2.0"
edition = "2021"
authors = ["ggen contributors"]
license = "MIT"
description = "Hyper-advanced configuration system for ggen"
repository = "https://github.com/seanchatmangpt/ggen"

[dependencies]
# Core dependencies
ggen-core = { workspace = true }
ggen-utils = { workspace = true }

# Async runtime
tokio = { workspace = true }
async-trait = { workspace = true }

# Serialization
serde = { workspace = true }
serde_json = { workspace = true }
toml = { workspace = true }

# Error handling
thiserror = { workspace = true }
anyhow = { workspace = true }

# Logging
tracing = { workspace = true }
log = { workspace = true }

# RDF
oxigraph = { workspace = true }

# Templates
tera = { workspace = true }

[dev-dependencies]
tempfile = { workspace = true }
assert_fs = "1.1"
insta = "1.43"
proptest = { workspace = true }
```

## Testing Strategy

### Unit Tests
- Parser: Valid/invalid TOML parsing
- Validator: Each validation rule
- Normalizer: Auto-detection logic
- Profile resolver: Inheritance and merging

### Integration Tests
- End-to-end configuration loading
- Graph conversion and queries
- Template resolution
- Plugin loading
- Profile application

### Property Tests
- TOML roundtrip (parse → serialize → parse)
- Profile merging associativity
- Graph conversion correctness

### Fixtures
- `fixtures/valid/`: Valid configurations
- `fixtures/invalid/`: Invalid configurations (error testing)
- `fixtures/complex/`: Real-world complex examples

## Implementation Priority

1. **Phase 1: Core parsing** (parser.rs, types.rs, error.rs)
2. **Phase 2: Validation** (validator.rs, normalizer.rs)
3. **Phase 3: Graph integration** (graph_integration.rs)
4. **Phase 4: Templates** (template_integration.rs)
5. **Phase 5: Ontology** (ontology_integration.rs)
6. **Phase 6: Profiles** (profile_resolver.rs)
7. **Phase 7: Plugins** (plugin_integration.rs)
8. **Phase 8: Testing & docs**

## Next Steps

1. Create `crates/ggen-config` directory structure
2. Implement Phase 1 (core parsing)
3. Add comprehensive tests for each phase
4. Integrate with ggen-cli for `ggen config` commands
5. Document public API
6. Create migration guides from Cargo.toml/package.json
