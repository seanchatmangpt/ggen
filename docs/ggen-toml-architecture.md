# ggen.toml Architecture: Hyper-Advanced Configuration System

## Executive Summary

This document defines the architecture for `ggen.toml`, a hyper-advanced project configuration system that leverages ggen's unique strengths in graph-based dependency resolution, ontology awareness, and template composition. Unlike traditional package managers, `ggen.toml` treats configuration as **executable knowledge graphs** that drive deterministic code generation.

## Core Philosophy

### 1. Configuration as Knowledge Graph
- **Traditional approach**: Configuration is static key-value pairs
- **ggen approach**: Configuration is an RDF graph with semantic relationships
- **Benefit**: Queries, inference, and composition are first-class operations

### 2. Progressive Disclosure
- **Sane defaults**: Zero-config for simple projects
- **Gradual complexity**: Expose advanced features only when needed
- **Smart inference**: Auto-detect project type, language, and dependencies

### 3. Template-First Design
- **Templates are executable**: Configuration drives template rendering
- **Inheritance**: Templates can inherit and compose from base templates
- **Multi-language**: Single configuration supports polyglot projects

## Architecture Layers

```
┌─────────────────────────────────────────────────────────────────┐
│                   User Interface Layer                          │
│  ggen.toml (TOML) ─→ Parsed ─→ Validated ─→ Normalized         │
└────────────────────────┬────────────────────────────────────────┘
                         │
┌────────────────────────▼────────────────────────────────────────┐
│                  Semantic Layer (RDF/OWL)                       │
│  Configuration Graph ←→ Ontology (Σ²) ←→ Domain Models         │
└────────────────────────┬────────────────────────────────────────┘
                         │
┌────────────────────────▼────────────────────────────────────────┐
│                 Execution Layer (ggen-core)                     │
│  Graph Query ─→ Template Resolution ─→ Code Generation         │
└─────────────────────────────────────────────────────────────────┘
```

## Schema Design

### Top-Level Sections

```toml
[project]           # Project metadata and identity
[workspace]         # Mono-repo configuration
[graph]             # Graph-based dependency resolution
[ontology]          # RDF/OWL ontology integration
[templates]         # Template configuration and inheritance
[generators]        # Code generation pipelines
[lifecycle]         # Build lifecycle hooks
[plugins]           # Plugin system configuration
[profiles]          # Environment-specific overrides
```

### 1. Project Section

```toml
[project]
name = "my-project"           # Project identifier
version = "1.0.0"             # Semantic version
description = "Project description"
authors = ["Author <email>"]
license = "MIT"
edition = "2021"              # Language edition (Rust, TS version, etc.)

# Smart detection - auto-infer from project structure
type = "auto"                 # auto | library | binary | workspace
language = "auto"             # auto | rust | typescript | python | multi

# Graph identity - RDF URI for this project
uri = "http://example.org/my-project"
namespace = "myproj"          # Short prefix for RDF queries

# Template inheritance
extends = "ggen:rust-cli"     # Inherit from base template
```

**Design rationale:**
- **Auto-detection**: Minimize required fields by detecting project structure
- **Graph identity**: Every project has an RDF URI for semantic queries
- **Template inheritance**: Projects inherit defaults from base templates

### 2. Workspace Section (Mono-repo Support)

```toml
[workspace]
members = [
    "crates/*",
    "packages/*"
]
exclude = [
    "target",
    "node_modules"
]

# Shared dependencies across workspace
[workspace.dependencies]
serde = { version = "1.0", features = ["derive"] }
tokio = { version = "1.0", features = ["full"] }

# Workspace-level graph queries
[workspace.graph]
query = """
SELECT ?member ?dependency
WHERE {
    ?member a :WorkspaceMember .
    ?member :dependsOn ?dependency .
}
"""
```

**Design rationale:**
- **Cargo-compatible**: Aligns with Cargo workspace conventions
- **Graph queries**: SPARQL queries across workspace members
- **Shared config**: DRY principle for common dependencies

### 3. Graph Section (Dependency Resolution)

```toml
[graph]
# Resolution strategy
strategy = "smart"            # smart | conservative | aggressive

# Conflict resolution
conflict_resolution = "newest" # newest | oldest | semver-compatible

# Graph-based queries for dependencies
[graph.queries]
critical_path = """
SELECT ?dep ?version
WHERE {
    ?dep a :Dependency .
    ?dep :version ?version .
    ?dep :criticalPath true .
}
ORDER BY ?dep
"""

# Feature flags as graph nodes
[graph.features]
default = ["cli", "templates"]
cli = ["clap", "anyhow"]
templates = ["tera", "serde_yaml"]
async = ["tokio", "async-trait"]

# Dependencies as graph
[dependencies]
clap = { version = "4.5", features = ["derive"] }
serde = { version = "1.0", optional = true }

# Conditional dependencies (graph guards)
[target.'cfg(unix)'.dependencies]
libc = "0.2"

[target.'cfg(windows)'.dependencies]
winapi = "0.3"
```

**Design rationale:**
- **Graph-based resolution**: Dependencies are RDF triples, not flat lists
- **SPARQL queries**: First-class support for dependency queries
- **Conditional logic**: Platform-specific deps as graph guards
- **Feature flags**: Features are graph nodes with relationships

### 4. Ontology Section (RDF/OWL Integration)

```toml
[ontology]
# Base ontology files
files = [
    "ontology/core.ttl",
    "ontology/domain.ttl"
]

# Inline RDF (Turtle format)
inline = """
@prefix ex: <http://example.org/> .
@prefix ggen: <http://ggen.org/ontology/> .

ex:MyProject a ggen:RustProject ;
    ggen:hasFeature ex:AsyncRuntime ;
    ggen:requiresVersion "1.75.0" .
"""

# Ontology validation (SHACL shapes)
shapes = [
    "shapes/project-shape.ttl"
]

# Constitution (invariant checks)
[ontology.constitution]
checks = [
    "NoRetrocausation",
    "TypeSoundness",
    "GuardSoundness"
]

# Custom invariants (SPARQL ASK queries)
[ontology.constitution.custom]
no_circular_deps = """
ASK {
    ?a :dependsOn+ ?a .
}
"""
semantic_versioning = """
ASK {
    ?dep :version ?v .
    FILTER(!regex(str(?v), "^[0-9]+\\.[0-9]+\\.[0-9]+$"))
}
"""
```

**Design rationale:**
- **Ontology-aware**: Configuration validates against OWL ontologies
- **Constitution**: Hard invariants checked at configuration time
- **SHACL validation**: Schema validation for RDF data
- **Custom checks**: User-defined invariants as SPARQL queries

### 5. Templates Section (Composition & Inheritance)

```toml
[templates]
# Template search paths
paths = [
    ".ggen/templates",
    "~/.ggen/templates",
    "registry://ggen/templates"
]

# Default template variables
[templates.vars]
project_name = "{{ project.name }}"
author = "{{ project.authors[0] }}"
year = "2024"

# Template inheritance chain
[templates.extends]
"src/lib.rs" = "ggen:rust-lib"
"src/main.rs" = "ggen:rust-cli"
"README.md" = "ggen:readme"

# Template composition (merge multiple templates)
[templates.compose]
"Cargo.toml" = [
    "ggen:cargo-base",
    "ggen:cargo-workspace",
    ".ggen/cargo-custom.toml"
]

# Template guards (conditional rendering)
[templates.guards]
"tests/integration.rs" = "features.contains('testing')"
"benches/perf.rs" = "features.contains('benchmarks')"

# SPARQL-driven template selection
[templates.queries]
find_templates = """
SELECT ?template ?targetFile
WHERE {
    ?template a ggen:Template .
    ?template ggen:targetFile ?targetFile .
    ?template ggen:applicableWhen ?condition .
    FILTER(eval(?condition))
}
"""
```

**Design rationale:**
- **Multi-source**: Templates from local, home dir, and registry
- **Inheritance**: Templates extend base templates
- **Composition**: Merge multiple templates into single output
- **Guards**: Conditional rendering based on features/conditions
- **SPARQL selection**: Query-driven template discovery

### 6. Generators Section (Code Generation Pipelines)

```toml
[generators]
# Generator registry
registry = "https://registry.ggen.org"

# Installed generators
[generators.installed]
rust-cli = { version = "1.0.0", registry = "ggen" }
typescript-api = { version = "2.1.0", registry = "ggen" }

# Generator pipelines
[[generators.pipeline]]
name = "scaffold"
description = "Generate project scaffold"
steps = [
    { action = "template", template = "Cargo.toml" },
    { action = "template", template = "src/main.rs" },
    { action = "template", template = "README.md" },
    { action = "exec", command = "cargo fmt" }
]

[[generators.pipeline]]
name = "api"
description = "Generate REST API from OpenAPI spec"
inputs = ["openapi.yaml"]
steps = [
    { action = "parse", parser = "openapi" },
    { action = "transform", query = "sparql/extract-endpoints.rq" },
    { action = "template", template = "api/handler.rs.tera" },
    { action = "template", template = "api/routes.rs.tera" }
]

# Generator hooks
[generators.hooks]
before_generate = "npm run prebuild"
after_generate = "cargo fmt && cargo test"
on_error = "echo 'Generation failed' && exit 1"
```

**Design rationale:**
- **Pipeline architecture**: Multi-step code generation workflows
- **Composable steps**: Mix parsing, SPARQL transforms, and templates
- **Hooks**: Lifecycle hooks for validation and cleanup
- **Registry support**: Download generators from central registry

### 7. Lifecycle Section (Build Hooks)

```toml
[lifecycle]
# Lifecycle phases (cargo-make compatible)
phases = ["init", "build", "test", "deploy"]

# Hooks for each phase
[lifecycle.hooks]
before_build = [
    "cargo check",
    "ggen validate"
]
after_build = [
    "cargo test",
    "ggen audit"
]

# Task definitions
[lifecycle.tasks.build]
description = "Build the project"
dependencies = ["format", "lint"]
command = "cargo build --release"

[lifecycle.tasks.format]
description = "Format code"
command = "cargo fmt --all"

[lifecycle.tasks.lint]
description = "Lint code"
command = "cargo clippy -- -D warnings"

# Parallel execution
[lifecycle.parallel]
format_and_lint = ["format", "lint"]
test_suite = ["test-unit", "test-integration", "test-e2e"]
```

**Design rationale:**
- **Cargo-make compatible**: Aligns with existing Rust tooling
- **Hooks**: Execute commands before/after lifecycle phases
- **Parallel execution**: Run independent tasks concurrently
- **Validation integration**: Hook into ggen's validation system

### 8. Plugins Section (Extensibility)

```toml
[plugins]
# Plugin discovery paths
paths = [
    ".ggen/plugins",
    "~/.ggen/plugins"
]

# Installed plugins
[plugins.installed]
openapi = { version = "1.0.0", source = "ggen-plugins/openapi" }
graphql = { version = "0.9.0", source = "git+https://github.com/ggen/graphql-plugin" }

# Plugin configuration
[plugins.config.openapi]
spec_file = "openapi.yaml"
output_dir = "src/api"
generate_models = true
generate_routes = true

# Plugin hooks (extend ggen lifecycle)
[plugins.hooks]
openapi = ["before_build", "on_spec_change"]
graphql = ["after_generate"]

# Plugin permissions (security)
[plugins.permissions.openapi]
filesystem = ["read:openapi.yaml", "write:src/api/**"]
network = ["https://api.example.org"]
exec = ["rustfmt"]
```

**Design rationale:**
- **Discoverable**: Auto-discover plugins from configured paths
- **Versioned**: Plugins have semantic versions and sources
- **Configurable**: Each plugin has its own config section
- **Secure**: Permission model restricts plugin capabilities
- **Lifecycle integration**: Plugins hook into build lifecycle

### 9. Profiles Section (Environment Overrides)

```toml
[profiles]
# Default profile (dev)
default = "dev"

# Development profile
[profiles.dev]
optimization = "debug"
debug_assertions = true
overflow_checks = true

# Override dependencies
[profiles.dev.dependencies]
tracing-subscriber = { version = "0.3", features = ["env-filter"] }

# Override templates
[profiles.dev.templates.vars]
log_level = "debug"

# Production profile
[profiles.production]
optimization = "release"
debug_assertions = false
overflow_checks = false
lto = "thin"
strip = true

[profiles.production.templates.vars]
log_level = "info"
telemetry_enabled = true

# Override ontology checks for production
[profiles.production.ontology.constitution]
enforce_strict = true
fail_on_warning = true

# Testing profile
[profiles.test]
extends = "dev"
code_coverage = true
test_threads = 4

[profiles.test.lifecycle.hooks]
before_test = "ggen setup-test-db"
after_test = "ggen cleanup-test-db"
```

**Design rationale:**
- **Environment-specific**: Different configs for dev/prod/test
- **Inheritance**: Profiles can extend other profiles
- **Override everything**: Dependencies, templates, lifecycle, ontology
- **Cargo-compatible**: Aligns with Cargo profile model

## Integration with ggen Architecture

### Graph Integration

```rust
// crates/ggen-config/src/graph_integration.rs

/// Convert ggen.toml configuration to RDF graph
pub async fn config_to_graph(config: &GgenConfig) -> Result<Graph> {
    let graph = Graph::new()?;

    // Project metadata as RDF
    graph.insert_turtle(&format!(r#"
        @prefix ggen: <http://ggen.org/ontology/> .
        @prefix proj: <{}> .

        proj: a ggen:Project ;
            ggen:name "{}" ;
            ggen:version "{}" ;
            ggen:language "{}" .
    "#, config.project.uri, config.project.name,
        config.project.version, config.project.language))?;

    // Dependencies as graph
    for (name, dep) in &config.dependencies {
        graph.insert_turtle(&format!(r#"
            proj: ggen:dependsOn proj:{} .
            proj:{} a ggen:Dependency ;
                ggen:version "{}" .
        "#, name, name, dep.version))?;
    }

    Ok(graph)
}

/// Query configuration graph with SPARQL
pub async fn query_config(
    config: &GgenConfig,
    sparql: &str
) -> Result<Vec<BTreeMap<String, String>>> {
    let graph = config_to_graph(config).await?;
    graph.query_to_map(sparql)
}
```

### Template Integration

```rust
// crates/ggen-config/src/template_integration.rs

/// Resolve templates with inheritance and composition
pub async fn resolve_template(
    config: &GgenConfig,
    target_file: &str
) -> Result<Template> {
    // Check for template composition
    if let Some(sources) = config.templates.compose.get(target_file) {
        return compose_templates(config, sources).await;
    }

    // Check for template inheritance
    if let Some(base) = config.templates.extends.get(target_file) {
        return extend_template(config, base, target_file).await;
    }

    // Load direct template
    load_template(config, target_file).await
}

/// Compose multiple templates into single output
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

### Ontology Integration

```rust
// crates/ggen-config/src/ontology_integration.rs

/// Validate configuration against ontology
pub async fn validate_ontology(config: &GgenConfig) -> Result<()> {
    let graph = config_to_graph(config).await?;

    // Load ontology files
    for file in &config.ontology.files {
        graph.insert_from_file(file, RdfFormat::Turtle)?;
    }

    // Insert inline RDF
    if let Some(inline) = &config.ontology.inline {
        graph.insert_turtle(inline)?;
    }

    // Run constitution checks
    let runtime = SigmaRuntime::new(graph);

    for check_name in &config.ontology.constitution.checks {
        runtime.run_check(check_name).await?;
    }

    // Run custom SPARQL checks
    for (name, query) in &config.ontology.constitution.custom {
        let result = graph.query_ask(query)?;
        if result {
            return Err(Error::new(&format!(
                "Constitution check '{}' failed", name
            )));
        }
    }

    Ok(())
}
```

### Plugin Integration

```rust
// crates/ggen-config/src/plugin_integration.rs

/// Plugin trait for extending ggen
#[async_trait]
pub trait Plugin: Send + Sync {
    /// Plugin name
    fn name(&self) -> &str;

    /// Plugin version
    fn version(&self) -> &str;

    /// Initialize plugin with configuration
    async fn init(&mut self, config: PluginConfig) -> Result<()>;

    /// Lifecycle hooks this plugin responds to
    fn hooks(&self) -> Vec<LifecycleHook>;

    /// Execute plugin on lifecycle hook
    async fn execute(&self, hook: LifecycleHook, context: &Context) -> Result<()>;
}

/// Plugin manager
pub struct PluginManager {
    plugins: Vec<Box<dyn Plugin>>,
    config: GgenConfig,
}

impl PluginManager {
    /// Discover and load plugins
    pub async fn discover(config: &GgenConfig) -> Result<Self> {
        let mut plugins = Vec::new();

        for (name, plugin_config) in &config.plugins.installed {
            let plugin = load_plugin(name, plugin_config).await?;
            plugins.push(plugin);
        }

        Ok(Self {
            plugins,
            config: config.clone(),
        })
    }

    /// Execute all plugins for a lifecycle hook
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

## Rust Data Structures

```rust
// crates/ggen-config/src/types.rs

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::PathBuf;

/// Root configuration structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GgenConfig {
    pub project: ProjectConfig,
    #[serde(default)]
    pub workspace: Option<WorkspaceConfig>,
    #[serde(default)]
    pub graph: GraphConfig,
    #[serde(default)]
    pub ontology: OntologyConfig,
    #[serde(default)]
    pub templates: TemplateConfig,
    #[serde(default)]
    pub generators: GeneratorConfig,
    #[serde(default)]
    pub lifecycle: LifecycleConfig,
    #[serde(default)]
    pub plugins: PluginConfig,
    #[serde(default)]
    pub profiles: BTreeMap<String, ProfileConfig>,
    #[serde(default)]
    pub dependencies: BTreeMap<String, Dependency>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectConfig {
    pub name: String,
    pub version: String,
    #[serde(default)]
    pub description: Option<String>,
    #[serde(default)]
    pub authors: Vec<String>,
    #[serde(default)]
    pub license: Option<String>,
    #[serde(default)]
    pub edition: Option<String>,
    #[serde(default = "default_project_type")]
    pub r#type: String,
    #[serde(default = "default_language")]
    pub language: String,
    #[serde(default)]
    pub uri: Option<String>,
    #[serde(default)]
    pub namespace: Option<String>,
    #[serde(default)]
    pub extends: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkspaceConfig {
    pub members: Vec<String>,
    #[serde(default)]
    pub exclude: Vec<String>,
    #[serde(default)]
    pub dependencies: BTreeMap<String, Dependency>,
    #[serde(default)]
    pub graph: GraphConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct GraphConfig {
    #[serde(default = "default_strategy")]
    pub strategy: String,
    #[serde(default = "default_conflict_resolution")]
    pub conflict_resolution: String,
    #[serde(default)]
    pub queries: BTreeMap<String, String>,
    #[serde(default)]
    pub features: BTreeMap<String, Vec<String>>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct OntologyConfig {
    #[serde(default)]
    pub files: Vec<PathBuf>,
    #[serde(default)]
    pub inline: Option<String>,
    #[serde(default)]
    pub shapes: Vec<PathBuf>,
    #[serde(default)]
    pub constitution: ConstitutionConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ConstitutionConfig {
    #[serde(default)]
    pub checks: Vec<String>,
    #[serde(default)]
    pub custom: BTreeMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct TemplateConfig {
    #[serde(default)]
    pub paths: Vec<PathBuf>,
    #[serde(default)]
    pub vars: BTreeMap<String, String>,
    #[serde(default)]
    pub extends: BTreeMap<String, String>,
    #[serde(default)]
    pub compose: BTreeMap<String, Vec<String>>,
    #[serde(default)]
    pub guards: BTreeMap<String, String>,
    #[serde(default)]
    pub queries: BTreeMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct GeneratorConfig {
    #[serde(default)]
    pub registry: Option<String>,
    #[serde(default)]
    pub installed: BTreeMap<String, GeneratorRef>,
    #[serde(default)]
    pub pipeline: Vec<Pipeline>,
    #[serde(default)]
    pub hooks: GeneratorHooks,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GeneratorRef {
    pub version: String,
    #[serde(default)]
    pub registry: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Pipeline {
    pub name: String,
    #[serde(default)]
    pub description: Option<String>,
    #[serde(default)]
    pub inputs: Vec<PathBuf>,
    pub steps: Vec<PipelineStep>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "action")]
pub enum PipelineStep {
    #[serde(rename = "template")]
    Template { template: String },
    #[serde(rename = "parse")]
    Parse { parser: String },
    #[serde(rename = "transform")]
    Transform { query: PathBuf },
    #[serde(rename = "exec")]
    Exec { command: String },
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct GeneratorHooks {
    #[serde(default)]
    pub before_generate: Option<String>,
    #[serde(default)]
    pub after_generate: Option<String>,
    #[serde(default)]
    pub on_error: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct LifecycleConfig {
    #[serde(default)]
    pub phases: Vec<String>,
    #[serde(default)]
    pub hooks: BTreeMap<String, Vec<String>>,
    #[serde(default)]
    pub tasks: BTreeMap<String, Task>,
    #[serde(default)]
    pub parallel: BTreeMap<String, Vec<String>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Task {
    #[serde(default)]
    pub description: Option<String>,
    #[serde(default)]
    pub dependencies: Vec<String>,
    #[serde(default)]
    pub command: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct PluginConfig {
    #[serde(default)]
    pub paths: Vec<PathBuf>,
    #[serde(default)]
    pub installed: BTreeMap<String, PluginRef>,
    #[serde(default)]
    pub config: BTreeMap<String, BTreeMap<String, serde_json::Value>>,
    #[serde(default)]
    pub hooks: BTreeMap<String, Vec<String>>,
    #[serde(default)]
    pub permissions: BTreeMap<String, PluginPermissions>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PluginRef {
    pub version: String,
    pub source: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct PluginPermissions {
    #[serde(default)]
    pub filesystem: Vec<String>,
    #[serde(default)]
    pub network: Vec<String>,
    #[serde(default)]
    pub exec: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProfileConfig {
    #[serde(default)]
    pub extends: Option<String>,
    #[serde(default)]
    pub optimization: Option<String>,
    #[serde(default)]
    pub debug_assertions: Option<bool>,
    #[serde(default)]
    pub overflow_checks: Option<bool>,
    #[serde(default)]
    pub lto: Option<String>,
    #[serde(default)]
    pub strip: Option<bool>,
    #[serde(default)]
    pub dependencies: BTreeMap<String, Dependency>,
    #[serde(default)]
    pub templates: TemplateConfig,
    #[serde(default)]
    pub lifecycle: LifecycleConfig,
    #[serde(default)]
    pub ontology: OntologyConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Dependency {
    #[serde(default)]
    pub version: Option<String>,
    #[serde(default)]
    pub features: Vec<String>,
    #[serde(default)]
    pub optional: bool,
    #[serde(default)]
    pub default_features: bool,
}

// Default functions
fn default_project_type() -> String { "auto".to_string() }
fn default_language() -> String { "auto".to_string() }
fn default_strategy() -> String { "smart".to_string() }
fn default_conflict_resolution() -> String { "newest".to_string() }
```

## Module Structure

```
crates/ggen-config/
├── Cargo.toml
├── src/
│   ├── lib.rs                      # Public API
│   ├── types.rs                    # Data structures
│   ├── parser.rs                   # TOML parsing
│   ├── validator.rs                # Configuration validation
│   ├── normalizer.rs               # Smart defaults & inference
│   ├── graph_integration.rs        # Graph conversion
│   ├── template_integration.rs     # Template resolution
│   ├── ontology_integration.rs     # Ontology validation
│   ├── plugin_integration.rs       # Plugin system
│   ├── profile_resolver.rs         # Profile merging
│   └── error.rs                    # Error types
└── tests/
    ├── parser_tests.rs
    ├── validator_tests.rs
    ├── integration_tests.rs
    └── fixtures/
        └── ggen.toml
```

## Key Innovations

### 1. Graph-Based Dependency Resolution
Unlike flat dependency lists, ggen treats dependencies as an RDF graph:
- **SPARQL queries**: Find transitive dependencies, critical paths, conflicts
- **Graph algorithms**: Shortest path, cycle detection, topological sort
- **Semantic versioning**: Leverage RDF reasoning for version constraints

### 2. Ontology-Aware Configuration
Configuration validates against OWL ontologies:
- **SHACL validation**: Schema validation for configuration data
- **Constitution checks**: Hard invariants enforced at config time
- **Inference**: Derive implicit configuration from ontology rules

### 3. Template Composition
Templates are first-class citizens:
- **Inheritance**: Templates extend base templates
- **Composition**: Merge multiple templates into single output
- **Guards**: Conditional rendering based on SPARQL queries
- **Variables**: Expose graph queries to templates via Tera

### 4. Plugin System
Secure, discoverable plugin architecture:
- **Auto-discovery**: Scan configured paths for plugins
- **Permissions**: Restrict filesystem, network, and exec access
- **Lifecycle hooks**: Plugins hook into build phases
- **Configuration**: Each plugin has isolated config section

### 5. Profile System
Environment-specific overrides:
- **Inheritance**: Profiles extend other profiles
- **Complete override**: Dependencies, templates, lifecycle, ontology
- **Cargo-compatible**: Aligns with existing Rust tooling

## Security Considerations

1. **Plugin sandboxing**: Plugins run with restricted permissions
2. **SPARQL injection prevention**: Parameterized queries only
3. **Path traversal protection**: Validate all file paths
4. **Dependency pinning**: Lock file for reproducible builds
5. **Signature verification**: Verify plugin and template signatures

## Migration Path

### From Cargo.toml
```toml
# Cargo.toml
[package]
name = "my-crate"
version = "1.0.0"

[dependencies]
serde = "1.0"
```

Becomes:
```toml
# ggen.toml
[project]
name = "my-crate"
version = "1.0.0"
language = "rust"

[dependencies]
serde = { version = "1.0" }
```

### From package.json
```json
{
  "name": "my-package",
  "version": "1.0.0",
  "dependencies": {
    "express": "^4.18.0"
  }
}
```

Becomes:
```toml
# ggen.toml
[project]
name = "my-package"
version = "1.0.0"
language = "typescript"

[dependencies]
express = { version = "^4.18.0" }
```

## Performance Optimizations

1. **Lazy loading**: Load ontology and templates on-demand
2. **Caching**: Cache parsed TOML, SPARQL queries, and graph results
3. **Parallel resolution**: Resolve dependencies in parallel
4. **Incremental builds**: Only regenerate changed templates
5. **Graph indexing**: Index frequently-queried graph patterns

## Future Extensions

1. **Remote ontologies**: Load ontologies from HTTP(S)
2. **Schema registry**: Central registry for project schemas
3. **AI-assisted configuration**: Suggest configuration improvements
4. **Visual editor**: GUI for editing ggen.toml
5. **Migration tools**: Auto-convert from Cargo.toml, package.json, etc.

## Conclusion

The `ggen.toml` architecture represents a paradigm shift in project configuration:
- **Knowledge graphs over key-value pairs**
- **Semantic queries over static lookups**
- **Template composition over copy-paste**
- **Ontology validation over manual checks**

This hyper-advanced system leverages ggen's unique strengths to provide a configuration experience that is both more powerful and easier to use than traditional package managers.
