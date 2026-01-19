# Configuration Management Guide

## ggen.toml Structure

Projects declare configuration in `ggen.toml`:

```toml
[project]
name = "my-api"
version = "0.1.0"
description = "My REST API"
authors = ["team@example.com"]

[generation]
framework = "axum"
language = "rust"
template = "rust-api"
output_dir = "./src"

[ontology]
source = "domain.ttl"
validate = true
strict_mode = false

[marketplace]
registry_url = "https://marketplace.ggen.io"
packages = [
    "base-rust",
    "error-handling",
    "database-postgres",
]

[generation.features]
auth = true
database = true
testing = true
documentation = true

[generation.options]
async_runtime = "tokio"
database_type = "postgresql"
auth_type = "jwt"
api_version = "v1"

[performance]
timeout_seconds = 30
max_memory_mb = 256
parallel_jobs = 4

[validation]
enforce_naming_conventions = true
require_tests = true
minimum_coverage = 80
```

## Project Hierarchy: Workspace → Project → Package

Configuration applies at multiple levels with inheritance:

```
ggen/                          (Workspace level)
├── ggen.toml                 # Global defaults
│
├── my-project/               (Project level)
│   └── ggen.toml             # Overrides workspace settings
│
├── crates/
│   ├── ggen-core/            (Package level)
│   │   └── gpack.toml        # Package-specific configuration
│   │
│   ├── ggen-cli/
│   │   └── gpack.toml
│   │
│   └── ggen-domain/
│       └── gpack.toml
```

### Inheritance Rules

Settings inherit from broader to narrower scope:

```
Workspace (ggen.toml)
    ↓
Project (project-ggen.toml) - Overrides workspace
    ↓
Package (crate/gpack.toml) - Overrides project
```

Example:

```toml
# Workspace ggen.toml
[generation]
timeout_seconds = 30
framework = "axum"

# Project-level override
# my-project/ggen.toml
[generation]
framework = "actix"  # Override: use actix instead
# timeout_seconds inherits 30 from workspace

# Package-level override
# crates/ggen-core/gpack.toml
[generation]
async_runtime = "tokio"  # Package-specific setting
# Inherits framework=actix, timeout_seconds=30
```

## gpack.toml for Individual Packages

Each crate declares `gpack.toml` (package-level configuration):

```toml
[package]
name = "ggen-core"
version = "0.4.0"
authors = ["team@ggen.io"]
description = "Core RDF and template engine"

[metadata]
category = "core"
responsible_team = "platform"
contact = "platform@ggen.io"

[features]
required = ["std"]
optional = ["distributed", "metrics"]

[performance]
timeout_seconds = 15
max_memory_mb = 128

[testing]
unit_timeout_seconds = 10
integration_timeout_seconds = 30
minimum_coverage = 85

[validation]
enforce_naming = true
require_docs = true
require_tests = true
clippy_level = "warn"  # "warn" or "deny"

[build]
incremental_build_target = 2  # seconds SLA
test_target = 10  # seconds SLA
bench_target = 60  # seconds SLA

[dependencies]
optional = ["serde_json", "tokio", "oxigraph"]
```

## Makefile.toml Lifecycle Tasks

Build configuration via `Makefile.toml`:

```toml
[tasks.setup]
description = "Setup development environment"
dependencies = ["install-deps", "validate-config"]
script = '''
mkdir -p ./tmp
echo "Setup complete!"
'''

[tasks.install-deps]
command = "cargo"
args = ["fetch", "--all"]
description = "Fetch dependencies"

[tasks.validate-config]
script = '''
echo "Validating configuration..."
cargo make validate-ggen-toml
cargo make validate-package-files
'''

[tasks.generate]
description = "Generate code from ontology"
dependencies = ["setup"]
script = '''
cargo run --bin ggen -- \
    --config ggen.toml \
    --ontology domain.ttl \
    --template rust-api \
    --output ./src
'''

[tasks.pre-build]
dependencies = ["setup", "validate-config"]
description = "Run before build"

[tasks.post-generate]
script = '''
echo "Code generation complete!"
cargo make fmt
cargo make lint
'''

[tasks.cleanup]
script = '''
rm -rf ./tmp
cargo clean
'''
```

## Configuration Parsing

Load and validate configuration:

```rust
use serde::Deserialize;
use std::fs;
use toml;

#[derive(Deserialize, Debug)]
pub struct GenerationConfig {
    pub project: ProjectConfig,
    pub generation: GenerationOptions,
    pub ontology: OntologyConfig,
    pub marketplace: MarketplaceConfig,
    pub validation: ValidationConfig,
}

#[derive(Deserialize, Debug)]
pub struct ProjectConfig {
    pub name: String,
    pub version: String,
    pub authors: Vec<String>,
}

#[derive(Deserialize, Debug)]
pub struct GenerationOptions {
    pub framework: String,
    pub language: String,
    pub template: String,
    pub output_dir: String,
    #[serde(default)]
    pub features: std::collections::HashMap<String, bool>,
}

#[derive(Deserialize, Debug)]
pub struct OntologyConfig {
    pub source: String,
    #[serde(default)]
    pub validate: bool,
    #[serde(default)]
    pub strict_mode: bool,
}

#[derive(Deserialize, Debug)]
pub struct MarketplaceConfig {
    pub registry_url: String,
    pub packages: Vec<String>,
}

#[derive(Deserialize, Debug)]
pub struct ValidationConfig {
    pub enforce_naming_conventions: bool,
    pub require_tests: bool,
    pub minimum_coverage: u32,
}

// Load configuration
pub fn load_config(config_path: &str) -> Result<GenerationConfig> {
    let contents = fs::read_to_string(config_path)
        .map_err(|e| format!("Failed to read config: {}", e))?;

    toml::from_str(&contents)
        .map_err(|e| format!("Invalid config: {}", e))
}

// Validate configuration
pub fn validate_config(config: &GenerationConfig) -> Result<()> {
    if config.project.name.is_empty() {
        return Err("Project name cannot be empty".into());
    }

    if !fs::metadata(&config.ontology.source).is_ok() {
        return Err(format!("Ontology file not found: {}", config.ontology.source));
    }

    // Verify marketplace packages exist
    for package in &config.marketplace.packages {
        if package.is_empty() {
            return Err("Empty package name in marketplace config".into());
        }
    }

    Ok(())
}
```

## Environment Variable Overrides

Configuration supports environment variable overrides:

```rust
pub fn load_config_with_env() -> Result<GenerationConfig> {
    let mut config = load_config("ggen.toml")?;

    // Override from environment variables
    if let Ok(framework) = std::env::var("GGEN_FRAMEWORK") {
        config.generation.framework = framework;
    }

    if let Ok(output_dir) = std::env::var("GGEN_OUTPUT_DIR") {
        config.generation.output_dir = output_dir;
    }

    if let Ok(registry) = std::env::var("GGEN_REGISTRY_URL") {
        config.marketplace.registry_url = registry;
    }

    Ok(config)
}
```

Usage:

```bash
# Override framework via environment
GGEN_FRAMEWORK=actix cargo run --bin ggen

# Override multiple settings
GGEN_FRAMEWORK=actix GGEN_OUTPUT_DIR=/tmp/output ggen generate
```

## Configuration Validation

```rust
pub fn validate_generation_config(config: &GenerationOptions) -> Result<()> {
    // Validate framework
    let valid_frameworks = vec!["axum", "actix", "rocket", "warp"];
    if !valid_frameworks.contains(&config.framework.as_str()) {
        return Err(format!("Unknown framework: {}", config.framework));
    }

    // Validate language
    let valid_languages = vec!["rust", "typescript", "python"];
    if !valid_languages.contains(&config.language.as_str()) {
        return Err(format!("Unknown language: {}", config.language));
    }

    // Validate template exists
    if !template_exists(&config.template) {
        return Err(format!("Template not found: {}", config.template));
    }

    // Validate output directory is writable
    if !is_writable(&config.output_dir) {
        return Err(format!("Output directory not writable: {}", config.output_dir));
    }

    Ok(())
}
```

## Configuration Testing

Test configuration loading and validation:

```rust
#[test]
fn test_load_valid_config() {
    let config = load_config("test_fixtures/valid_config.toml").unwrap();
    assert_eq!(config.project.name, "test-project");
    assert_eq!(config.generation.framework, "axum");
}

#[test]
fn test_invalid_config_fails() {
    let result = load_config("test_fixtures/invalid_config.toml");
    assert!(result.is_err());
}

#[test]
fn test_config_validation() {
    let config = load_config("test_fixtures/valid_config.toml").unwrap();
    let result = validate_config(&config);
    assert!(result.is_ok());
}

#[test]
fn test_env_override() {
    std::env::set_var("GGEN_FRAMEWORK", "actix");
    let config = load_config_with_env().unwrap();
    assert_eq!(config.generation.framework, "actix");
}
```

## Critical Rules

1. **ALWAYS validate configuration** - On load, before use
2. **DECLARE defaults explicitly** - In schema, not code
3. **SUPPORT environment overrides** - For CI/CD flexibility
4. **VALIDATE inheritance** - Verify lower scopes override correctly
5. **TEST all config paths** - Valid and invalid configs
6. **DOCUMENT required fields** - Especially breaking changes
7. **VERSION configuration schema** - Track changes to structure
8. **PROVIDE helpful error messages** - What's missing? What's invalid?

---

## Quick Reference

```toml
# Basic ggen.toml
[project]
name = "my-api"
version = "0.1.0"

[generation]
framework = "axum"
template = "rust-api"

[ontology]
source = "domain.ttl"
validate = true

[marketplace]
packages = ["base-rust"]
```

```rust
// Load and validate
let config = load_config("ggen.toml")?;
validate_config(&config)?;

// Use in code
let generation_options = config.generation;
```
