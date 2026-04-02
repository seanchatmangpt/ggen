<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Path Validation Migration Guide](#path-validation-migration-guide)
  - [Overview](#overview)
  - [Migration Checklist](#migration-checklist)
    - [Step 1: Identify File Operations](#step-1-identify-file-operations)
    - [Step 2: Categorize by Operation Type](#step-2-categorize-by-operation-type)
    - [Step 3: Create Validators](#step-3-create-validators)
    - [Step 4: Migrate File Operations](#step-4-migrate-file-operations)
      - [Before (Unsafe)](#before-unsafe)
      - [After (Safe)](#after-safe)
    - [Step 5: Update Function Signatures](#step-5-update-function-signatures)
    - [Step 6: Handle Error Messages](#step-6-handle-error-messages)
  - [Module-Specific Migration](#module-specific-migration)
    - [Template Loading (ggen-domain/src/template/)](#template-loading-ggen-domainsrctemplate)
    - [RDF Loading (ggen-ontology-core/src/triple_store.rs)](#rdf-loading-ggen-ontology-coresrctriple_storers)
    - [Code Generation (ggen-core/src/codegen/executor.rs)](#code-generation-ggen-coresrccodegenexecutorrs)
    - [Config Loading (ggen-config/src/parser.rs)](#config-loading-ggen-configsrcparserrs)
  - [Testing Migration](#testing-migration)
    - [Before Migration Tests](#before-migration-tests)
    - [After Migration Tests](#after-migration-tests)
  - [Common Patterns](#common-patterns)
    - [Pattern 1: Struct with Validator](#pattern-1-struct-with-validator)
    - [Pattern 2: Lazy Validator](#pattern-2-lazy-validator)
    - [Pattern 3: Validator Factory](#pattern-3-validator-factory)
  - [Rollout Plan](#rollout-plan)
  - [Monitoring](#monitoring)
  - [Success Metrics](#success-metrics)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Path Validation Migration Guide

## Overview

This guide provides step-by-step instructions for migrating existing code to use `PathValidator` for secure file operations.

## Migration Checklist

### Step 1: Identify File Operations

Find all file operations that need validation:

```bash
# Find all fs::read operations
grep -r "fs::read" crates/ | grep -v "test" | wc -l

# Find all fs::write operations
grep -r "fs::write" crates/ | grep -v "test" | wc -l

# Find all PathBuf::from operations
grep -r "PathBuf::from" crates/ | grep -v "test" | wc -l

# Find all unsafe path operations
grep -r "Path::new" crates/ | grep -v "test" | wc -l
```

### Step 2: Categorize by Operation Type

Organize file operations by category:

1. **Template Loading** - `.tera`, `.tmpl` files
2. **RDF/Ontology Loading** - `.ttl`, `.rdf`, `.xml`, `.n3` files
3. **Code Generation Output** - `.rs`, `.ts`, `.py`, `.go`, etc.
4. **Configuration Loading** - `.toml`, `.yaml`, `.json` files
5. **Audit/Cache Files** - `.json`, `.log` files

### Step 3: Create Validators

Create appropriate validators for each category:

```rust
use ggen_utils::path_validator::PathValidator;
use std::path::Path;

// Template validator
fn create_template_validator(workspace: &Path) -> PathValidator {
    PathValidator::new(workspace)
        .with_allowed_extensions(vec!["tera", "tmpl"])
        .with_max_depth(5)
}

// RDF validator
fn create_rdf_validator(workspace: &Path) -> PathValidator {
    PathValidator::new(workspace)
        .with_allowed_extensions(vec!["ttl", "rdf", "xml", "n3"])
        .with_max_depth(10)
}

// Output validator
fn create_output_validator(workspace: &Path) -> PathValidator {
    PathValidator::new(workspace)
        .with_allowed_extensions(vec!["rs", "ts", "py", "go", "java"])
        .with_max_depth(10)
}

// Config validator
fn create_config_validator(workspace: &Path) -> PathValidator {
    PathValidator::new(workspace)
        .with_allowed_extensions(vec!["toml", "yaml", "json"])
        .with_max_depth(5)
}
```

### Step 4: Migrate File Operations

#### Before (Unsafe)

```rust
// ❌ DANGEROUS - No validation
fn load_template(path: &str) -> Result<String> {
    Ok(std::fs::read_to_string(path)?)
}

fn write_output(path: &str, content: &str) -> Result<()> {
    std::fs::write(path, content)?;
    Ok(())
}

fn load_ontology(path: &Path) -> Result<String> {
    let content = std::fs::read_to_string(path)?;
    Ok(content)
}
```

#### After (Safe)

```rust
// ✅ SAFE - Full validation
use ggen_utils::path_validator::{PathValidator, SafePath};
use ggen_utils::error::Result;

fn load_template(path: &str, workspace: &Path) -> Result<String> {
    let validator = create_template_validator(workspace);
    let safe_path = validator.validate(path)?;
    Ok(std::fs::read_to_string(safe_path.as_path())?)
}

fn write_output(path: &str, content: &str, workspace: &Path) -> Result<()> {
    let validator = create_output_validator(workspace);
    let safe_path = validator.validate(path)?;

    // Create parent directories if needed
    if let Some(parent) = safe_path.as_path().parent() {
        std::fs::create_dir_all(parent)?;
    }

    std::fs::write(safe_path.as_path(), content)?;
    Ok(())
}

fn load_ontology(path: &Path, workspace: &Path) -> Result<String> {
    let validator = create_rdf_validator(workspace);
    let safe_path = validator.validate(path)?;
    Ok(std::fs::read_to_string(safe_path.as_path())?)
}
```

### Step 5: Update Function Signatures

Add workspace parameter to functions that need validation:

```rust
// Before
pub fn generate_code(template: &str) -> Result<String>

// After
pub fn generate_code(template: &str, workspace: &Path) -> Result<String>
```

Or pass validator directly:

```rust
// Alternative: Pass validator
pub fn generate_code(template: &str, validator: &PathValidator) -> Result<String>
```

Or use a context struct:

```rust
// Best: Use context struct
pub struct GeneratorContext {
    workspace: PathBuf,
    template_validator: PathValidator,
    output_validator: PathValidator,
}

pub fn generate_code(&self, template: &str) -> Result<String>
```

### Step 6: Handle Error Messages

Provide clear error messages for validation failures:

```rust
fn load_template(path: &str, workspace: &Path) -> Result<String> {
    let validator = create_template_validator(workspace);

    let safe_path = validator.validate(path).map_err(|e| {
        Error::new(&format!(
            "Template validation failed for '{}': {}",
            path, e
        ))
    })?;

    std::fs::read_to_string(safe_path.as_path())
        .map_err(|e| Error::new(&format!(
            "Failed to read template '{}': {}",
            safe_path, e
        )))
}
```

## Module-Specific Migration

### Template Loading (ggen-domain/src/template/)

```rust
// File: ggen-domain/src/template/generate.rs

// Before
pub fn load_template(path: &str) -> Result<String> {
    std::fs::read_to_string(path)
        .map_err(|e| Error::new(&format!("Failed to read template: {}", e)))
}

// After
use ggen_utils::path_validator::PathValidator;

pub fn load_template(path: &str, workspace: &Path) -> Result<String> {
    let validator = PathValidator::new(workspace)
        .with_allowed_extensions(vec!["tera", "tmpl"])
        .with_max_depth(5);

    let safe_path = validator.validate(path)
        .map_err(|e| Error::new(&format!("Template path validation failed: {}", e)))?;

    std::fs::read_to_string(safe_path.as_path())
        .map_err(|e| Error::new(&format!("Failed to read template: {}", e)))
}
```

### RDF Loading (ggen-ontology-core/src/triple_store.rs)

```rust
// File: ggen-ontology-core/src/triple_store.rs

// Before
pub fn load_ontology(&mut self, path: &Path) -> Result<()> {
    let content = std::fs::read_to_string(path)?;
    self.parse_turtle(&content)?;
    Ok(())
}

// After
use ggen_utils::path_validator::PathValidator;

pub fn load_ontology(&mut self, path: &Path, workspace: &Path) -> Result<()> {
    let validator = PathValidator::new(workspace)
        .with_allowed_extensions(vec!["ttl", "rdf", "xml", "n3"])
        .with_max_depth(10);

    let safe_path = validator.validate(path)
        .map_err(|e| Error::new(&format!("Ontology path validation failed: {}", e)))?;

    let content = std::fs::read_to_string(safe_path.as_path())?;
    self.parse_turtle(&content)?;
    Ok(())
}
```

### Code Generation (ggen-core/src/codegen/executor.rs)

```rust
// File: ggen-core/src/codegen/executor.rs

// Before
pub fn write_file(&self, path: &str, content: &str) -> Result<()> {
    std::fs::write(path, content)?;
    Ok(())
}

// After
use ggen_utils::path_validator::PathValidator;

pub fn write_file(&self, path: &str, content: &str, workspace: &Path) -> Result<()> {
    let validator = PathValidator::new(workspace)
        .with_allowed_extensions(vec!["rs", "ts", "py", "go", "java"])
        .with_max_depth(10);

    let safe_path = validator.validate(path)
        .map_err(|e| Error::new(&format!("Output path validation failed: {}", e)))?;

    // Create parent directories
    if let Some(parent) = safe_path.as_path().parent() {
        std::fs::create_dir_all(parent)?;
    }

    std::fs::write(safe_path.as_path(), content)?;
    Ok(())
}
```

### Config Loading (ggen-config/src/parser.rs)

```rust
// File: ggen-config/src/parser.rs

// Before
pub fn load_config(path: &Path) -> Result<Config> {
    let content = std::fs::read_to_string(path)?;
    let config: Config = toml::from_str(&content)?;
    Ok(config)
}

// After
use ggen_utils::path_validator::PathValidator;

pub fn load_config(path: &Path, workspace: &Path) -> Result<Config> {
    let validator = PathValidator::new(workspace)
        .with_allowed_extensions(vec!["toml", "yaml", "json"])
        .with_max_depth(5);

    let safe_path = validator.validate(path)
        .map_err(|e| Error::new(&format!("Config path validation failed: {}", e)))?;

    let content = std::fs::read_to_string(safe_path.as_path())?;
    let config: Config = toml::from_str(&content)?;
    Ok(config)
}
```

## Testing Migration

### Before Migration Tests

```rust
#[test]
fn test_load_template() {
    let path = "templates/example.tera";
    let result = load_template(path);
    assert!(result.is_ok());
}
```

### After Migration Tests

```rust
#[test]
fn test_load_template_with_validation() {
    let workspace = tempdir().unwrap();
    let template_path = workspace.path().join("templates/example.tera");

    // Create template file
    std::fs::create_dir_all(template_path.parent().unwrap()).unwrap();
    std::fs::write(&template_path, "content").unwrap();

    // Test with valid path
    let result = load_template("templates/example.tera", workspace.path());
    assert!(result.is_ok());
}

#[test]
fn test_load_template_blocks_traversal() {
    let workspace = tempdir().unwrap();

    // Test path traversal is blocked
    let result = load_template("../../../etc/passwd", workspace.path());
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("traversal"));
}

#[test]
fn test_load_template_validates_extension() {
    let workspace = tempdir().unwrap();

    // Test invalid extension is blocked
    let result = load_template("malware.exe", workspace.path());
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("extension"));
}
```

## Common Patterns

### Pattern 1: Struct with Validator

```rust
pub struct TemplateEngine {
    workspace: PathBuf,
    validator: PathValidator,
}

impl TemplateEngine {
    pub fn new(workspace: PathBuf) -> Self {
        let validator = PathValidator::new(&workspace)
            .with_allowed_extensions(vec!["tera", "tmpl"])
            .with_max_depth(5);

        Self { workspace, validator }
    }

    pub fn load(&self, path: &str) -> Result<String> {
        let safe_path = self.validator.validate(path)?;
        Ok(std::fs::read_to_string(safe_path.as_path())?)
    }
}
```

### Pattern 2: Lazy Validator

```rust
use std::sync::OnceLock;

static TEMPLATE_VALIDATOR: OnceLock<PathValidator> = OnceLock::new();

pub fn get_template_validator(workspace: &Path) -> &PathValidator {
    TEMPLATE_VALIDATOR.get_or_init(|| {
        PathValidator::new(workspace)
            .with_allowed_extensions(vec!["tera", "tmpl"])
            .with_max_depth(5)
    })
}
```

### Pattern 3: Validator Factory

```rust
pub struct ValidatorFactory;

impl ValidatorFactory {
    pub fn for_templates(workspace: &Path) -> PathValidator {
        PathValidator::new(workspace)
            .with_allowed_extensions(vec!["tera", "tmpl"])
            .with_max_depth(5)
    }

    pub fn for_rdf(workspace: &Path) -> PathValidator {
        PathValidator::new(workspace)
            .with_allowed_extensions(vec!["ttl", "rdf", "xml", "n3"])
            .with_max_depth(10)
    }

    pub fn for_output(workspace: &Path) -> PathValidator {
        PathValidator::new(workspace)
            .with_allowed_extensions(vec!["rs", "ts", "py", "go", "java"])
            .with_max_depth(10)
    }
}
```

## Rollout Plan

1. **Week 1**: Template loading validation (highest risk)
2. **Week 2**: RDF/ontology loading validation
3. **Week 3**: Code generation output validation
4. **Week 4**: Configuration and other file operations
5. **Week 5**: Testing, security audit, documentation
6. **Week 6**: Production deployment with monitoring

## Monitoring

Add logging for validation failures:

```rust
fn load_template(path: &str, workspace: &Path) -> Result<String> {
    let validator = create_template_validator(workspace);

    match validator.validate(path) {
        Ok(safe_path) => {
            log::debug!("Template validated: {}", safe_path);
            std::fs::read_to_string(safe_path.as_path())
                .map_err(|e| Error::new(&format!("Read failed: {}", e)))
        }
        Err(e) => {
            log::warn!("Template validation failed for '{}': {}", path, e);
            Err(e.into())
        }
    }
}
```

## Success Metrics

- **Security**: Zero path traversal incidents in production
- **Performance**: Validation overhead < 1ms per file operation
- **Coverage**: 100% of file operations use validation
- **Quality**: Zero clippy warnings, all tests passing
- **Adoption**: All modules migrated within 6 weeks
