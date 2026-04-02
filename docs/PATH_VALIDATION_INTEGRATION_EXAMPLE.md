<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Path Validation Integration Example](#path-validation-integration-example)
  - [Quick Start: Integrating PathValidator into Existing Code](#quick-start-integrating-pathvalidator-into-existing-code)
    - [Step 1: Add Import](#step-1-add-import)
    - [Step 2: Create Workspace-Scoped Validator](#step-2-create-workspace-scoped-validator)
    - [Step 3: Replace Unsafe File Operations](#step-3-replace-unsafe-file-operations)
      - [Template Loading](#template-loading)
      - [RDF Loading](#rdf-loading)
      - [Code Generation Output](#code-generation-output)
  - [Real Integration Examples](#real-integration-examples)
    - [ggen-core/src/template.rs](#ggen-coresrctemplaters)
    - [ggen-ontology-core/src/triple_store.rs](#ggen-ontology-coresrctriple_storers)
    - [ggen-core/src/codegen/executor.rs](#ggen-coresrccodegenexecutorrs)
    - [ggen-cli/src/cmds/sync.rs](#ggen-clisrccmdssyncrs)
  - [Testing Integration](#testing-integration)
  - [Rollout Checklist](#rollout-checklist)
    - [Phase 1: Infrastructure (Complete)](#phase-1-infrastructure-complete)
    - [Phase 2: High-Priority Modules (Week 1-2)](#phase-2-high-priority-modules-week-1-2)
    - [Phase 3: Medium-Priority Modules (Week 3-4)](#phase-3-medium-priority-modules-week-3-4)
    - [Phase 4: Testing & Validation (Week 5)](#phase-4-testing--validation-week-5)
    - [Phase 5: Production Deployment (Week 6)](#phase-5-production-deployment-week-6)
  - [Common Pitfalls](#common-pitfalls)
    - [Pitfall 1: Forgetting to Pass Workspace](#pitfall-1-forgetting-to-pass-workspace)
    - [Pitfall 2: Not Handling Validation Errors](#pitfall-2-not-handling-validation-errors)
    - [Pitfall 3: Bypassing Validation](#pitfall-3-bypassing-validation)
  - [Performance Considerations](#performance-considerations)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Path Validation Integration Example

## Quick Start: Integrating PathValidator into Existing Code

### Step 1: Add Import

```rust
use ggen_utils::path_validator::{PathValidator, SafePath};
use ggen_utils::error::Result;
```

### Step 2: Create Workspace-Scoped Validator

```rust
// In your module initialization or constructor
pub struct MyModule {
    workspace: PathBuf,
    validator: PathValidator,
}

impl MyModule {
    pub fn new(workspace: PathBuf) -> Self {
        let validator = PathValidator::new(&workspace)
            .with_allowed_extensions(vec!["tera", "tmpl", "ttl", "rdf"])
            .with_max_depth(10);

        Self { workspace, validator }
    }
}
```

### Step 3: Replace Unsafe File Operations

#### Template Loading

```rust
// BEFORE (UNSAFE)
pub fn load_template(&self, path: &str) -> Result<String> {
    std::fs::read_to_string(path)
        .map_err(|e| Error::new(&format!("Failed to read: {}", e)))
}

// AFTER (SAFE)
pub fn load_template(&self, path: &str) -> Result<String> {
    let safe_path = self.validator.validate(path)?;
    std::fs::read_to_string(safe_path.as_path())
        .map_err(|e| Error::new(&format!("Failed to read: {}", e)))
}
```

#### RDF Loading

```rust
// BEFORE (UNSAFE)
pub fn load_ontology(&mut self, path: &Path) -> Result<()> {
    let content = std::fs::read_to_string(path)?;
    self.parse_turtle(&content)
}

// AFTER (SAFE)
pub fn load_ontology(&mut self, path: &Path) -> Result<()> {
    let safe_path = self.validator.validate(path)?;
    let content = std::fs::read_to_string(safe_path.as_path())?;
    self.parse_turtle(&content)
}
```

#### Code Generation Output

```rust
// BEFORE (UNSAFE)
pub fn write_file(&self, path: &str, content: &str) -> Result<()> {
    std::fs::write(path, content)?;
    Ok(())
}

// AFTER (SAFE)
pub fn write_file(&self, path: &str, content: &str) -> Result<()> {
    let safe_path = self.validator.validate(path)?;

    // Create parent directories if needed
    if let Some(parent) = safe_path.as_path().parent() {
        std::fs::create_dir_all(parent)?;
    }

    std::fs::write(safe_path.as_path(), content)?;
    Ok(())
}
```

## Real Integration Examples

### ggen-core/src/template.rs

```rust
use ggen_utils::path_validator::PathValidator;

pub struct TemplateEngine {
    workspace: PathBuf,
    template_validator: PathValidator,
}

impl TemplateEngine {
    pub fn new(workspace: PathBuf) -> Self {
        let template_validator = PathValidator::new(&workspace)
            .with_allowed_extensions(vec!["tera", "tmpl"])
            .with_max_depth(5);

        Self {
            workspace,
            template_validator,
        }
    }

    pub fn load_template(&self, path: &str) -> Result<String> {
        // Validate path before reading
        let safe_path = self.template_validator.validate(path)
            .map_err(|e| Error::new(&format!(
                "Template path validation failed: {}",
                e
            )))?;

        // Safe to read - path has been validated
        std::fs::read_to_string(safe_path.as_path())
            .map_err(|e| Error::new(&format!(
                "Failed to read template '{}': {}",
                safe_path, e
            )))
    }

    pub fn render(&self, template_name: &str, context: &Context) -> Result<String> {
        let template_content = self.load_template(template_name)?;

        // Render with tera
        let mut tera = tera::Tera::default();
        tera.add_raw_template(template_name, &template_content)?;
        let rendered = tera.render(template_name, context)?;

        Ok(rendered)
    }
}
```

### ggen-ontology-core/src/triple_store.rs

```rust
use ggen_utils::path_validator::PathValidator;

pub struct TripleStore {
    workspace: PathBuf,
    rdf_validator: PathValidator,
    graph: Graph,
}

impl TripleStore {
    pub fn new(workspace: PathBuf) -> Self {
        let rdf_validator = PathValidator::new(&workspace)
            .with_allowed_extensions(vec!["ttl", "rdf", "xml", "n3"])
            .with_max_depth(10);

        Self {
            workspace,
            rdf_validator,
            graph: Graph::new(),
        }
    }

    pub fn load_ontology(&mut self, path: &Path) -> Result<()> {
        // Validate path before loading
        let safe_path = self.rdf_validator.validate(path)
            .map_err(|e| Error::new(&format!(
                "Ontology path validation failed: {}",
                e
            )))?;

        // Load and parse RDF
        let content = std::fs::read_to_string(safe_path.as_path())?;
        self.parse_turtle(&content)?;

        Ok(())
    }

    pub fn load_multiple(&mut self, paths: &[&Path]) -> Result<()> {
        // Batch validation for efficiency
        let safe_paths = self.rdf_validator.validate_batch(paths)?;

        for safe_path in safe_paths {
            let content = std::fs::read_to_string(safe_path.as_path())?;
            self.parse_turtle(&content)?;
        }

        Ok(())
    }
}
```

### ggen-core/src/codegen/executor.rs

```rust
use ggen_utils::path_validator::PathValidator;

pub struct CodeGenerator {
    workspace: PathBuf,
    output_validator: PathValidator,
}

impl CodeGenerator {
    pub fn new(workspace: PathBuf) -> Self {
        let output_validator = PathValidator::new(&workspace)
            .with_allowed_extensions(vec!["rs", "ts", "py", "go", "java"])
            .with_max_depth(10);

        Self {
            workspace,
            output_validator,
        }
    }

    pub fn generate_file(&self, relative_path: &str, content: &str) -> Result<()> {
        // Validate output path
        let safe_path = self.output_validator.validate(relative_path)
            .map_err(|e| Error::new(&format!(
                "Output path validation failed: {}",
                e
            )))?;

        // Create parent directories
        if let Some(parent) = safe_path.as_path().parent() {
            std::fs::create_dir_all(parent)?;
        }

        // Write generated code
        std::fs::write(safe_path.as_path(), content)?;

        Ok(())
    }

    pub fn generate_multiple(&self, files: Vec<(&str, String)>) -> Result<()> {
        // Validate all paths first (fail fast)
        let paths: Vec<&str> = files.iter().map(|(p, _)| *p).collect();
        let safe_paths = self.output_validator.validate_batch(&paths)?;

        // Generate all files
        for (safe_path, (_orig_path, content)) in safe_paths.iter().zip(files.iter()) {
            if let Some(parent) = safe_path.as_path().parent() {
                std::fs::create_dir_all(parent)?;
            }
            std::fs::write(safe_path.as_path(), content)?;
        }

        Ok(())
    }
}
```

### ggen-cli/src/cmds/sync.rs

```rust
use ggen_utils::path_validator::PathValidator;

pub fn sync(manifest_path: Option<String>) -> Result<()> {
    let current_dir = std::env::current_dir()?;
    let workspace = current_dir.as_path();

    // Validate manifest path
    let manifest_validator = PathValidator::new(workspace)
        .with_allowed_extensions(vec!["toml"])
        .with_max_depth(5);

    let manifest_path_str = manifest_path.unwrap_or_else(|| "ggen.toml".to_string());
    let safe_manifest_path = manifest_validator.validate(&manifest_path_str)
        .map_err(|e| Error::new(&format!(
            "Manifest path validation failed: {}",
            e
        )))?;

    // Load and parse manifest
    let manifest_content = std::fs::read_to_string(safe_manifest_path.as_path())?;
    let manifest: Manifest = toml::from_str(&manifest_content)?;

    // Create validators for different file types
    let ontology_validator = PathValidator::new(workspace)
        .with_allowed_extensions(vec!["ttl", "rdf", "xml", "n3"])
        .with_max_depth(10);

    let template_validator = PathValidator::new(workspace)
        .with_allowed_extensions(vec!["tera", "tmpl"])
        .with_max_depth(5);

    let output_validator = PathValidator::new(workspace)
        .with_allowed_extensions(vec!["rs", "ts", "py", "go", "java"])
        .with_max_depth(10);

    // Validate ontology path
    let safe_ontology_path = ontology_validator.validate(&manifest.ontology.source)?;

    // Validate template paths
    for rule in &manifest.generation.rules {
        if let Some(template_file) = &rule.template_file {
            template_validator.validate(template_file)?;
        }
    }

    // Continue with sync...
    Ok(())
}
```

## Testing Integration

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_template_loading_with_validation() {
        // Arrange
        let workspace = tempdir().unwrap();
        let engine = TemplateEngine::new(workspace.path().to_path_buf());

        let template_path = workspace.path().join("templates/example.tera");
        std::fs::create_dir_all(template_path.parent().unwrap()).unwrap();
        std::fs::write(&template_path, "Hello {{ name }}").unwrap();

        // Act
        let result = engine.load_template("templates/example.tera");

        // Assert
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), "Hello {{ name }}");
    }

    #[test]
    fn test_template_loading_blocks_traversal() {
        // Arrange
        let workspace = tempdir().unwrap();
        let engine = TemplateEngine::new(workspace.path().to_path_buf());

        // Act - attempt path traversal
        let result = engine.load_template("../../../etc/passwd");

        // Assert
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("traversal"));
    }

    #[test]
    fn test_code_generation_validates_extension() {
        // Arrange
        let workspace = tempdir().unwrap();
        let generator = CodeGenerator::new(workspace.path().to_path_buf());

        // Act - attempt to generate executable
        let result = generator.generate_file("malware.exe", "evil code");

        // Assert
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("extension"));
    }
}
```

## Rollout Checklist

### Phase 1: Infrastructure (Complete)
- [x] PathValidator implementation
- [x] SafePath type
- [x] Comprehensive tests
- [x] Documentation

### Phase 2: High-Priority Modules (Week 1-2)
- [ ] ggen-core/src/template.rs
- [ ] ggen-core/src/tera_env.rs
- [ ] ggen-ontology-core/src/triple_store.rs
- [ ] ggen-core/src/codegen/executor.rs
- [ ] ggen-core/src/codegen/pipeline.rs

### Phase 3: Medium-Priority Modules (Week 3-4)
- [ ] ggen-cli/src/cmds/sync.rs
- [ ] ggen-cli/src/cmds/init.rs
- [ ] ggen-config/src/parser.rs
- [ ] ggen-domain/src/template/generate.rs
- [ ] ggen-domain/src/ontology/extract.rs

### Phase 4: Testing & Validation (Week 5)
- [ ] Integration tests
- [ ] Security tests
- [ ] Performance benchmarks
- [ ] Code review

### Phase 5: Production Deployment (Week 6)
- [ ] Monitoring setup
- [ ] Rollout to production
- [ ] Post-deployment validation

## Common Pitfalls

### Pitfall 1: Forgetting to Pass Workspace

```rust
// ❌ WRONG - No workspace context
fn load_template(path: &str) -> Result<String> {
    let validator = PathValidator::new(Path::new(".")); // Wrong!
    // ...
}

// ✅ CORRECT - Workspace from context
fn load_template(&self, path: &str) -> Result<String> {
    let safe_path = self.validator.validate(path)?;
    // ...
}
```

### Pitfall 2: Not Handling Validation Errors

```rust
// ❌ WRONG - Swallowing validation errors
fn load_template(&self, path: &str) -> Result<String> {
    let safe_path = self.validator.validate(path).unwrap(); // Panic!
    // ...
}

// ✅ CORRECT - Propagating errors with context
fn load_template(&self, path: &str) -> Result<String> {
    let safe_path = self.validator.validate(path)
        .map_err(|e| Error::new(&format!("Validation failed: {}", e)))?;
    // ...
}
```

### Pitfall 3: Bypassing Validation

```rust
// ❌ WRONG - Using both validated and unvalidated paths
pub fn load_file(&self, path: &str, skip_validation: bool) -> Result<String> {
    if skip_validation {
        Ok(std::fs::read_to_string(path)?) // Bypasses security!
    } else {
        let safe_path = self.validator.validate(path)?;
        Ok(std::fs::read_to_string(safe_path.as_path())?)
    }
}

// ✅ CORRECT - Always validate
pub fn load_file(&self, path: &str) -> Result<String> {
    let safe_path = self.validator.validate(path)?;
    Ok(std::fs::read_to_string(safe_path.as_path())?)
}
```

## Performance Considerations

Path validation adds minimal overhead:

```rust
use std::time::Instant;

fn benchmark_validation() {
    let workspace = Path::new("/workspace");
    let validator = PathValidator::new(workspace);

    let start = Instant::now();
    for _ in 0..1000 {
        let _ = validator.validate("templates/example.tera");
    }
    let duration = start.elapsed();

    println!("1000 validations in {:?}", duration);
    println!("Average per validation: {:?}", duration / 1000);
    // Typical output: Average per validation: ~50µs
}
```

## Summary

1. **Always use PathValidator** for file operations
2. **Configure appropriately** for each file type (templates, RDF, output, config)
3. **Batch validate** when possible for better performance
4. **Test security** with path traversal, symlinks, null bytes
5. **Propagate errors** with clear messages
6. **Monitor** validation failures in production
