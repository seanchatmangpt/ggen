<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Error Handling & Validation Guide](#error-handling--validation-guide)
  - [Result<T, E> Pattern](#resultt-e-pattern)
    - [The ? Operator](#the--operator)
  - [Custom Error Types](#custom-error-types)
  - [Error Context](#error-context)
  - [Validation Chains](#validation-chains)
  - [Poka-Yoke (Mistake-Proofing)](#poka-yoke-mistake-proofing)
  - [Security Validation Patterns](#security-validation-patterns)
    - [Path Traversal Prevention](#path-traversal-prevention)
    - [Input Sanitization](#input-sanitization)
    - [SPARQL Injection Prevention](#sparql-injection-prevention)
  - [Error Recovery](#error-recovery)
  - [Testing Error Cases](#testing-error-cases)
  - [Critical Rules](#critical-rules)
  - [Quick Reference](#quick-reference)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Error Handling & Validation Guide

## Result<T, E> Pattern

All fallible operations in ggen return `Result<T, E>`:

```rust
// CORRECT: Returns Result
pub fn generate_code(template: &str, ontology: &str) -> Result<String> {
    let template_obj = load_template(template)?; // Early return on error
    let ontology_obj = load_ontology(ontology)?;
    let code = template_obj.render(&ontology_obj)?;
    Ok(code)
}

// WRONG: Panics on error
pub fn generate_code_bad(template: &str, ontology: &str) -> String {
    let template_obj = load_template(template).unwrap(); // PANIC!
    let ontology_obj = load_ontology(ontology).unwrap(); // PANIC!
    let code = template_obj.render(&ontology_obj).unwrap(); // PANIC!
    code
}
```

### The ? Operator

The `?` operator converts errors up the call stack:

```rust
pub fn validate_and_generate(
    template: &str,
    ontology: &str,
) -> Result<GenerationOutput> {
    // ? automatically converts errors to our Result type
    validate_template(template)?;
    validate_ontology(ontology)?;
    let code = generate_code(template, ontology)?;

    Ok(GenerationOutput {
        code,
        timestamp: Utc::now(),
    })
}
```

## Custom Error Types

Define descriptive error types with `thiserror`:

```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum GenerationError {
    #[error("Template not found: {path}")]
    TemplateNotFound { path: String },

    #[error("Invalid ontology: {reason}")]
    InvalidOntology { reason: String },

    #[error("Template rendering failed: {0}")]
    RenderingFailed(String),

    #[error("Validation error: {0}")]
    ValidationFailed(String),

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),

    #[error("RDF error: {0}")]
    RdfError(#[from] oxigraph::sparql::EvaluationError),
}

// Custom Result type
pub type Result<T> = std::result::Result<T, GenerationError>;
```

## Error Context

Add rich context to errors with `.context()`:

```rust
use anyhow::{Context, Result};

pub fn load_package(package_id: &str) -> Result<Package> {
    let path = format!("packages/{}.toml", package_id);

    let content = std::fs::read_to_string(&path)
        .with_context(|| format!("Failed to read package file: {}", path))?;

    let package = toml::from_str(&content)
        .with_context(|| format!("Invalid package TOML in {}", package_id))?;

    Ok(package)
}
```

Error output:
```
Error: Failed to load package

Caused by:
    0: Failed to read package file: packages/rust-api.toml
    1: No such file or directory (os error 2)
```

## Validation Chains

Chain multiple validations with descriptive errors:

```rust
pub struct PackageValidator;

impl PackageValidator {
    pub fn validate(pkg: &Package) -> Result<()> {
        Self::validate_id(&pkg.id)?;
        Self::validate_version(&pkg.version)?;
        Self::validate_dependencies(&pkg.dependencies)?;
        Self::validate_manifest(&pkg.manifest)?;
        Ok(())
    }

    fn validate_id(id: &str) -> Result<()> {
        if id.is_empty() {
            return Err(GenerationError::ValidationFailed(
                "Package ID cannot be empty".to_string(),
            ));
        }
        if !id.chars().all(|c| c.is_alphanumeric() || c == '-' || c == '_') {
            return Err(GenerationError::ValidationFailed(
                format!("Invalid package ID: {} (alphanumeric, -, _ only)", id),
            ));
        }
        Ok(())
    }

    fn validate_version(version: &str) -> Result<()> {
        semver::Version::parse(version)
            .map_err(|e| GenerationError::ValidationFailed(
                format!("Invalid version {}: {}", version, e),
            ))?;
        Ok(())
    }

    fn validate_dependencies(deps: &[Dependency]) -> Result<()> {
        for dep in deps {
            if dep.package_id.is_empty() {
                return Err(GenerationError::ValidationFailed(
                    "Dependency package ID cannot be empty".to_string(),
                ));
            }
        }
        Ok(())
    }

    fn validate_manifest(manifest: &PackageManifest) -> Result<()> {
        if manifest.files.is_empty() {
            return Err(GenerationError::ValidationFailed(
                "Package manifest must contain files".to_string(),
            ));
        }
        Ok(())
    }
}
```

## Poka-Yoke (Mistake-Proofing)

Use types to make invalid states impossible:

```rust
// GOOD: Type-safe, impossible to create invalid Package
pub struct Package {
    id: NonEmptyString,  // Guarantees non-empty
    version: SemVer,     // Guarantees valid semver
    dependencies: Vec<Dependency>,
}

// Helper types that enforce invariants
pub struct NonEmptyString(String);

impl NonEmptyString {
    pub fn new(s: String) -> Result<Self> {
        if s.is_empty() {
            Err(GenerationError::ValidationFailed(
                "String cannot be empty".to_string(),
            ))
        } else {
            Ok(NonEmptyString(s))
        }
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

// Use in code
pub fn create_package(id: &str, version: &str) -> Result<Package> {
    let safe_id = NonEmptyString::new(id.to_string())?; // Validates at construction
    let safe_version = SemVer::parse(version)?;

    Ok(Package {
        id: safe_id,
        version: safe_version,
        dependencies: vec![],
    })
}
```

## Security Validation Patterns

### Path Traversal Prevention

```rust
pub fn validate_extraction_path(archive_path: &Path, extract_to: &Path) -> Result<()> {
    let canonical_extract = extract_to.canonicalize()
        .with_context(|| format!("Cannot access extraction directory: {:?}", extract_to))?;

    let archive_contents = std::fs::read_dir(archive_path)?;

    for entry in archive_contents {
        let path = entry?.path();
        let canonical_path = path.canonicalize()?;

        // Verify extracted file is within extraction directory
        if !canonical_path.starts_with(&canonical_extract) {
            return Err(GenerationError::ValidationFailed(
                format!("Path traversal attempt detected: {:?}", path),
            ));
        }
    }

    Ok(())
}
```

### Input Sanitization

```rust
pub fn validate_template_name(name: &str) -> Result<String> {
    const MAX_LENGTH: usize = 100;
    const ALLOWED_CHARS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_";

    if name.is_empty() {
        return Err(GenerationError::ValidationFailed(
            "Template name cannot be empty".to_string(),
        ));
    }

    if name.len() > MAX_LENGTH {
        return Err(GenerationError::ValidationFailed(
            format!("Template name exceeds {} characters", MAX_LENGTH),
        ));
    }

    if !name.chars().all(|c| ALLOWED_CHARS.contains(c)) {
        return Err(GenerationError::ValidationFailed(
            format!("Template name contains invalid characters: {}", name),
        ));
    }

    Ok(name.to_string())
}
```

### SPARQL Injection Prevention

```rust
pub fn safe_sparql_query(
    store: &Store,
    template: &str,
    variables: &[(String, String)],
) -> Result<Vec<Solution>> {
    // Parameterized queries prevent injection
    let query = r#"
        SELECT ?template
        WHERE {
            ?template a ggen:Template .
            ?template rdfs:label ?name .
            FILTER (?name = ?searchTerm)
        }
    "#;

    // Don't do this:
    // let bad = format!("FILTER (?name = \"{}\")", search_term); // INJECTION!

    // Do this instead:
    store.query_with_variables(query, variables)
        .map_err(|e| GenerationError::RdfError(e))
}
```

## Error Recovery

Implement graceful degradation when appropriate:

```rust
pub fn load_package_with_fallback(
    primary_path: &Path,
    fallback_path: &Path,
) -> Result<Package> {
    match load_package(primary_path) {
        Ok(pkg) => Ok(pkg),
        Err(e) => {
            eprintln!("Failed to load from primary: {}", e);
            eprintln!("Attempting fallback location...");

            load_package(fallback_path)
                .with_context(|| format!(
                    "Failed to load package from both {:?} and {:?}",
                    primary_path, fallback_path
                ))
        }
    }
}
```

## Testing Error Cases

Always test error paths:

```rust
#[test]
fn test_validation_error_message() {
    // Arrange
    let invalid_package = Package {
        id: "".to_string(),  // Empty ID
        version: "1.0.0".to_string(),
        dependencies: vec![],
    };

    // Act
    let result = PackageValidator::validate(&invalid_package);

    // Assert
    assert!(result.is_err());
    if let Err(GenerationError::ValidationFailed(msg)) = result {
        assert!(msg.contains("Package ID cannot be empty"));
    }
}

#[test]
fn test_path_traversal_rejected() {
    // Arrange
    let malicious_file = "../../../etc/passwd";

    // Act
    let result = validate_extraction_path(Path::new("archive"), Path::new(malicious_file));

    // Assert
    assert!(result.is_err());
}
```

## Critical Rules

1. **NEVER `.unwrap()` or `.expect()`** in production code - Use `Result<T>`
2. **ALWAYS provide error context** - What failed and why?
3. **VALIDATE on input boundaries** - Entry points to modules
4. **USE type system** - Make invalid states impossible
5. **TEST error paths** - Not just happy path
6. **HANDLE errors gracefully** - Show meaningful messages
7. **PROPAGATE errors with ?** - Don't swallow or hide them
8. **DOCUMENT error conditions** - Comment non-obvious error cases

---

## Quick Reference

```rust
// Error type definition
use thiserror::Error;

#[derive(Error, Debug)]
pub enum MyError {
    #[error("Not found: {0}")]
    NotFound(String),
    #[error("Invalid: {0}")]
    Invalid(String),
}

pub type Result<T> = std::result::Result<T, MyError>;

// Fallible function
pub fn my_function() -> Result<String> {
    let value = get_value()?;  // ? propagates errors
    validate(&value)?;
    Ok(value)
}

// Testing errors
#[test]
fn test_error() {
    let result = my_function();
    assert!(result.is_err());
}
```
