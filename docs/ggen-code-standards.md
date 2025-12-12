# ggen Code Standards & Style Guide

**Version**: 1.0
**Status**: Draft
**Last Updated**: 2025-11-19

## Purpose

This document defines coding standards for the `ggen` project to ensure consistency, maintainability, and quality across all crates.

---

## 1. General Principles

### 1.1 SOLID Principles
- ✅ **Single Responsibility**: One module/struct/function = one purpose
- ✅ **Open/Closed**: Extend behavior via traits, not modification
- ✅ **Liskov Substitution**: Subtypes must be substitutable for base types
- ✅ **Interface Segregation**: Small, focused trait definitions
- ✅ **Dependency Inversion**: Depend on abstractions, not concretions

### 1.2 Lean Manufacturing Principles
- ❌ **Muda** (Waste): No unused code, imports, or complexity
- ❌ **Mura** (Inconsistency): Standardized patterns and naming
- ✅ **Poka-Yoke** (Mistake-Proofing): Compile-time safety, good errors

---

## 2. Naming Conventions

### 2.1 Crate Names
- **Format**: `ggen-{domain}` (kebab-case)
- **Examples**: `ggen-config`, `ggen-cli-validation`, `ggen-core`
- **Avoid**: Abbreviations unless universally understood (OK: `cli`, `io`, `rdf`)

### 2.2 Module Names
- **Format**: `snake_case`
- **Examples**: `error`, `parser`, `validator`, `io_validator`
- **Structure**: One concept per module (error types, parsing logic, etc.)

### 2.3 Type Names
- **Structs**: `PascalCase` with descriptive names
  ```rust
  ✅ struct GgenConfig { ... }
  ✅ struct AiConfig { ... }
  ❌ struct Cfg { ... }  // Too abbreviated
  ```

- **Enums**: `PascalCase` for type, `PascalCase` for variants
  ```rust
  ✅ enum Permission { Read, Write, Execute }
  ❌ enum permission { read, write }  // Wrong case
  ```

- **Traits**: `PascalCase`, often adjectives or `-able` suffix
  ```rust
  ✅ trait Validate { ... }
  ✅ trait Configurable { ... }
  ✅ trait LoadConfigFromGgenToml { ... }
  ```

### 2.4 Function Names
- **Format**: `snake_case`, imperative verbs
  ```rust
  ✅ fn validate_config(...) -> Result<()>
  ✅ fn load_from_file(...) -> Result<Config>
  ❌ fn configValidator(...)  // Wrong case
  ❌ fn config(...)  // Not descriptive
  ```

### 2.5 Constants and Statics
- **Format**: `SCREAMING_SNAKE_CASE`
  ```rust
  ✅ const MAX_RETRIES: u32 = 3;
  ✅ const DEFAULT_TIMEOUT: Duration = Duration::from_secs(30);
  ❌ const maxRetries: u32 = 3;  // Wrong case
  ```

- **Magic Numbers**: Extract magic numbers to named constants
  ```rust
  // ❌ BAD: Magic number
  if triples.len() > 1000 {
      return Err(ParseError::BatchTooLarge);
  }

  // ✅ GOOD: Named constant
  const MAX_TRIPLES_BATCH_SIZE: usize = 1000;
  if triples.len() > MAX_TRIPLES_BATCH_SIZE {
      return Err(ParseError::BatchTooLarge);
  }
  ```

- **Repeated Strings**: Extract repeated string literals to constants
  ```rust
  // ❌ BAD: Repeated string literal
  turtle.push_str("@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n");
  // ... later ...
  turtle.push_str("@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n");

  // ✅ GOOD: Named constant
  const RDF_PREFIX_DECL: &str = "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n";
  turtle.push_str(RDF_PREFIX_DECL);
  // ... later ...
  turtle.push_str(RDF_PREFIX_DECL);
  ```

- **Documentation**: All constants should have doc comments explaining their purpose
  ```rust
  /// Default cache capacity (maximum entries)
  ///
  /// **Kaizen improvement**: Extracted magic number to named constant for clarity and maintainability.
  const DEFAULT_CACHE_CAPACITY: usize = 1000;
  ```

### 2.6 Variable Names
- **Format**: `snake_case`, descriptive
  ```rust
  ✅ let config_path = Path::new("ggen.toml");
  ✅ let validation_result = validator.validate(&config);
  ❌ let p = Path::new(...);  // Too short
  ❌ let cfgPath = ...;  // Wrong case
  ```

---

## 3. Error Handling Standards

### 3.1 Error Type Definition

**Standard Format**:
```rust
use thiserror::Error;

#[derive(Debug, thiserror::Error)]  // Always Debug first, then Error
pub enum MyError {
    /// Brief description (one line)
    #[error("Descriptive message: {field}")]  // Capital letter, no period
    VariantName {
        /// Field documentation
        field: String,
    },

    /// I/O error wrapper
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),
}
```

**Rules**:
1. ✅ Use `#[derive(Debug, thiserror::Error)]` (Debug before Error)
2. ✅ Error messages start with capital letter, no ending period
3. ✅ Use structured fields `{ field: Type }` over tuple variants when meaningful
4. ✅ Use `PathBuf` for file paths (not `String`)
5. ✅ Add `#[from]` for wrapping standard errors
6. ✅ Document each variant with `///`

### 3.2 Result Type Alias

**Standard**:
```rust
/// Result type for configuration operations
pub type Result<T> = std::result::Result<T, ConfigError>;
```

**Usage**:
```rust
✅ pub fn load_config() -> Result<Config> { ... }
❌ pub fn load_config() -> std::result::Result<Config, ConfigError> { ... }
```

### 3.3 Error Messages

**Format**: Actionable, with examples when possible

```rust
// ❌ BAD
if name.is_empty() {
    return Err(MyError::Invalid);
}

// ✅ GOOD
if name.is_empty() {
    return Err(MyError::InvalidField {
        field: "name".to_string(),
        reason: "Name cannot be empty. Example: name = \"my-project\"".to_string(),
    });
}
```

### 3.4 Panic Documentation

**Always document panic conditions**:
```rust
/// Load configuration from file
///
/// # Errors
/// Returns `ConfigError::FileNotFound` if file doesn't exist
///
/// # Panics
/// Never panics (returns error instead)
pub fn load_config(path: &Path) -> Result<Config> { ... }
```

---

## 4. Documentation Standards

### 4.1 Module-Level Documentation

**Required at top of each file**:
```rust
//! # Module Name
//!
//! Brief description of module purpose (1-2 sentences).
//!
//! ## Features
//! - Feature 1
//! - Feature 2
//!
//! ## Example
//! ```rust
//! use crate::module;
//! // Example usage
//! ```
```

### 4.2 Type Documentation

**Structs and Enums**:
```rust
/// Brief one-line description
///
/// Detailed explanation (optional)
///
/// # Examples
/// ```rust
/// let config = GgenConfig::default();
/// ```
#[derive(Debug, Clone)]
pub struct GgenConfig {
    /// Project metadata
    pub project: ProjectConfig,
}
```

### 4.3 Function Documentation

**Standard Format**:
```rust
/// Brief description (imperative mood: "Load config", not "Loads config")
///
/// Detailed explanation (optional, use active voice)
///
/// # Arguments
/// * `path` - Path to configuration file
/// * `validate` - Whether to validate after loading
///
/// # Returns
/// Returns the loaded configuration
///
/// # Errors
/// - `ConfigError::FileNotFound` if file doesn't exist
/// - `ConfigError::ParseError` if TOML is invalid
///
/// # Panics
/// Never panics
///
/// # Examples
/// ```rust
/// let config = load_config("ggen.toml", true)?;
/// ```
pub fn load_config(path: &Path, validate: bool) -> Result<GgenConfig> {
    // ...
}
```

### 4.4 Documentation Style

**Rules**:
1. ✅ Use active voice ("Validates config", not "Config is validated")
2. ✅ Use imperative mood for functions ("Load", not "Loads")
3. ✅ Start with capital letter, end with period for full sentences
4. ✅ Include examples for non-trivial functions
5. ✅ Document all public items (enforced by `#![warn(missing_docs)]`)
6. ✅ Use inline code for references: `` `ConfigError` `` not "ConfigError"

---

## 5. Testing Standards

### 5.1 Test Organization

**Unit Tests** (in same file):
```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_specific_behavior() {
        // Arrange
        let input = ...;

        // Act
        let result = function(input);

        // Assert
        assert_eq!(result, expected);
    }
}
```

**Integration Tests** (in `tests/` directory):
```rust
// tests/integration_test.rs
use my_crate::*;

#[test]
fn test_end_to_end_workflow() {
    // Test full workflow
}
```

### 5.2 Test Naming

**Format**: `test_{what}_{condition}_{expected}`

```rust
✅ fn test_validate_config_with_empty_name_returns_error()
✅ fn test_load_config_with_valid_file_succeeds()
❌ fn test1() // Not descriptive
❌ fn testConfigValidation() // Wrong case
```

### 5.3 Test Coverage Goals

- ✅ **Unit Tests**: 80%+ line coverage
- ✅ **Integration Tests**: All public APIs
- ✅ **Error Paths**: 100% of error variants tested
- ✅ **Edge Cases**: Empty strings, max values, boundary conditions

### 5.4 Test Assertions

**Prefer specific assertions**:
```rust
// ❌ AVOID
assert!(result.is_ok());

// ✅ PREFER
assert_eq!(result.unwrap().name, "expected-name");

// ✅ FOR ERRORS
assert!(matches!(result, Err(ConfigError::FileNotFound(_))));
```

**Test .unwrap() usage**: ✅ OK in tests, ❌ NEVER in production code

---

## 6. Code Organization Standards

### 6.1 Module Structure

**Standard Layout**:
```
crate/
├── src/
│   ├── lib.rs           # Public API, re-exports
│   ├── error.rs         # Error types
│   ├── {domain}.rs      # Domain logic (one concept per file)
│   ├── {domain}/        # Or subdirectory for complex domains
│   │   ├── mod.rs
│   │   ├── submodule1.rs
│   │   └── submodule2.rs
│   └── bin/             # Binary targets (optional)
│       └── main.rs
├── tests/
│   └── integration_test.rs
├── benches/             # Benchmarks (optional)
│   └── benchmark.rs
└── Cargo.toml
```

### 6.2 Import Organization

**Standard Order**:
1. Standard library (`std::`)
2. External crates
3. Internal crates (`crate::`)
4. Parent/sibling modules (`super::`, `self::`)

**Example**:
```rust
use std::collections::HashMap;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::error::{ConfigError, Result};
use crate::schema::GgenConfig;
```

### 6.3 File Size Limits

**Guidelines**:
- ✅ **Target**: < 500 lines per file
- ⚠️ **Warning**: 500-800 lines (consider splitting)
- ❌ **Too Large**: > 800 lines (must refactor)

**Exception**: Generated code, comprehensive test suites

---

## 7. Type Safety Standards

### 7.1 Avoid Stringly-Typed APIs

**Bad**:
```rust
❌ fn set_log_level(level: &str) -> Result<()> {
    match level {
        "debug" | "info" | "warn" | "error" => Ok(()),
        _ => Err(...),
    }
}
```

**Good**:
```rust
✅ enum LogLevel { Debug, Info, Warn, Error }

✅ fn set_log_level(level: LogLevel) -> Result<()> {
    // Type-safe, no runtime validation needed
}
```

### 7.2 Use Newtypes for Domain Concepts

**Example**:
```rust
✅ pub struct Temperature(f32);

impl Temperature {
    pub fn new(value: f32) -> Result<Self> {
        if (0.0..=1.0).contains(&value) {
            Ok(Self(value))
        } else {
            Err(ConfigError::InvalidValue { ... })
        }
    }
}

// Usage
let temp = Temperature::new(0.7)?;  // Validated at construction
```

### 7.3 Builder Pattern for Complex Types

**Use when**:
- Type has > 5 fields
- Many fields are optional
- Validation depends on multiple fields

**Example**:
```rust
pub struct ConfigBuilder {
    name: Option<String>,
    version: Option<String>,
    // ...
}

impl ConfigBuilder {
    pub fn new() -> Self { ... }

    pub fn name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }

    pub fn build(self) -> Result<Config> {
        // Validate and construct
    }
}
```

---

## 8. Performance Standards

### 8.1 Avoid Unnecessary Allocations

**Prefer**:
```rust
✅ fn process(data: &str) -> &str { ... }  // No allocation
❌ fn process(data: &str) -> String { data.to_string() }  // Unnecessary
```

### 8.2 Use Iterators Over Loops

**Example**:
```rust
// ❌ AVOID
let mut result = Vec::new();
for item in items {
    if item.is_valid() {
        result.push(item.process());
    }
}

// ✅ PREFER
let result: Vec<_> = items
    .iter()
    .filter(|item| item.is_valid())
    .map(|item| item.process())
    .collect();
```

### 8.3 Benchmark Critical Paths

**Use criterion for benchmarks**:
```rust
// benches/benchmark.rs
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn benchmark_parser(c: &mut Criterion) {
    c.bench_function("parse_config", |b| {
        b.iter(|| parse_config(black_box(SAMPLE_TOML)))
    });
}

criterion_group!(benches, benchmark_parser);
criterion_main!(benches);
```

---

## 9. Clippy and Linting

### 9.1 Required Lints

**In `lib.rs` or `main.rs`**:
```rust
#![warn(missing_docs)]
#![warn(clippy::all)]
#![warn(clippy::pedantic)]
#![warn(clippy::nursery)]
```

### 9.2 Allowed Exceptions

**Use sparingly and document**:
```rust
// ✅ OK: Document why
#![allow(clippy::struct_excessive_bools)]  // Config structs have many flags

// ❌ AVOID: Silencing without reason
#![allow(clippy::all)]
```

### 9.3 Pre-Commit Checks

**Required**:
- ✅ `cargo fmt --check`
- ✅ `cargo clippy -- -D warnings`
- ✅ `cargo test --all-features`
- ✅ `cargo doc --no-deps`

---

## 10. Security Standards

### 10.1 Input Validation

**Always validate external input**:
```rust
pub fn load_config(path: &Path) -> Result<Config> {
    // 1. Path traversal check
    validate_path(path)?;

    // 2. Read file
    let content = fs::read_to_string(path)?;

    // 3. Parse TOML
    let config: Config = toml::from_str(&content)?;

    // 4. Validate constraints
    validate_config(&config)?;

    Ok(config)
}
```

### 10.2 No Secrets in Code

**Never**:
```rust
❌ const API_KEY: &str = "sk-123456789";
❌ const PASSWORD: &str = "admin";
```

**Always**:
```rust
✅ let api_key = env::var("API_KEY")?;
✅ let password = read_from_secure_storage()?;
```

### 10.3 Path Traversal Prevention

**Always check paths**:
```rust
fn validate_path(path: &Path) -> Result<()> {
    let path_str = path.to_string_lossy();
    if path_str.contains("../") || path_str.starts_with("..") {
        return Err(SecurityError::PathTraversal { ... });
    }
    Ok(())
}
```

---

## 11. Git and Versioning Standards

### 11.1 Commit Messages

**Format**: `type(scope): brief description`

**Types**:
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation only
- `refactor`: Code restructuring (no behavior change)
- `test`: Add or update tests
- `perf`: Performance improvement
- `chore`: Build, dependencies, etc.

**Examples**:
```
✅ feat(config): add environment variable expansion
✅ fix(validator): correct temperature range check
✅ docs(readme): update installation instructions
❌ fixed bug
❌ WIP
```

### 11.2 Version Numbering

**Follow Semantic Versioning (semver)**:
- `MAJOR.MINOR.PATCH`
- `MAJOR`: Breaking changes
- `MINOR`: New features (backward compatible)
- `PATCH`: Bug fixes (backward compatible)

**Examples**:
- `0.1.0` → `0.1.1`: Bug fix
- `0.1.1` → `0.2.0`: New feature
- `0.2.0` → `1.0.0`: Stable release or breaking change

---

## 12. Code Review Checklist

### Before Submitting PR

- [ ] All tests pass (`cargo test --all-features`)
- [ ] No clippy warnings (`cargo clippy -- -D warnings`)
- [ ] Code is formatted (`cargo fmt`)
- [ ] Documentation is complete (`cargo doc --no-deps`)
- [ ] Commit messages follow standard
- [ ] No TODOs/FIXMEs in production code (use GitHub issues)
- [ ] Error messages are helpful and actionable
- [ ] No panics in production code (document if unavoidable)

### During Review

- [ ] Code follows naming conventions
- [ ] Error handling is consistent
- [ ] No unnecessary complexity
- [ ] Tests cover error paths
- [ ] Documentation is accurate
- [ ] No security vulnerabilities
- [ ] No performance regressions

---

## 13. Enforcement

### Automated Checks (CI)
- ✅ Formatting (`cargo fmt --check`)
- ✅ Linting (`cargo clippy`)
- ✅ Tests (`cargo test`)
- ✅ Documentation (`cargo doc`)
- ✅ Coverage (`cargo tarpaulin`)

### Manual Review
- ✅ Architecture consistency
- ✅ Code readability
- ✅ Documentation quality
- ✅ Test coverage

---

## 14. Examples and Anti-Patterns

### ✅ Good Example: Complete Module

```rust
//! # Configuration Parser
//!
//! Provides type-safe parsing of ggen.toml files.

use std::fs;
use std::path::Path;

use crate::error::{ConfigError, Result};
use crate::schema::GgenConfig;

/// Load and parse configuration from file
///
/// # Arguments
/// * `path` - Path to ggen.toml file
///
/// # Errors
/// - `ConfigError::FileNotFound` if file doesn't exist
/// - `ConfigError::ParseError` if TOML is invalid
///
/// # Examples
/// ```rust
/// let config = load_config("ggen.toml")?;
/// ```
pub fn load_config(path: &Path) -> Result<GgenConfig> {
    let content = fs::read_to_string(path)
        .map_err(|_| ConfigError::FileNotFound(path.to_path_buf()))?;

    toml::from_str(&content)
        .map_err(ConfigError::from)
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_load_config_with_valid_file_succeeds() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("ggen.toml");
        fs::write(&path, r#"
            [project]
            name = "test"
            version = "1.0.0"
        "#).unwrap();

        let result = load_config(&path);
        assert!(result.is_ok());
    }

    #[test]
    fn test_load_config_with_missing_file_returns_error() {
        let result = load_config(Path::new("/nonexistent.toml"));
        assert!(matches!(result, Err(ConfigError::FileNotFound(_))));
    }
}
```

### ❌ Anti-Pattern: Bad Example

```rust
// ❌ No module documentation
use std::fs;

// ❌ No documentation
pub fn load(p: &str) -> String {  // ❌ String for path, String return
    // ❌ unwrap in production code
    fs::read_to_string(p).unwrap()
}

// ❌ No tests
```

---

## Conclusion

Following these standards ensures:
- ✅ Consistent codebase
- ✅ Easy onboarding for new developers
- ✅ Reduced bugs and security issues
- ✅ Better maintainability
- ✅ Clear code review expectations

**Questions?** Open an issue in the repository.
