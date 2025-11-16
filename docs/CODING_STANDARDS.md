# ggen Coding Standards - Eliminate Mura

## Philosophy

Consistency is more important than perfection. We maintain consistent code quality and patterns across the codebase to reduce cognitive load and maintenance cost.

---

## 1. Error Handling Standards

### Standard Pattern: Result<T, E>

**Rule**: Use `Result<T, E>` for all fallible operations. Never use `panic!()` in production code paths.

**Allowed in production**:
- `Result<T, E>` - Preferred for all error cases
- `Option<T>` - Only for presence/absence (not errors)
- `Option::ok_or()` - Convert Option to Result

**Never use in production**:
- `panic!()` - Will be caught in CI
- `.unwrap()` - Will be caught in CI
- `.expect()` - Will be caught in CI
- `!` operator without proper error handling

**Example - Correct**:
```rust
pub fn process(input: &str) -> Result<String, ProcessError> {
    if input.is_empty() {
        return Err(ProcessError::EmptyInput);
    }
    Ok(input.to_uppercase())
}
```

**Example - Incorrect**:
```rust
pub fn process(input: &str) -> String {
    input.to_uppercase() // No error handling
    // OR
    // panic!("Input cannot be empty"); // FORBIDDEN
    // OR
    // input.len() > 0 ? ... : panic!(); // FORBIDDEN
}
```

**Test coverage requirement**: All error paths must have tests

---

## 2. Error Type Standards

### Standard: One Error Type Per Module

**Rule**: Each module defines and uses a single error type that wraps specific error variants.

**Pattern**:
```rust
pub mod mymodule {
    #[derive(Debug, thiserror::Error)]
    pub enum MyModuleError {
        #[error("Empty input provided")]
        EmptyInput,

        #[error("Invalid format: {0}")]
        InvalidFormat(String),

        #[error("IO error: {0}")]
        Io(#[from] std::io::Error),
    }

    pub type Result<T> = std::result::Result<T, MyModuleError>;

    pub fn process(input: &str) -> Result<String> {
        // Implementation
    }
}
```

**Benefits**:
- Single error type per module reduces cognitive load
- Clear error propagation path
- Type system ensures all errors handled correctly
- Module-level error mapping simplifies code

---

## 3. Documentation Standards

### Module-Level Documentation (`//!`)

**Rule**: All public modules must have module-level documentation.

**Required sections**:
1. Module purpose and overview
2. Key concepts
3. Usage examples with doctests
4. Module organization (submodules)

**Template**:
```rust
//! # Module Name - Brief Description
//!
//! Detailed description of what this module provides.
//!
//! ## Overview
//!
//! Key concepts and architecture.
//!
//! ## Examples
//!
//! ```rust
//! use crate::mymodule::process;
//!
//! let result = process("input")?;
//! # Ok::<_, Box<dyn std::error::Error>>(())
//! ```
```

### Item-Level Documentation (`///`)

**Rule**: All public functions, types, and constants must have documentation.

**Required sections**:
1. One-line summary
2. Detailed description
3. Arguments (for functions)
4. Return value
5. Errors (for fallible operations)
6. Examples with doctests

**Template**:
```rust
/// Processes input and returns result.
///
/// Detailed description explaining the operation.
///
/// # Arguments
///
/// * `input` - Input to process
///
/// # Returns
///
/// Processed result on success.
///
/// # Errors
///
/// * `ProcessError::EmptyInput` - When input is empty
///
/// # Examples
///
/// ```rust
/// use crate::mymodule::process;
///
/// let result = process("valid")?;
/// assert_eq!(result, "processed");
/// # Ok::<_, Box<dyn std::error::Error>>(())
/// ```
pub fn process(input: &str) -> Result<String, ProcessError> {
    // Implementation
}
```

### Doctest Standards

**Format**: Use `Result`-based examples with `?` operator

```rust
/// Example with error handling
///
/// ```rust
/// use crate::mymodule::process;
///
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let result = process("input")?;
/// assert_eq!(result, "expected");
/// # Ok(())
/// # }
/// ```
```

**Rules**:
- All public APIs must have at least one doctest
- Doctests must compile and run (`cargo test --doc`)
- Use `# fn main() -> Result<..> {` for error-handling examples
- Use `no_run` only when examples require external resources
- Include error case examples for fallible operations

---

## 4. Test Coverage Standards

### Minimum Coverage: 80%

**Rule**: All modules must maintain ≥80% test coverage.

**Coverage by type**:
- **Unit tests**: ≥70% of module functions
- **Integration tests**: ≥50% of module interactions
- **Error path tests**: 100% of error variants

**Coverage calculation**:
```bash
cargo tarpaulin --workspace --out Html
# Open coverage/index.html to verify
```

**Test organization**:
```rust
#[cfg(test)]
mod tests {
    use super::*;

    // Success case tests
    test!(test_valid_input, { /* ... */ });

    // Error case tests
    test!(test_empty_input, { /* ... */ });
    test!(test_invalid_format, { /* ... */ });
}
```

---

## 5. Code Style Standards

### Style Enforcement: cargo fmt

**Rule**: All code must pass `cargo fmt --check`.

**Apply formatting**:
```bash
cargo fmt
```

**CI enforcement**:
```bash
cargo fmt --check  # Fails if formatting needed
```

### Naming Conventions

**Functions and variables**: `snake_case`
```rust
pub fn process_input() { }
let input_data = "";
```

**Types and traits**: `PascalCase`
```rust
pub struct ProcessConfig { }
pub trait InputProcessor { }
```

**Constants**: `SCREAMING_SNAKE_CASE`
```rust
pub const MAX_INPUT_SIZE: usize = 1024;
```

### Import Organization

**Rule**: Imports organized in three groups (std, external, local)

```rust
// Standard library
use std::fmt;
use std::io;

// External crates
use serde::Deserialize;
use thiserror::Error;

// Local modules
use crate::error::ProcessError;
use crate::config::Config;
```

---

## 6. Quality Gates

### Pre-commit Checks

**Local validation** (enforce before commit):
```bash
cargo fmt --check              # Code style
cargo clippy                   # Lint warnings
cargo test --lib             # Unit tests
cargo test --doc             # Doctests
```

### CI Checks

**GitHub Actions** (enforce on push/PR):
- ✅ Code formatting (`cargo fmt --check`)
- ✅ Linting (`cargo clippy`)
- ✅ Unit tests (`cargo test --lib`)
- ✅ Integration tests (`cargo test --test ...`)
- ✅ Doctests (`cargo test --doc`)
- ✅ Code coverage (≥80%)
- ✅ No panic points (quality-gates.yml)
- ✅ No hardcoded paths
- ✅ All platforms build (Ubuntu, macOS, Windows)

### Breaking Changes

**Mura that causes CI failure**:
1. New `panic!()` in production code
2. Documentation missing on public API
3. Doctest fails to compile
4. Test coverage drops below threshold
5. Code style violations
6. Linting warnings in modified code

---

## 7. Refactoring Guidelines

### When to Refactor

**Refactor to standard when**:
- Adding new functionality (establish pattern)
- Fixing critical bugs (apply standard error handling)
- Improving module (full standardization)
- Code review identifies inconsistency

### Refactoring Pattern

**Example: Error handling standardization**

Before:
```rust
pub fn process(input: &str) -> Option<String> {
    if input.is_empty() {
        return None;  // Inconsistent with other modules
    }
    Some(input.to_uppercase())
}
```

After:
```rust
pub fn process(input: &str) -> Result<String, ProcessError> {
    if input.is_empty() {
        return Err(ProcessError::EmptyInput);
    }
    Ok(input.to_uppercase())
}

#[cfg(test)]
mod tests {
    use super::*;

    test!(test_valid_input, {
        assert_ok!(&process("hello"));
    });

    test!(test_empty_input, {
        assert_err!(&process(""));
    });
}
```

---

## 8. Documentation Examples

### Good Example: Complete Standard

```rust
//! Cache management module
//!
//! Provides efficient caching with deterministic key generation.
//!
//! # Examples
//!
//! ```rust
//! use crate::cache::Cache;
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let cache = Cache::new(100)?;
//! cache.insert("key", "value");
//! assert_eq!(cache.get("key"), Some("value"));
//! # Ok(())
//! # }
//! ```

use std::collections::HashMap;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum CacheError {
    #[error("Cache is full")]
    CacheFull,

    #[error("Key not found")]
    KeyNotFound,
}

pub type Result<T> = std::result::Result<T, CacheError>;

/// Simple in-memory cache with size limit.
///
/// Provides O(1) get/insert operations with maximum capacity.
///
/// # Arguments
///
/// * `capacity` - Maximum number of items to store
///
/// # Errors
///
/// * `CacheError::CacheFull` - When adding item to full cache
/// * `CacheError::KeyNotFound` - When retrieving missing key
///
/// # Examples
///
/// ```rust
/// use crate::cache::Cache;
///
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let cache = Cache::new(10)?;
/// cache.insert("name", "ggen");
/// assert_eq!(cache.get("name"), Some("ggen"));
/// # Ok(())
/// # }
/// ```
pub struct Cache {
    data: HashMap<String, String>,
    capacity: usize,
}

impl Cache {
    /// Creates new cache with specified capacity.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use crate::cache::Cache;
    ///
    /// let cache = Cache::new(100)?;
    /// # Ok::<_, Box<dyn std::error::Error>>(())
    /// ```
    pub fn new(capacity: usize) -> Result<Self> {
        if capacity == 0 {
            return Err(CacheError::CacheFull);
        }
        Ok(Self {
            data: HashMap::new(),
            capacity,
        })
    }

    /// Retrieves value by key.
    ///
    /// # Arguments
    ///
    /// * `key` - Key to look up
    ///
    /// # Returns
    ///
    /// Some(value) if key exists, None otherwise
    ///
    /// # Examples
    ///
    /// ```rust
    /// use crate::cache::Cache;
    ///
    /// let cache = Cache::new(10)?;
    /// cache.insert("x", "y");
    /// assert_eq!(cache.get("x"), Some("y"));
    /// # Ok::<_, Box<dyn std::error::Error>>(())
    /// ```
    pub fn get(&self, key: &str) -> Option<String> {
        self.data.get(key).cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    test!(test_create_cache, {
        let cache = Cache::new(10)?;
        assert_eq!(cache.capacity, 10);
        Ok(())
    });

    test!(test_insert_and_get, {
        let cache = Cache::new(10)?;
        cache.insert("key", "value");
        assert_eq!(cache.get("key"), Some("value"));
        Ok(())
    });

    test!(test_missing_key, {
        let cache = Cache::new(10)?;
        assert_eq!(cache.get("missing"), None);
        Ok(())
    });
}
```

---

## 9. Enforcement Summary

| Standard | Tool | CI Check | Severity |
|----------|------|----------|----------|
| Error handling (Result<T,E>) | clippy | quality-gates.yml | BLOCK |
| Panic points | quality-gates.yml | panic-detection | BLOCK |
| Documentation | cargo doc | workflow | WARN |
| Doctests compile | cargo test --doc | workflow | BLOCK |
| Code formatting | cargo fmt | ci.yml | BLOCK |
| Linting | cargo clippy | ci.yml | BLOCK |
| Test coverage ≥80% | cargo tarpaulin | coverage.yml | BLOCK |
| No hardcoded paths | quality-gates.yml | hardcoded-paths | BLOCK |

---

## 10. Quick Reference

### Checklist for New Code

- [ ] Uses `Result<T, E>` for errors
- [ ] Module has `//!` documentation
- [ ] Public APIs have `///` documentation
- [ ] Doctests included and compile (`cargo test --doc`)
- [ ] Error cases documented with examples
- [ ] Unit tests for all functions
- [ ] Integration tests for module interactions
- [ ] ≥80% code coverage
- [ ] No `panic!()`, `unwrap()`, or `expect()`
- [ ] Code passes `cargo fmt --check`
- [ ] Code passes `cargo clippy`

### Checklist for Code Review

- [ ] Code follows error handling standard
- [ ] Public APIs documented
- [ ] Doctests compile and run
- [ ] Error cases have tests
- [ ] Coverage maintained ≥80%
- [ ] No style violations
- [ ] Consistent with similar code in codebase

---

## References

- **Quality Gates**: `.github/workflows/quality-gates.yml`
- **CI Pipeline**: `.github/workflows/ci.yml`
- **Testing Framework**: `chicago-tdd-tools` prelude

---

**Last Updated**: 2025-11-16
**Standards Version**: 1.0
**Status**: In Effect for v2.7.0 and beyond
