# Mura (Unevenness) Inventory

This document tracks inconsistencies identified in the ggen codebase that need to be standardized.

## 1. Error Handling Pattern Inconsistency

### Problem
Different error handling patterns are used across the codebase:
- **anyhow**: 68 files use `anyhow::Result<T>`
- **ggen_utils::error**: 48 files use `ggen_utils::error::Result<T>`
- **Mixed usage**: Some modules use both patterns

### Impact
- **High**: Makes error handling inconsistent
- **Maintenance burden**: Developers need to know which pattern to use
- **Type incompatibility**: Can't easily compose functions with different error types

### Examples
```rust
// Pattern 1: anyhow (68 files)
use anyhow::Result;
fn process() -> Result<String> { ... }

// Pattern 2: ggen_utils::error (48 files)
use ggen_utils::error::Result;
fn process() -> Result<String> { ... }
```

### Standard
**Decision needed**: Choose one pattern as standard
- **Option A**: Use `ggen_utils::error::Result` everywhere (project-specific error type)
- **Option B**: Use `anyhow::Result` everywhere (simpler, but less type-safe)
- **Recommendation**: Use `ggen_utils::error::Result` for libraries, `anyhow::Result` only in binaries

## 2. Return Type Inconsistency

### Problem
Similar operations use different return types:
- **Option<T>**: 16 public functions return `Option<T>`
- **Result<T, E>**: 174 public functions return `Result<T, E>`

### Impact
- **Medium**: Some functions that should return `Result` return `Option` instead
- **Error information loss**: `Option` doesn't provide error context

### Examples
```rust
// Inconsistent: Returns Option
pub fn find_template(name: &str) -> Option<Template> { ... }

// Consistent: Returns Result
pub fn load_template(path: &Path) -> Result<Template> { ... }
```

### Standard
**Standard**: Use `Result<T, E>` for all fallible operations
- **Rationale**: Provides error context and follows Rust best practices
- **Exception**: Only use `Option<T>` when absence is not an error (e.g., optional configuration)

## 3. unwrap/expect Usage

### Problem
- **3339 matches** of `.unwrap()` or `.expect()` across 286 files
- Many are in tests (acceptable), but some are in production code

### Impact
- **High**: Potential panics in production code
- **Reliability**: Panics can crash the application

### Standard
**Standard**: No `unwrap()` or `expect()` in production code
- **Allowed**: Only in tests, examples, and `main()` functions
- **Required**: All library code must use proper error handling

## 4. Code Style Inconsistency

### Problem
- Formatting may be inconsistent (need to verify with `cargo fmt --check`)
- Import ordering may vary

### Impact
- **Low**: Mostly cosmetic, but affects readability

### Standard
**Standard**: Use `cargo make fmt` to enforce formatting
- **Enforcement**: Run in CI to prevent inconsistencies

## Measurement Summary

### Error Handling Patterns
- **Library files using `anyhow`** (should use `ggen_utils::error`): **90 files** ❌
- **Binaries using `anyhow`** (acceptable): 3 files ✅
- **Files using `ggen_utils::error`**: 48 files ✅
- **Inconsistency score**: **HIGH** - 90 library files violate standard

**Standard (from `.cursor/rules/rust-standards.mdc`)**:
- Libraries use typed errors via `ggen_utils::error::Result`
- `anyhow` only in binaries (CLI), not in libraries

### Return Types
- Functions returning `Option<T>`: 16
  - Most are legitimate (lookup operations where absence is not an error)
  - Examples: `find_file()`, `get_metadata()`, `last_run()`
- Functions returning `Result<T, E>`: 174
- **Inconsistency score**: **LOW** - Option usage appears appropriate

### unwrap/expect Usage
- **Total occurrences**: 3339 across 286 files
- **In production code**: Need detailed audit
- **In tests**: Most occurrences are in tests (acceptable)
- **Inconsistency score**: **MEDIUM** - Need to identify production code usage

**Standard (from `.cursor/rules/rust-standards.mdc`)**:
- Never use `unwrap()` or `expect()` in production code paths
- Allowed only in tests, examples, and `main()` functions

## Next Steps

1. **Standardize error handling**: Choose one pattern and migrate
2. **Convert Option to Result**: Review 16 functions returning `Option`
3. **Audit unwrap/expect**: Identify production code usage
4. **Enforce formatting**: Add `cargo fmt --check` to CI

