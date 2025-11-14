# Standardization Plan - Eliminate Mura

This document outlines the plan to standardize inconsistencies identified in the Mura inventory.

## Priority 1: Documentation Consistency (HIGH)

### Problem
- 60% of public APIs (709 APIs) lack doctest examples
- Inconsistent doctest formats (178 runnable vs 285 non-runnable)
- Missing error case examples

### Standard
**From `.cursor/rules/rust-standards.mdc` and documentation standards**:
- All public APIs must have doctest examples
- Use runnable doctests when possible (for simple operations)
- Use `no_run` only for I/O, async, or complex setup
- Always include `# fn main() -> anyhow::Result<()> {` pattern for fallible examples
- Include error case examples for Result-returning functions

### Migration Strategy

#### Phase 1: High-Value APIs First
1. Focus on frequently-used public APIs
2. Start with simple constructors and utility functions
3. Add runnable doctests with assertions

#### Phase 2: Convert Unnecessary `no_run`
1. Identify simple examples marked `no_run` unnecessarily
2. Convert to runnable doctests
3. Add `# fn main()` pattern where needed

#### Phase 3: Add Missing Doctests
1. Identify undocumented public APIs
2. Add doctests following standard pattern
3. Prioritize by usage frequency

### Example Standardization

**Before**:
```rust
/// Create a new instance
pub fn new() -> Self {
    // ...
}
```

**After**:
```rust
/// Create a new instance
///
/// # Examples
///
/// ```rust
/// use crate::module::Type;
///
/// # fn main() {
/// let instance = Type::new();
/// assert_eq!(instance.field, expected_value);
/// # }
/// ```
pub fn new() -> Self {
    // ...
}
```

## Priority 2: Error Handling Pattern Standardization (HIGH)

### Problem
88 library files use `anyhow::Result` when they should use `ggen_utils::error::Result`.

### Standard
**From `.cursor/rules/rust-standards.mdc`**:
- Libraries use typed errors via `ggen_utils::error::Result`
- `anyhow` only in binaries (CLI), not in libraries

### Migration Strategy

#### Phase 1: Identify All Violations
```bash
find crates -name "*.rs" -path "*/src/*" ! -path "*/tests/*" ! -path "*/examples/*" ! -name "main.rs" | xargs grep -l "use.*anyhow\|anyhow::Result"
```

#### Phase 2: Create Migration Script
1. Replace `use anyhow::Result` with `use ggen_utils::error::Result`
2. Replace `anyhow::Result<T>` with `Result<T>` (after import change)
3. Replace `anyhow::Context` with `ggen_utils::error::Error::with_context`
4. Replace `anyhow::bail!` with `return Err(Error::new(...))`
5. Replace `anyhow::ensure!` with manual checks

#### Phase 3: Apply Migration
- Start with `ggen-core` (foundation crate)
- Then migrate dependent crates
- Verify compilation after each crate

#### Phase 4: Verification
- Run `cargo make check` to verify compilation
- Run `cargo make test` to verify functionality
- Check for any remaining `anyhow` usage in libraries

### Example Migration

**Before**:
```rust
use anyhow::{Context, Result};

pub fn load_config(path: &Path) -> Result<Config> {
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Failed to read config: {}", path.display()))?;
    toml::from_str(&content)
        .context("Failed to parse config")
}
```

**After**:
```rust
use ggen_utils::error::{Error, Result};

pub fn load_config(path: &Path) -> Result<Config> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| Error::with_context(
            "Failed to read config",
            &format!("{}: {}", path.display(), e)
        ))?;
    toml::from_str(&content)
        .map_err(|e| Error::with_context("Failed to parse config", &e.to_string()))
}
```

## Priority 3: Doctest Format Standardization (MEDIUM)

### Problem
62% of doctests (285) are marked `no_run`, many unnecessarily.

### Standard
- Use runnable doctests for simple operations
- Use `no_run` only for I/O, async, or complex setup
- Always include `# fn main()` pattern for consistency

### Migration Strategy

#### Phase 1: Identify Convertible Doctests
1. Find simple examples marked `no_run`
2. Check if they can be made runnable
3. Convert to runnable with assertions

#### Phase 2: Standardize Pattern
1. Add `# fn main()` wrapper where missing
2. Add assertions to verify behavior
3. Ensure consistent formatting

## Implementation Order

1. **Documentation Consistency** (Start here - highest impact, lowest risk)
   - Add doctests to high-value APIs
   - Convert unnecessary `no_run` to runnable
   - Standardize doctest patterns

2. **Error Handling Standardization** (High impact, medium risk)
   - Migrate library files from `anyhow` to `ggen_utils::error`
   - Verify after each crate migration

3. **Doctest Format Standardization** (Medium impact, low risk)
   - Convert unnecessary `no_run` to runnable
   - Standardize patterns

## Success Criteria

- **Documentation**: 80%+ of public APIs have doctests
- **Error Handling**: 0 library files use `anyhow` (only binaries)
- **Doctest Format**: 70%+ of doctests are runnable (up from 38%)

