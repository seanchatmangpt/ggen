# Standardization Plan - Eliminate Mura

This document outlines the plan to standardize inconsistencies identified in the Mura inventory.

## Priority 1: Error Handling Pattern Standardization

### Problem
90 library files use `anyhow::Result` when they should use `ggen_utils::error::Result`.

### Standard
**From `.cursor/rules/rust-standards.mdc`**:
- Libraries use typed errors via `ggen_utils::error::Result`
- `anyhow` only in binaries (CLI), not in libraries

### Migration Strategy

#### Phase 1: Identify All Violations
```bash
# Find all library files using anyhow
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
            &format!("Failed to read config: {}", path.display()),
            &e.to_string()
        ))?;
    toml::from_str(&content)
        .map_err(|e| Error::with_context("Failed to parse config", &e.to_string()))
}
```

## Priority 2: unwrap/expect Audit

### Problem
3339 occurrences of `unwrap()`/`expect()` need to be audited to identify production code usage.

### Standard
- Never use `unwrap()` or `expect()` in production code paths
- Allowed only in tests, examples, and `main()` functions

### Audit Strategy

#### Step 1: Filter Production Code
```bash
# Find unwrap/expect in production code (exclude tests, examples, main.rs)
find crates -name "*.rs" -path "*/src/*" ! -path "*/tests/*" ! -path "*/examples/*" ! -name "main.rs" | xargs grep -n "\.unwrap()\|\.expect("
```

#### Step 2: Categorize Findings
- **Legitimate**: In tests, examples, or `main()` functions
- **Needs Fix**: In library code (production paths)

#### Step 3: Fix Production Code
- Replace with proper error handling
- Use `Result` return types
- Provide meaningful error messages

### Example Fix

**Before**:
```rust
pub fn parse_config(path: &Path) -> Config {
    let content = std::fs::read_to_string(path).unwrap();
    toml::from_str(&content).unwrap()
}
```

**After**:
```rust
pub fn parse_config(path: &Path) -> Result<Config> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| Error::with_context(
            &format!("Failed to read config: {}", path.display()),
            &e.to_string()
        ))?;
    toml::from_str(&content)
        .map_err(|e| Error::with_context("Failed to parse config", &e.to_string()))
}
```

## Priority 3: Code Style Enforcement

### Problem
Need to ensure consistent formatting across codebase.

### Standard
- Use `cargo make fmt` to enforce formatting
- Run in CI to prevent inconsistencies

### Implementation
1. Add `cargo make fmt --check` to CI
2. Fail CI if formatting is inconsistent
3. Document formatting standards in CONTRIBUTING.md

## Implementation Timeline

### Week 1: Error Handling Migration
- [ ] Day 1-2: Migrate `ggen-core` crate
- [ ] Day 3-4: Migrate `ggen-utils` and `ggen-domain` crates
- [ ] Day 5: Migrate remaining crates
- [ ] Verification: Run full test suite

### Week 2: unwrap/expect Audit
- [ ] Day 1-2: Audit and categorize all occurrences
- [ ] Day 3-4: Fix production code violations
- [ ] Day 5: Verification and testing

### Week 3: Style Enforcement
- [ ] Day 1: Add CI checks
- [ ] Day 2: Fix any formatting issues
- [ ] Day 3: Document standards

## Success Criteria

- ✅ Zero library files using `anyhow::Result`
- ✅ Zero `unwrap()`/`expect()` in production code
- ✅ All code passes `cargo make fmt --check`
- ✅ All tests pass after migration
- ✅ No regressions in functionality

## Risk Mitigation

- **Risk**: Large migration may introduce bugs
- **Mitigation**: Migrate incrementally, test after each crate
- **Risk**: Breaking changes in error types
- **Mitigation**: Use type aliases during transition if needed

