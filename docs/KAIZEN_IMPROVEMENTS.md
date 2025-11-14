# Kaizen Improvements Log

This document tracks small, incremental improvements made to the codebase following Kaizen principles.

## Improvement #1: Migrate pipeline.rs from anyhow to ggen_utils::error

**Date**: 2025-01-XX  
**File**: `crates/ggen-core/src/pipeline.rs`  
**Type**: Error Handling Standardization

### What Changed
- Replaced `use anyhow::Result;` with `use ggen_utils::error::{Error, Result};`
- Replaced `anyhow::anyhow!()` macro calls with `Error::new()`
- Updated documentation examples to use `ggen_utils::error::Result`

### Why This Improvement
- **Aligns with standards**: Libraries should use `ggen_utils::error::Result` per `.cursor/rules/rust-standards.mdc`
- **Consistency**: Matches error handling pattern used in other library modules
- **Type safety**: `ggen_utils::error::Error` provides better type safety than `anyhow`

### Changes Made
1. **Import change**:
   ```rust
   // Before
   use anyhow::Result;
   
   // After
   use ggen_utils::error::{Error, Result};
   ```

2. **Error construction**:
   ```rust
   // Before
   return Err(anyhow::anyhow!("File already exists: {}", path));
   
   // After
   return Err(Error::new(&format!("File already exists: {}", path)));
   ```

3. **Documentation examples**: Updated all doc examples to use `ggen_utils::error::Result`

### Verification
- ✅ Code compiles successfully
- ✅ No linter errors
- ✅ Tests still pass (test code can use `anyhow`)
- ✅ No `anyhow` usage in production code (only in `#[cfg(test)]` module)
- ✅ Fixed `regex::Error` conversion by using explicit `map_err`

### Technical Details
**Error conversion**: `regex::Error` doesn't have automatic `From` implementation, so we use explicit conversion:
```rust
// Before
if regex::Regex::new(skip_if)?.is_match(&existing_content) {

// After
let regex = regex::Regex::new(skip_if)
    .map_err(|e| Error::new(&format!("Invalid regex pattern '{}': {}", skip_if, e)))?;
if regex.is_match(&existing_content) {
```

### Pattern Established
**Standard**: Library modules should use `ggen_utils::error::Result`, not `anyhow::Result`

**When to apply**: 
- All library crates (`ggen-core`, `ggen-utils`, `ggen-domain`, etc.)
- Exception: Test modules can use `anyhow` for convenience

**Next candidates**: 
- `crates/ggen-core/src/preprocessor.rs`
- `crates/ggen-core/src/telemetry.rs`
- `crates/ggen-core/src/cache.rs`
- `crates/ggen-core/src/registry.rs`

### Impact
- **Files improved**: 1
- **Lines changed**: ~10
- **Risk**: Low (type-only change, no logic changes)
- **Value**: High (aligns with project standards)

