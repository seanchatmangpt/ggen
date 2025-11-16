# Documentation Improvements Plan - v2.7.0

## Overview

This document outlines documentation improvements aligned with the Coding Standards (docs/CODING_STANDARDS.md) to achieve consistent, high-quality documentation across the ggen codebase.

## Current State

**Measurements** (from Phase 4: Eliminate Mura):
- **Total public APIs**: 1,012
- **Files with doctests**: 122
- **Doctest format consistency**: Multiple formats (good, no_run, ignore)
- **Documentation ratio**: 956% (9,675 documentation lines)
- **Status**: ✅ Strong baseline - main crates well documented

---

## Documentation Standards Compliance

### ✅ Fully Compliant Modules

The following modules meet or exceed the Coding Standards:

**ggen-core** (lib.rs)
- ✅ Module-level documentation with overview
- ✅ Key modules documented
- ✅ Quick start examples
- ✅ Architecture description

**ggen-cli** (lib.rs)
- ✅ Architecture documentation
- ✅ Feature description
- ✅ Example usage
- ✅ Module organization

**ggen-ai** (lib.rs)
- ✅ Feature-rich documentation
- ✅ Provider list
- ✅ Quick start example with error handling
- ✅ Module organization

**ggen-domain** (lib.rs)
- ✅ Architecture documentation
- ✅ No CLI dependencies claim
- ✅ Module organization by functional area

**ggen-marketplace** (lib.rs)
- ✅ Architecture explanation
- ✅ Trait-based design documentation
- ✅ Feature list
- ✅ Quick start section

**ggen-utils** (lib.rs & error.rs)
- ✅ Module documentation with examples
- ✅ Error type documentation with examples
- ✅ Helper methods documented
- ✅ Usage examples with doctests

---

## Documentation Gaps Identified

### Priority 1: Item-Level Documentation

Some public functions may lack full documentation. Priority areas:

**Areas to verify**:
1. Error handling helper methods
2. Configuration builders
3. Cache/storage interfaces
4. Registry client methods
5. Template processing functions

**Standard template for item-level docs**:
```rust
/// Brief one-line summary.
///
/// Detailed description explaining the function's purpose,
/// behavior, and any important considerations.
///
/// # Arguments
///
/// * `param1` - Description of parameter
/// * `param2` - Description of parameter
///
/// # Returns
///
/// Description of return value.
///
/// # Errors
///
/// * `ErrorType::Variant` - When this error occurs
/// * `ErrorType::Other` - Other error condition
///
/// # Examples
///
/// ```rust
/// use crate::module::function;
///
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let result = function("input")?;
/// assert_eq!(result, "expected");
/// # Ok(())
/// # }
/// ```
pub fn function(param1: &str, param2: u32) -> Result<String, ErrorType> {
    // Implementation
}
```

---

## Doctest Improvements

### Current Status
- ✅ 122 files have doctests
- ✅ Doctest compilation verified in Gate 7
- ⏳ Doctest format consistency can be improved

### Doctest Standards

**All doctests must**:
1. Compile and run successfully (`cargo test --doc`)
2. Follow consistent format patterns
3. Include error case examples for fallible functions
4. Use `# fn main() -> Result<..> {` wrapper for Result-returning examples
5. Include `# Ok(())` at end for Result examples

**Doctest patterns**:

**Success case** (runnable):
```rust
/// # Examples
///
/// ```rust
/// use crate::module::function;
///
/// let result = function("input");
/// assert_eq!(result, "expected");
/// ```
```

**Error case** (runnable):
```rust
/// # Examples
///
/// ```rust
/// use crate::module::function;
///
/// let result = function("");
/// assert!(result.is_err());
/// ```
```

**Fallible function** (with error handling):
```rust
/// # Examples
///
/// ```rust
/// use crate::module::process;
///
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let result = process("valid")?;
/// assert_eq!(result, "processed");
/// # Ok(())
/// # }
/// ```
```

**External resources** (no_run):
```rust
/// # Examples
///
/// ```rust,no_run
/// use crate::module::read_file;
///
/// # async fn example() -> Result<(), Box<dyn std::error::Error>> {
/// let content = read_file("path/to/file").await?;
/// println!("{}", content);
/// # Ok(())
/// # }
/// ```
```

---

## Documentation Tasks

### Task 1: Audit Main Crates

**Objective**: Verify module-level documentation in primary crates

**Crates to verify**:
- [x] ggen-core (✅ Complete)
- [x] ggen-cli (✅ Complete)
- [x] ggen-ai (✅ Complete)
- [x] ggen-domain (✅ Complete)
- [x] ggen-marketplace (✅ Complete)
- [x] ggen-utils (✅ Complete)
- [ ] ggen-node

**Verification checklist**:
- [ ] Module has `//!` module-level documentation
- [ ] Module documentation includes:
  - [ ] One-line summary
  - [ ] Detailed description
  - [ ] Key modules section
  - [ ] Quick start example
  - [ ] Feature list (if applicable)

---

### Task 2: Document Key Modules

**Objective**: Add documentation to sub-modules lacking it

**Key modules to document**:

1. **Error Handling**
   - `ggen-utils::error` (✅ Complete)
   - `ggen-core::error` (verify)

2. **Configuration**
   - Config builders
   - Environment loading
   - Default values

3. **Cache & Storage**
   - Cache interfaces
   - Storage backends
   - Invalidation strategies

4. **Template Processing**
   - Template rendering
   - Variable resolution
   - Filter registration

5. **Registry & Marketplace**
   - Registry client
   - Package discovery
   - Installation flow

---

### Task 3: Improve Function Documentation

**Objective**: Ensure all public functions have documentation

**Process**:
1. Run: `cargo doc --document-private-items --open`
2. Identify functions without `///` documentation
3. Add documentation following standard template
4. Include at least one doctest example
5. Include error case examples for fallible functions

**Example improvement**:

```rust
// Before: No documentation
pub fn process(input: &str) -> Result<String> {
    // Implementation
}

// After: Full documentation
/// Processes input and returns uppercase result.
///
/// This function takes a string and returns it converted to uppercase.
/// Returns an error if the input is empty.
///
/// # Arguments
///
/// * `input` - The string to process
///
/// # Returns
///
/// Uppercase version of the input string.
///
/// # Errors
///
/// Returns `ProcessError::EmptyInput` if input is empty.
///
/// # Examples
///
/// ```rust
/// use crate::module::process;
///
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let result = process("hello")?;
/// assert_eq!(result, "HELLO");
/// # Ok(())
/// # }
/// ```
///
/// Error case:
///
/// ```rust
/// use crate::module::process;
///
/// let result = process("");
/// assert!(result.is_err());
/// ```
pub fn process(input: &str) -> Result<String> {
    // Implementation
}
```

---

### Task 4: Add Error Documentation

**Objective**: Document all error types and variants

**Pattern**:
```rust
/// Errors that can occur during processing
#[derive(Debug, thiserror::Error)]
pub enum ProcessError {
    /// Returned when input is empty
    #[error("Empty input provided")]
    EmptyInput,

    /// Returned when input format is invalid
    #[error("Invalid format: {0}")]
    InvalidFormat(String),

    /// Returned when I/O operation fails
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),
}

/// Module-level documentation with error examples
///
/// # Errors
///
/// Functions in this module return `ProcessError` with specific variants
/// for different failure modes:
///
/// - `ProcessError::EmptyInput` - Input is empty
/// - `ProcessError::InvalidFormat` - Input has invalid format
/// - `ProcessError::Io` - File I/O failed
```

---

### Task 5: Verify Doctest Compilation

**Objective**: Ensure all doctests compile and run

**Commands**:
```bash
# Verify doctests compile and run
cargo test --doc --workspace

# Verify documentation builds
cargo doc --workspace --no-deps

# Check for broken doc links
cargo doc --document-private-items --check 2>&1 | grep "warning:"
```

**Expected output**:
```
test result: ok. X passed; 0 failed; 0 ignored; Y measured
```

---

## Documentation Checklist

### For Each Public Module

- [ ] Has module-level `//!` documentation
- [ ] Includes one-line summary
- [ ] Includes detailed description
- [ ] Documents key types/functions
- [ ] Includes at least one example
- [ ] Example compiles and runs

### For Each Public Function

- [ ] Has `///` documentation
- [ ] Includes one-line summary
- [ ] Documents all parameters
- [ ] Documents return value
- [ ] Documents error cases (if fallible)
- [ ] Includes at least one doctest
- [ ] Includes error case example (if fallible)

### For Each Public Type

- [ ] Has `///` documentation
- [ ] Documents field meaning
- [ ] Includes usage example
- [ ] Documents construction methods

---

## Quality Gates

All documentation is validated in CI/CD through Gate 7:

**Gate 7: Documentation Consistency**
- ✅ Doctests compile and run (`cargo test --doc`)
- ✅ Documentation builds (`cargo doc`)
- ⚠️ Undocumented public APIs (soft warning, not blocking)

**Gate status**: Pass all checks before merge

---

## Documentation Tools

### Visual verification:
```bash
# Open generated documentation
cargo doc --open
cargo doc --document-private-items --open
```

### Automated checking:
```bash
# Check for dead doc links
cargo doc --document-private-items 2>&1 | grep "warning:"

# Verify doctests
cargo test --doc --workspace

# Count undocumented items
cargo doc --document-private-items 2>&1 | grep -i "missing" | wc -l
```

---

## Success Metrics

**Documentation Quality Metrics**:
- [ ] ≥95% of public APIs documented
- [ ] ≥90% of public APIs have doctests
- [ ] 100% of error types documented
- [ ] All doctests compile and run
- [ ] Zero broken doc links
- [ ] Consistent documentation format across codebase

**Effort Estimates**:
- Audit main crates: 1-2 hours
- Document key modules: 2-3 hours
- Add function documentation: 3-4 hours
- Add error documentation: 1-2 hours
- Verify compilation: 0.5-1 hour
- **Total**: 7.5-12 hours

---

## Implementation Priority

### Phase 1 (Quick wins - 1-2 hours)
1. Audit existing documentation coverage
2. Document highest-impact public functions
3. Add error case examples to existing doctests

### Phase 2 (Medium effort - 3-4 hours)
4. Complete function documentation
5. Add documentation to error types
6. Improve doctest examples

### Phase 3 (Full compliance - 2-3 hours)
7. Final verification and cleanup
8. Fix any broken doc links
9. Verify Gate 7 passes in CI

---

## References

- **Coding Standards**: `docs/CODING_STANDARDS.md`
- **Quality Gates**: `.github/workflows/quality-gates.yml` (Gate 7)
- **Test Framework**: `chicago-tdd-tools` prelude
- **Rust Doc Guide**: https://doc.rust-lang.org/rustdoc/

---

## Integration with Standards

This documentation plan directly implements the **Documentation Standard** from `docs/CODING_STANDARDS.md`:

✅ **Module-Level Documentation** (`//!`):
- Purpose and overview
- Key modules/types
- Usage examples

✅ **Item-Level Documentation** (`///`):
- One-line summary
- Detailed description
- Arguments/returns/errors
- Examples with doctests

✅ **Doctest Standards**:
- Compile and run successfully
- Use consistent format (`# fn main() -> Result<..>`)
- Include error case examples
- Test actual behavior

✅ **Quality Control**:
- Automated in Gate 7 (Quality Gates)
- Prevents regressions
- Enforced on every PR

---

**Status**: Ready for implementation
**Last Updated**: 2025-11-16
**Target Completion**: Before v2.7.0 release
