<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Mura (Unevenness) Inventory - Current State](#mura-unevenness-inventory---current-state)
  - [Measurement Summary](#measurement-summary)
    - [Documentation Consistency](#documentation-consistency)
    - [Error Handling Pattern Consistency](#error-handling-pattern-consistency)
    - [Return Type Consistency](#return-type-consistency)
    - [Doctest Format Consistency](#doctest-format-consistency)
    - [Code Style Consistency](#code-style-consistency)
  - [Priority Issues](#priority-issues)
    - [Priority 1: Documentation Inconsistency (HIGH)](#priority-1-documentation-inconsistency-high)
    - [Priority 2: Error Handling Pattern Inconsistency (HIGH)](#priority-2-error-handling-pattern-inconsistency-high)
    - [Priority 3: Doctest Format Inconsistency (MEDIUM)](#priority-3-doctest-format-inconsistency-medium)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Mura (Unevenness) Inventory - Current State

This document tracks current inconsistencies identified in the ggen codebase that need to be standardized.

## Measurement Summary

### Documentation Consistency

- **Public APIs**: 1,172 total
- **Documentation comments**: 12,641 (includes module-level and item-level docs)
- **Runnable doctests**: 178 (15% of public APIs)
- **Non-runnable doctests**: 285 (24% of public APIs)
- **Total doctests**: 463 (40% of public APIs)
- **Undocumented APIs**: ~709 (60% of public APIs)
- **Inconsistency score**: **HIGH** - 60% of public APIs lack doctests

**Issues**:
- Many public APIs have documentation but no doctest examples
- Inconsistent doctest formats (some runnable, some `no_run`)
- Missing error case examples in doctests

### Error Handling Pattern Consistency

- **Library files using `anyhow`** (should use `ggen_utils::error`): **88 files** ❌
- **Binaries using `anyhow`** (acceptable): 3 files ✅
- **Files using `ggen_utils::error`**: 48 files ✅
- **Inconsistency score**: **HIGH** - 88 library files violate standard

**Standard (from `.cursor/rules/rust-standards.mdc`)**:
- Libraries use typed errors via `ggen_utils::error::Result`
- `anyhow` only in binaries (CLI), not in libraries

### Return Type Consistency

- **Functions returning `Option<T>`**: 45
- **Functions returning `Result<T, E>`**: 341
- **Inconsistency score**: **LOW** - Option usage appears appropriate for lookup operations

**Standard**: Use `Result<T, E>` for all fallible operations
- **Exception**: Only use `Option<T>` when absence is not an error (e.g., optional configuration)

### Doctest Format Consistency

- **Runnable doctests**: 178 (38% of total doctests)
- **Non-runnable doctests**: 285 (62% of total doctests)
- **Inconsistency score**: **MEDIUM** - Many doctests could be runnable but are marked `no_run`

**Issues**:
- Some simple examples are marked `no_run` unnecessarily
- Inconsistent use of `# fn main()` pattern
- Missing assertions in some doctests

### Code Style Consistency

- **Formatting**: Need to verify with `cargo make fmt --check`
- **Import ordering**: May vary across files
- **Inconsistency score**: **UNKNOWN** - Need to measure

## Priority Issues

### Priority 1: Documentation Inconsistency (HIGH)

**Problem**: 60% of public APIs lack doctest examples

**Impact**:
- Developers can't easily understand API usage
- No compile-time verification of examples
- Inconsistent developer experience

**Standard**:
- All public APIs must have doctest examples
- Use runnable doctests when possible
- Include error case examples for fallible operations

### Priority 2: Error Handling Pattern Inconsistency (HIGH)

**Problem**: 88 library files use `anyhow::Result` when they should use `ggen_utils::error::Result`

**Impact**:
- Makes error handling inconsistent
- Type incompatibility between modules
- Maintenance burden

**Standard**:
- Libraries use `ggen_utils::error::Result`
- `anyhow` only in binaries

### Priority 3: Doctest Format Inconsistency (MEDIUM)

**Problem**: 62% of doctests are marked `no_run`, many unnecessarily

**Impact**:
- Examples don't verify at compile time
- Inconsistent developer experience

**Standard**:
- Use runnable doctests for simple examples
- Use `no_run` only for I/O, async, or complex setup
- Always include `# fn main()` pattern

## Next Steps

1. **Add doctests to undocumented APIs**: Focus on high-value public APIs first
2. **Standardize error handling**: Migrate 88 library files from `anyhow` to `ggen_utils::error`
3. **Convert unnecessary `no_run` doctests**: Make simple examples runnable
4. **Enforce standards**: Add checks to CI

