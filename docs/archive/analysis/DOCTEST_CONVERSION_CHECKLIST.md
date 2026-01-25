<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Doctest Conversion Checklist](#doctest-conversion-checklist)
  - [CRITICAL: Always Verify After Conversion](#critical-always-verify-after-conversion)
  - [Conversion Criteria Checklist](#conversion-criteria-checklist)
  - [Conversion Steps](#conversion-steps)
  - [Error Handling Patterns](#error-handling-patterns)
    - [For Infallible Functions](#for-infallible-functions)
    - [For Fallible Functions (Simple)](#for-fallible-functions-simple)
    - [For Fallible Functions (Complex)](#for-fallible-functions-complex)
  - [Verification Commands](#verification-commands)
  - [Common Mistakes to Avoid](#common-mistakes-to-avoid)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Doctest Conversion Checklist

## CRITICAL: Always Verify After Conversion

**MANDATORY STEP**: After converting any doctest from `no_run` to runnable, you MUST:

1. ✅ Run `cargo make test-doc` to verify doctest compiles and runs
2. ✅ Verify doctest actually tests the intended behavior (has assertions)
3. ✅ Check that no existing doctests were broken
4. ✅ Verify conversion follows standards (consistent error handling)

## Conversion Criteria Checklist

Before converting a `no_run` doctest to runnable, verify:

- [ ] ✅ No file I/O required (no `std::fs`, `Path::new()` with real files)
- [ ] ✅ No network access required (no HTTP requests, registry calls)
- [ ] ✅ No async operations (or uses `# async fn example()` pattern)
- [ ] ✅ Pure function or simple constructor
- [ ] ✅ In-memory operations only

## Conversion Steps

1. **Identify candidate** - Check if doctest meets conversion criteria
2. **Convert syntax** - Remove `no_run`, fix error handling (`unwrap()` vs `?`)
3. **Add assertions** - Ensure doctest verifies behavior, not just compiles
4. **Verify** - Run `cargo make test-doc` immediately
5. **Check existing** - Verify no other doctests were broken
6. **Document** - Update conversion log if needed

## Error Handling Patterns

### For Infallible Functions
```rust
/// ```rust
/// use crate::module::Type;
///
/// let instance = Type::new();
/// assert_eq!(instance.field, expected);
/// ```
```

### For Fallible Functions (Simple)
```rust
/// ```rust
/// use crate::module::Type;
///
/// let instance = Type::new().unwrap();
/// assert_eq!(instance.field, expected);
/// ```
```

### For Fallible Functions (Complex)
```rust
/// ```rust
/// use crate::module::Type;
///
/// # fn main() -> anyhow::Result<()> {
/// let instance = Type::new()?;
/// assert_eq!(instance.field, expected);
/// # Ok(())
/// # }
/// ```
```

## Verification Commands

```bash
# Verify specific doctest
cargo test --doc --package ggen-core <module>::<function>

# Verify all doctests
cargo make test-doc

# Verify no regressions
cargo test --doc --workspace
```

## Common Mistakes to Avoid

- ❌ Converting doctest that requires file I/O
- ❌ Converting doctest that requires network
- ❌ Forgetting to run `cargo make test-doc` after conversion
- ❌ Converting without adding assertions
- ❌ Using inconsistent error handling patterns
- ❌ Breaking existing runnable doctests

