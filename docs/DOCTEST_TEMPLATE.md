<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Doctest Template - Standard Format for ggen](#doctest-template---standard-format-for-ggen)
  - [Standard Doctest Pattern](#standard-doctest-pattern)
    - [For Fallible Functions (Return `Result<T, E>`)](#for-fallible-functions-return-resultt-e)
    - [For Infallible Functions (Return `T`)](#for-infallible-functions-return-t)
    - [For Async Functions](#for-async-functions)
  - [Doctest Format Guidelines](#doctest-format-guidelines)
    - [When to Use `no_run`](#when-to-use-no_run)
    - [When to Use Runnable Doctests](#when-to-use-runnable-doctests)
    - [Error Case Examples](#error-case-examples)
  - [Complete Example](#complete-example)
  - [Checklist](#checklist)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Doctest Template - Standard Format for ggen

This document provides templates for writing consistent, comprehensive doctests in ggen.

## Standard Doctest Pattern

### For Fallible Functions (Return `Result<T, E>`)

```rust
/// Brief description of the function.
///
/// Longer description explaining what the function does, when to use it,
/// and any important considerations.
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
/// Returns an error if:
/// - Error condition 1
/// - Error condition 2
///
/// # Examples
///
/// ## Success case
///
/// ```rust,no_run
/// use crate::module::function;
///
/// # fn main() -> anyhow::Result<()> {
/// let result = function("input")?;
/// assert_eq!(result, "expected");
/// # Ok(())
/// # }
/// ```
///
/// ## Error case - [Specific error condition]
///
/// ```rust,no_run
/// use crate::module::function;
///
/// # fn main() -> anyhow::Result<()> {
/// // This will fail because [reason]
/// let result = function("invalid_input");
/// assert!(result.is_err());
/// // Error message indicates [what error message says]
/// # Ok(())
/// # }
/// ```
pub fn function(param1: &str, param2: u32) -> Result<String, ErrorType> {
    // Implementation
}
```

### For Infallible Functions (Return `T`)

```rust
/// Brief description of the function.
///
/// Longer description explaining what the function does.
///
/// # Arguments
///
/// * `param1` - Description of parameter
///
/// # Returns
///
/// Description of return value.
///
/// # Examples
///
/// ```rust
/// use crate::module::function;
///
/// let result = function("input");
/// assert_eq!(result, "expected");
/// ```
pub fn function(param1: &str) -> String {
    // Implementation
}
```

### For Async Functions

```rust
/// Brief description of the async function.
///
/// # Examples
///
/// ## Success case
///
/// ```rust,no_run
/// use crate::module::async_function;
///
/// # async fn example() -> anyhow::Result<()> {
/// let result = async_function("input").await?;
/// assert_eq!(result, "expected");
/// # Ok(())
/// # }
/// ```
///
/// ## Error case
///
/// ```rust,no_run
/// use crate::module::async_function;
///
/// # async fn example() -> anyhow::Result<()> {
/// let result = async_function("invalid").await;
/// assert!(result.is_err());
/// # Ok(())
/// # }
/// ```
pub async fn async_function(input: &str) -> Result<String, ErrorType> {
    // Implementation
}
```

## Doctest Format Guidelines

### When to Use `no_run`

Use `no_run` when:
- The example requires external resources (files, network, etc.)
- The example would fail in CI environment
- The example is for demonstration only

**Pattern**: Always use `no_run` for examples that:
- Read/write files
- Make network requests
- Require specific environment setup
- Use async/await (use `# async fn example()` pattern)

### When to Use Runnable Doctests

Use runnable doctests (no `no_run`) when:
- The example is self-contained
- The example can run in any environment
- The example verifies actual behavior

**Pattern**: Use runnable doctests for:
- Simple data transformations
- Type conversions
- Pure functions
- Unit test-like examples

### Error Case Examples

**Required for all fallible functions**:
- At least one error case example
- Show what causes the error
- Show how to check for errors (`assert!(result.is_err())`)
- Comment explaining what the error message indicates

**Pattern**:
```rust
/// ## Error case - [Specific condition]
///
/// ```rust,no_run
/// use crate::module::function;
///
/// # fn main() -> anyhow::Result<()> {
/// // This will fail because [reason]
/// let result = function("invalid");
/// assert!(result.is_err());
/// // Error message indicates [what error says]
/// # Ok(())
/// # }
/// ```
```

## Complete Example

```rust
/// Process input data and return result.
///
/// This function validates input and processes it according to business rules.
///
/// # Arguments
///
/// * `input` - Input string to process (must be non-empty)
///
/// # Returns
///
/// Processed string on success.
///
/// # Errors
///
/// Returns an error if:
/// - Input is empty (`ProcessingError::EmptyInput`)
/// - Input format is invalid (`ProcessingError::InvalidFormat`)
///
/// # Examples
///
/// ## Success case
///
/// ```rust
/// use crate::module::process;
///
/// let result = process("hello").unwrap();
/// assert_eq!(result, "HELLO");
/// ```
///
/// ## Error case - Empty input
///
/// ```rust
/// use crate::module::process;
///
/// let result = process("");
/// assert!(result.is_err());
/// ```
///
/// ## Error case - Invalid format
///
/// ```rust,no_run
/// use crate::module::process;
///
/// # fn main() -> anyhow::Result<()> {
/// // This will fail because input contains invalid characters
/// let result = process("hello@world");
/// assert!(result.is_err());
/// // Error message indicates invalid format
/// # Ok(())
/// # }
/// ```
pub fn process(input: &str) -> Result<String, ProcessingError> {
    if input.is_empty() {
        return Err(ProcessingError::EmptyInput);
    }
    // ... processing logic
    Ok(input.to_uppercase())
}
```

## Checklist

For every public function, ensure:
- [ ] Brief one-line description
- [ ] Detailed description
- [ ] Arguments documented (if any)
- [ ] Return value documented
- [ ] Errors documented (if fallible)
- [ ] At least one success case example
- [ ] At least one error case example (if fallible)
- [ ] Examples use consistent format (`# fn main() -> anyhow::Result<()>` pattern)
- [ ] Examples are runnable or marked `no_run` appropriately
- [ ] Error examples show how to check for errors

