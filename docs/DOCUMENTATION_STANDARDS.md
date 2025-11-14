# Documentation Standards for ggen

This document defines the standards for code documentation in the ggen project, ensuring consistency and quality across all modules.

## Module-Level Documentation (`//!`)

All public modules must have module-level documentation using `//!` comments.

### Top-Level Crates (`lib.rs`)

**Format**: Use markdown heading format with `#`

```rust
//! # crate-name - Brief description
//!
//! Longer description of what the crate provides and its purpose.
//!
//! ## Architecture
//!
//! Description of the crate's architecture and design decisions.
//!
//! ## Features
//!
//! - **Feature 1**: Description
//! - **Feature 2**: Description
//!
//! ## Quick Start
//!
//! ```rust,no_run
//! // Example code
//! ```
//!
//! ## Module Organization
//!
//! - `module1` - Description
//! - `module2` - Description
```

**Required Sections**:
- Title with `#` heading
- Brief description
- Architecture (for complex crates)
- Features list
- Quick Start example
- Module Organization (for crates with multiple modules)

### Sub-Modules (`mod.rs` or individual files)

**Format**: Use descriptive title without `#` (unless it's a major module)

```rust
//! Module name - Brief description
//!
//! Detailed description of what this module provides.
//!
//! ## Features
//!
//! - **Feature 1**: Description
//! - **Feature 2**: Description
//!
//! ## Examples
//!
//! ### Example Title
//!
//! ```rust,no_run
//! // Example code
//! ```
```

**Required Sections**:
- Title and brief description
- Features list (if applicable)
- Examples (at least one for public modules)

**Optional Sections**:
- Architecture (for complex modules)
- Configuration (if applicable)
- Error Handling (if relevant)

## Function Documentation (`///`)

All public functions must have documentation comments.

### Format

```rust
/// Brief one-line description.
///
/// Longer description explaining what the function does, when to use it,
/// and any important details.
///
/// # Arguments
///
/// * `arg1` - Description of argument
/// * `arg2` - Description of argument
///
/// # Returns
///
/// Description of return value and error conditions.
///
/// # Errors
///
/// * `ErrorType1` - When this error occurs
/// * `ErrorType2` - When this error occurs
///
/// # Examples
///
/// ```rust,no_run
/// // Example usage
/// ```
```

### Required Elements

- **Brief description**: One-line summary
- **Detailed description**: When needed for complex functions
- **Arguments**: Document all parameters
- **Returns**: Document return value and error conditions
- **Examples**: At least one example for public APIs

## Type Documentation (`///`)

All public types (structs, enums, traits) must have documentation.

### Format

```rust
/// Brief description of the type.
///
/// Detailed description explaining the purpose, usage, and important
/// design decisions.
///
/// # Examples
///
/// ```rust,no_run
/// // Example usage
/// ```
```

## Documentation Quality Standards

### Completeness

- ✅ All public APIs must be documented
- ✅ All modules must have module-level docs
- ✅ Examples should compile and run (use `rust,no_run` if needed)
- ✅ Error conditions must be documented

### Consistency

- ✅ Use consistent terminology across documentation
- ✅ Follow the same structure for similar modules
- ✅ Use markdown formatting consistently
- ✅ Include code examples where helpful

### Clarity

- ✅ Write clear, concise descriptions
- ✅ Explain "why" not just "what" for complex logic
- ✅ Use proper grammar and spelling
- ✅ Avoid jargon without explanation

## Examples

### Good Module Documentation

```rust
//! Template registry client for gpack discovery and resolution
//!
//! This module provides a client for interacting with the ggen template registry
//! (registry.ggen.dev). It handles fetching pack metadata, searching for templates,
//! and resolving pack versions for installation.
//!
//! ## Features
//!
//! - **Registry Index**: Fetch and parse the central registry index
//! - **Search**: Search for packs by name, description, tags, and keywords
//! - **Version Resolution**: Resolve pack IDs to specific versions with metadata
//! - **Retry Logic**: Automatic retry with exponential backoff for network failures
//!
//! ## Examples
//!
//! ### Basic Search
//!
//! ```rust,no_run
//! use ggen_core::registry::RegistryClient;
//!
//! # async fn example() -> anyhow::Result<()> {
//! let client = RegistryClient::new()?;
//! let results = client.search("rust").await?;
//! for result in results {
//!     println!("Found: {} - {}", result.name, result.description);
//! }
//! # Ok(())
//! # }
//! ```
```

### Good Function Documentation

```rust
/// Resolve a pack ID to a specific version with metadata.
///
/// This function fetches the pack metadata from the registry and resolves
/// it to a specific version. If no version is specified, it returns the
/// latest stable version.
///
/// # Arguments
///
/// * `pack_id` - The pack identifier (e.g., "io.ggen.rust.api")
/// * `version` - Optional version constraint (e.g., "1.0.0" or "^1.0")
///
/// # Returns
///
/// A `ResolvedPack` containing the pack metadata, git URL, and checksum.
///
/// # Errors
///
/// * `RegistryError::NotFound` - If the pack doesn't exist
/// * `RegistryError::NetworkError` - If the registry is unreachable
/// * `RegistryError::ParseError` - If the registry response is invalid
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_core::registry::RegistryClient;
///
/// # async fn example() -> anyhow::Result<()> {
/// let client = RegistryClient::new()?;
/// let resolved = client.resolve("io.ggen.rust.api", None).await?;
/// println!("Pack: {}@{}", resolved.id, resolved.version);
/// # Ok(())
/// # }
/// ```
```

## Enforcement

### Automated Checks

- Documentation coverage can be checked with `cargo doc --no-deps`
- Missing documentation will show warnings in `cargo doc`
- CI should verify documentation completeness

### Code Review

- All PRs must include documentation for new public APIs
- Reviewers should check documentation quality and completeness
- Examples should be tested to ensure they compile

## References

- [Rust API Guidelines - Documentation](https://rust-lang.github.io/api-guidelines/documentation.html)
- [Rust Book - Documentation Comments](https://doc.rust-lang.org/book/ch14-02-publishing-to-crates-io.html#making-useful-documentation-comments)
- [CONTRIBUTING.md](../CONTRIBUTING.md) - Project contribution guidelines

