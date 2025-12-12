<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Development Workflow](#development-workflow)
  - [Overview](#overview)
  - [Development Commands](#development-commands)
    - [Required: Use `cargo make` Commands](#required-use-cargo-make-commands)
    - [Timeout SLAs](#timeout-slas)
  - [Testing Strategy](#testing-strategy)
    - [Chicago TDD (Test-Driven Development)](#chicago-tdd-test-driven-development)
      - [Test Macros](#test-macros)
      - [Testing Principles](#testing-principles)
      - [Test Execution](#test-execution)
  - [Error Handling Standards](#error-handling-standards)
    - [Error Types](#error-types)
    - [Error Construction](#error-construction)
    - [Error Handling Patterns](#error-handling-patterns)
  - [Andon Signals (Quality Assurance)](#andon-signals-quality-assurance)
    - [Signal Types](#signal-types)
    - [Andon Workflow](#andon-workflow)
    - [Verification Before Completion](#verification-before-completion)
  - [Code Quality Standards](#code-quality-standards)
    - [Prohibited Patterns](#prohibited-patterns)
    - [Required Patterns](#required-patterns)
  - [Continuous Improvement (Kaizen)](#continuous-improvement-kaizen)
    - [Kaizen Workflow](#kaizen-workflow)
    - [Improvement Criteria](#improvement-criteria)
  - [Documentation Standards](#documentation-standards)
    - [Module Documentation](#module-documentation)
    - [Public API Documentation](#public-api-documentation)
    - [Documentation Waste Prevention (Muda Elimination)](#documentation-waste-prevention-muda-elimination)
  - [Git Workflow](#git-workflow)
    - [Pre-Commit Hooks](#pre-commit-hooks)
    - [Pre-Push Hooks](#pre-push-hooks)
  - [Performance SLOs](#performance-slos)
  - [Resources](#resources)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Development Workflow

This document describes the development workflow for ggen, including testing practices, error handling standards, and quality assurance processes.

## Overview

The ggen development workflow follows Lean Six Sigma principles with a focus on:
- **Continuous Improvement (Kaizen)**: Small, incremental improvements
- **Quality Assurance (Andon Signals)**: Visual problem indicators with "stop the line" workflow
- **Deterministic Testing**: Chicago TDD with behavior verification
- **Error Prevention**: Type-safe error handling throughout

## Development Commands

### Required: Use `cargo make` Commands

**CRITICAL**: All development workflows MUST use `cargo make` commands. **NEVER use direct `cargo` commands**.

```bash
# Quick feedback (5s timeout)
cargo make check          # Compile check
cargo make test-unit       # Unit tests only
cargo make lint            # Clippy linting

# Full validation (longer timeouts)
cargo make test            # All tests (10s unit + 30s integration)
cargo make pre-commit      # Format + lint + test-unit
cargo make ci              # Full CI pipeline

# Specialized commands
cargo make timeout-check   # Verify timeout command exists
cargo make test-timings    # Identify slow tests
cargo make completions     # Generate shell completions
```

### Quickstart (cargo make only)

1. `cargo make timeout-check` (verify timeouts exist)
2. `cargo make check` (compile; stop on any warning/error)
3. `cargo make test-unit` (fast loop) → `cargo make test` (full suite)
4. `cargo make lint` (clippy; zero warnings)
5. `cargo make slo-check` (performance SLO guardrails)
6. `cargo make audit` (security advisories) + `cargo make validate-templates` + `cargo make validate-rdf`
7. For single tests: `cargo make test test_name` (respect timeout wrappers)

### Toolchain / MSRV

- Rust stable pinned; MSRV: 1.74+ (edition 2021) per workspace toolchain.
- Use `rustup show` to confirm; do not upgrade toolchain without coordination.
- Feature gating: enable optional features explicitly; avoid cfg mazes.

### Timeout SLAs

All CLI commands have timeout wrappers to prevent indefinite hangs:

- **Quick checks**: `timeout 5s` (cargo check, cargo fmt, cargo clippy)
- **Compilation**: `timeout 10s` (cargo build debug)
- **Release builds**: `timeout 30s` (cargo build --release)
- **Unit tests**: `timeout 10s` (cargo test --lib)
- **Integration tests**: `timeout 30s` (cargo test --test)
- **Pre-push hooks**: `timeout 30s` (cargo make check-pre-push) - Longer timeout for lock contention
- **Security/validation**: `timeout 60s` (cargo make audit, cargo make validate-templates, cargo make validate-rdf)

## Testing Strategy

### Chicago TDD (Test-Driven Development)

ggen uses Chicago School TDD with state-based testing and behavior verification.

#### Test Macros

Use `chicago_tdd_tools` macros instead of standard `#[test]` attributes:

```rust
use chicago_tdd_tools::prelude::*;  // prelude::* imports all common macros and types

// Synchronous test
test!(test_name, {
    // Arrange
    let input = "test";
    
    // Act
    let result = function_under_test(input);
    
    // Assert - verify observable outputs
    assert_eq!(result, expected_value);
});

// Async test
async_test!(test_async_function, {
    // Arrange
    let input = "test";
    
    // Act
    let result = async_function(input).await;
    
    // Assert
    assert_eq!(result, expected_value);
});

// Test with Result handling
test!(test_with_result, {
    // Arrange
    let result: Result<u32, String> = Ok(42);
    
    // Act & Assert
    assert_ok!(&result, "Result should be Ok");
    if let Ok(value) = result {
        assert_eq!(value, 42);
    }
});
```

**Note**: Assertion macros like `assert_ok!` and `assert_err!` are exported with `#[macro_export]`, so they're available at crate root automatically. You can use them directly or with fully qualified paths like `chicago_tdd_tools::assert_ok!`.

#### Testing Principles

1. **State-Based Testing**: Verify outputs and state changes, not implementation details
2. **Real Collaborators**: Use real objects, minimize mocks
3. **Behavior Verification**: Verify what code does (observable outputs/state changes)
4. **AAA Pattern**: Arrange-Act-Assert required
5. **No Meaningless Tests**: Tests must verify observable outputs, not just `assert_ok!()`

#### Test Execution

```bash
# Single-threaded for determinism
cargo make test-single-threaded

# Fixed seed for reproducibility
cargo make deterministic

# Identify slow tests
cargo make test-timings
```

## Determinism and Reproducibility

- **Seeds**: Use `cargo make deterministic` (fixed seed) and `cargo make test-single-threaded` to eliminate scheduling variance. For generators, set explicit seeds (`GGEN_SEED` or command-level seed flags where available).
- **Environment**: Pin locale/timezone (`TZ=UTC`), ensure stable input ordering (prefer `BTreeMap`/sorted collections over `HashMap`), and avoid wall-clock dependent code in tests.
- **Artifacts**: Compare outputs across runs; snapshots must match byte-for-byte. If differences appear, stop the line and trace nondeterministic sources (hash iteration, temp paths, clock, randomness without seed).

## Error Handling Standards

### Error Types

**Libraries** (`ggen-core`, `ggen-utils`, `ggen-domain`):
- Use `ggen_utils::error::Result<T>` for all fallible operations
- Use `ggen_utils::error::Error` for error construction
- **NEVER** use `anyhow::Result` or `anyhow::anyhow!()` in library code

**Binaries** (`ggen-cli`):
- May use `anyhow::Result` for CLI error handling
- Use `ggen_utils::error::Result` when calling library functions

### Error Construction

```rust
use ggen_utils::error::{Error, Result};

// Simple error
return Err(Error::new("Operation failed"));

// Error with context
return Err(Error::with_context("Failed to read file", &path.to_string_lossy()));

// Error with source
.map_err(|e| Error::with_source("Failed to process", Box::new(e)))?;

// Error with formatted message
return Err(Error::new(&format!("Invalid value: {}", value)));
```

### Error Handling Patterns

**DO**:
- Use `Result<T, E>` for all fallible operations
- Provide clear, actionable error messages
- Include context (paths, values, etc.) in error messages
- Chain errors with `Error::with_source()` when appropriate

**DON'T**:
- Use `unwrap()` or `expect()` in production code
- Use `anyhow` in library code
- Swallow errors silently
- Use `panic!()` except in unreachable code

## Andon Signals (Quality Assurance)

Andon signals are visual problem indicators that trigger a "stop the line" workflow.

### Signal Types

1. **CRITICAL (Red)** - Must stop immediately:
   - Compiler errors (`error[E...]`)
   - Test failures (`test ... FAILED`)

2. **HIGH (Yellow)** - Should stop:
   - Compiler warnings (`warning:`)
   - Linting errors (clippy warnings/errors)

3. **MEDIUM (Yellow)** - Investigate:
   - Performance regressions
   - Code quality warnings

### Andon Workflow

1. **Monitor**: Run `cargo make check`, `cargo make test`, `cargo make lint`
2. **Stop**: When signal appears, immediately stop current work
3. **Investigate**: Use root cause analysis (5 Whys) to understand why signal appeared
4. **Fix**: Address root cause, not just symptom
5. **Verify**: Re-run checks to confirm signal cleared

### Verification Before Completion

Before marking any task as complete:

```bash
# Verify timeout command exists
cargo make timeout-check

# Check for compiler errors (CRITICAL)
cargo make check

# Run tests (CRITICAL)
cargo make test

# Check for linting errors (HIGH)
cargo make lint

# Verify performance SLOs
cargo make slo-check
```

**All signals must be cleared before work continues.**

## Security and Compliance

- `cargo make audit` (advisories), `cargo make validate-templates`, `cargo make validate-rdf` (template/RDF integrity).
- Treat audit failures as Andon (critical); fix or explicitly document and gate before proceeding.
- Lock down third-party updates; prefer minimal dependency surface and feature flags off by default.

## Spec-First Workflow (Speckit)

- Define/refresh specs before coding: `cargo make speckit-check` (verifies spec presence) and follow `/speckit.*` commands as per team workflow.
- Keep evidence in `.specify/specs/<feature>/`; do not implement features without an up-to-date spec/plan/tasks chain.

## Code Quality Standards

### Prohibited Patterns

- **NEVER** use direct `cargo` commands - always use `cargo make`
- **NEVER** use `unwrap()` or `expect()` in production code
- **NEVER** use `anyhow` in library code
- **NEVER** ignore Andon signals
- **NEVER** proceed with signals present
- **NEVER** suppress or hide signals without fixing root cause

### Required Patterns

- Real implementations - No placeholders or stubs
- Error handling - `Result<T, E>` for all fallible operations
- Feature gating - `#[cfg(feature = "...")]` for optional dependencies
- Test verification - All code must be testable and tested
- Behavior verification - Tests must verify observable outputs/state changes

## Troubleshooting (common failures)

- **Direct `cargo` usage blocked**: Use `cargo make <task>` equivalents; see Quickstart.
- **Timeouts hit**: Check for parallel builds holding locks; retry after `cargo make timeout-check` and ensure tasks match SLA category.
- **Nondeterministic tests/output**: Run `cargo make deterministic` + `cargo make test-single-threaded`; replace unordered collections with deterministic ones.
- **Clippy warnings**: Treat as Andon (stop); run `cargo make lint` and fix root causes—no `#[allow]` unless justified and documented.
- **SLO regression**: Run `cargo make slo-check`, profile hot paths, and optimize allocations/ordering.
- **Audit failures**: Run `cargo make audit`/`validate-templates`/`validate-rdf`; pin or patch dependencies before continuing.
- **Spec missing/stale**: Refresh specs with Speckit (`cargo make speckit-check`) before coding changes.

## Continuous Improvement (Kaizen)

### Kaizen Workflow

1. **Identify**: Find small, focused improvement opportunities
2. **Plan**: Design minimal change that improves code
3. **Do**: Implement the improvement
4. **Check**: Verify improvement achieved its goal
5. **Act**: Standardize the improvement if successful

### Improvement Criteria

- **Small**: Can be done in minutes, not hours
- **Focused**: Addresses one specific thing
- **Safe**: Low risk of breaking things
- **Value**: Adds clarity, performance, or maintainability

## Documentation Standards

### Module Documentation

All modules must have comprehensive `//!` documentation:

```rust
//! Module description
//!
//! This module provides [purpose]. It includes:
//!
//! - **Feature 1**: Description
//! - **Feature 2**: Description
//!
//! ## Examples
//!
//! ```rust,no_run
//! use crate::module::Type;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let instance = Type::new()?;
//! # Ok(())
//! # }
//! ```
```

### Public API Documentation

All public APIs must have:
- Description of what the function/type does
- `# Examples` section with runnable doctests when possible
- Error documentation for `Result`-returning functions
- Parameter and return value documentation

### Documentation Waste Prevention (Muda Elimination)

Documentation work-in-progress files should be actively managed to prevent waste accumulation:

**Work-in-Progress Documentation**:
- Place intermediate analysis and reports in `docs/wip/`
- Use `_CURRENT` suffix for active tracking documents (e.g., `KAIZEN_IMPROVEMENT_CURRENT.md`)
- Remove outdated versions immediately when creating new ones

**When to Archive**:
- ✅ Work is complete and documented in code (improvements applied)
- ✅ Document is superseded by a newer version
- ✅ Information is consolidated into permanent documentation

**When NOT to Keep**:
- ❌ Historical analysis reports for completed work
- ❌ Intermediate work summaries that are no longer referenced
- ❌ Gemba walks and FMEA analyses after action items are implemented
- ❌ Duplicate files (keep only the `_CURRENT` version)

**Archival Schedule**:
- Monthly review: Check `docs/wip/` for outdated documents
- Remove files immediately after work is completed and verified
- Keep documentation clean to reduce cognitive load
- Prevent WIP directory from becoming permanent dumping ground

## Git Workflow

### Pre-Commit Hooks

Pre-commit hooks run:
- Code formatting (`cargo fmt --check`)
- Linting (`cargo clippy`)
- Quick compilation check

### Pre-Push Hooks

Pre-push hooks run:
- Full test suite
- Comprehensive linting
- Performance SLO checks

**All hooks must pass before pushing.**

## Performance SLOs

- **First build**: ≤ 15s
- **Incremental build**: ≤ 2s
- **RDF processing**: ≤ 5s for 1k+ triples
- **Generation memory**: ≤ 100MB
- **CLI scaffolding**: ≤ 3s end-to-end

Verify SLOs with:
```bash
cargo make slo-check
```

## Resources

- [Rust Standards](.cursor/rules/rust-standards.mdc)
- [Build System Practices](.cursor/rules/build-system-practices.mdc)
- [Determinism Standards](.cursor/rules/determinism-standards.mdc)
- [Contributing Guide](../CONTRIBUTING.md)

