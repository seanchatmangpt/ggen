# Poka-Yoke Design for Marketplace Package Lifecycle

## Overview

This document explains the poka-yoke (mistake-proofing) design applied to the marketplace package project lifecycle. Poka-yoke uses Rust's type system to prevent entire classes of errors at compile time, making invalid states and operations impossible.

## What is Poka-Yoke?

**Poka-yoke** (Japanese: ポカヨケ) means "mistake-proofing" - designing systems that prevent errors through design rather than detection. In Rust, we achieve this through:

1. **Type-level invariants** - Using types to encode valid states
2. **Newtype wrappers** - Separating validated from unvalidated data
3. **Builder patterns** - Ensuring required fields before construction
4. **State machines** - Preventing invalid transitions at compile time

## Error Prevention Mechanisms

### 1. Type-Level Lifecycle State Machine

**Problem**: Running phases out of order (e.g., `deploy` before `build`)

**Solution**: Type-level state machine using PhantomData markers

```rust
use ggen_core::lifecycle::state_machine::*;

// Start with initial state
let lifecycle = LifecycleStateMachine::<Initial>::new();

// Valid transitions - compiler enforces order
let lifecycle = lifecycle.init()?;
let lifecycle = lifecycle.setup()?;
let lifecycle = lifecycle.build()?;
let lifecycle = lifecycle.test()?;
let lifecycle = lifecycle.deploy()?;

// Invalid transitions fail at compile time:
// lifecycle.deploy() // Compile error: method doesn't exist on Initial state
```

**Benefits**:
- Invalid phase transitions are impossible - compiler prevents them
- Type system encodes valid lifecycle progression
- Zero runtime cost (PhantomData is compile-time only)

### 2. Validated Package Types

**Problem**: Installing unvalidated packages that may be broken or malicious

**Solution**: Newtype wrappers for validated vs unvalidated packages

```rust
use ggen_marketplace::models::{PackageBuilder, UnvalidatedPackage, ValidatedPackage};

// Build unvalidated package
let unvalidated = PackageBuilder::new(id, version)
    .title("My Package")
    .description("A great package")
    .content_id(content_id)
    .build()?;

// Validate before use
let validated = unvalidated.validate()?;

// Only validated packages can be installed
install_package(validated)?; // Type system prevents installing unvalidated
```

**Benefits**:
- Cannot install unvalidated packages - type prevents it
- Validation is explicit and required
- Clear separation between unvalidated and validated data

### 3. Phase Builder Pattern

**Problem**: Phases with no commands cause silent failures

**Solution**: Builder pattern that requires at least one command

```rust
use ggen_core::lifecycle::model::PhaseBuilder;

// Valid phase - has commands
let phase = PhaseBuilder::new("build")
    .command("cargo build --release")
    .build()
    .unwrap(); // Guaranteed to have at least one command

// Invalid phase - no commands
let phase = PhaseBuilder::new("empty")
    .build(); // Returns Err - cannot build phase without commands
```

**Benefits**:
- Phases always have commands - builder enforces this
- Prevents runtime errors from empty phases
- Clear API for phase construction

### 4. State Validation

**Problem**: Using corrupted or invalid lifecycle state

**Solution**: Validated state wrapper that ensures state passes validation

```rust
use ggen_core::lifecycle::{load_state, ValidatedLifecycleState};

// Load and validate state
let validated_state = ValidatedLifecycleState::new(load_state(path)?)?;

// Only validated state can be used in operations
use_validated_state(validated_state)?; // Type prevents using unvalidated state
```

**Benefits**:
- Cannot use corrupted state - validation required
- Catches invalid phase history, missing prerequisites
- Prevents cache key inconsistencies

### 5. Hook Validation

**Problem**: Circular hook dependencies cause infinite loops

**Solution**: Validation that checks for cycles and invalid references

```rust
use ggen_core::lifecycle::{validate_hooks, Make};

let make = load_make("make.toml")?;

// Validate hooks before use
let validated_hooks = validate_hooks(&make)?;

// Only validated hooks can be used
use_hooks(validated_hooks)?; // Type prevents using invalid hooks
```

**Benefits**:
- Prevents circular dependencies
- Catches invalid phase references
- Prevents self-referential hooks

## Why Types Prevent Errors

### Compile-Time Safety

Rust's type system catches errors at compile time, before code runs:

```rust
// This code doesn't compile - type system prevents it
let lifecycle = LifecycleStateMachine::<Initial>::new();
lifecycle.deploy(); // Error: no method `deploy` on `LifecycleStateMachine<Initial>`
```

### Zero Runtime Cost

Type-level invariants use PhantomData, which has zero runtime cost:

```rust
pub struct LifecycleStateMachine<State> {
    state: LifecycleState,
    _marker: PhantomData<State>, // Compile-time only, zero runtime cost
}
```

### Impossible States

Types make invalid states unrepresentable:

```rust
// Cannot represent a package that's both validated and unvalidated
// Type system enforces: UnvalidatedPackage -> ValidatedPackage (one-way)
```

## Usage Examples

### Example 1: Complete Lifecycle Flow

```rust
use ggen_core::lifecycle::state_machine::*;

fn run_lifecycle() -> Result<()> {
    let lifecycle = LifecycleStateMachine::<Initial>::new();
    
    // Each transition is type-safe
    let lifecycle = lifecycle.init()?;
    let lifecycle = lifecycle.setup()?;
    let lifecycle = lifecycle.build()?;
    let lifecycle = lifecycle.test()?;
    let lifecycle = lifecycle.deploy()?;
    
    Ok(())
}
```

### Example 2: Validated Package Installation

```rust
use ggen_marketplace::models::{PackageBuilder, ValidatedPackage};

fn install_package_safely(id: PackageId, version: Version) -> Result<()> {
    // Build and validate package
    let validated = PackageBuilder::new(id, version)
        .title("My Package")
        .description("Description")
        .content_id(content_id)
        .build()?
        .validate()?; // Must validate before installation
    
    // Type system ensures only validated packages can be installed
    install_validated_package(validated)?;
    
    Ok(())
}
```

### Example 3: Phase Construction

```rust
use ggen_core::lifecycle::model::PhaseBuilder;

fn create_build_phase() -> Result<ValidatedPhase> {
    // Builder ensures phase has commands
    let phase = PhaseBuilder::new("build")
        .command("cargo build --release")
        .cache(true)
        .build()?; // Returns error if no commands
    
    Ok(phase)
}
```

## Migration Guide

### From Unvalidated to Validated Types

**Before**:
```rust
let package = PackageBuilder::new(id, version)
    .title("Package")
    .build()?;
install_package(package)?; // No validation
```

**After**:
```rust
let package = PackageBuilder::new(id, version)
    .title("Package")
    .build()?
    .validate()?; // Explicit validation
install_package(package)?; // Type ensures validated
```

### From Direct State to Validated State

**Before**:
```rust
let state = load_state(path)?;
use_state(state)?; // No validation
```

**After**:
```rust
let state = ValidatedLifecycleState::new(load_state(path)?)?;
use_state(state)?; // Type ensures validated
```

## Benefits Summary

1. **Compile-Time Safety**: Errors caught before code runs
2. **Zero Runtime Cost**: Type-level invariants have no runtime overhead
3. **Impossible States**: Invalid states cannot be represented
4. **Clear API**: Types document valid operations
5. **Refactoring Safety**: Type system catches breaking changes

## Further Reading

- [Error Modes Inventory](./error_modes.md) - Complete list of prevented errors
- [State Machine Implementation](./state_machine.rs) - Type-level state machine code
- [Validation Logic](./state_validation.rs) - State validation implementation

## Conclusion

Poka-yoke design uses Rust's type system to prevent entire classes of errors at compile time. By making invalid states unrepresentable and invalid operations impossible, we eliminate runtime errors and improve code safety.

**Key Principle**: "Make invalid states unrepresentable" - Use types to encode invariants, preventing errors through design rather than detection.


