<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Error Modes Inventory - Marketplace Package Lifecycle](#error-modes-inventory---marketplace-package-lifecycle)
  - [1. Invalid Lifecycle State Transitions](#1-invalid-lifecycle-state-transitions)
    - [Error: Running phases out of order](#error-running-phases-out-of-order)
    - [Error: Skipping required phases](#error-skipping-required-phases)
    - [Error: Re-running phases in wrong order](#error-re-running-phases-in-wrong-order)
  - [2. Invalid Package States](#2-invalid-package-states)
    - [Error: Installing unvalidated packages](#error-installing-unvalidated-packages)
    - [Error: Using packages without required metadata](#error-using-packages-without-required-metadata)
    - [Error: Installing packages with invalid dependencies](#error-installing-packages-with-invalid-dependencies)
  - [3. Invalid Phase Configuration](#3-invalid-phase-configuration)
    - [Error: Phases with no commands](#error-phases-with-no-commands)
    - [Error: Invalid hook dependencies](#error-invalid-hook-dependencies)
    - [Error: Circular hook dependencies](#error-circular-hook-dependencies)
  - [4. Invalid State Persistence](#4-invalid-state-persistence)
    - [Error: Corrupted state files](#error-corrupted-state-files)
    - [Error: Invalid state transitions in persisted state](#error-invalid-state-transitions-in-persisted-state)
    - [Error: Cache keys for non-existent phases](#error-cache-keys-for-non-existent-phases)
  - [5. Invalid Hook Dependencies](#5-invalid-hook-dependencies)
    - [Error: Circular hook dependencies](#error-circular-hook-dependencies-1)
    - [Error: Hooks referencing non-existent phases](#error-hooks-referencing-non-existent-phases)
    - [Error: Self-referential hooks](#error-self-referential-hooks)
  - [6. Invalid Context Usage](#6-invalid-context-usage)
    - [Error: Using context after lifecycle completion](#error-using-context-after-lifecycle-completion)
    - [Error: Parallel execution conflicts](#error-parallel-execution-conflicts)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Error Modes Inventory - Marketplace Package Lifecycle

This document catalogs all error modes in the marketplace package project lifecycle that can be prevented through poka-yoke (type-level) design.

## 1. Invalid Lifecycle State Transitions

### Error: Running phases out of order
- **Example**: Running `deploy` before `build`, `test` before `build`
- **Current Detection**: Runtime validation (if any)
- **Impact**: Build failures, deployment of untested code
- **Prevention**: Type-level state machine using PhantomData markers

### Error: Skipping required phases
- **Example**: Running `deploy` without running `test` first
- **Current Detection**: None (implicit dependency)
- **Impact**: Deploying untested code
- **Prevention**: State machine enforces phase dependencies

### Error: Re-running phases in wrong order
- **Example**: Running `init` after `build` has already run
- **Current Detection**: None
- **Impact**: State corruption, unexpected behavior
- **Prevention**: State machine tracks completed phases

## 2. Invalid Package States

### Error: Installing unvalidated packages
- **Example**: Installing package without validation checks
- **Current Detection**: Runtime checks in `PackageBuilder::build()`
- **Impact**: Installing broken or malicious packages
- **Prevention**: Newtype wrapper `ValidatedPackage` - cannot install `UnvalidatedPackage`

### Error: Using packages without required metadata
- **Example**: Package missing title, description, or content_id
- **Current Detection**: Runtime validation in builder
- **Impact**: Incomplete package information, broken installations
- **Prevention**: Builder pattern ensures required fields before validation

### Error: Installing packages with invalid dependencies
- **Example**: Package depends on non-existent or incompatible packages
- **Current Detection**: Runtime during installation
- **Impact**: Installation failures, dependency conflicts
- **Prevention**: Validation step checks dependencies before creating `ValidatedPackage`

## 3. Invalid Phase Configuration

### Error: Phases with no commands
- **Example**: Phase defined but has empty `commands` array
- **Current Detection**: Runtime check in `run_phase` (returns Ok with warning)
- **Impact**: Silent failures, phases that do nothing
- **Prevention**: `PhaseBuilder` requires at least one command before building `ValidatedPhase`

### Error: Invalid hook dependencies
- **Example**: Hook references non-existent phase
- **Current Detection**: Runtime error when hook executes
- **Impact**: Hook failures, broken lifecycle
- **Prevention**: Hook validation checks all referenced phases exist

### Error: Circular hook dependencies
- **Example**: `before_build` includes `build`, which includes `before_build`
- **Current Detection**: Runtime recursion detection (`HookRecursion` error)
- **Impact**: Infinite loops, stack overflow
- **Prevention**: Compile-time DAG validation where possible, runtime cycle detection

## 4. Invalid State Persistence

### Error: Corrupted state files
- **Example**: JSON parse errors, invalid state structure
- **Current Detection**: Runtime parsing errors
- **Impact**: Lost state, cannot resume lifecycle
- **Prevention**: `ValidatedLifecycleState` wrapper ensures state passes validation before use

### Error: Invalid state transitions in persisted state
- **Example**: State shows `deploy` completed but `build` never ran
- **Current Detection**: None (state is trusted)
- **Impact**: Inconsistent state, incorrect caching
- **Prevention**: State validation checks phase history consistency

### Error: Cache keys for non-existent phases
- **Example**: Cache key references phase that was removed from config
- **Current Detection**: None
- **Impact**: Stale cache entries, incorrect cache hits
- **Prevention**: State validation checks cache keys reference valid phases

## 5. Invalid Hook Dependencies

### Error: Circular hook dependencies
- **Example**: `before_a` includes `b`, `before_b` includes `a`
- **Current Detection**: Runtime recursion detection
- **Impact**: Infinite loops, stack overflow
- **Prevention**: Compile-time validation for static hooks, runtime detection for dynamic

### Error: Hooks referencing non-existent phases
- **Example**: `before_build` includes `validate` but `validate` phase doesn't exist
- **Current Detection**: Runtime error when hook executes
- **Impact**: Hook failures, broken lifecycle
- **Prevention**: Hook validation checks all referenced phases exist in config

### Error: Self-referential hooks
- **Example**: `before_build` includes `build`
- **Current Detection**: Runtime recursion detection
- **Impact**: Infinite loops
- **Prevention**: Hook validation prevents self-references

## 6. Invalid Context Usage

### Error: Using context after lifecycle completion
- **Example**: Running phases after pipeline completed
- **Current Detection**: None
- **Impact**: State corruption, unexpected behavior
- **Prevention**: State machine tracks lifecycle completion

### Error: Parallel execution conflicts
- **Example**: Multiple threads modifying same state simultaneously
- **Current Detection**: Mutex poisoning (rare)
- **Impact**: State corruption, data races
- **Prevention**: Thread-safe state management (already implemented)

## Summary

All identified error modes can be prevented through:
1. **Type-level state machine** - Prevents invalid phase transitions
2. **Validated types** - Prevents using unvalidated data
3. **Builder patterns** - Ensures required fields before construction
4. **State validation** - Prevents using corrupted state
5. **Hook validation** - Prevents circular and invalid dependencies

These poka-yoke mechanisms make entire classes of errors impossible at compile time.


