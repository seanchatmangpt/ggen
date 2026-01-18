<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [WEEK 2: Critical Security Fixes](#week-2-critical-security-fixes)
  - [Overview](#overview)
  - [High-Priority Fixes (Week 2)](#high-priority-fixes-week-2)
    - [1. Graph Module (Critical Path)](#1-graph-module-critical-path)
    - [2. Ontology Module (Critical Path)](#2-ontology-module-critical-path)
    - [3. Lifecycle State Machine](#3-lifecycle-state-machine)
    - [4. Generator Pipeline](#4-generator-pipeline)
  - [Medium-Priority Fixes (Week 3)](#medium-priority-fixes-week-3)
    - [5. Input Validation](#5-input-validation)
    - [6. Command Injection Prevention](#6-command-injection-prevention)
    - [7. CORS Configuration](#7-cors-configuration)
    - [8. Unsafe Block Review](#8-unsafe-block-review)
  - [Implementation Strategy](#implementation-strategy)
    - [Pattern 1: Replace unwrap() with ?](#pattern-1-replace-unwrap-with-)
    - [Pattern 2: Replace expect() with context](#pattern-2-replace-expect-with-context)
    - [Pattern 3: Replace panic! with Result](#pattern-3-replace-panic-with-result)
  - [Tracking](#tracking)
  - [Agent Assignment](#agent-assignment)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# WEEK 2: Critical Security Fixes

## Overview
Identified 717 instances of `panic!()`, `unwrap()`, and `expect()` across the codebase.
Priority: Fix critical paths first (graph, ontology, lifecycle modules).

## High-Priority Fixes (Week 2)

### 1. Graph Module (Critical Path)
- **File**: `crates/ggen-core/src/graph/core.rs`
- **Issue**: Unwraps in node/edge operations
- **Fix**: Replace with `Result<T, GraphError>` propagation
- **Impact**: Prevents crashes during graph traversal

### 2. Ontology Module (Critical Path)
- **File**: `crates/ggen-core/src/ontology/*.rs`
- **Issue**: Panics in RDF parsing and SPARQL queries
- **Fix**: Implement proper error handling with context
- **Impact**: Graceful degradation on malformed ontologies

### 3. Lifecycle State Machine
- **File**: `crates/ggen-core/src/lifecycle/*.rs`
- **Issue**: Unwraps in state transitions
- **Fix**: Type-safe transitions with compile-time validation
- **Impact**: Prevents invalid state transitions

### 4. Generator Pipeline
- **File**: `crates/ggen-core/src/generator.rs`
- **Issue**: Expects in template rendering
- **Fix**: Propagate rendering errors with detailed context
- **Impact**: Better error messages for users

## Medium-Priority Fixes (Week 3)

### 5. Input Validation
- **Pattern**: Direct string manipulation without validation
- **Fix**: Add validation layer with sanitization
- **Modules**: preprocessor, template, inject

### 6. Command Injection Prevention
- **Pattern**: Unsanitized shell command construction
- **Fix**: Use typed command builders
- **Modules**: GitHub integration, external tools

### 7. CORS Configuration
- **Pattern**: Overly permissive CORS in examples
- **Fix**: Restrictive defaults with opt-in relaxation
- **Modules**: examples/async-web-service

### 8. Unsafe Block Review
- **Count**: 0 (already forbidden via `#![forbid(unsafe_code)]`)
- **Status**: ✓ Complete

## Implementation Strategy

### Pattern 1: Replace unwrap() with ?
```rust
// Before
let value = map.get(key).unwrap();

// After
let value = map.get(key).ok_or_else(|| Error::KeyNotFound(key.clone()))?;
```

### Pattern 2: Replace expect() with context
```rust
// Before
let value = operation().expect("operation failed");

// After
let value = operation().map_err(|e| Error::OperationFailed {
    source: e,
    context: "during graph traversal"
})?;
```

### Pattern 3: Replace panic! with Result
```rust
// Before
if invalid_state {
    panic!("Invalid state detected");
}

// After
if invalid_state {
    return Err(Error::InvalidState { details: "..." });
}
```

## Tracking

- **Total instances**: 717
- **Week 2 target**: 100 critical path fixes
- **Week 3 target**: 200 additional fixes
- **Remaining**: Gradual reduction over weeks 4-8

## Agent Assignment

- **Agent 3 (Security Manager)**: Lead security fixes
- **Agent 2 (Backend Developer)**: Implement error propagation
- **Agent 8 (Code Analyzer)**: Identify remaining instances
- **Agent 12 (Code Review Swarm)**: Review all changes
