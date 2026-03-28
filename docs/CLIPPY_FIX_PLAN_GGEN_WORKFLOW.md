<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Clippy Fix Plan: ggen-workflow](#clippy-fix-plan-ggen-workflow)
  - [Executive Summary](#executive-summary)
  - [Fix Plan by File](#fix-plan-by-file)
    - [1. `src/parser.rs` (13 warnings)](#1-srcparserrs-13-warnings)
      - [1.1 Derive `Default` instead of manual impl (6 instances)](#11-derive-default-instead-of-manual-impl-6-instances)
      - [1.2 `from_str` method name conflicts (3 instances) - MANUAL REVIEW REQUIRED](#12-from_str-method-name-conflicts-3-instances---manual-review-required)
      - [1.3 Unnecessary `if let` on `Ok` variant only](#13-unnecessary-if-let-on-ok-variant-only)
      - [1.4 `and_then` with `Some` construction](#14-and_then-with-some-construction)
      - [1.5 Using `map` over `inspect`](#15-using-map-over-inspect)
      - [1.6 Single-pattern `match` should be `if let`](#16-single-pattern-match-should-be-if-let)
      - [1.7 Iterating on map values](#17-iterating-on-map-values)
    - [2. `src/engine.rs` (3 warnings)](#2-srcenginers-3-warnings)
      - [2.1 Manual prefix stripping](#21-manual-prefix-stripping)
      - [2.2 `or_insert_with` for default value](#22-or_insert_with-for-default-value)
      - [2.3 Length comparison to one](#23-length-comparison-to-one)
    - [3. `src/receipts.rs` (2 warnings)](#3-srcreceiptsrs-2-warnings)
      - [3.1 Derive `Default` instead of manual impl](#31-derive-default-instead-of-manual-impl)
      - [3.2 Add `Default` for `ReceiptGenerator` (Optional)](#32-add-default-for-receiptgenerator-optional)
    - [4. `src/state.rs` (1 warning)](#4-srcstaters-1-warning)
      - [4.1 Unnecessary cast to same type](#41-unnecessary-cast-to-same-type)
  - [Fix Execution Order](#fix-execution-order)
    - [Phase 1: Simple Mechanical Fixes (15 warnings, ~5 min)](#phase-1-simple-mechanical-fixes-15-warnings-5-min)
    - [Phase 2: Manual Code Review (6 warnings, ~15 min)](#phase-2-manual-code-review-6-warnings-15-min)
  - [Commands](#commands)
    - [Apply fixes automatically (Phase 1 only):](#apply-fixes-automatically-phase-1-only)
    - [Verify after fixes:](#verify-after-fixes)
  - [Notes](#notes)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Clippy Fix Plan: ggen-workflow

**Generated**: 2026-02-08
**Total Warnings**: 21
**Package**: ggen-workflow v0.1.0

---

## Executive Summary

| Category | Count | Fix Type |
|----------|-------|----------|
| Manual `impl` blocks that can be derived | 6 | Simple - `#[derive(Default)]` |
| `or_insert_with` for default values | 1 | Simple - `.or_default()` |
| Length comparison to one | 1 | Simple - `!is_empty()` |
| Manual prefix stripping | 1 | Simple - `.strip_prefix()` |
| `from_str` method name conflicts | 3 | Manual - rename to `parse` or implement trait |
| Unnecessary `if let` | 1 | Simple - `filter_map()` or `flatten()` |
| `and_then` with `Some` | 1 | Simple - `.map()` |
| `map` over `inspect` | 1 | Simple - remove or use proper pattern |
| Single-pattern `match` | 1 | Simple - `if let` |
| Map values iteration | 1 | Simple - `.values()` |
| Missing `Default` impl suggestion | 1 | Optional - add derive |
| Unnecessary cast | 1 | Simple - remove cast |
| Unused dead code | 2 | Manual - review and remove/allow |

---

## Fix Plan by File

### 1. `src/parser.rs` (13 warnings)

#### 1.1 Derive `Default` instead of manual impl (6 instances)

**Lines 151-155**: `DecompositionType::Default`
```rust
// BEFORE:
impl Default for DecompositionType {
    fn default() -> Self {
        DecompositionType::WSNet
    }
}

// AFTER:
#[derive(Default)]
pub enum DecompositionType {
    #[default]
    WSNet,
    // ... rest
}
```

**Lines 218-222**: `TaskType::Default`
```rust
// BEFORE:
impl Default for TaskType {
    fn default() -> Self {
        TaskType::Atomic
    }
}

// AFTER:
#[derive(Default)]
pub enum TaskType {
    #[default]
    Atomic,
    // ... rest
}
```

**Lines 272-276**: `SplitType::Default`
```rust
// BEFORE:
impl Default for SplitType {
    fn default() -> Self {
        SplitType::Xor
    }
}

// AFTER:
#[derive(Default)]
pub enum SplitType {
    #[default]
    Xor,
    // ... rest
}
```

**Lines 310-314**: `JoinType::Default`
```rust
// BEFORE:
impl Default for JoinType {
    fn default() -> Self {
        JoinType::Xor
    }
}

// AFTER:
#[derive(Default)]
pub enum JoinType {
    #[default]
    Xor,
    // ... rest
}
```

**Line 444+**: `Flow::Default` (need to verify location)
```rust
// BEFORE:
impl Default for Flow {
    fn default() -> Self {
        Flow {
            id: String::new(),
            source: String::new(),
            target: String::new(),
            predicate: None,
        }
    }
}

// AFTER:
#[derive(Default)]
pub struct Flow {
    // ... fields
}
```

#### 1.2 `from_str` method name conflicts (3 instances) - MANUAL REVIEW REQUIRED

**Lines 253-263**: `SplitType::from_str`
**Lines 291-301**: `JoinType::from_str`
**Lines 418-430**: `TaskType::from_str` (or similar)

These conflict with `std::str::FromStr::from_str`. Two options:

**Option A**: Rename to `parse` (simpler, no trait impl needed)
```rust
// BEFORE:
impl SplitType {
    pub fn from_str(s: &str) -> WorkflowResult<Self> {
        match s.to_lowercase().as_str() {
            "and" => Ok(SplitType::And),
            // ...
        }
    }
}

// AFTER:
impl SplitType {
    pub fn parse(s: &str) -> WorkflowResult<Self> {
        match s.to_lowercase().as_str() {
            "and" => Ok(SplitType::And),
            // ...
        }
    }
}
```

**Option B**: Implement `FromStr` trait (more idiomatic, changes call sites)
```rust
// AFTER:
impl FromStr for SplitType {
    type Err = WorkflowError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "and" => Ok(SplitType::And),
            // ...
        }
    }
}
```

**RECOMMENDATION**: Option A (rename to `parse`) - less invasive, call sites updated with find/replace.

#### 1.3 Unnecessary `if let` on `Ok` variant only

**Line 1056**: `extract_attributes` method
```rust
// BEFORE:
for attr_result in element.attributes() {
    if let Ok(attr) = attr_result {
        let key = String::from_utf8_lossy(attr.key.as_ref()).to_string();
        // ...
    }
}

// AFTER:
for attr in element.attributes().filter_map(Result::ok) {
    let key = String::from_utf8_lossy(attr.key.as_ref()).to_string();
    // ...
}
```

#### 1.4 `and_then` with `Some` construction

**Lines 1097-1104**: `decomposition_type` parsing
```rust
// BEFORE:
decomposition_type: attrs.get("type")
    .and_then(|t| match t.as_str() {
        "WSNet" => Some(DecompositionType::WSNet),
        "CompositeNet" => Some(DecompositionType::CompositeNet),
        "OrJoinNet" => Some(DecompositionType::OrJoinNet),
        custom => Some(DecompositionType::Custom(custom.to_string())),
    })
    .unwrap_or_default(),

// AFTER:
decomposition_type: attrs.get("type")
    .map(|t| match t.as_str() {
        "WSNet" => DecompositionType::WSNet,
        "CompositeNet" => DecompositionType::CompositeNet,
        "OrJoinNet" => DecompositionType::OrJoinNet,
        custom => DecompositionType::Custom(custom.to_string()),
    })
    .unwrap_or_default(),
```

#### 1.5 Using `map` over `inspect`

**Lines 1304-1313**: Dead code path
```rust
// BEFORE:
let flows: Vec<Flow> = state
    .flows
    .into_iter()
    .map(|f| {
        // Set predicate from stored value if not already set
        let _ = f.predicate.is_none();
        // This would be matched with predicates during parsing
        f
    })
    .collect();

// AFTER:
let flows: Vec<Flow> = state.flows;
```

The `map` does nothing - remove it entirely.

#### 1.6 Single-pattern `match` should be `if let`

**Lines 1369-1377**: Task split/join validation
```rust
// BEFORE:
match (task.split_type, task.join_type) {
    (SplitType::And, JoinType::Xor) => {
        return Err(errors::yawl_validation(format!(
            "Task {} has invalid split/join combination: AND split with XOR join",
            task.id
        )))
    }
    _ => {}
}

// AFTER:
if matches!(task.split_type, SplitType::And)
    && matches!(task.join_type, JoinType::Xor)
{
    return Err(errors::yawl_validation(format!(
        "Task {} has invalid split/join combination: AND split with XOR join",
        task.id
    )));
}
```

#### 1.7 Iterating on map values

**Line 1530**: Serialize tasks
```rust
// BEFORE:
for (_id, task) in &spec.tasks {

// AFTER:
for task in spec.tasks.values() {
```

---

### 2. `src/engine.rs` (3 warnings)

#### 2.1 Manual prefix stripping

**Line 383**: SPARQL prefix removal
```rust
// BEFORE:
if expr.starts_with("SPARQL:") {
    return self.evaluate_sparql_condition(&expr[7..]);
}

// AFTER:
if let Some(rest) = expr.strip_prefix("SPARQL:") {
    return self.evaluate_sparql_condition(rest);
}
```

#### 2.2 `or_insert_with` for default value

**Line 1315**: Reverse dependencies
```rust
// BEFORE:
self.reverse_dependencies
    .entry(dep.clone())
    .or_insert_with(Vec::new)
    .push(task.id.clone());

// AFTER:
self.reverse_dependencies
    .entry(dep.clone())
    .or_default()
    .push(task.id.clone());
```

#### 2.3 Length comparison to one

**Line 1556**: Join condition check
```rust
// BEFORE:
JoinCondition::First => state.completed_branches.len() >= 1,

// AFTER:
JoinCondition::First => !state.completed_branches.is_empty(),
```

---

### 3. `src/receipts.rs` (2 warnings)

#### 3.1 Derive `Default` instead of manual impl

**Lines 71-75**: `HashAlgorithm::Default`
```rust
// BEFORE:
impl Default for HashAlgorithm {
    fn default() -> Self {
        HashAlgorithm::Sha256
    }
}

// AFTER:
#[derive(Default)]
pub enum HashAlgorithm {
    #[default]
    Sha256,
    // ...
}
```

#### 3.2 Add `Default` for `ReceiptGenerator` (Optional)

**Line 79**: Suggestion to add `Default` implementation

This is optional - can be added if useful:
```rust
impl Default for ReceiptGenerator {
    fn default() -> Self {
        Self::new()
    }
}
```

Or derive if the `new()` implementation becomes trivial:
```rust
#[derive(Default)]
pub struct ReceiptGenerator {
    private_key: [u8; 32],
    hash_algorithm: HashAlgorithm,
}
```

---

### 4. `src/state.rs` (1 warning)

#### 4.1 Unnecessary cast to same type

**Line 700**: Duration conversion
```rust
// BEFORE:
started_at: chrono::Utc::now() - chrono::Duration::milliseconds(duration.num_milliseconds() as i64),

// AFTER:
started_at: chrono::Utc::now() - chrono::Duration::milliseconds(duration.num_milliseconds()),
```

The `num_milliseconds()` already returns `i64`, the cast is redundant.

---

## Fix Execution Order

### Phase 1: Simple Mechanical Fixes (15 warnings, ~5 min)
These can be done with regex/search-replace without semantic changes:

1. `parser.rs`: Add `#[derive(Default)]` to enums (6 fixes)
2. `parser.rs`: `filter_map(Result::ok)` (1 fix)
3. `parser.rs`: `and_then` -> `map` (1 fix)
4. `parser.rs`: Remove identity `map` (1 fix)
5. `parser.rs`: `if let` for single pattern (1 fix)
6. `parser.rs`: `.values()` iteration (1 fix)
7. `engine.rs`: `.strip_prefix()` (1 fix)
8. `engine.rs`: `.or_default()` (1 fix)
9. `engine.rs`: `!is_empty()` (1 fix)
10. `receipts.rs`: `#[derive(Default)]` (1 fix)
11. `state.rs`: Remove cast (1 fix)

### Phase 2: Manual Code Review (6 warnings, ~15 min)
These require reviewing call sites or making design decisions:

1. `parser.rs`: Rename `from_str` -> `parse` (3 locations + call sites)
2. `receipts.rs`: Add `Default` for `ReceiptGenerator` (optional)
3. `engine.rs`: Review `#[allow(dead_code)]` on `check_join_condition`

---

## Commands

### Apply fixes automatically (Phase 1 only):
```bash
cargo clippy --fix --lib -p ggen-workflow --allow-dirty --allow-staged
```

### Verify after fixes:
```bash
cargo clippy --package ggen-workflow
cargo make lint
```

---

## Notes

1. The `from_str` warnings are the most complex - need to verify if `FromStr` trait should be implemented or just rename methods.

2. The dead code warning on `check_join_condition` (engine.rs:1550) suggests the method is unused but tested. Consider adding tests or removing if truly dead.

3. After all fixes, run full test suite to ensure no behavioral changes:
```bash
cargo make test
```
