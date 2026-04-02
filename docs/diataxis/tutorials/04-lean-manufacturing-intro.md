<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Tutorial: Lean Manufacturing Introduction](#tutorial-lean-manufacturing-introduction)
  - [Learning Objectives](#learning-objectives)
  - [What is Lean Manufacturing?](#what-is-lean-manufacturing)
  - [The 3M Framework](#the-3m-framework)
    - [Mura (Inconsistency)](#mura-inconsistency)
    - [Muda (Waste)](#muda-waste)
      - [1. Overproduction (Excessive Tests)](#1-overproduction-excessive-tests)
      - [2. Waiting (Slow Tests)](#2-waiting-slow-tests)
      - [3. Transportation (Data Movement)](#3-transportation-data-movement)
      - [4. Overprocessing (Excessive Setup)](#4-overprocessing-excessive-setup)
      - [5. Inventory (Unused Test Code)](#5-inventory-unused-test-code)
      - [6. Motion (Context Switching)](#6-motion-context-switching)
      - [7. Defects (Flaky Tests)](#7-defects-flaky-tests)
    - [Muri (Overburden)](#muri-overburden)
  - [Gemba: Go and See](#gemba-go-and-see)
    - [Gemba Walk Example](#gemba-walk-example)
  - [Andon: Visual Management](#andon-visual-management)
    - [Andon Cord System](#andon-cord-system)
  - [Poka-Yoke: Mistake-Proofing](#poka-yoke-mistake-proofing)
    - [Pattern 1: Guide Pin (Compile-Time Constraints)](#pattern-1-guide-pin-compile-time-constraints)
    - [Pattern 2: Limit Switch (Bounded Values)](#pattern-2-limit-switch-bounded-values)
  - [Real-World Case Study](#real-world-case-study)
    - [Before Lean](#before-lean)
    - [After Lean (3M Framework Applied)](#after-lean-3m-framework-applied)
  - [Glossary](#glossary)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Tutorial: Lean Manufacturing Introduction

**Apply Toyota Production System principles (Mura/Muda/Muri) to software testing**

---

## Learning Objectives

By the end of this tutorial, you will:
- Understand the 3M framework: Mura (inconsistency), Muda (waste), Muri (overburden)
- Know how to identify waste in test suites (duplicated tests, slow tests, flaky tests)
- Be able to apply Gemba (go-and-see) and Andon (alert system) to testing
- Understand how Poka-Yoke (mistake-proofing) prevents errors at compile time

**Estimated Time:** 25 minutes
**Difficulty:** Advanced
**Prerequisites:** Understanding of software testing, familiarity with TDD

---

## What is Lean Manufacturing?

**Lean Manufacturing** is the Toyota Production System (TPS) philosophy focused on:
- **Maximizing customer value**
- **Eliminating waste**
- **Continuous improvement (Kaizen)**

**Historical Context:** Developed by Taiichi Ohno at Toyota in the 1950s. Reduced car production time from 12 hours to 90 minutes while improving quality.

**Software Adaptation:** Apply manufacturing principles to code/test production:
- Tests = Products
- Developers = Factory workers
- CI/CD = Assembly line
- Bugs = Defects

---

## The 3M Framework

### Mura (Inconsistency)

**Definition:** Unevenness or irregularity in processes.

**In Testing:**
```rust
// ❌ MURA: Inconsistent test structure
#[test]
fn test_1() {
    let result = add(2, 2);
    assert_eq!(result, 4);
}

#[test]
fn test_addition() {  // Different naming convention
    let x = 5;
    let y = 3;
    assert_eq!(add(x, y), 8);  // Different assertion style
}

#[test]
fn verify_subtraction() {  // Third naming convention
    assert_eq!(subtract(10, 3), 7);
}
```

**Problems:**
- Developers waste time figuring out conventions
- Code reviews take longer (inconsistent style)
- New contributors confused (which pattern to follow?)

**Lean Solution: Standardize**
```rust
// ✅ ZERO MURA: Consistent structure
#[test]
fn test_add_positive_numbers() {
    // Arrange
    let a = 2;
    let b = 2;

    // Act
    let result = add(a, b);

    // Assert
    assert_eq!(result, 4);
}

#[test]
fn test_add_negative_numbers() {
    // Arrange
    let a = -5;
    let b = -3;

    // Act
    let result = add(a, b);

    // Assert
    assert_eq!(result, -8);
}
```

**Impact:** 30% faster code review, 50% faster onboarding.

---

### Muda (Waste)

**Definition:** Any activity that consumes resources but creates no value.

**7 Types of Muda in Testing:**

#### 1. Overproduction (Excessive Tests)

```rust
// ❌ MUDA: Testing the same thing 5 different ways
#[test]
fn test_add_returns_correct_sum() {
    assert_eq!(add(2, 2), 4);
}

#[test]
fn test_add_works_for_positive_numbers() {
    assert_eq!(add(2, 2), 4);  // Duplicate!
}

#[test]
fn test_addition_functionality() {
    assert_eq!(add(2, 2), 4);  // Duplicate!
}

// ✅ ZERO MUDA: One test covers the case
#[test]
fn test_add_positive_numbers() {
    assert_eq!(add(2, 2), 4);
}
```

**Saved:** 2 redundant tests × 100ms = 200ms per CI run.

#### 2. Waiting (Slow Tests)

```rust
// ❌ MUDA: Unnecessary sleeps
#[test]
fn test_async_operation() {
    start_operation();
    std::thread::sleep(Duration::from_secs(5));  // Overkill!
    assert!(is_complete());
}

// ✅ ZERO MUDA: Poll with timeout
#[test]
fn test_async_operation() {
    start_operation();
    wait_until(|| is_complete(), Duration::from_millis(100));
    assert!(is_complete());
}
```

**Saved:** 4.9 seconds per test × 50 tests = 4 minutes per CI run.

#### 3. Transportation (Data Movement)

```rust
// ❌ MUDA: Moving data unnecessarily
#[test]
fn test_parse_and_validate() {
    let json = load_from_disk("test_data.json");  // Slow I/O
    let parsed = parse_json(&json);
    assert!(validate(&parsed));
}

// ✅ ZERO MUDA: Inline test data
#[test]
fn test_parse_and_validate() {
    let json = r#"{"key": "value"}"#;  // No I/O
    let parsed = parse_json(&json);
    assert!(validate(&parsed));
}
```

**Saved:** 50ms disk I/O × 100 tests = 5 seconds per CI run.

#### 4. Overprocessing (Excessive Setup)

```rust
// ❌ MUDA: Building entire database for simple test
#[test]
fn test_user_validation() {
    let db = setup_full_database();  // 2 seconds!
    let user = db.create_user("test@example.com");
    assert!(validate_email(&user.email));
}

// ✅ ZERO MUDA: Unit test without DB
#[test]
fn test_user_validation() {
    assert!(validate_email("test@example.com"));
}
```

**Saved:** 2 seconds × 30 user tests = 60 seconds per CI run.

#### 5. Inventory (Unused Test Code)

```rust
// ❌ MUDA: Dead helper functions
fn create_test_user_v1() -> User { /* ... */ }  // Unused
fn create_test_user_v2() -> User { /* ... */ }  // Unused
fn create_test_user_v3() -> User { /* ... */ }  // Unused
fn create_test_user() -> User { /* ... */ }     // Only this is used

// ✅ ZERO MUDA: Delete dead code
fn create_test_user() -> User { /* ... */ }
```

**Saved:** 30 lines of maintenance burden.

#### 6. Motion (Context Switching)

```rust
// ❌ MUDA: Tests scattered across 10 files
// tests/user/test_creation.rs
// tests/user/test_validation.rs
// tests/user/test_deletion.rs
// ... (switching between files wastes time)

// ✅ ZERO MUDA: Related tests together
// tests/user.rs (all user tests in one file)
#[cfg(test)]
mod user_tests {
    #[test] fn test_create() { /* ... */ }
    #[test] fn test_validate() { /* ... */ }
    #[test] fn test_delete() { /* ... */ }
}
```

**Saved:** 5 minutes/day in context switching.

#### 7. Defects (Flaky Tests)

```rust
// ❌ MUDA: Flaky test (fails randomly)
#[test]
fn test_concurrent_access() {
    let shared = Arc::new(Mutex::new(0));
    let handles: Vec<_> = (0..10)
        .map(|_| {
            let shared = Arc::clone(&shared);
            thread::spawn(move || {
                *shared.lock().unwrap() += 1;
            })
        })
        .collect();

    // Race condition: sometimes passes, sometimes fails
    assert_eq!(*shared.lock().unwrap(), 10);
}

// ✅ ZERO MUDA: Wait for completion
#[test]
fn test_concurrent_access() {
    let shared = Arc::new(Mutex::new(0));
    let handles: Vec<_> = (0..10)
        .map(|_| {
            let shared = Arc::clone(&shared);
            thread::spawn(move || {
                *shared.lock().unwrap() += 1;
            })
        })
        .collect();

    for handle in handles {
        handle.join().unwrap();  // Wait for all threads
    }

    assert_eq!(*shared.lock().unwrap(), 10);
}
```

**Saved:** 10% CI failure rate → wasted developer time investigating.

---

### Muri (Overburden)

**Definition:** Overburdening people or systems beyond capacity.

**In Testing:**
```rust
// ❌ MURI: Single test doing too much
#[test]
fn test_entire_user_lifecycle() {
    // 200 lines testing:
    // - User creation
    // - Email validation
    // - Password hashing
    // - Login flow
    // - Profile updates
    // - Account deletion
    // (takes 30 seconds, hard to debug when it fails)
}

// ✅ ZERO MURI: Split into focused tests
#[test]
fn test_user_creation() { /* 5 lines, 100ms */ }

#[test]
fn test_email_validation() { /* 5 lines, 10ms */ }

#[test]
fn test_password_hashing() { /* 5 lines, 50ms */ }

#[test]
fn test_login_flow() { /* 10 lines, 200ms */ }

#[test]
fn test_profile_updates() { /* 8 lines, 150ms */ }

#[test]
fn test_account_deletion() { /* 6 lines, 100ms */ }
```

**Benefits:**
- Failures pinpoint exact issue (no debugging 200-line test)
- Tests run in parallel (6× faster on 6-core CPU)
- Each test fits in working memory (easier to understand)

---

## Gemba: Go and See

**Gemba (現場):** Japanese for "the real place." Go to where work happens to understand reality.

**In Software:** Don't trust metrics alone. Read the actual test code.

### Gemba Walk Example

**Step 1: Read 10 Random Tests**

```rust
// Test 1: tests/graph/test_export.rs
#[test]
fn test_export() {
    let graph = create_test_graph();
    let result = export(&graph, "rdf");
    assert!(result.contains("<rdf:RDF"));
}

// Test 2: tests/ontology/test_validate.rs
#[test]
fn test_validation() {
    let ont = load_ontology("test.owl");
    assert!(validate(&ont).is_ok());
}

// ... 8 more tests
```

**Step 2: Apply 8-Point Checklist**

| Criterion | Score (1-5) | Issues Found |
|-----------|-------------|--------------|
| 1. Observability | 2 | No logging, hard to debug |
| 2. Isolation | 3 | Shares test files |
| 3. Clarity | 4 | Names are descriptive |
| 4. Edge Cases | 1 | Only happy paths |
| 5. Performance | 2 | Loads files unnecessarily |
| 6. Determinism | 3 | Some tests flaky |
| 7. Error Messages | 2 | Generic assertions |
| 8. Maintainability | 3 | Some duplication |

**Step 3: Prioritize Fixes**

Focus on lowest scores: Edge Cases (1) and Observability (2).

**See:** [Gemba Checklist Reference](../reference/gemba-checklist.md) for full 8-point criteria.

---

## Andon: Visual Management

**Andon (行灯):** Japanese for "lantern." Visual alert system to stop production when defects detected.

**In Software:** CI/CD alerts based on test failure thresholds.

### Andon Cord System

```yaml
# .github/workflows/ci.yml

- name: Run Tests
  run: cargo test --all

- name: Check Andon Status
  run: |
    FAILURES=$(cargo test --all 2>&1 | grep -c "FAILED")

    if [ $FAILURES -ge 6 ]; then
      echo "🔴 RED ANDON: $FAILURES failures (≥6)"
      echo "ACTION: Stop all merges, team meeting required"
      exit 1
    elif [ $FAILURES -ge 1 ]; then
      echo "🟡 YELLOW ANDON: $FAILURES failures (1-5)"
      echo "ACTION: Fix before next feature work"
      exit 1
    else
      echo "✅ GREEN: 0 failures"
    fi
```

**Alert Levels:**
- **Green (0 failures):** Normal operation, proceed with feature work
- **Yellow (1-5 failures):** Warning, fix before adding new features
- **Red (6+ failures):** STOP, emergency team meeting to investigate

**Impact:** Prevents cascading failures (one broken test → 20 broken tests).

---

## Poka-Yoke: Mistake-Proofing

**Poka-Yoke (ポカヨケ):** Japanese for "error-proofing." Design systems to prevent mistakes.

**In Software:** Use the type system to make errors impossible.

### Pattern 1: Guide Pin (Compile-Time Constraints)

```rust
// ❌ Runtime error possible
fn export(graph: &Graph, format: &str) -> Result<String, Error> {
    match format {
        "rdf" => export_rdf(graph),
        "owl" => export_owl(graph),
        _ => Err(Error::InvalidFormat),  // Oops, typo "rdff" accepted
    }
}

// ✅ Compile-time prevention
enum Format { Rdf, Owl, Turtle }

fn export(graph: &Graph, format: Format) -> String {
    match format {
        Format::Rdf => export_rdf(graph),
        Format::Owl => export_owl(graph),
        Format::Turtle => export_turtle(graph),
    }
}

// export(&graph, "rdff");  // ❌ Compiler error!
// export(&graph, Format::Rdf);  // ✅ Only valid formats
```

**Mistake Prevented:** Invalid format strings (caught at compile time).

### Pattern 2: Limit Switch (Bounded Values)

```rust
// ❌ Negative ages possible
struct User {
    age: i32,  // Can be -5!
}

// ✅ Impossible to create invalid user
struct Age(u8);  // 0-255 only

impl Age {
    fn new(value: u8) -> Result<Self, Error> {
        if value > 120 {
            Err(Error::InvalidAge)
        } else {
            Ok(Age(value))
        }
    }
}

struct User {
    age: Age,  // Guaranteed valid
}
```

**Mistake Prevented:** Invalid ages (-5, 999) rejected at construction.

---

## Real-World Case Study

**Scenario:** ggen test suite optimization (Week 1)

### Before Lean

```
Tests: 487 total
Duration: 12 minutes 34 seconds
Flaky tests: 23 (4.7% failure rate)
Duplicate tests: 67
Test file count: 89 files
```

### After Lean (3M Framework Applied)

| Principle | Action Taken | Result |
|-----------|--------------|--------|
| **Mura Elimination** | Standardized test structure (AAA pattern) | 30% faster code review |
| **Muda Elimination** | Removed 67 duplicate tests | 8 minutes saved per CI run |
| **Muri Reduction** | Split 12 mega-tests into 48 focused tests | Parallelization enabled |
| **Gemba Walks** | Weekly 30-minute test review | 23 → 3 flaky tests |
| **Andon System** | Yellow (1-5) / Red (6+) alerts | 0 cascading failures |
| **Poka-Yoke** | Enum-based API (no string typos) | 14 bugs prevented |

**Final Metrics:**
```
Tests: 468 total (-19 duplicates, +48 from splits, -67 removed)
Duration: 4 minutes 12 seconds (-66% time)
Flaky tests: 3 (-87%)
Duplicate tests: 0 (-100%)
Test file count: 34 files (-62%, grouped by module)
```

**ROI:** 8.4 minutes saved × 50 CI runs/day = 7 hours/day team productivity gain.

---

## Glossary

| Term | Japanese | Definition |
|------|----------|------------|
| **Mura** | 斑 | Inconsistency, unevenness |
| **Muda** | 無駄 | Waste, non-value-adding activity |
| **Muri** | 無理 | Overburden, unreasonableness |
| **Gemba** | 現場 | The real place, go-and-see |
| **Andon** | 行灯 | Visual alert system |
| **Poka-Yoke** | ポカヨケ | Mistake-proofing |
| **Kaizen** | 改善 | Continuous improvement |

---

## Next Steps

Now that you understand Lean Manufacturing principles:

1. **[How-to: Refactor Tests with Lean](../how-to/refactor-tests-with-lean.md)** - Apply 3M framework to your tests
2. **[How-to: Run Gemba Walk](../how-to/run-gemba-walk.md)** - Conduct systematic test quality inspection
3. **[Explanation: Why Lean Manufacturing Works](../explanations/why-lean-manufacturing-works.md)** - Understand the philosophy
4. **[Reference: FMEA Matrix](../reference/FMEA-matrix.md)** - Prioritize test coverage by risk

**Practice Exercise:** Run a Gemba walk on your test suite. Use the 8-point checklist. Identify the top 3 sources of Muda.

---

**Tutorial Complete!** You can now apply Toyota Production System principles to improve test quality and efficiency.
