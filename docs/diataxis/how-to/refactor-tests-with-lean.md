# How-to: Refactor Tests with Lean

**Apply Mura (inconsistency) and Muda (waste) elimination to test suites**

---

## Prerequisites

- Existing test suite with known quality issues
- Understanding of Lean Manufacturing principles (see [Tutorial 04](../tutorials/04-lean-manufacturing-intro.md))
- Access to test execution metrics (duration, flakiness rate)

---

## Problem Statement

Your test suite has:
- Inconsistent structure (Mura)
- Duplicate tests (Muda)
- Slow execution times (Muda)
- Flaky tests (Muda - defects)

This guide shows you how to apply Lean principles to eliminate waste and inconsistency.

---

## Step 1: Measure Current State

### Collect Baseline Metrics

```bash
# Run tests with timing
cargo test --all -- --nocapture 2>&1 | tee baseline.log

# Extract metrics
grep "test result:" baseline.log
# Example output: test result: ok. 487 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 754.23s
```

**Record:**
- Total tests: 487
- Duration: 754.23s (12.6 minutes)
- Pass rate: 100% (but check for flakiness over 10 runs)

### Identify Flaky Tests

```bash
# Run tests 10 times
for i in {1..10}; do
    cargo test --all 2>&1 | grep "FAILED" >> flaky_tests.log
done

# Count failures per test
sort flaky_tests.log | uniq -c | sort -rn
```

**Example Output:**
```
7 test graph::test_concurrent_export ... FAILED
4 test ontology::test_async_validate ... FAILED
2 test lifecycle::test_deploy ... FAILED
```

**Analysis:** 3 tests are flaky (fail sometimes), need stabilization.

---

## Step 2: Eliminate Mura (Inconsistency)

### Pattern 1: Standardize Test Structure (AAA)

**Before (Inconsistent):**

```rust
// File 1: tests/graph_tests.rs
#[test]
fn export_test() {  // Name format 1
    let graph = Graph::new();
    graph.add_node("A");
    assert_eq!(export(&graph), "<graph>...</graph>");
}

// File 2: tests/ontology_tests.rs
#[test]
fn test_validate_ontology() {  // Name format 2
    let ont = load("test.owl");  // No comments
    assert!(validate(&ont).is_ok());
}

// File 3: tests/lifecycle_tests.rs
#[test]
fn it_should_optimize() {  // Name format 3
    // Setup
    let config = Config::default();

    // Execute
    let result = optimize(&config);

    // Verify
    assert!(result.is_ok());
}
```

**After (Consistent AAA Pattern):**

```rust
// All test files follow same structure
#[test]
fn test_graph_export_formats_correctly() {
    // Arrange
    let graph = Graph::new();
    graph.add_node("A");

    // Act
    let output = export(&graph);

    // Assert
    assert_eq!(output, "<graph>...</graph>");
}

#[test]
fn test_ontology_validates_successfully() {
    // Arrange
    let ontology = load("test.owl");

    // Act
    let result = validate(&ontology);

    // Assert
    assert!(result.is_ok());
}

#[test]
fn test_lifecycle_optimizes_configuration() {
    // Arrange
    let config = Config::default();

    // Act
    let result = optimize(&config);

    // Assert
    assert!(result.is_ok());
}
```

**Benefits:**
- 30% faster code review (predictable structure)
- 50% faster onboarding (one pattern to learn)
- Easier to spot missing assertions (every test has Assert section)

### Pattern 2: Consistent Naming

**Convention:**
```rust
// Format: test_{module}_{action}_{expected_outcome}
#[test]
fn test_graph_export_produces_valid_rdf() { /* ... */ }

#[test]
fn test_graph_import_handles_invalid_input() { /* ... */ }

#[test]
fn test_ontology_validate_rejects_circular_refs() { /* ... */ }
```

**Apply Across Codebase:**

```bash
# Find non-conforming test names
rg "fn (test_\w+|it_should_\w+|\w+_test)" tests/ --type rust | \
  grep -v "fn test_[a-z_]+_[a-z_]+_[a-z_]+" > non_conforming.txt

# Manually rename each (use sed for bulk renaming)
```

---

## Step 3: Eliminate Muda (Waste)

### Muda Type 1: Overproduction (Duplicate Tests)

**Detect Duplicates:**

```bash
# Find tests with identical assertions
rg "assert_eq!" tests/ -A 5 -B 5 | sort | uniq -c | grep "^\s*[2-9]"
```

**Example Found:**

```rust
// tests/graph/test_node.rs
#[test]
fn test_add_node() {
    let mut g = Graph::new();
    g.add_node("A");
    assert_eq!(g.node_count(), 1);
}

// tests/graph/test_basic.rs
#[test]
fn test_graph_node_addition() {
    let mut g = Graph::new();
    g.add_node("A");
    assert_eq!(g.node_count(), 1);  // Duplicate!
}
```

**Solution: Consolidate**

```rust
// Keep only one, delete the other
#[test]
fn test_graph_add_single_node() {
    // Arrange
    let mut graph = Graph::new();

    // Act
    graph.add_node("A");

    // Assert
    assert_eq!(graph.node_count(), 1);
}
```

**Result:** 67 duplicate tests removed → 420 tests remaining.

### Muda Type 2: Waiting (Slow Tests)

**Find Slowest Tests:**

```bash
# Run with timing
cargo test -- --nocapture 2>&1 | grep "test.*ok" | \
  awk '{print $NF, $2}' | sort -rh | head -20
```

**Example Output:**
```
5.234s test_async_graph_export
3.891s test_full_lifecycle_simulation
2.456s test_large_ontology_validation
```

**Optimize:**

```rust
// ❌ BEFORE: Unnecessary sleep
#[test]
fn test_async_graph_export() {
    start_export();
    std::thread::sleep(Duration::from_secs(5));  // Overkill!
    assert!(export_complete());
}

// ✅ AFTER: Poll with timeout
#[test]
fn test_async_graph_export() {
    start_export();

    let start = Instant::now();
    while !export_complete() {
        if start.elapsed() > Duration::from_millis(100) {
            panic!("Export timeout");
        }
        std::thread::sleep(Duration::from_millis(10));
    }

    assert!(export_complete());
}
```

**Result:** 5.234s → 0.087s (60× faster).

### Muda Type 3: Transportation (File I/O)

**Before:**

```rust
#[test]
fn test_parse_ontology() {
    let content = std::fs::read_to_string("tests/fixtures/test.owl").unwrap();  // Slow I/O
    let parsed = parse(&content);
    assert!(parsed.is_ok());
}
```

**After:**

```rust
#[test]
fn test_parse_ontology() {
    let content = r#"
        <rdf:RDF>
            <owl:Ontology />
        </rdf:RDF>
    "#;  // Inline, no I/O

    let parsed = parse(content);
    assert!(parsed.is_ok());
}
```

**Result:** 50ms → 2ms per test.

### Muda Type 4: Defects (Flaky Tests)

**Fix Flaky Test:**

```rust
// ❌ BEFORE: Race condition
#[test]
fn test_concurrent_export() {
    let graph = Arc::new(Graph::new());
    let handles: Vec<_> = (0..10)
        .map(|_| {
            let g = Arc::clone(&graph);
            thread::spawn(move || export(&g))
        })
        .collect();

    // No join! Test ends before threads complete
    assert!(true);  // Always passes, but meaningless
}

// ✅ AFTER: Wait for completion
#[test]
fn test_concurrent_export() {
    let graph = Arc::new(Graph::new());
    let handles: Vec<_> = (0..10)
        .map(|_| {
            let g = Arc::clone(&graph);
            thread::spawn(move || export(&g))
        })
        .collect();

    let results: Vec<_> = handles.into_iter()
        .map(|h| h.join().unwrap())
        .collect();

    assert_eq!(results.len(), 10);
    assert!(results.iter().all(|r| r.is_ok()));
}
```

**Result:** 70% flakiness → 0% flakiness.

---

## Step 4: Apply Poka-Yoke (Mistake-Proofing)

### Pattern 1: Use Type System to Prevent Errors

```rust
// ❌ BEFORE: String-based config (typos possible)
#[test]
fn test_export_format() {
    let output = export_graph(&graph, "rdff");  // Typo! Test passes but wrong
    assert!(output.contains("rdf"));  // False positive
}

// ✅ AFTER: Enum-based config
enum Format { Rdf, Owl, Turtle }

#[test]
fn test_export_format_rdf() {
    let output = export_graph(&graph, Format::Rdf);
    // Format::Rdff  // ❌ Compile error (poka-yoke!)
    assert!(output.contains("<rdf:RDF"));
}
```

### Pattern 2: Compile-Time Bounds

```rust
// ❌ BEFORE: Runtime validation
#[test]
fn test_node_limit() {
    let graph = create_graph_with_nodes(1000000);  // OOM!
    assert!(graph.node_count() <= 10000);  // Too late
}

// ✅ AFTER: Compile-time bounds
struct BoundedGraph<const MAX: usize> {
    nodes: ArrayVec<Node, MAX>,
}

#[test]
fn test_node_limit() {
    let graph = BoundedGraph::<100>::new();
    // graph.nodes[101];  // ❌ Compile error (array bounds)
    assert!(graph.node_count() <= 100);
}
```

---

## Step 5: Measure Improvement

### Re-run Metrics

```bash
cargo test --all -- --nocapture 2>&1 | tee after.log
grep "test result:" after.log
```

**Results:**

| Metric | Before | After | Improvement |
|--------|--------|-------|-------------|
| Total tests | 487 | 420 | -67 (removed duplicates) |
| Duration | 754.23s | 178.45s | -76% |
| Flaky tests | 3 | 0 | -100% |
| Avg test time | 1.55s | 0.42s | -73% |
| Code review time | 45 min | 32 min | -29% |

### Calculate ROI

```
Time saved per CI run: 754s - 178s = 576s (9.6 minutes)
CI runs per day: 50
Daily savings: 50 × 9.6 min = 480 min (8 hours)
Developer cost: $100/hour
Annual savings: 8 hours/day × $100/hour × 250 days = $200,000

Refactoring time investment: 16 hours
Break-even: 16 hours / 8 hours/day = 2 days
```

**ROI: 12,500% annually**

---

## Verification Checklist

After refactoring:

- [ ] All tests follow AAA (Arrange-Act-Assert) pattern
- [ ] Test names follow `test_{module}_{action}_{outcome}` convention
- [ ] Zero duplicate tests (check with assertion search)
- [ ] No flaky tests (run 10 times, 100% pass rate)
- [ ] Average test duration < 500ms
- [ ] All file I/O replaced with inline data (where feasible)
- [ ] Type system prevents common errors (enums vs strings)
- [ ] CI duration reduced by >50%

---

## Automation Script

```bash
#!/bin/bash
# scripts/lean_test_refactor.sh

echo "🏭 Lean Test Refactoring Tool"
echo "=============================="

# 1. Find duplicates
echo "📊 Analyzing for duplicates..."
DUPLICATES=$(rg "assert_eq!" tests/ -A 2 | sort | uniq -c | grep "^\s*[2-9]" | wc -l)
echo "Found $DUPLICATES potential duplicate assertions"

# 2. Find slow tests
echo "⏱️  Finding slow tests (>1s)..."
cargo test -- --nocapture 2>&1 | grep "test.*ok" | \
  awk '$NF ~ /[0-9]+\.[0-9]+s/ && $NF+0 > 1.0 {print $NF, $2}' | \
  sort -rh > slow_tests.txt
SLOW_COUNT=$(wc -l < slow_tests.txt)
echo "Found $SLOW_COUNT tests >1s"

# 3. Check naming consistency
echo "📝 Checking naming conventions..."
NONCONFORMING=$(rg "fn (it_should_|should_|\w+_test)\(" tests/ --type rust | wc -l)
echo "Found $NONCONFORMING non-conforming test names"

# 4. Measure flakiness
echo "🎲 Testing for flakiness (3 runs)..."
for i in {1..3}; do
    cargo test --all 2>&1 | grep "test result:" >> flaky_check.log
done
FLAKY=$(grep "failed" flaky_check.log | wc -l)
echo "Detected $FLAKY flaky test occurrences"

echo ""
echo "📋 Summary:"
echo "- Duplicates: $DUPLICATES"
echo "- Slow tests: $SLOW_COUNT"
echo "- Naming issues: $NONCONFORMING"
echo "- Flaky occurrences: $FLAKY"
```

---

## Related Guides

- [Tutorial: Lean Manufacturing Intro](../tutorials/04-lean-manufacturing-intro.md) - Learn 3M framework
- [How-to: Run Gemba Walk](run-gemba-walk.md) - Inspect test quality
- [Why Lean Manufacturing Works](../explanations/why-lean-manufacturing-works.md) - Understand philosophy

---

**Last Updated:** 2025-11-18
