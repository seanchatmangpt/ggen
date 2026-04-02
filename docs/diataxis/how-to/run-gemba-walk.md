<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How-to: Run Gemba Walk](#how-to-run-gemba-walk)
  - [Prerequisites](#prerequisites)
  - [Problem Statement](#problem-statement)
  - [What is a Gemba Walk?](#what-is-a-gemba-walk)
  - [Step 1: Randomly Sample Tests](#step-1-randomly-sample-tests)
    - [Select 10 Tests (Stratified Sampling)](#select-10-tests-stratified-sampling)
  - [Step 2: Apply 8-Point Quality Checklist](#step-2-apply-8-point-quality-checklist)
    - [1. Observability](#1-observability)
    - [2. Isolation](#2-isolation)
    - [3. Clarity](#3-clarity)
    - [4. Edge Cases](#4-edge-cases)
    - [5. Performance](#5-performance)
    - [6. Determinism](#6-determinism)
    - [7. Error Messages](#7-error-messages)
    - [8. Maintainability](#8-maintainability)
  - [Step 3: Score and Aggregate](#step-3-score-and-aggregate)
    - [Scoring Template](#scoring-template)
    - [Interpret Results](#interpret-results)
  - [Step 4: Prioritize Improvements](#step-4-prioritize-improvements)
    - [Focus on Lowest Scores](#focus-on-lowest-scores)
    - [Create Action Items](#create-action-items)
  - [Step 5: Track Progress](#step-5-track-progress)
    - [Monthly Gemba Walks](#monthly-gemba-walks)
    - [Trend Analysis](#trend-analysis)
  - [Verification Checklist](#verification-checklist)
  - [Related Guides](#related-guides)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# How-to: Run Gemba Walk

**Inspect test quality using 8-point checklist (observability, isolation, clarity, edge cases)**

---

## Prerequisites

- Access to test suite source code
- 30-60 minutes for inspection
- Pen and paper (or spreadsheet) for scoring

---

## Problem Statement

You don't know the true quality of your tests. Metrics (pass rate, coverage) don't tell the full story:
- Are tests easy to debug?
- Do they cover edge cases?
- Can new developers understand them?

This guide shows you how to conduct a Gemba (go-and-see) walk to assess real test quality.

---

## What is a Gemba Walk?

**Gemba (現場):** Japanese for "the real place." In Lean Manufacturing, managers go to the factory floor to observe actual work, not just read reports.

**In Software:** Read actual test code, don't just trust CI dashboards.

---

## Step 1: Randomly Sample Tests

### Select 10 Tests (Stratified Sampling)

```bash
# Find all test files
find tests/ -name "*.rs" -type f > test_files.txt

# Randomly select 3 files per module
shuf test_files.txt | head -10
```

**Example Output:**
```
tests/graph/test_export.rs
tests/ontology/test_validate.rs
tests/lifecycle/test_optimize.rs
tests/graph/test_import.rs
tests/ontology/test_circular_refs.rs
tests/lifecycle/test_deploy.rs
tests/graph/test_cycles.rs
tests/ontology/test_inference.rs
tests/lifecycle/test_rollback.rs
tests/graph/test_subgraphs.rs
```

**Why Random?** Prevents cherry-picking "good" tests. Get representative sample.

---

## Step 2: Apply 8-Point Quality Checklist

For each test, score 1-5 (1=poor, 5=excellent):

### 1. Observability

**Question:** If this test fails, can I quickly determine why?

**Scoring:**
- **5:** Clear error messages, logs at each step
- **3:** Generic assertion failure, some context
- **1:** No logging, cryptic failure

**Example:**

```rust
// Score: 1 (Poor observability)
#[test]
fn test_export() {
    let graph = create_graph();
    assert!(export(&graph).is_ok());  // ❌ Why did it fail?
}

// Score: 5 (Excellent observability)
#[test]
fn test_graph_export_produces_valid_rdf() {
    // Arrange
    let graph = create_sample_graph();
    eprintln!("Graph nodes: {}", graph.node_count());

    // Act
    let result = export(&graph, Format::Rdf);

    // Assert
    match result {
        Ok(output) => {
            eprintln!("Export succeeded, length: {}", output.len());
            assert!(output.contains("<rdf:RDF"), "Missing RDF header");
            assert!(output.contains("</rdf:RDF>"), "Missing RDF footer");
        }
        Err(e) => {
            eprintln!("Export failed: {:?}", e);
            panic!("Export should succeed");
        }
    }
}
```

### 2. Isolation

**Question:** Does this test depend on external state (files, network, other tests)?

**Scoring:**
- **5:** Completely isolated, uses in-memory mocks
- **3:** Reads test fixtures from disk
- **1:** Requires database setup, network access

**Example:**

```rust
// Score: 1 (Poor isolation)
#[test]
fn test_load_config() {
    let config = load_from_file("/etc/ggen/config.toml");  // ❌ File dependency
    assert!(config.is_ok());
}

// Score: 5 (Excellent isolation)
#[test]
fn test_parse_config() {
    let toml_content = r#"
        [settings]
        verbose = true
    "#;

    let config = parse_config(toml_content);  // ✅ No file I/O
    assert_eq!(config.verbose, true);
}
```

### 3. Clarity

**Question:** Can a new developer understand what's being tested in 30 seconds?

**Scoring:**
- **5:** Clear name, AAA structure, comments explaining why
- **3:** Descriptive name, basic structure
- **1:** Cryptic name, 200-line test doing many things

**Example:**

```rust
// Score: 1 (Poor clarity)
#[test]
fn test1() {  // ❌ What is test1?
    let g = Graph::new();
    g.add_node("A");
    g.add_node("B");
    g.add_edge("A", "B");
    assert_eq!(g.node_count(), 2);
    assert_eq!(g.edge_count(), 1);
    let result = export(&g);
    assert!(result.is_ok());
    // ... 50 more lines
}

// Score: 5 (Excellent clarity)
/// Verifies that exporting a simple 2-node graph produces valid RDF.
/// This test covers the happy path for small graphs.
#[test]
fn test_graph_export_two_nodes_produces_valid_rdf() {
    // Arrange: Create graph with 2 nodes and 1 edge
    let mut graph = Graph::new();
    graph.add_node("A");
    graph.add_node("B");
    graph.add_edge("A", "B");

    // Act: Export to RDF format
    let rdf_output = export(&graph, Format::Rdf);

    // Assert: Valid RDF structure
    assert!(rdf_output.is_ok());
    let output = rdf_output.unwrap();
    assert!(output.contains("<rdf:RDF"));
    assert!(output.contains("</rdf:RDF>"));
}
```

### 4. Edge Cases

**Question:** Does this test cover error conditions and boundary cases?

**Scoring:**
- **5:** Tests happy path + 3+ edge cases (empty input, nulls, max values)
- **3:** Tests happy path + 1 edge case
- **1:** Only happy path

**Example:**

```rust
// Score: 1 (Only happy path)
#[test]
fn test_add_node() {
    let mut g = Graph::new();
    g.add_node("A");
    assert_eq!(g.node_count(), 1);
}

// Score: 5 (Happy path + edge cases)
#[test]
fn test_add_node_happy_path() {
    let mut g = Graph::new();
    g.add_node("A");
    assert_eq!(g.node_count(), 1);
}

#[test]
fn test_add_node_duplicate_name() {
    let mut g = Graph::new();
    g.add_node("A");
    g.add_node("A");  // Duplicate
    assert_eq!(g.node_count(), 1);  // Should not add duplicate
}

#[test]
fn test_add_node_empty_name() {
    let mut g = Graph::new();
    let result = g.add_node("");  // Edge case: empty name
    assert!(result.is_err());
}

#[test]
fn test_add_node_max_capacity() {
    let mut g = Graph::with_capacity(100);
    for i in 0..100 {
        g.add_node(&format!("node_{}", i));
    }
    let result = g.add_node("overflow");  // Exceeds capacity
    assert!(result.is_err());
}
```

### 5. Performance

**Question:** Does this test run quickly (<100ms)?

**Scoring:**
- **5:** <10ms (unit test)
- **3:** 10-100ms (integration test)
- **1:** >1s (slow, blocks CI)

**Measure:**

```bash
cargo test test_name -- --nocapture 2>&1 | grep "finished in"
# Example: finished in 0.03s → Score: 5
# Example: finished in 2.45s → Score: 1
```

### 6. Determinism

**Question:** Does this test produce the same result every time?

**Scoring:**
- **5:** 100% deterministic (pure functions)
- **3:** 95% pass rate (occasional flakiness)
- **1:** <90% pass rate (chronically flaky)

**Test:**

```bash
# Run test 10 times
for i in {1..10}; do
    cargo test test_name --quiet || echo "FAIL $i"
done | grep -c "FAIL"

# 0 failures → Score: 5
# 1 failure → Score: 3
# 2+ failures → Score: 1
```

### 7. Error Messages

**Question:** Do assertion failures provide actionable information?

**Scoring:**
- **5:** Custom error messages explaining what went wrong
- **3:** Default assertion messages
- **1:** No context in failures

**Example:**

```rust
// Score: 1 (Poor error messages)
#[test]
fn test_export() {
    let graph = create_graph();
    assert!(export(&graph).is_ok());
    // Failure: "assertion failed: export(&graph).is_ok()"
    // ❌ Doesn't tell us WHY export failed
}

// Score: 5 (Excellent error messages)
#[test]
fn test_graph_export_succeeds() {
    let graph = create_sample_graph();
    let result = export(&graph, Format::Rdf);

    assert!(
        result.is_ok(),
        "Export should succeed for valid graph with {} nodes and {} edges, error: {:?}",
        graph.node_count(),
        graph.edge_count(),
        result.err()
    );
}
```

### 8. Maintainability

**Question:** If I need to update this test due to API changes, is it easy?

**Scoring:**
- **5:** Uses helper functions, no duplication
- **3:** Some duplication, but localized
- **1:** Copy-pasted code across 10 tests

**Example:**

```rust
// Score: 1 (Poor maintainability)
#[test]
fn test_export_rdf() {
    let mut g = Graph::new();
    g.add_node("A");
    g.add_node("B");
    g.add_edge("A", "B");
    // ... 20 more setup lines (duplicated in 10 tests)
    assert!(export(&g).is_ok());
}

// Score: 5 (Excellent maintainability)
fn create_two_node_graph() -> Graph {
    let mut g = Graph::new();
    g.add_node("A");
    g.add_node("B");
    g.add_edge("A", "B");
    g
}

#[test]
fn test_export_rdf() {
    let graph = create_two_node_graph();  // ✅ Reusable helper
    assert!(export(&graph, Format::Rdf).is_ok());
}

#[test]
fn test_export_owl() {
    let graph = create_two_node_graph();  // ✅ Same helper
    assert!(export(&graph, Format::Owl).is_ok());
}
```

---

## Step 3: Score and Aggregate

### Scoring Template

| Test Name | Obs. | Isol. | Clarity | Edge | Perf. | Determ. | Errors | Maint. | **Total** |
|-----------|------|-------|---------|------|-------|---------|--------|--------|-----------|
| test_export_rdf | 2 | 3 | 4 | 1 | 5 | 5 | 2 | 3 | **25/40** |
| test_validate_ont | 4 | 5 | 5 | 3 | 4 | 5 | 4 | 5 | **35/40** |
| test_lifecycle_opt | 1 | 2 | 2 | 1 | 1 | 3 | 1 | 2 | **13/40** |
| ... | ... | ... | ... | ... | ... | ... | ... | ... | ... |
| **Average** | 2.8 | 3.5 | 3.2 | 1.7 | 3.6 | 4.1 | 2.5 | 3.3 | **24.7/40** |

### Interpret Results

**Overall Score:**
- 32-40: Excellent test quality
- 24-31: Good, some improvement needed
- 16-23: Fair, significant gaps
- <16: Poor, urgent refactor needed

**Weakest Areas (from example above):**
1. **Edge Cases (1.7):** Most tests only cover happy path
2. **Observability (2.8):** Poor error messages, hard to debug
3. **Error Messages (2.5):** Generic assertions

**Action Plan:**
- Add edge case tests (empty inputs, boundary conditions)
- Improve logging in existing tests
- Use custom assertion messages

---

## Step 4: Prioritize Improvements

### Focus on Lowest Scores

```
Lowest-scoring criteria:
1. Edge Cases (1.7) → Add 3 edge case tests per module
2. Error Messages (2.5) → Enhance assertion messages
3. Observability (2.8) → Add eprintln! logging

Highest-impact improvements:
- Edge Cases: Prevents 70% of production bugs (historical data)
- Error Messages: Saves 15 min/bug investigation
```

### Create Action Items

```markdown
## Gemba Walk Action Items (Week of 2025-11-18)

**High Priority:**
- [ ] Add edge case tests for empty graph export (Score: 1 → 5)
- [ ] Add edge case tests for circular ontology references (Score: 1 → 5)
- [ ] Improve error messages in lifecycle tests (Score: 1 → 4)

**Medium Priority:**
- [ ] Add logging to graph export tests (Score: 2 → 4)
- [ ] Refactor test_lifecycle_opt to use helper functions (Score: 2 → 4)

**Low Priority:**
- [ ] Document why test_async_export has intentional flakiness (Score: 3 → 5)
```

---

## Step 5: Track Progress

### Monthly Gemba Walks

Schedule recurring reviews:

```
Week 1: Conduct Gemba walk (sample 10 tests)
Week 2: Implement high-priority fixes
Week 3: Re-score fixed tests
Week 4: Measure aggregate improvement
```

### Trend Analysis

| Month | Avg Score | Edge Cases | Observability | Maintainability |
|-------|-----------|------------|---------------|-----------------|
| Jan 2025 | 24.7 | 1.7 | 2.8 | 3.3 |
| Feb 2025 | 27.3 (+2.6) | 2.9 (+1.2) | 3.5 (+0.7) | 3.7 (+0.4) |
| Mar 2025 | 31.2 (+3.9) | 4.1 (+1.2) | 4.2 (+0.7) | 4.3 (+0.6) |

**Goal:** Reach 32+ average score (excellent) by Q2 2025.

---

## Verification Checklist

After Gemba walk:

- [ ] 10+ tests randomly sampled (stratified across modules)
- [ ] All 8 criteria scored for each test
- [ ] Average scores calculated
- [ ] Weakest areas identified
- [ ] Action items prioritized (high/medium/low)
- [ ] Follow-up Gemba walk scheduled (1 month out)

---

## Related Guides

- [Tutorial: Lean Manufacturing Intro](../tutorials/04-lean-manufacturing-intro.md) - Learn Gemba concept
- [Gemba Checklist Reference](../reference/gemba-checklist.md) - Full 8-point criteria
- [How-to: Refactor Tests with Lean](refactor-tests-with-lean.md) - Apply improvements

---

**Last Updated:** 2025-11-18
