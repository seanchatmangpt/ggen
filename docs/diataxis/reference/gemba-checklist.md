<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Reference: Gemba Checklist](#reference-gemba-checklist)
  - [Quick Reference Card](#quick-reference-card)
  - [Detailed Criteria](#detailed-criteria)
    - [1. Observability](#1-observability)
    - [2. Isolation](#2-isolation)
    - [3. Clarity](#3-clarity)
    - [4. Edge Cases](#4-edge-cases)
    - [5. Performance](#5-performance)
    - [6. Determinism](#6-determinism)
    - [7. Error Messages](#7-error-messages)
    - [8. Maintainability](#8-maintainability)
  - [Scoring Matrix](#scoring-matrix)
  - [Related Resources](#related-resources)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Reference: Gemba Checklist

**8-point test quality inspection criteria with scoring guide**

---

## Quick Reference Card

| # | Criterion | 5 (Excellent) | 3 (Fair) | 1 (Poor) |
|---|-----------|---------------|----------|----------|
| 1 | **Observability** | Detailed logs, custom error messages | Basic assertions | No context |
| 2 | **Isolation** | In-memory mocks, no external deps | Reads test fixtures | Requires DB/network |
| 3 | **Clarity** | AAA structure, clear name, comments | Basic structure | Cryptic, 200+ lines |
| 4 | **Edge Cases** | 3+ edge cases tested | 1 edge case | Only happy path |
| 5 | **Performance** | <10ms | 10-100ms | >1s |
| 6 | **Determinism** | 100% pass rate | 95% pass rate | <90% pass rate |
| 7 | **Error Messages** | Actionable context in assertions | Default messages | No context |
| 8 | **Maintainability** | Helper functions, DRY | Some duplication | Copy-pasted code |

**Scoring:** Add all 8 scores. Max = 40 points.
- 32-40: Excellent
- 24-31: Good
- 16-23: Needs work
- <16: Urgent refactor

---

## Detailed Criteria

### 1. Observability

**Question:** If this test fails, can I debug it in <5 minutes?

**Score 5:**
```rust
#[test]
fn test_export() {
    let graph = create_sample_graph();
    eprintln!("Graph: {} nodes, {} edges", graph.node_count(), graph.edge_count());

    let result = export(&graph, Format::Rdf);

    match result {
        Ok(output) => {
            eprintln!("Export succeeded, {} bytes", output.len());
            assert!(output.contains("<rdf:RDF"), "Missing RDF header in output:\n{}", output);
        }
        Err(e) => panic!("Export failed: {:?}", e),
    }
}
```

**Score 3:**
```rust
#[test]
fn test_export() {
    let graph = create_sample_graph();
    assert!(export(&graph).is_ok());
}
```

**Score 1:**
```rust
#[test]
fn test_export() {
    assert!(true);  // What does this even test?
}
```

---

### 2. Isolation

**Question:** Can this test run offline without external resources?

**Score 5:**
```rust
#[test]
fn test_parse() {
    let input = r#"{"key": "value"}"#;  // Inline, no I/O
    assert!(parse(input).is_ok());
}
```

**Score 3:**
```rust
#[test]
fn test_parse() {
    let input = std::fs::read_to_string("tests/fixtures/test.json").unwrap();
    assert!(parse(&input).is_ok());
}
```

**Score 1:**
```rust
#[test]
fn test_parse() {
    let db = connect_to_postgres("prod-db.example.com").unwrap();  // ❌ Network dependency
    assert!(db.query("SELECT 1").is_ok());
}
```

---

### 3. Clarity

**Question:** Can a junior developer understand this in 30 seconds?

**Score 5:**
```rust
/// Verifies RDF export includes required xmlns declarations.
#[test]
fn test_rdf_export_includes_namespace_declarations() {
    // Arrange: Create graph with 2 nodes
    let mut graph = Graph::new();
    graph.add_node("A");
    graph.add_node("B");

    // Act: Export to RDF
    let rdf = export(&graph, Format::Rdf).unwrap();

    // Assert: Namespaces present
    assert!(rdf.contains("xmlns:rdf="));
    assert!(rdf.contains("xmlns:rdfs="));
}
```

**Score 3:**
```rust
#[test]
fn test_export_rdf() {
    let g = Graph::new();
    g.add_node("A");
    assert!(export(&g).unwrap().contains("xmlns"));
}
```

**Score 1:**
```rust
#[test]
fn t1() {  // Cryptic name
    let g = Graph::new();
    // ... 200 lines of setup
    assert!(export(&g).is_ok());
}
```

---

### 4. Edge Cases

**Question:** Does this test cover failure modes?

**Score 5:**
```rust
#[test] fn test_add_node_happy_path() { /* ... */ }
#[test] fn test_add_node_duplicate_name() { /* ... */ }
#[test] fn test_add_node_empty_name() { /* ... */ }
#[test] fn test_add_node_exceeds_capacity() { /* ... */ }
```

**Score 3:**
```rust
#[test] fn test_add_node() { /* happy path */ }
#[test] fn test_add_node_duplicate() { /* 1 edge case */ }
```

**Score 1:**
```rust
#[test] fn test_add_node() { /* only happy path */ }
```

---

### 5. Performance

**Question:** Does this test complete quickly?

**Measure:**
```bash
cargo test test_name -- --nocapture 2>&1 | grep "finished in"
```

**Score 5:** <10ms (unit test)
**Score 3:** 10-100ms (integration test)
**Score 1:** >1s (slow, blocks CI)

---

### 6. Determinism

**Question:** Does this test pass consistently?

**Measure:**
```bash
for i in {1..10}; do cargo test test_name || echo "FAIL"; done | grep -c "FAIL"
```

**Score 5:** 0 failures (100% deterministic)
**Score 3:** 1 failure (95% pass rate)
**Score 1:** 2+ failures (<90% pass rate)

---

### 7. Error Messages

**Question:** Do failures explain what went wrong?

**Score 5:**
```rust
assert!(
    result.is_ok(),
    "Export failed for graph with {} nodes: {:?}",
    graph.node_count(),
    result.err()
);
```

**Score 3:**
```rust
assert!(result.is_ok());  // Default message
```

**Score 1:**
```rust
assert!(true);  // No information
```

---

### 8. Maintainability

**Question:** If the API changes, how many tests need updating?

**Score 5:**
```rust
fn create_two_node_graph() -> Graph { /* helper */ }

#[test] fn test_export_rdf() { let g = create_two_node_graph(); /* ... */ }
#[test] fn test_export_owl() { let g = create_two_node_graph(); /* ... */ }
// Change create_two_node_graph() once, all tests updated
```

**Score 3:**
```rust
#[test] fn test_export_rdf() {
    let g = Graph::new();
    g.add_node("A");  // Duplicated setup
}
#[test] fn test_export_owl() {
    let g = Graph::new();
    g.add_node("A");  // Duplicated setup
}
```

**Score 1:**
```rust
// Same setup copy-pasted across 20 tests
```

---

## Scoring Matrix

| Test Name | Obs. | Isol. | Clarity | Edge | Perf. | Determ. | Errors | Maint. | **Total** |
|-----------|------|-------|---------|------|-------|---------|--------|--------|-----------|
| (example) | 4 | 5 | 5 | 3 | 5 | 5 | 4 | 5 | **36/40** |

---

## Related Resources

- [How-to: Run Gemba Walk](../how-to/run-gemba-walk.md) - Application guide
- [Tutorial: Lean Manufacturing Intro](../tutorials/04-lean-manufacturing-intro.md) - Gemba philosophy

---

**Last Updated:** 2025-11-18
