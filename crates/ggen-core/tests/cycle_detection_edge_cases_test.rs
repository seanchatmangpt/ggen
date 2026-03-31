//! Edge case tests for cycle detection in directed graphs
//!
//! Tests boundary conditions, malformed input, and edge cases:
//! - Single node graphs
//! - Large graphs (stress testing)
//! - Graphs with isolated nodes
//! - Graphs with duplicate edges
//! - Empty graphs
//! - Self-loops
//! - Complex multi-cycle graphs
//! - Graphs with disconnected components

use ggen_core::graph::cycle_detection::{detect_cycles, validate_acyclic};
use std::collections::HashMap;

// ---------------------------------------------------------------------------
// Test 1: Single Node (No Edges)
// ---------------------------------------------------------------------------

#[test]
fn test_single_node_no_edges() {
    let mut graph = HashMap::new();
    graph.insert("A".to_string(), vec![]);

    let cycles = detect_cycles(&graph);

    assert_eq!(
        cycles.len(),
        0,
        "Single node with no edges should have no cycles"
    );
}

// ---------------------------------------------------------------------------
// Test 2: Single Node with Self-Loop
// ---------------------------------------------------------------------------

#[test]
fn test_single_node_self_loop() {
    let mut graph = HashMap::new();
    graph.insert("A".to_string(), vec!["A".to_string()]);

    let cycles = detect_cycles(&graph);

    assert_eq!(
        cycles.len(),
        1,
        "Single node with self-loop should have one cycle"
    );
    assert_eq!(cycles[0].len(), 2, "Cycle should be A -> A");
    assert_eq!(cycles[0][0], "A");
    assert_eq!(cycles[0][1], "A");
}

// ---------------------------------------------------------------------------
// Test 3: Empty Graph
// ---------------------------------------------------------------------------

#[test]
fn test_empty_graph() {
    let graph = HashMap::new();

    let cycles = detect_cycles(&graph);

    assert_eq!(cycles.len(), 0, "Empty graph should have no cycles");
}

// ---------------------------------------------------------------------------
// Test 4: Large Graph (Stress Test)
// ---------------------------------------------------------------------------

#[test]
fn test_large_graph_acyclic() {
    let mut graph = HashMap::new();

    // Create a chain of 100 nodes: 0 -> 1 -> 2 -> ... -> 99
    for i in 0..100 {
        let node = i.to_string();
        if i < 99 {
            graph.insert(node, vec![(i + 1).to_string()]);
        } else {
            graph.insert(node, vec![]);
        }
    }

    let cycles = detect_cycles(&graph);

    assert_eq!(cycles.len(), 0, "Large acyclic graph should have no cycles");
}

// ---------------------------------------------------------------------------
// Test 5: Large Graph with One Cycle
// ---------------------------------------------------------------------------

#[test]
fn test_large_graph_with_cycle() {
    let mut graph = HashMap::new();

    // Create a chain of 50 nodes
    for i in 0..50 {
        let node = i.to_string();
        if i < 49 {
            graph.insert(node, vec![(i + 1).to_string()]);
        } else {
            // Node 49 points back to node 25, creating a cycle
            graph.insert(node, vec!["25".to_string()]);
        }
    }

    let cycles = detect_cycles(&graph);

    assert_eq!(cycles.len(), 1, "Should detect one cycle");
    assert!(cycles[0].len() >= 2, "Cycle should have at least 2 nodes");
}

// ---------------------------------------------------------------------------
// Test 6: Graph with Isolated Nodes
// ---------------------------------------------------------------------------

#[test]
fn test_graph_with_isolated_nodes() {
    let mut graph = HashMap::new();

    // Main chain
    graph.insert("A".to_string(), vec!["B".to_string()]);
    graph.insert("B".to_string(), vec![]);

    // Isolated nodes (no edges)
    graph.insert("isolated1".to_string(), vec![]);
    graph.insert("isolated2".to_string(), vec![]);

    let cycles = detect_cycles(&graph);

    assert_eq!(
        cycles.len(),
        0,
        "Graph with isolated nodes should be acyclic"
    );
}

// ---------------------------------------------------------------------------
// Test 7: Graph with Duplicate Edges
// ---------------------------------------------------------------------------

#[test]
fn test_graph_with_duplicate_edges() {
    let mut graph = HashMap::new();

    // A -> B (duplicate edge)
    graph.insert("A".to_string(), vec!["B".to_string(), "B".to_string()]);
    graph.insert("B".to_string(), vec![]);

    let cycles = detect_cycles(&graph);

    assert_eq!(cycles.len(), 0, "Duplicate edges should not create cycles");
}

// ---------------------------------------------------------------------------
// Test 8: Multiple Independent Cycles
// ---------------------------------------------------------------------------

#[test]
fn test_multiple_independent_cycles() {
    let mut graph = HashMap::new();

    // Cycle 1: A -> B -> C -> A
    graph.insert("A".to_string(), vec!["B".to_string()]);
    graph.insert("B".to_string(), vec!["C".to_string()]);
    graph.insert("C".to_string(), vec!["A".to_string()]);

    // Cycle 2: X -> Y -> Z -> X
    graph.insert("X".to_string(), vec!["Y".to_string()]);
    graph.insert("Y".to_string(), vec!["Z".to_string()]);
    graph.insert("Z".to_string(), vec!["X".to_string()]);

    let cycles = detect_cycles(&graph);

    assert_eq!(cycles.len(), 2, "Should detect both independent cycles");

    // Verify we have both cycles
    let has_abc_cycle = cycles.iter().any(|cycle| {
        cycle.contains(&"A".to_string())
            && cycle.contains(&"B".to_string())
            && cycle.contains(&"C".to_string())
    });

    let has_xyz_cycle = cycles.iter().any(|cycle| {
        cycle.contains(&"X".to_string())
            && cycle.contains(&"Y".to_string())
            && cycle.contains(&"Z".to_string())
    });

    assert!(has_abc_cycle, "Should detect A-B-C cycle");
    assert!(has_xyz_cycle, "Should detect X-Y-Z cycle");
}

// ---------------------------------------------------------------------------
// Test 9: Nested Cycles
// ---------------------------------------------------------------------------

#[test]
fn test_nested_cycles() {
    let mut graph = HashMap::new();

    // Inner cycle: B -> C -> D -> B
    graph.insert("B".to_string(), vec!["C".to_string()]);
    graph.insert("C".to_string(), vec!["D".to_string()]);
    graph.insert("D".to_string(), vec!["B".to_string()]);

    // Outer cycle: A -> B -> E -> A
    graph.insert("A".to_string(), vec!["B".to_string()]);
    graph.insert("E".to_string(), vec!["A".to_string()]);

    let cycles = detect_cycles(&graph);

    // Should detect at least the inner cycle
    assert!(cycles.len() >= 1, "Should detect at least one cycle");

    // Verify inner cycle is found
    let has_inner_cycle = cycles.iter().any(|cycle| {
        cycle.contains(&"B".to_string())
            && cycle.contains(&"C".to_string())
            && cycle.contains(&"D".to_string())
    });

    assert!(has_inner_cycle, "Should detect B-C-D inner cycle");
}

// ---------------------------------------------------------------------------
// Test 10: Diamond Graph (No Cycle)
// ---------------------------------------------------------------------------

#[test]
fn test_diamond_graph_acyclic() {
    let mut graph = HashMap::new();

    // Diamond: A -> B, A -> C, B -> D, C -> D
    graph.insert("A".to_string(), vec!["B".to_string(), "C".to_string()]);
    graph.insert("B".to_string(), vec!["D".to_string()]);
    graph.insert("C".to_string(), vec!["D".to_string()]);
    graph.insert("D".to_string(), vec![]);

    let cycles = detect_cycles(&graph);

    assert_eq!(cycles.len(), 0, "Diamond graph should be acyclic");
}

// ---------------------------------------------------------------------------
// Test 11: Star Graph (No Cycle)
// ---------------------------------------------------------------------------

#[test]
fn test_star_graph_acyclic() {
    let mut graph = HashMap::new();

    // Star: Center -> Leaf1, Center -> Leaf2, Center -> Leaf3
    graph.insert(
        "Center".to_string(),
        vec![
            "Leaf1".to_string(),
            "Leaf2".to_string(),
            "Leaf3".to_string(),
        ],
    );
    graph.insert("Leaf1".to_string(), vec![]);
    graph.insert("Leaf2".to_string(), vec![]);
    graph.insert("Leaf3".to_string(), vec![]);

    let cycles = detect_cycles(&graph);

    assert_eq!(cycles.len(), 0, "Star graph should be acyclic");
}

// ---------------------------------------------------------------------------
// Test 12: Complete Graph (K3) - All Cycles
// ---------------------------------------------------------------------------

#[test]
fn test_complete_graph_k3() {
    let mut graph = HashMap::new();

    // Complete graph K3: A <-> B <-> C <-> A
    graph.insert("A".to_string(), vec!["B".to_string(), "C".to_string()]);
    graph.insert("B".to_string(), vec!["A".to_string(), "C".to_string()]);
    graph.insert("C".to_string(), vec!["A".to_string(), "B".to_string()]);

    let cycles = detect_cycles(&graph);

    // Should detect multiple cycles (A->B->A, A->C->A, B->C->B, A->B->C->A, etc.)
    assert!(cycles.len() >= 3, "Should detect at least 3 cycles in K3");
}

// ---------------------------------------------------------------------------
// Test 13: Graph with Missing Node Reference
// ---------------------------------------------------------------------------

#[test]
fn test_graph_with_missing_node_reference() {
    let mut graph = HashMap::new();

    // A references X, but X is not a key in the graph
    graph.insert("A".to_string(), vec!["X".to_string()]);
    graph.insert("B".to_string(), vec![]);

    // This should not crash - missing nodes are treated as leaf nodes
    let cycles = detect_cycles(&graph);

    assert_eq!(
        cycles.len(),
        0,
        "Missing node reference should not create cycle"
    );
}

// ---------------------------------------------------------------------------
// Test 14: Two-Node Cycle
// ---------------------------------------------------------------------------

#[test]
fn test_two_node_cycle() {
    let mut graph = HashMap::new();

    // A <-> B
    graph.insert("A".to_string(), vec!["B".to_string()]);
    graph.insert("B".to_string(), vec!["A".to_string()]);

    let cycles = detect_cycles(&graph);

    assert_eq!(cycles.len(), 1, "Should detect two-node cycle");
    assert_eq!(cycles[0].len(), 3, "Cycle should be A -> B -> A");
}

// ---------------------------------------------------------------------------
// Test 15: Long Chain Ending in Self-Loop
// ---------------------------------------------------------------------------

#[test]
fn test_long_chain_ending_in_self_loop() {
    let mut graph = HashMap::new();

    // A -> B -> C -> D -> D (self-loop at end)
    graph.insert("A".to_string(), vec!["B".to_string()]);
    graph.insert("B".to_string(), vec!["C".to_string()]);
    graph.insert("C".to_string(), vec!["D".to_string()]);
    graph.insert("D".to_string(), vec!["D".to_string()]);

    let cycles = detect_cycles(&graph);

    assert_eq!(cycles.len(), 1, "Should detect self-loop at end of chain");
    assert_eq!(cycles[0].len(), 2, "Self-loop cycle should be D -> D");
}

// ---------------------------------------------------------------------------
// Test 16: Disconnected Graphs with Mixed Cycles
// ---------------------------------------------------------------------------

#[test]
fn test_disconnected_graphs_mixed_cycles() {
    let mut graph = HashMap::new();

    // Component 1: A -> B -> C -> A (cycle)
    graph.insert("A".to_string(), vec!["B".to_string()]);
    graph.insert("B".to_string(), vec!["C".to_string()]);
    graph.insert("C".to_string(), vec!["A".to_string()]);

    // Component 2: X -> Y -> Z (acyclic)
    graph.insert("X".to_string(), vec!["Y".to_string()]);
    graph.insert("Y".to_string(), vec!["Z".to_string()]);
    graph.insert("Z".to_string(), vec![]);

    // Component 3: P -> Q -> P (cycle)
    graph.insert("P".to_string(), vec!["Q".to_string()]);
    graph.insert("Q".to_string(), vec!["P".to_string()]);

    let cycles = detect_cycles(&graph);

    assert_eq!(
        cycles.len(),
        2,
        "Should detect 2 cycles (one in each cyclic component)"
    );

    // Verify we have both cycles
    let has_abc_cycle = cycles.iter().any(|cycle| {
        cycle.contains(&"A".to_string())
            && cycle.contains(&"B".to_string())
            && cycle.contains(&"C".to_string())
    });

    let has_pq_cycle = cycles
        .iter()
        .any(|cycle| cycle.contains(&"P".to_string()) && cycle.contains(&"Q".to_string()));

    assert!(has_abc_cycle, "Should detect A-B-C cycle");
    assert!(has_pq_cycle, "Should detect P-Q cycle");
}

// ---------------------------------------------------------------------------
// Test 17: Validate Acyclic - Success Case
// ---------------------------------------------------------------------------

#[test]
fn test_validate_acyclic_success() {
    let mut graph = HashMap::new();

    graph.insert("A".to_string(), vec!["B".to_string()]);
    graph.insert("B".to_string(), vec!["C".to_string()]);
    graph.insert("C".to_string(), vec![]);

    let result = validate_acyclic(&graph);

    assert!(result.is_ok(), "Acyclic graph should pass validation");
}

// ---------------------------------------------------------------------------
// Test 18: Validate Acyclic - Failure Case
// ---------------------------------------------------------------------------

#[test]
fn test_validate_acyclic_failure() {
    let mut graph = HashMap::new();

    graph.insert("A".to_string(), vec!["B".to_string()]);
    graph.insert("B".to_string(), vec!["A".to_string()]);

    let result = validate_acyclic(&graph);

    assert!(result.is_err(), "Cyclic graph should fail validation");

    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("Cyclic"),
        "Error should mention cyclic dependencies: {}",
        error_msg
    );
    assert!(
        error_msg.contains("A") && error_msg.contains("B"),
        "Error should list nodes in cycle: {}",
        error_msg
    );
}

// ---------------------------------------------------------------------------
// Test 19: Cycle Detection is Deterministic
// ---------------------------------------------------------------------------

#[test]
fn test_cycle_detection_deterministic() {
    let mut graph = HashMap::new();

    // Create a consistent graph structure
    graph.insert("A".to_string(), vec!["B".to_string()]);
    graph.insert("B".to_string(), vec!["C".to_string()]);
    graph.insert("C".to_string(), vec!["A".to_string()]);

    // Run detection multiple times
    let cycles1 = detect_cycles(&graph);
    let cycles2 = detect_cycles(&graph);
    let cycles3 = detect_cycles(&graph);

    // Should produce identical results
    assert_eq!(cycles1.len(), cycles2.len());
    assert_eq!(cycles2.len(), cycles3.len());

    for (cycle1, cycle2) in cycles1.iter().zip(cycles2.iter()) {
        assert_eq!(cycle1, cycle2, "Cycles should be identical");
    }
}

// ---------------------------------------------------------------------------
// Test 20: Graph with Multiple Paths to Same Node
// ---------------------------------------------------------------------------

#[test]
fn test_graph_with_multiple_paths_to_same_node() {
    let mut graph = HashMap::new();

    // A -> B, A -> C, B -> D, C -> D (multiple paths to D)
    graph.insert("A".to_string(), vec!["B".to_string(), "C".to_string()]);
    graph.insert("B".to_string(), vec!["D".to_string()]);
    graph.insert("C".to_string(), vec!["D".to_string()]);
    graph.insert("D".to_string(), vec![]);

    let cycles = detect_cycles(&graph);

    assert_eq!(
        cycles.len(),
        0,
        "Multiple paths to same node should not create cycle"
    );
}

// ---------------------------------------------------------------------------
// Test 21: Cycle Detection with Unicode Node Names
// ---------------------------------------------------------------------------

#[test]
fn test_cycle_detection_unicode_node_names() {
    let mut graph = HashMap::new();

    // Use Unicode node names
    graph.insert("节点A".to_string(), vec!["节点B".to_string()]);
    graph.insert("节点B".to_string(), vec!["节点A".to_string()]);

    let cycles = detect_cycles(&graph);

    assert_eq!(cycles.len(), 1, "Should detect cycle with Unicode names");

    // Verify the cycle contains the Unicode names
    assert!(cycles[0].contains(&"节点A".to_string()));
    assert!(cycles[0].contains(&"节点B".to_string()));
}

// ---------------------------------------------------------------------------
// Test 22: Cycle Detection with Special Characters in Names
// ---------------------------------------------------------------------------

#[test]
fn test_cycle_detection_special_characters() {
    let mut graph = HashMap::new();

    // Use special characters in node names
    graph.insert("node-1".to_string(), vec!["node_2".to_string()]);
    graph.insert("node_2".to_string(), vec!["node.3".to_string()]);
    graph.insert("node.3".to_string(), vec!["node:1".to_string()]);

    let cycles = detect_cycles(&graph);

    assert_eq!(
        cycles.len(),
        0,
        "Special characters should not affect cycle detection"
    );
}

// ---------------------------------------------------------------------------
// Test 23: Very Long Node Names
// ---------------------------------------------------------------------------

#[test]
fn test_very_long_node_names() {
    let mut graph = HashMap::new();

    let long_name1 = "a".repeat(1000);
    let long_name2 = "b".repeat(1000);

    graph.insert(long_name1.clone(), vec![long_name2.clone()]);
    graph.insert(long_name2.clone(), vec![long_name1.clone()]);

    let cycles = detect_cycles(&graph);

    assert_eq!(
        cycles.len(),
        1,
        "Should detect cycle with very long node names"
    );
}

// ---------------------------------------------------------------------------
// Test 24: Graph with Node that References Many Others
// ---------------------------------------------------------------------------

#[test]
fn test_node_with_many_outgoing_edges() {
    let mut graph = HashMap::new();

    // A references 100 other nodes
    let many_nodes: Vec<String> = (0..100).map(|i| format!("node_{}", i)).collect();

    graph.insert("A".to_string(), many_nodes.clone());

    // All referenced nodes are leaves
    for node in &many_nodes {
        graph.insert(node.clone(), vec![]);
    }

    let cycles = detect_cycles(&graph);

    assert_eq!(
        cycles.len(),
        0,
        "Node with many outgoing edges should be acyclic"
    );
}

// ---------------------------------------------------------------------------
// Test 25: Graph with Node Referenced by Many Others
// ---------------------------------------------------------------------------

#[test]
fn test_node_with_many_incoming_edges() {
    let mut graph = HashMap::new();

    // 100 nodes all point to D
    for i in 0..100 {
        let node = format!("node_{}", i);
        graph.insert(node, vec!["D".to_string()]);
    }

    graph.insert("D".to_string(), vec![]);

    let cycles = detect_cycles(&graph);

    assert_eq!(
        cycles.len(),
        0,
        "Node with many incoming edges should be acyclic"
    );
}

// ---------------------------------------------------------------------------
// Test 26: Cycle Detection Order Independence
// ---------------------------------------------------------------------------

#[test]
fn test_cycle_detection_order_independence() {
    // Create same graph but with different insertion orders
    let mut graph1 = HashMap::new();
    graph1.insert("A".to_string(), vec!["B".to_string()]);
    graph1.insert("B".to_string(), vec!["C".to_string()]);
    graph1.insert("C".to_string(), vec![]);

    let mut graph2 = HashMap::new();
    graph2.insert("C".to_string(), vec![]);
    graph2.insert("B".to_string(), vec!["C".to_string()]);
    graph2.insert("A".to_string(), vec!["B".to_string()]);

    let cycles1 = detect_cycles(&graph1);
    let cycles2 = detect_cycles(&graph2);

    assert_eq!(
        cycles1.len(),
        cycles2.len(),
        "Should detect same cycles regardless of insertion order"
    );
}

// ---------------------------------------------------------------------------
// Test 27: Validate Acyclic Error Message Quality
// ---------------------------------------------------------------------------

#[test]
fn test_validate_acyclic_error_message_quality() {
    let mut graph = HashMap::new();

    graph.insert("file1.ttl".to_string(), vec!["file2.ttl".to_string()]);
    graph.insert("file2.ttl".to_string(), vec!["file3.ttl".to_string()]);
    graph.insert("file3.ttl".to_string(), vec!["file1.ttl".to_string()]);

    let result = validate_acyclic(&graph);

    assert!(result.is_err());

    let error_msg = result.unwrap_err().to_string();

    // Verify error message contains helpful information
    assert!(error_msg.contains("Cyclic"), "Should mention cyclic");
    assert!(error_msg.contains("file1.ttl"), "Should mention file1");
    assert!(error_msg.contains("file2.ttl"), "Should mention file2");
    assert!(error_msg.contains("file3.ttl"), "Should mention file3");
    assert!(
        error_msg.contains("→") || error_msg.contains("->"),
        "Should show cycle path"
    );
}

// ---------------------------------------------------------------------------
// Test 28: Empty Dependency List
// ---------------------------------------------------------------------------

#[test]
fn test_empty_dependency_list() {
    let mut graph = HashMap::new();

    // Node with empty dependency list
    graph.insert("A".to_string(), vec![]);
    graph.insert("B".to_string(), vec![]);
    graph.insert("C".to_string(), vec![]);

    let cycles = detect_cycles(&graph);

    assert_eq!(
        cycles.len(),
        0,
        "Nodes with empty dependency lists should be acyclic"
    );
}

// ---------------------------------------------------------------------------
// Test 29: Cycle Detection Performance on Large Acyclic Graph
// ---------------------------------------------------------------------------

#[test]
fn test_cycle_detection_performance_large_acyclic() {
    let mut graph = HashMap::new();

    // Create a large tree structure (1000 nodes)
    for i in 0..1000 {
        let node = format!("node_{}", i);
        let parent = format!("node_{}", i / 2); // Binary tree structure

        if i == 0 {
            graph.insert(node, vec![]);
        } else {
            graph.insert(node, vec![parent]);
        }
    }

    let start = std::time::Instant::now();
    let cycles = detect_cycles(&graph);
    let duration = start.elapsed();

    assert_eq!(cycles.len(), 0, "Tree should be acyclic");
    assert!(
        duration.as_millis() < 1000,
        "Cycle detection should complete in under 1 second, took {} ms",
        duration.as_millis()
    );
}

// ---------------------------------------------------------------------------
// Test 30: Minimal Two-Node Graph (Both Directions)
// ---------------------------------------------------------------------------

#[test]
fn test_minimal_two_node_both_directions() {
    let mut graph = HashMap::new();

    // A -> B and B -> A
    graph.insert("A".to_string(), vec!["B".to_string()]);
    graph.insert("B".to_string(), vec!["A".to_string()]);

    let cycles = detect_cycles(&graph);

    assert_eq!(cycles.len(), 1, "Should detect cycle");
    assert!(cycles[0].contains(&"A".to_string()));
    assert!(cycles[0].contains(&"B".to_string()));
}
