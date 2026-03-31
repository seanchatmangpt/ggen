//! Cycle detection for RDF ontology imports
//!
//! This module provides DFS-based cycle detection for ontology dependency graphs.
//! It ensures that ontology imports form a DAG (Directed Acyclic Graph).
//!
//! # Algorithm
//!
//! Uses depth-first search (DFS) with three-color marking:
//! - White (unvisited): Node hasn't been explored
//! - Gray (visiting): Node is currently being explored (in recursion stack)
//! - Black (visited): Node and all descendants have been fully explored
//!
//! A cycle is detected when we encounter a gray node during DFS.
//!
//! # Complexity
//!
//! - Time: O(V + E) where V = vertices (files), E = edges (imports)
//! - Space: O(V) for recursion stack and visited sets

use ggen_utils::error::{Error, Result};
use std::collections::{HashMap, HashSet};

/// Detect cycles in a directed graph represented as adjacency lists
///
/// # Arguments
///
/// * `graph` - HashMap where key = node name, value = list of dependencies
///
/// # Returns
///
/// * `Vec<Vec<String>>` - List of cycles found (each cycle is a vector of node names)
///
/// # Examples
///
/// ```ignore
/// use std::collections::HashMap;
/// use ggen_core::graph::cycle_detection::detect_cycles;
///
/// let mut graph = HashMap::new();
/// graph.insert("A".to_string(), vec!["B".to_string()]);
/// graph.insert("B".to_string(), vec!["C".to_string()]);
/// graph.insert("C".to_string(), vec!["A".to_string()]); // Cycle!
///
/// let cycles = detect_cycles(&graph);
/// assert_eq!(cycles.len(), 1);
/// assert!(cycles[0].contains(&"A".to_string()));
/// ```
pub fn detect_cycles(graph: &HashMap<String, Vec<String>>) -> Vec<Vec<String>> {
    let mut cycles = Vec::new();
    let mut visited = HashSet::new();
    let mut recursion_stack = HashSet::new();
    let mut path = Vec::new();

    for node in graph.keys() {
        if !visited.contains(node) {
            dfs(
                node,
                graph,
                &mut visited,
                &mut recursion_stack,
                &mut path,
                &mut cycles,
            );
        }
    }

    cycles
}

/// Depth-first search with cycle detection
///
/// # Parameters
///
/// * `node` - Current node being visited
/// * `graph` - Adjacency list representation of the graph
/// * `visited` - Set of fully visited nodes
/// * `recursion_stack` - Set of nodes in current DFS path (gray nodes)
/// * `path` - Current path from root to current node
/// * `cycles` - Accumulator for detected cycles (output parameter)
fn dfs(
    node: &str, graph: &HashMap<String, Vec<String>>, visited: &mut HashSet<String>,
    recursion_stack: &mut HashSet<String>, path: &mut Vec<String>, cycles: &mut Vec<Vec<String>>,
) {
    visited.insert(node.to_string());
    recursion_stack.insert(node.to_string());
    path.push(node.to_string());

    // Visit all neighbors
    if let Some(neighbors) = graph.get(node) {
        for neighbor in neighbors {
            if !visited.contains(neighbor) {
                dfs(neighbor, graph, visited, recursion_stack, path, cycles);
            } else if recursion_stack.contains(neighbor) {
                // Found a cycle! Extract the cycle from the current path.
                let cycle_start = path
                    .iter()
                    .position(|n| n == neighbor)
                    .unwrap_or(path.len());

                let mut cycle = path[cycle_start..].to_vec();
                // Close the cycle by adding the starting node at the end
                cycle.push(neighbor.clone());
                cycles.push(cycle);
            }
        }
    }

    // Backtrack: remove node from recursion stack
    recursion_stack.remove(node);
    path.pop();
}

/// Validate that an ontology import graph is acyclic
///
/// # Arguments
///
/// * `imports` - HashMap where key = file path, value = list of files it imports
///
/// # Returns
///
/// * `Result<()>` - Ok if acyclic, Err with cycle details if cyclic
///
/// # Examples
///
/// ```ignore
/// use ggen_core::graph::cycle_detection::validate_acyclic;
///
/// let mut imports = std::collections::HashMap::new();
/// imports.insert("ontology.ttl".to_string(), vec!["common.ttl".to_string()]);
/// imports.insert("common.ttl".to_string(), vec![]);
///
/// assert!(validate_acyclic(&imports).is_ok());
/// ```
pub fn validate_acyclic(imports: &HashMap<String, Vec<String>>) -> Result<()> {
    let cycles = detect_cycles(imports);

    if cycles.is_empty() {
        Ok(())
    } else {
        let cycle_strings: Vec<String> = cycles
            .iter()
            .map(|cycle| {
                let cycle_str = cycle.join(" → ");
                format!("  {}", cycle_str)
            })
            .collect();

        Err(Error::new(&format!(
            "Cyclic ontology dependencies detected:\n{}\n\nFix: Remove circular imports from ontology files",
            cycle_strings.join("\n")
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_no_cycles() {
        let mut graph = HashMap::new();
        graph.insert("A".to_string(), vec!["B".to_string(), "C".to_string()]);
        graph.insert("B".to_string(), vec!["D".to_string()]);
        graph.insert("C".to_string(), vec!["D".to_string()]);
        graph.insert("D".to_string(), vec![]);

        let cycles = detect_cycles(&graph);
        assert_eq!(cycles.len(), 0);
    }

    #[test]
    fn test_simple_cycle() {
        let mut graph = HashMap::new();
        graph.insert("A".to_string(), vec!["B".to_string()]);
        graph.insert("B".to_string(), vec!["C".to_string()]);
        graph.insert("C".to_string(), vec!["A".to_string()]);

        let cycles = detect_cycles(&graph);
        assert_eq!(cycles.len(), 1);
        assert_eq!(cycles[0].len(), 4); // A → B → C → A
    }

    #[test]
    fn test_self_loop() {
        let mut graph = HashMap::new();
        graph.insert("A".to_string(), vec!["A".to_string()]);

        let cycles = detect_cycles(&graph);
        assert_eq!(cycles.len(), 1);
    }

    #[test]
    fn test_multiple_cycles() {
        let mut graph = HashMap::new();
        graph.insert("A".to_string(), vec!["B".to_string()]);
        graph.insert("B".to_string(), vec!["C".to_string()]);
        graph.insert("C".to_string(), vec!["A".to_string()]);
        graph.insert("D".to_string(), vec!["E".to_string()]);
        graph.insert("E".to_string(), vec!["D".to_string()]);

        let cycles = detect_cycles(&graph);
        assert_eq!(cycles.len(), 2);
    }

    #[test]
    fn test_disconnected_components() {
        let mut graph = HashMap::new();
        graph.insert("A".to_string(), vec!["B".to_string()]);
        graph.insert("B".to_string(), vec![]);
        graph.insert("C".to_string(), vec!["D".to_string()]);
        graph.insert("D".to_string(), vec![]);

        let cycles = detect_cycles(&graph);
        assert_eq!(cycles.len(), 0);
    }

    #[test]
    fn test_validate_acyclic_success() {
        let mut graph = HashMap::new();
        graph.insert("A".to_string(), vec!["B".to_string()]);
        graph.insert("B".to_string(), vec![]);

        assert!(validate_acyclic(&graph).is_ok());
    }

    #[test]
    fn test_validate_acyclic_failure() {
        let mut graph = HashMap::new();
        graph.insert("A".to_string(), vec!["B".to_string()]);
        graph.insert("B".to_string(), vec!["A".to_string()]);

        let result = validate_acyclic(&graph);
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(error_msg.contains("Cyclic ontology dependencies detected"));
    }

    #[test]
    fn test_empty_graph() {
        let graph = HashMap::new();
        let cycles = detect_cycles(&graph);
        assert_eq!(cycles.len(), 0);
    }

    #[test]
    fn test_complex_dag() {
        // Diamond graph: A -> B, A -> C, B -> D, C -> D
        let mut graph = HashMap::new();
        graph.insert("A".to_string(), vec!["B".to_string(), "C".to_string()]);
        graph.insert("B".to_string(), vec!["D".to_string()]);
        graph.insert("C".to_string(), vec!["D".to_string()]);
        graph.insert("D".to_string(), vec![]);

        let cycles = detect_cycles(&graph);
        assert_eq!(cycles.len(), 0);
    }
}
