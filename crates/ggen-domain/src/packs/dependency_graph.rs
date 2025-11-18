//! Dependency graph for pack dependency resolution
//!
//! This module provides dependency resolution with:
//! - Circular dependency detection
//! - Topological sorting for install order
//! - Conflict detection

use crate::packs::types::Pack;
use ggen_utils::error::{Error, Result};
use std::collections::{HashMap, HashSet, VecDeque};

/// Dependency graph for resolving pack installation order
#[derive(Debug, Clone)]
pub struct DependencyGraph {
    /// Adjacency list: pack_id -> list of dependent pack_ids
    edges: HashMap<String, Vec<String>>,
    /// All pack IDs in the graph
    nodes: HashSet<String>,
}

impl DependencyGraph {
    /// Create empty dependency graph
    pub fn new() -> Self {
        Self {
            edges: HashMap::new(),
            nodes: HashSet::new(),
        }
    }

    /// Create dependency graph from packs
    pub fn from_packs(packs: &[Pack]) -> Result<Self> {
        let mut graph = Self::new();

        for pack in packs {
            graph.add_node(&pack.id);

            for dep in &pack.dependencies {
                if !dep.optional {
                    graph.add_edge(&pack.id, &dep.pack_id);
                }
            }
        }

        // Detect cycles
        graph.detect_cycles()?;

        Ok(graph)
    }

    /// Add a node to the graph
    pub fn add_node(&mut self, pack_id: &str) {
        self.nodes.insert(pack_id.to_string());
        self.edges
            .entry(pack_id.to_string())
            .or_insert_with(Vec::new);
    }

    /// Add an edge from pack to dependency
    pub fn add_edge(&mut self, from: &str, to: &str) {
        self.add_node(from);
        self.add_node(to);

        self.edges
            .entry(from.to_string())
            .or_insert_with(Vec::new)
            .push(to.to_string());
    }

    /// Detect circular dependencies using DFS
    pub fn detect_cycles(&self) -> Result<()> {
        let mut visited = HashSet::new();
        let mut rec_stack = HashSet::new();
        let mut path = Vec::new();

        for node in &self.nodes {
            if !visited.contains(node) {
                self.dfs_cycle_check(node, &mut visited, &mut rec_stack, &mut path)?;
            }
        }

        Ok(())
    }

    /// DFS helper for cycle detection
    fn dfs_cycle_check(
        &self, node: &str, visited: &mut HashSet<String>, rec_stack: &mut HashSet<String>,
        path: &mut Vec<String>,
    ) -> Result<()> {
        visited.insert(node.to_string());
        rec_stack.insert(node.to_string());
        path.push(node.to_string());

        if let Some(neighbors) = self.edges.get(node) {
            for neighbor in neighbors {
                if !visited.contains(neighbor) {
                    self.dfs_cycle_check(neighbor, visited, rec_stack, path)?;
                } else if rec_stack.contains(neighbor) {
                    // Found cycle - construct cycle path
                    let cycle_start = path.iter().position(|n| n == neighbor).unwrap();
                    let cycle_path: Vec<_> = path[cycle_start..]
                        .iter()
                        .chain(std::iter::once(neighbor))
                        .map(|s| s.as_str())
                        .collect();

                    return Err(Error::new(&format!(
                        "Circular dependency detected: {}",
                        cycle_path.join(" -> ")
                    )));
                }
            }
        }

        rec_stack.remove(node);
        path.pop();

        Ok(())
    }

    /// Get topological sort order for installation
    ///
    /// Returns pack IDs in order where dependencies come before dependents
    pub fn topological_sort(&self) -> Result<Vec<String>> {
        let mut in_degree: HashMap<String, usize> = HashMap::new();

        // Initialize in-degrees
        for node in &self.nodes {
            in_degree.insert(node.clone(), 0);
        }

        // Calculate in-degrees
        for neighbors in self.edges.values() {
            for neighbor in neighbors {
                *in_degree.get_mut(neighbor).unwrap() += 1;
            }
        }

        // Queue of nodes with in-degree 0
        let mut queue: VecDeque<String> = in_degree
            .iter()
            .filter(|(_, &deg)| deg == 0)
            .map(|(node, _)| node.clone())
            .collect();

        let mut result = Vec::new();

        while let Some(node) = queue.pop_front() {
            result.push(node.clone());

            if let Some(neighbors) = self.edges.get(&node) {
                for neighbor in neighbors {
                    let deg = in_degree.get_mut(neighbor).unwrap();
                    *deg -= 1;

                    if *deg == 0 {
                        queue.push_back(neighbor.clone());
                    }
                }
            }
        }

        // Check if all nodes are included (no cycles)
        if result.len() != self.nodes.len() {
            return Err(Error::new(
                "Cycle detected during topological sort (some nodes unreachable)",
            ));
        }

        // Reverse the result so dependencies come before dependents
        // (we want to install dependencies first)
        result.reverse();

        Ok(result)
    }

    /// Get dependencies of a pack
    pub fn dependencies(&self, pack_id: &str) -> Vec<String> {
        self.edges.get(pack_id).cloned().unwrap_or_else(Vec::new)
    }

    /// Get all transitive dependencies of a pack
    pub fn transitive_dependencies(&self, pack_id: &str) -> Result<HashSet<String>> {
        let mut result = HashSet::new();
        let mut queue = VecDeque::new();
        queue.push_back(pack_id.to_string());

        while let Some(node) = queue.pop_front() {
            if let Some(deps) = self.edges.get(&node) {
                for dep in deps {
                    if result.insert(dep.clone()) {
                        queue.push_back(dep.clone());
                    }
                }
            }
        }

        Ok(result)
    }

    /// Get the number of nodes in the graph
    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }

    /// Get the number of edges in the graph
    pub fn edge_count(&self) -> usize {
        self.edges.values().map(|v| v.len()).sum()
    }
}

impl Default for DependencyGraph {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::packs::types::{PackDependency, PackMetadata};
    use std::collections::HashMap;

    fn create_test_pack(id: &str, deps: Vec<&str>) -> Pack {
        Pack {
            id: id.to_string(),
            name: format!("Pack {}", id),
            version: "1.0.0".to_string(),
            description: format!("Test pack {}", id),
            category: "test".to_string(),
            author: None,
            repository: None,
            license: None,
            packages: vec![],
            templates: vec![],
            sparql_queries: HashMap::new(),
            dependencies: deps
                .into_iter()
                .map(|d| PackDependency {
                    pack_id: d.to_string(),
                    version: "1.0.0".to_string(),
                    optional: false,
                })
                .collect(),
            tags: vec![],
            keywords: vec![],
            production_ready: true,
            metadata: PackMetadata::default(),
        }
    }

    #[test]
    fn test_dependency_graph_from_packs() {
        let packs = vec![
            create_test_pack("A", vec!["B", "C"]),
            create_test_pack("B", vec!["C"]),
            create_test_pack("C", vec![]),
        ];

        let graph = DependencyGraph::from_packs(&packs).unwrap();

        assert_eq!(graph.node_count(), 3);
        assert_eq!(graph.edge_count(), 3);
    }

    #[test]
    fn test_topological_sort_simple() {
        let packs = vec![
            create_test_pack("A", vec!["B"]),
            create_test_pack("B", vec!["C"]),
            create_test_pack("C", vec![]),
        ];

        let graph = DependencyGraph::from_packs(&packs).unwrap();
        let sorted = graph.topological_sort().unwrap();

        // C should come before B, B should come before A
        let c_pos = sorted.iter().position(|x| x == "C").unwrap();
        let b_pos = sorted.iter().position(|x| x == "B").unwrap();
        let a_pos = sorted.iter().position(|x| x == "A").unwrap();

        assert!(c_pos < b_pos);
        assert!(b_pos < a_pos);
    }

    #[test]
    fn test_topological_sort_complex() {
        //   A -> B -> D
        //   |    |
        //   v    v
        //   C -> D
        let packs = vec![
            create_test_pack("A", vec!["B", "C"]),
            create_test_pack("B", vec!["D"]),
            create_test_pack("C", vec!["D"]),
            create_test_pack("D", vec![]),
        ];

        let graph = DependencyGraph::from_packs(&packs).unwrap();
        let sorted = graph.topological_sort().unwrap();

        // D should come before B and C
        // B and C should come before A
        let d_pos = sorted.iter().position(|x| x == "D").unwrap();
        let b_pos = sorted.iter().position(|x| x == "B").unwrap();
        let c_pos = sorted.iter().position(|x| x == "C").unwrap();
        let a_pos = sorted.iter().position(|x| x == "A").unwrap();

        assert!(d_pos < b_pos);
        assert!(d_pos < c_pos);
        assert!(b_pos < a_pos);
        assert!(c_pos < a_pos);
    }

    #[test]
    fn test_detect_circular_dependency() {
        // A -> B -> C -> A (cycle)
        let packs = vec![
            create_test_pack("A", vec!["B"]),
            create_test_pack("B", vec!["C"]),
            create_test_pack("C", vec!["A"]),
        ];

        let result = DependencyGraph::from_packs(&packs);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Circular dependency"));
    }

    #[test]
    fn test_detect_self_dependency() {
        // A -> A (self-cycle)
        let packs = vec![create_test_pack("A", vec!["A"])];

        let result = DependencyGraph::from_packs(&packs);
        assert!(result.is_err());
    }

    #[test]
    fn test_transitive_dependencies() {
        let packs = vec![
            create_test_pack("A", vec!["B"]),
            create_test_pack("B", vec!["C", "D"]),
            create_test_pack("C", vec![]),
            create_test_pack("D", vec!["E"]),
            create_test_pack("E", vec![]),
        ];

        let graph = DependencyGraph::from_packs(&packs).unwrap();
        let deps = graph.transitive_dependencies("A").unwrap();

        // A transitively depends on B, C, D, E
        assert_eq!(deps.len(), 4);
        assert!(deps.contains("B"));
        assert!(deps.contains("C"));
        assert!(deps.contains("D"));
        assert!(deps.contains("E"));
    }

    #[test]
    fn test_direct_dependencies() {
        let packs = vec![
            create_test_pack("A", vec!["B", "C"]),
            create_test_pack("B", vec![]),
            create_test_pack("C", vec![]),
        ];

        let graph = DependencyGraph::from_packs(&packs).unwrap();
        let deps = graph.dependencies("A");

        assert_eq!(deps.len(), 2);
        assert!(deps.contains(&"B".to_string()));
        assert!(deps.contains(&"C".to_string()));
    }

    #[test]
    fn test_independent_packs() {
        // Three independent packs with no dependencies
        let packs = vec![
            create_test_pack("A", vec![]),
            create_test_pack("B", vec![]),
            create_test_pack("C", vec![]),
        ];

        let graph = DependencyGraph::from_packs(&packs).unwrap();
        let sorted = graph.topological_sort().unwrap();

        // All three should be included
        assert_eq!(sorted.len(), 3);
    }
}
