//! Unit tests for dependency ordering
//!
//! Tests cover:
//! - Topological sort
//! - Circular dependency detection
//! - Dependency resolution
//! - Installation order optimization

use std::collections::{HashMap, HashSet, VecDeque};

#[derive(Debug, Clone, PartialEq)]
pub enum DependencyError {
    CircularDependency(Vec<String>),
    MissingDependency(String),
    InvalidGraph,
}

impl std::fmt::Display for DependencyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CircularDependency(cycle) => {
                write!(f, "Circular dependency: {}", cycle.join(" -> "))
            }
            Self::MissingDependency(pkg) => write!(f, "Missing dependency: {}", pkg),
            Self::InvalidGraph => write!(f, "Invalid dependency graph"),
        }
    }
}

impl std::error::Error for DependencyError {}

pub struct DependencyResolver {
    graph: HashMap<String, Vec<String>>,
}

impl DependencyResolver {
    pub fn new() -> Self {
        Self {
            graph: HashMap::new(),
        }
    }

    pub fn add_package(&mut self, package: String, dependencies: Vec<String>) {
        self.graph.insert(package, dependencies);
    }

    /// Topological sort using Kahn's algorithm
    pub fn resolve_order(&self) -> Result<Vec<String>, DependencyError> {
        let mut in_degree: HashMap<String, usize> = HashMap::new();
        let mut graph_clone = self.graph.clone();

        // Initialize in-degree for all nodes
        for pkg in self.graph.keys() {
            in_degree.entry(pkg.clone()).or_insert(0);
        }

        // Calculate in-degrees
        for deps in self.graph.values() {
            for dep in deps {
                *in_degree.entry(dep.clone()).or_insert(0) += 1;

                // Ensure dependency exists in graph
                if !self.graph.contains_key(dep) {
                    graph_clone.insert(dep.clone(), Vec::new());
                }
            }
        }

        // Find nodes with no incoming edges
        let mut queue: VecDeque<String> = in_degree
            .iter()
            .filter(|(_, &degree)| degree == 0)
            .map(|(pkg, _)| pkg.clone())
            .collect();

        let mut result = Vec::new();

        while let Some(pkg) = queue.pop_front() {
            result.push(pkg.clone());

            if let Some(deps) = graph_clone.get(&pkg) {
                for dep in deps {
                    if let Some(degree) = in_degree.get_mut(dep) {
                        *degree -= 1;
                        if *degree == 0 {
                            queue.push_back(dep.clone());
                        }
                    }
                }
            }
        }

        // Check for cycles
        if result.len() != in_degree.len() {
            let remaining: Vec<String> = in_degree
                .iter()
                .filter(|(_, &deg)| deg > 0)
                .map(|(pkg, _)| pkg.clone())
                .collect();
            return Err(DependencyError::CircularDependency(remaining));
        }

        // Reverse to get installation order (dependencies first)
        result.reverse();
        Ok(result)
    }

    /// Detect circular dependencies using DFS
    pub fn detect_cycles(&self) -> Option<Vec<String>> {
        let mut visited = HashSet::new();
        let mut rec_stack = HashSet::new();
        let mut path = Vec::new();

        for node in self.graph.keys() {
            if !visited.contains(node) {
                if let Some(cycle) =
                    self.dfs_cycle_detect(node, &mut visited, &mut rec_stack, &mut path)
                {
                    return Some(cycle);
                }
            }
        }

        None
    }

    fn dfs_cycle_detect(
        &self, node: &str, visited: &mut HashSet<String>, rec_stack: &mut HashSet<String>,
        path: &mut Vec<String>,
    ) -> Option<Vec<String>> {
        visited.insert(node.to_string());
        rec_stack.insert(node.to_string());
        path.push(node.to_string());

        if let Some(deps) = self.graph.get(node) {
            for dep in deps {
                if !visited.contains(dep) {
                    if let Some(cycle) = self.dfs_cycle_detect(dep, visited, rec_stack, path) {
                        return Some(cycle);
                    }
                } else if rec_stack.contains(dep) {
                    // Found cycle
                    let cycle_start = path.iter().position(|p| p == dep).unwrap();
                    return Some(path[cycle_start..].to_vec());
                }
            }
        }

        rec_stack.remove(node);
        path.pop();
        None
    }
}

// ============================================================================
// UNIT TESTS - Basic Dependency Resolution
// ============================================================================

#[test]
fn test_simple_dependency_chain() {
    let mut resolver = DependencyResolver::new();

    // C depends on nothing
    // B depends on C
    // A depends on B
    resolver.add_package("A".to_string(), vec!["B".to_string()]);
    resolver.add_package("B".to_string(), vec!["C".to_string()]);
    resolver.add_package("C".to_string(), vec![]);

    let order = resolver.resolve_order().unwrap();

    // Should install C, then B, then A
    assert_eq!(order, vec!["C", "B", "A"]);
}

#[test]
fn test_no_dependencies() {
    let mut resolver = DependencyResolver::new();

    resolver.add_package("A".to_string(), vec![]);
    resolver.add_package("B".to_string(), vec![]);
    resolver.add_package("C".to_string(), vec![]);

    let order = resolver.resolve_order().unwrap();

    // All packages can be installed in any order
    assert_eq!(order.len(), 3);
    assert!(order.contains(&"A".to_string()));
    assert!(order.contains(&"B".to_string()));
    assert!(order.contains(&"C".to_string()));
}

#[test]
fn test_diamond_dependency() {
    let mut resolver = DependencyResolver::new();

    // Diamond pattern:
    //     D
    //    / \
    //   B   C
    //    \ /
    //     A
    resolver.add_package("D".to_string(), vec!["B".to_string(), "C".to_string()]);
    resolver.add_package("B".to_string(), vec!["A".to_string()]);
    resolver.add_package("C".to_string(), vec!["A".to_string()]);
    resolver.add_package("A".to_string(), vec![]);

    let order = resolver.resolve_order().unwrap();

    // A must come before B and C
    // B and C must come before D
    let a_pos = order.iter().position(|p| p == "A").unwrap();
    let b_pos = order.iter().position(|p| p == "B").unwrap();
    let c_pos = order.iter().position(|p| p == "C").unwrap();
    let d_pos = order.iter().position(|p| p == "D").unwrap();

    assert!(a_pos < b_pos);
    assert!(a_pos < c_pos);
    assert!(b_pos < d_pos);
    assert!(c_pos < d_pos);
}

// ============================================================================
// UNIT TESTS - Circular Dependency Detection
// ============================================================================

#[test]
fn test_simple_cycle_detection() {
    let mut resolver = DependencyResolver::new();

    // A -> B -> C -> A (cycle)
    resolver.add_package("A".to_string(), vec!["B".to_string()]);
    resolver.add_package("B".to_string(), vec!["C".to_string()]);
    resolver.add_package("C".to_string(), vec!["A".to_string()]);

    let result = resolver.resolve_order();

    assert!(result.is_err());
    match result.unwrap_err() {
        DependencyError::CircularDependency(_) => (),
        _ => panic!("Expected CircularDependency error"),
    }
}

#[test]
fn test_self_dependency() {
    let mut resolver = DependencyResolver::new();

    // A depends on itself
    resolver.add_package("A".to_string(), vec!["A".to_string()]);

    let cycle = resolver.detect_cycles();
    assert!(cycle.is_some());
}

#[test]
fn test_complex_cycle_detection() {
    let mut resolver = DependencyResolver::new();

    // Complex graph with cycle: A -> B -> C -> D -> B
    resolver.add_package("A".to_string(), vec!["B".to_string()]);
    resolver.add_package("B".to_string(), vec!["C".to_string()]);
    resolver.add_package("C".to_string(), vec!["D".to_string()]);
    resolver.add_package("D".to_string(), vec!["B".to_string()]);

    let cycle = resolver.detect_cycles();
    assert!(cycle.is_some());

    let cycle_packages = cycle.unwrap();
    assert!(cycle_packages.contains(&"B".to_string()));
    assert!(cycle_packages.contains(&"C".to_string()));
    assert!(cycle_packages.contains(&"D".to_string()));
}

// ============================================================================
// INTEGRATION TESTS - Real-World Scenarios
// ============================================================================

#[test]
fn test_large_dependency_graph() {
    let mut resolver = DependencyResolver::new();

    // Simulate 20 packages with various dependencies
    for i in 0..20 {
        let pkg = format!("pkg{}", i);
        let mut deps = Vec::new();

        // Each package depends on 1-3 previous packages
        for j in 0..std::cmp::min(i, 3) {
            deps.push(format!("pkg{}", i - j - 1));
        }

        resolver.add_package(pkg, deps);
    }

    let order = resolver.resolve_order().unwrap();

    // All 20 packages should be in order
    assert_eq!(order.len(), 20);

    // Verify dependencies are satisfied (each package comes after its dependencies)
    for (i, pkg) in order.iter().enumerate() {
        if let Some(deps) = resolver.graph.get(pkg) {
            for dep in deps {
                let dep_pos = order.iter().position(|p| p == dep).unwrap();
                assert!(dep_pos < i, "{} should come before {}", dep, pkg);
            }
        }
    }
}

#[test]
fn test_multiple_root_packages() {
    let mut resolver = DependencyResolver::new();

    // Two separate dependency trees
    // Tree 1: A -> B -> C
    // Tree 2: X -> Y -> Z
    resolver.add_package("A".to_string(), vec!["B".to_string()]);
    resolver.add_package("B".to_string(), vec!["C".to_string()]);
    resolver.add_package("C".to_string(), vec![]);
    resolver.add_package("X".to_string(), vec!["Y".to_string()]);
    resolver.add_package("Y".to_string(), vec!["Z".to_string()]);
    resolver.add_package("Z".to_string(), vec![]);

    let order = resolver.resolve_order().unwrap();

    // Both trees should be resolved
    assert_eq!(order.len(), 6);

    // Verify each tree's internal ordering
    let c_pos = order.iter().position(|p| p == "C").unwrap();
    let b_pos = order.iter().position(|p| p == "B").unwrap();
    let a_pos = order.iter().position(|p| p == "A").unwrap();
    assert!(c_pos < b_pos && b_pos < a_pos);

    let z_pos = order.iter().position(|p| p == "Z").unwrap();
    let y_pos = order.iter().position(|p| p == "Y").unwrap();
    let x_pos = order.iter().position(|p| p == "X").unwrap();
    assert!(z_pos < y_pos && y_pos < x_pos);
}

// ============================================================================
// EDGE CASE TESTS
// ============================================================================

#[test]
fn test_empty_graph() {
    let resolver = DependencyResolver::new();

    let order = resolver.resolve_order().unwrap();

    assert_eq!(order.len(), 0);
}

#[test]
fn test_single_package() {
    let mut resolver = DependencyResolver::new();

    resolver.add_package("A".to_string(), vec![]);

    let order = resolver.resolve_order().unwrap();

    assert_eq!(order, vec!["A"]);
}

// ============================================================================
// FMEA MAPPING TESTS
// ============================================================================

#[test]
fn test_fmea_circular_dependency_detection() {
    // FMEA Failure Mode: Circular dependency causes infinite loop (RPN 64)
    // Mitigation: Detect cycles before installation

    let mut resolver = DependencyResolver::new();

    resolver.add_package("lodash".to_string(), vec!["underscore".to_string()]);
    resolver.add_package("underscore".to_string(), vec!["lodash".to_string()]);

    // Must detect cycle before attempting installation
    let cycle = resolver.detect_cycles();
    assert!(cycle.is_some());

    let result = resolver.resolve_order();
    assert!(result.is_err());
}

#[test]
fn test_fmea_missing_dependency_detection() {
    // FMEA Failure Mode: Missing dependency causes installation failure (RPN 48)
    // Mitigation: Resolve all dependencies before installation

    let mut resolver = DependencyResolver::new();

    // Package A depends on B, but B is not in repository
    resolver.add_package("A".to_string(), vec!["B".to_string()]);

    // Should still resolve (B will be added implicitly with no dependencies)
    let order = resolver.resolve_order().unwrap();

    // B should be installed before A
    assert!(
        order.iter().position(|p| p == "B").unwrap() < order.iter().position(|p| p == "A").unwrap()
    );
}

#[test]
fn test_fmea_installation_order_correctness() {
    // FMEA Failure Mode: Wrong installation order breaks dependencies (RPN 56)
    // Mitigation: Topological sort ensures correct order

    let mut resolver = DependencyResolver::new();

    // Database driver depends on database client
    // ORM depends on database driver
    // Application depends on ORM
    resolver.add_package("app".to_string(), vec!["orm".to_string()]);
    resolver.add_package("orm".to_string(), vec!["db-driver".to_string()]);
    resolver.add_package("db-driver".to_string(), vec!["db-client".to_string()]);
    resolver.add_package("db-client".to_string(), vec![]);

    let order = resolver.resolve_order().unwrap();

    // Verify correct installation order
    assert_eq!(order[0], "db-client");
    assert_eq!(order[1], "db-driver");
    assert_eq!(order[2], "orm");
    assert_eq!(order[3], "app");
}
