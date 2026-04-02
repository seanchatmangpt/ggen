//! Standalone cycle detection test to verify functionality
//! This test can be run independently of the main test suite

use std::collections::{HashMap, HashSet};

#[derive(Debug)]
struct Error {
    message: String,
}

impl Error {
    fn new(msg: &str) -> Self {
        Error {
            message: msg.to_string(),
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for Error {}

type Result<T> = std::result::Result<T, Error>;

/// Detect cycles in a directed graph represented as adjacency lists
fn detect_cycles(graph: &HashMap<String, Vec<String>>) -> Vec<Vec<String>> {
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
fn dfs(
    node: &str,
    graph: &HashMap<String, Vec<String>>,
    visited: &mut HashSet<String>,
    recursion_stack: &mut HashSet<String>,
    path: &mut Vec<String>,
    cycles: &mut Vec<Vec<String>>,
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

                let cycle = path[cycle_start..].to_vec();
                cycles.push(cycle);
            }
        }
    }

    // Backtrack: remove node from recursion stack
    recursion_stack.remove(node);
    path.pop();
}

/// Validate that an ontology import graph is acyclic
fn validate_acyclic(imports: &HashMap<String, Vec<String>>) -> Result<()> {
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

fn test_basic_cycle_detection() {
    // Create a simple graph with a cycle: A -> B -> C -> A
    let mut graph = HashMap::new();
    graph.insert("A".to_string(), vec!["B".to_string()]);
    graph.insert("B".to_string(), vec!["C".to_string()]);
    graph.insert("C".to_string(), vec!["A".to_string()]);

    let cycles = detect_cycles(&graph);

    assert_eq!(cycles.len(), 1, "Should detect exactly one cycle");
    assert_eq!(cycles[0].len(), 3, "Cycle should have 3 nodes: A -> B -> C (returns to start)");
    assert!(cycles[0].contains(&"A".to_string()));
    assert!(cycles[0].contains(&"B".to_string()));
    assert!(cycles[0].contains(&"C".to_string()));
}

fn test_no_cycle_detection() {
    // Create an acyclic graph: A -> B -> C, D -> B
    let mut graph = HashMap::new();
    graph.insert("A".to_string(), vec!["B".to_string()]);
    graph.insert("B".to_string(), vec!["C".to_string()]);
    graph.insert("D".to_string(), vec!["B".to_string()]);
    graph.insert("C".to_string(), vec![]);

    let cycles = detect_cycles(&graph);

    assert_eq!(cycles.len(), 0, "Should detect no cycles in acyclic graph");
}

fn test_validate_acyclic() {
    // Test validation with acyclic graph
    let mut graph = HashMap::new();
    graph.insert("A".to_string(), vec!["B".to_string()]);
    graph.insert("B".to_string(), vec!["C".to_string()]);
    graph.insert("C".to_string(), vec![]);

    let result = validate_acyclic(&graph);
    assert!(result.is_ok(), "Acyclic graph should pass validation");
}

fn test_validate_cyclic() {
    // Test validation with cyclic graph
    let mut graph = HashMap::new();
    graph.insert("A".to_string(), vec!["B".to_string()]);
    graph.insert("B".to_string(), vec!["A".to_string()]);

    let result = validate_acyclic(&graph);
    assert!(result.is_err(), "Cyclic graph should fail validation");

    let error_msg = result.unwrap_err().to_string();
    assert!(error_msg.contains("Cyclic ontology dependencies"),
             "Error message should mention cyclic dependencies");
}

fn main() {
    println!("Running cycle detection tests...");

    // Test 1: Basic cycle detection
    test_basic_cycle_detection();
    println!("✅ Basic cycle detection test passed");

    // Test 2: No cycle detection
    test_no_cycle_detection();
    println!("✅ No cycle detection test passed");

    // Test 3: Validate acyclic
    test_validate_acyclic();
    println!("✅ Validate acyclic test passed");

    // Test 4: Validate cyclic
    test_validate_cyclic();
    println!("✅ Validate cyclic test passed");

    println!("🎉 All cycle detection tests passed!");

    // Test with real RDF ontology scenarios
    println!("\nTesting with RDF ontology scenarios...");

    // Create a more complex cycle scenario
    let mut ontology_graph = HashMap::new();
    ontology_graph.insert("base.ttl".to_string(), vec!["core.ttl".to_string(), "utils.ttl".to_string()]);
    ontology_graph.insert("core.ttl".to_string(), vec!["utils.ttl".to_string()]);
    ontology_graph.insert("utils.ttl".to_string(), vec!["base.ttl".to_string()]);
    ontology_graph.insert("common.ttl".to_string(), vec!["base.ttl".to_string()]);

    let cycles = detect_cycles(&ontology_graph);
    println!("Found {} cycles in ontology graph", cycles.len());

    for (i, cycle) in cycles.iter().enumerate() {
        println!("Cycle {}: {}", i + 1, cycle.join(" → "));
    }

    // Test validation
    match validate_acyclic(&ontology_graph) {
        Ok(_) => println!("❌ Should have failed validation but passed"),
        Err(e) => println!("✅ Correctly detected cycles: {}", e),
    }

    println!("\n🔧 Testing cycle fixing strategies...");

    // Simulate fixing by removing imports
    let mut fixed_graph = ontology_graph.clone();
    // Remove the problematic import from utils.ttl to base.ttl
    if let Some(utils_imports) = fixed_graph.get_mut("utils.ttl") {
        utils_imports.retain(|import| import != "base.ttl");
    }

    let fixed_cycles = detect_cycles(&fixed_graph);
    println!("After fix: {} cycles remaining", fixed_cycles.len());

    if fixed_cycles.is_empty() {
        println!("✅ Cycle fix successful");
    } else {
        println!("⚠️  Still have cycles: {:?}", fixed_cycles);
    }

    println!("\n🎯 Cycle detection and fixing verification complete!");
}