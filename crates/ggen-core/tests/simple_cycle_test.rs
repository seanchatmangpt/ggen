//! Simple cycle detection test to verify functionality
//! This test bypasses the compilation issues in the existing test suite

use ggen_core::graph::{cycle_detection, cycle_fixer::CycleFixer, cycle_fixer::FixStrategy};
use ggen_utils::error::Result;
use std::collections::HashMap;
use tempfile::TempDir;

#[test]
fn test_basic_cycle_detection() -> Result<()> {
    // Create a simple graph with a cycle: A -> B -> C -> A
    let mut graph = HashMap::new();
    graph.insert("A".to_string(), vec!["B".to_string()]);
    graph.insert("B".to_string(), vec!["C".to_string()]);
    graph.insert("C".to_string(), vec!["A".to_string()]);

    let cycles = cycle_detection::detect_cycles(&graph);

    assert_eq!(cycles.len(), 1, "Should detect exactly one cycle");
    assert_eq!(
        cycles[0].len(),
        4,
        "Cycle should have 4 nodes: A -> B -> C -> A"
    );
    assert!(cycles[0].contains(&"A".to_string()));
    assert!(cycles[0].contains(&"B".to_string()));
    assert!(cycles[0].contains(&"C".to_string()));

    Ok(())
}

#[test]
fn test_no_cycle_detection() -> Result<()> {
    // Create an acyclic graph: A -> B -> C, D -> B
    let mut graph = HashMap::new();
    graph.insert("A".to_string(), vec!["B".to_string()]);
    graph.insert("B".to_string(), vec!["C".to_string()]);
    graph.insert("D".to_string(), vec!["B".to_string()]);
    graph.insert("C".to_string(), vec![]);

    let cycles = cycle_detection::detect_cycles(&graph);

    assert_eq!(cycles.len(), 0, "Should detect no cycles in acyclic graph");

    Ok(())
}

#[test]
fn test_validate_acyclic() -> Result<()> {
    // Test validation with acyclic graph
    let mut graph = HashMap::new();
    graph.insert("A".to_string(), vec!["B".to_string()]);
    graph.insert("B".to_string(), vec!["C".to_string()]);
    graph.insert("C".to_string(), vec![]);

    let result = cycle_detection::validate_acyclic(&graph);
    assert!(result.is_ok(), "Acyclic graph should pass validation");

    Ok(())
}

#[test]
fn test_validate_cyclic() -> Result<()> {
    // Test validation with cyclic graph
    let mut graph = HashMap::new();
    graph.insert("A".to_string(), vec!["B".to_string()]);
    graph.insert("B".to_string(), vec!["A".to_string()]);

    let result = cycle_detection::validate_acyclic(&graph);
    assert!(result.is_err(), "Cyclic graph should fail validation");

    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("Cyclic ontology dependencies"),
        "Error message should mention cyclic dependencies"
    );

    Ok(())
}

#[test]
fn test_cycle_fixer_dry_run() -> Result<()> {
    let temp_dir = TempDir::new()?;

    // Create test ontology files
    let base_content = r#"
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix ex: <http://example.org/> .

<> owl:imports <core.ttl> .

ex:BaseClass a owl:Class ;
    rdfs:label "Base class" .
"#;

    let core_content = r#"
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix ex: <http://example.org/> .

<> owl:imports <base.ttl> .

ex:CoreClass a owl:Class ;
    rdfs:label "Core class" ;
    rdfs:subClassOf ex:BaseClass .
"#;

    std::fs::write(temp_dir.path().join("base.ttl"), base_content)?;
    std::fs::write(temp_dir.path().join("core.ttl"), core_content)?;

    let fixer = CycleFixer::new(temp_dir.path());

    // Test dry run - should detect cycles but not fix them
    let report = fixer.detect_and_fix(FixStrategy::RemoveImport, true)?;

    assert_eq!(report.cycles_found, 1, "Should detect 1 cycle");
    assert_eq!(report.fixes_applied, 0, "Should apply 0 fixes in dry run");
    assert!(
        report.backup_path.is_none(),
        "Should not create backup in dry run"
    );

    // Verify files are unchanged
    let base_content_after = std::fs::read_to_string(temp_dir.path().join("base.ttl"))?;
    assert!(
        base_content_after.contains("owl:imports <core.ttl>"),
        "Base file should still contain imports after dry run"
    );

    Ok(())
}
