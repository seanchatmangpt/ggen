//! Comprehensive cycle detection and fixing tests
//!
//! This test suite verifies that cycle detection and fixing works correctly
//! with real RDF ontologies containing circular dependencies.

use ggen_core::graph::{cycle_detection, cycle_fixer::CycleFixer, cycle_fixer::FixStrategy};
use ggen_utils::error::Result;
use std::fs;
use tempfile::TempDir;

/// Create test ontology files with cycles
fn create_cyclic_ontologies(dir: &std::path::Path) -> Result<()> {
    // Create cycle: base.ttl -> core.ttl -> utils.ttl -> base.ttl
    let base_content = r#"
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <http://example.org/> .

<> owl:imports <core.ttl> .
<> owl:imports <utils.ttl> .

ex:BaseClass a owl:Class ;
    rdfs:label "Base class" .
"#;

    let core_content = r#"
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <http://example.org/> .

<> owl:imports <utils.ttl> .

ex:CoreClass a owl:Class ;
    rdfs:label "Core class" ;
    rdfs:subClassOf ex:BaseClass .
"#;

    let utils_content = r#"
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <http://example.org/> .

<> owl:imports <base.ttl> .

ex:UtilsClass a owl:Class ;
    rdfs:label "Utils class" ;
    rdfs:subClassOf ex:BaseClass .
"#;

    fs::write(dir.join("base.ttl"), base_content)?;
    fs::write(dir.join("core.ttl"), core_content)?;
    fs::write(dir.join("utils.ttl"), utils_content)?;

    // Create acyclic ontology
    let acyclic_content = r#"
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <http://example.org/> .

<> owl:imports <base.ttl> .

ex:IndependentClass a owl:Class ;
    rdfs:label "Independent class" .
"#;

    fs::write(dir.join("acyclic.ttl"), acyclic_content)?;

    Ok(())
}

#[test]
fn test_cycle_detection_with_real_ontologies() -> Result<()> {
    let temp_dir = TempDir::new()?;
    create_cyclic_ontologies(temp_dir.path())?;

    // Build import graph
    let mut graph = std::collections::HashMap::new();

    // Simulate the imports based on our test ontologies
    graph.insert(
        "base.ttl".to_string(),
        vec!["core.ttl".to_string(), "utils.ttl".to_string()],
    );
    graph.insert("core.ttl".to_string(), vec!["utils.ttl".to_string()]);
    graph.insert("utils.ttl".to_string(), vec!["base.ttl".to_string()]);
    graph.insert("acyclic.ttl".to_string(), vec!["base.ttl".to_string()]);

    // Detect cycles
    let cycles = cycle_detection::detect_cycles(&graph);

    assert!(cycles.len() >= 1, "Should detect at least one cycle");

    let has_main_cycle = cycles.iter().any(|cycle| {
        cycle.contains(&"base.ttl".to_string())
            && cycle.contains(&"core.ttl".to_string())
            && cycle.contains(&"utils.ttl".to_string())
    });

    assert!(has_main_cycle, "Should detect the main 3-file cycle");

    Ok(())
}

#[test]
fn test_cycle_fixing_with_real_ontologies() -> Result<()> {
    let temp_dir = TempDir::new()?;
    create_cyclic_ontologies(temp_dir.path())?;

    let fixer = CycleFixer::new(temp_dir.path());

    // Test dry run first
    let dry_run_report = fixer.detect_and_fix(FixStrategy::RemoveImport, true)?;
    println!("Dry run found {} cycles", dry_run_report.cycles_found);
    assert!(
        dry_run_report.cycles_found >= 1,
        "Should detect at least one cycle in dry run"
    );
    assert_eq!(
        dry_run_report.fixes_applied, 0,
        "Should apply no fixes in dry run"
    );

    // Actually fix the cycle
    let report = fixer.detect_and_fix(FixStrategy::RemoveImport, false)?;

    assert!(report.cycles_found >= 1, "Should find at least one cycle");
    assert!(report.fixes_applied >= 1, "Should apply at least one fix");
    assert!(
        report.files_modified.len() >= 1,
        "Should modify at least one file"
    );
    assert!(report.backup_path.is_some(), "Should create backup");

    // Verify the fix by re-detecting cycles
    let after_fix_report = fixer.detect_and_fix(FixStrategy::RemoveImport, true)?;
    assert_eq!(
        after_fix_report.cycles_found, 0,
        "Should have no cycles after fix"
    );

    Ok(())
}

#[test]
fn test_acyclic_validation() -> Result<()> {
    let temp_dir = TempDir::new()?;
    create_cyclic_ontologies(temp_dir.path())?;

    // Build acyclic graph
    let mut graph = std::collections::HashMap::new();
    graph.insert("acyclic.ttl".to_string(), vec!["base.ttl".to_string()]);
    graph.insert(
        "base.ttl".to_string(),
        vec!["core.ttl".to_string(), "utils.ttl".to_string()],
    );
    graph.insert("core.ttl".to_string(), vec![]);
    graph.insert("utils.ttl".to_string(), vec![]);

    // Validate that it's acyclic
    let result = cycle_detection::validate_acyclic(&graph);
    assert!(result.is_ok(), "Acyclic graph should pass validation");

    Ok(())
}

#[test]
fn test_complex_cycle_detection() -> Result<()> {
    let temp_dir = TempDir::new()?;
    create_cyclic_ontologies(temp_dir.path())?;

    // Build complex graph with multiple cycles
    let mut graph = std::collections::HashMap::new();

    // Cycle 1: base -> core -> utils -> base
    graph.insert(
        "base.ttl".to_string(),
        vec!["core.ttl".to_string(), "utils.ttl".to_string()],
    );
    graph.insert("core.ttl".to_string(), vec!["utils.ttl".to_string()]);
    graph.insert("utils.ttl".to_string(), vec!["base.ttl".to_string()]);

    // Cycle 2: service -> api -> service
    graph.insert("service.ttl".to_string(), vec!["api.ttl".to_string()]);
    graph.insert("api.ttl".to_string(), vec!["service.ttl".to_string()]);

    // Acyclic file
    graph.insert("common.ttl".to_string(), vec!["base.ttl".to_string()]);

    let cycles = cycle_detection::detect_cycles(&graph);
    println!("Found {} cycles: {:?}", cycles.len(), cycles);

    // Should find at least 2 cycles (DFS may find sub-cycles too)
    assert!(cycles.len() >= 2, "Should detect at least two cycles");

    // Verify we have both the expected cycles
    let has_base_cycle = cycles.iter().any(|cycle| {
        cycle.contains(&"base.ttl".to_string())
            && cycle.contains(&"core.ttl".to_string())
            && cycle.contains(&"utils.ttl".to_string())
    });

    let has_service_cycle = cycles.iter().any(|cycle| {
        cycle.contains(&"service.ttl".to_string()) && cycle.contains(&"api.ttl".to_string())
    });

    assert!(has_base_cycle, "Should find base/core/utils cycle");
    assert!(has_service_cycle, "Should find service/api cycle");

    Ok(())
}
