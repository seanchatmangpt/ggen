//! Phase 2 Recipe Validation Tests (Chicago TDD)
//!
//! Tests that verify Phase 2 recipes in justfile execute correctly:
//! - just coherence-check (validates ontology + shapes)
//! - just test-phase2 (runs all Phase 2 tests)
//! - just inverse-sync (runs inverse-sync command)
//! - just round-trip (full O→A→O cycle)
//! - Pre-commit integration (includes coherence-check)
//! - SLO validation (Phase 2 performance checks)

use std::path::PathBuf;
use std::process::Command;

/// Test that coherence-check recipe validates ontology and shapes
#[test]
fn test_coherence_check_recipe() {
    let status = Command::new("just")
        .arg("coherence-check")
        .current_dir("/home/user/ggen")
        .status()
        .expect("Failed to run 'just coherence-check'");

    assert!(
        status.success(),
        "just coherence-check failed with exit code: {:?}",
        status.code()
    );
}

/// Test that test-phase2 recipe runs all required tests
#[test]
fn test_phase2_recipe() {
    // This test would take ~120s, so we verify the recipe exists and is well-formed
    let output = Command::new("just")
        .arg("--show")
        .arg("test-phase2")
        .current_dir("/home/user/ggen")
        .output()
        .expect("Failed to run 'just --show test-phase2'");

    assert!(
        output.status.success(),
        "just --show test-phase2 failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let recipe = String::from_utf8(output.stdout).expect("Invalid UTF-8");
    // Verify all required tests are referenced in the recipe
    assert!(
        recipe.contains("ast_extractor_70pct_test"),
        "Recipe missing ast_extractor_70pct_test"
    );
    assert!(
        recipe.contains("inverse_receipt_chain_test"),
        "Recipe missing inverse_receipt_chain_test"
    );
    assert!(
        recipe.contains("provenance_envelope_test"),
        "Recipe missing provenance_envelope_test"
    );
    assert!(
        recipe.contains("ocel_conformance_test"),
        "Recipe missing ocel_conformance_test"
    );
    assert!(
        recipe.contains("coherence_hash_expectations_test"),
        "Recipe missing coherence_hash_expectations_test"
    );
    assert!(
        recipe.contains("pm4py_bridge_test"),
        "Recipe missing pm4py_bridge_test"
    );
    assert!(
        recipe.contains("post_chatman_coherence_integration"),
        "Recipe missing post_chatman_coherence_integration"
    );
}

/// Test that inverse-sync recipe validates and exits successfully
#[test]
fn test_inverse_sync_recipe() {
    let status = Command::new("just")
        .arg("inverse-sync")
        .current_dir("/home/user/ggen")
        .status()
        .expect("Failed to run 'just inverse-sync'");

    assert!(
        status.success(),
        "just inverse-sync failed with exit code: {:?}",
        status.code()
    );
}

/// Test that round-trip recipe runs coherence-check and inverse-sync
#[test]
fn test_round_trip_recipe() {
    let status = Command::new("just")
        .arg("round-trip")
        .current_dir("/home/user/ggen")
        .status()
        .expect("Failed to run 'just round-trip'");

    assert!(
        status.success(),
        "just round-trip failed with exit code: {:?}",
        status.code()
    );
}

/// Test that post-chatman ontology files exist and are valid
#[test]
fn test_post_chatman_ontology_files() {
    let ontology = PathBuf::from("/home/user/ggen/.specify/specs/post-chatman/post_chatman.ttl");
    let shapes = PathBuf::from("/home/user/ggen/.specify/specs/post-chatman/post_chatman_shapes.ttl");

    assert!(
        ontology.exists(),
        "post_chatman.ttl not found at {:?}",
        ontology
    );
    assert!(shapes.exists(), "post_chatman_shapes.ttl not found at {:?}", shapes);

    // Verify files are non-empty
    let ontology_content = std::fs::read_to_string(&ontology)
        .expect("Failed to read ontology file");
    assert!(!ontology_content.is_empty(), "ontology file is empty");

    let shapes_content = std::fs::read_to_string(&shapes)
        .expect("Failed to read shapes file");
    assert!(!shapes_content.is_empty(), "shapes file is empty");
}

/// Test that Phase 2 recipe structure is correct (via --show)
#[test]
fn test_phase2_recipe_structure() {
    let output = Command::new("just")
        .arg("--show")
        .arg("test-phase2")
        .current_dir("/home/user/ggen")
        .output()
        .expect("Failed to run 'just --show test-phase2'");

    assert!(output.status.success(), "Recipe structure check failed");

    let recipe = String::from_utf8(output.stdout).expect("Invalid UTF-8");

    // Verify recipe has proper error handling
    assert!(
        recipe.contains("set -euo pipefail"),
        "Recipe missing error handling"
    );

    // Verify recipe has exit code checks
    assert!(recipe.contains("|| exit 1"), "Recipe missing exit code checks");
}

/// Test that SLO-check recipe includes Phase 2 components
#[test]
fn test_slo_check_includes_phase2() {
    let output = Command::new("just")
        .arg("--show")
        .arg("slo-check")
        .current_dir("/home/user/ggen")
        .output()
        .expect("Failed to run 'just --show slo-check'");

    assert!(output.status.success(), "SLO check recipe structure failed");

    let recipe = String::from_utf8(output.stdout).expect("Invalid UTF-8");

    // Verify Phase 2 performance tests are included
    assert!(
        recipe.contains("inverse_receipt_chain_test")
            || recipe.contains("coherence_hash_expectations_test"),
        "SLO check missing Phase 2 performance validation"
    );
}

/// Test that pre-commit recipe includes coherence-check
#[test]
fn test_pre_commit_includes_coherence() {
    let output = Command::new("just")
        .arg("--show")
        .arg("pre-commit")
        .current_dir("/home/user/ggen")
        .output()
        .expect("Failed to run 'just --show pre-commit'");

    assert!(output.status.success(), "Pre-commit recipe check failed");

    let recipe = String::from_utf8(output.stdout).expect("Invalid UTF-8");

    // Verify coherence-check is a dependency
    assert!(
        recipe.contains("coherence-check"),
        "Pre-commit recipe missing coherence-check dependency"
    );
}

/// Test that CI workflow includes phase2 job
#[test]
fn test_ci_workflow_includes_phase2() {
    let ci_workflow = std::fs::read_to_string("/home/user/ggen/.github/workflows/ci.yml")
        .expect("Failed to read CI workflow");

    assert!(
        ci_workflow.contains("phase2:"),
        "CI workflow missing phase2 job"
    );
    assert!(
        ci_workflow.contains("name: Phase 2 (Inverse Sync + Coherence)"),
        "CI workflow phase2 job missing correct name"
    );
    assert!(
        ci_workflow.contains("coherence-check passed") || ci_workflow.contains("Coherence"),
        "CI workflow phase2 job missing coherence validation"
    );
    assert!(
        ci_workflow.contains("ast_extractor_70pct_test")
            || ci_workflow.contains("Phase 2 test suite"),
        "CI workflow phase2 job missing Phase 2 tests"
    );
}

/// Test that CI status gate includes phase2 as required dependency
#[test]
fn test_ci_status_requires_phase2() {
    let ci_workflow = std::fs::read_to_string("/home/user/ggen/.github/workflows/ci.yml")
        .expect("Failed to read CI workflow");

    assert!(
        ci_workflow.contains("needs: [check, build, test, doctest, phase2]"),
        "CI status gate doesn't require phase2 job"
    );
}

/// Test that Makefile.toml has backward-compatible Phase 2 recipes
#[test]
fn test_makefile_backward_compatibility() {
    let makefile = std::fs::read_to_string("/home/user/ggen/Makefile.toml")
        .expect("Failed to read Makefile.toml");

    assert!(
        makefile.contains("[tasks.test-phase2]"),
        "Makefile.toml missing test-phase2 task"
    );
    assert!(
        makefile.contains("[tasks.coherence-check]"),
        "Makefile.toml missing coherence-check task"
    );
    assert!(
        makefile.contains("[tasks.inverse-sync]"),
        "Makefile.toml missing inverse-sync task"
    );
    assert!(
        makefile.contains("[tasks.round-trip]"),
        "Makefile.toml missing round-trip task"
    );
    assert!(
        makefile.contains("DEPRECATED: use `just"),
        "Makefile.toml tasks missing deprecation warnings"
    );
}

/// Test ontology validates with ggen validate command
#[test]
fn test_ontology_ggen_validate() {
    let output = Command::new("ggen")
        .arg("validate")
        .arg(".specify/specs/post-chatman/post_chatman.ttl")
        .current_dir("/home/user/ggen")
        .output()
        .expect("Failed to run ggen validate");

    assert!(
        output.status.success(),
        "ggen validate ontology failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
}

/// Test shapes validate with ggen validate command
#[test]
fn test_shapes_ggen_validate() {
    let output = Command::new("ggen")
        .arg("validate")
        .arg(".specify/specs/post-chatman/post_chatman_shapes.ttl")
        .current_dir("/home/user/ggen")
        .output()
        .expect("Failed to run ggen validate");

    assert!(
        output.status.success(),
        "ggen validate shapes failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
}
