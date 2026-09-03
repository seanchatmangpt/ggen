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

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn get_ggen_binary_path() -> PathBuf {
    let root = workspace_root();
    let debug_path = root.join("target").join("debug").join("ggen");
    if debug_path.exists() {
        debug_path
    } else {
        PathBuf::from("ggen")
    }
}

/// Test that coherence-check recipe validates ontology and shapes
#[test]
fn test_coherence_check_recipe() {
    let status = Command::new("just")
        .arg("coherence-check")
        .current_dir(workspace_root())
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
        .current_dir(workspace_root())
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
        recipe.contains("coherence_hash_expectations_test"),
        "Recipe missing coherence_hash_expectations_test"
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
        .current_dir(workspace_root())
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
        .current_dir(workspace_root())
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
    let ontology = workspace_root().join(".specify/specs/post-chatman/post_chatman.ttl");
    let shapes = workspace_root().join(".specify/specs/post-chatman/post_chatman_shapes.ttl");

    assert!(
        ontology.exists(),
        "post_chatman.ttl not found at {:?}",
        ontology
    );
    assert!(
        shapes.exists(),
        "post_chatman_shapes.ttl not found at {:?}",
        shapes
    );

    // Verify files are non-empty
    let ontology_content =
        std::fs::read_to_string(&ontology).expect("Failed to read ontology file");
    assert!(!ontology_content.is_empty(), "ontology file is empty");

    let shapes_content = std::fs::read_to_string(&shapes).expect("Failed to read shapes file");
    assert!(!shapes_content.is_empty(), "shapes file is empty");
}

/// Test that Phase 2 recipe structure is correct (via --show)
#[test]
fn test_phase2_recipe_structure() {
    let output = Command::new("just")
        .arg("--show")
        .arg("test-phase2")
        .current_dir(workspace_root())
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
    assert!(
        recipe.contains("|| exit 1"),
        "Recipe missing exit code checks"
    );
}

/// Test that SLO-check recipe includes Phase 2 components
#[test]
fn test_slo_check_includes_phase2() {
    let output = Command::new("just")
        .arg("--show")
        .arg("slo-check")
        .current_dir(workspace_root())
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
        .current_dir(workspace_root())
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

/// Test that CI workflow still runs Phase 2 evidence (inverse-sync +
/// coherence + the rest of the workspace test suite).
///
/// `d68cee811` ("refactor CI to 80/20 ERRC", 2026-08-05) replaced the fixed
/// `check`/`build`/`test`/`doctest`/`phase2`/`cargo-cicd` job list this test
/// originally checked for with a path-routed model: a single `admission` job
/// computes a `deep_matrix_json` (via `scripts/ci/errc_router.py`) and a
/// matrixed `deep` job runs only the lanes whose owned paths actually
/// changed. There is no standalone `phase2:` job anymore -- Phase 2 evidence
/// (this very test file, plus `just coherence-check` / `just inverse-sync`
/// covered above) now runs inside the `integration_deep` lane's
/// `cargo test --workspace --exclude ggen-lsp` step, gated the same way
/// every other deep lane is. Verified live: `errc_router.classify_path`
/// routes `tests/phase2_recipes_test.rs` itself into `integration_deep`.
#[test]
fn test_ci_workflow_includes_phase2() {
    let ci_workflow = std::fs::read_to_string(workspace_root().join(".github/workflows/ci.yml"))
        .expect("Failed to read CI workflow");

    assert!(
        ci_workflow.contains("integration_deep) cargo test --workspace"),
        "CI workflow's integration_deep lane no longer runs the workspace test suite \
         (this is what now exercises Phase 2 tests: inverse-sync, coherence, ast_extractor, etc.)"
    );

    // Confirm the router actually routes this test file's own path -- i.e.
    // the file containing the Phase 2 recipe assertions -- into the lane
    // asserted above, using the real router module as the collaborator
    // rather than re-implementing its classification rules here.
    let output = Command::new("python3")
        .args([
            "-c",
            "import sys; sys.path.insert(0, 'scripts/ci'); import errc_router as r; \
             print('integration_deep' in r.classify_path('tests/phase2_recipes_test.rs'))",
        ])
        .current_dir(workspace_root())
        .output()
        .expect("Failed to run errc_router.classify_path via python3");
    assert!(
        output.status.success(),
        "errc_router.classify_path invocation failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout).trim(),
        "True",
        "errc_router no longer routes tests/phase2_recipes_test.rs into integration_deep -- \
         Phase 2 evidence would silently stop being gated in CI"
    );
}

/// Test that CI status gate still requires the deep evidence lane that
/// carries Phase 2 (integration_deep) to have run and succeeded.
///
/// Post-`d68cee811` there is no per-job `needs: [check, build, test, ...]`
/// list to check job names against -- `ci-status` gates on exactly two
/// upstream jobs, `admission` and the matrixed `deep` job. `deep`'s matrix
/// is computed by `admission` from `errc_router`'s path routing, and GitHub
/// Actions fails a matrixed job as a whole if any included lane fails
/// (`fail-fast: false` only disables early cancellation, not the overall
/// pass/fail rollup), so requiring `deep` transitively requires every lane
/// the router activated for the change -- including `integration_deep`
/// whenever Phase-2-relevant paths (this test file among them, per the
/// prior test) are touched.
#[test]
fn test_ci_status_requires_phase2() {
    let ci_workflow = std::fs::read_to_string(workspace_root().join(".github/workflows/ci.yml"))
        .expect("Failed to read CI workflow");

    let needs_line = ci_workflow
        .lines()
        .find(|line| line.trim_start().starts_with("needs: [") && line.contains("admission"))
        .expect("CI status gate's needs: [...] line (containing admission) not found");

    for required_job in ["admission", "deep"] {
        assert!(
            needs_line.contains(required_job),
            "CI status gate doesn't require {required_job} job (needs line: {needs_line})"
        );
    }

    // The gate must actually fail closed, not just list the names: assert
    // the enforcement step ties DEEP's real result to CI status.
    assert!(
        ci_workflow.contains("DEEP: ${{ needs.deep.result }}")
            && ci_workflow.contains(r#"case "$DEEP" in success|skipped) ;; *)"#),
        "ci-status no longer fails closed on the deep job's result"
    );
}

/// Test that Makefile.toml has backward-compatible Phase 2 recipes
#[test]
fn test_makefile_backward_compatibility() {
    let makefile = std::fs::read_to_string(workspace_root().join("Makefile.toml"))
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
    let output = Command::new(get_ggen_binary_path())
        .arg("graph")
        .arg("validate")
        // ggen-engine's graph validate takes repeatable --files (not the old
        // ggen-core positional --schema-file), per the v26.7.16 CLI-routing flip.
        .arg("--files")
        .arg(".specify/specs/post-chatman/post_chatman.ttl")
        .current_dir(workspace_root())
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
    let output = Command::new(get_ggen_binary_path())
        .arg("graph")
        .arg("validate")
        // ggen-engine's graph validate takes repeatable --files (not the old
        // ggen-core positional --schema-file), per the v26.7.16 CLI-routing flip.
        .arg("--files")
        .arg(".specify/specs/post-chatman/post_chatman_shapes.ttl")
        .current_dir(workspace_root())
        .output()
        .expect("Failed to run ggen validate");

    assert!(
        output.status.success(),
        "ggen validate shapes failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
}
