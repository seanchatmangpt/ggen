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

/// Test that CI workflow wires the deep-evidence lanes covering what the old
/// "phase2" job used to cover: build/test/lint coverage over the workspace.
///
/// Rewritten (2026-08-10) after `.github/workflows/ci.yml` was replaced by
/// the "80/20 ERRC" admission/deep-lane design (commit `d68cee811`, "refactor
/// CI to 80/20 ERRC") -- the old multi-job topology this test used to assert
/// (`phase2:`, `name: Phase 2 (Inverse Sync + Coherence)`,
/// `ast_extractor_70pct_test`) no longer exists anywhere in the file; that
/// design deleted 80 separate workflow files and replaced them with one
/// `admission` job (fast checks) feeding a matrixed `deep` job (per-lane
/// evidence: `core_deep`/`integration_deep`/`quality_deep`/`security_deep`/
/// `docs_deep`/`lsp_deep`/`ci_deep`) plus a `ci-status` aggregate gate. The
/// substantive invariant this test protects -- "the required CI gate
/// actually builds, tests, and lints the workspace, not just fast-admits
/// it" -- still holds, just via different job/step names; asserted against
/// those real names instead of the retired ones. `test_pre_commit_includes_coherence`
/// (above) separately covers the coherence-check invariant via `just
/// pre-commit`, which is unrelated to this CI-workflow-file structure.
#[test]
fn test_ci_workflow_wires_deep_evidence_lanes() {
    let ci_workflow = std::fs::read_to_string(workspace_root().join(".github/workflows/ci.yml"))
        .expect("Failed to read CI workflow");

    assert!(
        ci_workflow.contains("deep:"),
        "CI workflow missing the deep-evidence job"
    );
    assert!(
        ci_workflow.contains("ci-status:"),
        "CI workflow missing the ci-status aggregate gate"
    );
    for lane_evidence in [
        "cargo check --workspace",
        "cargo build --workspace",
        "cargo test --workspace",
        "cargo test --doc --workspace",
        "cargo clippy --workspace",
    ] {
        assert!(
            ci_workflow.contains(lane_evidence),
            "CI workflow's deep-evidence lanes missing real command: {lane_evidence}"
        );
    }
}

/// Test that the CI status aggregate gate genuinely depends on both the fast
/// admission pass and the deep-evidence matrix, and refuses to report success
/// when the deep lane fails -- the real ERRC-era equivalent of the retired
/// "phase2 is a required job" invariant.
///
/// Rewritten (2026-08-10) for the same reason as
/// `test_ci_workflow_wires_deep_evidence_lanes` above: the old `needs: [...]`
/// line naming `check`/`build`/`test`/`doctest`/`phase2`/`cargo-cicd` no
/// longer exists -- `ci-status` now depends on exactly `[admission, deep]`
/// and gates on their real job `result`s (`needs.admission.result` /
/// `needs.deep.result`), not a static job-name list.
#[test]
fn test_ci_status_requires_admission_and_deep() {
    let ci_workflow = std::fs::read_to_string(workspace_root().join(".github/workflows/ci.yml"))
        .expect("Failed to read CI workflow");

    let needs_line = ci_workflow
        .lines()
        .find(|line| line.trim_start().starts_with("needs: [") && line.contains("admission"))
        .expect("ci-status's needs: [...] line (containing admission) not found");

    for required_job in ["admission", "deep"] {
        assert!(
            needs_line.contains(required_job),
            "CI status gate doesn't require {required_job} job (needs line: {needs_line})"
        );
    }

    // The gate must actually fail closed when the deep lane fails, not just
    // list it as a dependency -- assert the real refusal logic is present.
    assert!(
        ci_workflow.contains("BUILD_BROKEN:DEEP_LANE_FAILED"),
        "CI status gate missing its deep-lane-failure refusal"
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
