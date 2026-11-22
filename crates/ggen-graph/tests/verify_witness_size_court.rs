//! Chicago court: offline `verify_witness` WASM size budget (&lt; 262_144 bytes).
//!
//! ECOSYSTEM-SPEC-27.1.1 Gate 1 Plane 4 — AAA, no mocks, state-based.
//! Falsifier:
//! `cargo test -p ggen-graph --test verify_witness_size_court`

use ggen_graph::witness::{
    court_verify_witness_size, load_production_verify_witness, placeholder_artifact_path,
    production_build_standing, verify_placeholder_witness, verify_production_witness,
    verify_witness, ArtifactProvenance, WitnessRefusal, WitnessStanding,
    AFFIDAVIT_VERIFY_WITNESS_SIZE_BUDGET_BYTES, VERIFY_WITNESS_SIZE_BUDGET_BYTES, WASM_MAGIC,
};

#[test]
fn placeholder_artifact_is_under_budget_and_partial_alive() {
    // Arrange / Act — real fixture bytes on disk (no mocks)
    let receipt = verify_placeholder_witness()
        .expect("placeholder verify_witness.wasm must admit under Gate-1 budget");

    // Assert
    assert!(
        receipt.size.size_bytes < VERIFY_WITNESS_SIZE_BUDGET_BYTES,
        "placeholder size {} must be < {}",
        receipt.size.size_bytes,
        VERIFY_WITNESS_SIZE_BUDGET_BYTES
    );
    assert_eq!(receipt.size.budget_bytes, VERIFY_WITNESS_SIZE_BUDGET_BYTES);
    assert_eq!(receipt.size.provenance, ArtifactProvenance::Placeholder);
    assert_eq!(receipt.size.standing, WitnessStanding::PartialAlive);
    assert!(receipt.wasm_magic_ok);
}

#[test]
fn artifact_at_or_over_262144_bytes_is_refused() {
    // Arrange — exact budget boundary is refuse (≥ ceiling)
    let mut over = WASM_MAGIC.to_vec();
    over.extend(std::iter::repeat(0u8).take(VERIFY_WITNESS_SIZE_BUDGET_BYTES - WASM_MAGIC.len()));
    assert_eq!(over.len(), VERIFY_WITNESS_SIZE_BUDGET_BYTES);

    // Act
    let result = verify_witness(&over, ArtifactProvenance::CallerProvided);

    // Assert
    match result {
        Err(WitnessRefusal::OverBudget {
            size_bytes,
            budget_bytes,
        }) => {
            assert_eq!(size_bytes, VERIFY_WITNESS_SIZE_BUDGET_BYTES);
            assert_eq!(budget_bytes, VERIFY_WITNESS_SIZE_BUDGET_BYTES);
        }
        other => panic!("expected OverBudget refusal, got {other:?}"),
    }
}

#[test]
fn artifact_just_under_budget_with_wasm_magic_is_admitted() {
    // Arrange — 262_143 bytes of WASM-shaped payload
    let mut under = WASM_MAGIC.to_vec();
    under.extend(
        std::iter::repeat(0u8).take(VERIFY_WITNESS_SIZE_BUDGET_BYTES - 1 - WASM_MAGIC.len()),
    );
    assert_eq!(under.len(), VERIFY_WITNESS_SIZE_BUDGET_BYTES - 1);

    // Act
    let receipt = court_verify_witness_size(&under, ArtifactProvenance::CallerProvided)
        .expect("under-budget artifact must admit");
    let verified =
        verify_witness(&under, ArtifactProvenance::CallerProvided).expect("verify must admit");

    // Assert
    assert_eq!(receipt.size_bytes, VERIFY_WITNESS_SIZE_BUDGET_BYTES - 1);
    assert_eq!(receipt.standing, WitnessStanding::PartialAlive);
    assert!(verified.wasm_magic_ok);
}

#[test]
fn missing_wasm_magic_is_refused() {
    // Arrange
    let not_wasm = b"not-a-wasm-module-but-small";

    // Act
    let result = verify_witness(not_wasm, ArtifactProvenance::CallerProvided);

    // Assert
    assert!(
        matches!(result, Err(WitnessRefusal::InvalidWasmMagic)),
        "expected InvalidWasmMagic, got {result:?}"
    );
}

#[test]
fn production_artifact_absent_is_typed_unsupported() {
    // Arrange — clear env so no ambient path leaks in
    // (safe in court: we only unset our own key)
    std::env::remove_var("GGEN_VERIFY_WITNESS_WASM");

    // Act
    let loaded = load_production_verify_witness(None);

    // Assert
    match loaded {
        Err(WitnessRefusal::Unsupported { reason }) => {
            assert!(
                reason.contains("GGEN_VERIFY_WITNESS_WASM")
                    || reason.contains("UNSUPPORTED")
                    || reason.contains("not configured"),
                "reason must name the missing production path: {reason}"
            );
        }
        other => panic!("expected Unsupported, got {other:?}"),
    }
}

#[test]
fn production_build_without_wasm_target_is_unsupported_or_partial() {
    // Act — environment-dependent; both outcomes are typed (never silent)
    let standing = production_build_standing();

    // Assert
    match standing {
        Ok(WitnessStanding::PartialAlive) => {
            // Toolchain present in this VM — still not runtime ALIVE.
        }
        Err(WitnessRefusal::Unsupported { reason }) => {
            assert!(
                reason.contains("wasm32") || reason.contains("UNSUPPORTED"),
                "unsupported reason must cite toolchain: {reason}"
            );
        }
        other => panic!("unexpected standing {other:?}"),
    }
}

#[test]
fn env_ggen_verify_witness_wasm_courts_placeholder_as_partial_external() {
    // Arrange — real on-disk placeholder via env wire (no mocks).
    // Env mutations are process-global; restore before asserts that may panic.
    let path = placeholder_artifact_path();
    assert!(
        path.is_file(),
        "placeholder fixture must exist at {}",
        path.display()
    );
    let prior = std::env::var_os("GGEN_VERIFY_WITNESS_WASM");
    std::env::set_var("GGEN_VERIFY_WITNESS_WASM", &path);

    // Act
    let receipt = verify_production_witness(None);

    // Restore
    match prior {
        None => std::env::remove_var("GGEN_VERIFY_WITNESS_WASM"),
        Some(v) => std::env::set_var("GGEN_VERIFY_WITNESS_WASM", v),
    }

    // Assert — env wire PRESENT; standing is PARTIAL (not production ALIVE)
    let receipt = receipt.expect("env-wired placeholder must court under affidavit+Gate-1 budget");
    assert!(receipt.size.size_bytes < AFFIDAVIT_VERIFY_WITNESS_SIZE_BUDGET_BYTES);
    assert!(receipt.size.size_bytes < VERIFY_WITNESS_SIZE_BUDGET_BYTES);
    assert_eq!(receipt.size.provenance, ArtifactProvenance::ExternalPath);
    assert_eq!(receipt.size.standing, WitnessStanding::PartialAlive);
    assert!(receipt.wasm_magic_ok);
}

#[test]
fn production_path_at_or_over_256000_is_refused_before_gate1() {
    // Arrange — wasm-shaped payload that clears Gate-1 (<262144) but hits
    // affidavit hand-off ceiling (>=256000). Proves production wire uses
    // the tighter bound.
    let mut over = WASM_MAGIC.to_vec();
    over.extend(
        std::iter::repeat(0u8).take(AFFIDAVIT_VERIFY_WITNESS_SIZE_BUDGET_BYTES - WASM_MAGIC.len()),
    );
    assert_eq!(over.len(), AFFIDAVIT_VERIFY_WITNESS_SIZE_BUDGET_BYTES);
    assert!(over.len() < VERIFY_WITNESS_SIZE_BUDGET_BYTES);

    let dir = tempfile::tempdir().expect("tempdir");
    let wasm_path = dir.path().join("over_affidavit_budget.wasm");
    std::fs::write(&wasm_path, &over).expect("write temp wasm");

    // Act — explicit path (same loader as env; avoids racing other env tests)
    let result = verify_production_witness(Some(&wasm_path));

    // Assert
    match result {
        Err(WitnessRefusal::OverBudget {
            size_bytes,
            budget_bytes,
        }) => {
            assert_eq!(size_bytes, AFFIDAVIT_VERIFY_WITNESS_SIZE_BUDGET_BYTES);
            assert_eq!(budget_bytes, AFFIDAVIT_VERIFY_WITNESS_SIZE_BUDGET_BYTES);
        }
        other => panic!("expected OverBudget at affidavit ceiling, got {other:?}"),
    }
}
