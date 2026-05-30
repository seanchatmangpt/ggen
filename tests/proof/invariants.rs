//! T1 Layer: Invariant Tests
//! Validates manifest rules, audit trails, and deterministic execution
//!
//! **Purpose**: The T1 (invariant) layer validates that core system rules
//! are enforced: receipts are written, signatures are non-empty, deterministic
//! execution produces identical outputs, and manifest files are valid.

use assert_cmd::Command;
use std::fs;
use tempfile::TempDir;

type TestResult = Result<(), Box<dyn std::error::Error>>;

/// Helper: Create a minimal ggen.toml in a temp directory
fn setup_minimal_ggen_toml(dir: &TempDir) -> TestResult {
    let ggen_toml = r#"[project]
name = "test-project"
version = "0.1.0"

[ontology]
source = "ontology.ttl"
"#;
    fs::write(dir.path().join("ggen.toml"), ggen_toml)?;
    Ok(())
}

/// Helper: Create a minimal valid TTL file
fn setup_minimal_ttl(dir: &TempDir) -> TestResult {
    let ttl = r#"@prefix ex: <http://example.org/> .
ex:Thing a rdfs:Class .
"#;
    fs::write(dir.path().join("ontology.ttl"), ttl)?;
    Ok(())
}

// ========================================================================
// CLI Tests
// ========================================================================

/// invariant_id: CLI-01
/// Validates: sync exits 0 only when receipt is written
/// Failure class: Decorative Completion (code returns 0 but no state change)
#[test]
fn cli_01_sync_exits_zero_with_receipt() -> TestResult {
    let temp = TempDir::new()?;
    setup_minimal_ggen_toml(&temp)?;
    setup_minimal_ttl(&temp)?;

    // Arrange: create workspace with ggen.toml and ontology
    let _ = Command::cargo_bin("ggen")?
        .arg("sync")
        .current_dir(temp.path())
        .assert()
        .success();

    // Act: run sync and check for receipt
    // Assert: .ggen/receipts/latest.json exists
    let receipt_path = temp
        .path()
        .join(".ggen")
        .join("receipts")
        .join("latest.json");
    assert!(
        receipt_path.exists(),
        "Receipt must be written at {} when sync succeeds",
        receipt_path.display()
    );
    Ok(())
}

/// invariant_id: CLI-02
/// Validates: sync --locked exits non-zero if packs.lock absent
/// Failure class: Fail-Open (command continues when locked file is missing)
#[test]
fn cli_02_sync_locked_fails_without_packs_lock() -> TestResult {
    let temp = TempDir::new()?;
    setup_minimal_ggen_toml(&temp)?;
    setup_minimal_ttl(&temp)?;

    // Arrange: workspace with ggen.toml but NO .ggen/packs.lock
    // Act: attempt sync --locked
    let _ = Command::cargo_bin("ggen")?
        .arg("sync")
        .arg("--locked")
        .current_dir(temp.path())
        .assert()
        .failure();

    // Assert: command must exit non-zero
    Ok(())
}

/// invariant_id: CLI-03
/// Validates: init creates valid ggen.toml
/// Failure class: Legacy Path Contamination (multiple config writers)
#[test]
fn cli_03_init_creates_valid_ggen_toml() -> TestResult {
    let temp = TempDir::new()?;

    // Act: run init
    let _ = Command::cargo_bin("ggen")?
        .arg("init")
        .arg("test-proj")
        .current_dir(temp.path())
        .assert()
        .success();

    // Assert: ggen.toml exists and contains [project] section
    let config_path = temp.path().join("ggen.toml");
    assert!(
        config_path.exists(),
        "ggen.toml must exist after init at {}",
        config_path.display()
    );

    let content = fs::read_to_string(&config_path)?;
    assert!(
        content.contains("[project]"),
        "ggen.toml must contain [project] section"
    );
    Ok(())
}

/// invariant_id: CLI-04
/// Validates: doctor succeeds on valid workspace
/// Failure class: Contract Drift (doctor ignores state mismatches)
#[test]
fn cli_04_doctor_succeeds_on_valid_workspace() -> TestResult {
    let temp = TempDir::new()?;
    setup_minimal_ggen_toml(&temp)?;
    setup_minimal_ttl(&temp)?;

    // Act: run doctor on valid workspace
    let _ = Command::cargo_bin("ggen")?
        .arg("doctor")
        .current_dir(temp.path())
        .assert()
        .success();

    // Assert: command exits 0
    Ok(())
}

/// invariant_id: CLI-05
/// Validates: pack add fails on missing pack
/// Failure class: Fail-Open (missing pack is ignored instead of error)
#[test]
fn cli_05_pack_add_fails_on_missing_pack() -> TestResult {
    let temp = TempDir::new()?;
    setup_minimal_ggen_toml(&temp)?;

    // Act: attempt to add non-existent pack
    let _ = Command::cargo_bin("ggen")?
        .arg("pack")
        .arg("add")
        .arg("nonexistent/pack")
        .current_dir(temp.path())
        .assert()
        .failure();

    // Assert: command must exit non-zero
    Ok(())
}

// ========================================================================
// Pipeline Tests
// ========================================================================

/// invariant_id: PIPE-01
/// Validates: sync fails on invalid TTL
/// Failure class: Epistemic Bypass (pipeline doesn't validate input)
#[test]
fn pipe_01_sync_fails_on_invalid_ttl() -> TestResult {
    let temp = TempDir::new()?;
    setup_minimal_ggen_toml(&temp)?;

    // Arrange: create INVALID TTL (malformed RDF)
    let invalid_ttl = r#"@prefix ex: <http://example.org/> .
ex:Thing broken syntax here
"#;
    fs::write(temp.path().join("ontology.ttl"), invalid_ttl)?;

    // Act: run sync with invalid input
    let _ = Command::cargo_bin("ggen")?
        .arg("sync")
        .current_dir(temp.path())
        .assert()
        .failure();

    // Assert: command must exit non-zero
    Ok(())
}

/// invariant_id: PIPE-02
/// Validates: validate succeeds on valid TTL
/// Failure class: Epistemic Bypass (validation skipped or cached)
#[test]
fn pipe_02_validate_succeeds_on_valid_ttl() -> TestResult {
    let temp = TempDir::new()?;
    setup_minimal_ttl(&temp)?;

    // Act: run graph validate on valid TTL
    let _ = Command::cargo_bin("ggen")?
        .arg("graph")
        .arg("validate")
        .arg(temp.path().join("ontology.ttl"))
        .assert()
        .success();

    // Assert: command exits 0
    Ok(())
}

// ========================================================================
// Receipt Tests
// ========================================================================

/// invariant_id: RCPT-01
/// Validates: sync writes receipt directory
/// Failure class: Decorative Completion (no receipt created)
#[test]
fn rcpt_01_sync_writes_receipt_directory() -> TestResult {
    let temp = TempDir::new()?;
    setup_minimal_ggen_toml(&temp)?;
    setup_minimal_ttl(&temp)?;

    // Act: run sync
    let _ = Command::cargo_bin("ggen")?
        .arg("sync")
        .current_dir(temp.path())
        .assert()
        .success();

    // Assert: .ggen/receipts/ directory exists and contains latest.json
    let receipts_dir = temp.path().join(".ggen").join("receipts");
    assert!(
        receipts_dir.exists(),
        ".ggen/receipts directory must exist after sync at {}",
        receipts_dir.display()
    );

    let latest_path = receipts_dir.join("latest.json");
    assert!(
        latest_path.exists(),
        "latest.json must exist in receipts directory at {}",
        latest_path.display()
    );

    let content = fs::read_to_string(&latest_path)?;
    assert!(!content.trim().is_empty(), "latest.json must not be empty");
    Ok(())
}

/// invariant_id: RCPT-02
/// Validates: receipt has non-empty signature
/// Failure class: Contract Drift (signature field is empty or absent)
#[test]
fn rcpt_02_receipt_has_non_empty_signature() -> TestResult {
    let temp = TempDir::new()?;
    setup_minimal_ggen_toml(&temp)?;
    setup_minimal_ttl(&temp)?;

    // Act: run sync
    let _ = Command::cargo_bin("ggen")?
        .arg("sync")
        .current_dir(temp.path())
        .assert()
        .success();

    // Assert: latest.json contains valid JSON with non-empty signature
    let receipt_path = temp
        .path()
        .join(".ggen")
        .join("receipts")
        .join("latest.json");
    let receipt_json = fs::read_to_string(&receipt_path)?;
    let receipt: serde_json::Value = serde_json::from_str(&receipt_json)?;

    let sig = receipt["signature"]
        .as_str()
        .expect("signature field must be a string");
    assert!(
        !sig.is_empty(),
        "receipt signature must not be empty, got: {:?}",
        sig
    );
    Ok(())
}

// ========================================================================
// Manifest Tests
// ========================================================================

/// invariant_id: MAN-01
/// Validates: ggen.toml contains [project] section
/// Failure class: Contract Drift (config schema changed but not validated)
#[test]
fn man_01_ggen_toml_contains_project_section() -> TestResult {
    let temp = TempDir::new()?;

    // Act: run init
    let _ = Command::cargo_bin("ggen")?
        .arg("init")
        .arg("test-proj")
        .current_dir(temp.path())
        .assert()
        .success();

    // Assert: read ggen.toml and verify [project] section
    let config_path = temp.path().join("ggen.toml");
    let content = fs::read_to_string(&config_path)?;

    // Parse as TOML to verify structure
    let config: toml::Value = toml::from_str(&content)?;
    assert!(
        config.get("project").is_some(),
        "ggen.toml must contain [project] section"
    );
    Ok(())
}

// ========================================================================
// Graph/Determinism Tests
// ========================================================================

/// invariant_id: GRAPH-01
/// Validates: two validate calls produce identical output
/// Failure class: Epistemic Bypass (non-deterministic validation)
#[test]
fn graph_01_deterministic_validate_output() -> TestResult {
    let temp = TempDir::new()?;
    setup_minimal_ttl(&temp)?;

    let ttl_path = temp.path().join("ontology.ttl");

    // Act: run validate twice
    let output1 = Command::cargo_bin("ggen")?
        .arg("graph")
        .arg("validate")
        .arg(&ttl_path)
        .output()?;

    let output2 = Command::cargo_bin("ggen")?
        .arg("graph")
        .arg("validate")
        .arg(&ttl_path)
        .output()?;

    // Assert: both outputs are identical
    assert!(
        output1.stdout == output2.stdout,
        "validate must produce identical output on consecutive runs"
    );
    Ok(())
}
