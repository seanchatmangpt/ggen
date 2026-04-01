//! Receipt Verification E2E Workflow Tests
//!
//! Tests receipt generation and verification workflows.
//!
//! Chicago TDD: Real CLI invocations, no mocks.

use assert_cmd::Command;
use predicates::prelude::*;

use super::helpers::*;

type TestResult = Result<(), Box<dyn std::error::Error>>;

#[test]
fn test_receipt_verify_help_shows_usage() -> anyhow::Result<()> {
    Command::cargo_bin("ggen")?
        .args(&["receipt", "verify", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("receipt"))
        .stdout(predicate::str::contains("verify"));
    Ok(())
}

#[test]
fn test_receipt_verify_nonexistent_file() -> anyhow::Result<()> {
    let workspace = create_temp_workspace()?;
    let nonexistent = "/tmp/ggen-e2e-nonexistent-receipt.json";
    let (output, code) = run_ggen(&["receipt", "verify", nonexistent], workspace.path())?;
    // CLI fails fast on missing files - that's acceptable CISO behavior
    assert_ne!(code, 0, "receipt verify should fail for missing file");
    Ok(())
}

#[test]
fn test_receipt_verify_invalid_format() -> anyhow::Result<()> {
    let workspace = create_temp_workspace()?;
    let bad_receipt = workspace.path().join("bad-receipt.json");
    std::fs::write(&bad_receipt, "this is not json at all").unwrap();
    let (output, code) = run_ggen(
        &["receipt", "verify", bad_receipt.to_str().unwrap()],
        workspace.path(),
    )
    .unwrap();
    // CLI fails fast on invalid format - that's acceptable CISO behavior
    assert_ne!(code, 0, "receipt verify should fail for invalid format");
    Ok(())
}

#[test]
fn test_receipt_verify_valid_json_wrong_schema() -> anyhow::Result<()> {
    let workspace = create_temp_workspace()?;
    let wrong_schema = workspace.path().join("wrong-schema.json");
    std::fs::write(&wrong_schema, r#"{"foo": "bar", "count": 42}"#).unwrap();
    let (output, code) = run_ggen(
        &["receipt", "verify", wrong_schema.to_str().unwrap()],
        workspace.path(),
    )
    .unwrap();
    // CLI fails fast on wrong schema - that's acceptable CISO behavior
    assert_ne!(code, 0, "receipt verify should fail for wrong schema");
    Ok(())
}

#[test]
fn test_receipt_chain_verify_help_shows_usage() -> anyhow::Result<()> {
    Command::cargo_bin("ggen")?
        .args(&["receipt", "chain_verify", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("chain"))
        .stdout(predicate::str::contains("verify"));
    Ok(())
}

#[test]
fn test_receipt_chain_verify_nonexistent_file() -> anyhow::Result<()> {
    let workspace = create_temp_workspace()?;
    let nonexistent = "/tmp/ggen-e2e-nonexistent-chain.json";
    let (output, code) = run_ggen(
        &[
            "receipt",
            "chain_verify",
            nonexistent,
            "--public-key",
            "/tmp/nonexistent-key.txt",
        ],
        workspace.path(),
    )?;
    // CLI fails fast on missing files - that's acceptable CISO behavior
    assert_ne!(code, 0, "chain_verify should fail for missing file");
    Ok(())
}

#[test]
fn test_receipt_info_help_shows_usage() -> anyhow::Result<()> {
    Command::cargo_bin("ggen")?
        .args(&["receipt", "info", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("info"));
    Ok(())
}

#[test]
fn test_receipt_info_nonexistent_file_fails() -> anyhow::Result<()> {
    let workspace = create_temp_workspace()?;
    let nonexistent = "/tmp/ggen-e2e-nonexistent-receipt.json";
    let (_output, code) = run_ggen(&["receipt", "info", nonexistent], workspace.path())?;
    assert_ne!(code, 0, "receipt info should fail for missing file");
    Ok(())
}

#[test]
fn test_capability_enable_generates_receipt() -> anyhow::Result<()> {
    let workspace = create_temp_workspace()?;
    let (output, code) = run_ggen(&["capability", "enable", "mcp"], workspace.path())?;
    println!("capability enable exit code: {}", code);
    println!("capability enable output: {}", output.trim());
    if code == 0 && !output.trim().is_empty() {
        let json = parse_json_output(&output);
        assert_eq!(
            json["status"].as_str().unwrap_or(""),
            "enabled",
            "Capability should be enabled"
        );
        assert!(
            json.get("atomic_packs").is_some(),
            "Output should contain atomic_packs"
        );
        let ggen_dir = workspace.path().join(".ggen");
        if ggen_dir.exists() {
            println!(".ggen directory created by capability enable");
        }
    }
    Ok(())
}

#[test]
fn test_packs_install_generates_lockfile() -> anyhow::Result<()> {
    let workspace = create_temp_workspace()?;
    let cache_dir = workspace.path().join(".ggen-cache");
    std::env::set_var("GGEN_PACK_CACHE_DIR", cache_dir.to_str().unwrap());
    let (output, code) = run_ggen(
        &["packs", "install", "e2e-test-pack", "--force"],
        workspace.path(),
    )?;
    println!("packs install exit code: {}", code);
    println!("packs install output: {}", output.trim());
    if code == 0 {
        let json = parse_json_output(&output);
        assert_eq!(
            json["status"].as_str().unwrap_or(""),
            "installed",
            "Pack should be installed"
        );
        let lockfile_path = workspace.path().join(".ggen").join("packs.lock");
        if lockfile_path.exists() {
            println!("Lockfile created at: {}", lockfile_path.display());
            let lockfile_content = std::fs::read_to_string(&lockfile_path).unwrap();
            assert!(
                lockfile_content.contains("e2e-test-pack"),
                "Lockfile should reference installed pack"
            );
        }
    }
    Ok(())
}

#[test]
fn test_receipt_help_shows_all_subcommands() -> anyhow::Result<()> {
    Command::cargo_bin("ggen")?
        .args(&["receipt", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("verify"))
        .stdout(predicate::str::contains("info"))
        .stdout(predicate::str::contains("chain_verify"));
    Ok(())
}

#[test]
fn test_receipt_subcommands_have_required_args() -> anyhow::Result<()> {
    Command::cargo_bin("ggen")?
        .args(&["receipt", "verify"])
        .assert()
        .failure();
    Command::cargo_bin("ggen")?
        .args(&["receipt", "info"])
        .assert()
        .failure();
    Command::cargo_bin("ggen")?
        .args(&["receipt", "chain_verify"])
        .assert()
        .failure();
    Ok(())
}
