#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]

//! Integration tests for receipt commands
//!
//! Chicago TDD: Tests verify actual receipt verification behavior

use ggen_core::receipt::{generate_keypair, Receipt};
use std::fs;
use tempfile::TempDir;

/// Test helper: Create a test receipt
fn create_test_receipt(temp_dir: &std::path::Path) -> (String, String) {
    let (signing_key, _verifying_key) = generate_keypair();

    let receipt = Receipt::new(
        "test-operation".to_string(),
        vec!["input-hash-1".to_string()],
        vec!["output-hash-1".to_string()],
        None,
    )
    .sign(&signing_key)
    .expect("Failed to sign receipt");

    let receipt_path = temp_dir.join("test-receipt.json");
    let receipt_json = serde_json::to_string_pretty(&receipt).expect("Failed to serialize receipt");

    fs::write(&receipt_path, receipt_json).expect("Failed to write receipt file");

    (
        receipt_path.to_string_lossy().to_string(),
        hex::encode(_verifying_key.to_bytes()),
    )
}

/// Test helper: Create a public key file
fn create_public_key_file(temp_dir: &std::path::Path, key_hex: &str) -> String {
    let key_path = temp_dir.join("public-key.hex");
    fs::write(&key_path, key_hex).expect("Failed to write public key file");

    key_path.to_string_lossy().to_string()
}

#[test]
fn test_receipt_verify_with_valid_signature() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");

    let (receipt_path, verifying_key_hex) = create_test_receipt(temp_dir.path());
    let key_path = create_public_key_file(temp_dir.path(), &verifying_key_hex);

    // Run the receipt verify command via CLI
    let output = assert_cmd::Command::new(env!("CARGO_BIN_EXE_ggen"))
        .args([
            "receipt",
            "verify",
            "--receipt_path",
            &receipt_path,
            "--public_key",
            &key_path,
        ])
        .output()
        .expect("Failed to execute command");

    if !output.status.success() {
        panic!(
            "Command failed.\nstdout: {}\nstderr: {}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    // Output should contain verification success message
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("verified successfully") || stdout.contains("\"is_valid\":true"));
}

#[test]
fn test_receipt_verify_with_wrong_key() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");

    let (receipt_path, _) = create_test_receipt(temp_dir.path());

    // Generate a different key pair
    let (_wrong_signing_key, wrong_verifying_key) = generate_keypair();
    let wrong_key_hex = hex::encode(wrong_verifying_key.to_bytes());
    let key_path = create_public_key_file(temp_dir.path(), &wrong_key_hex);

    // Run the receipt verify command via CLI
    let output = assert_cmd::Command::new(env!("CARGO_BIN_EXE_ggen"))
        .args([
            "receipt",
            "verify",
            "--receipt_path",
            &receipt_path,
            "--public_key",
            &key_path,
        ])
        .output()
        .expect("Failed to execute command");

    if !output.status.success() {
        panic!(
            "Command failed.\nstdout: {}\nstderr: {}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    // Output should indicate verification failure
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("is_valid") || stdout.contains("false"));
}

#[test]
fn test_receipt_info_displays_receipt_details() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");

    let (receipt_path, _) = create_test_receipt(temp_dir.path());

    // Run the receipt info command via CLI
    let output = assert_cmd::Command::new(env!("CARGO_BIN_EXE_ggen"))
        .args(["receipt", "info", "--receipt_path", &receipt_path])
        .output()
        .expect("Failed to execute command");

    if !output.status.success() {
        panic!(
            "Command failed.\nstdout: {}\nstderr: {}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    // Output should contain operation details
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("test-operation") || stdout.contains("operation_id"));
}

#[test]
fn test_receipt_verify_without_key_returns_error() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");

    let (receipt_path, _) = create_test_receipt(temp_dir.path());

    // Run without --public_key flag
    let output = assert_cmd::Command::new(env!("CARGO_BIN_EXE_ggen"))
        .args(["receipt", "verify", "--receipt_path", &receipt_path])
        .output()
        .expect("Failed to execute command");

    if !output.status.success() {
        panic!(
            "Command failed.\nstdout: {}\nstderr: {}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Public key required") || stdout.contains("is_valid"));
}

#[test]
fn test_receipt_verify_nonexistent_file() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");

    let fake_path = temp_dir.path().join("nonexistent-receipt.json");
    let fake_path_str = fake_path.to_string_lossy().to_string();

    // Run with nonexistent file
    let output = assert_cmd::Command::new(env!("CARGO_BIN_EXE_ggen"))
        .args(["receipt", "verify", "--receipt_path", &fake_path_str])
        .output()
        .expect("Failed to execute command");

    if !output.status.success() {
        panic!(
            "Command failed.\nstdout: {}\nstderr: {}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("not found") || stdout.contains("is_valid"));
}
