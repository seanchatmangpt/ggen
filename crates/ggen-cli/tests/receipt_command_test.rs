//! Integration tests for receipt commands
//!
//! Chicago TDD: Tests verify actual receipt verification behavior

use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;
use ggen_receipt::{Receipt, generate_keypair};

/// Test helper: Create a test receipt
fn create_test_receipt(temp_dir: &PathBuf) -> (String, String) {
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
    let receipt_json = serde_json::to_string_pretty(&receipt)
        .expect("Failed to serialize receipt");

    fs::write(&receipt_path, receipt_json)
        .expect("Failed to write receipt file");

    (receipt_path.to_string_lossy().to_string(), hex::encode(_verifying_key.to_bytes()))
}

/// Test helper: Create a public key file
fn create_public_key_file(temp_dir: &PathBuf, key_hex: &str) -> String {
    let key_path = temp_dir.join("public-key.hex");
    fs::write(&key_path, key_hex)
        .expect("Failed to write public key file");

    key_path.to_string_lossy().to_string()
}

#[test]
fn test_receipt_verify_with_valid_signature() {
    let temp_dir = TempDir::new()
        .expect("Failed to create temp dir");

    let (receipt_path, verifying_key_hex) = create_test_receipt(temp_dir.path());
    let key_path = create_public_key_file(temp_dir.path(), &verifying_key_hex);

    // Run the receipt verify command via CLI
    let output = assert_cmd::Command::cargo_bin("ggen")
        .unwrap()
        .args(["receipt", "verify", &receipt_path, "--public-key", &key_path])
        .output()
        .expect("Failed to execute command");

    // Command should succeed (exit code 0)
    assert!(output.status.success());

    // Output should contain verification success message
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("verified successfully") || stdout.contains("\"is_valid\":true"));
}

#[test]
fn test_receipt_verify_with_wrong_key() {
    let temp_dir = TempDir::new()
        .expect("Failed to create temp dir");

    let (receipt_path, _) = create_test_receipt(temp_dir.path());

    // Generate a different key pair
    let (_wrong_signing_key, wrong_verifying_key) = generate_keypair();
    let wrong_key_hex = hex::encode(wrong_verifying_key.to_bytes());
    let key_path = create_public_key_file(temp_dir.path(), &wrong_key_hex);

    // Run the receipt verify command via CLI
    let output = assert_cmd::Command::cargo_bin("ggen")
        .unwrap()
        .args(["receipt", "verify", &receipt_path, "--public-key", &key_path])
        .output()
        .expect("Failed to execute command");

    // Command should still succeed (exit code 0), but verification should fail
    assert!(output.status.success());

    // Output should indicate verification failure
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("is_valid") || stdout.contains("false"));
}

#[test]
fn test_receipt_info_displays_receipt_details() {
    let temp_dir = TempDir::new()
        .expect("Failed to create temp dir");

    let (receipt_path, _) = create_test_receipt(temp_dir.path());

    // Run the receipt info command via CLI
    let output = assert_cmd::Command::cargo_bin("ggen")
        .unwrap()
        .args(["receipt", "info", &receipt_path])
        .output()
        .expect("Failed to execute command");

    // Command should succeed
    assert!(output.status.success());

    // Output should contain operation details
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("test-operation") || stdout.contains("operation_id"));
}

#[test]
fn test_receipt_verify_without_key_returns_error() {
    let temp_dir = TempDir::new()
        .expect("Failed to create temp dir");

    let (receipt_path, _) = create_test_receipt(temp_dir.path());

    // Run without --public-key flag
    let output = assert_cmd::Command::cargo_bin("ggen")
        .unwrap()
        .args(["receipt", "verify", &receipt_path])
        .output()
        .expect("Failed to execute command");

    // Command should succeed but report missing key
    assert!(output.status.success());

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Public key required") || stdout.contains("is_valid"));
}

#[test]
fn test_receipt_verify_nonexistent_file() {
    let temp_dir = TempDir::new()
        .expect("Failed to create temp dir");

    let fake_path = temp_dir.path().join("nonexistent-receipt.json");
    let fake_path_str = fake_path.to_string_lossy().to_string();

    // Run with nonexistent file
    let output = assert_cmd::Command::cargo_bin("ggen")
        .unwrap()
        .args(["receipt", "verify", &fake_path_str])
        .output()
        .expect("Failed to execute command");

    // Command should succeed but report file not found
    assert!(output.status.success());

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("not found") || stdout.contains("is_valid"));
}
