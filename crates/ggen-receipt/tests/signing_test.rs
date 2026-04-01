//! Comprehensive tests for Ed25519 signing and receipt verification.
//!
//! Tests cover:
//! - Ed25519 key generation
//! - Signing and verification
//! - SHA-256 hash verification
//! - Receipt chain verification
//! - Invalid signatures fail verification
//! - Tampered receipts fail verification

use ggen_receipt::{generate_keypair, hash_data, Receipt, ReceiptChain};
use tempfile::TempDir;

/// Test Ed25519 key generation produces valid keypairs.
#[test]
fn test_ed25519_key_generation() {
    let (signing_key, verifying_key) = generate_keypair();

    // Keys should be 32 bytes
    assert_eq!(signing_key.to_bytes().len(), 32);
    assert_eq!(verifying_key.to_bytes().len(), 32);

    // Verify key is derived correctly
    let derived_verifying = signing_key.verifying_key();
    assert_eq!(derived_verifying.to_bytes(), verifying_key.to_bytes());
}

/// Test signing and verification with valid keypair.
#[test]
fn test_signing_and_verification() {
    let (signing_key, verifying_key) = generate_keypair();

    let receipt = Receipt::new(
        "test-operation".to_string(),
        vec!["input-hash-1".to_string()],
        vec!["output-hash-1".to_string()],
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    // Signature should be non-empty
    assert!(!receipt.signature.is_empty());
    assert_eq!(receipt.signature.len(), 128); // Ed25519 signature is 64 bytes = 128 hex chars

    // Verification should succeed
    assert!(receipt.verify(&verifying_key).is_ok());
}

/// Test verification fails with wrong verifying key.
#[test]
fn test_verification_fails_with_wrong_key() {
    let (signing_key, _) = generate_keypair();
    let (_, wrong_verifying_key) = generate_keypair();

    let receipt = Receipt::new(
        "test-operation".to_string(),
        vec!["input-hash-1".to_string()],
        vec!["output-hash-1".to_string()],
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    // Verification should fail with wrong key
    assert!(receipt.verify(&wrong_verifying_key).is_err());
}

/// Test SHA-256 hash verification produces consistent results.
#[test]
fn test_sha256_hash_verification() {
    let data = b"test data for hashing";

    let hash1 = hash_data(data);
    let hash2 = hash_data(data);

    // Hashes should be identical
    assert_eq!(hash1, hash2);

    // SHA-256 produces 32 bytes = 64 hex characters
    assert_eq!(hash1.len(), 64);

    // Different data should produce different hash
    let hash3 = hash_data(b"different data");
    assert_ne!(hash1, hash3);
}

/// Test receipt hash computation is deterministic.
#[test]
fn test_receipt_hash_deterministic() {
    let receipt = Receipt::new(
        "test-operation".to_string(),
        vec!["input-1".to_string(), "input-2".to_string()],
        vec!["output-1".to_string()],
        None,
    );

    let hash1 = receipt.hash().expect("Hash computation failed");
    let hash2 = receipt.hash().expect("Hash computation failed");

    assert_eq!(hash1, hash2);
    assert_eq!(hash1.len(), 64);
}

/// Test receipt chain verification with valid chain.
#[test]
fn test_receipt_chain_verification_valid() {
    let (signing_key, verifying_key) = generate_keypair();

    // Create genesis receipt
    let genesis = Receipt::new(
        "genesis-operation".to_string(),
        vec![],
        vec![],
        None,
    )
    .sign(&signing_key)
    .expect("Genesis signing failed");

    let mut chain = ReceiptChain::from_genesis(genesis.clone())
        .expect("Chain creation failed");

    // Add second receipt
    let receipt2 = Receipt::new(
        "second-operation".to_string(),
        vec![],
        vec![],
        None,
    )
    .chain(&genesis)
    .expect("Chaining failed")
    .sign(&signing_key)
    .expect("Signing failed");

    chain.append(receipt2).expect("Append failed");

    // Add third receipt
    let receipt3 = Receipt::new(
        "third-operation".to_string(),
        vec![],
        vec![],
        None,
    )
    .chain(chain.last().unwrap())
    .expect("Chaining failed")
    .sign(&signing_key)
    .expect("Signing failed");

    chain.append(receipt3).expect("Append failed");

    // Verify entire chain
    assert!(chain.verify(&verifying_key).is_ok());
    assert_eq!(chain.len(), 3);
}

/// Test receipt chain verification fails with broken link.
#[test]
fn test_receipt_chain_verification_broken_link() {
    let (signing_key, verifying_key) = generate_keypair();

    // Create genesis receipt
    let genesis = Receipt::new(
        "genesis-operation".to_string(),
        vec![],
        vec![],
        None,
    )
    .sign(&signing_key)
    .expect("Genesis signing failed");

    let mut chain = ReceiptChain::from_genesis(genesis)
        .expect("Chain creation failed");

    // Create receipt with wrong previous hash
    let bad_receipt = Receipt::new(
        "bad-operation".to_string(),
        vec![],
        vec![],
        Some("wrong_hash_1234567890abcdef".to_string()),
    )
    .sign(&signing_key)
    .expect("Signing failed");

    // Should fail to append
    assert!(chain.append(bad_receipt).is_err());
}

/// Test invalid signature fails verification.
#[test]
fn test_invalid_signature_fails_verification() {
    let (signing_key, verifying_key) = generate_keypair();

    let mut receipt = Receipt::new(
        "test-operation".to_string(),
        vec!["input-hash".to_string()],
        vec!["output-hash".to_string()],
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    // Tamper with signature
    receipt.signature = "0".repeat(128); // Invalid signature

    assert!(receipt.verify(&verifying_key).is_err());
}

/// Test tampered receipt fails verification.
#[test]
fn test_tampered_receipt_fails_verification() {
    let (signing_key, verifying_key) = generate_keypair();

    let mut receipt = Receipt::new(
        "test-operation".to_string(),
        vec!["input-hash".to_string()],
        vec!["output-hash".to_string()],
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    // Tamper with operation_id after signing
    receipt.operation_id = "tampered-operation".to_string();

    // Verification should fail because signature doesn't match tampered data
    assert!(receipt.verify(&verifying_key).is_err());
}

/// Test tampered input hashes fail verification.
#[test]
fn test_tampered_input_hashes_fail_verification() {
    let (signing_key, verifying_key) = generate_keypair();

    let mut receipt = Receipt::new(
        "test-operation".to_string(),
        vec!["original-input-hash".to_string()],
        vec!["output-hash".to_string()],
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    // Tamper with input hashes
    receipt.input_hashes = vec!["tampered-input-hash".to_string()];

    // Verification should fail
    assert!(receipt.verify(&verifying_key).is_err());
}

/// Test tampered output hashes fail verification.
#[test]
fn test_tampered_output_hashes_fail_verification() {
    let (signing_key, verifying_key) = generate_keypair();

    let mut receipt = Receipt::new(
        "test-operation".to_string(),
        vec!["input-hash".to_string()],
        vec!["original-output-hash".to_string()],
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    // Tamper with output hashes
    receipt.output_hashes = vec!["tampered-output-hash".to_string()];

    // Verification should fail
    assert!(receipt.verify(&verifying_key).is_err());
}

/// Test receipt chain verification fails with tampered intermediate receipt.
#[test]
fn test_chain_verification_tampered_intermediate_receipt() {
    let (signing_key, verifying_key) = generate_keypair();

    // Create genesis
    let genesis = Receipt::new("op1".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("Signing failed");

    // Create second receipt
    let mut receipt2 = Receipt::new("op2".to_string(), vec![], vec![], None)
        .chain(&genesis)
        .expect("Chaining failed")
        .sign(&signing_key)
        .expect("Signing failed");

    // Tamper with second receipt
    receipt2.operation_id = "tampered-op2".to_string();

    // Create third receipt (links to tampered receipt)
    let receipt3 = Receipt::new("op3".to_string(), vec![], vec![], None)
        .chain(&receipt2)
        .expect("Chaining failed")
        .sign(&signing_key)
        .expect("Signing failed");

    let mut chain = ReceiptChain::from_genesis(genesis)
        .expect("Chain creation failed");

    // This should fail because receipt2 signature is invalid
    assert!(chain.append(receipt2).is_err());
}

/// Test empty chain verification.
#[test]
fn test_empty_chain_verification() {
    let (_, verifying_key) = generate_keypair();
    let chain = ReceiptChain::new();

    // Empty chain should verify successfully
    assert!(chain.verify(&verifying_key).is_ok());
    assert!(chain.is_empty());
    assert_eq!(chain.len(), 0);
}

/// Test receipt serialization and deserialization.
#[test]
fn test_receipt_serialization() {
    let (signing_key, verifying_key) = generate_keypair();

    let original = Receipt::new(
        "test-operation".to_string(),
        vec!["input-1".to_string()],
        vec!["output-1".to_string()],
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    // Serialize to JSON
    let json = serde_json::to_string(&original).expect("Serialization failed");

    // Deserialize
    let deserialized: Receipt = serde_json::from_str(&json).expect("Deserialization failed");

    // Should be identical
    assert_eq!(original, deserialized);

    // Deserialized receipt should verify
    assert!(deserialized.verify(&verifying_key).is_ok());
}

/// Test chain serialization and deserialization.
#[test]
fn test_chain_serialization() {
    let (signing_key, verifying_key) = generate_keypair();

    let genesis = Receipt::new("op1".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("Signing failed");

    let mut chain = ReceiptChain::from_genesis(genesis.clone())
        .expect("Chain creation failed");

    let receipt2 = Receipt::new("op2".to_string(), vec![], vec![], None)
        .chain(&genesis)
        .expect("Chaining failed")
        .sign(&signing_key)
        .expect("Signing failed");

    chain.append(receipt2).expect("Append failed");

    // Serialize to JSON
    let json = serde_json::to_string(&chain).expect("Serialization failed");

    // Deserialize
    let deserialized: ReceiptChain = serde_json::from_str(&json).expect("Deserialization failed");

    // Should verify
    assert!(deserialized.verify(&verifying_key).is_ok());
    assert_eq!(deserialized.len(), 2);
}

/// Test hash_data with empty input.
#[test]
fn test_hash_data_empty() {
    let empty_data = b"";
    let hash = hash_data(empty_data);

    assert_eq!(hash.len(), 64);
    assert!(!hash.is_empty());
}

/// Test hash_data with large input.
#[test]
fn test_hash_data_large_input() {
    let large_data = vec![0u8; 10_000_000]; // 10 MB
    let hash1 = hash_data(&large_data);
    let hash2 = hash_data(&large_data);

    assert_eq!(hash1, hash2);
    assert_eq!(hash1.len(), 64);
}

/// Test multiple receipts with same content have different timestamps.
#[test]
fn test_receipts_different_timestamps() {
    let (signing_key, _) = generate_keypair();

    let receipt1 = Receipt::new(
        "test-operation".to_string(),
        vec!["input".to_string()],
        vec!["output".to_string()],
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    // Small delay to ensure different timestamp
    std::thread::sleep(std::time::Duration::from_millis(10));

    let receipt2 = Receipt::new(
        "test-operation".to_string(),
        vec!["input".to_string()],
        vec!["output".to_string()],
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    // Timestamps should differ
    assert_ne!(receipt1.timestamp, receipt2.timestamp);

    // Signatures should differ due to different timestamps
    assert_ne!(receipt1.signature, receipt2.signature);
}

/// Test receipt with multiple input and output hashes.
#[test]
fn test_receipt_multiple_hashes() {
    let (signing_key, verifying_key) = generate_keypair();

    let receipt = Receipt::new(
        "complex-operation".to_string(),
        vec!["input-1".to_string(), "input-2".to_string(), "input-3".to_string()],
        vec!["output-1".to_string(), "output-2".to_string()],
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    assert_eq!(receipt.input_hashes.len(), 3);
    assert_eq!(receipt.output_hashes.len(), 2);
    assert!(receipt.verify(&verifying_key).is_ok());
}

/// Test chain accessor methods.
#[test]
fn test_chain_accessors() {
    let (signing_key, _) = generate_keypair();

    let genesis = Receipt::new("op1".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("Signing failed");

    let chain = ReceiptChain::from_genesis(genesis.clone())
        .expect("Chain creation failed");

    assert_eq!(chain.genesis().unwrap().operation_id, "op1");
    assert_eq!(chain.last().unwrap().operation_id, "op1");
    assert_eq!(chain.receipts().len(), 1);
    assert!(!chain.is_empty());
}
