//! Comprehensive security tests for ggen-receipt Ed25519 signing.
//!
//! Chicago TDD: Real cryptographic operations, no mocks.
//! Tests cover:
//! - Ed25519 key generation and validation
//! - Signing and verification with real keys
//! - SHA-256 hash verification
//! - Receipt chain security
//! - Invalid signature rejection
//! - Tampered receipt detection
//! - Edge cases and error paths

use ggen_receipt::{generate_keypair, hash_data, Receipt, ReceiptChain};
use std::fs;
use tempfile::TempDir;

/// Test helper to create a signed receipt
fn create_test_receipt(operation_id: &str) -> (Receipt, ed25519_dalek::VerifyingKey) {
    let (signing_key, verifying_key) = generate_keypair();

    let receipt = Receipt::new(
        operation_id.to_string(),
        vec!["input-hash-1".to_string()],
        vec!["output-hash-1".to_string()],
        None,
    )
    .sign(&signing_key)
    .expect("signing failed");

    (receipt, verifying_key)
}

#[test]
fn test_ed25519_keypair_generation() {
    // Generate multiple keypairs to ensure uniqueness
    let keypair1 = generate_keypair();
    let keypair2 = generate_keypair();

    // Keypairs should be different (cryptographic randomness)
    let key1_bytes = keypair1.0.to_bytes();
    let key2_bytes = keypair2.0.to_bytes();
    assert_ne!(key1_bytes, key2_bytes, "Keypairs must be unique");

    // Verifying keys should also be different
    let vk1_bytes = keypair1.1.to_bytes();
    let vk2_bytes = keypair2.1.to_bytes();
    assert_ne!(vk1_bytes, vk2_bytes, "Verifying keys must be unique");
}

#[test]
fn test_ed25519_signing_produces_valid_signature() {
    let (receipt, verifying_key) = create_test_receipt("test-op");

    // Signature should be non-empty
    assert!(!receipt.signature.is_empty(), "Signature must not be empty");

    // Ed25519 signatures are 64 bytes
    let sig_bytes = hex::decode(&receipt.signature).expect("Invalid hex encoding");
    assert_eq!(sig_bytes.len(), 64, "Ed25519 signature must be 64 bytes");

    // Verification should succeed
    assert!(
        receipt.verify(&verifying_key).is_ok(),
        "Signature must verify"
    );
}

#[test]
fn test_ed25519_verification_fails_with_wrong_key() {
    let (receipt, _) = create_test_receipt("test-op");
    let (_, wrong_verifying_key) = generate_keypair();

    // Verification with wrong key should fail
    let result = receipt.verify(&wrong_verifying_key);
    assert!(result.is_err(), "Verification should fail with wrong key");

    let err = result.unwrap_err();
    assert_eq!(err.to_string(), "Signature verification failed");
}

#[test]
fn test_ed25519_verification_fails_with_tampered_signature() {
    let (mut receipt, verifying_key) = create_test_receipt("test-op");

    // Tamper with the signature (flip one bit)
    let mut sig_bytes = hex::decode(&receipt.signature).expect("Invalid hex");
    sig_bytes[0] ^= 0x01; // Flip first bit
    receipt.signature = hex::encode(sig_bytes);

    // Verification should fail
    let result = receipt.verify(&verifying_key);
    assert!(result.is_err(), "Tampered signature should not verify");
}

#[test]
fn test_ed25519_verification_fails_with_corrupted_signature_hex() {
    let (mut receipt, verifying_key) = create_test_receipt("test-op");

    // Corrupt the hex encoding
    receipt.signature = "not-valid-hex!!".to_string();

    // Verification should fail
    let result = receipt.verify(&verifying_key);
    assert!(result.is_err(), "Corrupted hex should not verify");
}

#[test]
fn test_sha256_hash_deterministic() {
    let data = b"test data for hashing";

    let hash1 = hash_data(data);
    let hash2 = hash_data(data);

    assert_eq!(hash1, hash2, "Hashing same data must produce same result");
    assert_eq!(hash1.len(), 64, "SHA-256 hex must be 64 characters");
}

#[test]
fn test_sha256_hash_avalanche_effect() {
    let data1 = b"test data";
    let data2 = b"test datf"; // One bit difference

    let hash1 = hash_data(data1);
    let hash2 = hash_data(data2);

    // Hashes should be completely different (avalanche effect)
    assert_ne!(hash1, hash2, "Different data must produce different hashes");

    // Count differing bits (should be ~50% for SHA-256)
    let bytes1 = hex::decode(&hash1).expect("Invalid hex");
    let bytes2 = hex::decode(&hash2).expect("Invalid hex");

    let mut differing_bits = 0;
    for (b1, b2) in bytes1.iter().zip(bytes2.iter()) {
        let xor = b1 ^ b2;
        differing_bits += xor.count_ones();
    }

    // At least 100 bits should differ (out of 256)
    assert!(
        differing_bits >= 100,
        "SHA-256 must have good avalanche effect"
    );
}

#[test]
fn test_sha256_hash_empty_input() {
    let hash = hash_data(b"");

    // Empty input has known SHA-256 hash
    assert_eq!(
        hash,
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    );
}

#[test]
fn test_receipt_hash_deterministic() {
    let (receipt, _) = create_test_receipt("test-op");

    let hash1 = receipt.hash().expect("hashing failed");
    let hash2 = receipt.hash().expect("hashing failed");

    assert_eq!(hash1, hash2, "Receipt hash must be deterministic");
    assert_eq!(hash1.len(), 64, "SHA-256 hash must be 64 hex chars");
}

#[test]
fn test_receipt_hash_includes_all_fields() {
    let (signing_key, _) = generate_keypair();

    let receipt1 = Receipt::new(
        "op-1".to_string(),
        vec!["input-1".to_string()],
        vec!["output-1".to_string()],
        None,
    )
    .sign(&signing_key)
    .expect("signing failed");

    let receipt2 = Receipt::new(
        "op-2".to_string(), // Different operation_id
        vec!["input-1".to_string()],
        vec!["output-1".to_string()],
        None,
    )
    .sign(&signing_key)
    .expect("signing failed");

    let hash1 = receipt1.hash().expect("hashing failed");
    let hash2 = receipt2.hash().expect("hashing failed");

    assert_ne!(
        hash1, hash2,
        "Different operation_id must produce different hash"
    );
}

#[test]
fn test_receipt_chain_verification_valid_chain() {
    let (signing_key, verifying_key) = generate_keypair();

    // Create genesis receipt
    let genesis = Receipt::new("genesis-op".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("signing failed");

    let mut chain = ReceiptChain::from_genesis(genesis.clone()).expect("genesis creation failed");

    // Add linked receipts
    let receipt2 = Receipt::new("second-op".to_string(), vec![], vec![], None)
        .chain(&genesis)
        .expect("chaining failed")
        .sign(&signing_key)
        .expect("signing failed");

    let receipt3 = Receipt::new("third-op".to_string(), vec![], vec![], None)
        .chain(&receipt2)
        .expect("chaining failed")
        .sign(&signing_key)
        .expect("signing failed");

    chain.append(receipt2).expect("append failed");
    chain.append(receipt3).expect("append failed");

    // Verify entire chain
    assert!(
        chain.verify(&verifying_key).is_ok(),
        "Valid chain must verify"
    );
    assert_eq!(chain.len(), 3, "Chain must have 3 receipts");
}

#[test]
fn test_receipt_chain_verification_fails_with_broken_link() {
    let (signing_key, verifying_key) = generate_keypair();

    let genesis = Receipt::new("genesis-op".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("signing failed");

    let mut chain = ReceiptChain::from_genesis(genesis).expect("genesis creation failed");

    // Create receipt with wrong previous hash
    let fake_receipt = Receipt::new(
        "fake-op".to_string(),
        vec![],
        vec![],
        Some("wrong_hash".to_string()),
    )
    .sign(&signing_key)
    .expect("signing failed");

    let result = chain.append(fake_receipt);
    assert!(result.is_err(), "Broken link should be rejected");

    let err = result.unwrap_err();
    assert!(
        err.to_string().contains("Hash mismatch"),
        "Error must indicate hash mismatch"
    );
}

#[test]
fn test_receipt_chain_verification_fails_with_wrong_signature() {
    let (signing_key, _) = generate_keypair();
    let (_, wrong_verifying_key) = generate_keypair();

    let genesis = Receipt::new("genesis-op".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("signing failed");

    let chain = ReceiptChain::from_genesis(genesis).expect("genesis creation failed");

    // Verify with wrong key should fail
    let result = chain.verify(&wrong_verifying_key);
    assert!(result.is_err(), "Chain with wrong verifying key must fail");
}

#[test]
fn test_receipt_chain_rejects_genesis_with_previous_hash() {
    let (signing_key, _) = generate_keypair();

    // Create invalid genesis (has previous hash)
    let invalid_genesis = Receipt::new(
        "genesis".to_string(),
        vec![],
        vec![],
        Some("some-previous-hash".to_string()),
    )
    .sign(&signing_key)
    .expect("signing failed");

    let result = ReceiptChain::from_genesis(invalid_genesis);
    assert!(
        result.is_err(),
        "Genesis with previous hash must be rejected"
    );
}

#[test]
fn test_receipt_serialization_preserves_signature() {
    let (receipt, verifying_key) = create_test_receipt("test-op");

    // Serialize
    let json = serde_json::to_string(&receipt).expect("serialization failed");

    // Deserialize
    let deserialized: Receipt = serde_json::from_str(&json).expect("deserialization failed");

    // Signature should be preserved
    assert_eq!(receipt.signature, deserialized.signature);

    // Verification should still work
    assert!(
        deserialized.verify(&verifying_key).is_ok(),
        "Deserialized receipt must verify"
    );
}

#[test]
fn test_receipt_serialization_preserves_chain_links() {
    let (signing_key, _) = generate_keypair();

    let receipt1 = Receipt::new("op1".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("signing failed");

    let receipt2 = Receipt::new("op2".to_string(), vec![], vec![], None)
        .chain(&receipt1)
        .expect("chaining failed")
        .sign(&signing_key)
        .expect("signing failed");

    // Serialize and deserialize
    let json = serde_json::to_string(&receipt2).expect("serialization failed");
    let deserialized: Receipt = serde_json::from_str(&json).expect("deserialization failed");

    assert_eq!(
        receipt2.previous_receipt_hash, deserialized.previous_receipt_hash,
        "Chain links must be preserved"
    );
}

#[test]
fn test_receipt_persistence_to_file() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let receipt_path = temp_dir.path().join("receipt.json");

    let (receipt, verifying_key) = create_test_receipt("persistent-op");

    // Write to file
    let json = serde_json::to_string_pretty(&receipt).expect("serialization failed");
    fs::write(&receipt_path, json).expect("write failed");

    // Read from file
    let read_json = fs::read_to_string(&receipt_path).expect("read failed");
    let loaded: Receipt = serde_json::from_str(&read_json).expect("deserialization failed");

    // Verify loaded receipt
    assert_eq!(receipt.signature, loaded.signature);
    assert!(
        loaded.verify(&verifying_key).is_ok(),
        "Loaded receipt must verify"
    );
}

#[test]
fn test_multiple_signatures_with_same_key() {
    let (signing_key, verifying_key) = generate_keypair();

    // Sign multiple receipts with same key
    let receipt1 = Receipt::new("op1".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("signing failed");

    let receipt2 = Receipt::new("op2".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("signing failed");

    // Signatures should be different (different data)
    assert_ne!(
        receipt1.signature, receipt2.signature,
        "Different data must produce different signatures"
    );

    // Both should verify
    assert!(receipt1.verify(&verifying_key).is_ok());
    assert!(receipt2.verify(&verifying_key).is_ok());
}

#[test]
fn test_signature_includes_all_fields() {
    let (signing_key, verifying_key) = generate_keypair();

    let receipt1 = Receipt::new(
        "op".to_string(),
        vec!["input1".to_string()],
        vec!["output1".to_string()],
        None,
    )
    .sign(&signing_key)
    .expect("signing failed");

    let receipt2 = Receipt::new(
        "op".to_string(),
        vec!["input2".to_string()], // Different input
        vec!["output1".to_string()],
        None,
    )
    .sign(&signing_key)
    .expect("signing failed");

    // Signatures must be different
    assert_ne!(
        receipt1.signature, receipt2.signature,
        "Signature must depend on all fields"
    );
}

#[test]
fn test_empty_receipt_can_be_signed() {
    let (signing_key, verifying_key) = generate_keypair();

    let receipt = Receipt::new("empty-op".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("signing failed");

    assert!(
        receipt.verify(&verifying_key).is_ok(),
        "Empty receipt must verify"
    );
}

#[test]
fn test_large_input_output_hashes() {
    let (signing_key, verifying_key) = generate_keypair();

    // Create receipt with many hashes
    let inputs: Vec<String> = (0..100).map(|i| format!("input-hash-{}", i)).collect();
    let outputs: Vec<String> = (0..100).map(|i| format!("output-hash-{}", i)).collect();

    let receipt = Receipt::new(
        "large-op".to_string(),
        inputs.clone(),
        outputs.clone(),
        None,
    )
    .sign(&signing_key)
    .expect("signing failed");

    assert!(
        receipt.verify(&verifying_key).is_ok(),
        "Large receipt must verify"
    );
    assert_eq!(receipt.input_hashes.len(), 100);
    assert_eq!(receipt.output_hashes.len(), 100);
}
