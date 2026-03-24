//! Comprehensive tests for receipt signing and verification.

use ggen_receipt::{generate_keypair, hash_data, Receipt};

#[test]
fn test_receipt_new() {
    let receipt = Receipt::new(
        "test-operation".to_string(),
        vec!["in1".to_string(), "in2".to_string()],
        vec!["out1".to_string()],
        None,
    );

    assert_eq!(receipt.operation_id, "test-operation");
    assert_eq!(receipt.input_hashes.len(), 2);
    assert_eq!(receipt.output_hashes.len(), 1);
    assert!(receipt.previous_receipt_hash.is_none());
    assert!(receipt.signature.is_empty());
}

#[test]
fn test_receipt_signing() {
    let (signing_key, _) = generate_keypair();

    let receipt = Receipt::new(
        "op1".to_string(),
        vec!["input".to_string()],
        vec!["output".to_string()],
        None,
    );

    let signed = receipt.sign(&signing_key).expect("Signing should succeed");

    assert!(!signed.signature.is_empty());
    assert_eq!(signed.signature.len(), 128); // Ed25519 signature is 64 bytes = 128 hex chars
}

#[test]
fn test_receipt_verification_success() {
    let (signing_key, verifying_key) = generate_keypair();

    let receipt = Receipt::new(
        "op1".to_string(),
        vec!["input".to_string()],
        vec!["output".to_string()],
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    assert!(receipt.verify(&verifying_key).is_ok());
}

#[test]
fn test_receipt_verification_fails_wrong_key() {
    let (signing_key, _) = generate_keypair();
    let (_, wrong_verifying_key) = generate_keypair();

    let receipt = Receipt::new(
        "op1".to_string(),
        vec!["input".to_string()],
        vec!["output".to_string()],
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    assert!(receipt.verify(&wrong_verifying_key).is_err());
}

#[test]
fn test_receipt_verification_fails_tampered_data() {
    let (signing_key, verifying_key) = generate_keypair();

    let mut receipt = Receipt::new(
        "op1".to_string(),
        vec!["input".to_string()],
        vec!["output".to_string()],
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    // Tamper with the data
    receipt.operation_id = "tampered".to_string();

    assert!(receipt.verify(&verifying_key).is_err());
}

#[test]
fn test_receipt_hashing() {
    let receipt = Receipt::new(
        "op1".to_string(),
        vec!["input".to_string()],
        vec!["output".to_string()],
        None,
    );

    let hash1 = receipt.hash().expect("Hashing failed");
    let hash2 = receipt.hash().expect("Hashing failed");

    assert_eq!(hash1, hash2);
    assert_eq!(hash1.len(), 64); // SHA-256 = 64 hex chars
}

#[test]
fn test_receipt_hash_deterministic() {
    let receipt1 = Receipt::new(
        "op1".to_string(),
        vec!["input".to_string()],
        vec!["output".to_string()],
        None,
    );

    let receipt2 = Receipt::new(
        "op1".to_string(),
        vec!["input".to_string()],
        vec!["output".to_string()],
        None,
    );

    // Different receipts with same data should not have same hash due to timestamp
    let hash1 = receipt1.hash().expect("Hashing failed");
    let hash2 = receipt2.hash().expect("Hashing failed");

    // Timestamps will differ, so hashes should differ
    assert_ne!(hash1, hash2);
}

#[test]
fn test_receipt_chaining() {
    let (signing_key, _) = generate_keypair();

    let receipt1 = Receipt::new(
        "op1".to_string(),
        vec!["input1".to_string()],
        vec!["output1".to_string()],
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    let receipt2 = Receipt::new(
        "op2".to_string(),
        vec!["input2".to_string()],
        vec!["output2".to_string()],
        None,
    )
    .chain(&receipt1)
    .expect("Chaining failed")
    .sign(&signing_key)
    .expect("Signing failed");

    let receipt1_hash = receipt1.hash().expect("Hashing failed");

    assert!(receipt2.previous_receipt_hash.is_some());
    assert_eq!(receipt2.previous_receipt_hash.as_ref().unwrap(), &receipt1_hash);
}

#[test]
fn test_hash_data_utility() {
    let data1 = b"test data";
    let data2 = b"test data";
    let data3 = b"different data";

    let hash1 = hash_data(data1);
    let hash2 = hash_data(data2);
    let hash3 = hash_data(data3);

    assert_eq!(hash1, hash2);
    assert_ne!(hash1, hash3);
    assert_eq!(hash1.len(), 64);
}

#[test]
fn test_receipt_serialization() {
    let (signing_key, _) = generate_keypair();

    let receipt = Receipt::new(
        "op1".to_string(),
        vec!["input".to_string()],
        vec!["output".to_string()],
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    let json = serde_json::to_string(&receipt).expect("Serialization failed");
    let deserialized: Receipt = serde_json::from_str(&json).expect("Deserialization failed");

    assert_eq!(receipt.operation_id, deserialized.operation_id);
    assert_eq!(receipt.signature, deserialized.signature);
}

#[test]
fn test_multiple_inputs_outputs() {
    let (signing_key, verifying_key) = generate_keypair();

    let inputs: Vec<String> = (0..10).map(|i| format!("input{}", i)).collect();
    let outputs: Vec<String> = (0..5).map(|i| format!("output{}", i)).collect();

    let receipt = Receipt::new(
        "multi-io".to_string(),
        inputs.clone(),
        outputs.clone(),
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    assert_eq!(receipt.input_hashes.len(), 10);
    assert_eq!(receipt.output_hashes.len(), 5);
    assert!(receipt.verify(&verifying_key).is_ok());
}

#[test]
fn test_empty_inputs_outputs() {
    let (signing_key, verifying_key) = generate_keypair();

    let receipt = Receipt::new(
        "empty".to_string(),
        vec![],
        vec![],
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    assert!(receipt.input_hashes.is_empty());
    assert!(receipt.output_hashes.is_empty());
    assert!(receipt.verify(&verifying_key).is_ok());
}

#[test]
fn test_long_operation_id() {
    let (signing_key, verifying_key) = generate_keypair();

    let long_id = "a".repeat(1000);
    let receipt = Receipt::new(
        long_id.clone(),
        vec!["input".to_string()],
        vec!["output".to_string()],
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    assert_eq!(receipt.operation_id.len(), 1000);
    assert!(receipt.verify(&verifying_key).is_ok());
}

#[test]
fn test_receipt_timestamp_present() {
    let receipt = Receipt::new(
        "op1".to_string(),
        vec![],
        vec![],
        None,
    );

    // Just verify that timestamp is set to something reasonable
    let now = chrono::Utc::now();
    let diff = now.signed_duration_since(receipt.timestamp);

    // Should be created within the last second
    assert!(diff.num_seconds() < 2);
}
