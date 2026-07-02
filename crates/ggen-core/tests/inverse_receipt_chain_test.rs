#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::useless_borrows_in_formatting
)]
//! Chicago TDD tests for InverseReceiptChain cryptographic linkage.
//!
//! Tests the complete chain with:
//! - Real Ed25519 keypairs (not mocks)
//! - Real BLAKE3 hashing
//! - Real signature verification
//! - Operation ID UUID v4 validation
//! - Chain hash consistency verification
//!
//! This is a boundary-crossing test that exercises the full inverse receipt
//! lifecycle: creation, signing, chaining, and multi-receipt verification.

use ggen_core::generate_keypair;
use ggen_core::receipt::OperationLink;
use ggen_core::reverse_sync::{InverseReceipt, InverseReceiptChain, InverseStage};
use std::collections::HashMap;

/// Helper to create a valid InverseReceipt with real signature.
fn create_signed_receipt(signing_key: &ed25519_dalek::SigningKey) -> InverseReceipt {
    let mut input_hashes = HashMap::new();
    input_hashes.insert("test.rs".to_string(), "deadbeef".to_string());

    let mut receipt = InverseReceipt {
        operation_id: uuid::Uuid::new_v4().to_string(),
        timestamp: chrono::Utc::now(),
        input_hashes,
        output_hash: "cafebabe".to_string(),
        recovered_triple_count: 42,
        shacl_valid: true,
        last_stage: InverseStage::Emit,
        signature: String::new(), // Will be filled by sign()
        previous_operation_id: None,
    };

    receipt = receipt.sign(signing_key).expect("sign should succeed");
    receipt
}

fn create_linked_receipt(
    signing_key: &ed25519_dalek::SigningKey, forward_op_id: &str,
) -> InverseReceipt {
    let mut receipt = create_signed_receipt(signing_key);
    receipt.previous_operation_id = Some(forward_op_id.to_string());
    receipt.sign(signing_key).expect("sign should succeed")
}

#[test]
fn test_inverse_receipt_chain_new_empty() {
    let chain = InverseReceiptChain::new();

    assert_eq!(chain.receipts.len(), 0, "new chain must be empty");
    assert_eq!(
        chain.chain_hash.len(),
        64,
        "chain_hash must be 64 hex chars (BLAKE3)"
    );
    assert!(
        chain.chain_hash.chars().all(|c| c.is_ascii_hexdigit()),
        "chain_hash must be valid hex"
    );
}

#[test]
fn test_inverse_receipt_chain_append_single_valid() {
    // Arrange
    let (signing_key, verifying_key) = generate_keypair();
    let receipt = create_signed_receipt(&signing_key);
    let initial_chain_hash = receipt.hash().expect("receipt hash should succeed");

    // Act
    let mut chain = InverseReceiptChain::new();
    let result = chain.append(receipt.clone(), &verifying_key);

    // Assert
    assert!(
        result.is_ok(),
        "append should succeed for valid signed receipt"
    );
    assert_eq!(chain.receipts.len(), 1, "chain must contain one receipt");
    assert_eq!(
        chain.receipts[0].operation_id, receipt.operation_id,
        "receipt must match appended receipt"
    );
    // Chain hash must change after appending (no longer the empty hash).
    let empty_hash = {
        let mut hasher = blake3::Hasher::new();
        hasher.update(b"");
        hasher.finalize().to_hex().to_string()
    };
    assert_ne!(
        chain.chain_hash, empty_hash,
        "chain_hash must change after appending"
    );
}

#[test]
fn test_inverse_receipt_chain_append_multiple_grows_correctly() {
    // Arrange
    let (signing_key, verifying_key) = generate_keypair();
    let mut chain = InverseReceiptChain::new();

    // Act
    let r1 = create_signed_receipt(&signing_key);
    let r2 = create_signed_receipt(&signing_key);
    let r3 = create_signed_receipt(&signing_key);

    chain
        .append(r1.clone(), &verifying_key)
        .expect("append r1 should succeed");
    let hash_after_r1 = chain.chain_hash.clone();

    chain
        .append(r2.clone(), &verifying_key)
        .expect("append r2 should succeed");
    let hash_after_r2 = chain.chain_hash.clone();

    chain
        .append(r3.clone(), &verifying_key)
        .expect("append r3 should succeed");
    let hash_after_r3 = chain.chain_hash.clone();

    // Assert
    assert_eq!(chain.receipts.len(), 3, "chain must contain 3 receipts");
    assert_ne!(
        hash_after_r1, hash_after_r2,
        "hash must differ after each append"
    );
    assert_ne!(
        hash_after_r2, hash_after_r3,
        "hash must differ after each append"
    );
    // All three hashes must be distinct.
    assert_ne!(hash_after_r1, hash_after_r3);
}

#[test]
fn test_inverse_receipt_chain_append_invalid_signature_rejected() {
    // Arrange
    let (signing_key, verifying_key) = generate_keypair();
    let mut receipt = create_signed_receipt(&signing_key);
    receipt.signature = String::new(); // Blank the signature (fail-closed test)

    // Act
    let mut chain = InverseReceiptChain::new();
    let result = chain.append(receipt, &verifying_key);

    // Assert
    assert!(
        result.is_err(),
        "append must reject receipt with empty signature"
    );
    assert_eq!(
        chain.receipts.len(),
        0,
        "chain must remain empty after rejection"
    );
}

#[test]
fn test_inverse_receipt_chain_append_tampered_receipt_rejected() {
    // Arrange
    let (signing_key, verifying_key) = generate_keypair();
    let receipt = create_signed_receipt(&signing_key);

    // Act — tamper with the receipt AFTER signing (invalidates signature).
    let mut tampered = receipt.clone();
    tampered.output_hash = format!("{}deadbeef", &tampered.output_hash);

    let mut chain = InverseReceiptChain::new();
    let result = chain.append(tampered, &verifying_key);

    // Assert
    assert!(
        result.is_err(),
        "append must reject tampered receipt (signature no longer valid)"
    );
    assert_eq!(
        chain.receipts.len(),
        0,
        "chain must remain empty after rejection"
    );
}

#[test]
fn test_inverse_receipt_chain_verify_all_valid_returns_true() {
    // Arrange
    let (signing_key, verifying_key) = generate_keypair();
    let mut chain = InverseReceiptChain::new();

    let r1 = create_signed_receipt(&signing_key);
    let r2 = create_signed_receipt(&signing_key);
    let r3 = create_signed_receipt(&signing_key);

    // Act
    chain
        .append(r1, &verifying_key)
        .expect("append r1 should succeed");
    chain
        .append(r2, &verifying_key)
        .expect("append r2 should succeed");
    chain
        .append(r3, &verifying_key)
        .expect("append r3 should succeed");

    // Verify the chain.
    let is_valid = chain.verify(&verifying_key);

    // Assert
    assert!(
        is_valid,
        "valid chain with all correct signatures must verify true"
    );
}

#[test]
fn test_inverse_receipt_chain_verify_empty_chain_is_valid() {
    let (_signing_key, verifying_key) = generate_keypair();
    let chain = InverseReceiptChain::new();

    let is_valid = chain.verify(&verifying_key);
    assert!(is_valid, "empty chain must be trivially valid");
}

#[test]
fn test_inverse_receipt_chain_verify_tampered_signature_returns_false() {
    // Arrange
    let (signing_key, verifying_key) = generate_keypair();
    let mut chain = InverseReceiptChain::new();

    let r1 = create_signed_receipt(&signing_key);
    let r2 = create_signed_receipt(&signing_key);

    chain
        .append(r1, &verifying_key)
        .expect("append r1 should succeed");
    chain
        .append(r2, &verifying_key)
        .expect("append r2 should succeed");

    // Act — tamper with a receipt signature in the stored chain.
    chain.receipts[0].signature = "".to_string();

    let is_valid = chain.verify(&verifying_key);

    // Assert
    assert!(
        !is_valid,
        "chain with tampered signature must verify false (fail-closed)"
    );
}

#[test]
fn test_inverse_receipt_chain_verify_wrong_key_returns_false() {
    // Arrange
    let (signing_key1, verifying_key1) = generate_keypair();
    let (_signing_key2, verifying_key2) = generate_keypair();

    let mut chain = InverseReceiptChain::new();
    let r1 = create_signed_receipt(&signing_key1);

    chain
        .append(r1, &verifying_key1)
        .expect("append with correct key should succeed");

    // Act — verify with a different key.
    let is_valid = chain.verify(&verifying_key2);

    // Assert
    assert!(
        !is_valid,
        "verification with wrong key must return false (fail-closed)"
    );
}

#[test]
fn test_inverse_receipt_chain_verify_corrupted_chain_hash_returns_false() {
    // Arrange
    let (signing_key, verifying_key) = generate_keypair();
    let mut chain = InverseReceiptChain::new();

    let r1 = create_signed_receipt(&signing_key);
    chain
        .append(r1, &verifying_key)
        .expect("append should succeed");

    // Act — corrupt the stored chain_hash.
    chain.chain_hash = "0".repeat(64); // All zeros (invalid for real computation)

    let is_valid = chain.verify(&verifying_key);

    // Assert
    assert!(
        !is_valid,
        "verification with corrupted chain_hash must return false"
    );
}

#[test]
fn test_inverse_receipt_previous_operation_id_links_to_forward() {
    // Arrange — simulate a forward operation_id.
    let forward_op_id = uuid::Uuid::new_v4().to_string();
    let (signing_key, _verifying_key) = generate_keypair();

    // Act — create an inverse receipt linked to the forward operation.
    let mut inverse = create_signed_receipt(&signing_key);
    inverse.previous_operation_id = Some(forward_op_id.clone());

    // Assert
    assert_eq!(
        inverse.previous_operation_id,
        Some(forward_op_id),
        "inverse receipt must record the forward operation_id"
    );
}

#[test]
fn test_inverse_receipt_chain_preserves_operation_linkage() {
    // Arrange
    let forward_op_id = uuid::Uuid::new_v4().to_string();
    let (signing_key, verifying_key) = generate_keypair();

    let receipt = create_linked_receipt(&signing_key, &forward_op_id);

    // Act
    let mut chain = InverseReceiptChain::new();
    chain
        .append(receipt.clone(), &verifying_key)
        .expect("append should succeed");

    // Assert
    assert_eq!(
        chain.receipts[0].previous_operation_id,
        Some(forward_op_id),
        "chain must preserve the operation linkage"
    );
}

#[test]
fn test_inverse_receipt_hash_deterministic() {
    // Arrange
    let (signing_key, _verifying_key) = generate_keypair();
    let receipt = create_signed_receipt(&signing_key);

    // Act — compute hash twice on the same receipt.
    let hash1 = receipt.hash().expect("hash should succeed");
    let hash2 = receipt.hash().expect("hash should succeed");

    // Assert
    assert_eq!(hash1, hash2, "receipt hash must be deterministic");
    assert_eq!(hash1.len(), 64, "BLAKE3 hex must be 64 chars");
    assert!(
        hash1.chars().all(|c| c.is_ascii_hexdigit()),
        "hash must be valid hex"
    );
}

#[test]
fn test_inverse_receipt_hash_changes_with_modification() {
    // Arrange
    let (signing_key, _verifying_key) = generate_keypair();
    let receipt1 = create_signed_receipt(&signing_key);
    let hash1 = receipt1.hash().expect("hash should succeed");

    // Act — create a different receipt.
    let receipt2 = create_signed_receipt(&signing_key);
    let hash2 = receipt2.hash().expect("hash should succeed");

    // Assert
    assert_ne!(
        hash1, hash2,
        "different receipts must have different hashes"
    );
}

#[test]
fn test_operation_link_create_valid_uuid_v4() {
    // Arrange
    let fwd_id = uuid::Uuid::new_v4().to_string();
    let inv_id = uuid::Uuid::new_v4().to_string();

    // Act
    let link = OperationLink::create(&fwd_id, &inv_id).expect("link creation should succeed");

    // Assert
    assert_eq!(link.forward_operation_id, fwd_id);
    assert_eq!(link.inverse_operation_id, inv_id);
    assert_eq!(link.link_hash.len(), 64, "link_hash must be 64 hex chars");
    assert!(
        link.link_hash.chars().all(|c| c.is_ascii_hexdigit()),
        "link_hash must be valid hex"
    );
}

#[test]
fn test_operation_link_hash_deterministic() {
    // Arrange
    let fwd_id = uuid::Uuid::new_v4().to_string();
    let inv_id = uuid::Uuid::new_v4().to_string();

    // Act — create links with same IDs twice.
    let link1 = OperationLink::create(&fwd_id, &inv_id).expect("first link should succeed");
    let link2 = OperationLink::create(&fwd_id, &inv_id).expect("second link should succeed");

    // Assert
    assert_eq!(
        link1.link_hash, link2.link_hash,
        "same operation IDs must produce same link hash"
    );
}

#[test]
fn test_operation_link_hash_sensitive_to_forward_id() {
    // Arrange
    let fwd_id1 = uuid::Uuid::new_v4().to_string();
    let fwd_id2 = uuid::Uuid::new_v4().to_string();
    let inv_id = uuid::Uuid::new_v4().to_string();

    // Act
    let link1 = OperationLink::create(&fwd_id1, &inv_id).expect("first link should succeed");
    let link2 = OperationLink::create(&fwd_id2, &inv_id).expect("second link should succeed");

    // Assert
    assert_ne!(
        link1.link_hash, link2.link_hash,
        "different forward IDs must produce different link hashes"
    );
}

#[test]
fn test_operation_link_hash_sensitive_to_inverse_id() {
    // Arrange
    let fwd_id = uuid::Uuid::new_v4().to_string();
    let inv_id1 = uuid::Uuid::new_v4().to_string();
    let inv_id2 = uuid::Uuid::new_v4().to_string();

    // Act
    let link1 = OperationLink::create(&fwd_id, &inv_id1).expect("first link should succeed");
    let link2 = OperationLink::create(&fwd_id, &inv_id2).expect("second link should succeed");

    // Assert
    assert_ne!(
        link1.link_hash, link2.link_hash,
        "different inverse IDs must produce different link hashes"
    );
}

#[test]
fn test_operation_link_rejects_invalid_forward_id() {
    // Arrange
    let inv_id = uuid::Uuid::new_v4().to_string();
    let invalid_fwd = "not-a-uuid";

    // Act
    let result = OperationLink::create(invalid_fwd, &inv_id);

    // Assert
    assert!(result.is_err(), "must reject invalid forward_operation_id");
    assert!(
        result.unwrap_err().contains("forward_operation_id"),
        "error must reference forward ID"
    );
}

#[test]
fn test_operation_link_rejects_invalid_inverse_id() {
    // Arrange
    let fwd_id = uuid::Uuid::new_v4().to_string();
    let invalid_inv = "also-not-a-uuid";

    // Act
    let result = OperationLink::create(&fwd_id, invalid_inv);

    // Assert
    assert!(result.is_err(), "must reject invalid inverse_operation_id");
    assert!(
        result.unwrap_err().contains("inverse_operation_id"),
        "error must reference inverse ID"
    );
}

#[test]
fn test_operation_link_rejects_non_v4_uuid() {
    // Arrange — create a UUID v1 (not v4).
    let v1_uuid = "c106a26a-21bb-11eb-adc1-0242ac120002".to_string();
    let v4_uuid = uuid::Uuid::new_v4().to_string();

    // Act
    let result = OperationLink::create(&v1_uuid, &v4_uuid);

    // Assert
    assert!(result.is_err(), "must reject non-v4 UUID");
    assert!(
        result.unwrap_err().contains("UUID v4"),
        "error must mention UUID v4 requirement"
    );
}

#[test]
fn test_full_e2e_forward_inverse_linkage() {
    // Arrange — simulate a forward operation and then create a linked inverse.
    let forward_op_id = uuid::Uuid::new_v4().to_string();
    let (signing_key, verifying_key) = generate_keypair();

    // Create and sign an inverse receipt linked to the forward operation.
    let inverse_receipt = create_linked_receipt(&signing_key, &forward_op_id);

    // Build the inverse chain.
    let mut chain = InverseReceiptChain::new();
    chain
        .append(inverse_receipt.clone(), &verifying_key)
        .expect("append should succeed");

    // Create an operation link between forward and inverse.
    let link = OperationLink::create(&forward_op_id, &inverse_receipt.operation_id)
        .expect("operation link should succeed");

    // Act + Assert — verify all three pieces exist and are consistent.
    assert_eq!(
        inverse_receipt.previous_operation_id,
        Some(forward_op_id.clone()),
        "inverse receipt must link to forward operation"
    );
    assert_eq!(
        link.forward_operation_id, forward_op_id,
        "operation link must record forward operation"
    );
    assert_eq!(
        link.inverse_operation_id, inverse_receipt.operation_id,
        "operation link must record inverse operation"
    );
    assert!(
        chain.verify(&verifying_key),
        "inverse chain must verify with all linkages in place"
    );
}

#[test]
fn test_chain_default_returns_new_chain() {
    let chain1 = InverseReceiptChain::new();
    let chain2 = InverseReceiptChain::default();

    assert_eq!(
        chain1.chain_hash, chain2.chain_hash,
        "Default must match new()"
    );
    assert_eq!(chain1.receipts.len(), chain2.receipts.len());
}
