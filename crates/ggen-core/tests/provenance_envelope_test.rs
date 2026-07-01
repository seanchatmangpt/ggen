//! Comprehensive Chicago TDD tests for ProvenanceEnvelope.
//!
//! Tests the complete lifecycle of merging forward and inverse receipts
//! into a unified provenance envelope with cryptographic verification.
//! Uses real Receipt and InverseReceipt objects with real Ed25519 signatures.

use ggen_core::receipt::{generate_keypair, CoherenceReport, ProvenanceEnvelope, Receipt};
use ggen_core::reverse_sync::inverse_pipeline::{InverseReceipt, InverseStage};
use std::collections::HashMap;

/// Helper to create a real signed forward receipt.
fn make_forward_receipt(op_id: &str) -> (Receipt, ed25519_dalek::SigningKey) {
    let (signing_key, _) = generate_keypair();
    let receipt = Receipt::new(
        op_id.to_string(),
        vec!["input-1".to_string()],
        vec!["output-1".to_string()],
        None,
    )
    .sign(&signing_key)
    .expect("failed to sign forward receipt");

    (receipt, signing_key)
}

/// Helper to create a real signed inverse receipt.
fn make_inverse_receipt(op_id: &str) -> (InverseReceipt, ed25519_dalek::SigningKey) {
    let (signing_key, _) = generate_keypair();
    let mut input_hashes = HashMap::new();
    input_hashes.insert("src/main.rs".to_string(), "file-hash-abc123".to_string());
    input_hashes.insert("src/lib.rs".to_string(), "file-hash-def456".to_string());

    let receipt = InverseReceipt {
        operation_id: op_id.to_string(),
        timestamp: chrono::Utc::now(),
        input_hashes,
        output_hash: "recovered-rdf-hash-xyz789".to_string(),
        recovered_triple_count: 127,
        shacl_valid: true,
        last_stage: InverseStage::Emit,
        signature: String::new(),
        previous_operation_id: None,
    }
    .sign(&signing_key)
    .expect("failed to sign inverse receipt");

    (receipt, signing_key)
}

// ──────────────────────────────────────────────────────────────────────────
// BASIC ENVELOPE LIFECYCLE
// ──────────────────────────────────────────────────────────────────────────

#[test]
fn test_from_forward_creates_envelope_with_operation_chain() {
    // Arrange
    let (forward_receipt, _signing_key) = make_forward_receipt("forward-op-001");

    // Act
    let envelope = ProvenanceEnvelope::from_forward(forward_receipt.clone());

    // Assert
    assert!(envelope.forward_receipt.is_some());
    assert!(envelope.inverse_receipt.is_none());
    assert!(envelope.coherence_report.is_none());
    assert_eq!(envelope.operation_chain.len(), 1);
    assert_eq!(envelope.operation_chain[0], "forward-op-001");
    assert!(!envelope.envelope_hash.is_empty());
    assert_eq!(envelope.envelope_hash.len(), 64); // SHA-256 hex
}

#[test]
fn test_from_inverse_creates_envelope_with_operation_chain() {
    // Arrange
    let (inverse_receipt, _signing_key) = make_inverse_receipt("inverse-op-001");

    // Act
    let envelope = ProvenanceEnvelope::from_inverse(inverse_receipt.clone());

    // Assert
    assert!(envelope.forward_receipt.is_none());
    assert!(envelope.inverse_receipt.is_some());
    assert!(envelope.coherence_report.is_none());
    assert_eq!(envelope.operation_chain.len(), 1);
    assert_eq!(envelope.operation_chain[0], "inverse-op-001");
    assert!(!envelope.envelope_hash.is_empty());
}

#[test]
fn test_envelope_hash_updates_when_receipts_added() {
    // Arrange
    let (forward_receipt, _) = make_forward_receipt("op1");
    let (inverse_receipt, _) = make_inverse_receipt("op2");

    let env_empty = ProvenanceEnvelope::new();
    let hash_empty = env_empty.hash();

    let env_with_forward = ProvenanceEnvelope::new()
        .add_forward(forward_receipt)
        .expect("add_forward failed");
    let hash_with_forward = env_with_forward.hash();

    let env_with_both = env_with_forward
        .add_inverse(inverse_receipt)
        .expect("add_inverse failed");
    let hash_with_both = env_with_both.hash();

    // Assert — all three hashes are distinct
    assert_ne!(hash_empty, hash_with_forward);
    assert_ne!(hash_with_forward, hash_with_both);
    assert_ne!(hash_empty, hash_with_both);

    // All hashes are 64-char SHA-256
    assert_eq!(hash_empty.len(), 64);
    assert_eq!(hash_with_forward.len(), 64);
    assert_eq!(hash_with_both.len(), 64);
}

// ──────────────────────────────────────────────────────────────────────────
// BIDIRECTIONAL PROVENANCE (Forward + Inverse)
// ──────────────────────────────────────────────────────────────────────────

#[test]
fn test_add_forward_then_inverse_builds_complete_envelope() {
    // Arrange
    let (forward_receipt, _) = make_forward_receipt("breed-ontology");
    let (inverse_receipt, _) = make_inverse_receipt("reverse-breed");

    // Act
    let envelope = ProvenanceEnvelope::new()
        .add_forward(forward_receipt)
        .expect("add_forward failed")
        .add_inverse(inverse_receipt)
        .expect("add_inverse failed");

    // Assert
    assert!(envelope.forward_receipt.is_some());
    assert!(envelope.inverse_receipt.is_some());
    assert!(envelope.coherence_report.is_none());

    assert_eq!(envelope.operation_chain.len(), 2);
    assert_eq!(envelope.operation_chain[0], "breed-ontology");
    assert_eq!(envelope.operation_chain[1], "reverse-breed");

    let forward = envelope.forward_receipt.as_ref().unwrap();
    let inverse = envelope.inverse_receipt.as_ref().unwrap();

    assert_eq!(forward.operation_id, "breed-ontology");
    assert_eq!(inverse.operation_id, "reverse-breed");
}

#[test]
fn test_add_inverse_then_forward_preserves_operation_order() {
    // Arrange
    let (forward_receipt, _) = make_forward_receipt("forward-op");
    let (inverse_receipt, _) = make_inverse_receipt("inverse-op");

    // Act — add in reverse order
    let envelope = ProvenanceEnvelope::new()
        .add_inverse(inverse_receipt)
        .expect("add_inverse failed")
        .add_forward(forward_receipt)
        .expect("add_forward failed");

    // Assert — operation_chain reflects the order of add_* calls
    assert_eq!(envelope.operation_chain.len(), 2);
    assert_eq!(envelope.operation_chain[0], "inverse-op");
    assert_eq!(envelope.operation_chain[1], "forward-op");
}

// ──────────────────────────────────────────────────────────────────────────
// COHERENCE VALIDATION
// ──────────────────────────────────────────────────────────────────────────

#[test]
fn test_add_coherence_report_to_complete_envelope() {
    // Arrange
    let (forward_receipt, _) = make_forward_receipt("op1");
    let (inverse_receipt, _) = make_inverse_receipt("op2");
    let coherence_report = CoherenceReport::new(
        "validation-001".to_string(),
        "forward-artifact-hash-abc123".to_string(),
        "recovered-artifact-hash-xyz789".to_string(),
        true,
        None,
    );

    // Act
    let envelope = ProvenanceEnvelope::new()
        .add_forward(forward_receipt)
        .expect("add_forward failed")
        .add_inverse(inverse_receipt)
        .expect("add_inverse failed")
        .add_coherence(coherence_report);

    // Assert
    assert!(envelope.coherence_report.is_some());
    let report = envelope.coherence_report.as_ref().unwrap();
    assert_eq!(report.validation_id, "validation-001");
    assert!(report.admitted);
    assert!(report.divergence.is_none());
}

#[test]
fn test_coherence_report_with_divergence() {
    // Arrange
    let (forward_receipt, _) = make_forward_receipt("op1");
    let coherence_report = CoherenceReport::new(
        "validation-fail".to_string(),
        "forward-hash".to_string(),
        "recovered-hash-different".to_string(),
        false,
        Some("namespace URN mismatch in recovered ontology".to_string()),
    );

    // Act
    let envelope = ProvenanceEnvelope::new()
        .add_forward(forward_receipt)
        .expect("add_forward failed")
        .add_coherence(coherence_report);

    // Assert
    let report = envelope.coherence_report.as_ref().unwrap();
    assert!(!report.admitted);
    assert!(report.divergence.is_some());
    assert!(report
        .divergence
        .as_ref()
        .unwrap()
        .contains("namespace URN mismatch"));
}

#[test]
fn test_coherence_report_hash_is_computed() {
    // Arrange
    let coherence_report = CoherenceReport::new(
        "val-001".to_string(),
        "hash1".to_string(),
        "hash2".to_string(),
        true,
        None,
    );

    // Assert
    assert!(!coherence_report.report_hash.is_empty());
    assert_eq!(coherence_report.report_hash.len(), 64); // SHA-256 hex
}

// ──────────────────────────────────────────────────────────────────────────
// VERIFICATION
// ──────────────────────────────────────────────────────────────────────────

#[test]
fn test_verify_forward_receipt_with_correct_key() {
    // Arrange
    let (forward_receipt, signing_key) = make_forward_receipt("op1");
    let verifying_key = signing_key.verifying_key();

    let envelope = ProvenanceEnvelope::new()
        .add_forward(forward_receipt)
        .expect("add_forward failed");

    // Act & Assert
    assert!(envelope.verify(&verifying_key));
}

#[test]
fn test_verify_forward_receipt_with_wrong_key() {
    // Arrange
    let (forward_receipt, _signing_key) = make_forward_receipt("op1");
    let (_wrong_signing_key, wrong_verifying_key) = generate_keypair();

    let envelope = ProvenanceEnvelope::new()
        .add_forward(forward_receipt)
        .expect("add_forward failed");

    // Act & Assert
    assert!(!envelope.verify(&wrong_verifying_key));
}

#[test]
fn test_verify_both_forward_and_inverse_with_same_key() {
    // Arrange — both receipts signed with same key
    let (signing_key, _) = generate_keypair();
    let verifying_key = signing_key.verifying_key();

    let forward = Receipt::new(
        "op1".to_string(),
        vec!["input".to_string()],
        vec!["output".to_string()],
        None,
    )
    .sign(&signing_key)
    .expect("failed to sign forward");

    let mut input_hashes = HashMap::new();
    input_hashes.insert("file.rs".to_string(), "hash".to_string());
    let inverse = InverseReceipt {
        operation_id: "op2".to_string(),
        timestamp: chrono::Utc::now(),
        input_hashes,
        output_hash: "out".to_string(),
        recovered_triple_count: 10,
        shacl_valid: true,
        last_stage: InverseStage::Emit,
        signature: String::new(),
        previous_operation_id: None,
    }
    .sign(&signing_key)
    .expect("failed to sign inverse");

    // Act
    let envelope = ProvenanceEnvelope::new()
        .add_forward(forward)
        .expect("add_forward failed")
        .add_inverse(inverse)
        .expect("add_inverse failed");

    // Assert
    assert!(envelope.verify(&verifying_key));
}

#[test]
fn test_verify_fails_when_coherence_not_admitted() {
    // Arrange
    let (forward_receipt, signing_key) = make_forward_receipt("op1");
    let verifying_key = signing_key.verifying_key();

    let coherence_report = CoherenceReport::new(
        "val-fail".to_string(),
        "hash1".to_string(),
        "hash2".to_string(),
        false, // NOT admitted
        Some("semantic divergence".to_string()),
    );

    let envelope = ProvenanceEnvelope::new()
        .add_forward(forward_receipt)
        .expect("add_forward failed")
        .add_coherence(coherence_report);

    // Act & Assert
    assert!(!envelope.verify(&verifying_key));
}

#[test]
fn test_verify_passes_when_coherence_admitted() {
    // Arrange
    let (forward_receipt, signing_key) = make_forward_receipt("op1");
    let verifying_key = signing_key.verifying_key();

    let coherence_report = CoherenceReport::new(
        "val-pass".to_string(),
        "hash1".to_string(),
        "hash1".to_string(),
        true, // admitted
        None,
    );

    let envelope = ProvenanceEnvelope::new()
        .add_forward(forward_receipt)
        .expect("add_forward failed")
        .add_coherence(coherence_report);

    // Act & Assert
    assert!(envelope.verify(&verifying_key));
}

#[test]
fn test_verify_empty_envelope_returns_true() {
    // Arrange
    let (_, verifying_key) = generate_keypair();
    let envelope = ProvenanceEnvelope::new();

    // Act & Assert
    assert!(envelope.verify(&verifying_key));
}

// ──────────────────────────────────────────────────────────────────────────
// SERIALIZATION / DESERIALIZATION
// ──────────────────────────────────────────────────────────────────────────

#[test]
fn test_to_json_from_json_roundtrip_with_forward_only() {
    // Arrange
    let (forward_receipt, _) = make_forward_receipt("forward-op-abc");

    let original = ProvenanceEnvelope::new()
        .add_forward(forward_receipt)
        .expect("add_forward failed");
    let original_hash = original.envelope_hash.clone();

    // Act
    let json = original.to_json().expect("to_json failed");
    let deserialized = ProvenanceEnvelope::from_json(&json).expect("from_json failed");

    // Assert
    assert!(deserialized.forward_receipt.is_some());
    assert!(deserialized.inverse_receipt.is_none());
    assert_eq!(deserialized.operation_chain.len(), 1);
    assert_eq!(deserialized.operation_chain[0], "forward-op-abc");
    assert_eq!(deserialized.envelope_hash, original_hash);
}

#[test]
fn test_to_json_from_json_roundtrip_with_inverse_only() {
    // Arrange
    let (inverse_receipt, _) = make_inverse_receipt("inverse-op-xyz");

    let original = ProvenanceEnvelope::new()
        .add_inverse(inverse_receipt)
        .expect("add_inverse failed");
    let original_hash = original.envelope_hash.clone();

    // Act
    let json = original.to_json().expect("to_json failed");
    let deserialized = ProvenanceEnvelope::from_json(&json).expect("from_json failed");

    // Assert
    assert!(deserialized.forward_receipt.is_none());
    assert!(deserialized.inverse_receipt.is_some());
    assert_eq!(deserialized.operation_chain.len(), 1);
    assert_eq!(deserialized.operation_chain[0], "inverse-op-xyz");
    assert_eq!(deserialized.envelope_hash, original_hash);
}

#[test]
fn test_to_json_from_json_roundtrip_with_both_receipts_and_coherence() {
    // Arrange
    let (forward_receipt, _) = make_forward_receipt("forward-op");
    let (inverse_receipt, _) = make_inverse_receipt("inverse-op");
    let coherence_report = CoherenceReport::new(
        "validation-001".to_string(),
        "fwd-hash".to_string(),
        "inv-hash".to_string(),
        true,
        None,
    );

    let original = ProvenanceEnvelope::new()
        .add_forward(forward_receipt)
        .expect("add_forward failed")
        .add_inverse(inverse_receipt)
        .expect("add_inverse failed")
        .add_coherence(coherence_report);

    let original_hash = original.envelope_hash.clone();

    // Act
    let json = original.to_json().expect("to_json failed");
    let deserialized = ProvenanceEnvelope::from_json(&json).expect("from_json failed");

    // Assert
    assert!(deserialized.forward_receipt.is_some());
    assert!(deserialized.inverse_receipt.is_some());
    assert!(deserialized.coherence_report.is_some());
    assert_eq!(deserialized.operation_chain.len(), 2);
    assert_eq!(deserialized.envelope_hash, original_hash);

    let report = deserialized.coherence_report.unwrap();
    assert_eq!(report.validation_id, "validation-001");
    assert!(report.admitted);
}

#[test]
fn test_json_survives_prettification() {
    // Arrange
    let (forward_receipt, _) = make_forward_receipt("op1");
    let original = ProvenanceEnvelope::new()
        .add_forward(forward_receipt)
        .expect("add_forward failed");

    // Act
    let json = original.to_json().expect("to_json failed");
    let prettified = serde_json::to_string_pretty(
        &serde_json::from_str::<serde_json::Value>(&json).expect("parse json"),
    )
    .expect("prettify");
    let deserialized = ProvenanceEnvelope::from_json(&prettified).expect("from_json failed");

    // Assert
    assert_eq!(deserialized.envelope_hash, original.envelope_hash);
}

// ──────────────────────────────────────────────────────────────────────────
// NEGATIVE PATH / SABOTAGE TESTS
// ──────────────────────────────────────────────────────────────────────────

#[test]
fn sabotage_add_forward_rejects_unsigned_receipt() {
    // Arrange
    let unsigned = Receipt::new(
        "op1".to_string(),
        vec!["input".to_string()],
        vec!["output".to_string()],
        None,
    );
    assert!(
        unsigned.signature.is_empty(),
        "precondition: signature must be empty"
    );

    // Act
    let result = ProvenanceEnvelope::new().add_forward(unsigned);

    // Assert
    assert!(
        result.is_err(),
        "add_forward must reject receipt with empty signature"
    );
    let err_msg = result.unwrap_err().to_string();
    assert!(
        err_msg.contains("non-empty signature"),
        "error message should reference signature requirement"
    );
}

#[test]
fn sabotage_add_inverse_rejects_unsigned_receipt() {
    // Arrange
    let mut input_hashes = HashMap::new();
    input_hashes.insert("file.rs".to_string(), "hash".to_string());

    let unsigned = InverseReceipt {
        operation_id: "op".to_string(),
        timestamp: chrono::Utc::now(),
        input_hashes,
        output_hash: "hash".to_string(),
        recovered_triple_count: 0,
        shacl_valid: true,
        last_stage: InverseStage::Emit,
        signature: String::new(),
        previous_operation_id: None,
    };
    assert!(
        unsigned.signature.is_empty(),
        "precondition: signature must be empty"
    );

    // Act
    let result = ProvenanceEnvelope::new().add_inverse(unsigned);

    // Assert
    assert!(
        result.is_err(),
        "add_inverse must reject receipt with empty signature"
    );
    let err_msg = result.unwrap_err().to_string();
    assert!(
        err_msg.contains("non-empty signature"),
        "error message should reference signature requirement"
    );
}

#[test]
fn sabotage_verify_forward_with_corrupted_signature() {
    // Arrange
    let (forward_receipt, _) = make_forward_receipt("op1");
    let (_, wrong_verifying_key) = generate_keypair();

    let envelope = ProvenanceEnvelope::new()
        .add_forward(forward_receipt)
        .expect("add_forward failed");

    // Act & Assert
    assert!(!envelope.verify(&wrong_verifying_key));
}

#[test]
fn sabotage_verify_fails_when_any_receipt_is_invalid() {
    // Arrange — forward is valid, but we'll verify with wrong key
    let (forward_receipt, _signing_key) = make_forward_receipt("op1");
    let (_, wrong_verifying_key) = generate_keypair();

    let envelope = ProvenanceEnvelope::new()
        .add_forward(forward_receipt)
        .expect("add_forward failed");

    // Act & Assert
    assert!(
        !envelope.verify(&wrong_verifying_key),
        "verify must fail if any proof element is invalid"
    );
}

#[test]
fn sabotage_coherence_report_not_admitted_blocks_verification() {
    // Arrange
    let (forward_receipt, signing_key) = make_forward_receipt("op1");
    let verifying_key = signing_key.verifying_key();

    let bad_coherence = CoherenceReport::new(
        "val-bad".to_string(),
        "hash1".to_string(),
        "hash2".to_string(),
        false,
        Some("divergence detected".to_string()),
    );

    let envelope = ProvenanceEnvelope::new()
        .add_forward(forward_receipt)
        .expect("add_forward failed")
        .add_coherence(bad_coherence);

    // Act & Assert
    assert!(
        !envelope.verify(&verifying_key),
        "verify must fail if coherence is not admitted"
    );
}

// ──────────────────────────────────────────────────────────────────────────
// EDGE CASES & MULTI-STEP OPERATIONS
// ──────────────────────────────────────────────────────────────────────────

#[test]
fn test_multiple_forward_receipts_last_one_wins() {
    // Arrange
    let (fwd1, _) = make_forward_receipt("op1");
    let (fwd2, _) = make_forward_receipt("op2");

    // Act
    let envelope = ProvenanceEnvelope::new()
        .add_forward(fwd1)
        .expect("add_forward failed")
        .add_forward(fwd2)
        .expect("add_forward failed");

    // Assert — operation_chain has both, but forward_receipt is the latest
    assert_eq!(envelope.operation_chain.len(), 2);
    assert_eq!(envelope.operation_chain[0], "op1");
    assert_eq!(envelope.operation_chain[1], "op2");
    assert_eq!(envelope.forward_receipt.unwrap().operation_id, "op2");
}

#[test]
fn test_multiple_inverse_receipts_last_one_wins() {
    // Arrange
    let (inv1, _) = make_inverse_receipt("inv1");
    let (inv2, _) = make_inverse_receipt("inv2");

    // Act
    let envelope = ProvenanceEnvelope::new()
        .add_inverse(inv1)
        .expect("add_inverse failed")
        .add_inverse(inv2)
        .expect("add_inverse failed");

    // Assert
    assert_eq!(envelope.operation_chain.len(), 2);
    assert_eq!(envelope.operation_chain[0], "inv1");
    assert_eq!(envelope.operation_chain[1], "inv2");
    assert_eq!(envelope.inverse_receipt.unwrap().operation_id, "inv2");
}

#[test]
fn test_linked_at_timestamp_is_rfc3339() {
    // Arrange
    let envelope = ProvenanceEnvelope::new();

    // Act & Assert
    assert!(!envelope.linked_at.is_empty());
    // RFC-3339 timestamps contain 'T' and 'Z'
    assert!(envelope.linked_at.contains('T'));
    assert!(envelope.linked_at.contains('Z'));
}

#[test]
fn test_envelope_operations_are_chainable() {
    // Arrange
    let (fwd, _) = make_forward_receipt("fwd");
    let (inv, _) = make_inverse_receipt("inv");
    let coherence = CoherenceReport::new(
        "val".to_string(),
        "h1".to_string(),
        "h2".to_string(),
        true,
        None,
    );

    // Act — fluent builder pattern
    let envelope = ProvenanceEnvelope::new()
        .add_forward(fwd)
        .expect("add_forward")
        .add_inverse(inv)
        .expect("add_inverse")
        .add_coherence(coherence);

    // Assert
    assert!(envelope.forward_receipt.is_some());
    assert!(envelope.inverse_receipt.is_some());
    assert!(envelope.coherence_report.is_some());
    assert_eq!(envelope.operation_chain.len(), 2);
}

#[test]
fn test_hash_determinism_multiple_computations() {
    // Arrange
    let (forward_receipt, _) = make_forward_receipt("op1");
    let envelope = ProvenanceEnvelope::new()
        .add_forward(forward_receipt)
        .expect("add_forward failed");

    // Act — compute hash multiple times
    let hash1 = envelope.hash();
    let hash2 = envelope.hash();

    // Assert
    assert_eq!(hash1, hash2);
}
