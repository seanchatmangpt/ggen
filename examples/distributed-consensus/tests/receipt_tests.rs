//! Cryptographic receipt and audit trail tests
use distributed_consensus::Receipt;

#[test]
fn test_receipt_creation() {
    let receipt = Receipt::new(1, 0, "test_value".to_string());

    assert_eq!(receipt.round, 1);
    assert_eq!(receipt.view, 0);
    assert_eq!(receipt.value, "test_value");
    assert!(!receipt.id.is_empty());
    assert!(!receipt.content_hash.is_empty());
    assert_eq!(receipt.signatures.len(), 0);
}

#[test]
fn test_content_hash_deterministic() {
    let hash1 = Receipt::compute_hash("consensus_value");
    let hash2 = Receipt::compute_hash("consensus_value");

    assert_eq!(hash1, hash2);
}

#[test]
fn test_content_hash_different_values() {
    let hash1 = Receipt::compute_hash("value_1");
    let hash2 = Receipt::compute_hash("value_2");

    assert_ne!(hash1, hash2);
}

#[test]
fn test_receipt_add_signature() {
    let mut receipt = Receipt::new(1, 0, "value".to_string());

    let result = receipt.add_signature(0, "sig_from_node_0".to_string());
    assert!(result.is_ok());
    assert_eq!(receipt.signatures.len(), 1);
    assert_eq!(receipt.signatures[0].node_id, 0);
}

#[test]
fn test_receipt_duplicate_signature_rejected() {
    let mut receipt = Receipt::new(1, 0, "value".to_string());

    receipt
        .add_signature(0, "sig1".to_string())
        .expect("First sig");
    let result = receipt.add_signature(0, "sig2".to_string());

    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Duplicate"));
}

#[test]
fn test_receipt_multiple_signatures() {
    let mut receipt = Receipt::new(1, 0, "value".to_string());

    for i in 0..5 {
        receipt
            .add_signature(i, format!("sig_{}", i))
            .expect("Sig added");
    }

    assert_eq!(receipt.signatures.len(), 5);
}

#[test]
fn test_receipt_quorum_check_not_met() {
    let mut receipt = Receipt::new(1, 0, "value".to_string());

    receipt.add_signature(0, "sig_0".to_string()).expect("Sig");
    receipt.add_signature(1, "sig_1".to_string()).expect("Sig");

    assert!(!receipt.has_quorum(3)); // Need 3, have 2
}

#[test]
fn test_receipt_quorum_check_met() {
    let mut receipt = Receipt::new(1, 0, "value".to_string());

    for i in 0..3 {
        receipt.add_signature(i, format!("sig_{}", i)).expect("Sig");
    }

    assert!(receipt.has_quorum(3));
    assert!(receipt.has_quorum(2)); // Also satisfied with 2
    assert!(!receipt.has_quorum(4)); // But not with 4
}

#[test]
fn test_receipt_audit_trail_generation() {
    let mut receipt = Receipt::new(5, 2, "audit_test".to_string());

    receipt.add_signature(0, "sig_0".to_string()).expect("Sig");
    receipt.add_signature(1, "sig_1".to_string()).expect("Sig");
    receipt.add_signature(2, "sig_2".to_string()).expect("Sig");

    let audit = receipt.audit_report();

    assert!(audit.contains("Round: 5"));
    assert!(audit.contains("View: 2"));
    assert!(audit.contains("Value: audit_test"));
    assert!(audit.contains("Node 0"));
    assert!(audit.contains("Node 1"));
    assert!(audit.contains("Node 2"));
}

#[test]
fn test_receipt_content_hash_in_audit() {
    let receipt = Receipt::new(1, 0, "value".to_string());
    let audit = receipt.audit_report();

    assert!(audit.contains("Content Hash:"));
    assert!(audit.contains(&receipt.content_hash));
}

#[test]
fn test_receipt_timestamp_valid() {
    let receipt = Receipt::new(1, 0, "value".to_string());
    let now = chrono::Utc::now();

    // Timestamp should be recent (within 1 second)
    let diff = now.signed_duration_since(receipt.timestamp);
    assert!(diff.num_seconds() >= 0 && diff.num_seconds() < 2);
}

#[test]
fn test_receipt_unique_ids() {
    let receipt1 = Receipt::new(1, 0, "value".to_string());
    let receipt2 = Receipt::new(1, 0, "value".to_string());

    assert_ne!(receipt1.id, receipt2.id); // Each receipt gets unique ID
}

#[test]
fn test_large_cluster_receipt() {
    let mut receipt = Receipt::new(100, 50, "large_cluster".to_string());

    // Add 11 signatures (simulating 16-node cluster quorum)
    for i in 0..11 {
        receipt.add_signature(i, format!("sig_{}", i)).expect("Sig");
    }

    assert!(receipt.has_quorum(11));
    assert_eq!(receipt.signatures.len(), 11);

    let audit = receipt.audit_report();
    assert!(audit.contains("Signatures: 11 of"));
}

#[test]
fn test_receipt_value_immutability() {
    let receipt = Receipt::new(1, 0, "original_value".to_string());

    // Receipt value doesn't change after creation
    assert_eq!(receipt.value, "original_value");
    assert_eq!(receipt.round, 1);
    assert_eq!(receipt.view, 0);
}

#[test]
fn test_receipt_signature_ordering() {
    let mut receipt = Receipt::new(1, 0, "value".to_string());

    // Add signatures in order
    for i in 0..3 {
        receipt.add_signature(i, format!("sig_{}", i)).expect("Sig");
    }

    // Signatures should be in order of addition
    assert_eq!(receipt.signatures[0].node_id, 0);
    assert_eq!(receipt.signatures[1].node_id, 1);
    assert_eq!(receipt.signatures[2].node_id, 2);
}

#[test]
fn test_receipt_signature_contains_node_id() {
    let mut receipt = Receipt::new(1, 0, "value".to_string());
    receipt
        .add_signature(42, "sig_42".to_string())
        .expect("Sig");

    let sig = &receipt.signatures[0];
    assert_eq!(sig.node_id, 42);
    assert_eq!(sig.signature, "sig_42");
}

#[test]
fn test_receipt_different_rounds_different_hashes() {
    let receipt1 = Receipt::new(1, 0, "same_value".to_string());
    let receipt2 = Receipt::new(2, 0, "same_value".to_string());

    // Different rounds should produce different hashes
    // (content includes round number)
    assert_ne!(receipt1.content_hash, receipt2.content_hash);
}

#[test]
fn test_receipt_different_views_different_hashes() {
    let receipt1 = Receipt::new(1, 0, "same_value".to_string());
    let receipt2 = Receipt::new(1, 1, "same_value".to_string());

    // Different views should produce different hashes
    assert_ne!(receipt1.content_hash, receipt2.content_hash);
}

#[test]
fn test_receipt_audit_trail_immutable() {
    let receipt = Receipt::new(1, 0, "value".to_string());
    let audit1 = receipt.audit_trail.clone();
    let audit2 = receipt.audit_trail.clone();

    assert_eq!(audit1, audit2);
    assert!(audit1.contains("round=1"));
    assert!(audit1.contains("view=0"));
}

#[test]
fn test_receipt_zero_signatures() {
    let receipt = Receipt::new(1, 0, "value".to_string());

    assert!(!receipt.has_quorum(1));
    assert_eq!(receipt.signatures.len(), 0);
}

#[test]
fn test_receipt_minimal_quorum() {
    let mut receipt = Receipt::new(1, 0, "value".to_string());

    // Add exactly quorum_size (3) signatures
    for i in 0..3 {
        receipt.add_signature(i, format!("sig_{}", i)).expect("Sig");
    }

    assert!(receipt.has_quorum(3));
    assert!(!receipt.has_quorum(4)); // One more than available
}

#[test]
fn test_signature_timestamp_tracking() {
    let mut receipt = Receipt::new(1, 0, "value".to_string());

    receipt.add_signature(0, "sig_0".to_string()).expect("Sig");

    let sig = &receipt.signatures[0];
    let now = chrono::Utc::now();
    let diff = now.signed_duration_since(sig.timestamp);

    assert!(diff.num_seconds() >= 0 && diff.num_seconds() < 2);
}
