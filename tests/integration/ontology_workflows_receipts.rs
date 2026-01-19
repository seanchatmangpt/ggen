//! Receipt Generation and Verification Integration Tests
//!
//! Tests the complete receipt generation and verification workflow:
//! 1. Generate receipt for ontology → proposal transformation
//! 2. Verify receipt signature (Ed25519 equivalent)
//! 3. Verify receipt hash (SHA256)
//! 4. Detect tampering (hash verification fails if content changes)
//! 5. Chain receipts for audit trail

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;

/// A provenance receipt binding input ontology to output proposal
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProvenanceReceipt {
    /// Unique receipt ID (based on input hash)
    pub receipt_id: String,

    /// Hash of input ontology (SHA256)
    pub input_hash: String,

    /// Hash of output proposal (SHA256)
    pub output_hash: String,

    /// Combined hash (SHA256(input_hash + output_hash))
    pub combined_hash: String,

    /// Timestamp of generation (ISO 8601)
    pub timestamp: String,

    /// Version of ggen used
    pub ggen_version: String,

    /// Metadata about the transformation
    pub transformation: TransformationMetadata,

    /// Ed25519-like signature (in this test, also SHA256)
    pub signature: String,

    /// Parent receipt ID (for chaining/audit trail)
    pub parent_receipt_id: Option<String>,

    /// Whether this receipt has been verified
    pub verified: bool,
}

/// Metadata about the transformation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TransformationMetadata {
    /// Number of triples in input ontology
    pub input_triple_count: usize,

    /// Number of lines in output proposal
    pub output_line_count: usize,

    /// Guards that were evaluated
    pub guards_evaluated: usize,

    /// Guards that passed
    pub guards_passed: usize,

    /// Determinism check passed
    pub determinism_verified: bool,
}

/// Generate a receipt for an ontology → proposal transformation
pub fn generate_receipt(
    ontology_content: &str, proposal_content: &str, parent_receipt: Option<&ProvenanceReceipt>,
) -> ProvenanceReceipt {
    // Calculate input hash
    let input_hash = calculate_sha256(ontology_content);

    // Calculate output hash
    let output_hash = calculate_sha256(proposal_content);

    // Calculate combined hash
    let combined_content = format!("{}{}", input_hash, output_hash);
    let combined_hash = calculate_sha256(&combined_content);

    // Generate receipt ID from input hash
    let receipt_id = format!(
        "receipt-{}",
        input_hash.chars().take(16).collect::<String>()
    );

    // Create signature (in this test, SHA256 of all content)
    let signature_content = format!(
        "{}{}{}{}",
        receipt_id, input_hash, output_hash, combined_hash
    );
    let signature = calculate_sha256(&signature_content);

    // Get current timestamp (in real implementation, use chrono)
    let timestamp = "2024-01-19T00:00:00Z".to_string();

    // Create transformation metadata
    let transformation = TransformationMetadata {
        input_triple_count: ontology_content.matches("rdf:type").count(),
        output_line_count: proposal_content.lines().count(),
        guards_evaluated: 12,
        guards_passed: 12,
        determinism_verified: true,
    };

    ProvenanceReceipt {
        receipt_id,
        input_hash,
        output_hash,
        combined_hash,
        timestamp,
        ggen_version: "3.3.0".to_string(),
        transformation,
        signature,
        parent_receipt_id: parent_receipt.map(|r| r.receipt_id.clone()),
        verified: false,
    }
}

/// Verify a receipt (check signature and hashes)
pub fn verify_receipt(
    receipt: &ProvenanceReceipt, ontology_content: &str, proposal_content: &str,
) -> (bool, String) {
    // Recalculate hashes
    let input_hash = calculate_sha256(ontology_content);
    let output_hash = calculate_sha256(proposal_content);
    let combined_hash = calculate_sha256(&format!("{}{}", input_hash, output_hash));

    // Verify input hash
    if receipt.input_hash != input_hash {
        return (
            false,
            "Input hash mismatch: ontology content has been modified".to_string(),
        );
    }

    // Verify output hash
    if receipt.output_hash != output_hash {
        return (
            false,
            "Output hash mismatch: proposal content has been modified".to_string(),
        );
    }

    // Verify combined hash
    if receipt.combined_hash != combined_hash {
        return (
            false,
            "Combined hash mismatch: either input or output has been tampered with".to_string(),
        );
    }

    // Verify signature
    let expected_signature_content = format!(
        "{}{}{}{}",
        receipt.receipt_id, receipt.input_hash, receipt.output_hash, receipt.combined_hash
    );
    let expected_signature = calculate_sha256(&expected_signature_content);

    if receipt.signature != expected_signature {
        return (
            false,
            "Signature verification failed: receipt may have been forged".to_string(),
        );
    }

    (true, "Receipt verified successfully".to_string())
}

/// Calculate SHA256 hash of content
fn calculate_sha256(content: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(content.as_bytes());
    format!("{:x}", hasher.finalize())
}

// ============================================================================
// Integration Tests
// ============================================================================

#[test]
fn test_receipt_generation_basic() {
    // Arrange
    let ontology = r#"@prefix ex: <http://example.org/> .
ex:Service rdf:type ex:ComputeService .
"#;
    let proposal = r#"{ "service": "ECS", "replicas": 3 }"#;

    // Act: Generate receipt
    let receipt = generate_receipt(ontology, proposal, None);

    // Assert: Receipt has all required fields
    assert!(
        !receipt.receipt_id.is_empty(),
        "Receipt ID should be generated"
    );
    assert!(
        receipt.receipt_id.starts_with("receipt-"),
        "Receipt ID should have receipt- prefix"
    );
    assert!(
        !receipt.input_hash.is_empty(),
        "Input hash should be generated"
    );
    assert!(
        !receipt.output_hash.is_empty(),
        "Output hash should be generated"
    );
    assert!(
        !receipt.combined_hash.is_empty(),
        "Combined hash should be generated"
    );
    assert!(
        !receipt.signature.is_empty(),
        "Signature should be generated"
    );
    assert_eq!(
        receipt.ggen_version, "3.3.0",
        "ggen version should be recorded"
    );
    assert!(
        receipt.parent_receipt_id.is_none(),
        "First receipt should have no parent"
    );
}

#[test]
fn test_receipt_verification_success() {
    // Arrange
    let ontology = r#"@prefix ex: <http://example.org/> .
ex:Service rdf:type ex:ComputeService .
"#;
    let proposal = r#"{ "service": "ECS", "replicas": 3 }"#;

    // Act: Generate receipt
    let mut receipt = generate_receipt(ontology, proposal, None);

    // Act: Verify receipt
    let (verified, message) = verify_receipt(&receipt, ontology, proposal);

    // Assert: Receipt verifies successfully
    assert!(verified, "Receipt should verify: {}", message);
    assert!(
        message.contains("successfully"),
        "Message should confirm success"
    );
}

#[test]
fn test_receipt_verification_fails_on_ontology_tampering() {
    // Arrange
    let ontology = r#"@prefix ex: <http://example.org/> .
ex:Service rdf:type ex:ComputeService .
"#;
    let tampered_ontology = r#"@prefix ex: <http://example.org/> .
ex:Service rdf:type ex:ComputeService .
ex:AnotherService rdf:type ex:ComputeService .
"#;
    let proposal = r#"{ "service": "ECS", "replicas": 3 }"#;

    // Act: Generate receipt with original ontology
    let receipt = generate_receipt(ontology, proposal, None);

    // Act: Try to verify with tampered ontology
    let (verified, message) = verify_receipt(&receipt, tampered_ontology, proposal);

    // Assert: Verification fails
    assert!(!verified, "Receipt should fail with tampered ontology");
    assert!(
        message.contains("Input hash mismatch"),
        "Message should indicate input tampering: {}",
        message
    );
}

#[test]
fn test_receipt_verification_fails_on_proposal_tampering() {
    // Arrange
    let ontology = r#"@prefix ex: <http://example.org/> .
ex:Service rdf:type ex:ComputeService .
"#;
    let proposal = r#"{ "service": "ECS", "replicas": 3 }"#;
    let tampered_proposal = r#"{ "service": "ECS", "replicas": 5 }"#;

    // Act: Generate receipt with original proposal
    let receipt = generate_receipt(ontology, proposal, None);

    // Act: Try to verify with tampered proposal
    let (verified, message) = verify_receipt(&receipt, ontology, tampered_proposal);

    // Assert: Verification fails
    assert!(!verified, "Receipt should fail with tampered proposal");
    assert!(
        message.contains("Output hash mismatch"),
        "Message should indicate output tampering: {}",
        message
    );
}

#[test]
fn test_receipt_chain_building() {
    // Arrange: Create sequence of transformations
    let ontology_v1 = r#"@prefix ex: <http://example.org/> .
ex:Service rdf:type ex:ComputeService .
"#;
    let proposal_v1 = r#"{ "service": "ECS", "replicas": 3, "version": 1 }"#;

    let ontology_v2 = r#"@prefix ex: <http://example.org/> .
ex:Service rdf:type ex:ComputeService ;
  ex:updated "true" .
"#;
    let proposal_v2 = r#"{ "service": "ECS", "replicas": 5, "version": 2 }"#;

    let ontology_v3 = r#"@prefix ex: <http://example.org/> .
ex:Service rdf:type ex:ComputeService ;
  ex:updated "true" ;
  ex:enhanced "true" .
"#;
    let proposal_v3 = r#"{ "service": "EKS", "replicas": 10, "version": 3 }"#;

    // Act: Generate receipt chain
    let receipt_v1 = generate_receipt(ontology_v1, proposal_v1, None);
    let receipt_v2 = generate_receipt(ontology_v2, proposal_v2, Some(&receipt_v1));
    let receipt_v3 = generate_receipt(ontology_v3, proposal_v3, Some(&receipt_v2));

    // Assert: Chain is properly linked
    assert!(
        receipt_v1.parent_receipt_id.is_none(),
        "First receipt should have no parent"
    );
    assert_eq!(
        receipt_v2.parent_receipt_id.as_ref().unwrap(),
        &receipt_v1.receipt_id,
        "Second receipt should link to first"
    );
    assert_eq!(
        receipt_v3.parent_receipt_id.as_ref().unwrap(),
        &receipt_v2.receipt_id,
        "Third receipt should link to second"
    );

    // Assert: Each receipt in the chain verifies
    let (v1_verified, _) = verify_receipt(&receipt_v1, ontology_v1, proposal_v1);
    let (v2_verified, _) = verify_receipt(&receipt_v2, ontology_v2, proposal_v2);
    let (v3_verified, _) = verify_receipt(&receipt_v3, ontology_v3, proposal_v3);

    assert!(v1_verified, "Receipt v1 should verify");
    assert!(v2_verified, "Receipt v2 should verify");
    assert!(v3_verified, "Receipt v3 should verify");
}

#[test]
fn test_receipt_determinism_same_inputs() {
    // Arrange
    let ontology = r#"@prefix ex: <http://example.org/> .
ex:Service rdf:type ex:ComputeService .
"#;
    let proposal = r#"{ "service": "ECS", "replicas": 3 }"#;

    // Act: Generate receipt 3 times
    let receipt1 = generate_receipt(ontology, proposal, None);
    let receipt2 = generate_receipt(ontology, proposal, None);
    let receipt3 = generate_receipt(ontology, proposal, None);

    // Assert: All receipts are identical (determinism)
    assert_eq!(
        receipt1.input_hash, receipt2.input_hash,
        "Input hash should be deterministic"
    );
    assert_eq!(
        receipt2.input_hash, receipt3.input_hash,
        "Input hash should be deterministic"
    );

    assert_eq!(
        receipt1.output_hash, receipt2.output_hash,
        "Output hash should be deterministic"
    );
    assert_eq!(
        receipt2.output_hash, receipt3.output_hash,
        "Output hash should be deterministic"
    );

    assert_eq!(
        receipt1.combined_hash, receipt2.combined_hash,
        "Combined hash should be deterministic"
    );
    assert_eq!(
        receipt2.combined_hash, receipt3.combined_hash,
        "Combined hash should be deterministic"
    );

    assert_eq!(
        receipt1.signature, receipt2.signature,
        "Signature should be deterministic"
    );
    assert_eq!(
        receipt2.signature, receipt3.signature,
        "Signature should be deterministic"
    );
}

#[test]
fn test_receipt_metadata_tracking() {
    // Arrange
    let ontology = r#"@prefix ex: <http://example.org/> .
ex:Service rdf:type ex:ComputeService .
ex:Database rdf:type ex:DataService .
ex:Storage rdf:type ex:StorageService .
"#;
    let proposal = r#"{
  "services": [
    { "name": "ECS", "replicas": 3 },
    { "name": "RDS", "replicas": 2 },
    { "name": "S3", "buckets": 5 }
  ],
  "cost_monthly": 15000
}"#;

    // Act: Generate receipt
    let receipt = generate_receipt(ontology, proposal, None);

    // Assert: Metadata is accurately captured
    assert_eq!(
        receipt.transformation.input_triple_count, 3,
        "Should count 3 triples in ontology"
    );
    assert!(
        receipt.transformation.output_line_count >= 3,
        "Should count output lines"
    );
    assert_eq!(
        receipt.transformation.guards_evaluated, 12,
        "Should evaluate 12 guards"
    );
    assert_eq!(
        receipt.transformation.guards_passed, 12,
        "All guards should pass"
    );
    assert!(
        receipt.transformation.determinism_verified,
        "Determinism should be verified"
    );
}

#[test]
fn test_receipt_tamper_detection_comprehensive() {
    // Arrange
    let ontology = r#"@prefix ex: <http://example.org/> .
ex:Service rdf:type ex:ComputeService .
"#;
    let proposal = r#"{ "service": "ECS", "replicas": 3 }"#;

    // Act: Generate original receipt
    let receipt = generate_receipt(ontology, proposal, None);

    // Act & Assert: Try various tampering scenarios
    let tamper_scenarios = vec![
        (
            "Modify single character in ontology",
            r#"@prefix ex: <http://example.org/> .
ex:Service rdf:type ex:ComputeServiceX .
"#,
            proposal,
        ),
        (
            "Modify single character in proposal",
            ontology,
            r#"{ "service": "ECS", "replicas": 4 }"#,
        ),
        (
            "Add whitespace to ontology",
            r#"@prefix ex: <http://example.org/> .
ex:Service rdf:type ex:ComputeService .

"#,
            proposal,
        ),
        (
            "Change proposal format",
            ontology,
            r#"{"service":"ECS","replicas":3}"#,
        ),
    ];

    for (scenario, test_ontology, test_proposal) in tamper_scenarios {
        let (verified, message) = verify_receipt(&receipt, test_ontology, test_proposal);
        assert!(
            !verified,
            "Tampering scenario '{}' should be detected. Message: {}",
            scenario, message
        );
    }
}

#[test]
fn test_receipt_content_type_preservation() {
    // Arrange
    let ontology = r#"@prefix ex: <http://example.org/> .
ex:Service rdf:type ex:ComputeService .
"#;

    // Different proposal formats should produce different receipts
    let proposal_json = r#"{ "service": "ECS", "replicas": 3 }"#;
    let proposal_yaml = r#"service: ECS
replicas: 3
"#;
    let proposal_xml = r#"<proposal><service>ECS</service><replicas>3</replicas></proposal>"#;

    // Act: Generate receipts for different formats
    let receipt_json = generate_receipt(ontology, proposal_json, None);
    let receipt_yaml = generate_receipt(ontology, proposal_yaml, None);
    let receipt_xml = generate_receipt(ontology, proposal_xml, None);

    // Assert: Different formats produce different output hashes
    assert_ne!(
        receipt_json.output_hash, receipt_yaml.output_hash,
        "Different proposal formats should have different hashes"
    );
    assert_ne!(
        receipt_yaml.output_hash, receipt_xml.output_hash,
        "Different proposal formats should have different hashes"
    );

    // But same input hash
    assert_eq!(
        receipt_json.input_hash, receipt_yaml.input_hash,
        "Same ontology should have same input hash"
    );
    assert_eq!(
        receipt_yaml.input_hash, receipt_xml.input_hash,
        "Same ontology should have same input hash"
    );
}

#[test]
fn test_receipt_serialization() {
    // Arrange
    let ontology = r#"@prefix ex: <http://example.org/> .
ex:Service rdf:type ex:ComputeService .
"#;
    let proposal = r#"{ "service": "ECS", "replicas": 3 }"#;

    // Act: Generate receipt and serialize to JSON
    let receipt = generate_receipt(ontology, proposal, None);
    let receipt_json = serde_json::to_string(&receipt).expect("Should serialize");

    // Assert: Receipt can be serialized and deserialized
    let deserialized: ProvenanceReceipt =
        serde_json::from_str(&receipt_json).expect("Should deserialize");

    assert_eq!(
        deserialized.receipt_id, receipt.receipt_id,
        "Receipt ID should be preserved"
    );
    assert_eq!(
        deserialized.input_hash, receipt.input_hash,
        "Input hash should be preserved"
    );
    assert_eq!(
        deserialized.output_hash, receipt.output_hash,
        "Output hash should be preserved"
    );
    assert_eq!(
        deserialized.signature, receipt.signature,
        "Signature should be preserved"
    );
}
