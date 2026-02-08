//! Receipt Verification Tests for YAWL Workflow Generation
//!
//! This module tests the receipt generation and verification across the
//! full pipeline μ₁-μ₅ (Normalization, Extraction, Emission, Canonicalization, Receipt).
//!
//! Tests include:
//! - BLAKE3 chain verification (when blake3 feature is enabled)
//! - Determinism verification (same input -> same output)
//! - Independent receipt verification
//! - Tamper detection
//! - Cross-pipeline receipt linking

use ggen_core::v6::{BuildReceipt, Epoch, OntologyInput, OutputFile, PassExecution, PassType};
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

/// Test fixtures for receipt verification
mod fixtures {
    use super::*;

    /// Creates a minimal test ontology with known content
    pub fn create_test_ontology(dir: &Path, name: &str) -> PathBuf {
        let ontology_path = dir.join(format!("{}.ttl", name));
        let content = r#"
            @prefix ex: <http://example.org/> .
            @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
            @prefix owl: <http://www.w3.org/2002/07/owl#> .
            @prefix yawl: <http://www.yawlfoundation.org/yawlbpmn#> .

            ex:TestProcess a owl:Class ;
                rdfs:label "Test Process" ;
                rdfs:subClassOf yawl:WorkflowSpecification .

            ex:TestTask a owl:Class ;
                rdfs:label "Test Task" ;
                rdfs:subClassOf yawl:Task .

            ex:hasTask a rdf:Property ;
                rdfs:domain ex:TestProcess ;
                rdfs:range ex:TestTask .
        "#;
        std::fs::write(&ontology_path, content).unwrap();
        ontology_path
    }

    /// Creates a temporary directory with test setup
    pub fn setup_test_env() -> TempDir {
        TempDir::new().unwrap()
    }

    /// Creates a test receipt from an ontology file
    pub fn create_test_receipt_from_ontology(ontology_path: &Path) -> BuildReceipt {
        let content = std::fs::read(ontology_path).unwrap();
        let hash = format!("{:x}", Sha256::digest(&content));
        let hash_clone = hash.clone();

        let mut inputs = BTreeMap::new();
        inputs.insert(
            ontology_path.to_path_buf(),
            OntologyInput {
                path: ontology_path.to_path_buf(),
                hash,
                size_bytes: content.len(),
                triple_count: 5,
            },
        );

        let epoch = Epoch {
            id: format!("{:x}", Sha256::digest(hash_clone.as_bytes())),
            timestamp: "2024-01-01T00:00:00Z".to_string(),
            inputs,
            total_triples: 5,
        };

        BuildReceipt::new(&epoch, vec![], vec![], "6.0.0")
    }

    /// Creates a full pipeline receipt with all passes
    pub fn create_full_pipeline_receipt(ontology_path: &Path) -> BuildReceipt {
        let content = std::fs::read(ontology_path).unwrap();
        let hash = format!("{:x}", Sha256::digest(&content));

        let mut inputs = BTreeMap::new();
        inputs.insert(
            ontology_path.to_path_buf(),
            OntologyInput {
                path: ontology_path.to_path_buf(),
                hash: hash.clone(),
                size_bytes: content.len(),
                triple_count: 5,
            },
        );

        let epoch = Epoch {
            id: format!("{:x}", Sha256::digest(hash.as_bytes())),
            timestamp: "2024-01-01T00:00:00Z".to_string(),
            inputs,
            total_triples: 5,
        };

        let passes = vec![
            PassExecution {
                name: "μ₁:normalization".to_string(),
                pass_type: PassType::Normalization,
                order_index: 1,
                duration_ms: 50,
                query_hash: None,
                triples_produced: 10,
                files_generated: vec![],
                success: true,
                error: None,
            },
            PassExecution {
                name: "μ₂:extraction".to_string(),
                pass_type: PassType::Extraction,
                order_index: 2,
                duration_ms: 75,
                query_hash: Some(format!("{:x}", Sha256::digest(b"query"))),
                triples_produced: 0,
                files_generated: vec![],
                success: true,
                error: None,
            },
            PassExecution {
                name: "μ₃:emission".to_string(),
                pass_type: PassType::Emission,
                order_index: 3,
                duration_ms: 100,
                query_hash: None,
                triples_produced: 0,
                files_generated: vec![PathBuf::from("output.yawl")],
                success: true,
                error: None,
            },
            PassExecution {
                name: "μ₄:canonicalization".to_string(),
                pass_type: PassType::Canonicalization,
                order_index: 4,
                duration_ms: 25,
                query_hash: None,
                triples_produced: 0,
                files_generated: vec![],
                success: true,
                error: None,
            },
        ];

        BuildReceipt::new(&epoch, passes, vec![], "6.0.0")
    }
}

// ============================================================================
// BLAKE3 CHAIN VERIFICATION TESTS
// ============================================================================

/// Receipt chain for linking receipts in a verifiable chain
#[derive(Debug, Clone)]
pub struct ReceiptChain {
    links: Vec<ReceiptLink>,
    root_hash: Option<String>,
}

/// Receipt chain link
#[derive(Debug, Clone)]
pub struct ReceiptLink {
    pub current_hash: String,
    pub parent_hash: Option<String>,
    pub position: u64,
}

impl ReceiptChain {
    pub fn new() -> Self {
        Self {
            links: Vec::new(),
            root_hash: None,
        }
    }

    pub fn add_receipt(&mut self, receipt: &BuildReceipt) -> Result<(), String> {
        let receipt_bytes = serde_json::to_vec(receipt).unwrap();
        let current_hash = format!("{:x}", Sha256::digest(&receipt_bytes));
        let parent_hash = self.root_hash.clone();
        let position = self.links.len() as u64;

        self.links.push(ReceiptLink {
            current_hash: current_hash.clone(),
            parent_hash,
            position,
        });

        self.root_hash = Some(current_hash);
        Ok(())
    }

    pub fn verify_chain(&self) -> bool {
        for (i, link) in self.links.iter().enumerate() {
            if i > 0 {
                let expected_parent = self.links[i - 1].current_hash.clone();
                if link.parent_hash != Some(expected_parent) {
                    return false;
                }
            } else if link.parent_hash.is_some() {
                return false;
            }

            if link.position != i as u64 {
                return false;
            }
        }
        true
    }

    pub fn len(&self) -> usize {
        self.links.len()
    }

    pub fn is_empty(&self) -> bool {
        self.links.is_empty()
    }
}

impl Default for ReceiptChain {
    fn default() -> Self {
        Self::new()
    }
}

#[test]
fn test_receipt_chain_verification() {
    let mut chain = ReceiptChain::new();
    let temp_dir = fixtures::setup_test_env();

    // Add multiple receipts to chain
    for i in 0..3 {
        let ontology_path =
            fixtures::create_test_ontology(temp_dir.path(), &format!("chain_{}", i));
        let receipt = fixtures::create_test_receipt_from_ontology(&ontology_path);
        chain.add_receipt(&receipt).unwrap();
    }

    assert!(chain.verify_chain(), "Chain should be valid");
    assert_eq!(chain.len(), 3, "Chain should have 3 receipts");
}

#[test]
fn test_empty_chain_valid() {
    let chain = ReceiptChain::new();
    assert!(chain.verify_chain(), "Empty chain should be valid");
    assert!(chain.is_empty());
}

// ============================================================================
// DETERMINISM TESTS
// ============================================================================

#[test]
fn test_receipt_determinism() {
    let temp_dir = fixtures::setup_test_env();
    let ontology_path = fixtures::create_test_ontology(temp_dir.path(), "determinism");

    // Create epoch twice - should have same ID
    let epoch1 = Epoch::create(temp_dir.path(), &[ontology_path.clone()]).unwrap();
    let epoch2 = Epoch::create(temp_dir.path(), &[ontology_path.clone()]).unwrap();

    assert_eq!(epoch1.id, epoch2.id, "Epoch ID should be deterministic");
}

#[test]
fn test_receipt_id_determinism() {
    let temp_dir = fixtures::setup_test_env();
    let ontology_path = fixtures::create_test_ontology(temp_dir.path(), "receipt_id");

    let receipt1 = fixtures::create_test_receipt_from_ontology(&ontology_path);
    let receipt2 = fixtures::create_test_receipt_from_ontology(&ontology_path);

    assert_eq!(
        receipt1.id, receipt2.id,
        "Receipt ID should be deterministic"
    );
}

#[test]
fn test_input_ordering_determinism() {
    let temp_dir = fixtures::setup_test_env();

    let path1 = fixtures::create_test_ontology(temp_dir.path(), "ordering_a");
    let path2 = fixtures::create_test_ontology(temp_dir.path(), "ordering_b");
    let path3 = fixtures::create_test_ontology(temp_dir.path(), "ordering_c");

    let epoch1 = Epoch::create(
        temp_dir.path(),
        &[path1.clone(), path2.clone(), path3.clone()],
    )
    .unwrap();
    let epoch2 = Epoch::create(
        temp_dir.path(),
        &[path3.clone(), path1.clone(), path2.clone()],
    )
    .unwrap();
    let epoch3 = Epoch::create(
        temp_dir.path(),
        &[path2.clone(), path3.clone(), path1.clone()],
    )
    .unwrap();

    assert_eq!(
        epoch1.id, epoch2.id,
        "Epoch ID should be order-independent (1=2)"
    );
    assert_eq!(
        epoch2.id, epoch3.id,
        "Epoch ID should be order-independent (2=3)"
    );
}

// ============================================================================
// TAMPER DETECTION TESTS
// ============================================================================

#[test]
fn test_tampered_input_detection() {
    let temp_dir = fixtures::setup_test_env();

    // Create ontology file
    let ontology_path = fixtures::create_test_ontology(temp_dir.path(), "tamper_input");
    let content = std::fs::read(&ontology_path).unwrap();
    let original_hash = format!("{:x}", Sha256::digest(&content));

    // Create epoch with original hash
    let mut inputs = BTreeMap::new();
    inputs.insert(
        ontology_path.clone(),
        OntologyInput {
            path: ontology_path.clone(),
            hash: original_hash,
            size_bytes: content.len(),
            triple_count: 5,
        },
    );

    let epoch = Epoch {
        id: format!("{:x}", Sha256::digest(b"test")),
        timestamp: "2024-01-01T00:00:00Z".to_string(),
        inputs,
        total_triples: 5,
    };

    // No tampering initially
    assert!(
        epoch.verify(temp_dir.path()).unwrap(),
        "Untampered input should pass"
    );

    // Tamper with the file
    std::fs::write(&ontology_path, b"tampered content").unwrap();

    assert!(
        !epoch.verify(temp_dir.path()).unwrap(),
        "Tampered input should fail"
    );
}

#[test]
fn test_missing_file_detection() {
    let temp_dir = fixtures::setup_test_env();

    // Create epoch with non-existent file
    let mut inputs = BTreeMap::new();
    inputs.insert(
        PathBuf::from("nonexistent.ttl"),
        OntologyInput {
            path: PathBuf::from("nonexistent.ttl"),
            hash: "abcd1234".repeat(8),
            size_bytes: 100,
            triple_count: 5,
        },
    );

    let epoch = Epoch {
        id: format!("{:x}", Sha256::digest(b"test")),
        timestamp: "2024-01-01T00:00:00Z".to_string(),
        inputs,
        total_triples: 5,
    };

    // Verify should return an error for missing file
    let result = epoch.verify(temp_dir.path());
    assert!(
        result.is_err() || !result.unwrap(),
        "Missing file should fail verification"
    );
}

// ============================================================================
// RECEIPT SERIALIZATION TESTS
// ============================================================================

#[test]
fn test_receipt_serialization_roundtrip() {
    let temp_dir = fixtures::setup_test_env();
    let receipt_path = temp_dir.path().join("receipt.json");

    let ontology_path = fixtures::create_test_ontology(temp_dir.path(), "serialize_test");
    let receipt1 = fixtures::create_test_receipt_from_ontology(&ontology_path);

    // Write and read back
    receipt1.write_to_file(&receipt_path).unwrap();
    assert!(receipt_path.exists(), "Receipt file should exist");

    let receipt2 = BuildReceipt::read_from_file(&receipt_path).unwrap();

    assert_eq!(receipt1.id, receipt2.id, "Receipt ID should be preserved");
    assert_eq!(
        receipt1.epoch_id, receipt2.epoch_id,
        "Epoch ID should be preserved"
    );
    assert_eq!(
        receipt1.outputs_hash, receipt2.outputs_hash,
        "Outputs hash should be preserved"
    );
}

// ============================================================================
// FULL PIPELINE CHAIN TESTS
// ============================================================================

#[test]
fn test_full_pipeline_chain() {
    let temp_dir = fixtures::setup_test_env();
    let ontology_path = fixtures::create_test_ontology(temp_dir.path(), "pipeline_chain");

    let receipt = fixtures::create_full_pipeline_receipt(&ontology_path);

    // Verify all passes exist and succeeded
    assert_eq!(receipt.passes.len(), 4, "Should have 4 passes");

    let mu1 = receipt
        .passes
        .iter()
        .find(|p| p.pass_type == PassType::Normalization);
    assert!(mu1.is_some(), "μ₁ should be present");
    assert!(mu1.unwrap().success, "μ₁ should succeed");

    let mu2 = receipt
        .passes
        .iter()
        .find(|p| p.pass_type == PassType::Extraction);
    assert!(mu2.is_some(), "μ₂ should be present");
    assert!(mu2.unwrap().success, "μ₂ should succeed");

    let mu3 = receipt
        .passes
        .iter()
        .find(|p| p.pass_type == PassType::Emission);
    assert!(mu3.is_some(), "μ₃ should be present");
    assert!(mu3.unwrap().success, "μ₃ should succeed");

    let mu4 = receipt
        .passes
        .iter()
        .find(|p| p.pass_type == PassType::Canonicalization);
    assert!(mu4.is_some(), "μ₄ should be present");
    assert!(mu4.unwrap().success, "μ₄ should succeed");

    // Verify receipt is valid
    assert!(receipt.is_valid, "Receipt should be valid");
}

#[test]
fn test_pass_ordering() {
    let temp_dir = fixtures::setup_test_env();
    let ontology_path = fixtures::create_test_ontology(temp_dir.path(), "pass_order");

    let receipt = fixtures::create_full_pipeline_receipt(&ontology_path);

    // Verify passes are in correct order
    let mut last_order = 0;
    for pass in &receipt.passes {
        assert!(
            pass.order_index > last_order,
            "Pass order should be increasing"
        );
        last_order = pass.order_index;
    }
}

#[test]
fn test_duration_consistency() {
    let temp_dir = fixtures::setup_test_env();
    let ontology_path = fixtures::create_test_ontology(temp_dir.path(), "duration");

    let receipt = fixtures::create_full_pipeline_receipt(&ontology_path);

    let sum: u64 = receipt.passes.iter().map(|p| p.duration_ms).sum();

    assert_eq!(
        sum, receipt.total_duration_ms,
        "Total duration should match sum of pass durations"
    );
}

// ============================================================================
// INDEPENDENT VERIFICATION TESTS
// ============================================================================

#[test]
fn test_independent_receipt_verification() {
    let temp_dir = fixtures::setup_test_env();
    let ontology_path = fixtures::create_test_ontology(temp_dir.path(), "independent_verify");

    let receipt = fixtures::create_test_receipt_from_ontology(&ontology_path);

    // Verify receipt structure
    assert!(receipt.is_valid, "Receipt should be valid");
    assert!(!receipt.id.is_empty(), "Receipt ID should not be empty");
    assert!(!receipt.epoch_id.is_empty(), "Epoch ID should not be empty");
}

#[test]
fn test_multiple_outputs_hash_consistency() {
    let temp_dir = fixtures::setup_test_env();
    let ontology_path = fixtures::create_test_ontology(temp_dir.path(), "multi_hash");

    let content = std::fs::read(&ontology_path).unwrap();
    let hash = format!("{:x}", Sha256::digest(&content));

    let mut inputs = BTreeMap::new();
    inputs.insert(
        ontology_path.clone(),
        OntologyInput {
            path: ontology_path.clone(),
            hash,
            size_bytes: content.len(),
            triple_count: 5,
        },
    );

    let epoch = Epoch {
        id: format!("{:x}", Sha256::digest(b"test")),
        timestamp: "2024-01-01T00:00:00Z".to_string(),
        inputs,
        total_triples: 5,
    };

    let outputs = vec![
        OutputFile {
            path: PathBuf::from("model.rs"),
            hash: format!("{:x}", Sha256::digest(b"model")),
            size_bytes: 5,
            produced_by: "μ₃:emission".to_string(),
        },
        OutputFile {
            path: PathBuf::from("api.rs"),
            hash: format!("{:x}", Sha256::digest(b"api")),
            size_bytes: 3,
            produced_by: "μ₃:emission".to_string(),
        },
        OutputFile {
            path: PathBuf::from("types.rs"),
            hash: format!("{:x}", Sha256::digest(b"types")),
            size_bytes: 5,
            produced_by: "μ₃:emission".to_string(),
        },
    ];

    let receipt = BuildReceipt::new(&epoch, vec![], outputs, "6.0.0");

    // Verify outputs_hash is consistent
    let mut hasher = Sha256::new();
    for output in &receipt.outputs {
        hasher.update(output.path.to_string_lossy().as_bytes());
        hasher.update(b":");
        hasher.update(output.hash.as_bytes());
        hasher.update(b"\n");
    }
    let expected_hash = format!("{:x}", hasher.finalize());

    assert_eq!(
        receipt.outputs_hash, expected_hash,
        "Outputs hash should be consistent"
    );
}

#[test]
fn test_receipt_id_computation() {
    let temp_dir = fixtures::setup_test_env();
    let ontology_path = fixtures::create_test_ontology(temp_dir.path(), "id_compute");

    let receipt = fixtures::create_test_receipt_from_ontology(&ontology_path);

    // Manually compute receipt ID
    let mut hasher = Sha256::new();
    hasher.update(receipt.epoch_id.as_bytes());
    hasher.update("6.0.0".as_bytes());
    hasher.update(receipt.outputs_hash.as_bytes());
    let expected_id = format!("{:x}", hasher.finalize());

    assert_eq!(
        receipt.id, expected_id,
        "Receipt ID should be correctly computed"
    );
}
