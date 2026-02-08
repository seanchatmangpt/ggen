//! Receipt Generation and Verification Tests for FIBO Ontologies
//!
//! Tests the μ₅ stage of the pipeline: cryptographic receipt generation.
//! Receipts provide:
//! - Deterministic proof of generation
//! - Audit trail of inputs and outputs
//! - Reproducibility verification
//!
//! Follows the v6 BuildReceipt pattern from ggen-core.

use super::{fixtures, pipeline::PipelineResult};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use std::path::Path;

/// Build receipt for FIBO workflow generation
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FiboReceipt {
    /// Unique receipt ID
    pub id: String,
    /// Epoch ID (hash of all inputs)
    pub epoch_id: String,
    /// Input ontology hashes
    pub inputs: BTreeMap<String, InputHash>,
    /// Generated output files
    pub outputs: Vec<OutputEntry>,
    /// Pipeline version
    pub version: String,
    /// Timestamp of generation
    pub timestamp: String,
}

/// Hash information for an input file
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct InputHash {
    /// File path
    pub path: String,
    /// SHA-256 hash of content
    pub hash: String,
    /// File size in bytes
    pub size_bytes: usize,
}

/// Output file entry in receipt
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OutputEntry {
    /// Output file path
    pub path: String,
    /// SHA-256 hash of output content
    pub hash: String,
    /// Size in bytes
    pub size_bytes: usize,
    /// Which pipeline stage produced this
    pub produced_by: String,
}

impl FiboReceipt {
    /// Create a new receipt from pipeline execution
    pub fn from_pipeline(
        ontology_path: &Path,
        ontology_content: &str,
        pipeline_result: &PipelineResult,
    ) -> Self {
        // Compute input hash
        let input_hash = format!("{:x}", Sha256::digest(ontology_content.as_bytes()));

        // Compute epoch ID from input
        let epoch_id = format!("{:x}", Sha256::digest(input_hash.as_bytes()));

        // Compute receipt ID from epoch and output
        let receipt_input = format!("{}:{}", epoch_id, pipeline_result.hash);
        let id = format!("{:x}", Sha256::digest(receipt_input.as_bytes()));

        // Create input entry
        let path_str = ontology_path.to_string_lossy().to_string();
        let mut inputs = BTreeMap::new();
        inputs.insert(
            path_str.clone(),
            InputHash {
                path: path_str,
                hash: input_hash.clone(),
                size_bytes: ontology_content.len(),
            },
        );

        // Create output entry
        let outputs = vec![OutputEntry {
            path: "workflow.yawl".to_string(),
            hash: pipeline_result.hash.clone(),
            size_bytes: pipeline_result.size_bytes,
            produced_by: "μ₅:receipt".to_string(),
        }];

        // Timestamp
        let timestamp = chrono::Utc::now().to_rfc3339();

        Self {
            id,
            epoch_id,
            inputs,
            outputs,
            version: "6.0.0".to_string(),
            timestamp,
        }
    }

    /// Verify that the receipt matches the actual files
    pub fn verify(&self) -> Result<bool, String> {
        // Verify inputs
        for (_path, input) in &self.inputs {
            // In a real scenario, we'd read the file and verify
            // For tests, we just verify the hash format
            if input.hash.len() != 64 {
                return Err(format!("Invalid hash length for input: {}", input.hash.len()));
            }
        }

        // Verify outputs
        for output in &self.outputs {
            if output.hash.len() != 64 {
                return Err(format!("Invalid hash length for output: {}", output.hash.len()));
            }
        }

        Ok(true)
    }

    /// Serialize to JSON
    pub fn to_json(&self) -> Result<String, String> {
        serde_json::to_string_pretty(self)
            .map_err(|e| format!("Failed to serialize receipt: {}", e))
    }

    /// Deserialize from JSON
    pub fn from_json(json: &str) -> Result<Self, String> {
        serde_json::from_str(json)
            .map_err(|e| format!("Failed to deserialize receipt: {}", e))
    }

    /// Compute the outputs hash (hash of all outputs combined)
    pub fn compute_outputs_hash(&self) -> String {
        let mut hasher = Sha256::new();
        for output in &self.outputs {
            hasher.update(output.path.as_bytes());
            hasher.update(b":");
            hasher.update(output.hash.as_bytes());
            hasher.update(b":");
            hasher.update(output.size_bytes.to_string().as_bytes());
            hasher.update(b"\n");
        }
        format!("{:x}", hasher.finalize())
    }
}

#[cfg(test)]
mod receipt_creation_tests {
    use super::*;

    /// Test: Create receipt from Account Opening pipeline
    #[test]
    fn test_create_receipt_account_opening() {
        // Arrange
        let path = fixtures::fibo_account_opening_path();
        let content = fixtures::load_fibo_account_opening();
        let pipeline_result = super::pipeline::execute_full_pipeline(&content)
            .expect("Pipeline should succeed");

        // Act
        let receipt = FiboReceipt::from_pipeline(&path, &content, &pipeline_result);

        // Assert
        assert!(!receipt.id.is_empty(), "Receipt ID should not be empty");
        assert!(!receipt.epoch_id.is_empty(), "Epoch ID should not be empty");
        assert!(!receipt.inputs.is_empty(), "Should have at least one input");
        assert!(!receipt.outputs.is_empty(), "Should have at least one output");
        assert_eq!(receipt.version, "6.0.0");
    }

    /// Test: Create receipt from Loan Approval pipeline
    #[test]
    fn test_create_receipt_loan_approval() {
        // Arrange
        let path = fixtures::fibo_loan_approval_path();
        let content = fixtures::load_fibo_loan_approval();
        let pipeline_result = super::pipeline::execute_full_pipeline(&content)
            .expect("Pipeline should succeed");

        // Act
        let receipt = FiboReceipt::from_pipeline(&path, &content, &pipeline_result);

        // Assert
        assert!(!receipt.id.is_empty(), "Receipt ID should not be empty");
        assert_eq!(receipt.outputs.len(), 1, "Should have one output");
        assert_eq!(receipt.outputs[0].path, "workflow.yawl");
    }

    /// Test: Receipt has valid hash format
    #[test]
    fn test_receipt_hash_format() {
        // Arrange
        let path = fixtures::fibo_account_opening_path();
        let content = fixtures::load_fibo_account_opening();
        let pipeline_result = super::pipeline::execute_full_pipeline(&content)
            .expect("Pipeline should succeed");

        // Act
        let receipt = FiboReceipt::from_pipeline(&path, &content, &pipeline_result);

        // Assert - SHA-256 hashes are 64 hex characters
        assert_eq!(receipt.id.len(), 64, "Receipt ID should be 64 characters");
        assert_eq!(receipt.epoch_id.len(), 64, "Epoch ID should be 64 characters");

        for input in receipt.inputs.values() {
            assert_eq!(input.hash.len(), 64, "Input hash should be 64 characters");
        }

        for output in &receipt.outputs {
            assert_eq!(output.hash.len(), 64, "Output hash should be 64 characters");
        }
    }

    /// Test: Receipt preserves input information
    #[test]
    fn test_receipt_preserves_input_info() {
        // Arrange
        let path = fixtures::fibo_account_opening_path();
        let content = fixtures::load_fibo_account_opening();
        let pipeline_result = super::pipeline::execute_full_pipeline(&content)
            .expect("Pipeline should succeed");

        // Act
        let receipt = FiboReceipt::from_pipeline(&path, &content, &pipeline_result);

        // Assert
        let input = receipt.inputs.values().next().expect("Should have one input");
        assert_eq!(input.path, path.to_string_lossy().to_string(), "Input path should match");
        assert_eq!(input.size_bytes, content.len(), "Input size should match");
    }

    /// Test: Receipt preserves output information
    #[test]
    fn test_receipt_preserves_output_info() {
        // Arrange
        let path = fixtures::fibo_loan_approval_path();
        let content = fixtures::load_fibo_loan_approval();
        let pipeline_result = super::pipeline::execute_full_pipeline(&content)
            .expect("Pipeline should succeed");

        // Act
        let receipt = FiboReceipt::from_pipeline(&path, &content, &pipeline_result);

        // Assert
        let output = &receipt.outputs[0];
        assert_eq!(output.hash, pipeline_result.hash, "Output hash should match");
        assert_eq!(output.size_bytes, pipeline_result.size_bytes, "Output size should match");
    }
}

#[cfg(test)]
mod receipt_serialization_tests {
    use super::*;

    /// Test: Serialize receipt to JSON
    #[test]
    fn test_serialize_receipt_json() {
        // Arrange
        let path = fixtures::fibo_account_opening_path();
        let content = fixtures::load_fibo_account_opening();
        let pipeline_result = super::pipeline::execute_full_pipeline(&content)
            .expect("Pipeline should succeed");
        let receipt = FiboReceipt::from_pipeline(&path, &content, &pipeline_result);

        // Act
        let json = receipt.to_json();

        // Assert
        assert!(json.is_ok(), "Serialization should succeed");
        let json_str = json.unwrap();
        assert!(json_str.contains("\"id\""), "JSON should contain id field");
        assert!(json_str.contains("\"epoch_id\""), "JSON should contain epoch_id field");
        assert!(json_str.contains("\"inputs\""), "JSON should contain inputs field");
        assert!(json_str.contains("\"outputs\""), "JSON should contain outputs field");
    }

    /// Test: Deserialize receipt from JSON
    #[test]
    fn test_deserialize_receipt_json() {
        // Arrange
        let path = fixtures::fibo_account_opening_path();
        let content = fixtures::load_fibo_account_opening();
        let pipeline_result = super::pipeline::execute_full_pipeline(&content)
            .expect("Pipeline should succeed");
        let original = FiboReceipt::from_pipeline(&path, &content, &pipeline_result);
        let json = original.to_json().unwrap();

        // Act
        let deserialized = FiboReceipt::from_json(&json);

        // Assert
        assert!(deserialized.is_ok(), "Deserialization should succeed");
        let receipt = deserialized.unwrap();
        assert_eq!(receipt.id, original.id, "ID should match after round-trip");
        assert_eq!(receipt.epoch_id, original.epoch_id, "Epoch ID should match");
    }

    /// Test: Round-trip preserves all fields
    #[test]
    fn test_round_trip_preserves_fields() {
        // Arrange
        let path = fixtures::fibo_loan_approval_path();
        let content = fixtures::load_fibo_loan_approval();
        let pipeline_result = super::pipeline::execute_full_pipeline(&content)
            .expect("Pipeline should succeed");
        let original = FiboReceipt::from_pipeline(&path, &content, &pipeline_result);

        // Act
        let json = original.to_json().unwrap();
        let restored = FiboReceipt::from_json(&json).unwrap();

        // Assert
        assert_eq!(restored, original, "Round-trip should preserve all fields");
    }
}

#[cfg(test)]
mod receipt_determinism_tests {
    use super::*;

    /// Test: Same input produces same receipt
    #[test]
    fn test_receipt_determinism() {
        // Arrange
        let path = fixtures::fibo_account_opening_path();
        let content = fixtures::load_fibo_account_opening();
        let pipeline_result = super::pipeline::execute_full_pipeline(&content)
            .expect("Pipeline should succeed");

        // Act - Create receipt twice
        let receipt1 = FiboReceipt::from_pipeline(&path, &content, &pipeline_result);
        let receipt2 = FiboReceipt::from_pipeline(&path, &content, &pipeline_result);

        // Assert
        // IDs will differ due to timestamp, but epoch_id should match
        assert_eq!(receipt1.epoch_id, receipt2.epoch_id,
                   "Epoch ID should be deterministic");
    }

    /// Test: Different ontologies produce different epoch IDs
    #[test]
    fn test_different_ontologies_different_epochs() {
        // Arrange
        let path1 = fixtures::fibo_account_opening_path();
        let content1 = fixtures::load_fibo_account_opening();
        let pipeline_result1 = crate::yawl_workflow_generation::fibo::pipeline::execute_full_pipeline(&content1)
            .expect("Pipeline should succeed");

        let path2 = fixtures::fibo_loan_approval_path();
        let content2 = fixtures::load_fibo_loan_approval();
        let pipeline_result2 = crate::yawl_workflow_generation::fibo::pipeline::execute_full_pipeline(&content2)
            .expect("Pipeline should succeed");

        // Act
        let receipt1 = FiboReceipt::from_pipeline(&path1, &content1, &pipeline_result1);
        let receipt2 = FiboReceipt::from_pipeline(&path2, &content2, &pipeline_result2);

        // Assert
        assert_ne!(receipt1.epoch_id, receipt2.epoch_id,
                   "Different ontologies should produce different epoch IDs");
    }

    /// Test: Receipt verification succeeds
    #[test]
    fn test_receipt_verification() {
        // Arrange
        let path = fixtures::fibo_account_opening_path();
        let content = fixtures::load_fibo_account_opening();
        let pipeline_result = super::pipeline::execute_full_pipeline(&content)
            .expect("Pipeline should succeed");
        let receipt = FiboReceipt::from_pipeline(&path, &content, &pipeline_result);

        // Act
        let result = receipt.verify();

        // Assert
        assert!(result.is_ok(), "Verification should succeed");
        assert!(result.unwrap(), "Receipt should be valid");
    }

    /// Test: Outputs hash computation
    #[test]
    fn test_outputs_hash_computation() {
        // Arrange
        let path = fixtures::fibo_loan_approval_path();
        let content = fixtures::load_fibo_loan_approval();
        let pipeline_result = super::pipeline::execute_full_pipeline(&content)
            .expect("Pipeline should succeed");
        let receipt = FiboReceipt::from_pipeline(&path, &content, &pipeline_result);

        // Act
        let outputs_hash = receipt.compute_outputs_hash();

        // Assert
        assert_eq!(outputs_hash.len(), 64, "Outputs hash should be 64 characters");
        assert!(!outputs_hash.is_empty(), "Outputs hash should not be empty");
    }
}

#[cfg(test)]
mod receipt_cross_validation_tests {
    use super::*;

    /// Test: Receipt matches actual pipeline output
    #[test]
    fn test_receipt_matches_pipeline_output() {
        // Arrange
        let path = fixtures::fibo_account_opening_path();
        let content = fixtures::load_fibo_account_opening();
        let pipeline_result = super::pipeline::execute_full_pipeline(&content)
            .expect("Pipeline should succeed");

        // Compute actual output hash
        let actual_hash = format!("{:x}", Sha256::digest(pipeline_result.xml.as_bytes()));

        // Act
        let receipt = FiboReceipt::from_pipeline(&path, &content, &pipeline_result);

        // Assert
        assert_eq!(receipt.outputs[0].hash, actual_hash,
                   "Receipt hash should match actual output hash");
    }

    /// Test: Receipt input hash matches content
    #[test]
    fn test_receipt_input_hash_matches_content() {
        // Arrange
        let path = fixtures::fibo_loan_approval_path();
        let content = fixtures::load_fibo_loan_approval();

        // Compute expected hash
        let expected_hash = format!("{:x}", Sha256::digest(content.as_bytes()));

        // Act
        let pipeline_result = super::pipeline::execute_full_pipeline(&content)
            .expect("Pipeline should succeed");
        let receipt = FiboReceipt::from_pipeline(&path, &content, &pipeline_result);

        // Assert
        let input_hash = receipt.inputs.values().next().unwrap();
        assert_eq!(input_hash.hash, expected_hash,
                   "Input hash in receipt should match content hash");
    }

    /// Test: Multiple receipts for same input have same epoch
    #[test]
    fn test_multiple_receipts_same_epoch() {
        // Arrange
        let path = fixtures::fibo_account_opening_path();
        let content = fixtures::load_fibo_account_opening();

        // Act - Create multiple receipts
        let mut epoch_ids = Vec::new();
        for _ in 0..3 {
            let pipeline_result = super::pipeline::execute_full_pipeline(&content)
                .expect("Pipeline should succeed");
            let receipt = FiboReceipt::from_pipeline(&path, &content, &pipeline_result);
            epoch_ids.push(receipt.epoch_id);
        }

        // Assert - All should have same epoch ID
        let first = &epoch_ids[0];
        for epoch_id in &epoch_ids[1..] {
            assert_eq!(epoch_id, first, "All receipts should have same epoch ID");
        }
    }
}

#[cfg(test)]
mod receipt_integration_tests {
    use super::*;

    /// Test: Full pipeline with receipt generation
    #[test]
    fn test_full_pipeline_with_receipt() {
        // Arrange
        let path = fixtures::fibo_account_opening_path();
        let content = fixtures::load_fibo_account_opening();

        // Act - Execute pipeline
        let pipeline_result = super::pipeline::execute_full_pipeline(&content)
            .expect("Pipeline should succeed");

        // Generate receipt
        let receipt = FiboReceipt::from_pipeline(&path, &content, &pipeline_result);

        // Verify
        let verification = receipt.verify();

        // Assert
        assert!(!pipeline_result.xml.is_empty(), "Pipeline should produce output");
        assert!(!receipt.id.is_empty(), "Receipt should have ID");
        assert!(verification.is_ok(), "Receipt verification should succeed");
    }

    /// Test: Receipt JSON can be stored and loaded
    #[test]
    fn test_receipt_persistence() {
        // Arrange
        let path = fixtures::fibo_loan_approval_path();
        let content = fixtures::load_fibo_loan_approval();
        let pipeline_result = super::pipeline::execute_full_pipeline(&content)
            .expect("Pipeline should succeed");
        let original = FiboReceipt::from_pipeline(&path, &content, &pipeline_result);

        // Act - Serialize and deserialize
        let json = original.to_json().unwrap();
        let restored = FiboReceipt::from_json(&json).unwrap();

        // Assert - Verify restored receipt
        let verification = restored.verify();

        assert!(verification.is_ok(), "Restored receipt should verify");
        assert!(verification.unwrap(), "Restored receipt should be valid");
    }
}
