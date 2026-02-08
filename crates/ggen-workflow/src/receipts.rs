//! Workflow receipt generation and verification
//!
//! This module provides cryptographic receipt generation for workflow executions
//! ensuring deterministic outputs and audit trail capabilities.

use crate::{error::{WorkflowError}, WorkflowContext};
use crate::patterns::TraceEvent;
use sha2::{Sha256, Digest};
use serde::{Serialize, Deserialize};
use std::collections::HashMap;

/// Receipt generation errors
pub type ReceiptResult<T> = Result<T, WorkflowError>;

/// Cryptographic receipt for workflow execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowReceipt {
    /// Receipt ID
    pub receipt_id: String,
    /// Workflow ID
    pub workflow_id: String,
    /// Execution timestamp
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Input data hash
    pub input_hash: String,
    /// Output data hash
    pub output_hash: String,
    /// Trace data hash
    pub trace_hash: String,
    /// Execution metadata
    pub metadata: ReceiptMetadata,
    /// Digital signature
    pub signature: String,
}

/// Metadata for receipts
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReceiptMetadata {
    /// Total execution time in milliseconds
    pub execution_time_ms: u64,
    /// Number of steps executed
    pub steps_count: usize,
    /// Memory usage in bytes
    pub memory_usage_bytes: u64,
    /// Peak memory usage in bytes
    pub peak_memory_bytes: u64,
    /// CPU time used
    pub cpu_time_ms: u64,
}

/// Receipt generator for workflow executions
#[derive(Clone)]
pub struct ReceiptGenerator {
    /// Private key for signing (in production, this would be properly secured)
    private_key: [u8; 32],
    /// Hash algorithm
    hash_algorithm: HashAlgorithm,
}

/// Hash algorithm selection
#[derive(Debug, Clone)]
pub enum HashAlgorithm {
    /// SHA-256 (default)
    Sha256,
    /// SHA-512
    Sha512,
}

impl Default for HashAlgorithm {
    fn default() -> Self {
        HashAlgorithm::Sha256
    }
}

impl ReceiptGenerator {
    /// Create a new receipt generator
    pub fn new() -> Self {
        // In production, this would use proper key management
        let mut private_key = [0u8; 32];
        private_key[0..16].copy_from_slice(b"receipt_generator_key");

        ReceiptGenerator {
            private_key,
            hash_algorithm: HashAlgorithm::default(),
        }
    }

    /// Generate a receipt for workflow execution
    pub fn generate_receipt(&self, context: &WorkflowContext) -> ReceiptResult<WorkflowReceipt> {
        let receipt_id = self.generate_receipt_id();
        let input_hash = self.hash_data(&context.input);
        let output_hash = self.hash_data(&context.output);
        let trace_hash = self.hash_trace(&context.metadata.trace);

        let metadata = ReceiptMetadata {
            execution_time_ms: 0, // Would be measured in real implementation
            steps_count: context.metadata.trace.len(),
            memory_usage_bytes: 0,
            peak_memory_bytes: 0,
            cpu_time_ms: 0,
        };

        let signature = self.sign_receipt_data(&receipt_id, &input_hash, &output_hash, &trace_hash)?;

        Ok(WorkflowReceipt {
            receipt_id,
            workflow_id: context.metadata.workflow_id.clone(),
            timestamp: context.metadata.timestamp,
            input_hash,
            output_hash,
            trace_hash,
            metadata,
            signature,
        })
    }

    /// Verify a receipt's integrity
    pub fn verify_receipt(&self, receipt: &WorkflowReceipt) -> ReceiptResult<bool> {
        // Verify receipt ID format
        if receipt.receipt_id.is_empty() || receipt.receipt_id.len() > 64 {
            return Err(WorkflowError::Validation("Invalid receipt ID format".to_string()));
        }

        // Verify hashes are valid
        if receipt.input_hash.len() != 64 || receipt.output_hash.len() != 64 || receipt.trace_hash.len() != 64 {
            return Err(WorkflowError::Validation("Invalid hash format".to_string()));
        }

        // Verify signature
        let expected_signature = self.sign_receipt_data(
            &receipt.receipt_id,
            &receipt.input_hash,
            &receipt.output_hash,
            &receipt.trace_hash,
        )?;

        if expected_signature != receipt.signature {
            return Ok(false);
        }

        // Verify workflow ID matches
        if receipt.workflow_id.is_empty() {
            return Err(WorkflowError::Validation("Missing workflow ID in receipt".to_string()));
        }

        // Verify timestamp is reasonable (not in the future)
        let now = chrono::Utc::now();
        if receipt.timestamp > now {
            return Err(WorkflowError::Validation("Receipt timestamp is in the future".to_string()));
        }

        Ok(true)
    }

    /// Hash arbitrary data
    fn hash_data(&self, data: &HashMap<String, serde_json::Value>) -> String {
        let serialized = serde_json::to_string(data)
            .unwrap_or_else(|_| "{}".to_string());

        match self.hash_algorithm {
            HashAlgorithm::Sha256 => {
                let mut hasher = Sha256::new();
                hasher.update(serialized.as_bytes());
                format!("{:x}", hasher.finalize())
            }
            HashAlgorithm::Sha512 => {
                use sha2::{Sha512, Digest};
                let mut hasher = Sha512::new();
                hasher.update(serialized.as_bytes());
                format!("{:x}", hasher.finalize())
            }
        }
    }

    /// Hash trace data
    fn hash_trace(&self, trace: &[TraceEvent]) -> String {
        let serialized = serde_json::to_string(trace)
            .unwrap_or_else(|_| "[]".to_string());

        match self.hash_algorithm {
            HashAlgorithm::Sha256 => {
                let mut hasher = Sha256::new();
                hasher.update(serialized.as_bytes());
                format!("{:x}", hasher.finalize())
            }
            HashAlgorithm::Sha512 => {
                use sha2::{Sha512, Digest};
                let mut hasher = Sha512::new();
                hasher.update(serialized.as_bytes());
                format!("{:x}", hasher.finalize())
            }
        }
    }

    /// Generate receipt ID
    fn generate_receipt_id(&self) -> String {
        let timestamp = chrono::Utc::now().timestamp();
        let random_bytes: [u8; 8] = rand::random();
        format!("receipt_{}_{}", timestamp, hex::encode(random_bytes))
    }

    /// Sign receipt data
    fn sign_receipt_data(&self, receipt_id: &str, input_hash: &str, output_hash: &str, trace_hash: &str) -> ReceiptResult<String> {
        let data_to_sign = format!("{}{}{}{}", receipt_id, input_hash, output_hash, trace_hash);
        let mut hasher = Sha256::new();
        hasher.update(data_to_sign.as_bytes());

        let signature = hasher.finalize();
        Ok(hex::encode(signature))
    }
}

/// Receipt storage and retrieval
pub struct ReceiptStore {
    /// In-memory storage (in production, this would be persistent)
    pub receipts: HashMap<String, WorkflowReceipt>,
}

impl ReceiptStore {
    /// Create a new receipt store
    pub fn new() -> Self {
        ReceiptStore {
            receipts: HashMap::new(),
        }
    }

    /// Store a receipt
    pub fn store_receipt(&mut self, receipt: WorkflowReceipt) -> ReceiptResult<()> {
        self.receipts.insert(receipt.receipt_id.clone(), receipt);
        Ok(())
    }

    /// Retrieve a receipt by ID
    pub fn get_receipt(&self, receipt_id: &str) -> Option<&WorkflowReceipt> {
        self.receipts.get(receipt_id)
    }

    /// List all receipts
    pub fn list_receipts(&self) -> Vec<&WorkflowReceipt> {
        self.receipts.values().collect()
    }

    /// Remove a receipt
    pub fn remove_receipt(&mut self, receipt_id: &str) -> Option<WorkflowReceipt> {
        self.receipts.remove(receipt_id)
    }

    /// Verify receipt exists and is valid
    pub fn verify_receipt_exists(&self, receipt_id: &str, generator: &ReceiptGenerator) -> ReceiptResult<bool> {
        match self.get_receipt(receipt_id) {
            Some(receipt) => generator.verify_receipt(receipt),
            None => Ok(false),
        }
    }
}

impl Default for ReceiptStore {
    fn default() -> Self {
        ReceiptStore::new()
    }
}

/// Receipt validation utilities
pub mod receipt_utils {
    use super::*;

    /// Validate receipt timestamp is within acceptable range
    pub fn validate_timestamp(receipt: &WorkflowReceipt, max_age_hours: i64) -> ReceiptResult<()> {
        let now = chrono::Utc::now();
        let max_age = chrono::Duration::hours(max_age_hours);

        if now.signed_duration_since(receipt.timestamp) > max_age {
            return Err(WorkflowError::Validation(format!(
                "Receipt is too old ({} hours), maximum allowed: {} hours",
                now.signed_duration_since(receipt.timestamp).num_hours(),
                max_age_hours
            )));
        }

        Ok(())
    }

    /// Validate receipt execution time is reasonable
    pub fn validate_execution_time(receipt: &WorkflowReceipt, max_time_ms: u64) -> ReceiptResult<()> {
        if receipt.metadata.execution_time_ms > max_time_ms {
            return Err(WorkflowError::Validation(format!(
                "Execution time exceeded maximum: {}ms > {}ms",
                receipt.metadata.execution_time_ms,
                max_time_ms
            )));
        }

        Ok(())
    }

    /// Generate receipt summary for audit
    pub fn generate_summary(receipt: &WorkflowReceipt) -> String {
        format!(
            "Workflow: {}\nReceipt: {}\nTimestamp: {}\nSteps: {}\nExecution Time: {}ms",
            receipt.workflow_id,
            receipt.receipt_id,
            receipt.timestamp.format("%Y-%m-%d %H:%M:%S UTC"),
            receipt.metadata.steps_count,
            receipt.metadata.execution_time_ms
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_receipt_generation() {
        let mut context = WorkflowContext::default();
        context.input.insert("test_key".to_string(), serde_json::json!("test_value"));

        let generator = ReceiptGenerator::new();
        let receipt = generator.generate_receipt(&context).unwrap();

        assert!(!receipt.receipt_id.is_empty());
        assert_eq!(receipt.workflow_id, context.metadata.workflow_id);
        assert!(!receipt.input_hash.is_empty());
        assert!(!receipt.signature.is_empty());
    }

    #[test]
    fn test_receipt_verification() {
        let mut context = WorkflowContext::default();
        context.input.insert("test_key".to_string(), serde_json::json!("test_value"));

        let generator = ReceiptGenerator::new();
        let receipt = generator.generate_receipt(&context).unwrap();

        // Verify the receipt
        let is_valid = generator.verify_receipt(&receipt).unwrap();
        assert!(is_valid);
    }

    #[test]
    fn test_receipt_store() {
        let mut store = ReceiptStore::new();
        let generator = ReceiptGenerator::new();
        let context = WorkflowContext::default();

        let receipt = generator.generate_receipt(&context).unwrap();

        // Store receipt
        store.store_receipt(receipt).unwrap();

        // Retrieve receipt
        let retrieved = store.get_receipt(&receipt.receipt_id).unwrap();
        assert_eq!(receipt.receipt_id, retrieved.receipt_id);

        // List receipts
        let receipts = store.list_receipts();
        assert_eq!(receipts.len(), 1);
    }

    #[test]
    fn test_receipt_validation() {
        let receipt = WorkflowReceipt {
            receipt_id: "test_receipt".to_string(),
            workflow_id: "test_workflow".to_string(),
            timestamp: chrono::Utc::now(),
            input_hash: "test_input_hash".to_string(),
            output_hash: "test_output_hash".to_string(),
            trace_hash: "test_trace_hash".to_string(),
            metadata: ReceiptMetadata {
                execution_time_ms: 1000,
                steps_count: 5,
                memory_usage_bytes: 1024,
                peak_memory_bytes: 2048,
                cpu_time_ms: 500,
            },
            signature: "test_signature".to_string(),
        };

        // Test timestamp validation
        receipt_utils::validate_timestamp(&receipt, 24).unwrap();

        // Test execution time validation
        receipt_utils::validate_execution_time(&receipt, 2000).unwrap();

        // Test summary generation
        let summary = receipt_utils::generate_summary(&receipt);
        assert!(summary.contains("Workflow: test_workflow"));
        assert!(summary.contains("Steps: 5"));
    }
}