//! μ₅ (Receipt): Cryptographic proof generation, audit trail
//!
//! This final stage generates a cryptographic receipt proving
//! the deterministic relationship between input RDF and output code.

use crate::error::{CraftplanError, Result};
use crate::models::GenerationReceipt;
use crate::models::ReceiptMetadata;
use serde_json;
use std::collections::BTreeMap;
use std::fs;
use std::path::PathBuf;
use sha2::{Digest, Sha256};
use tracing::{info, instrument};

/// Receipt generator for audit trail
///
/// Generates cryptographic proofs and audit trails for
/// reproducibility verification.
pub struct ReceiptGenerator;

impl ReceiptGenerator {
    /// Create a new receipt generator
    pub fn new() -> Self {
        Self
    }

    /// Generate a receipt for the generation run
    ///
    /// # Arguments
    /// * `input_path` - Path to input RDF file
    /// * `output_files` - Paths to generated files
    /// * `entity_count` - Number of entities processed
    /// * `duration_ms` - Pipeline execution time
    ///
    /// # Returns
    /// * `Ok(GenerationReceipt)` - Generated receipt
    /// * `Err(CraftplanError)` - Receipt generation failed
    #[instrument(skip(self))]
    pub fn generate(
        &self,
        input_path: &str,
        output_files: &[String],
        entity_count: usize,
        duration_ms: u64,
    ) -> Result<GenerationReceipt> {
        info!("Generating receipt for {} output files", output_files.len());

        // Compute input hash
        let input_hash = self.hash_file(input_path)?;

        // Compute output hashes
        let mut output_hashes = BTreeMap::new();
        for file_path in output_files {
            let hash = self.hash_file(file_path)?;
            output_hashes.insert(file_path.clone(), hash);
        }

        // Build metadata
        let metadata = ReceiptMetadata {
            timestamp: chrono::Utc::now().to_rfc3339(),
            generator_version: env!("CARGO_PKG_VERSION").to_string(),
            entity_count,
            file_count: output_files.len(),
            duration_ms,
            stages: vec![
                "μ₁ (Normalize)".to_string(),
                "μ₂ (Extract)".to_string(),
                "μ₃ (Emit)".to_string(),
                "μ₄ (Canonicalize)".to_string(),
                "μ₅ (Receipt)".to_string(),
            ],
        };

        // Build receipt
        let receipt_json = serde_json::to_string_pretty(&metadata).map_err(|e| {
            CraftplanError::ReceiptGeneration {
                reason: format!("Failed to serialize receipt metadata: {}", e),
            }
        })?;

        let receipt_hash = self.compute_hash(&receipt_json);

        let receipt = GenerationReceipt {
            input_hash,
            output_hashes,
            metadata,
            receipt_hash,
        };

        info!("Receipt generated: {}", receipt.receipt_hash);
        Ok(receipt)
    }

    /// Write receipt to file
    ///
    /// # Arguments
    /// * `receipt` - Receipt to write
    /// * `output_path` - Path to write receipt to
    pub fn write_receipt(&self, receipt: &GenerationReceipt, output_path: &str) -> Result<()> {
        let json = serde_json::to_string_pretty(receipt).map_err(|e| {
            CraftplanError::ReceiptGeneration {
                reason: format!("Failed to serialize receipt: {}", e),
            }
        })?;

        fs::write(output_path, json).map_err(|e| CraftplanError::FileError {
            path: output_path.into(),
            message: format!("Failed to write receipt: {}", e),
            source: Some(e),
        })?;

        info!("Receipt written to: {}", output_path);
        Ok(())
    }

    /// Verify a receipt against current files
    ///
    /// # Arguments
    /// * `receipt` - Receipt to verify
    /// * `input_path` - Current input RDF file
    /// * `output_files` - Current output files
    ///
    /// # Returns
    /// * `Ok(true)` - Receipt is valid
    /// * `Ok(false)` - Receipt is invalid
    /// * `Err(CraftplanError)` - Verification failed
    pub fn verify(
        &self,
        receipt: &GenerationReceipt,
        input_path: &str,
        output_files: &[String],
    ) -> Result<bool> {
        info!("Verifying receipt: {}", receipt.receipt_hash);

        // Verify input hash
        let current_input_hash = self.hash_file(input_path)?;
        if current_input_hash != receipt.input_hash {
            return Ok(false);
        }

        // Verify output hashes
        for (file_path, expected_hash) in &receipt.output_hashes {
            if !output_files.contains(file_path) {
                return Ok(false);
            }

            let current_hash = self.hash_file(file_path)?;
            if current_hash != *expected_hash {
                return Ok(false);
            }
        }

        Ok(true)
    }

    /// Compute hash of a file
    fn hash_file(&self, file_path: &str) -> Result<String> {
        let content = fs::read(file_path).map_err(|e| CraftplanError::FileError {
            path: file_path.into(),
            message: format!("Failed to read file for hashing: {}", e),
            source: Some(e),
        })?;

        Ok(self.compute_hash_bytes(&content))
    }

    /// Compute SHA-256 hash of bytes
    fn compute_hash_bytes(&self, bytes: &[u8]) -> String {
        let mut hasher = Sha256::new();
        hasher.update(bytes);
        format!("{:x}", hasher.finalize())
    }

    /// Compute SHA-256 hash of string
    fn compute_hash(&self, content: &str) -> String {
        self.compute_hash_bytes(content.as_bytes())
    }
}

impl Default for ReceiptGenerator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_receipt_generator_creation() {
        let generator = ReceiptGenerator::new();
        // Just verify it exists
    }

    #[test]
    fn test_compute_hash() {
        let generator = ReceiptGenerator::new();

        let hash1 = generator.compute_hash("test content");
        let hash2 = generator.compute_hash("test content");
        let hash3 = generator.compute_hash("different content");

        assert_eq!(hash1, hash2);
        assert_ne!(hash1, hash3);
    }
}
