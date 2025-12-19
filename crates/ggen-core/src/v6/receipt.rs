//! Receipt: Provenance binding outputs to inputs
//!
//! A Receipt cryptographically binds the projection outputs (A) to the
//! frozen inputs (O) via the epoch. This enables verification that
//! hash(A) = hash(μ(O)).

use crate::v6::{Epoch, PassExecution};
use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::path::{Path, PathBuf};

/// A build receipt binding outputs to inputs
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildReceipt {
    /// Unique receipt ID (SHA-256 of all contents)
    pub id: String,

    /// Reference to the input epoch
    pub epoch_id: String,

    /// When the projection was performed (ISO 8601)
    pub timestamp: String,

    /// ggen version used
    pub toolchain_version: String,

    /// Passes that were executed
    pub passes: Vec<PassExecution>,

    /// Output files with hashes
    pub outputs: Vec<OutputFile>,

    /// SHA-256 of all output hashes (for quick verification)
    pub outputs_hash: String,

    /// Whether the projection is valid (hash(A) = hash(μ(O)))
    pub is_valid: bool,

    /// Total execution time in milliseconds
    pub total_duration_ms: u64,

    /// Policies applied during projection
    pub policies: ReceiptPolicies,
}

/// Output file record in a receipt
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OutputFile {
    /// Relative path from output root
    pub path: PathBuf,

    /// SHA-256 hash of file contents
    pub hash: String,

    /// File size in bytes
    pub size_bytes: usize,

    /// Name of the pass that produced this file
    pub produced_by: String,
}

/// Policies enforced during projection
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ReceiptPolicies {
    /// Blank node handling policy
    pub blank_node_policy: String,

    /// Output ordering policy
    pub ordering_policy: String,

    /// Formatting policy
    pub formatting_policy: String,

    /// Guards that were active
    pub active_guards: Vec<String>,
}

impl BuildReceipt {
    /// Create a new build receipt
    ///
    /// # Arguments
    /// * `epoch` - The frozen input epoch
    /// * `passes` - Executed passes
    /// * `outputs` - Generated output files
    /// * `toolchain_version` - ggen version
    pub fn new(
        epoch: &Epoch, passes: Vec<PassExecution>, outputs: Vec<OutputFile>,
        toolchain_version: &str,
    ) -> Self {
        let timestamp = chrono::Utc::now().to_rfc3339();

        // Compute outputs hash
        let outputs_hash = Self::compute_outputs_hash(&outputs);

        // Compute total duration
        let total_duration_ms: u64 = passes.iter().map(|p| p.duration_ms).sum();

        // Check validity (all passes succeeded)
        let is_valid = passes.iter().all(|p| p.success);

        // Compute receipt ID
        let mut receipt = Self {
            id: String::new(), // Will be computed below
            epoch_id: epoch.id.clone(),
            timestamp,
            toolchain_version: toolchain_version.to_string(),
            passes,
            outputs,
            outputs_hash,
            is_valid,
            total_duration_ms,
            policies: ReceiptPolicies::default(),
        };

        receipt.id = receipt.compute_id();
        receipt
    }

    /// Compute the receipt ID from its contents
    fn compute_id(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.epoch_id.as_bytes());
        hasher.update(self.toolchain_version.as_bytes());
        hasher.update(self.outputs_hash.as_bytes());
        format!("{:x}", hasher.finalize())
    }

    /// Compute hash of all output hashes
    fn compute_outputs_hash(outputs: &[OutputFile]) -> String {
        let mut hasher = Sha256::new();
        for output in outputs {
            hasher.update(output.path.to_string_lossy().as_bytes());
            hasher.update(b":");
            hasher.update(output.hash.as_bytes());
            hasher.update(b"\n");
        }
        format!("{:x}", hasher.finalize())
    }

    /// Verify that output files still match their recorded hashes
    ///
    /// # Arguments
    /// * `output_root` - Root directory for output files
    ///
    /// # Returns
    /// * `Ok(true)` - All files match their recorded hashes
    /// * `Ok(false)` - At least one file has changed
    /// * `Err(Error)` - If a file cannot be read
    pub fn verify_outputs(&self, output_root: &Path) -> Result<bool> {
        for output in &self.outputs {
            let full_path = output_root.join(&output.path);
            let current_hash = Self::hash_file(&full_path)?;
            if current_hash != output.hash {
                return Ok(false);
            }
        }
        Ok(true)
    }

    /// Get list of files that have changed since receipt was created
    pub fn get_changed_outputs(&self, output_root: &Path) -> Result<Vec<PathBuf>> {
        let mut changed = Vec::new();

        for output in &self.outputs {
            let full_path = output_root.join(&output.path);
            if !full_path.exists() {
                changed.push(output.path.clone());
                continue;
            }

            let current_hash = Self::hash_file(&full_path)?;
            if current_hash != output.hash {
                changed.push(output.path.clone());
            }
        }

        Ok(changed)
    }

    /// Compute SHA-256 hash of a file
    fn hash_file(path: &Path) -> Result<String> {
        let content = std::fs::read(path)
            .map_err(|e| Error::new(&format!("Failed to read file '{}': {}", path.display(), e)))?;

        let hash = Sha256::digest(&content);
        Ok(format!("{:x}", hash))
    }

    /// Write receipt to a file
    ///
    /// # Arguments
    /// * `path` - Path to write the receipt
    pub fn write_to_file(&self, path: &Path) -> Result<()> {
        let json = serde_json::to_string_pretty(self)
            .map_err(|e| Error::new(&format!("Failed to serialize receipt: {}", e)))?;

        std::fs::write(path, json)
            .map_err(|e| Error::new(&format!("Failed to write receipt to '{}': {}", path.display(), e)))
    }

    /// Read receipt from a file
    ///
    /// # Arguments
    /// * `path` - Path to read the receipt from
    pub fn read_from_file(path: &Path) -> Result<Self> {
        let content = std::fs::read_to_string(path).map_err(|e| {
            Error::new(&format!(
                "Failed to read receipt from '{}': {}",
                path.display(),
                e
            ))
        })?;

        serde_json::from_str(&content)
            .map_err(|e| Error::new(&format!("Failed to parse receipt: {}", e)))
    }

    /// Create an output file record
    pub fn create_output(path: PathBuf, content: &[u8], produced_by: &str) -> OutputFile {
        let hash = format!("{:x}", Sha256::digest(content));
        OutputFile {
            path,
            hash,
            size_bytes: content.len(),
            produced_by: produced_by.to_string(),
        }
    }

    /// Set policies
    pub fn with_policies(mut self, policies: ReceiptPolicies) -> Self {
        self.policies = policies;
        self
    }
}

impl OutputFile {
    /// Create a new output file record from a path
    pub fn from_path(full_path: &Path, rel_path: PathBuf, produced_by: &str) -> Result<Self> {
        let content = std::fs::read(full_path).map_err(|e| {
            Error::new(&format!(
                "Failed to read output file '{}': {}",
                full_path.display(),
                e
            ))
        })?;

        Ok(Self {
            path: rel_path,
            hash: format!("{:x}", Sha256::digest(&content)),
            size_bytes: content.len(),
            produced_by: produced_by.to_string(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn create_test_epoch() -> Epoch {
        Epoch {
            id: "0123456789abcdef".repeat(4), // 64 chars
            timestamp: chrono::Utc::now().to_rfc3339(),
            inputs: Default::default(),
            total_triples: 0,
        }
    }

    #[test]
    fn test_receipt_creation() {
        let epoch = create_test_epoch();
        let outputs = vec![OutputFile {
            path: PathBuf::from("src/generated/model.rs"),
            hash: "fedcba9876543210".repeat(4), // 64 chars
            size_bytes: 1024,
            produced_by: "μ₃:emission".to_string(),
        }];

        let receipt = BuildReceipt::new(&epoch, vec![], outputs, "6.0.0");

        assert_eq!(receipt.id.len(), 64);
        assert_eq!(receipt.toolchain_version, "6.0.0");
        assert_eq!(receipt.outputs.len(), 1);
        assert!(receipt.is_valid);
    }

    #[test]
    fn test_receipt_verify_outputs() {
        let temp_dir = TempDir::new().unwrap();
        let output_path = temp_dir.path().join("test.rs");

        // Write initial content
        let content = b"fn main() {}";
        std::fs::write(&output_path, content).unwrap();

        let epoch = create_test_epoch();
        let outputs = vec![OutputFile {
            path: PathBuf::from("test.rs"),
            hash: format!("{:x}", Sha256::digest(content)),
            size_bytes: content.len(),
            produced_by: "μ₃:emission".to_string(),
        }];

        let receipt = BuildReceipt::new(&epoch, vec![], outputs, "6.0.0");

        // Should verify true when unchanged
        assert!(receipt.verify_outputs(temp_dir.path()).unwrap());

        // Modify file
        std::fs::write(&output_path, b"fn main() { println!(\"hello\"); }").unwrap();

        // Should verify false when changed
        assert!(!receipt.verify_outputs(temp_dir.path()).unwrap());
    }

    #[test]
    fn test_receipt_serialization() {
        let temp_dir = TempDir::new().unwrap();
        let receipt_path = temp_dir.path().join("receipt.json");

        let epoch = create_test_epoch();
        let receipt = BuildReceipt::new(&epoch, vec![], vec![], "6.0.0");

        // Write and read back
        receipt.write_to_file(&receipt_path).unwrap();
        let loaded = BuildReceipt::read_from_file(&receipt_path).unwrap();

        assert_eq!(receipt.id, loaded.id);
        assert_eq!(receipt.epoch_id, loaded.epoch_id);
    }
}
