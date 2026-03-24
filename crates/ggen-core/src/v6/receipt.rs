//! Receipt: Provenance binding outputs to inputs
//!
//! A Receipt cryptographically binds the projection outputs (A) to the
//! frozen inputs (O) via the epoch. This enables verification that
//! hash(A) = hash(μ(O)).
//!
//! ## A2A-RS Integration (μ₅)
//!
//! For A2A-RS code generation, the receipt provides:
//! - Cryptographic proof that generated code matches ontology
//! - Timestamped storage in `.ggen/receipts/<timestamp>.json`
//! - Verification that ontology_hash matches inputs
//! - Complete file listing with SHA-256 hashes

use crate::v6::{Epoch, PassExecution};
use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fs;
use std::path::{Path, PathBuf};

/// A build receipt binding outputs to inputs
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildReceipt {
    /// Unique receipt ID (SHA-256 of all contents)
    pub id: String,

    /// Reference to the input epoch (ontology hash)
    pub epoch_id: String,

    /// SHA-256 hash of the ontology source files
    pub ontology_hash: String,

    /// When the projection was performed (ISO 8601)
    pub timestamp: String,

    /// ggen version used
    pub toolchain_version: String,

    /// Passes that were executed
    pub passes: Vec<PassExecution>,

    /// Output files with hashes (Vec<FileInfo>)
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

/// File information for receipt tracking
pub type FileInfo = OutputFile;

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

        // The epoch_id serves as the ontology_hash
        let ontology_hash = epoch.id.clone();

        // Compute receipt ID
        let mut receipt = Self {
            id: String::new(), // Will be computed below
            epoch_id: epoch.id.clone(),
            ontology_hash,
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

    /// Generate a receipt from generated files
    ///
    /// This is the main μ₅ receipt generation function for A2A-RS integration.
    /// It creates SHA-256 hashes of all generated files and returns a receipt.
    ///
    /// # Arguments
    /// * `epoch` - The frozen input epoch containing ontology hashes
    /// * `output_dir` - Directory containing generated files
    /// * `toolchain_version` - ggen version string
    ///
    /// # Returns
    /// * `Ok(BuildReceipt)` - Receipt with all file hashes
    /// * `Err(Error)` - If files cannot be read
    pub fn generate(epoch: &Epoch, output_dir: &Path, toolchain_version: &str) -> Result<Self> {
        let outputs = Self::scan_output_files(output_dir)?;
        Ok(Self::new(epoch, Vec::new(), outputs, toolchain_version))
    }

    /// Scan output directory and create file records
    fn scan_output_files(output_dir: &Path) -> Result<Vec<OutputFile>> {
        let mut outputs = Vec::new();

        if !output_dir.exists() {
            return Ok(outputs);
        }

        let entries = fs::read_dir(output_dir).map_err(|e| {
            Error::new(&format!(
                "Failed to read output directory '{}': {}",
                output_dir.display(),
                e
            ))
        })?;

        for entry in entries {
            let entry =
                entry.map_err(|e| Error::new(&format!("Failed to read directory entry: {}", e)))?;

            let path = entry.path();

            // Skip directories
            if path.is_dir() {
                continue;
            }

            // Create output file record
            let rel_path = path
                .strip_prefix(output_dir)
                .map_err(|e| Error::new(&format!("Failed to get relative path: {}", e)))?
                .to_path_buf();

            let content = fs::read(&path).map_err(|e| {
                Error::new(&format!(
                    "Failed to read output file '{}': {}",
                    path.display(),
                    e
                ))
            })?;

            let hash = format!("{:x}", Sha256::digest(&content));

            outputs.push(OutputFile {
                path: rel_path,
                hash,
                size_bytes: content.len(),
                produced_by: "μ₅:receipt".to_string(),
            });
        }

        Ok(outputs)
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

    /// Verify receipt matches current state
    ///
    /// This is the main μ₅ receipt verification function for A2A-RS integration.
    /// It verifies both the ontology hash and the generated output files.
    ///
    /// # Arguments
    /// * `output_root` - Root directory for output files
    /// * `epoch` - Current epoch to verify ontology hash against
    ///
    /// # Returns
    /// * `Ok(true)` - Receipt is valid (hash(A) = hash(μ(O)))
    /// * `Ok(false)` - Receipt is invalid
    /// * `Err(Error)` - Verification failed
    pub fn verify(&self, output_root: &Path, epoch: &Epoch) -> Result<bool> {
        // Verify ontology hash
        if self.ontology_hash != epoch.id {
            return Ok(false);
        }

        // Verify all output files
        self.verify_outputs(output_root)
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
        // Ensure parent directory exists
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).map_err(|e| {
                Error::new(&format!(
                    "Failed to create receipt directory '{}': {}",
                    parent.display(),
                    e
                ))
            })?;
        }

        let json = serde_json::to_string_pretty(self)
            .map_err(|e| Error::new(&format!("Failed to serialize receipt: {}", e)))?;

        fs::write(path, json).map_err(|e| {
            Error::new(&format!(
                "Failed to write receipt to '{}': {}",
                path.display(),
                e
            ))
        })
    }

    /// Save receipt to `.ggen/receipts/<timestamp>.json`
    ///
    /// This is the standard μ₅ receipt saving function for A2A-RS integration.
    ///
    /// # Arguments
    /// * `project_root` - Project root directory
    ///
    /// # Returns
    /// * `Ok(PathBuf)` - Path where receipt was saved
    /// * `Err(Error)` - If receipt cannot be saved
    pub fn save(&self, project_root: &Path) -> Result<PathBuf> {
        // Create .ggen/receipts directory
        let receipts_dir = project_root.join(".ggen").join("receipts");
        fs::create_dir_all(&receipts_dir).map_err(|e| {
            Error::new(&format!(
                "Failed to create receipts directory '{}': {}",
                receipts_dir.display(),
                e
            ))
        })?;

        // Create filename from timestamp
        // Format: receipt-YYYY-MM-DD-HHMMSS.json
        let filename = format!("receipt-{}.json", self.timestamp.replace([':', '.'], "-"));
        let receipt_path = receipts_dir.join(&filename);

        // Write receipt
        self.write_to_file(&receipt_path)?;

        // Also write to .ggen/latest.json for convenience
        let latest_path = project_root.join(".ggen").join("latest.json");
        self.write_to_file(&latest_path)?;

        Ok(receipt_path)
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

/// Generate receipt from generated files (μ₅)
///
/// Standalone function for A2A-RS integration that creates a receipt
/// by hashing all generated files in the output directory.
///
/// # Arguments
/// * `epoch` - The frozen input epoch (O)
/// * `output_dir` - Directory containing generated artifacts (A)
/// * `ggen_version` - ggen version string
///
/// # Returns
/// * `Ok(BuildReceipt)` - Receipt binding A to O
/// * `Err(Error)` - If files cannot be read
///
/// # Example
/// ```rust,no_run
/// use ggen_core::v6::{epoch::Epoch, receipt::generate_receipt};
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let epoch = Epoch::create(&std::path::Path::new("."), &vec![])?;
/// let receipt = generate_receipt(&epoch, std::path::Path::new("src/generated"), "6.0.0")?;
/// println!("Generated receipt: {}", receipt.id);
/// # Ok(())
/// # }
/// ```
pub fn generate_receipt(
    epoch: &Epoch, output_dir: &Path, ggen_version: &str,
) -> Result<BuildReceipt> {
    BuildReceipt::generate(epoch, output_dir, ggen_version)
}

/// Save receipt to `.ggen/receipts/<timestamp>.json`
///
/// Standalone function for A2A-RS integration that saves a receipt
/// to the standard location.
///
/// # Arguments
/// * `receipt` - Receipt to save
/// * `project_root` - Project root directory
///
/// # Returns
/// * `Ok(PathBuf)` - Path where receipt was saved
/// * `Err(Error)` - If receipt cannot be saved
///
/// # Example
/// ```rust,no_run
/// use ggen_core::v6::receipt::{generate_receipt, save_receipt};
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// # let epoch = unimplemented!();
/// let receipt = generate_receipt(&epoch, std::path::Path::new("src/generated"), "6.0.0")?;
/// let path = save_receipt(&receipt, std::path::Path::new("."))?;
/// println!("Saved receipt to: {:?}", path);
/// # Ok(())
/// # }
/// ```
pub fn save_receipt(receipt: &BuildReceipt, project_root: &Path) -> Result<PathBuf> {
    receipt.save(project_root)
}

/// Verify receipt matches current state
///
/// Standalone function for A2A-RS integration that verifies a receipt
/// matches both the ontology hash and generated files.
///
/// # Arguments
/// * `receipt` - Receipt to verify
/// * `output_root` - Root directory for output files
/// * `epoch` - Current epoch to verify against
///
/// # Returns
/// * `Ok(true)` - Receipt is valid (hash(A) = hash(μ(O)))
/// * `Ok(false)` - Receipt is invalid
/// * `Err(Error)` - Verification failed
///
/// # Example
/// ```rust,no_run
/// use ggen_core::v6::receipt::verify_receipt;
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// # let receipt = unimplemented!();
/// # let epoch = unimplemented!();
/// let is_valid = verify_receipt(&receipt, std::path::Path::new("src/generated"), &epoch)?;
/// assert!(is_valid, "Generated files have been modified!");
/// # Ok(())
/// # }
/// ```
pub fn verify_receipt(receipt: &BuildReceipt, output_root: &Path, epoch: &Epoch) -> Result<bool> {
    receipt.verify(output_root, epoch)
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
            path: PathBuf::from("ontology/generated/domain.ttl"),
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
        let output_path = temp_dir.path().join("domain.ttl");

        // Write initial content
        let content = b"@prefix ex: <http://example.org/> .";
        std::fs::write(&output_path, content).unwrap();

        let epoch = create_test_epoch();
        let outputs = vec![OutputFile {
            path: PathBuf::from("domain.ttl"),
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

    #[test]
    fn test_generate_receipt() {
        let temp_dir = TempDir::new().unwrap();
        let output_dir = temp_dir.path().join("generated");
        fs::create_dir_all(&output_dir).unwrap();

        // Create test files
        fs::write(output_dir.join("agent.rs"), "pub struct Agent {}").unwrap();
        fs::write(output_dir.join("message.rs"), "pub struct Message {}").unwrap();

        let epoch = create_test_epoch();

        let receipt = generate_receipt(&epoch, &output_dir, "6.0.0").unwrap();

        assert!(!receipt.id.is_empty());
        assert_eq!(receipt.toolchain_version, "6.0.0");
        assert_eq!(receipt.outputs.len(), 2);
        assert_eq!(receipt.ontology_hash, epoch.id);
    }

    #[test]
    fn test_save_receipt() {
        let temp_dir = TempDir::new().unwrap();
        let output_dir = temp_dir.path().join("generated");
        fs::create_dir_all(&output_dir).unwrap();

        fs::write(output_dir.join("agent.rs"), "pub struct Agent {}").unwrap();

        let epoch = create_test_epoch();
        let receipt = BuildReceipt::generate(&epoch, &output_dir, "6.0.0").unwrap();

        let saved_path = save_receipt(&receipt, temp_dir.path()).unwrap();

        // Verify receipt was saved to .ggen/receipts/
        assert!(saved_path.starts_with(temp_dir.path().join(".ggen/receipts")));
        assert!(saved_path.exists());

        // Verify .ggen/latest.json was created
        let latest_path = temp_dir.path().join(".ggen/latest.json");
        assert!(latest_path.exists());
    }

    #[test]
    fn test_verify_receipt_valid() {
        let temp_dir = TempDir::new().unwrap();
        let output_dir = temp_dir.path().join("generated");
        fs::create_dir_all(&output_dir).unwrap();

        let content = b"pub struct Agent {}";
        fs::write(output_dir.join("agent.rs"), content).unwrap();

        let epoch = create_test_epoch();
        let receipt = BuildReceipt::generate(&epoch, &output_dir, "6.0.0").unwrap();

        // Verify should pass
        let is_valid = verify_receipt(&receipt, &output_dir, &epoch).unwrap();
        assert!(is_valid);
    }

    #[test]
    fn test_verify_receipt_invalid_file() {
        let temp_dir = TempDir::new().unwrap();
        let output_dir = temp_dir.path().join("generated");
        fs::create_dir_all(&output_dir).unwrap();

        let content = b"pub struct Agent {}";
        fs::write(output_dir.join("agent.rs"), content).unwrap();

        let epoch = create_test_epoch();
        let receipt = BuildReceipt::generate(&epoch, &output_dir, "6.0.0").unwrap();

        // Modify file
        fs::write(output_dir.join("agent.rs"), b"modified content").unwrap();

        // Verify should fail
        let is_valid = verify_receipt(&receipt, &output_dir, &epoch).unwrap();
        assert!(!is_valid);
    }

    #[test]
    fn test_verify_receipt_invalid_epoch() {
        let temp_dir = TempDir::new().unwrap();
        let output_dir = temp_dir.path().join("generated");
        fs::create_dir_all(&output_dir).unwrap();

        fs::write(output_dir.join("agent.rs"), b"pub struct Agent {}").unwrap();

        let epoch1 = create_test_epoch();
        let epoch2 = Epoch {
            id: "differenthash".repeat(4),
            timestamp: chrono::Utc::now().to_rfc3339(),
            inputs: Default::default(),
            total_triples: 0,
        };

        let receipt = BuildReceipt::generate(&epoch1, &output_dir, "6.0.0").unwrap();

        // Verify should fail due to epoch mismatch
        let is_valid = verify_receipt(&receipt, &output_dir, &epoch2).unwrap();
        assert!(!is_valid);
    }

    #[test]
    fn test_receipt_with_ontology_hash() {
        let epoch = create_test_epoch();
        let receipt = BuildReceipt::new(&epoch, vec![], vec![], "6.0.0");

        // ontology_hash should equal epoch_id
        assert_eq!(receipt.ontology_hash, epoch.id);
        assert_eq!(receipt.ontology_hash, receipt.epoch_id);
    }

    #[test]
    fn test_scan_output_files_empty() {
        let temp_dir = TempDir::new().unwrap();
        let output_dir = temp_dir.path().join("generated");
        fs::create_dir_all(&output_dir).unwrap();

        let outputs = BuildReceipt::scan_output_files(&output_dir).unwrap();
        assert_eq!(outputs.len(), 0);
    }

    #[test]
    fn test_scan_output_files_with_subdirectories() {
        let temp_dir = TempDir::new().unwrap();
        let output_dir = temp_dir.path().join("generated");
        let subdir = output_dir.join("subdir");
        fs::create_dir_all(&subdir).unwrap();

        // Create files in main and subdirectory
        fs::write(output_dir.join("main.rs"), "fn main() {}").unwrap();
        fs::write(subdir.join("lib.rs"), "pub fn lib() {}").unwrap();
        fs::write(subdir.join("nested.rs"), "pub fn nested() {}").unwrap();

        let outputs = BuildReceipt::scan_output_files(&output_dir).unwrap();

        // Only files in the root of output_dir (not recursive)
        assert_eq!(outputs.len(), 1);
        assert_eq!(outputs[0].path, PathBuf::from("main.rs"));
    }
}
