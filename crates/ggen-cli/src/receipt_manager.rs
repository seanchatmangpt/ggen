//! Receipt manager for CLI operations
//!
//! This module provides utilities for generating cryptographic receipts
//! after CLI operations like pack installation.

use ed25519_dalek::{SigningKey, VerifyingKey};
use ggen_receipt::{hash_data, Receipt};
use ggen_utils::Result;
use serde::Serialize;
use std::fs;
use std::path::PathBuf;
use tracing::info;

/// Verification output for receipt operations
#[derive(Serialize)]
pub struct VerifyOutput {
    pub receipt_file: String,
    pub is_valid: bool,
    pub message: String,
    pub operation_id: Option<String>,
    pub timestamp: Option<String>,
    pub input_hashes: Option<usize>,
    pub output_hashes: Option<usize>,
    pub chain_position: Option<String>,
}


/// Receipt manager for generating and storing receipts
pub struct ReceiptManager {
    /// Path to receipts directory
    receipts_dir: PathBuf,
    /// Path to keys directory
    keys_dir: PathBuf,
    /// Signing key
    signing_key: Option<SigningKey>,
    /// Verifying key
    verifying_key: Option<VerifyingKey>,
}

impl ReceiptManager {
    /// Create a new receipt manager
    ///
    /// # Arguments
    ///
    /// * `base_dir` - Base directory (usually .ggen/)
    pub fn new(base_dir: PathBuf) -> Result<Self> {
        let receipts_dir = base_dir.join("receipts");
        let keys_dir = base_dir.join("keys");

        // Create directories if they don't exist
        fs::create_dir_all(&receipts_dir).map_err(|e| {
            ggen_utils::Error::new(&format!("Failed to create receipts directory: {}", e))
        })?;

        fs::create_dir_all(&keys_dir).map_err(|e| {
            ggen_utils::Error::new(&format!("Failed to create keys directory: {}", e))
        })?;

        Ok(Self {
            receipts_dir,
            keys_dir,
            signing_key: None,
            verifying_key: None,
        })
    }

    /// Load or generate Ed25519 keypair
    ///
    /// Keys are stored in .ggen/keys/ directory:
    /// - private.pem - Signing key (hex-encoded)
    /// - public.pem - Verifying key (hex-encoded)
    pub fn load_or_generate_keys(&mut self) -> Result<&VerifyingKey> {
        let private_key_path = self.keys_dir.join("private.pem");
        let public_key_path = self.keys_dir.join("public.pem");

        // Try to load existing keys
        if private_key_path.exists() && public_key_path.exists() {
            info!("Loading existing keys from {:?}", self.keys_dir);

            let private_key_hex = fs::read_to_string(&private_key_path).map_err(|e| {
                ggen_utils::Error::new(&format!("Failed to read private key: {}", e))
            })?;

            let public_key_hex = fs::read_to_string(&public_key_path).map_err(|e| {
                ggen_utils::Error::new(&format!("Failed to read public key: {}", e))
            })?;

            // Decode hex keys
            let signing_key_bytes =
                hex::decode(private_key_hex.trim()).map_err(|e| {
                    ggen_utils::Error::new(&format!("Failed to decode private key: {}", e))
                })?;

            let verifying_key_bytes =
                hex::decode(public_key_hex.trim()).map_err(|e| {
                    ggen_utils::Error::new(&format!("Failed to decode public key: {}", e))
                })?;

            // Parse keys - convert slices to fixed arrays for ed25519-dalek 2.x
            let signing_key_array: [u8; 32] = signing_key_bytes[..32]
                .try_into()
                .map_err(|_| ggen_utils::Error::new("Invalid signing key length"))?;
            let verifying_key_array: [u8; 32] = verifying_key_bytes[..32]
                .try_into()
                .map_err(|_| ggen_utils::Error::new("Invalid verifying key length"))?;

            let signing_key = SigningKey::from_bytes(&signing_key_array);
            let verifying_key = VerifyingKey::from_bytes(&verifying_key_array)
                .map_err(|e| ggen_utils::Error::new(&format!("Invalid verifying key: {}", e)))?;

            self.signing_key = Some(signing_key);
            self.verifying_key = Some(verifying_key);

            return Ok(self.verifying_key.as_ref().unwrap());
        }

        // Generate new keys
        info!("Generating new Ed25519 keypair");
        let (signing_key, verifying_key) = ggen_receipt::generate_keypair();

        // Store keys
        let private_key_hex = hex::encode(signing_key.to_bytes());
        let public_key_hex = hex::encode(verifying_key.to_bytes());

        fs::write(&private_key_path, private_key_hex).map_err(|e| {
            ggen_utils::Error::new(&format!("Failed to write private key: {}", e))
        })?;

        fs::write(&public_key_path, public_key_hex).map_err(|e| {
            ggen_utils::Error::new(&format!("Failed to write public key: {}", e))
        })?;

        info!(
            "Generated new keypair: private={:?}, public={:?}",
            private_key_path, public_key_path
        );

        self.signing_key = Some(signing_key);
        self.verifying_key = Some(verifying_key);

        Ok(self.verifying_key.as_ref().unwrap())
    }

    /// Generate a receipt for a pack installation
    ///
    /// # Arguments
    ///
    /// * `pack_id` - Pack identifier
    /// * `pack_version` - Pack version
    /// * `packages_installed` - List of packages installed
    /// * `install_path` - Installation path
    ///
    /// # Returns
    ///
    /// Path to the generated receipt file
    pub fn generate_pack_install_receipt(
        &mut self,
        pack_id: &str,
        pack_version: &str,
        packages_installed: &[String],
        _install_path: &PathBuf,
    ) -> Result<PathBuf> {
        // Ensure keys are loaded
        self.load_or_generate_keys()?;

        // Create operation ID
        let timestamp = chrono::Utc::now().format("%Y%m%d-%H%M%S");
        let operation_id = format!("pack-install-{}-{}", pack_id, timestamp);

        // Hash input data (pack spec)
        let input_data = format!("{}@{}", pack_id, pack_version);
        let input_hash = hash_data(input_data.as_bytes());

        // Hash output data (installed packages)
        let output_hashes: Vec<String> = packages_installed
            .iter()
            .map(|pkg| hash_data(pkg.as_bytes()))
            .collect();

        // Create and sign receipt
        let receipt = Receipt::new(
            operation_id.clone(),
            vec![input_hash],
            output_hashes,
            None, // Genesis receipt (no previous)
        )
        .sign(self.signing_key.as_ref().unwrap())
        .map_err(|e| {
            ggen_utils::Error::new(&format!("Failed to sign receipt: {}", e))
        })?;

        // Write receipt to file
        let receipt_filename = format!("{}.json", operation_id);
        let receipt_path = self.receipts_dir.join(receipt_filename);

        let receipt_json = serde_json::to_string_pretty(&receipt).map_err(|e| {
            ggen_utils::Error::new(&format!("Failed to serialize receipt: {}", e))
        })?;

        fs::write(&receipt_path, receipt_json).map_err(|e| {
            ggen_utils::Error::new(&format!("Failed to write receipt: {}", e))
        })?;

        info!(
            "Generated receipt: {} ({} packages installed)",
            receipt_path.display(),
            packages_installed.len()
        );

        Ok(receipt_path)
    }

    /// Verify a receipt file
    ///
    /// # Arguments
    ///
    /// * `receipt_path` - Path to receipt file
    ///
    /// # Returns
    ///
    /// Verification output with status
    pub fn verify_receipt(&self, receipt_path: &PathBuf) -> Result<VerifyOutput> {
        // Load verifying key
        let public_key_path = self.keys_dir.join("public.pem");
        let verifying_key = self.read_verifying_key(&public_key_path)?;

        // Read receipt file
        let receipt_content = fs::read_to_string(receipt_path).map_err(|e| {
            ggen_utils::Error::new(&format!("Failed to read receipt: {}", e))
        })?;

        // Parse receipt
        let receipt: Receipt = serde_json::from_str(&receipt_content).map_err(|e| {
            ggen_utils::Error::new(&format!("Failed to parse receipt: {}", e))
        })?;

        // Verify signature
        let is_valid = receipt.verify(&verifying_key).is_ok();

        Ok(VerifyOutput {
            receipt_file: receipt_path.display().to_string(),
            is_valid,
            message: if is_valid {
                "Receipt signature verified successfully".to_string()
            } else {
                "Signature verification failed".to_string()
            },
            operation_id: Some(receipt.operation_id.clone()),
            timestamp: Some(receipt.timestamp.format("%Y-%m-%d %H:%M:%S UTC").to_string()),
            input_hashes: Some(receipt.input_hashes.len()),
            output_hashes: Some(receipt.output_hashes.len()),
            chain_position: receipt.previous_receipt_hash.as_ref().map(|_| "chained".to_string()),
        })
    }

    /// Read verifying key from file
    fn read_verifying_key(&self, key_path: &PathBuf) -> Result<VerifyingKey> {
        let content = fs::read_to_string(key_path).map_err(|e| {
            ggen_utils::Error::new(&format!("Failed to read public key: {}", e))
        })?;

        let key_bytes = hex::decode(content.trim()).map_err(|e| {
            ggen_utils::Error::new(&format!("Failed to decode public key: {}", e))
        })?;

        let key_array: [u8; 32] = key_bytes[..32]
            .try_into()
            .map_err(|_| ggen_utils::Error::new("Invalid key length"))?;
        VerifyingKey::from_bytes(&key_array)
            .map_err(|e| ggen_utils::Error::new(&format!("Invalid verifying key: {}", e)))
    }

    /// Get path to receipts directory
    pub fn receipts_dir(&self) -> &PathBuf {
        &self.receipts_dir
    }

    /// Get path to keys directory
    pub fn keys_dir(&self) -> &PathBuf {
        &self.keys_dir
    }

    /// Generate a receipt for capability composition
    ///
    /// # Arguments
    ///
    /// * `capability_id` - Capability identifier
    /// * `atomic_packs` - List of atomic packs in the composition
    /// * `project_root` - Project root path
    ///
    /// # Returns
    ///
    /// Path to the generated receipt file
    pub fn generate_composition_receipt(
        &mut self,
        capability_id: &str,
        atomic_packs: &[String],
        _project_root: &PathBuf,
    ) -> Result<PathBuf> {
        // Ensure keys are loaded
        self.load_or_generate_keys()?;

        // Create operation ID
        let timestamp = chrono::Utc::now().format("%Y%m%d-%H%M%S");
        let operation_id = format!("capability-{}-{}", capability_id, timestamp);

        // Hash input data (capability spec)
        let input_data = format!("{}@composition", capability_id);
        let input_hash = hash_data(input_data.as_bytes());

        // Hash output data (atomic packs)
        let output_hashes: Vec<String> = atomic_packs
            .iter()
            .map(|pack| hash_data(pack.as_bytes()))
            .collect();

        // Create and sign receipt
        let receipt = Receipt::new(
            operation_id.clone(),
            vec![input_hash],
            output_hashes,
            None, // Genesis receipt (no previous)
        )
        .sign(self.signing_key.as_ref().unwrap())
        .map_err(|e| {
            ggen_utils::Error::new(&format!("Failed to sign receipt: {}", e))
        })?;

        // Write receipt to file
        let receipt_filename = format!("{}.json", operation_id);
        let receipt_path = self.receipts_dir.join(receipt_filename);

        let receipt_json = serde_json::to_string_pretty(&receipt).map_err(|e| {
            ggen_utils::Error::new(&format!("Failed to serialize receipt: {}", e))
        })?;

        fs::write(&receipt_path, receipt_json).map_err(|e| {
            ggen_utils::Error::new(&format!("Failed to write receipt: {}", e))
        })?;

        info!(
            "Generated composition receipt: {} ({} packs composed)",
            receipt_path.display(),
            atomic_packs.len()
        );

        Ok(receipt_path)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_receipt_manager_creation() {
        let temp_dir = TempDir::new().unwrap();
        let base_dir = temp_dir.path().join(".ggen");

        let manager = ReceiptManager::new(base_dir).unwrap();

        assert!(manager.receipts_dir().exists());
        assert!(manager.keys_dir().exists());
    }

    #[test]
    fn test_key_generation() {
        let temp_dir = TempDir::new().unwrap();
        let base_dir = temp_dir.path().join(".ggen");

        let mut manager = ReceiptManager::new(base_dir).unwrap();
        let _verifying_key = manager.load_or_generate_keys().unwrap();

        // Verify keys were created
        assert!(manager.keys_dir().join("private.pem").exists());
        assert!(manager.keys_dir().join("public.pem").exists());
    }

    #[test]
    fn test_pack_install_receipt() {
        let temp_dir = TempDir::new().unwrap();
        let base_dir = temp_dir.path().join(".ggen");

        let mut manager = ReceiptManager::new(base_dir).unwrap();
        let receipt_path = manager
            .generate_pack_install_receipt(
                "test-pack",
                "1.0.0",
                &["pkg1".to_string(), "pkg2".to_string()],
                &temp_dir.path().join("install"),
            )
            .unwrap();

        assert!(receipt_path.exists());
        assert!(receipt_path.to_string_lossy().contains("pack-install-test-pack"));
    }

    #[test]
    fn test_receipt_verification() {
        let temp_dir = TempDir::new().unwrap();
        let base_dir = temp_dir.path().join(".ggen");

        let mut manager = ReceiptManager::new(base_dir).unwrap();

        // Generate receipt
        let receipt_path = manager
            .generate_pack_install_receipt(
                "test-pack",
                "1.0.0",
                &["pkg1".to_string()],
                &temp_dir.path().join("install"),
            )
            .unwrap();

        // Verify receipt
        let verify_output = manager.verify_receipt(&receipt_path).unwrap();

        assert!(verify_output.is_valid);
        assert_eq!(
            verify_output.message,
            "Receipt signature verified successfully"
        );
        assert!(verify_output.operation_id.is_some());
        assert!(verify_output.timestamp.is_some());
        assert_eq!(verify_output.input_hashes, Some(1));
        assert_eq!(verify_output.output_hashes, Some(1));
    }
}
