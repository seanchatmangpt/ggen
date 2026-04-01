//! Pack Receipt Generation
//!
//! This module provides cryptographic receipt generation for pack operations.

use ed25519_dalek::{SecretKey, SigningKey, VerifyingKey};
use std::fs;
use std::path::PathBuf;

/// Error type for pack receipt operations
#[derive(Debug)]
pub enum PackReceiptError {
    Runtime(String),
}

impl std::fmt::Display for PackReceiptError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PackReceiptError::Runtime(msg) => write!(f, "{}", msg),
        }
    }
}

impl std::error::Error for PackReceiptError {}

/// Result type for pack receipt operations
pub type Result<T> = std::result::Result<T, PackReceiptError>;

/// Generates a cryptographic receipt for pack installation
pub fn generate_pack_install_receipt(
    pack_id: &str,
    status: &str,
) -> Result<PathBuf> {
    use ggen_receipt::{hash_data, Receipt};

    // Create .ggen/receipts and .ggen/keys directories
    let receipts_dir = PathBuf::from(".ggen/receipts");
    let keys_dir = PathBuf::from(".ggen/keys");

    fs::create_dir_all(&receipts_dir).map_err(|e| {
        PackReceiptError::Runtime(format!("Failed to create receipts directory: {}", e))
    })?;

    fs::create_dir_all(&keys_dir)
        .map_err(|e| PackReceiptError::Runtime(format!("Failed to create keys directory: {}", e)))?;

    // Load or generate Ed25519 keypair
    let private_key_path = keys_dir.join("private.pem");
    let public_key_path = keys_dir.join("public.pem");

    let (signing_key, _verifying_key) = if private_key_path.exists() {
        // Load existing keypair
        load_or_generate_keypair(&private_key_path, &public_key_path)?
    } else {
        // Generate new keypair
        let (signing_key, verifying_key) = ggen_receipt::generate_keypair();

        // Save keys
        save_keypair(&signing_key, &verifying_key, &private_key_path, &public_key_path)?;

        (signing_key, verifying_key)
    };

    // Create operation ID with timestamp
    let timestamp = chrono::Utc::now();
    let operation_id = format!(
        "pack-install-{}-{}",
        pack_id,
        timestamp.format("%Y%m%d-%H%M%S")
    );

    // Create input hashes (pack_id as input)
    let input_hashes = vec![hash_data(pack_id.as_bytes())];

    // Create output hashes (status as output)
    let output_hashes = vec![hash_data(status.as_bytes())];

    // Create and sign receipt
    let receipt = Receipt::new(operation_id, input_hashes, output_hashes, None)
        .sign(&signing_key)
        .map_err(|e| PackReceiptError::Runtime(format!("Failed to sign receipt: {}", e)))?;

    // Save receipt to file
    let receipt_filename = format!("pack-{}-{}.json", pack_id, timestamp.format("%Y%m%d-%H%M%S"));
    let receipt_path = receipts_dir.join(&receipt_filename);

    let receipt_json = serde_json::to_string_pretty(&receipt)
        .map_err(|e| PackReceiptError::Runtime(format!("Failed to serialize receipt: {}", e)))?;

    fs::write(&receipt_path, receipt_json)
        .map_err(|e| PackReceiptError::Runtime(format!("Failed to write receipt: {}", e)))?;

    Ok(receipt_path)
}

/// Loads existing keypair or generates a new one
fn load_or_generate_keypair(
    private_key_path: &PathBuf,
    _public_key_path: &PathBuf,
) -> Result<(SigningKey, VerifyingKey)> {
    use ed25519_dalek::SECRET_KEY_LENGTH;

    // Read private key
    let private_key_bytes = fs::read(private_key_path).map_err(|e| {
        PackReceiptError::Runtime(format!("Failed to read private key: {}", e))
    })?;

    if private_key_bytes.len() != SECRET_KEY_LENGTH {
        return Err(PackReceiptError::Runtime("Invalid private key length".to_string()));
    }

    let secret_key = SecretKey::try_from(private_key_bytes.as_slice())
        .map_err(|_| PackReceiptError::Runtime("Invalid private key".to_string()))?;

    let signing_key: SigningKey = secret_key.into();
    let verifying_key = signing_key.verifying_key();

    Ok((signing_key, verifying_key))
}

/// Saves Ed25519 keypair to PEM files
fn save_keypair(
    signing_key: &SigningKey,
    verifying_key: &VerifyingKey,
    private_key_path: &PathBuf,
    public_key_path: &PathBuf,
) -> Result<()> {
    // Save private key (hex-encoded for simplicity)
    let private_key_hex = hex::encode(signing_key.to_bytes());
    fs::write(private_key_path, private_key_hex)
        .map_err(|e| PackReceiptError::Runtime(format!("Failed to write private key: {}", e)))?;

    // Save public key (hex-encoded for simplicity)
    let public_key_hex = hex::encode(verifying_key.to_bytes());
    fs::write(public_key_path, public_key_hex)
        .map_err(|e| PackReceiptError::Runtime(format!("Failed to write public key: {}", e)))?;

    Ok(())
}
