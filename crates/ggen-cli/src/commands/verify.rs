//! Verify Command - Cryptographic verification of receipts and chains.
//!
//! Provides CLI command for verifying cryptographic receipts and receipt chains.
//! Implements `ggen verify` as a root-level command.

use clap_noun_verb::{NounVerbError, Result as VerbResult};
use clap_noun_verb_macros::verb;
use ed25519_dalek::VerifyingKey;
use ggen_core::receipt::{Receipt, ReceiptChain};
use serde::Serialize;
use std::{fs, path::{Path, PathBuf}};

/// Output for the `ggen verify` command
#[derive(Debug, Clone, Serialize)]
pub struct VerifyOutput {
    /// Overall status: "success" or "error"
    pub status: String,
    /// Path verified
    pub path: String,
    /// Whether verification passed
    pub valid: bool,
    /// Detailed message
    pub message: String,
    /// Number of receipts in chain (if applicable)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub chain_length: Option<usize>,
}

/// Verify a cryptographic receipt or a directory of chained receipts.
///
/// Usage:
///   ggen verify                           # Verify .ggen/receipts/latest.json
///   ggen verify <path>                    # Verify specific file or directory
///   ggen verify --public-key <key-path>   # Use specific public key
#[verb("verify", "root")]
pub fn verify(
    path: Option<String>,
    public_key: Option<String>,
) -> VerbResult<VerifyOutput> {
    let target_path = path.unwrap_or_else(|| ".ggen/receipts/latest.json".to_string());
    let path_buf = PathBuf::from(&target_path);

    if !path_buf.exists() {
        return Ok(VerifyOutput {
            status: "error".to_string(),
            path: target_path,
            valid: false,
            message: format!("Path not found: {}", path_buf.display()),
            chain_length: None,
        });
    }

    // Resolve public key
    let key_path = resolve_key_path(public_key);
    let verifying_key = match load_verifying_key(&key_path) {
        Ok(k) => k,
        Err(e) => {
            return Ok(VerifyOutput {
                status: "error".to_string(),
                path: target_path,
                valid: false,
                message: format!("Public key error: {}", e),
                chain_length: None,
            });
        }
    };

    if path_buf.is_dir() {
        verify_chain(&path_buf, &verifying_key)
    } else {
        verify_single(&path_buf, &verifying_key)
    }
}

fn verify_single(path: &Path, key: &VerifyingKey) -> VerbResult<VerifyOutput> {
    let content = fs::read_to_string(path)
        .map_err(|e| NounVerbError::execution_error(format!("Failed to read receipt: {}", e)))?;
    
    let receipt: Receipt = serde_json::from_str(&content)
        .map_err(|e| NounVerbError::execution_error(format!("Failed to parse receipt: {}", e)))?;

    match receipt.verify(key) {
        Ok(_) => Ok(VerifyOutput {
            status: "success".to_string(),
            path: path.to_string_lossy().into_owned(),
            valid: true,
            message: "Receipt signature verified successfully".to_string(),
            chain_length: Some(1),
        }),
        Err(e) => Ok(VerifyOutput {
            status: "error".to_string(),
            path: path.to_string_lossy().into_owned(),
            valid: false,
            message: format!("Verification failed: {}", e),
            chain_length: Some(1),
        }),
    }
}

fn verify_chain(path: &Path, key: &VerifyingKey) -> VerbResult<VerifyOutput> {
    let mut receipts = Vec::new();
    
    let entries = fs::read_dir(path)
        .map_err(|e| NounVerbError::execution_error(format!("Failed to read directory: {}", e)))?;

    for entry in entries {
        let entry = entry.map_err(|e| NounVerbError::execution_error(e.to_string()))?;
        let entry_path = entry.path();
        
        if entry_path.is_file() && entry_path.extension().map_or(false, |ext| ext == "json") {
            // Skip latest.json if it's in the same directory to avoid duplicates
            if entry_path.file_name().map_or(false, |n| n == "latest.json") {
                continue;
            }

            let content = fs::read_to_string(&entry_path)
                .map_err(|e| NounVerbError::execution_error(format!("Failed to read {}: {}", entry_path.display(), e)))?;
            
            if let Ok(receipt) = serde_json::from_str::<Receipt>(&content) {
                receipts.push(receipt);
            }
        }
    }

    if receipts.is_empty() {
        return Ok(VerifyOutput {
            status: "error".to_string(),
            path: path.to_string_lossy().into_owned(),
            valid: false,
            message: "No receipts found in directory".to_string(),
            chain_length: Some(0),
        });
    }

    // Sort receipts by timestamp
    receipts.sort_by_key(|r| r.timestamp);

    // Try to build a chain
    let mut chain = match ReceiptChain::from_genesis(receipts[0].clone()) {
        Ok(c) => c,
        Err(e) => {
            return Ok(VerifyOutput {
                status: "error".to_string(),
                path: path.to_string_lossy().into_owned(),
                valid: false,
                message: format!("Failed to create chain from genesis: {}", e),
                chain_length: Some(receipts.len()),
            });
        }
    };

    for receipt in receipts.into_iter().skip(1) {
        if let Err(e) = chain.append(receipt) {
            return Ok(VerifyOutput {
                status: "error".to_string(),
                path: path.to_string_lossy().into_owned(),
                valid: false,
                message: format!("Chain integrity error: {}", e),
                chain_length: Some(chain.len()),
            });
        }
    }

    match chain.verify(key) {
        Ok(_) => Ok(VerifyOutput {
            status: "success".to_string(),
            path: path.to_string_lossy().into_owned(),
            valid: true,
            message: format!("Receipt chain of length {} verified successfully", chain.len()),
            chain_length: Some(chain.len()),
        }),
        Err(e) => Ok(VerifyOutput {
            status: "error".to_string(),
            path: path.to_string_lossy().into_owned(),
            valid: false,
            message: format!("Chain verification failed: {}", e),
            chain_length: Some(chain.len()),
        }),
    }
}

fn resolve_key_path(public_key: Option<String>) -> PathBuf {
    public_key.map(PathBuf::from).unwrap_or_else(|| {
        let p = PathBuf::from(".ggen/keys/verifying.key");
        if p.exists() {
            p
        } else {
            PathBuf::from(".ggen/keys/public.pem")
        }
    })
}

fn load_verifying_key(key_path: &Path) -> std::result::Result<VerifyingKey, String> {
    if !key_path.exists() {
        return Err(format!("Public key file not found: {}", key_path.display()));
    }

    let key_content = fs::read_to_string(key_path)
        .map_err(|e| format!("Failed to read public key: {}", e))?;
    
    let key_bytes = if key_content.contains("-----BEGIN PUBLIC KEY-----") {
        // Handle PEM format if needed, but for now assuming hex as per emit_sync_receipt
        return Err("PEM format not yet supported in this command, use hex".to_string());
    } else {
        hex::decode(key_content.trim())
            .map_err(|e| format!("Failed to decode public key hex: {}", e))?
    };

    let key_array: [u8; 32] = key_bytes
        .as_slice()
        .try_into()
        .map_err(|_| "Public key must be exactly 32 bytes".to_string())?;
    
    VerifyingKey::from_bytes(&key_array)
        .map_err(|e| format!("Invalid verifying key: {}", e))
}
