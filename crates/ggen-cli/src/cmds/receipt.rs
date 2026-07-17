//! Receipt Commands
//!
//! Exposes cryptographic receipt verification via the `ggen receipt` noun:
//!   - `ggen receipt verify <receipt-path> [--public-key <key-path>]`
//!   - `ggen receipt info <receipt-path>`

use clap_noun_verb::{NounVerbError, Result};
use clap_noun_verb_macros::verb;
use ed25519_dalek::VerifyingKey;
use ggen_config::receipt::Receipt;
use serde::Serialize;
use std::{fs, path::PathBuf};

// ============================================================================
// Output Types
// ============================================================================

/// Output from `ggen receipt verify`
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

/// Output from `ggen receipt info`
#[derive(Serialize)]
pub struct InfoOutput {
    pub receipt_file: String,
    pub operation_id: String,
    pub timestamp: String,
    pub input_hashes: usize,
    pub output_hashes: usize,
    pub has_previous: bool,
    pub signature_present: bool,
}

// ============================================================================
// Domain helpers (keep verbs thin for Poka-Yoke complexity gate FM-1.1)
// ============================================================================

fn load_verifying_key(key_path: &PathBuf) -> std::result::Result<VerifyingKey, String> {
    let key_content =
        fs::read_to_string(key_path).map_err(|e| format!("Failed to read public key: {}", e))?;
    let key_bytes = hex::decode(key_content.trim())
        .map_err(|e| format!("Failed to decode public key hex: {}", e))?;
    let key_array: [u8; 32] = key_bytes
        .as_slice()
        .try_into()
        .map_err(|_| "Public key must be exactly 32 bytes".to_string())?;
    VerifyingKey::from_bytes(&key_array).map_err(|e| format!("Invalid verifying key: {}", e))
}

fn load_receipt(path: &PathBuf) -> std::result::Result<Receipt, String> {
    let content = fs::read_to_string(path).map_err(|e| format!("Failed to read receipt: {}", e))?;
    serde_json::from_str(&content).map_err(|e| format!("Failed to parse receipt JSON: {}", e))
}

fn resolve_key_path(public_key: Option<String>) -> Option<PathBuf> {
    public_key.map(PathBuf::from).or_else(|| {
        let d = PathBuf::from(".ggen/keys/public.pem");
        d.exists().then_some(d)
    })
}

fn do_verify(
    receipt_path: String, public_key: Option<String>,
) -> std::result::Result<VerifyOutput, NounVerbError> {
    let receipt_file = PathBuf::from(&receipt_path);

    if !receipt_file.exists() {
        return Ok(VerifyOutput {
            receipt_file: receipt_path,
            is_valid: false,
            message: format!("Receipt file not found: {}", receipt_file.display()),
            operation_id: None,
            timestamp: None,
            input_hashes: None,
            output_hashes: None,
            chain_position: None,
        });
    }

    let key_path = match resolve_key_path(public_key) {
        Some(p) => p,
        None => {
            return Ok(VerifyOutput {
                receipt_file: receipt_path,
                is_valid: false,
                message: "Public key required: pass --public-key <path> or ensure .ggen/keys/public.pem exists".to_string(),
                operation_id: None,
                timestamp: None,
                input_hashes: None,
                output_hashes: None,
                chain_position: None,
            });
        }
    };

    let receipt = load_receipt(&receipt_file).map_err(|e| NounVerbError::execution_error(e))?;
    let verifying_key =
        load_verifying_key(&key_path).map_err(|e| NounVerbError::execution_error(e))?;
    let is_valid = receipt.verify(&verifying_key).is_ok();

    Ok(VerifyOutput {
        receipt_file: receipt_path,
        is_valid,
        message: if is_valid {
            "Receipt signature verified successfully".to_string()
        } else {
            "Signature verification failed".to_string()
        },
        operation_id: Some(receipt.operation_id.clone()),
        timestamp: Some(receipt.timestamp.to_rfc3339()),
        input_hashes: Some(receipt.input_hashes.len()),
        output_hashes: Some(receipt.output_hashes.len()),
        chain_position: receipt
            .previous_receipt_hash
            .as_ref()
            .map(|_| "chained".to_string()),
    })
}

fn do_info(receipt_path: String) -> std::result::Result<InfoOutput, NounVerbError> {
    let receipt_file = PathBuf::from(&receipt_path);
    if !receipt_file.exists() {
        return Err(NounVerbError::execution_error(format!(
            "Receipt file not found: {}",
            receipt_file.display()
        )));
    }
    let receipt = load_receipt(&receipt_file).map_err(|e| NounVerbError::execution_error(e))?;
    Ok(InfoOutput {
        receipt_file: receipt_path,
        operation_id: receipt.operation_id,
        timestamp: receipt.timestamp.to_rfc3339(),
        input_hashes: receipt.input_hashes.len(),
        output_hashes: receipt.output_hashes.len(),
        has_previous: receipt.previous_receipt_hash.is_some(),
        signature_present: !receipt.signature.is_empty(),
    })
}

// ============================================================================
// Verbs (thin wrappers; complexity ≤ 5 per Poka-Yoke gate FM-1.1)
// ============================================================================

/// Verify the cryptographic Ed25519 signature on a receipt file.
///
/// Pass `--public-key <path>` to supply the verifying key; falls back to
/// `.ggen/keys/public.pem`. Returns `is_valid: true` on a valid signature.
#[verb]
pub fn verify(receipt_path: String, public_key: Option<String>) -> Result<VerifyOutput> {
    do_verify(receipt_path, public_key)
}

/// Print human-readable fields from a receipt file without signature verification.
#[verb]
pub fn info(receipt_path: String) -> Result<InfoOutput> {
    do_info(receipt_path)
}
