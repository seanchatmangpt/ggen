//! Receipt Commands
//!
//! This module provides receipt verification and management commands.

use clap_noun_verb::Result as VerbResult;
use clap_noun_verb_macros::verb;
use ed25519_dalek::VerifyingKey;
use serde::Serialize;
use std::fs;
use std::path::PathBuf;

// ============================================================================
// Output Types
// ============================================================================

#[derive(Serialize)]
struct VerifyOutput {
    receipt_file: String,
    is_valid: bool,
    message: String,
    operation_id: Option<String>,
    timestamp: Option<String>,
    input_hashes: Option<usize>,
    output_hashes: Option<usize>,
    chain_position: Option<String>,
}

#[derive(Serialize)]
struct InfoOutput {
    receipt_file: String,
    operation_id: String,
    timestamp: String,
    input_hashes: Vec<String>,
    output_hashes: Vec<String>,
    signature_present: bool,
    previous_hash: Option<String>,
    own_hash: String,
}

#[derive(Serialize)]
struct ChainVerifyOutput {
    chain_file: String,
    is_valid: bool,
    message: String,
    receipt_count: usize,
    genesis_operation: Option<String>,
    latest_operation: Option<String>,
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Reads a verifying key from a file (hex-encoded or base64-encoded)
fn read_verifying_key(path: &PathBuf) -> Result<VerifyingKey, String> {
    let content = fs::read_to_string(path)
        .map_err(|e| format!("Failed to read public key file: {}", e))?;

    let content = content.trim();

    // Try hex decoding first
    if let Ok(key_bytes) = hex::decode(content) {
        if key_bytes.len() == 32 {
            return VerifyingKey::from_bytes(&key_bytes.try_into().unwrap())
                .map_err(|e| format!("Invalid verifying key: {}", e));
        }
    }

    // Try base64 decoding
    use base64::Engine;
    if let Ok(key_bytes) = base64::engine::general_purpose::STANDARD.decode(content) {
        if key_bytes.len() == 32 {
            return VerifyingKey::from_bytes(&key_bytes.try_into().unwrap())
                .map_err(|e| format!("Invalid verifying key: {}", e));
        }
    }

    Err("Public key must be 32 bytes (hex or base64 encoded)".to_string())
}

/// Formats a timestamp for display
fn format_timestamp(ts: &chrono::DateTime<chrono::Utc>) -> String {
    ts.format("%Y-%m-%d %H:%M:%S UTC").to_string()
}

fn load_optional_key(
    public_key: &Option<String>,
) -> Result<Option<VerifyingKey>, clap_noun_verb::NounVerbError> {
    match public_key {
        Some(key_path) => {
            let key = read_verifying_key(&PathBuf::from(key_path))
                .map_err(|e| clap_noun_verb::NounVerbError::argument_error(&e))?;
            Ok(Some(key))
        }
        None => Ok(None),
    }
}

fn not_found_output(receipt_file: &str) -> VerifyOutput {
    VerifyOutput {
        receipt_file: receipt_file.to_string(),
        is_valid: false,
        message: format!("Receipt file not found: {}", receipt_file),
        operation_id: None,
        timestamp: None,
        input_hashes: None,
        output_hashes: None,
        chain_position: None,
    }
}

fn invalid_format_output(receipt_file: &str) -> VerifyOutput {
    VerifyOutput {
        receipt_file: receipt_file.to_string(),
        is_valid: false,
        message: "Invalid receipt file format. Expected a Receipt or ReceiptChain JSON.".to_string(),
        operation_id: None,
        timestamp: None,
        input_hashes: None,
        output_hashes: None,
        chain_position: None,
    }
}

fn verify_single_receipt(
    receipt_file: String,
    receipt: &ggen_receipt::Receipt,
    key: &Option<VerifyingKey>,
) -> VerbResult<VerifyOutput> {
    let verifying_key = match key {
        Some(k) => k,
        None => {
            return Ok(VerifyOutput {
                receipt_file,
                is_valid: false,
                message: "Public key required for single receipt verification. Use --public-key <path>".to_string(),
                operation_id: Some(receipt.operation_id.clone()),
                timestamp: Some(format_timestamp(&receipt.timestamp)),
                input_hashes: Some(receipt.input_hashes.len()),
                output_hashes: Some(receipt.output_hashes.len()),
                chain_position: receipt.previous_receipt_hash.as_ref().map(|_| "chained".to_string()),
            });
        }
    };

    let is_valid = receipt.verify(verifying_key).is_ok();
    let message = if is_valid {
        "Receipt signature verified successfully".to_string()
    } else {
        "Signature verification failed".to_string()
    };

    Ok(VerifyOutput {
        receipt_file,
        is_valid,
        message,
        operation_id: Some(receipt.operation_id.clone()),
        timestamp: Some(format_timestamp(&receipt.timestamp)),
        input_hashes: Some(receipt.input_hashes.len()),
        output_hashes: Some(receipt.output_hashes.len()),
        chain_position: receipt.previous_receipt_hash.as_ref().map(|_| "chained".to_string()),
    })
}

fn verify_receipt_chain(
    receipt_file: String,
    chain: &ggen_receipt::ReceiptChain,
    key: &Option<VerifyingKey>,
) -> VerbResult<VerifyOutput> {
    let verifying_key = match key {
        Some(k) => k,
        None => {
            return Ok(VerifyOutput {
                receipt_file,
                is_valid: false,
                message: "Public key required for chain verification. Use --public-key <path>".to_string(),
                operation_id: None,
                timestamp: None,
                input_hashes: None,
                output_hashes: None,
                chain_position: None,
            });
        }
    };

    let is_valid = chain.verify(verifying_key).is_ok();
    let message = if is_valid {
        format!("Chain verified successfully: {} receipts", chain.len())
    } else {
        format!("Chain verification failed: {} receipts", chain.len())
    };
    let latest = chain.last();

    Ok(VerifyOutput {
        receipt_file,
        is_valid,
        message,
        operation_id: latest.map(|r| r.operation_id.clone()),
        timestamp: latest.map(|r| format_timestamp(&r.timestamp)),
        input_hashes: latest.map(|r| r.input_hashes.len()),
        output_hashes: latest.map(|r| r.output_hashes.len()),
        chain_position: Some(format!("chain of {} receipts", chain.len())),
    })
}

fn verify_receipt_content(
    receipt_file: &str,
    file_content: &str,
    key: &Option<VerifyingKey>,
) -> VerbResult<VerifyOutput> {
    use ggen_receipt::{Receipt, ReceiptChain};

    if let Ok(receipt) = serde_json::from_str::<Receipt>(file_content) {
        return verify_single_receipt(receipt_file.to_string(), &receipt, key);
    }
    if let Ok(chain) = serde_json::from_str::<ReceiptChain>(file_content) {
        return verify_receipt_chain(receipt_file.to_string(), &chain, key);
    }
    Ok(invalid_format_output(receipt_file))
}

// ============================================================================
// Verb Functions
// ============================================================================

/// Verify a cryptographic receipt
#[verb]
fn verify(
    receipt_file: String,
    public_key: Option<String>,
) -> VerbResult<VerifyOutput> {
    let receipt_path = PathBuf::from(&receipt_file);

    if !receipt_path.exists() {
        return Ok(not_found_output(&receipt_file));
    }

    let file_content = fs::read_to_string(&receipt_path)
        .map_err(|e| {
            clap_noun_verb::NounVerbError::argument_error(&format!(
                "Failed to read receipt file: {}",
                e
            ))
        })?;

    let key = load_optional_key(&public_key)?;
    verify_receipt_content(&receipt_file, &file_content, &key)
}

/// Show detailed receipt information
#[verb]
fn info(receipt_file: String) -> VerbResult<InfoOutput> {
    use ggen_receipt::{Receipt, ReceiptChain};

    let receipt_path = PathBuf::from(&receipt_file);

    if !receipt_path.exists() {
        return Err(clap_noun_verb::NounVerbError::argument_error(
            &format!("Receipt file not found: {}", receipt_file),
        ));
    }

    let file_content = fs::read_to_string(&receipt_path)
        .map_err(|e| {
            clap_noun_verb::NounVerbError::argument_error(&format!(
                "Failed to read receipt file: {}",
                e
            ))
        })?;

    if let Ok(receipt) = serde_json::from_str::<Receipt>(&file_content) {
        Ok(InfoOutput {
            receipt_file,
            operation_id: receipt.operation_id.clone(),
            timestamp: format_timestamp(&receipt.timestamp),
            input_hashes: receipt.input_hashes.clone(),
            output_hashes: receipt.output_hashes.clone(),
            signature_present: !receipt.signature.is_empty(),
            previous_hash: receipt.previous_receipt_hash.clone(),
            own_hash: receipt.hash().map_err(|e| {
                clap_noun_verb::NounVerbError::argument_error(&format!(
                    "Failed to compute receipt hash: {}",
                    e
                ))
            })?,
        })
    } else if let Ok(chain) = serde_json::from_str::<ReceiptChain>(&file_content) {
        let latest = chain.last().ok_or_else(|| {
            clap_noun_verb::NounVerbError::argument_error("Empty receipt chain")
        })?;

        Ok(InfoOutput {
            receipt_file,
            operation_id: latest.operation_id.clone(),
            timestamp: format_timestamp(&latest.timestamp),
            input_hashes: latest.input_hashes.clone(),
            output_hashes: latest.output_hashes.clone(),
            signature_present: !latest.signature.is_empty(),
            previous_hash: latest.previous_receipt_hash.clone(),
            own_hash: latest.hash().map_err(|e| {
                clap_noun_verb::NounVerbError::argument_error(&format!(
                    "Failed to compute receipt hash: {}",
                    e
                ))
            })?,
        })
    } else {
        Err(clap_noun_verb::NounVerbError::argument_error(
            "Invalid receipt file format. Expected a Receipt or ReceiptChain JSON.",
        ))
    }
}

/// Verify a receipt chain
#[verb]
fn chain_verify(
    chain_file: String,
    public_key: String,
) -> VerbResult<ChainVerifyOutput> {
    use ggen_receipt::ReceiptChain;

    let chain_path = PathBuf::from(&chain_file);

    if !chain_path.exists() {
        return Ok(ChainVerifyOutput {
            chain_file: chain_file.clone(),
            is_valid: false,
            message: format!("Chain file not found: {}", chain_file),
            receipt_count: 0,
            genesis_operation: None,
            latest_operation: None,
        });
    }

    let file_content = fs::read_to_string(&chain_path)
        .map_err(|e| {
            clap_noun_verb::NounVerbError::argument_error(&format!(
                "Failed to read chain file: {}",
                e
            ))
        })?;

    let chain: ReceiptChain = serde_json::from_str(&file_content)
        .map_err(|e| {
            clap_noun_verb::NounVerbError::argument_error(&format!(
                "Failed to parse chain: {}",
                e
            ))
        })?;

    let verifying_key = read_verifying_key(&PathBuf::from(public_key))
        .map_err(|e| clap_noun_verb::NounVerbError::argument_error(&e))?;

    let is_valid = chain.verify(&verifying_key).is_ok();
    let message = if is_valid {
        format!("Chain verified successfully: {} receipts", chain.len())
    } else {
        format!("Chain verification failed: {} receipts", chain.len())
    };
    let genesis = chain.genesis();
    let latest = chain.last();

    Ok(ChainVerifyOutput {
        chain_file,
        is_valid,
        message,
        receipt_count: chain.len(),
        genesis_operation: genesis.map(|r| r.operation_id.clone()),
        latest_operation: latest.map(|r| r.operation_id.clone()),
    })
}
