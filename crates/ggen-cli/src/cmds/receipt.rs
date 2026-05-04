//! Receipt Commands
//!
//! This module provides receipt verification and management commands.

use clap_noun_verb::Result as VerbResult;
use clap_noun_verb_macros::verb;
use ed25519_dalek::{SigningKey, VerifyingKey};
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

#[derive(Serialize)]
struct SignOutput {
    receipt_file: String,
    output_file: String,
    operation_id: String,
    own_hash: String,
    previous_receipt_hash: Option<String>,
    chain_file: Option<String>,
    chain_length: Option<usize>,
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Reads an Ed25519 signing key from a file (hex-encoded, 32 bytes / 64 hex chars).
/// Matches the convention used by `receipt_manager.rs` for `private.pem`.
fn read_signing_key(path: &PathBuf) -> Result<SigningKey, String> {
    let content =
        fs::read_to_string(path).map_err(|e| format!("Failed to read private key file: {}", e))?;

    let key_bytes = hex::decode(content.trim())
        .map_err(|e| format!("Failed to decode private key (expected hex): {}", e))?;

    if key_bytes.len() != 32 {
        return Err(format!(
            "Private key must be 32 bytes (got {})",
            key_bytes.len()
        ));
    }

    let key_array: [u8; 32] = key_bytes
        .try_into()
        .map_err(|_| "Invalid signing key length".to_string())?;
    Ok(SigningKey::from_bytes(&key_array))
}

/// Reads a verifying key from a file (hex-encoded or base64-encoded)
fn read_verifying_key(path: &PathBuf) -> Result<VerifyingKey, String> {
    let content =
        fs::read_to_string(path).map_err(|e| format!("Failed to read public key file: {}", e))?;

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
                .map_err(clap_noun_verb::NounVerbError::argument_error)?;
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
        message: "Invalid receipt file format. Expected a Receipt or ReceiptChain JSON."
            .to_string(),
        operation_id: None,
        timestamp: None,
        input_hashes: None,
        output_hashes: None,
        chain_position: None,
    }
}

fn verify_single_receipt(
    receipt_file: String, receipt: &ggen_receipt::Receipt, key: &Option<VerifyingKey>,
) -> VerbResult<VerifyOutput> {
    let verifying_key = match key {
        Some(k) => k,
        None => {
            return Ok(VerifyOutput {
                receipt_file,
                is_valid: false,
                message:
                    "No public key provided and no local key found at .ggen/keys/verifying.key. Use --public-key <path> or run ggen sync first to generate local keys."
                        .to_string(),
                operation_id: Some(receipt.operation_id.clone()),
                timestamp: Some(format_timestamp(&receipt.timestamp)),
                input_hashes: Some(receipt.input_hashes.len()),
                output_hashes: Some(receipt.output_hashes.len()),
                chain_position: receipt
                    .previous_receipt_hash
                    .as_ref()
                    .map(|_| "chained".to_string()),
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
        chain_position: receipt
            .previous_receipt_hash
            .as_ref()
            .map(|_| "chained".to_string()),
    })
}

fn verify_receipt_chain(
    receipt_file: String, chain: &ggen_receipt::ReceiptChain, key: &Option<VerifyingKey>,
) -> VerbResult<VerifyOutput> {
    let verifying_key = match key {
        Some(k) => k,
        None => {
            return Ok(VerifyOutput {
                receipt_file,
                is_valid: false,
                message: "No public key provided and no local key found at .ggen/keys/verifying.key. Use --public-key <path> or run ggen sync first to generate local keys."
                    .to_string(),
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
    receipt_file: &str, file_content: &str, key: &Option<VerifyingKey>,
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

#[verb]
fn verify(receipt_file: String, public_key: Option<String>) -> VerbResult<VerifyOutput> {
    let receipt_path = PathBuf::from(&receipt_file);

    if !receipt_path.exists() {
        return Ok(not_found_output(&receipt_file));
    }

    let file_content = fs::read_to_string(&receipt_path).map_err(|e| {
        clap_noun_verb::NounVerbError::argument_error(format!("Failed to read receipt file: {}", e))
    })?;

    // Auto-discover local key from .ggen/keys/verifying.key if no --public-key provided
    let resolved_key_path = if public_key.is_some() {
        public_key.clone()
    } else {
        let local_key = std::path::Path::new(".ggen/keys/verifying.key");
        if local_key.exists() {
            Some(local_key.to_string_lossy().to_string())
        } else {
            None
        }
    };
    let key = load_optional_key(&resolved_key_path)?;
    verify_receipt_content(&receipt_file, &file_content, &key)
}

/// Show detailed receipt information
#[verb]
fn info(receipt_file: String) -> VerbResult<InfoOutput> {
    use ggen_receipt::{Receipt, ReceiptChain};

    let receipt_path = PathBuf::from(&receipt_file);

    if !receipt_path.exists() {
        return Err(clap_noun_verb::NounVerbError::argument_error(format!(
            "Receipt file not found: {}",
            receipt_file
        )));
    }

    let file_content = fs::read_to_string(&receipt_path).map_err(|e| {
        clap_noun_verb::NounVerbError::argument_error(format!("Failed to read receipt file: {}", e))
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
                clap_noun_verb::NounVerbError::argument_error(format!(
                    "Failed to compute receipt hash: {}",
                    e
                ))
            })?,
        })
    } else if let Ok(chain) = serde_json::from_str::<ReceiptChain>(&file_content) {
        let latest = chain
            .last()
            .ok_or_else(|| clap_noun_verb::NounVerbError::argument_error("Empty receipt chain"))?;

        Ok(InfoOutput {
            receipt_file,
            operation_id: latest.operation_id.clone(),
            timestamp: format_timestamp(&latest.timestamp),
            input_hashes: latest.input_hashes.clone(),
            output_hashes: latest.output_hashes.clone(),
            signature_present: !latest.signature.is_empty(),
            previous_hash: latest.previous_receipt_hash.clone(),
            own_hash: latest.hash().map_err(|e| {
                clap_noun_verb::NounVerbError::argument_error(format!(
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
fn chain_verify(chain_file: String, public_key: String) -> VerbResult<ChainVerifyOutput> {
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

    let file_content = fs::read_to_string(&chain_path).map_err(|e| {
        clap_noun_verb::NounVerbError::argument_error(format!("Failed to read chain file: {}", e))
    })?;

    let chain: ReceiptChain = serde_json::from_str(&file_content).map_err(|e| {
        clap_noun_verb::NounVerbError::argument_error(format!("Failed to parse chain: {}", e))
    })?;

    let verifying_key = read_verifying_key(&PathBuf::from(public_key))
        .map_err(clap_noun_verb::NounVerbError::argument_error)?;

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

/// Sign an unsigned receipt with an Ed25519 key, optionally chaining to a previous receipt
#[verb]
fn sign(
    receipt_file: String, private_key: String, chain_file: Option<String>, output: Option<String>,
) -> VerbResult<SignOutput> {
    use ggen_receipt::{Receipt, ReceiptChain};

    let receipt_path = PathBuf::from(&receipt_file);
    if !receipt_path.exists() {
        return Err(clap_noun_verb::NounVerbError::argument_error(format!(
            "Receipt file not found: {}",
            receipt_file
        )));
    }

    let receipt_content = fs::read_to_string(&receipt_path).map_err(|e| {
        clap_noun_verb::NounVerbError::argument_error(format!("Failed to read receipt file: {}", e))
    })?;

    let mut receipt: Receipt = serde_json::from_str(&receipt_content).map_err(|e| {
        clap_noun_verb::NounVerbError::argument_error(format!(
            "Failed to parse unsigned receipt JSON: {}",
            e
        ))
    })?;

    let signing_key = read_signing_key(&PathBuf::from(&private_key))
        .map_err(clap_noun_verb::NounVerbError::argument_error)?;

    // Optional: link to chain by setting previous_receipt_hash to last chain entry.
    let mut chain_to_persist: Option<(PathBuf, ReceiptChain)> = None;
    if let Some(ref chain_path_str) = chain_file {
        let chain_path = PathBuf::from(chain_path_str);
        let chain = if chain_path.exists() {
            let chain_content = fs::read_to_string(&chain_path).map_err(|e| {
                clap_noun_verb::NounVerbError::argument_error(format!(
                    "Failed to read chain file: {}",
                    e
                ))
            })?;
            serde_json::from_str::<ReceiptChain>(&chain_content).map_err(|e| {
                clap_noun_verb::NounVerbError::argument_error(format!(
                    "Failed to parse chain file: {}",
                    e
                ))
            })?
        } else {
            ReceiptChain::new()
        };

        if let Some(last) = chain.last() {
            receipt = receipt.chain(last).map_err(|e| {
                clap_noun_verb::NounVerbError::argument_error(format!(
                    "Failed to link receipt to previous chain entry: {}",
                    e
                ))
            })?;
        } else {
            // Genesis: chain expects no previous_receipt_hash.
            receipt.previous_receipt_hash = None;
        }

        chain_to_persist = Some((chain_path, chain));
    }

    let signed = receipt.sign(&signing_key).map_err(|e| {
        clap_noun_verb::NounVerbError::argument_error(format!("Failed to sign receipt: {}", e))
    })?;

    let own_hash = signed.hash().map_err(|e| {
        clap_noun_verb::NounVerbError::argument_error(format!(
            "Failed to compute receipt hash: {}",
            e
        ))
    })?;

    // Persist signed receipt.
    let output_path = output.clone().unwrap_or_else(|| receipt_file.clone());
    let signed_json = serde_json::to_string_pretty(&signed).map_err(|e| {
        clap_noun_verb::NounVerbError::argument_error(format!(
            "Failed to serialize signed receipt: {}",
            e
        ))
    })?;
    fs::write(&output_path, &signed_json).map_err(|e| {
        clap_noun_verb::NounVerbError::argument_error(format!(
            "Failed to write signed receipt: {}",
            e
        ))
    })?;

    // Append to chain if requested.
    let chain_length = if let Some((chain_path, mut chain)) = chain_to_persist {
        chain.append(signed.clone()).map_err(|e| {
            clap_noun_verb::NounVerbError::argument_error(format!(
                "Failed to append signed receipt to chain: {}",
                e
            ))
        })?;
        let chain_json = serde_json::to_string_pretty(&chain).map_err(|e| {
            clap_noun_verb::NounVerbError::argument_error(format!(
                "Failed to serialize chain: {}",
                e
            ))
        })?;
        fs::write(&chain_path, &chain_json).map_err(|e| {
            clap_noun_verb::NounVerbError::argument_error(format!(
                "Failed to write chain file: {}",
                e
            ))
        })?;
        Some(chain.len())
    } else {
        None
    };

    Ok(SignOutput {
        receipt_file,
        output_file: output_path,
        operation_id: signed.operation_id.clone(),
        own_hash,
        previous_receipt_hash: signed.previous_receipt_hash.clone(),
        chain_file,
        chain_length,
    })
}
