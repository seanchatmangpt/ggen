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
    receipt_file: String, receipt: &mcpp_receipt::Receipt, key: &Option<VerifyingKey>,
) -> VerbResult<VerifyOutput> {
    let verifying_key = match key {
        Some(k) => k,
        None => {
            return Ok(VerifyOutput {
                receipt_file,
                is_valid: false,
                message:
                    "Public key required for single receipt verification. Use --public-key <path>"
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
    receipt_file: String, chain: &mcpp_receipt::ReceiptChain, key: &Option<VerifyingKey>,
) -> VerbResult<VerifyOutput> {
    let verifying_key = match key {
        Some(k) => k,
        None => {
            return Ok(VerifyOutput {
                receipt_file,
                is_valid: false,
                message: "Public key required for chain verification. Use --public-key <path>"
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
    use mcpp_receipt::{Receipt, ReceiptChain};

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
///
/// Validates the Ed25519 signature and chain integrity of a receipt or receipt chain.
/// This command ensures that the receipt has not been tampered with and that all
/// cryptographic signatures are valid.
///
/// The verification process:
/// 1. Reads the receipt file (JSON format)
/// 2. Parses as either a single Receipt or a ReceiptChain
/// 3. Verifies Ed25519 signature using the provided public key
/// 4. Checks chain integrity (for chained receipts)
///
/// ## Arguments
///
/// * `receipt_file` - Path to the receipt JSON file to verify
///   - Can be a single receipt or a receipt chain
///   - Must exist and be valid JSON
///   - **Required**
///
/// * `public_key` - Path to the Ed25519 verifying key file
///   - Key must be 32 bytes (hex or base64 encoded)
///   - If not provided, command will report that key is required
///   - **Optional** (but verification will fail without it)
///
/// ## Examples
///
/// ### Verify a single receipt
/// ```bash
/// mcpp receipt verify .mcpp/receipts/latest.json
/// ```
///
/// ### Verify with explicit public key
/// ```bash
/// mcpp receipt verify .mcpp/receipts/latest.json --public-key .mcpp/keys/verifying.key
/// ```
///
/// ### Verify a receipt chain
/// ```bash
/// mcpp receipt verify .mcpp/receipts/chain.json --public-key .mcpp/keys/verifying.key
/// ```
///
/// ## Exit Codes
///
/// * **0** - Receipt is valid (signature verified, chain integrity confirmed)
/// * **1** - Verification failed (invalid signature, corrupted file, or missing key)
///
/// ## Output Format
///
/// The command outputs JSON with the following fields:
/// * `receipt_file` - Path to the verified receipt
/// * `is_valid` - Boolean indicating verification success
/// * `message` - Human-readable verification result
/// * `operation_id` - UUID of the operation (if present)
/// * `timestamp` - When the receipt was created
/// * `input_hashes` - Number of input file hashes
/// * `output_hashes` - Number of output file hashes
/// * `chain_position` - Position in chain (e.g., "chained", "chain of N receipts")
///
/// ## Ed25519 Signature Requirements
///
/// Receipts are signed using Ed25519:
/// * **Signing key** - Private key used to create receipts (stored in `.mcpp/keys/signing.key`)
/// * **Verifying key** - Public key used to verify receipts (stored in `.mcpp/keys/verifying.key`)
/// * **Key format** - 32 bytes, hex or base64 encoded
/// * **Signature** - 64 bytes Ed25519 signature stored in receipt `signature` field
///
/// To extract the verifying key from a signing key:
/// ```bash
/// # The verifying key is derived from the signing key
/// # Use mcpp key commands to manage keypairs
/// mcpp key extract-public --private .mcpp/keys/signing.key --output .mcpp/keys/verifying.key
/// ```
///
/// ## Security Considerations
///
/// * Always verify receipts after `mcpp sync` operations
/// * Store the verifying key securely (it cannot be changed without breaking verification)
/// * A missing or empty signature field causes verification to fail
/// * Chain verification ensures no receipts were removed or reordered
#[verb]
fn verify(
    receipt_file: String,
    public_key: Option<String>,
) -> VerbResult<VerifyOutput> {
    let receipt_path = PathBuf::from(&receipt_file);

    if !receipt_path.exists() {
        return Ok(not_found_output(&receipt_file));
    }

    let file_content = fs::read_to_string(&receipt_path).map_err(|e| {
        clap_noun_verb::NounVerbError::argument_error(&format!(
            "Failed to read receipt file: {}",
            e
        ))
    })?;

    let key = load_optional_key(&public_key)?;
    verify_receipt_content(&receipt_file, &file_content, &key)
}

/// Show detailed receipt information
///
/// Displays comprehensive information about a receipt or receipt chain, including
/// operation metadata, hash values, and chain position. This command is useful for
/// inspecting receipt contents without performing verification.
///
/// The info command extracts:
/// * Operation ID (UUID v4)
/// * Timestamp (RFC 3339)
/// * Input and output file hashes
/// * Signature presence (does not verify validity)
/// * Chain position (previous hash for chained receipts)
/// * Computed receipt hash (SHA-256)
///
/// ## Arguments
///
/// * `receipt_file` - Path to the receipt JSON file to inspect
///   - Can be a single receipt or a receipt chain
///   - Must exist and be valid JSON
///   - **Required**
///
/// ## Examples
///
/// ### Show info for latest receipt
/// ```bash
/// mcpp receipt info .mcpp/receipts/latest.json
/// ```
///
/// ### Show info for receipt chain
/// ```bash
/// mcpp receipt info .mcpp/receipts/chain.json
/// ```
///
/// ### Pretty-print JSON output
/// ```bash
/// mcpp receipt info .mcpp/receipts/latest.json | jq
/// ```
///
/// ## Output Format
///
/// The command outputs JSON with the following fields:
/// * `receipt_file` - Path to the inspected receipt
/// * `operation_id` - UUID of the operation (e.g., "550e8400-e29b-41d4-a716-446655440000")
/// * `timestamp` - When the receipt was created (UTC, format: "YYYY-MM-DD HH:MM:SS UTC")
/// * `input_hashes` - Object mapping pack@version to SHA-256 hashes
/// * `output_hashes` - Object mapping generated file paths to SHA-256 hashes
/// * `signature_present` - Boolean indicating if signature field is non-empty
/// * `previous_hash` - Hash of previous receipt in chain (if chained)
/// * `own_hash` - Computed SHA-256 hash of this receipt
///
/// ## Receipt Chain Information
///
/// For receipt chains, the command displays information about the **latest**
/// receipt in the chain. To inspect earlier receipts, use `mcpp receipt verify`
/// with the chain file and examine individual receipts.
///
/// ## Signature Presence vs. Validity
///
/// The `signature_present` field only indicates whether a signature exists
/// in the receipt. It does **not** verify that the signature is valid.
/// To verify cryptographic validity, use `mcpp receipt verify`.
///
/// ## Hash Computation
///
/// The `own_hash` field is computed by:
/// 1. Canonicalizing the receipt JSON (deterministic serialization)
/// 2. Computing SHA-256 hash of the canonical form
/// 3. Encoding as hex string
///
/// This hash is used for chain linking: each receipt's `previous_hash`
/// field should match the `own_hash` of the preceding receipt.
///
/// ## Use Cases
///
/// * **Debugging** - Inspect receipt structure when verification fails
/// * **Auditing** - Review what files were generated in an operation
/// * **Chain tracing** - Follow the history of operations via previous_hash
/// * **Metadata extraction** - Get operation IDs and timestamps for logging
#[verb]
fn info(
    receipt_file: String,
) -> VerbResult<InfoOutput> {
    use mcpp_receipt::{Receipt, ReceiptChain};

    let receipt_path = PathBuf::from(&receipt_file);

    if !receipt_path.exists() {
        return Err(clap_noun_verb::NounVerbError::argument_error(&format!(
            "Receipt file not found: {}",
            receipt_file
        )));
    }

    let file_content = fs::read_to_string(&receipt_path).map_err(|e| {
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
///
/// Validates the cryptographic integrity of an entire receipt chain, ensuring that
/// all receipts are properly signed and linked. This is the strongest form of
/// verification for multi-operation workflows.
///
/// Chain verification process:
/// 1. Loads the receipt chain from the specified file
/// 2. Verifies each receipt's Ed25519 signature using the provided public key
/// 3. Validates chain linkage (each receipt's `previous_hash` matches the prior receipt's hash)
/// 4. Ensures no receipts are missing or reordered
///
/// A valid chain proves:
/// * **Authenticity** - All receipts were signed by the holder of the private key
/// * **Integrity** - No receipts were modified after signing
/// * **Continuity** - The chain is complete and in correct order
/// * **Non-repudiation** - The signer cannot deny creating these receipts
///
/// ## Arguments
///
/// * `chain_file` - Path to the receipt chain JSON file
///   - Must be a valid ReceiptChain JSON array
///   - Must contain at least one receipt
///   - **Required**
///
/// * `public_key` - Path to the Ed25519 verifying key file
///   - Key must be 32 bytes (hex or base64 encoded)
///   - Must be the corresponding public key for the signing key used to create the chain
///   - **Required** (command will fail without it)
///
/// ## Examples
///
/// ### Verify a receipt chain
/// ```bash
/// mcpp receipt chain-verify .mcpp/receipts/chain.json --public-key .mcpp/keys/verifying.key
/// ```
///
/// ### Verify with short option
/// ```bash
/// mcpp receipt chain-verify .mcpp/receipts/chain.json -k .mcpp/keys/verifying.key
/// ```
///
/// ### Pretty-print verification result
/// ```bash
/// mcpp receipt chain-verify .mcpp/receipts/chain.json -k .mcpp/keys/verifying.key | jq
/// ```
///
/// ## Exit Codes
///
/// * **0** - Chain is valid (all signatures verified, linkage intact)
/// * **1** - Verification failed (invalid signature, broken linkage, or missing file)
///
/// ## Output Format
///
/// The command outputs JSON with the following fields:
/// * `chain_file` - Path to the verified chain
/// * `is_valid` - Boolean indicating verification success
/// * `message` - Human-readable verification result with receipt count
/// * `receipt_count` - Number of receipts in the chain
/// * `genesis_operation` - Operation ID of the first receipt in chain
/// * `latest_operation` - Operation ID of the most recent receipt
///
/// ## Chain Structure
///
/// A receipt chain is a JSON array of receipts where each receipt (except the first)
/// contains a `previous_hash` field that links to the prior receipt:
///
/// ```json
/// [
///   {
///     "operation_id": "uuid-1",
///     "previous_hash": null,
///     "signature": "..."
///   },
///   {
///     "operation_id": "uuid-2",
///     "previous_hash": "hash-of-receipt-1",
///     "signature": "..."
///   }
/// ]
/// ```
///
/// Chain verification ensures:
/// * `receipt_1.previous_hash` is `null` (genesis receipt)
/// * `receipt_2.previous_hash` equals `hash(receipt_1)`
/// * `receipt_3.previous_hash` equals `hash(receipt_2)`
/// * And so on for all receipts in the chain
///
/// ## Security Properties
///
/// A verified receipt chain provides:
/// * **Tamper evidence** - Any modification to any receipt breaks verification
/// * **Insertion detection** - Adding receipts to the middle breaks hash linkage
/// * **Deletion detection** - Removing receipts breaks hash linkage
/// * **Reordering detection** - Swapping receipts breaks hash linkage
///
/// ## Comparison with `verify` Command
///
/// * `mcpp receipt verify` - Verifies a single receipt or chain (basic check)
/// * `mcpp receipt chain-verify` - Verifies chain with detailed metadata (advanced)
///
/// Use `chain-verify` when you need:
/// * Detailed chain statistics (count, genesis, latest)
/// * Structured output for automation
/// * Explicit verification of multi-operation workflows
///
/// ## Troubleshooting
///
/// ### "Chain file not found"
/// * Ensure the chain file path is correct
/// * Check that the file exists and is readable
///
/// ### "Failed to parse chain"
/// * Ensure the file is valid JSON
/// * Verify it's a ReceiptChain array, not a single Receipt object
///
/// ### "Invalid verifying key"
/// * Ensure the key file is 32 bytes (hex or base64)
/// * Verify the key matches the signing key used to create receipts
/// * Use `mcpp key extract-public` to derive the correct verifying key
///
/// ### "Chain verification failed"
/// * Check that all receipts have valid signatures
/// * Verify that `previous_hash` fields match the prior receipt's hash
/// * Ensure no receipts were manually edited or reordered
#[verb]
fn chain_verify(
    chain_file: String,
    public_key: String,
) -> VerbResult<ChainVerifyOutput> {
    use mcpp_receipt::ReceiptChain;

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
        clap_noun_verb::NounVerbError::argument_error(&format!("Failed to read chain file: {}", e))
    })?;

    let chain: ReceiptChain = serde_json::from_str(&file_content).map_err(|e| {
        clap_noun_verb::NounVerbError::argument_error(&format!("Failed to parse chain: {}", e))
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
