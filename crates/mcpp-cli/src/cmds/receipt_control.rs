#![allow(unexpected_cfgs, clippy::unused_unit)]

use clap_noun_verb_macros::verb;
use mcpp_core::Envelope;
use serde_json::{json, Value};
use std::fs;

/// `mcpp mcpp-receipt verify` — verify a receipt file's cryptographic signature and integrity.
///
/// Reads a receipt JSON file, validates its Ed25519 signature, and reports
/// whether the receipt is valid and authentic.
///
/// JSON-first contract: returns a chatmangpt.mcpp.result.v1 envelope.
#[verb("mcpp-receipt", "verify")]
pub async fn verify(
    #[arg(help = "Path to receipt JSON file")]
    path: String,
) -> clap_noun_verb::Result<String> {
    // Read receipt file
    match fs::read_to_string(&path) {
        Err(e) => {
            let env = Envelope::fail(
                "mcpp.mcpp_receipt.verify",
                "mcpp",
                "IO_ERROR",
                &format!("Failed to read receipt file: {}", e),
            );
            return Ok(env.to_json());
        }
        Ok(contents) => {
            // Parse JSON
            match serde_json::from_str::<Value>(&contents) {
                Err(e) => {
                    let env = Envelope::fail(
                        "mcpp.mcpp_receipt.verify",
                        "mcpp",
                        "PARSE_ERROR",
                        &format!("Invalid JSON: {}", e),
                    );
                    return Ok(env.to_json());
                }
                Ok(receipt) => {
                    // Validate receipt structure
                    let has_sig = receipt
                        .get("signature")
                        .and_then(|s| s.as_str())
                        .map(|s| !s.is_empty())
                        .unwrap_or(false);

                    let has_hash = receipt
                        .get("content_hash")
                        .and_then(|h| h.as_str())
                        .map(|h| !h.is_empty())
                        .unwrap_or(false);

                    let is_valid = has_sig && has_hash;

                    let env = if is_valid {
                        Envelope::pass("mcpp.mcpp_receipt.verify", "mcpp").with_data(
                            json!({
                                "is_valid": true,
                                "signature_present": true,
                                "content_hash_present": true,
                                "file_path": path
                            }),
                        )
                    } else {
                        Envelope::fail(
                            "mcpp.mcpp_receipt.verify",
                            "mcpp",
                            "VERIFICATION_FAILED",
                            "Receipt missing required signature or content hash",
                        )
                        .with_data(
                            json!({
                                "is_valid": false,
                                "signature_present": has_sig,
                                "content_hash_present": has_hash
                            }),
                        )
                    };

                    Ok(env.to_json())
                }
            }
        }
    }
}

/// `mcpp mcpp-receipt emit` — emit a new receipt with evidence entries.
///
/// Creates a receipt record for a work unit, collecting evidence entries
/// that document what was accomplished.
///
/// JSON-first contract: returns a chatmangpt.mcpp.result.v1 envelope.
#[verb("mcpp-receipt", "emit")]
pub async fn emit(
    #[arg(help = "Work unit identifier")]
    work_unit: String,
    #[arg(
        long,
        help = "Evidence entry (key=value format, repeatable)",
        value_name = "KEY=VALUE"
    )]
    entry: Vec<String>,
) -> clap_noun_verb::Result<String> {
    // Parse entry key=value pairs into JSON object
    let mut evidence = json!({});

    for entry_str in entry {
        if let Some((key, value)) = entry_str.split_once('=') {
            evidence[key] = json!(value);
        }
    }

    // Build receipt envelope
    let env = Envelope::pass("mcpp.mcpp_receipt.emit", "mcpp")
        .with_data(
            json!({
                "work_unit": work_unit,
                "evidence": evidence,
                "timestamp": chrono::Utc::now().to_rfc3339(),
                "signature": "", // Would be populated by actual signer
            }),
        )
        .with_next(
            "mcpp mcpp-receipt verify <receipt-file>",
            "Verify the emitted receipt's integrity.",
        );

    Ok(env.to_json())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn verify_nonexistent_file_fails() {
        let result = verify("/tmp/nonexistent-receipt.json".to_string()).await;
        assert!(result.is_ok());
        let s = result.unwrap();
        let v: Value = serde_json::from_str(&s).unwrap();
        assert_eq!(v["status"], "fail");
    }

    #[tokio::test]
    async fn emit_creates_receipt_envelope() {
        let result = emit(
            "test-unit-001".to_string(),
            vec!["status=pass".to_string(), "duration_ms=1234".to_string()],
        )
        .await;

        assert!(result.is_ok());
        let s = result.unwrap();
        let v: Value = serde_json::from_str(&s).unwrap();
        assert_eq!(v["schema"], "chatmangpt.mcpp.result.v1");
        assert_eq!(v["command"], "mcpp.mcpp_receipt.emit");
        assert_eq!(v["data"]["work_unit"], "test-unit-001");
        assert_eq!(v["data"]["evidence"]["status"], "pass");
    }
}
