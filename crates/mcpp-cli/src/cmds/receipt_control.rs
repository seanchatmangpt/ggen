#![allow(unexpected_cfgs, clippy::unused_unit)]

use clap_noun_verb_macros::verb;
use mcpp_core::Envelope;
use serde_json::{json, Value};

/// `mcpp mcpp-receipt emit` — emit a new receipt with evidence entries.
///
/// Creates a receipt record with work unit and evidence data,
/// returning a JSON-encoded result envelope.
///
/// JSON-first contract: returns a chatmangpt.mcpp.result.v1 envelope.
#[verb("mcpp-receipt", "emit")]
pub fn emit() -> clap_noun_verb::Result<String> {
    // Build receipt envelope with placeholder data
    let work_unit = "mcpp-receipt-001";
    let env = Envelope::pass("mcpp.mcpp_receipt.emit", "mcpp")
        .with_data(json!({
            "work_unit": work_unit,
            "evidence": {
                "status": "pass",
                "timestamp": chrono::Utc::now().to_rfc3339(),
            },
            "signature": "", // Would be populated by actual signer
        }))
        .with_next(
            "mcpp mcpp-receipt verify <receipt-file>",
            "Verify the emitted receipt's integrity.",
        );

    Ok(env.to_json())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn emit_creates_receipt_envelope() {
        let result = emit();
        assert!(result.is_ok());
        let s = result.unwrap();
        let v: Value = serde_json::from_str(&s).unwrap();
        assert_eq!(v["schema"], "chatmangpt.mcpp.result.v1");
        assert_eq!(v["command"], "mcpp.mcpp_receipt.emit");
        assert_eq!(v["status"], "pass");
        assert!(!v["data"]["evidence"].is_null());
    }
}
