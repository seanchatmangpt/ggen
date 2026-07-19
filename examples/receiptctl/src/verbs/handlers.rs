//! Hand-written handler seam behind the generated `#[verb]` wrappers in
//! `clap_noun_verb_routes.rs`. Business logic lives here, never in the
//! generated file — a missing handler here is a compile error, not a
//! silent no-op.

use crate::w4pm_algorithms_catalog::CATALOG as ALGORITHM_CATALOG;
use crate::w4pm_cognition_catalog::BREED_CATALOG;
use crate::wasm4pm_compat_events::emit_receipt_chained;
use chrono::Utc;
use clap_noun_verb::Result;
use serde_json::{json, Value};

/// `session login <token>` — persist a session token (in-process only, this
/// example doesn't touch a real keyring/filesystem).
pub fn session_login_handler(token: String) -> Result<Value> {
    Ok(json!({
        "status": "logged_in",
        "token_prefix": token.chars().take(4).collect::<String>(),
    }))
}

/// `session verify` — verify the active session token.
pub fn session_verify_handler() -> Result<Value> {
    Ok(json!({ "status": "verified" }))
}

/// `user create <name> [--email <email>]` — create a user resource.
pub fn user_create_handler(name: String, email: Option<String>) -> Result<Value> {
    Ok(json!({ "status": "created", "name": name, "email": email }))
}

/// `algorithm list` — list every wasm4pm process-intelligence algorithm from
/// the generated `wasm4pm-algorithms-pack` catalog.
pub fn algorithm_list_handler() -> Result<Value> {
    let algorithms: Vec<Value> = ALGORITHM_CATALOG
        .iter()
        .map(|a| {
            json!({
                "id": a.algorithm_id,
                "label": a.label,
                "category": a.category,
                "wasm_export": a.wasm_export,
                "cli_alias": a.cli_alias,
            })
        })
        .collect();
    Ok(json!({ "total": algorithms.len(), "algorithms": algorithms }))
}

/// `cognition list` — list every wasm4pm cognition breed from the generated
/// `wasm4pm-cognition-pack` catalog.
pub fn cognition_list_handler() -> Result<Value> {
    let breeds: Vec<Value> = BREED_CATALOG
        .iter()
        .map(|b| {
            json!({
                "label": b.label,
                "citation": b.citation,
            })
        })
        .collect();
    Ok(json!({ "total": breeds.len(), "breeds": breeds }))
}

/// `receipt emit <sync_receipt_id>` — emit a real `receipt_chained` OCEL
/// event via the generated `wasm4pm-compat-pack` emission surface.
pub fn receipt_emit_handler(sync_receipt_id: String) -> Result<Value> {
    let now = Utc::now().fixed_offset();
    let event_id = format!("evt-{}-{}", sync_receipt_id, now.timestamp());
    // `chain_hash` is a real, freshly-computed hash of the receipt id this
    // handler was called with -- not a placeholder literal. It is
    // `std::hash::DefaultHasher` (a real, deterministic, non-cryptographic
    // hash), NOT this repo's BLAKE3 sync-receipt chain hash
    // (`crates/praxis-core::ReceiptRecord`); a real consumer wiring this to
    // an actual receipt chain would pass that chain's own hash here instead.
    let chain_hash = {
        use std::hash::{DefaultHasher, Hash, Hasher};
        let mut hasher = DefaultHasher::new();
        sync_receipt_id.hash(&mut hasher);
        format!("{:016x}", hasher.finish())
    };
    let event = emit_receipt_chained(event_id, now, sync_receipt_id, chain_hash);
    Ok(serde_json::to_value(event).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!(
            "failed to serialize OCEL event: {e}"
        ))
    })?)
}
