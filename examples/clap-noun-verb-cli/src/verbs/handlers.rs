//! Hand-written handler seam behind the generated `#[verb]` wrappers in
//! `clap_noun_verb_routes.rs`. Business logic lives here, never in the
//! generated file — a missing handler here is a compile error, not a
//! silent no-op.

use clap_noun_verb::Result;
use serde_json::{json, Value};

/// `session login <token>` — persist a session token.
///
/// This example keeps state in-process only (no real keyring/filesystem
/// persistence) to stay a minimal, dependency-free demonstration of the
/// generated-route -> handler seam; a real CLI would write to a keyring or
/// a local state file here.
pub fn session_login_handler(token: String) -> Result<Value> {
    Ok(json!({
        "status": "logged_in",
        "token_prefix": token.chars().take(4).collect::<String>(),
    }))
}

/// `session verify` — verify the active session token.
pub fn session_verify_handler() -> Result<Value> {
    Ok(json!({
        "status": "verified",
    }))
}

/// `user create <name> [--email <email>]` — create a user resource.
pub fn user_create_handler(name: String, email: Option<String>) -> Result<Value> {
    Ok(json!({
        "status": "created",
        "name": name,
        "email": email,
    }))
}
