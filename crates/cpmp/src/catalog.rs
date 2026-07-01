//! `cpmp catalog` — Computer Project Mapping Protocol command surface.
//!
//! These clap-noun-verb `#[verb]` functions replace the former hand-rolled clap
//! `Subcommand` enum in `main.rs`. clap-noun-verb derives the CLI from each
//! function signature and auto-discovers the verbs at startup, so the binary
//! stays a one-line `cpmp::run_cli()`. Arguments are taken as `String`s (the
//! scalar types the `#[verb]` macro maps to CLI args) and converted to `PathBuf`
//! here.

use clap_noun_verb::{NounVerbError, Result};
use clap_noun_verb_macros::verb;
use serde_json::{json, Value};
use std::path::PathBuf;

/// Scan a project path and write the catalog into `--out` (default `.cpmp`).
#[verb]
pub fn scan(#[arg(index = 1)] path: String, out: Option<String>) -> Result<Value> {
    let path = PathBuf::from(&path);
    let out = out
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(".cpmp"));
    crate::scanner::scan(std::slice::from_ref(&path), &out)
        .map_err(|e| NounVerbError::execution_error(format!("scan failed: {e}")))?;
    Ok(json!({
        "scanned_path": path.display().to_string(),
        "out": out.display().to_string(),
    }))
}

/// Verify that no catalog entries were deleted between two scan snapshots.
///
/// The verb name is set explicitly: clap-noun-verb strips the `verify_` prefix
/// from the function name, which would otherwise yield the verb `no-deletion`.
#[verb("verify-no-deletion")]
pub fn verify_no_deletion(before: String, after: String) -> Result<Value> {
    let before = PathBuf::from(&before);
    let after = PathBuf::from(&after);
    crate::receipt::verify_no_deletion(&before, &after)
        .map_err(|e| NounVerbError::execution_error(format!("verification failed: {e}")))?;
    Ok(json!({
        "verified": true,
        "before": before.display().to_string(),
        "after": after.display().to_string(),
    }))
}
