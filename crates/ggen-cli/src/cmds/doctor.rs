//! Universal non-actuating Doctor surface.

use std::path::Path;

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use serde_json::Value;

use super::maximalism;

/// Diagnose the complete admissible-work program and prescribe deterministic remediation.
#[verb]
///
/// # Errors
///
/// Returns an error if the operation cannot be completed.
pub fn inspect(manifest: String) -> Result<Value> {
    maximalism::doctor_report(Path::new(&manifest))
}

/// Inspect accepted Doctor-domain capability closure.
#[verb]
///
/// # Errors
///
/// Returns an error if the operation cannot be completed.
pub fn domain(manifest: String) -> Result<Value> {
    maximalism::doctor_domain(Path::new(&manifest))
}
