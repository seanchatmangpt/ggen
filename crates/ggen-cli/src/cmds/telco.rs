//! Universal Telco surface over CLI, MCP, A2A, local, edge, fog, and cloud.

use std::path::Path;

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use serde_json::Value;

use super::maximalism;

/// Inspect office, register, line, bridge, and record closure.
#[verb]
pub fn inspect(manifest: String) -> Result<Value> {
    maximalism::telco_report(Path::new(&manifest))
}

/// Inspect the Telco office namespace surface.
#[verb]
pub fn office(manifest: String) -> Result<Value> {
    maximalism::telco_surface(Path::new(&manifest), "office")
}

/// Inspect the Telco capability register surface.
#[verb]
pub fn register(manifest: String) -> Result<Value> {
    maximalism::telco_surface(Path::new(&manifest), "register")
}

/// Inspect the typed Telco line surface.
#[verb]
pub fn line(manifest: String) -> Result<Value> {
    maximalism::telco_surface(Path::new(&manifest), "line")
}

/// Inspect the protocol and trust-domain bridge surface.
#[verb]
pub fn bridge(manifest: String) -> Result<Value> {
    maximalism::telco_surface(Path::new(&manifest), "bridge")
}

/// Inspect immutable communication record and settlement evidence.
#[verb]
pub fn record(manifest: String) -> Result<Value> {
    maximalism::telco_surface(Path::new(&manifest), "record")
}
