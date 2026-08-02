//! Universal non-actuating Wizard surface.

use std::path::Path;

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use serde_json::Value;

use super::maximalism;

/// Inspect accepted Wizard-domain capability closure.
#[verb]
pub fn inspect(manifest: String) -> Result<Value> {
    maximalism::wizard_domain(Path::new(&manifest))
}

/// Construct a bounded, dependency-ordered production-cell plan.
#[verb]
pub fn plan(manifest: String, capability: String) -> Result<Value> {
    maximalism::wizard_plan(Path::new(&manifest), &capability)
}
