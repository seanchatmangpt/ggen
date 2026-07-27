//! Hand-written domain logic for every `cnv:CustomBehavior` command.
//!
//! GENERATED ONCE by ggen (`unless_exists: true` — this file is scaffolding,
//! never overwritten by a later `ggen sync run`, so your edits below are
//! safe). Every function here is called directly by `src/generated_cli.rs`;
//! a missing or renamed function is a compile error, not a silent gap. This
//! is the sole consumer handler seam the zero-code compiler ever emits, and
//! only for commands that explicitly opted out of the closed behavior
//! primitives by declaring `cnv:CustomBehavior` in the ontology.

use clap_noun_verb::Result;
use serde_json::{Map, Value};

/// `price lookup` -- Look up a price via hand-written domain logic.
///
/// This is exactly what `cnv:CustomBehavior` exists for: a small lookup
/// table keyed by SKU is not expressible by the six closed primitives (no
/// primitive supports branching on an argument's value against a domain
/// table). Real deployments would query a database or an external service
/// here; this demo uses a fixed table to stay a real, executable proof
/// without a network dependency.
pub fn price_lookup(inputs: Map<String, Value>) -> Result<Value> {
    let sku = inputs
        .get("sku")
        .and_then(Value::as_str)
        .ok_or_else(|| clap_noun_verb::NounVerbError::execution_error("missing sku argument"))?;

    let price_cents = match sku {
        "SKU-1" => 1999,
        "SKU-2" => 4200,
        other => {
            return Err(clap_noun_verb::NounVerbError::execution_error(format!(
                "unknown sku {other:?}"
            )));
        }
    };

    Ok(serde_json::json!({ "sku": sku, "price_cents": price_cents }))
}
