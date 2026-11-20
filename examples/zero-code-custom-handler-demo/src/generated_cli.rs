//! Typed noun-verb routes GENERATED from first-class RDF arguments.
//!
//! Every body closes through `BehaviorSpec`, except commands carrying the
//! explicit, admitted `cnv:CustomBehavior` opt-out, which route to a
//! hand-written `crate::custom_handlers::<noun>_<verb>` function instead. That
//! is the sole consumer handler seam this compiler ever emits, and it is
//! visible per-command in the source ontology, not a blanket escape hatch.

use crate::generated_behavior::{self, BehaviorSpec};
use clap_noun_verb::{NounVerbError, Result};
use clap_noun_verb_macros::verb;
use serde::Serialize;
use serde_json::{Map, Value};
#[doc = "Look up a price via hand-written domain logic."]
#[doc = ""]
#[doc = "# Arguments"]
/// * `sku` - Generated argument metadata.
#[verb("lookup", "price")]
fn price_lookup(#[arg(help = "Product SKU.", index = 1)] sku: String) -> Result<Value> {
    let mut inputs = Map::new();
    inputs.insert("sku".to_owned(), encode_input("sku", &sku)?);
    crate::custom_handlers::price_lookup(inputs)
}

#[doc = "Return a deterministic liveness value."]
#[verb("ping", "system")]
fn system_ping() -> Result<Value> {
    let inputs = Map::new();
    generated_behavior::execute(
        BehaviorSpec::StaticJson {
            json: "{\"status\":\"alive\"}",
        },
        inputs,
    )
}

#[linkme::distributed_slice(::clap_noun_verb::cli::registry::__NOUN_REGISTRY)]
static REGISTER_PRICE_NOUN: fn() = register_price_noun;

fn register_price_noun() {
    ::clap_noun_verb::cli::registry::CommandRegistry::register_noun(
        "price",
        "Domain-specific pricing logic that the six closed primitives cannot express.",
    );
}

#[linkme::distributed_slice(::clap_noun_verb::cli::registry::__NOUN_REGISTRY)]
static REGISTER_SYSTEM_NOUN: fn() = register_system_noun;

fn register_system_noun() {
    ::clap_noun_verb::cli::registry::CommandRegistry::register_noun(
        "system",
        "Inspect the system.",
    );
}

#[allow(dead_code)]
fn encode_input<T: Serialize>(name: &str, value: &T) -> Result<Value> {
    serde_json::to_value(value).map_err(|error| {
        NounVerbError::execution_error(format!(
            "failed to encode generated argument {name:?}: {error}"
        ))
    })
}

#[cfg(test)]
mod generated_route_proofs {
    const GENERATED_SOURCE: &str =
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/src/generated_cli.rs"));

    fn routes_only() -> &'static str {
        GENERATED_SOURCE
            .split("#[cfg(test)]")
            .next()
            .unwrap_or(GENERATED_SOURCE)
    }

    #[test]
    fn every_admitted_command_became_one_registered_verb() {
        assert_eq!(routes_only().matches("#[verb(").count(), 2);
    }

    #[test]
    fn no_consumer_handler_seam_survives_generation() {
        // `custom_handlers::` is the sole, explicitly admitted escape hatch
        // (cnv:CustomBehavior); strip it before checking for the legacy,
        // unbounded `handlers::` seam this compiler never emits otherwise.
        let without_admitted_custom_seam = routes_only().replace("custom_handlers::", "");
        assert!(!without_admitted_custom_seam.contains("handlers::"));
        assert!(!routes_only().contains("todo!"));
        assert!(!routes_only().contains("unimplemented!"));
    }

    #[test]
    fn every_custom_command_routes_to_its_own_handler() {
        // Proves per-command wiring, not just that a seam exists somewhere:
        // each cnv:CustomBehavior command must call the one handler function
        // named after ITS OWN noun and verb, not a neighboring command's.
        assert!(
            routes_only().contains("custom_handlers::price_lookup("),
            "price lookup must route to its own custom handler"
        );
    }
}
