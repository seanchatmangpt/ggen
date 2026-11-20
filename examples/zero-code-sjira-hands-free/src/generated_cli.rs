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
#[doc = "Admit one WorkOrder JSON file via ggen sjira admit."]
#[doc = ""]
#[doc = "# Arguments"]
/// * `file` - Generated argument metadata.
#[verb("admit", "issue")]
fn issue_admit(
    #[arg(help = "Path to a WorkOrder JSON file.", index = 1)] file: std::path::PathBuf,
) -> Result<Value> {
    let mut inputs = Map::new();
    inputs.insert("file".to_owned(), encode_input("file", &file)?);
    crate::custom_handlers::issue_admit(inputs)
}

#[doc = "Run the sjira execute loop via ggen sjira execute."]
#[doc = ""]
#[doc = "# Arguments"]
/// * `authority_path` - Generated argument metadata.
/// * `epoch_extensions_path` - Generated argument metadata.
/// * `epoch_watermark_path` - Generated argument metadata.
/// * `lease_path` - Generated argument metadata.
/// * `ledger_path` - Generated argument metadata.
/// * `max` - Generated argument metadata.
/// * `orders_path` - Generated argument metadata.
#[verb("execute", "system")]
fn system_execute(
    #[arg(help = "Authority graph JSON for execute.")] authority_path: std::path::PathBuf,
    #[arg(help = "Optional epoch extensions JSON.")] epoch_extensions_path: Option<
        std::path::PathBuf,
    >,
    #[arg(help = "Optional epoch watermark JSON.")] epoch_watermark_path: Option<
        std::path::PathBuf,
    >,
    #[arg(help = "Optional admitted lease JSON.")] lease_path: Option<std::path::PathBuf>,
    #[arg(help = "Path to the real file ledger.")] ledger_path: std::path::PathBuf,
    #[arg(help = "Optional max execute-loop iterations.")] max: Option<u64>,
    #[arg(help = "JSON array of frontier order entries.")] orders_path: std::path::PathBuf,
) -> Result<Value> {
    let mut inputs = Map::new();
    inputs.insert(
        "authority_path".to_owned(),
        encode_input("authority_path", &authority_path)?,
    );
    if let Some(value) = epoch_extensions_path {
        inputs.insert(
            "epoch_extensions_path".to_owned(),
            encode_input("epoch_extensions_path", &value)?,
        );
    }
    if let Some(value) = epoch_watermark_path {
        inputs.insert(
            "epoch_watermark_path".to_owned(),
            encode_input("epoch_watermark_path", &value)?,
        );
    }
    if let Some(value) = lease_path {
        inputs.insert("lease_path".to_owned(), encode_input("lease_path", &value)?);
    }
    inputs.insert(
        "ledger_path".to_owned(),
        encode_input("ledger_path", &ledger_path)?,
    );
    if let Some(value) = max {
        inputs.insert("max".to_owned(), encode_input("max", &value)?);
    }
    inputs.insert(
        "orders_path".to_owned(),
        encode_input("orders_path", &orders_path)?,
    );
    crate::custom_handlers::system_execute(inputs)
}

#[doc = "Select the ready frontier via ggen sjira frontier."]
#[doc = ""]
#[doc = "# Arguments"]
/// * `authority_path` - Generated argument metadata.
/// * `events_path` - Generated argument metadata.
/// * `evidence_path` - Generated argument metadata.
/// * `orders_path` - Generated argument metadata.
#[verb("frontier", "system")]
fn system_frontier(
    #[arg(help = "Optional authority IRI → digest index JSON.")] authority_path: Option<
        std::path::PathBuf,
    >,
    #[arg(help = "Optional transition-event log JSON.")] events_path: Option<std::path::PathBuf>,
    #[arg(help = "Optional evidence map JSON.")] evidence_path: Option<std::path::PathBuf>,
    #[arg(help = "JSON array of frontier order entries.")] orders_path: std::path::PathBuf,
) -> Result<Value> {
    let mut inputs = Map::new();
    if let Some(value) = authority_path {
        inputs.insert(
            "authority_path".to_owned(),
            encode_input("authority_path", &value)?,
        );
    }
    if let Some(value) = events_path {
        inputs.insert(
            "events_path".to_owned(),
            encode_input("events_path", &value)?,
        );
    }
    if let Some(value) = evidence_path {
        inputs.insert(
            "evidence_path".to_owned(),
            encode_input("evidence_path", &value)?,
        );
    }
    inputs.insert(
        "orders_path".to_owned(),
        encode_input("orders_path", &orders_path)?,
    );
    crate::custom_handlers::system_frontier(inputs)
}

#[doc = "Return a deterministic liveness value."]
#[verb("ping", "system")]
fn system_ping() -> Result<Value> {
    let inputs = Map::new();
    generated_behavior::execute(
        BehaviorSpec::StaticJson {
            json: "{\"status\":\"alive\",\"surface\":\"sjira-hands-free\"}",
        },
        inputs,
    )
}

#[linkme::distributed_slice(::clap_noun_verb::cli::registry::__NOUN_REGISTRY)]
static REGISTER_ISSUE_NOUN: fn() = register_issue_noun;

fn register_issue_noun() {
    ::clap_noun_verb::cli::registry::CommandRegistry::register_noun(
        "issue",
        "Admit a WorkOrder through ggen sjira.",
    );
}

#[linkme::distributed_slice(::clap_noun_verb::cli::registry::__NOUN_REGISTRY)]
static REGISTER_SYSTEM_NOUN: fn() = register_system_noun;

fn register_system_noun() {
    ::clap_noun_verb::cli::registry::CommandRegistry::register_noun(
        "system",
        "Inspect the wrapper and drive sjira frontier/execute.",
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
        assert_eq!(routes_only().matches("#[verb(").count(), 4);
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
            routes_only().contains("custom_handlers::issue_admit("),
            "issue admit must route to its own custom handler"
        );
        assert!(
            routes_only().contains("custom_handlers::system_execute("),
            "system execute must route to its own custom handler"
        );
        assert!(
            routes_only().contains("custom_handlers::system_frontier("),
            "system frontier must route to its own custom handler"
        );
    }
}
