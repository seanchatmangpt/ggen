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
#[doc = "Admit one WorkOrder JSON file through the ggen-sjira kernel."]
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

#[doc = "Run the kernel frontier→authority→plan→receipt→reconcile loop over a real ledger."]
#[doc = ""]
#[doc = "# Arguments"]
/// * `authority_path` - Generated argument metadata.
/// * `epoch_extensions_path` - Generated argument metadata.
/// * `epoch_watermark_path` - Generated argument metadata.
/// * `lease_path` - Generated argument metadata.
/// * `ledger_path` - Generated argument metadata.
/// * `max` - Generated argument metadata.
/// * `orders_path` - Generated argument metadata.
#[verb("execute", "project")]
fn project_execute(
    #[arg(help = "Authority graph JSON.")] authority_path: std::path::PathBuf,
    #[arg(help = "Optional epoch extensions JSON.")] epoch_extensions_path: Option<
        std::path::PathBuf,
    >,
    #[arg(help = "Optional epoch watermark JSON.")] epoch_watermark_path: Option<
        std::path::PathBuf,
    >,
    #[arg(help = "Optional admitted-lease JSON.")] lease_path: Option<std::path::PathBuf>,
    #[arg(help = "File-backed ledger directory path.")] ledger_path: std::path::PathBuf,
    #[arg(help = "Max execute-loop iterations (kernel default 64).")] max: Option<u64>,
    #[arg(help = "JSON array of frontier inputs.")] orders_path: std::path::PathBuf,
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
    crate::custom_handlers::project_execute(inputs)
}

#[doc = "Select the eligible frontier over real orders, events, evidence, and authority files."]
#[doc = ""]
#[doc = "# Arguments"]
/// * `authority_path` - Generated argument metadata.
/// * `events_path` - Generated argument metadata.
/// * `evidence_path` - Generated argument metadata.
/// * `orders_path` - Generated argument metadata.
#[verb("frontier", "project")]
fn project_frontier(
    #[arg(help = "Optional authority index JSON; absent ⇒ kernel fail-closed blocks.")]
    authority_path: Option<std::path::PathBuf>,
    #[arg(help = "Optional transition-event journal JSON.")] events_path: Option<
        std::path::PathBuf,
    >,
    #[arg(help = "Optional evidence index JSON.")] evidence_path: Option<std::path::PathBuf>,
    #[arg(help = "JSON array of frontier inputs (work_order + optional dependencies).")]
    orders_path: std::path::PathBuf,
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
    crate::custom_handlers::project_frontier(inputs)
}

#[doc = "Return typed command inputs as a JSON object."]
#[doc = ""]
#[doc = "# Arguments"]
/// * `uppercase` - Generated argument metadata.
/// * `message` - Generated argument metadata.
#[verb("echo", "system")]
fn system_echo(
    #[arg(
        help = "Include an uppercase-request flag in the returned input object.",
        short = 'u'
    )]
    uppercase: bool,
    #[arg(help = "Message to return.", index = 1)] message: String,
) -> Result<Value> {
    let mut inputs = Map::new();
    inputs.insert(
        "uppercase".to_owned(),
        encode_input("uppercase", &uppercase)?,
    );
    inputs.insert("message".to_owned(), encode_input("message", &message)?);
    generated_behavior::execute(BehaviorSpec::Echo, inputs)
}

#[doc = "Return a deterministic ontology-owned liveness value."]
#[verb("ping", "system")]
fn system_ping() -> Result<Value> {
    let inputs = Map::new();
    generated_behavior::execute(
        BehaviorSpec::StaticJson { json: "{\"status\":\"alive\",\"binary\":\"sjira\",\"compiler\":\"clap-noun-verb-six-pack\"}" },
        inputs,
    )
}

#[doc = "Exercise a typed ontology-declared refusal path."]
#[verb("refuse", "system")]
fn system_refuse() -> Result<Value> {
    let inputs = Map::new();
    generated_behavior::execute(
        BehaviorSpec::Refusal {
            code: "SJIRA_DEMONSTRATION_REFUSAL",
            message: "This command is intentionally refused by the admitted sjira graph.",
        },
        inputs,
    )
}

#[linkme::distributed_slice(::clap_noun_verb::cli::registry::__NOUN_REGISTRY)]
static REGISTER_ISSUE_NOUN: fn() = register_issue_noun;

fn register_issue_noun() {
    ::clap_noun_verb::cli::registry::CommandRegistry::register_noun(
        "issue",
        "Admit a single work-order issue.",
    );
}

#[linkme::distributed_slice(::clap_noun_verb::cli::registry::__NOUN_REGISTRY)]
static REGISTER_PROJECT_NOUN: fn() = register_project_noun;

fn register_project_noun() {
    ::clap_noun_verb::cli::registry::CommandRegistry::register_noun(
        "project",
        "Project-scoped frontier selection and execute loop.",
    );
}

#[linkme::distributed_slice(::clap_noun_verb::cli::registry::__NOUN_REGISTRY)]
static REGISTER_SYSTEM_NOUN: fn() = register_system_noun;

fn register_system_noun() {
    ::clap_noun_verb::cli::registry::CommandRegistry::register_noun(
        "system",
        "Liveness, echo, and explicit refusal paths.",
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
        assert_eq!(routes_only().matches("#[verb(").count(), 6);
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
            routes_only().contains("custom_handlers::project_execute("),
            "project execute must route to its own custom handler"
        );
        assert!(
            routes_only().contains("custom_handlers::project_frontier("),
            "project frontier must route to its own custom handler"
        );
    }
}
