//! `ggen ea` — first-class CLI surface for the ggen-abb-sbb admission kernel
//! (WO-02, docs/jira/v26.11.3/WO-02-cli-abb-verbs.md).
//!
//! Thin consumer surface: every verb loads an EA graph, builds a kernel
//! [`ggen_abb_sbb::Request`] from flags, calls one kernel function, and maps
//! the outcome onto the house exit contract:
//!
//! | Exit | Meaning |
//! |---|---|
//! | 0 | Admitted / verified |
//! | 1 | Kernel [`ggen_abb_sbb::Refusal`] (typed JSON on stderr) |
//! | 2 | Usage / input error (bad flag value, unreadable graph, malformed JSON) |
//!
//! All kernel logic stays in `ggen_abb_sbb`; this module only parses a graph +
//! Request and maps results. The admission kernel lives here; the older
//! capability-density `ggen sbb` noun is a different feature (Resolution R6)
//! and shares no code with this one.

pub mod admit;
pub mod manufacture;
pub mod plan;
pub mod replay;
pub mod verify_receipt;

use std::io::Write;
use std::process;

use clap_noun_verb::Result;
use serde_json::{json, Value};

type StdResult<T, E> = std::result::Result<T, E>;

/// Default graph path (`--graph`, default `./ea.graph.json`).
pub(crate) const DEFAULT_GRAPH_PATH: &str = "./ea.graph.json";
/// Default receipt path for `replay`/`verify-receipt` (`--receipt`).
pub(crate) const DEFAULT_RECEIPT_PATH: &str = "./receipt.json";
/// Default generator identity bound into receipts (`--generator-id`).
pub(crate) const DEFAULT_GENERATOR_ID: &str = "ggen-cli";
/// Default generator version (`--generator-version`).
pub(crate) const DEFAULT_GENERATOR_VERSION: &str = env!("CARGO_PKG_VERSION");

/// Exit code for a kernel refusal (deterministic admission rejection).
const EXIT_REFUSAL: i32 = 1;
/// Exit code for a usage / input error.
const EXIT_USAGE: i32 = 2;

/// A usage/input error inside an `ea` verb: print typed JSON on stderr and
/// exit 2. Distinct from a kernel [`ggen_abb_sbb::Refusal`] (kernel admission
/// rejection, exit 1).
pub(crate) struct UsageError {
    pub detail: String,
}

/// Exit-code mapping, kernel [`ggen_abb_sbb::Refusal`] variant -> exit code.
///
/// Total and explicit: every one of the 27 refusal variants is enumerated by
/// its serialized SCREAMING_SNAKE tag. MALFORMED_GRAPH is the malformed-JSON /
/// unreadable-shape class the WO-02 exit table pins to exit 2; everything else
/// maps to exit 1. The final arm is loud (never 0), so an unknown/future
/// variant cannot pass silently (WO-02 falsifier F2).
pub(crate) fn refusal_exit_code(tag: &str) -> i32 {
    match tag {
        "MALFORMED_GRAPH" => EXIT_USAGE,
        "DUPLICATE_ELEMENT" => EXIT_REFUSAL,
        "DANGLING_REFERENCE" => EXIT_REFUSAL,
        "GRAPH_DIGEST_MISMATCH" => EXIT_REFUSAL,
        "AUTHORITY_EXCEEDED" => EXIT_REFUSAL,
        "INSUFFICIENT_AUTHORITY" => EXIT_REFUSAL,
        "PACK_CONFLATION" => EXIT_REFUSAL,
        "UNKNOWN_ABB" => EXIT_REFUSAL,
        "UNKNOWN_SBB" => EXIT_REFUSAL,
        "SBB_ABB_MISMATCH" => EXIT_REFUSAL,
        "SBB_IDENTITY_UNKNOWN" => EXIT_REFUSAL,
        "SBB_MUTABLE" => EXIT_REFUSAL,
        "SBB_DIGEST_MISMATCH" => EXIT_REFUSAL,
        "SBB_UNQUALIFIED" => EXIT_REFUSAL,
        "STALE_QUALIFICATION" => EXIT_REFUSAL,
        "CONTRACT_CHANGED" => EXIT_REFUSAL,
        "EXCEEDS_CONTRACT" => EXIT_REFUSAL,
        "EXCEEDS_AUTHORITY_CEILING" => EXIT_REFUSAL,
        "UNSAFE_ARTIFACT_PATH" => EXIT_REFUSAL,
        "DUPLICATE_ARTIFACT_PATH" => EXIT_REFUSAL,
        "CYCLIC_PACK_DEPENDENCY" => EXIT_REFUSAL,
        "UNBOUND_PORT" => EXIT_REFUSAL,
        "UNBOUND_PLACEHOLDER" => EXIT_REFUSAL,
        "REFUSED_STALE_LEASE" => EXIT_REFUSAL,
        "RECEIPT_TAMPERED" => EXIT_REFUSAL,
        "REPLAY_MISMATCH" => EXIT_REFUSAL,
        _ => EXIT_REFUSAL,
    }
}

/// Print the typed refusal JSON `{refusal, detail}` on stderr and exit with
/// the variant's mapped code (1, or 2 for the malformed-input class).
pub(crate) fn refuse_and_exit(refusal: &ggen_abb_sbb::Refusal) -> ! {
    let value = serde_json::to_value(refusal).unwrap_or_else(|e| {
        json!({
            "refusal": "SERIALIZATION_FAILURE",
            "detail": e.to_string(),
        })
    });
    let tag = value
        .get("refusal")
        .and_then(|t| t.as_str())
        .unwrap_or("UNKNOWN_REFUSAL")
        .to_string();
    let payload = json!({
        "refusal": tag,
        "detail": refusal.to_string(),
    });
    eprintln!("{payload}");
    let code = refusal_exit_code(&tag);
    flush_and_exit(code);
}

/// Print a usage/input error as typed JSON on stderr and exit 2.
pub(crate) fn usage_error_and_exit(detail: impl std::fmt::Display) -> ! {
    eprintln!(
        "{}",
        json!({
            "refusal": "INVALID_INVOCATION",
            "detail": detail.to_string(),
        })
    );
    flush_and_exit(EXIT_USAGE);
}

fn flush_and_exit(code: i32) -> ! {
    let _ = std::io::stdout().flush();
    let _ = std::io::stderr().flush();
    process::exit(code);
}

/// Read and parse the EA graph at `path` (kernel [`ggen_abb_sbb::parse_graph`]).
///
/// Unreadable file -> [`usage_error_and_exit`] (exit 2); malformed JSON or
/// wrong schema -> kernel `MALFORMED_GRAPH` refusal path (exit 2 via the
/// total refusal mapping).
pub(crate) fn load_graph(path: &str) -> ggen_abb_sbb::EaGraph {
    let raw = match std::fs::read_to_string(path) {
        Ok(r) => r,
        Err(e) => usage_error_and_exit(format!("cannot read graph file {path}: {e}")),
    };
    match ggen_abb_sbb::parse_graph(&raw) {
        Ok(g) => g,
        // parse_graph only produces MALFORMED_GRAPH; still routed through the
        // total mapping so the exit code is derived, not hardcoded here.
        Err(r) => refuse_and_exit(&r),
    }
}

/// Parse an authority string (NONE/SELECT/CONSTRUCT/DO, case-insensitive).
/// Defaults to CONSTRUCT when the flag is absent.
pub(crate) fn parse_authority(raw: Option<&str>) -> ggen_abb_sbb::Authority {
    let raw = raw.unwrap_or("CONSTRUCT");
    match raw.to_ascii_uppercase().as_str() {
        "NONE" => ggen_abb_sbb::Authority::None,
        "SELECT" => ggen_abb_sbb::Authority::Select,
        "CONSTRUCT" => ggen_abb_sbb::Authority::Construct,
        "DO" => ggen_abb_sbb::Authority::Do,
        _ => usage_error_and_exit(format!(
            "invalid authority '{raw}' (expected NONE, SELECT, CONSTRUCT, or DO)"
        )),
    }
}

/// Parse a generation / baseline-generation flag value (absent = 0).
pub(crate) fn parse_generation(raw: Option<&str>, flag: &str) -> u64 {
    match raw {
        None => 0,
        Some(s) => match s.parse::<u64>() {
            Ok(v) => v,
            Err(e) => usage_error_and_exit(format!("invalid {flag} value '{s}': {e}")),
        },
    }
}

/// Shared success envelope for every `ea` verb.
pub(crate) fn ok_json(verb: &str, payload: Value) -> Result<Value> {
    Ok(json!({
        "ok": true,
        "verb": verb,
        "result": payload,
    }))
}
