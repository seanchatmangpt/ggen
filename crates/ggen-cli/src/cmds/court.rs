//! Court Commands
//!
//! The vacuity-court compiler CLI surface (`ggen court`):
//!   - `ggen court compile --catalog <path> --out <dir>` — compile a `vc:`
//!     mutant catalog into the shim-court artifacts (runner, register row,
//!     fixture, shim patch) with typed exits.
//!
//! Typed exits (fail-closed contract, `ggen-engine::court`):
//!   - 0 = PASS (flip authorized by a witness at/after the shim)
//!   - 1 = VACUOUS GATE (witness stale — flip refused)
//!   - 2 = COURT_BLOCKED (malformed catalog / I/O / template divergence)
//!   - 3 = loud SKIP (flip unverifiable — never silent)

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use ggen_engine::court::{compile_court_from_catalog_file, CourtError};
use serde_json::json;

fn loud_exit(code: i32, message: &str) -> ! {
    eprintln!("{message}");
    std::process::exit(code);
}

fn court_error_exit(err: &CourtError) -> ! {
    loud_exit(2, &format!("{err}"))
}

/// Compile the court artifacts for one mutant catalog.
///
/// # Errors
/// Returns a verb error only for argument-shape problems; catalog, I/O and
/// template failures surface as typed exit 2 (COURT_BLOCKED) on stderr,
/// witness-ordering refusals as exit 1 (VACUOUS GATE), unverifiable flips
/// as exit 3 (loud SKIP).
#[verb]
/// Arguments: `catalog` — path to the `vc:` mutant-catalog Turtle file;
/// `out` — output directory for the compiled court artifacts.
pub fn compile(catalog: String, out: String) -> Result<serde_json::Value> {
    let manifest = match compile_court_from_catalog_file(
        std::path::Path::new(&catalog),
        std::path::Path::new(&out),
    ) {
        Ok(manifest) => manifest,
        Err(err) => court_error_exit(&err),
    };

    let artifacts: Vec<serde_json::Value> = manifest
        .artifacts
        .iter()
        .map(|(name, sha)| json!({ "file": name, "sha256": sha }))
        .collect();

    match &manifest.verdict {
        ggen_engine::court::CourtVerdict::Pass {
            witness_ts,
            shim_ts,
        } => Ok(json!({
            "status": "PASS",
            "gate_id": manifest.catalog_gate_id,
            "out_dir": out,
            "witness_ts": witness_ts,
            "shim_ts": shim_ts,
            "artifacts": artifacts,
        })),
        ggen_engine::court::CourtVerdict::Refused { code, detail, .. } => loud_exit(
            manifest.verdict.exit_code(),
            &format!(
                "VACUOUS GATE ({code}): {detail} — artifacts written to {out} with the \
                 refusal compiled in; the flip is refused"
            ),
        ),
        ggen_engine::court::CourtVerdict::Unknown { reason } => loud_exit(
            manifest.verdict.exit_code(),
            &format!("SKIP (loud): {reason} — artifacts written to {out}"),
        ),
    }
}
