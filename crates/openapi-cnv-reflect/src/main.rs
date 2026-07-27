//! `openapi-cnv-reflect <openapi.json> <output-dir>` -- writes
//! `<output-dir>/ontology.ttl` reflected from the given OpenAPI 3.x document,
//! and reports (to stderr) every operation it had to skip and why.
//!
//! Deliberately not wired into the shipped `ggen` binary: this tool's only
//! contract with the rest of the system is "produces a valid `cnv:Cli`
//! ontology.ttl," consumed unchanged by the existing zero-code pipeline.

use std::path::PathBuf;
use std::process::ExitCode;

use clap::Parser;

/// Reflect an OpenAPI 3.x document into a cnv:Cli ontology.ttl.
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Path to the OpenAPI 3.x JSON document.
    openapi_json: PathBuf,
    /// Directory to write ontology.ttl into (created if missing).
    output_dir: PathBuf,
}

fn main() -> ExitCode {
    let args = Args::parse();

    let raw = match std::fs::read_to_string(&args.openapi_json) {
        Ok(raw) => raw,
        Err(error) => {
            eprintln!(
                "ERROR: failed to read {}: {error}",
                args.openapi_json.display()
            );
            return ExitCode::FAILURE;
        }
    };
    let spec: serde_json::Value = match serde_json::from_str(&raw) {
        Ok(spec) => spec,
        Err(error) => {
            eprintln!(
                "ERROR: {} is not valid JSON: {error}",
                args.openapi_json.display()
            );
            return ExitCode::FAILURE;
        }
    };

    let outcome = match openapi_cnv_reflect::reflect(&spec) {
        Ok(outcome) => outcome,
        Err(error) => {
            eprintln!("ERROR: {error}");
            return ExitCode::FAILURE;
        }
    };

    for warning in &outcome.warnings {
        eprintln!(
            "SKIPPED {} {}: {}",
            warning.method.to_uppercase(),
            warning.path,
            warning.reason
        );
    }

    let output_path = args.output_dir.join("ontology.ttl");
    if let Err(error) = openapi_cnv_reflect::write_ontology(&outcome.store, &output_path) {
        eprintln!("ERROR: {error}");
        return ExitCode::FAILURE;
    }

    eprintln!(
        "wrote {} ({} operation(s) skipped, see above)",
        output_path.display(),
        outcome.warnings.len()
    );
    ExitCode::SUCCESS
}
