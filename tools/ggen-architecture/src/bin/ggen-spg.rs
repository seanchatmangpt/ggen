//! CLI for Semantic Procedural Graph validation, diff, and projection.

use clap::{Parser, Subcommand};
use ggen_architecture::{
    compile_projection, spg_from_json, spg_semantic_diff, validate_spg, SpgGraph,
};
use std::{fs, path::PathBuf, process::ExitCode};

#[derive(Debug, Parser)]
#[command(name = "ggen-spg")]
#[command(about = "Validate, diff, and compile Semantic Procedural Graphs")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Validate structural SPG law.
    Validate {
        /// SPG JSON file.
        graph: PathBuf,
    },
    /// Compute a semantic graph diff.
    Diff {
        /// Old SPG JSON file.
        old: PathBuf,
        /// New SPG JSON file.
        new: PathBuf,
    },
    /// Compile one declared projection family.
    Compile {
        /// SPG JSON file.
        graph: PathBuf,
        /// Projection family, e.g. hddl, tla_plus, ocel2, sa2a, brce.
        #[arg(long)]
        family: String,
    },
}

fn read_graph(path: &PathBuf) -> Result<SpgGraph, String> {
    let bytes = fs::read(path).map_err(|error| format!("REFUSED:SPG_READ:{path:?}:{error}"))?;
    spg_from_json(&bytes).map_err(|error| error.to_string())
}

fn emit<T: serde::Serialize>(value: &T) -> Result<(), String> {
    let rendered = serde_json::to_string_pretty(value)
        .map_err(|error| format!("REFUSED:SPG_RENDER:{error}"))?;
    println!("{rendered}");
    Ok(())
}

fn run(cli: Cli) -> Result<(), String> {
    match cli.command {
        Command::Validate { graph } => {
            let graph = read_graph(&graph)?;
            validate_spg(&graph).map_err(|error| error.to_string())?;
            emit(&serde_json::json!({
                "schema": "chatman.spg-validation.v1",
                "graph": graph.id,
                "version": graph.version,
                "state": "ADMITTED_STRUCTURE",
                "standing": "NONE"
            }))
        }
        Command::Diff { old, new } => {
            let old = read_graph(&old)?;
            let new = read_graph(&new)?;
            validate_spg(&old).map_err(|error| error.to_string())?;
            validate_spg(&new).map_err(|error| error.to_string())?;
            emit(&spg_semantic_diff(&old, &new))
        }
        Command::Compile { graph, family } => {
            let graph = read_graph(&graph)?;
            let projection =
                compile_projection(&graph, &family).map_err(|error| error.to_string())?;
            emit(&projection)
        }
    }
}

fn main() -> ExitCode {
    match run(Cli::parse()) {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            eprintln!("{error}");
            ExitCode::from(2)
        }
    }
}
