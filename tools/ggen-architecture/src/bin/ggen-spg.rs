//! CLI for Semantic Procedural Graph validation, diff, and projection.

use clap::{Parser, Subcommand};
use ggen_architecture::{
    apply_spg_rewrite, compile_projection, plan_spg_rewrite, replay_spg_rewrite,
    spg_from_json, spg_graph_digest, spg_semantic_diff, validate_spg, SpgExactSubject,
    SpgGraph, SpgRewritePlan,
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
    /// Manufacture an exact-subject deterministic rewrite plan.
    RewritePlan {
        /// Exact source SPG JSON file.
        old: PathBuf,
        /// Candidate target SPG JSON file.
        new: PathBuf,
        /// Repository identity in owner/name form.
        #[arg(long)]
        repository: String,
        /// Immutable lowercase 40-hex Git source commit.
        #[arg(long)]
        commit: String,
    },
    /// Apply a previously manufactured rewrite plan.
    Apply {
        /// Exact source SPG JSON file.
        graph: PathBuf,
        /// Rewrite-plan JSON file.
        plan: PathBuf,
    },
    /// Replay a rewrite twice and emit target plus replay receipt.
    Replay {
        /// Exact source SPG JSON file.
        graph: PathBuf,
        /// Rewrite-plan JSON file.
        plan: PathBuf,
    },
}

fn read_graph(path: &PathBuf) -> Result<SpgGraph, String> {
    let bytes = fs::read(path).map_err(|error| format!("REFUSED:SPG_READ:{path:?}:{error}"))?;
    spg_from_json(&bytes).map_err(|error| error.to_string())
}

fn read_plan(path: &PathBuf) -> Result<SpgRewritePlan, String> {
    let bytes = fs::read(path).map_err(|error| format!("REFUSED:SPG_PLAN_READ:{path:?}:{error}"))?;
    serde_json::from_slice(&bytes).map_err(|error| format!("REFUSED:SPG_PLAN_JSON:{error}"))
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
        Command::RewritePlan {
            old,
            new,
            repository,
            commit,
        } => {
            let old = read_graph(&old)?;
            let new = read_graph(&new)?;
            let subject = SpgExactSubject {
                repository,
                commit,
                graph_digest: spg_graph_digest(&old).map_err(|error| error.to_string())?,
            };
            let plan =
                plan_spg_rewrite(&old, &new, subject).map_err(|error| error.to_string())?;
            emit(&plan)
        }
        Command::Apply { graph, plan } => {
            let graph = read_graph(&graph)?;
            let plan = read_plan(&plan)?;
            let target =
                apply_spg_rewrite(&graph, &plan).map_err(|error| error.to_string())?;
            emit(&target)
        }
        Command::Replay { graph, plan } => {
            let graph = read_graph(&graph)?;
            let plan = read_plan(&plan)?;
            let (target, receipt) =
                replay_spg_rewrite(&graph, &plan).map_err(|error| error.to_string())?;
            emit(&serde_json::json!({
                "schema": "chatman.spg-rewrite-replay-envelope.v1",
                "target": target,
                "receipt": receipt
            }))
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
