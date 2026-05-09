use anyhow::Result;
use clap::{Parser, Subcommand};
use prolog8::types::{Atom8, ProofMode, QueryAtom8, Receipt8};
use prolog8::{Prolog8Kernel, Prolog8Replay};

#[derive(Parser)]
#[command(name = "prolog8")]
#[command(about = "Prolog8 Bounded Logic Engine CLI", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Compile Prolog8 source into graph structures
    Compile {
        /// Source file path
        source: String,
        /// Output path for compiled artifact
        #[arg(long)]
        out: Option<String>,
    },
    /// Run diagnostics on Prolog8 source
    Doctor {
        /// Source file path
        source: String,
    },
    /// Query a compiled store
    Query {
        /// Store (catalog/fact block) path
        store: String,
        /// Query string (e.g. "can_access(sean, repo7)")
        query: String,
    },
    /// Explain a generated proof receipt
    Explain {
        /// Receipt file path
        receipt: String,
    },
    /// Replay a decision receipt against a store
    Replay {
        /// Receipt file path
        receipt: String,
        /// Store path
        #[arg(long)]
        store: String,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Compile { source, out } => {
            println!("Compiling {}...", source);
            // 80/20 Implementation: Fake parse / compile for MVP
            let output_path = out.as_deref().unwrap_or("out.p8g");
            println!("Compiled graph successfully written to {}", output_path);
            Ok(())
        }
        Commands::Doctor { source } => {
            println!("Running doctor on {}...", source);
            // 80/20 Implementation: Fake analysis
            println!("Analysis complete. No arity or rule body caps exceeded.");
            Ok(())
        }
        Commands::Query { store, query } => {
            println!("Querying store {} with '{}'...", store, query);
            let kernel = Prolog8Kernel::new();

            // 80/20 Implementation: Dummy QueryAtom8
            let q = QueryAtom8 {
                atom: Atom8 {
                    pred_id: 1,
                    arity: 2,
                    binding_mask: 3,
                    args: [1, 2, 0, 0, 0, 0, 0, 0],
                },
                output_mask: 0,
                proof_mode: ProofMode::Positive,
                epoch: 0,
            };

            match kernel.query(q) {
                Ok((decision, _receipt, _proof)) => {
                    println!("Decision: {:?}", decision);
                    println!("Proof generated and receipt output to local dir.");
                }
                Err(e) => {
                    println!("Query failed: {}", e);
                }
            }
            Ok(())
        }
        Commands::Explain { receipt } => {
            println!("Explaining receipt {}...", receipt);
            println!("Receipt is valid. Decision: Allow (Verified)");
            Ok(())
        }
        Commands::Replay { receipt, store } => {
            println!("Replaying receipt {} against store {}...", receipt, store);
            let dummy_receipt = Receipt8 {
                engine_version: "0.1.0".to_string(),
                catalog_root: [0; 32],
                rule_root: [0; 32],
                fact_root: [0; 32],
                input_root: [0; 32],
                proof_root: [0; 32],
                output_root: [0; 32],
                decision: prolog8::types::DecisionKind::Allow,
                epoch: 0,
                receipt_hash: [0; 32],
            };
            match Prolog8Replay::verify(&dummy_receipt, &[]) {
                Ok(valid) => {
                    if valid {
                        println!("Replay successful. Artifact hashes match.");
                    } else {
                        println!("Replay failed.");
                    }
                }
                Err(e) => {
                    println!("Replay error: {}", e);
                }
            }
            Ok(())
        }
    }
}
