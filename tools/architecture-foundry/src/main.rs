use clap::{Parser, Subcommand};
use ggen_architecture_foundry::{
    admit_solution, admit_workstream, create_baseline, extract_components, initialize_corpus,
    load_final_evidence, load_migration_manifest, load_program, load_workstream_report,
    replay_all_receipts, validate_program, verify_corpus,
};
use serde::Serialize;
use std::path::PathBuf;

#[derive(Debug, Parser)]
#[command(
    name = "ggen-foundry",
    version,
    about = "Executable control plane for Enterprise Architecture Reconstitution and Manufacture"
)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    ValidateProgram {
        #[arg(long)]
        program: PathBuf,
    },
    Baseline {
        #[arg(long)]
        program: PathBuf,
        #[arg(long)]
        source: PathBuf,
        #[arg(long)]
        corpus: PathBuf,
        #[arg(long)]
        out: PathBuf,
    },
    InitializeCorpus {
        #[arg(long)]
        program: PathBuf,
        #[arg(long)]
        source: PathBuf,
        #[arg(long)]
        corpus: PathBuf,
    },
    Extract {
        #[arg(long)]
        program: PathBuf,
        #[arg(long)]
        source: PathBuf,
        #[arg(long)]
        corpus: PathBuf,
        #[arg(long)]
        migration: PathBuf,
    },
    AdmitWorkstream {
        #[arg(long)]
        program: PathBuf,
        #[arg(long)]
        source: PathBuf,
        #[arg(long)]
        corpus: PathBuf,
        #[arg(long)]
        report: PathBuf,
    },
    AdmitSolution {
        #[arg(long)]
        program: PathBuf,
        #[arg(long)]
        source: PathBuf,
        #[arg(long)]
        corpus: PathBuf,
        #[arg(long)]
        evidence: PathBuf,
    },
    Verify {
        #[arg(long)]
        program: PathBuf,
        #[arg(long)]
        source: PathBuf,
        #[arg(long)]
        corpus: PathBuf,
    },
    Replay {
        #[arg(long)]
        source: PathBuf,
        #[arg(long)]
        corpus: PathBuf,
    },
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Command::ValidateProgram { program } => {
            let program = load_program(&program)?;
            print_json(&validate_program(&program)?)?;
        }
        Command::Baseline {
            program,
            source,
            corpus,
            out,
        } => {
            let program = load_program(&program)?;
            print_json(&create_baseline(&program, &source, &corpus, &out)?)?;
        }
        Command::InitializeCorpus {
            program,
            source,
            corpus,
        } => {
            let program = load_program(&program)?;
            print_json(&initialize_corpus(&program, &source, &corpus)?)?;
        }
        Command::Extract {
            program,
            source,
            corpus,
            migration,
        } => {
            let program = load_program(&program)?;
            let migration = load_migration_manifest(&migration)?;
            print_json(&extract_components(&program, &source, &corpus, &migration)?)?;
        }
        Command::AdmitWorkstream {
            program,
            source,
            corpus,
            report,
        } => {
            let program = load_program(&program)?;
            let report = load_workstream_report(&report)?;
            print_json(&admit_workstream(&program, &source, &corpus, &report)?)?;
        }
        Command::AdmitSolution {
            program,
            source,
            corpus,
            evidence,
        } => {
            let program = load_program(&program)?;
            let evidence = load_final_evidence(&evidence)?;
            print_json(&admit_solution(&program, &source, &corpus, &evidence)?)?;
        }
        Command::Verify {
            program,
            source,
            corpus,
        } => {
            let program = load_program(&program)?;
            print_json(&verify_corpus(&program, &source, &corpus)?)?;
        }
        Command::Replay { source, corpus } => {
            #[derive(Serialize)]
            struct ReplayReport {
                receipts_checked: usize,
                replay_match: bool,
            }
            let receipts_checked = replay_all_receipts(&source, &corpus)?;
            print_json(&ReplayReport {
                receipts_checked,
                replay_match: true,
            })?;
        }
    }
    Ok(())
}

fn print_json<T: Serialize>(value: &T) -> anyhow::Result<()> {
    println!("{}", serde_json::to_string_pretty(value)?);
    Ok(())
}
