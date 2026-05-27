use clap::{Parser, Subcommand};
use std::path::PathBuf;
use anyhow::Result;
use cpmp::{scanner, receipt};

#[derive(Parser)]
#[command(name = "cpmp", about = "Computer Project Mapping Protocol (Open Ontologies Catalog)")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Scan {
        paths: Vec<PathBuf>,
        #[arg(long, default_value = ".cpmp")]
        out: PathBuf,
    },
    VerifyNoDeletion {
        #[arg(long)]
        before: PathBuf,
        #[arg(long)]
        after: PathBuf,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Commands::Scan { paths, out } => {
            scanner::scan(&paths, &out)?;
        }
        Commands::VerifyNoDeletion { before, after } => {
            receipt::verify_no_deletion(&before, &after)?;
        }
    }
    Ok(())
}
