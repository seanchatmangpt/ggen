//! Advanced CLI tool with full lifecycle support

use anyhow::Result;
use clap::{Parser, Subcommand};
use tracing_subscriber::EnvFilter;

mod processor;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Enable debug logging
    #[arg(short, long, global = true)]
    debug: bool,
}

#[derive(Subcommand)]
enum Commands {
    /// Process a file
    Process {
        /// Input file path
        #[arg(short, long)]
        input: String,

        /// Output file path
        #[arg(short, long)]
        output: Option<String>,
    },

    /// Analyze input
    Analyze {
        /// Input to analyze
        input: String,
    },

    /// Benchmark performance
    Bench {
        /// Number of iterations
        #[arg(short, long, default_value = "1000")]
        iterations: usize,
    },
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    // Initialize tracing
    let filter = if cli.debug {
        EnvFilter::new("debug")
    } else {
        EnvFilter::new("info")
    };

    tracing_subscriber::fmt()
        .with_env_filter(filter)
        .with_target(false)
        .init();

    tracing::info!("Advanced CLI starting");

    match cli.command {
        Commands::Process { input, output } => {
            processor::process_file(&input, output.as_deref()).await?;
        }
        Commands::Analyze { input } => {
            let stats = processor::analyze(&input)?;
            println!("{}", serde_json::to_string_pretty(&stats)?);
        }
        Commands::Bench { iterations } => {
            processor::benchmark(iterations).await?;
        }
    }

    tracing::info!("Execution completed successfully");
    Ok(())
}
