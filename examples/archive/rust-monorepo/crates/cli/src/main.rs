//! CLI application demonstrating lifecycle integration

use anyhow::Result;
use clap::Parser;
use example_core::{Config, Processor};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Input to process
    #[arg(short, long)]
    input: String,

    /// Enable verbose logging
    #[arg(short, long)]
    verbose: bool,

    /// Disable processing
    #[arg(short, long)]
    disable: bool,
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();

    // Initialize logging
    let log_level = if args.verbose {
        tracing::Level::DEBUG
    } else {
        tracing::Level::INFO
    };

    tracing_subscriber::registry()
        .with(
            tracing_subscriber::fmt::layer()
                .with_target(false)
                .with_level(true),
        )
        .with(tracing_subscriber::filter::LevelFilter::from_level(
            log_level,
        ))
        .init();

    tracing::info!("Starting example CLI application");

    let mut config = Config::default();
    config.enabled = !args.disable;

    let processor = Processor::new(config);
    let result = processor.process(&args.input)?;

    println!("Result: {}", result);

    tracing::info!("Application completed successfully");
    Ok(())
}
