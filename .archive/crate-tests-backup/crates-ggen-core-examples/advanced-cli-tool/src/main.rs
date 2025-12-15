//! Advanced CLI Tool - Production-ready example with ggen lifecycle
//!
//! Demonstrates:
//! - Async I/O with tokio
//! - Multiple subcommands with clap
//! - Structured logging with tracing
//! - Configuration management
//! - Progress bars and interactive UI
//! - Error handling with anyhow

use anyhow::Result;
use clap::{Parser, Subcommand};
use console::style;
use tracing::{info, Level};
use tracing_subscriber::{fmt, prelude::*, EnvFilter};

mod config;
mod processor;

use config::Config;
use processor::{FileProcessor, ProcessingOptions};

#[derive(Parser)]
#[command(name = "advanced-cli-tool")]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Enable verbose logging
    #[arg(short, long, global = true)]
    verbose: bool,

    /// Configuration file path
    #[arg(short, long, default_value = "config.toml")]
    config: String,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Process files with various transformations
    Process {
        /// Input file or directory path
        #[arg(short, long)]
        input: String,

        /// Output directory path
        #[arg(short, long)]
        output: String,

        /// Number of parallel workers
        #[arg(short, long, default_value = "4")]
        workers: usize,

        /// Enable compression
        #[arg(long)]
        compress: bool,
    },

    /// Analyze files and generate statistics
    Analyze {
        /// Path to analyze
        path: String,

        /// Show detailed statistics
        #[arg(short, long)]
        detailed: bool,

        /// Output format (text, json)
        #[arg(short, long, default_value = "text")]
        format: String,
    },

    /// Convert files between formats
    Convert {
        /// Input file path
        input: String,

        /// Output file path
        output: String,

        /// Target format
        #[arg(short, long)]
        format: String,
    },

    /// Run performance benchmarks
    Benchmark {
        /// Number of iterations
        #[arg(short, long, default_value = "100")]
        iterations: usize,

        /// File size in MB
        #[arg(short, long, default_value = "10")]
        size: usize,
    },
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    // Initialize logging
    setup_logging(cli.verbose)?;

    info!("Starting advanced-cli-tool v{}", env!("CARGO_PKG_VERSION"));

    // Load configuration
    let config = Config::load(&cli.config)?;
    info!("Loaded configuration from {}", cli.config);

    // Execute command
    match cli.command {
        Commands::Process {
            input,
            output,
            workers,
            compress,
        } => {
            println!(
                "{} Processing files...",
                style("►").green().bold()
            );

            let options = ProcessingOptions {
                workers,
                compress,
                buffer_size: config.buffer_size,
            };

            let processor = FileProcessor::new(options);
            let stats = processor.process_directory(&input, &output).await?;

            println!(
                "{} Processed {} files in {:.2}s",
                style("✓").green().bold(),
                stats.files_processed,
                stats.elapsed_secs
            );
            println!(
                "  {} bytes processed, {} files/sec",
                stats.bytes_processed,
                stats.throughput()
            );
        }

        Commands::Analyze { path, detailed, format } => {
            println!("{} Analyzing {}...", style("►").cyan().bold(), path);

            let processor = FileProcessor::default();
            let analysis = processor.analyze_path(&path).await?;

            match format.as_str() {
                "json" => {
                    println!("{}", serde_json::to_string_pretty(&analysis)?);
                }
                _ => {
                    println!("\n{}", style("Analysis Results:").bold().underlined());
                    println!("  Total files: {}", analysis.total_files);
                    println!("  Total size:  {} bytes", analysis.total_size);
                    println!("  File types:  {}", analysis.file_types.len());

                    if detailed {
                        println!("\n{}", style("File Type Distribution:").bold());
                        for (ext, count) in &analysis.file_types {
                            println!("  .{:<10} {}", ext, count);
                        }
                    }
                }
            }
        }

        Commands::Convert { input, output, format } => {
            println!(
                "{} Converting {} to {}...",
                style("►").yellow().bold(),
                input,
                format
            );

            let processor = FileProcessor::default();
            processor.convert_file(&input, &output, &format).await?;

            println!("{} Conversion complete!", style("✓").green().bold());
        }

        Commands::Benchmark { iterations, size } => {
            println!(
                "{} Running benchmark ({} iterations, {} MB)...",
                style("►").magenta().bold(),
                iterations,
                size
            );

            let processor = FileProcessor::default();
            let bench_result = processor.benchmark(iterations, size).await?;

            println!("\n{}", style("Benchmark Results:").bold().underlined());
            println!("  Iterations:    {}", bench_result.iterations);
            println!("  Total time:    {:.2}s", bench_result.total_secs);
            println!("  Avg time:      {:.4}s", bench_result.avg_secs);
            println!("  Throughput:    {:.2} MB/s", bench_result.throughput_mbps);
        }
    }

    info!("Operation completed successfully");
    Ok(())
}

/// Setup logging with tracing
fn setup_logging(verbose: bool) -> Result<()> {
    let level = if verbose { Level::DEBUG } else { Level::INFO };

    tracing_subscriber::registry()
        .with(fmt::layer().with_target(false))
        .with(
            EnvFilter::from_default_env()
                .add_directive(level.into())
        )
        .init();

    Ok(())
}
