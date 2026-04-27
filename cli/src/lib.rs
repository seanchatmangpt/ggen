//! ggen v2.0.0 CLI Library with clap-noun-verb Auto-Discovery
//!
//! This library provides the CLI interface for ggen using clap-noun-verb v3.0.0.
//! Commands are automatically discovered from the cmds/ module.
//!
//! ## Architecture
//! - **Auto-discovery**: Commands in cmds/ are automatically registered
//! - **Global flags**: Config, debug, logging, and OpenTelemetry flags
//! - **Error handling**: Proper Result propagation with telemetry shutdown
//! - **Node.js integration**: Programmatic API for Node addon via run_for_node()
//!
//! ## 80/20 Focus
//! Core functionality:
//! - Clap argument parsing with global flags
//! - Command routing via auto-discovered Commands enum
//! - OpenTelemetry integration when enabled
//! - Configuration merging (CLI args > config file > defaults)

use clap::{CommandFactory, Parser};
use std::io::{Read, Write};
use std::path::PathBuf;

use ggen_utils::app_config::AppConfig;
use ggen_utils::error::Result;
use ggen_utils::types::LogLevel;

// Auto-discovered command modules
pub mod cmds;
pub mod domain;  // Export domain layer for testing
pub mod commands; // v2 commands layer
pub mod runtime;  // Runtime utilities for async/sync bridging

/// Main CLI structure with global flags and auto-discovered commands.
///
/// ## Global Flags
/// - `--config`: Path to custom config file
/// - `--manifest-path`: Path to ggen.toml manifest
/// - `--debug`: Enable debug logging
/// - `--log-level`: Set log level (trace, debug, info, warn, error)
/// - `--enable-otel`: Enable OpenTelemetry tracing
/// - `--otel-endpoint`: OTLP endpoint (default: http://localhost:4317)
/// - `--otel-exporter`: Exporter type (default: otlp)
/// - `--otel-sample-ratio`: Sampling ratio (0.0-1.0, default: 1.0)
///
/// ## Command Discovery
/// Commands are automatically discovered from cmds/mod.rs and registered
/// by clap-noun-verb. No manual registration required.
#[derive(Parser, Debug)]
#[command(
    name = "ggen",
    author,
    about = "Graph-aware code generator with auto-discovered commands",
    version,
    long_about = "ggen is a deterministic, language-agnostic code generation framework \
                  that treats software artifacts as projections of knowledge graphs. \
                  \n\nCommands are automatically discovered from the cmds/ directory."
)]
pub struct Cli {
    /// Path to custom configuration file
    #[arg(short, long, value_name = "FILE")]
    pub config: Option<PathBuf>,

    /// Path to ggen.toml manifest file
    #[arg(long, value_name = "PATH", help = "Path to ggen.toml manifest file")]
    pub manifest_path: Option<PathBuf>,

    /// Enable debug logging
    #[arg(name = "debug", short, long = "debug", value_name = "DEBUG")]
    pub debug: Option<bool>,

    /// Set log level (trace, debug, info, warn, error)
    #[arg(
        name = "log_level",
        short,
        long = "log-level",
        value_name = "LOG_LEVEL"
    )]
    pub log_level: Option<LogLevel>,

    /// OpenTelemetry OTLP endpoint (default: http://localhost:4317)
    /// Can also be set via OTEL_EXPORTER_OTLP_ENDPOINT environment variable
    #[arg(long, value_name = "ENDPOINT")]
    pub otel_endpoint: Option<String>,

    /// OpenTelemetry exporter type (default: otlp)
    #[arg(long, value_name = "EXPORTER", default_value = "otlp")]
    pub otel_exporter: String,

    /// OpenTelemetry sample ratio (0.0 to 1.0, default: 1.0)
    #[arg(long, value_name = "RATIO", default_value = "1.0")]
    pub otel_sample_ratio: f64,

    /// Enable OpenTelemetry tracing
    #[arg(long)]
    pub enable_otel: bool,

    /// Auto-discovered subcommands from cmds/ module
    #[clap(subcommand)]
    pub command: cmds::Commands,
}

/// Build the CLI command structure for inspection or custom parsing.
///
/// This is primarily used for:
/// - Shell completion generation
/// - Help text generation
/// - Testing and introspection
pub fn build_cli() -> clap::Command {
    Cli::command()
}

/// Parse command-line arguments and execute the matched command.
///
/// ## Execution Flow
/// 1. Parse CLI arguments using clap
/// 2. Merge configuration from --config file if provided
/// 3. Merge CLI arguments into AppConfig
/// 4. Initialize OpenTelemetry if --enable-otel flag is set
/// 5. Execute the matched command via auto-discovered Commands enum
/// 6. Shutdown OpenTelemetry if it was initialized
///
/// ## Configuration Priority (highest to lowest)
/// 1. CLI arguments (--debug, --log-level, etc.)
/// 2. Config file (via --config)
/// 3. Embedded defaults (from resources/ggen_config.toml)
///
/// ## Error Handling
/// - OpenTelemetry initialization errors are non-fatal
/// - Command execution errors propagate up as Result::Err
/// - Telemetry is always shut down cleanly, even on error
///
/// ## Example
/// ```no_run
/// #[tokio::main]
/// async fn main() -> ggen_utils::error::Result<()> {
///     ggen_cli_lib::cli_match().await
/// }
/// ```
pub async fn cli_match() -> Result<()> {
    let cli = Cli::parse();

    // Initialize OpenTelemetry if enabled
    if cli.enable_otel {
        use ggen_core::telemetry::{init_telemetry, shutdown_telemetry, TelemetryConfig};

        let otel_config = TelemetryConfig {
            endpoint: cli
                .otel_endpoint
                .clone()
                .or_else(|| std::env::var("OTEL_EXPORTER_OTLP_ENDPOINT").ok())
                .unwrap_or_else(|| "http://localhost:4317".to_string()),
            service_name: "ggen-cli".to_string(),
            sample_ratio: cli.otel_sample_ratio,
            console_output: true,
        };

        init_telemetry(otel_config)?;

        // Ensure telemetry is shut down on exit
        let result = async {
            AppConfig::merge_config(cli.config.as_deref())?;
            let app = Cli::command();
            let matches = app.get_matches();
            AppConfig::merge_args(&matches)?;

            // For now, skip ggen.toml loading until we implement the methods
            cli.command.run().await
        }
        .await;

        shutdown_telemetry();
        result
    } else {
        AppConfig::merge_config(cli.config.as_deref())?;
        let app = Cli::command();
        let matches = app.get_matches();
        AppConfig::merge_args(&matches)?;

        // For now, skip ggen.toml loading until we implement the methods
        cli.command.run().await
    }
}

/// Structured result for programmatic CLI execution (used by Node addon).
///
/// Contains the exit code and captured stdout/stderr from CLI execution.
#[derive(Debug, Clone)]
pub struct RunResult {
    /// Exit code (0 = success, 1 = error)
    pub code: i32,
    /// Captured stdout output
    pub stdout: String,
    /// Captured stderr output
    pub stderr: String,
}

/// Programmatic entrypoint to execute the CLI with provided arguments and capture output.
///
/// This function is primarily used by the Node.js addon (node/) to execute ggen
/// commands within the same process, avoiding the overhead of spawning a new process.
///
/// ## Execution Flow
/// 1. Prefix args with "ggen" to satisfy clap's binary name requirement
/// 2. Create thread-safe buffers for stdout/stderr capture
/// 3. Spawn blocking task to avoid Send issues with gag buffers
/// 4. Capture stdout/stderr using gag::BufferRedirect
/// 5. Parse CLI arguments with Cli::try_parse_from
/// 6. Execute command with telemetry if enabled
/// 7. Return RunResult with exit code and captured output
///
/// ## Error Handling
/// - Parse errors are written to stderr and return code 1
/// - Command execution errors are written to stderr and return code 1
/// - Telemetry is always shut down cleanly
/// - If stdout/stderr capture fails, execution continues without capture
///
/// ## Example
/// ```no_run
/// use ggen_cli_lib::run_for_node;
///
/// #[tokio::main]
/// async fn main() {
///     let args = vec!["doctor".to_string()];
///     let result = run_for_node(args).await.unwrap();
///     println!("Exit code: {}", result.code);
///     println!("Output: {}", result.stdout);
/// }
/// ```
///
/// ## Note
/// This function avoids spawning a new process and preserves deterministic behavior.
pub async fn run_for_node(args: Vec<String>) -> Result<RunResult> {
    use std::sync::Arc;
    use std::sync::Mutex;

    // Prefix with a binary name to satisfy clap parse_from semantics
    let argv: Vec<String> = std::iter::once("ggen".to_string()).chain(args.into_iter()).collect();

    // Create thread-safe buffers for capturing output
    let stdout_buffer = Arc::new(Mutex::new(Vec::new()));
    let stderr_buffer = Arc::new(Mutex::new(Vec::new()));

    let stdout_clone = Arc::clone(&stdout_buffer);
    let stderr_clone = Arc::clone(&stderr_buffer);

    // Execute in a blocking task to avoid Send issues with gag
    let result = tokio::task::spawn_blocking(move || {
        // Capture stdout/stderr using gag buffers
        let mut captured_stdout = Vec::new();
        let mut captured_stderr = Vec::new();

        let code = match (gag::BufferRedirect::stdout(), gag::BufferRedirect::stderr()) {
            (Ok(mut so), Ok(mut se)) => {
                // Parse CLI with captured stdout/stderr
                let cli = match Cli::try_parse_from(&argv) {
                    Ok(c) => c,
                    Err(e) => {
                        let _ = writeln!(std::io::stderr(), "{}", e);
                        let _ = se.read_to_end(&mut captured_stderr);
                        *stderr_clone.lock().unwrap() = captured_stderr;
                        return 1;
                    }
                };

                // Execute the command
                let runtime = tokio::runtime::Runtime::new().unwrap();
                let code_val = runtime.block_on(async {
                    if cli.enable_otel {
                        use ggen_core::telemetry::{init_telemetry, shutdown_telemetry, TelemetryConfig};

                        let otel_config = TelemetryConfig {
                            endpoint: cli
                                .otel_endpoint
                                .clone()
                                .or_else(|| std::env::var("OTEL_EXPORTER_OTLP_ENDPOINT").ok())
                                .unwrap_or_else(|| "http://localhost:4317".to_string()),
                            service_name: "ggen-cli".to_string(),
                            sample_ratio: cli.otel_sample_ratio,
                            console_output: true,
                        };

                        if let Err(e) = init_telemetry(otel_config) {
                            let _ = writeln!(std::io::stderr(), "Failed to initialize telemetry: {}", e);
                            return 1;
                        }

                        let result = async {
                            AppConfig::merge_config(cli.config.as_deref())?;
                            let app = Cli::command();
                            let matches = app.get_matches();
                            AppConfig::merge_args(&matches)?;
                            cli.command.run().await
                        }
                        .await;

                        shutdown_telemetry();

                        match result {
                            Ok(()) => 0,
                            Err(err) => {
                                let _ = writeln!(std::io::stderr(), "{}", err);
                                1
                            }
                        }
                    } else {
                        match async {
                            AppConfig::merge_config(cli.config.as_deref())?;
                            let app = Cli::command();
                            let matches = app.get_matches();
                            AppConfig::merge_args(&matches)?;
                            cli.command.run().await
                        }
                        .await
                        {
                            Ok(()) => 0,
                            Err(err) => {
                                let _ = writeln!(std::io::stderr(), "{}", err);
                                1
                            }
                        }
                    }
                });

                let _ = so.read_to_end(&mut captured_stdout);
                let _ = se.read_to_end(&mut captured_stderr);

                *stdout_clone.lock().unwrap() = captured_stdout;
                *stderr_clone.lock().unwrap() = captured_stderr;

                code_val
            }
            _ => {
                // Fallback: execute without capture
                let cli = match Cli::try_parse_from(&argv) {
                    Ok(c) => c,
                    Err(e) => {
                        eprintln!("{}", e);
                        return 1;
                    }
                };

                let runtime = tokio::runtime::Runtime::new().unwrap();
                runtime.block_on(async {
                    if cli.enable_otel {
                        use ggen_core::telemetry::{init_telemetry, shutdown_telemetry, TelemetryConfig};

                        let otel_config = TelemetryConfig {
                            endpoint: cli
                                .otel_endpoint
                                .clone()
                                .or_else(|| std::env::var("OTEL_EXPORTER_OTLP_ENDPOINT").ok())
                                .unwrap_or_else(|| "http://localhost:4317".to_string()),
                            service_name: "ggen-cli".to_string(),
                            sample_ratio: cli.otel_sample_ratio,
                            console_output: true,
                        };

                        let _ = init_telemetry(otel_config);
                        let result = cli.command.run().await;
                        shutdown_telemetry();

                        match result {
                            Ok(()) => 0,
                            Err(err) => {
                                eprintln!("{}", err);
                                1
                            }
                        }
                    } else {
                        match cli.command.run().await {
                            Ok(()) => 0,
                            Err(err) => {
                                eprintln!("{}", err);
                                1
                            }
                        }
                    }
                })
            }
        };

        code
    })
    .await
    .map_err(|e| anyhow::anyhow!("Failed to execute CLI: {}", e))?;

    let stdout = String::from_utf8_lossy(&stdout_buffer.lock().unwrap()).to_string();
    let stderr = String::from_utf8_lossy(&stderr_buffer.lock().unwrap()).to_string();

    Ok(RunResult {
        code: result,
        stdout,
        stderr,
    })
}
