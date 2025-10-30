use clap::{CommandFactory, Parser};
use std::path::PathBuf;
use std::io::{Read, Write};

use ggen_utils::app_config::AppConfig;
use ggen_utils::error::Result;
use ggen_utils::types::LogLevel;

pub mod cmds;

#[derive(Parser, Debug)]
#[command(name = "ggen", author, about = "Graph-aware code generator", version)]
pub struct Cli {
    #[arg(short, long, value_name = "FILE")]
    pub config: Option<PathBuf>,

    #[arg(long, value_name = "PATH", help = "Path to ggen.toml manifest file")]
    pub manifest_path: Option<PathBuf>,

    #[arg(name = "debug", short, long = "debug", value_name = "DEBUG")]
    pub debug: Option<bool>,

    #[arg(
        name = "log_level",
        short,
        long = "log-level",
        value_name = "LOG_LEVEL"
    )]
    pub log_level: Option<LogLevel>,

    /// OpenTelemetry OTLP endpoint (default: http://localhost:4318)
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

    #[clap(subcommand)]
    pub command: cmds::Commands,
}

pub fn build_cli() -> clap::Command {
    Cli::command()
}

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

/// Structured result for programmatic CLI execution (used by Node addon)
#[derive(Debug, Clone)]
pub struct RunResult {
    pub code: i32,
    pub stdout: String,
    pub stderr: String,
}

/// Programmatic entrypoint to execute the CLI with provided arguments and capture output.
/// This avoids spawning a new process and preserves deterministic behavior.
pub async fn run_for_node(args: Vec<String>) -> Result<RunResult> {
    // Prefix with a binary name to satisfy clap parse_from semantics
    let argv = std::iter::once("ggen").map(str::to_string).chain(args.into_iter());

    // Capture stdout/stderr using gag buffers. Fallback to empty capture if capture fails.
    let mut captured_stdout = Vec::new();
    let mut captured_stderr = Vec::new();

    // Defer importing gag until used to keep surface minimal
    let run_inner = async {
        let cli = Cli::parse_from(argv);

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
            let result = async {
                AppConfig::merge_config(cli.config.as_deref())?;
                let app = Cli::command();
                let matches = app.get_matches();
                AppConfig::merge_args(&matches)?;
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
            cli.command.run().await
        }
    };

    // Try to capture using gag; if gag unavailable at runtime, we still execute.
    let (code, stdout_str, stderr_str) = match (gag::BufferRedirect::stdout(), gag::BufferRedirect::stderr()) {
        (Ok(mut so), Ok(mut se)) => {
            let code_val = match run_inner.await {
                Ok(()) => 0,
                Err(err) => {
                    let _ = writeln!(std::io::stderr(), "{}", err);
                    1
                }
            };
            let _ = so.read_to_end(&mut captured_stdout);
            let _ = se.read_to_end(&mut captured_stderr);
            (code_val, captured_stdout, captured_stderr)
        }
        _ => {
            let code_val = match run_inner.await {
                Ok(()) => 0,
                Err(_err) => 1,
            };
            (code_val, captured_stdout, captured_stderr)
        }
    };

    let stdout = String::from_utf8_lossy(&stdout_str).to_string();
    let stderr = String::from_utf8_lossy(&stderr_str).to_string();

    Ok(RunResult { code, stdout, stderr })
}
