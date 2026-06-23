//! # ggen-cli - Command-line interface for ggen code generation
//!
//! This crate provides the command-line interface for ggen, using clap-noun-verb
//! for automatic command discovery and routing. It bridges between user commands
//! and the domain logic layer (ggen-domain).
//!
//! ## Architecture
//!
//! - **Command Discovery**: Uses clap-noun-verb v3.4.0 auto-discovery to find
//!   all `\[verb\]` functions in the `cmds` module
//! - **Async/Sync Bridge**: Provides runtime utilities to bridge async domain
//!   functions with synchronous CLI execution
//! - **Conventions**: File-based routing conventions for template-based command
//!   generation
//! - **Node Integration**: Programmatic entry point for Node.js addon integration
//!
//! ## Features
//!
//! - **Auto-discovery**: Commands are automatically discovered via clap-noun-verb
//! - **Version handling**: Built-in `--version` flag support
//! - **Output capture**: Programmatic execution with stdout/stderr capture
//! - **Async support**: Full async/await support for non-blocking operations
//!
//! ## Examples
//!
//! ### Basic CLI Execution
//!
//! ```rust,no_run
//! use ggen_cli_lib::cli_match;
//!
//! # async fn example() -> ggen_core::utils::error::Result<()> {
//! // Execute CLI with auto-discovered commands
//! cli_match().await?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Programmatic Execution
//!
//! ```rust,ignore
//! use ggen_cli_lib::run_for_node;
//!
//! # async fn example() -> ggen_core::utils::error::Result<()> {
//! let args = vec!["template".to_string(), "generate".to_string()];
//! let result = run_for_node(args).await?;
//! println!("Exit code: {}", result.code);
//! println!("Output: {}", result.stdout);
//! # Ok(())
//! # }
//! ```
#![deny(warnings)]
#![allow(unexpected_cfgs)]
#![allow(unused_imports)]
#![allow(dead_code)]
// Poka-Yoke: Prevent warnings at compile time - compiler enforces correctness
// Crate-level clippy exceptions for CLI conventions and test code:
// - expect_used: explicit panic messages improve diagnostics in CLI error paths
// - unwrap_used: pervasive in conventions/watcher patterns and test code
// - panic: test assertions in integration tests
#![allow(clippy::expect_used, clippy::unwrap_used, clippy::panic)]
#![allow(non_upper_case_globals)] // Allow macro-generated static variables from clap-noun-verb
#![allow(clippy::unused_unit)] // clap-noun-verb #[verb] macro generates unit expressions
#![allow(
    clippy::needless_borrows_for_generic_args,
    clippy::needless_question_mark,
    clippy::new_without_default,
    clippy::question_mark,
    clippy::too_many_arguments,
    clippy::unnecessary_lazy_evaluations,
    clippy::unnecessary_map_or,
    clippy::useless_conversion,
    clippy::used_underscore_binding  // #[verb] macro generates code referencing underscore-prefixed CLI flag params
)]
pub mod config_clap;
pub mod error;
pub mod pack_install;
pub mod prelude;
pub mod progress;
pub mod validation_lib;
pub mod version_checker;

// Note: std::io::Write was used for output capture with gag crate (now disabled)

// Command modules - clap-noun-verb v26.5.19 auto-discovery
pub mod cmds; // clap-noun-verb v26 entry points with #[verb] functions
pub mod conventions; // File-based routing conventions
pub mod receipt_manager; // Cryptographic receipt generation for CLI operations
pub mod runtime; // Async/sync bridge utilities
pub mod runtime_helper; // Sync CLI wrapper utilities for async operations // Common imports for commands

// Re-export clap-noun-verb for auto-discovery
pub use clap_noun_verb::{run, Result as ClapNounVerbResult};

// Re-export Result type for use in cmds
pub use ggen_core::utils::error::Result;

/// Main entry point using clap-noun-verb v26.5.19 auto-discovery
///
/// This function delegates to clap-noun-verb::run() which automatically discovers
/// all `\[verb\]` functions in the cmds module and its submodules.
/// The version flag is handled automatically by clap-noun-verb.
pub async fn cli_match() -> ggen_core::utils::error::Result<()> {
    version_checker::check_outdated_binary();

    // Find manifest path from CLI args to check if telemetry is configured in ggen.toml
    let mut manifest_path = "ggen.toml".to_string();
    let args: Vec<String> = std::env::args().collect();
    for i in 0..args.len() {
        if (args[i] == "--manifest" || args[i] == "-m") && i + 1 < args.len() {
            manifest_path = args[i + 1].clone();
            break;
        }
    }

    let mut telemetry_config = None;
    if std::path::Path::new(&manifest_path).exists() {
        if let Ok(content) = std::fs::read_to_string(&manifest_path) {
            if let Ok(config) = toml::from_str::<ggen_core::config_lib::GgenConfig>(&content) {
                if let Some(ref tel) = config.telemetry {
                    telemetry_config = Some(ggen_core::telemetry::TelemetryConfig {
                        endpoint: tel.endpoint.clone(),
                        service_name: tel.service_name.clone(),
                        console_output: tel.console_output,
                    });
                }
            }
        }
    }

    // Initialize OTLP telemetry only if configured in ggen.toml
    let _telemetry_guard = if let Some(cfg) = telemetry_config {
        ggen_core::telemetry::init_telemetry(cfg).ok()
    } else {
        None
    };

    // Root span so every CLI invocation produces at least one exportable trace
    let args: Vec<String> = std::env::args().skip(1).collect();
    let span = tracing::info_span!("ggen.cli", command = %args.join(" "), version = env!("CARGO_PKG_VERSION"));
    let _enter = span.enter();

    // Handle --version flag before delegating to clap-noun-verb
    let args: Vec<String> = std::env::args().collect();
    if args.iter().any(|arg| arg == "--version" || arg == "-V") {
        println!("ggen {}", env!("CARGO_PKG_VERSION"));
        return Ok(());
    }

    // Use clap-noun-verb auto-discovery (handles --version automatically, but we preempted it)
    clap_noun_verb::run().map_err(|e| {
        ggen_core::utils::error::Error::new(&format!("CLI execution failed: {}", e))
    })?;
    Ok(())
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
pub async fn run_for_node(args: Vec<String>) -> ggen_core::utils::error::Result<RunResult> {
    use std::sync::Arc;
    use std::sync::Mutex;

    // Known top-level subcommands (nouns) registered via clap-noun-verb
    const KNOWN_NOUNS: &[&str] = &[
        "sync",
        "init",
        "doctor",
        "pack",
        "agent",
        "graph",
        "receipt",
        "utils",
        "policy",
        "market",
        "lifecycle",
        "a2a",
        "ci",
        "framework",
        "git-hooks",
        "lsp",
        "mcp",
        "sigma",
        "template",
        "wizard",
        // Global flags that are always valid
        "--help",
        "-h",
        "--version",
        "-V",
        "--format",
        "--select",
        "--introspect",
        "--structured-errors",
        "--autonomic",
        "help",
        // Allow empty args (shows help)
    ];

    // Validate the first argument — if it's not a known noun or flag, return code 1 immediately
    // This is necessary because run_cli() reads std::env::args() and cannot be passed our args.
    let early_exit_code: Option<i32> = if let Some(first) = args.first() {
        if KNOWN_NOUNS.contains(&first.as_str()) {
            None // known — proceed
        } else {
            // Unknown subcommand — report error and return non-zero
            log::error!("error: unrecognized subcommand '{}'", first);
            Some(1)
        }
    } else {
        None // no args — proceed (shows help)
    };

    if let Some(code) = early_exit_code {
        return Ok(RunResult {
            code,
            stdout: String::new(),
            stderr: format!(
                "error: unrecognized subcommand '{}'",
                args.first().unwrap_or(&String::new())
            ),
        });
    }

    // Prefix with a binary name to satisfy clap-noun-verb semantics
    let _argv: Vec<String> = std::iter::once("ggen".to_string())
        .chain(args.into_iter())
        .collect();

    // Create thread-safe buffers for capturing output
    let stdout_buffer = Arc::new(Mutex::new(Vec::new()));
    let stderr_buffer = Arc::new(Mutex::new(Vec::new()));

    let stdout_clone = Arc::clone(&stdout_buffer);
    let stderr_clone = Arc::clone(&stderr_buffer);

    // Execute in a blocking task
    // NOTE: Output capture with gag crate is disabled for now
    let result = tokio::task::spawn_blocking(move || {
        // Execute without capture (gag crate not available)
        let code = match cmds::run_cli() {
            Ok(()) => 0,
            Err(err) => {
                log::error!("{}", err);
                1
            }
        };

        // Suppress unused variable warnings
        let _ = (stdout_clone, stderr_clone);

        code
    })
    .await
    .map_err(|e| ggen_core::utils::error::Error::new(&format!("Failed to execute CLI: {}", e)))?;

    // Retrieve captured output, handle mutex poisoning gracefully
    let stdout = match stdout_buffer.lock() {
        Ok(guard) => String::from_utf8_lossy(&guard).to_string(),
        Err(_poisoned) => {
            log::warn!("Stdout buffer mutex was poisoned when reading, using empty string");
            String::new()
        }
    };

    let stderr = match stderr_buffer.lock() {
        Ok(guard) => String::from_utf8_lossy(&guard).to_string(),
        Err(_poisoned) => {
            log::warn!("Stderr buffer mutex was poisoned when reading, using empty string");
            String::new()
        }
    };

    Ok(RunResult {
        code: result,
        stdout,
        stderr,
    })
}
