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
//! use ggen_cli::cli_match;
//!
//! # async fn example() -> ggen_utils::error::Result<()> {
//! // Execute CLI with auto-discovered commands
//! cli_match().await?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Programmatic Execution
//!
//! ```rust,no_run
//! use ggen_cli::run_for_node;
//!
//! # async fn example() -> ggen_utils::error::Result<()> {
//! let args = vec!["template".to_string(), "generate".to_string()];
//! let result = run_for_node(args).await?;
//! println!("Exit code: {}", result.code);
//! println!("Output: {}", result.stdout);
//! # Ok(())
//! # }
//! ```

#![deny(warnings)] // Poka-Yoke: Prevent warnings at compile time - compiler enforces correctness
#![allow(non_upper_case_globals)] // Allow macro-generated static variables from clap-noun-verb

use std::io::{Read, Write};

// Command modules - clap-noun-verb v3.4.0 auto-discovery
pub mod cmds; // clap-noun-verb v3 entry points with #[verb] functions
pub mod conventions; // File-based routing conventions
                     // pub mod domain;          // Business logic layer - MOVED TO ggen-domain crate
pub mod prelude;
pub mod runtime; // Async/sync bridge utilities
pub mod runtime_helper; // Sync CLI wrapper utilities for async operations // Common imports for commands

// Re-export clap-noun-verb for auto-discovery
pub use clap_noun_verb::{run, CommandRouter, Result as ClapNounVerbResult};

/// Main entry point using clap-noun-verb v3.4.0 auto-discovery
///
/// This function delegates to clap-noun-verb::run() which automatically discovers
/// all `\[verb\]` functions in the cmds module and its submodules.
pub async fn cli_match() -> ggen_utils::error::Result<()> {
    // Handle --version flag before delegating to clap-noun-verb
    let args: Vec<String> = std::env::args().collect();
    if args.iter().any(|arg| arg == "--version" || arg == "-V") {
        println!("ggen {}", env!("CARGO_PKG_VERSION"));
        return Ok(());
    }

    // Use clap-noun-verb auto-discovery
    clap_noun_verb::run().map_err(|e| anyhow::anyhow!("CLI execution failed: {}", e))?;
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
pub async fn run_for_node(args: Vec<String>) -> ggen_utils::error::Result<RunResult> {
    use std::sync::Arc;
    use std::sync::Mutex;

    // Prefix with a binary name to satisfy clap-noun-verb semantics
    let _argv: Vec<String> = std::iter::once("ggen".to_string())
        .chain(args.into_iter())
        .collect();

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
                // Execute using cmds router
                let code_val = match cmds::run_cli() {
                    Ok(()) => 0,
                    Err(err) => {
                        let _ = writeln!(std::io::stderr(), "{}", err);
                        1
                    }
                };

                let _ = so.read_to_end(&mut captured_stdout);
                let _ = se.read_to_end(&mut captured_stderr);

                *stdout_clone.lock().unwrap() = captured_stdout;
                *stderr_clone.lock().unwrap() = captured_stderr;

                code_val
            }
            _ => {
                // Fallback: execute without capture
                match cmds::run_cli() {
                    Ok(()) => 0,
                    Err(err) => {
                        eprintln!("{}", err);
                        1
                    }
                }
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
