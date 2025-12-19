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
//! ```rust,ignore
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
//! ```rust,ignore
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

use std::fs::OpenOptions;
use std::io::{Read, Write};
use std::time::{SystemTime, UNIX_EPOCH};

// Command modules - clap-noun-verb v4.0.2 auto-discovery
pub mod cmds; // clap-noun-verb v4 entry points with #[verb] functions
pub mod conventions; // File-based routing conventions
                     // pub mod domain;          // Business logic layer - MOVED TO ggen-domain crate
#[cfg(feature = "autonomic")]
pub mod introspection; // AI agent introspection: verb metadata discovery, capability handlers
pub mod prelude;
pub mod runtime; // Async/sync bridge utilities
pub mod runtime_helper; // Sync CLI wrapper utilities for async operations // Common imports for commands
#[cfg(any(feature = "full", feature = "test-quality"))]
pub mod validation; // Compile-time validation (Andon Signal Validation Framework)

// Re-export clap-noun-verb for auto-discovery
pub use clap_noun_verb::{run, CliBuilder, CommandRouter, Result as ClapNounVerbResult};
use serde_json::json;

fn debug_log(hypothesis_id: &str, location: &str, message: &str, data: serde_json::Value) {
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis())
        .unwrap_or(0);

    let payload = json!({
        "sessionId": "debug-session",
        "runId": "pre-fix",
        "hypothesisId": hypothesis_id,
        "location": location,
        "message": message,
        "data": data,
        "timestamp": timestamp
    });

    if let Ok(mut file) = OpenOptions::new()
        .create(true)
        .append(true)
        .open("/Users/sac/ggen/.cursor/debug.log")
    {
        let _ = writeln!(file, "{}", payload);
    }
}

/// Main entry point using clap-noun-verb v4.0.2 auto-discovery
///
/// This function handles global introspection flags (--capabilities, --introspect, --graph)
/// before delegating to clap-noun-verb::run() which automatically discovers
/// all `\[verb\]` functions in the cmds module and its submodules.
/// The version flag is handled automatically by clap-noun-verb.
pub async fn cli_match() -> ggen_utils::error::Result<()> {
    // Check for introspection flags (must come before clap-noun-verb processing)
    // These flags are for AI agent discovery and capability planning
    #[cfg(feature = "autonomic")]
    {
        let args: Vec<String> = std::env::args().collect();
        // #region agent log
        debug_log(
            "H1",
            "lib.rs:cli_match:entry",
            "cli_match entry with args",
            json!({ "args": args.clone() }),
        );
        // #endregion
        // Handle --graph flag (export complete command graph)
        if args.contains(&"--graph".to_string()) {
            let graph = introspection::build_command_graph();
            let json = serde_json::to_string_pretty(&graph).map_err(|e| {
                ggen_utils::error::Error::new(&format!("Failed to serialize command graph: {}", e))
            })?;
            println!("{}", json);
            // #region agent log
            debug_log(
                "H2",
                "lib.rs:cli_match:graph",
                "handled --graph flag",
                json!({ "total_verbs": graph.total_verbs, "noun_count": graph.nouns.len() }),
            );
            // #endregion
            return Ok(());
        }

        // Handle --capabilities noun verb (list verb metadata and arguments)
        if args.contains(&"--capabilities".to_string()) {
            if args.len() >= 4 {
                let noun = &args[args.iter().position(|x| x == "--capabilities").unwrap() + 1];
                let verb = &args[args.iter().position(|x| x == "--capabilities").unwrap() + 2];

                match introspection::get_verb_metadata(noun, verb) {
                    Some(metadata) => {
                        let json = serde_json::to_string_pretty(&metadata).map_err(|e| {
                            ggen_utils::error::Error::new(&format!(
                                "Failed to serialize metadata: {}",
                                e
                            ))
                        })?;
                        println!("{}", json);
                        // #region agent log
                        debug_log(
                            "H3",
                            "lib.rs:cli_match:capabilities",
                            "served --capabilities metadata",
                            json!({ "noun": metadata.noun, "verb": metadata.verb, "arg_count": metadata.arguments.len() }),
                        );
                        // #endregion
                        return Ok(());
                    }
                    None => {
                        eprintln!("Verb not found: {}::{}", noun, verb);
                        // #region agent log
                        debug_log(
                            "H3",
                            "lib.rs:cli_match:capabilities",
                            "capabilities verb not found",
                            json!({ "noun": noun, "verb": verb }),
                        );
                        // #endregion
                        return Err(ggen_utils::error::Error::new(&format!(
                            "Verb {}::{} not found",
                            noun, verb
                        )));
                    }
                }
            } else {
                // #region agent log
                debug_log(
                    "H3",
                    "lib.rs:cli_match:capabilities",
                    "capabilities usage error",
                    json!({ "arg_len": args.len() }),
                );
                // #endregion
                return Err(ggen_utils::error::Error::new(
                    "Usage: ggen --capabilities <noun> <verb>",
                ));
            }
        }

        // Handle --introspect noun verb (show type information)
        if args.contains(&"--introspect".to_string()) {
            if args.len() >= 4 {
                let noun = &args[args.iter().position(|x| x == "--introspect").unwrap() + 1];
                let verb = &args[args.iter().position(|x| x == "--introspect").unwrap() + 2];

                match introspection::get_verb_metadata(noun, verb) {
                    Some(metadata) => {
                        // Show detailed type information
                        println!("Verb: {}::{}", metadata.noun, metadata.verb);
                        println!("Description: {}", metadata.description);
                        println!("Return Type: {}", metadata.return_type);
                        println!("JSON Output: {}", metadata.supports_json_output);
                        println!("\nArguments:");
                        for arg in &metadata.arguments {
                            println!(
                                "  - {} ({}): {}",
                                arg.name, arg.argument_type, arg.description
                            );
                            if let Some(default) = &arg.default_value {
                                println!("    Default: {}", default);
                            }
                            if arg.optional {
                                println!("    Optional: yes");
                            } else {
                                println!("    Required: yes");
                            }
                        }
                        // #region agent log
                        debug_log(
                            "H4",
                            "lib.rs:cli_match:introspect",
                            "served --introspect metadata",
                            json!({ "noun": metadata.noun, "verb": metadata.verb, "arg_count": metadata.arguments.len(), "return_type": metadata.return_type }),
                        );
                        // #endregion
                        return Ok(());
                    }
                    None => {
                        eprintln!("Verb not found: {}::{}", noun, verb);
                        // #region agent log
                        debug_log(
                            "H4",
                            "lib.rs:cli_match:introspect",
                            "introspect verb not found",
                            json!({ "noun": noun, "verb": verb }),
                        );
                        // #endregion
                        return Err(ggen_utils::error::Error::new(&format!(
                            "Verb {}::{} not found",
                            noun, verb
                        )));
                    }
                }
            } else {
                // #region agent log
                debug_log(
                    "H4",
                    "lib.rs:cli_match:introspect",
                    "introspect usage error",
                    json!({ "arg_len": args.len() }),
                );
                // #endregion
                return Err(ggen_utils::error::Error::new(
                    "Usage: ggen --introspect <noun> <verb>",
                ));
            }
        }
    }

    // Use clap-noun-verb CliBuilder for explicit version configuration
    // This ensures the correct version (from ggen's Cargo.toml) is displayed
    // #region agent log
    debug_log(
        "H5",
        "lib.rs:cli_match:router",
        "delegating to CliBuilder with explicit version",
        json!({ "version": env!("CARGO_PKG_VERSION") }),
    );
    // #endregion

    // IMPORTANT: Don't wrap clap-noun-verb errors. Help/version are returned as errors
    // with exit code 0, and wrapping them causes "ERROR: CLI execution failed" to appear.
    // See: docs/howto/setup-help-and-version.md
    CliBuilder::new()
        .name("ggen")
        .about("Language-agnostic, deterministic code generation CLI. Ontologies + RDF → reproducible code projections.")
        .version(env!("CARGO_PKG_VERSION"))  // Use ggen's version (5.0.0), not clap-noun-verb's
        .run()
        .map_err(|e| ggen_utils::error::Error::new(&e.to_string()))?;

    // #region agent log
    debug_log(
        "H5",
        "lib.rs:cli_match:router",
        "CliBuilder run completed",
        json!({}),
    );
    // #endregion
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
///
/// Note: Uses deprecated run_cli() because cli_match() is async and cannot be called
/// inside spawn_blocking. This is a legitimate architectural constraint.
#[allow(deprecated)]
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

                // Store captured output, handle mutex poisoning gracefully
                match stdout_clone.lock() {
                    Ok(mut guard) => *guard = captured_stdout,
                    Err(poisoned) => {
                        // Recover from poisoned lock
                        log::warn!("Stdout mutex was poisoned, recovering");
                        let mut guard = poisoned.into_inner();
                        *guard = captured_stdout;
                    }
                }

                match stderr_clone.lock() {
                    Ok(mut guard) => *guard = captured_stderr,
                    Err(poisoned) => {
                        // Recover from poisoned lock
                        log::warn!("Stderr mutex was poisoned, recovering");
                        let mut guard = poisoned.into_inner();
                        *guard = captured_stderr;
                    }
                }

                code_val
            }
            _ => {
                // Fallback: execute without capture
                match cmds::run_cli() {
                    Ok(()) => 0,
                    Err(err) => {
                        log::error!("{}", err);
                        1
                    }
                }
            }
        };

        code
    })
    .await
    .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to execute CLI: {}", e)))?;

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
