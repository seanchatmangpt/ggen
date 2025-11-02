use std::io::{Read, Write};

// Command modules - clap-noun-verb auto-discovery
pub mod commands;        // Auto-discovered command implementations
pub mod domain;          // Business logic layer
pub mod runtime;         // Async/sync bridge utilities
pub mod runtime_helper;  // Sync CLI wrapper utilities for async operations
pub mod prelude;         // Common imports for commands

// Re-export clap-noun-verb for auto-discovery
pub use clap_noun_verb::{run, CommandRouter, Result as ClapNounVerbResult};

/// Main entry point using clap-noun-verb auto-discovery
pub async fn cli_match() -> ggen_utils::error::Result<()> {
    // Auto-discover and run commands from commands/ directory
    run()
        .map_err(|e| anyhow::anyhow!("CLI execution failed: {}", e))?;
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
                // Execute using clap-noun-verb auto-discovery
                let code_val = match run() {
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
                match run() {
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
