//! Command Router Module - ggen v5.0.0 (Fresh Start + Init)
//!
//! ggen v5 has two core commands:
//! - `ggen sync` - Code synchronization pipeline
//! - `ggen init` - Project scaffolding
//!
//! All other commands have been removed for a fresh start.
//! Utility commands may be added back incrementally in future versions.
//!
//! ## Core Commands
//!
//! ```bash
//! ggen sync [OPTIONS]    # Synchronize code generation
//! ggen init [OPTIONS]    # Initialize new ggen project
//! ```
//!
//! ## Removed Commands
//!
//! The following commands were removed in v5.0:
//! - `ggen generate` → Use `ggen sync`
//! - `ggen validate` → Use `ggen sync --validate-only`
//! - `ggen template *` → Use `ggen sync`
//! - `ggen project *` → Add back in v5.1+
//! - `ggen graph *` → Add back in v5.1+
//! - `ggen ontology *` → Add back in v5.1+
//! - `ggen marketplace *` → Add back in v5.1+
//! - `ggen ai *` → Add back in v5.1+
//! - `ggen test *` → Add back in v5.1+
//! - `ggen utils *` → Add back in v5.1+
//! - `ggen ci *` → Add back in v5.1+
//! - `ggen workflow *` → Add back in v5.1+

// Shared helpers for command modules
pub mod helpers;

// Core commands: ggen sync & ggen init
pub mod git_hooks;
pub mod init;
pub mod sync;

// CRITICAL: Force the modules to be linked so their #[verb] registration works
// Without this import, the linker optimizes away the linkme static items from #[verb] macro
#[allow(unused_imports)]
use init as _;
#[allow(unused_imports)]
use sync as _;

use ggen_utils::error::Result;
use serde_json::json;

use crate::debug_log;

/// Setup and run the command router using clap-noun-verb auto-discovery
///
/// ggen v5 has ONE command: `ggen sync`. All verb functions are discovered
/// automatically via the `#[verb]` macro.
pub fn run_cli() -> Result<()> {
    // Handle --version flag before delegating to clap-noun-verb
    let args: Vec<String> = std::env::args().collect();

    debug_log(
        "H6",
        "cmds/mod.rs:run_cli:entry",
        "run_cli entry with args",
        json!({ "args": args.clone() }),
    );

    if args.iter().any(|arg| arg == "--version" || arg == "-V") {
        log::info!("ggen {}", env!("CARGO_PKG_VERSION"));
        debug_log(
            "H6",
            "cmds/mod.rs:run_cli:version",
            "handled version flag",
            json!({}),
        );
        return Ok(());
    }

    // Use clap-noun-verb's auto-discovery to find all [verb] functions
    debug_log(
        "H7",
        "cmds/mod.rs:run_cli:router",
        "delegating to clap_noun_verb::run",
        json!({}),
    );

    clap_noun_verb::run()
        .map_err(|e| ggen_utils::error::Error::new(&format!("CLI execution failed: {}", e)))?;

    debug_log(
        "H7",
        "cmds/mod.rs:run_cli:router",
        "clap_noun_verb::run completed",
        json!({}),
    );

    Ok(())
}
