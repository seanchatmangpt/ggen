//! Command Router Module - clap-noun-verb v3.4.0 Auto-Discovery
//!
//! This module provides the entry point for clap-noun-verb auto-discovery.
//! All noun modules with `\[verb\]` functions are automatically discovered and registered.
//!
//! ## Architecture
//! ```text
//! cmds (router) -> auto-discovery -> [verb] functions -> domain (async logic)
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

// Command modules - clap-noun-verb auto-discovery
pub mod ai;
pub mod construct;
pub mod graph;
pub mod hook;
pub mod mcp;
// pub mod marketplace;  // DISABLED: Pending v2 API migration (128 compilation errors)
pub mod ontology;
pub mod packs;
pub mod paper;
pub mod project;
pub mod template;
pub mod utils;
pub mod workflow;
pub mod yawl;

use ggen_utils::error::Result;

/// Setup and run the command router using clap-noun-verb v3.4.0 auto-discovery
pub fn run_cli() -> Result<()> {
    // Handle --version flag before delegating to clap-noun-verb
    let args: Vec<String> = std::env::args().collect();
    if args.iter().any(|arg| arg == "--version" || arg == "-V") {
        log::info!("ggen {}", env!("CARGO_PKG_VERSION"));
        return Ok(());
    }

    // Use clap-noun-verb's auto-discovery to find all [verb] functions
    clap_noun_verb::run()
        .map_err(|e| ggen_utils::error::Error::new(&format!("CLI execution failed: {}", e)))?;
    Ok(())
}
