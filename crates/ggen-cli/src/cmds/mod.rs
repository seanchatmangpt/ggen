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
//! - `mcpp generate` → Use `mcpp sync`
//! - `mcpp validate` → Use `mcpp sync --validate-only`
//! - `mcpp template *` → Use `mcpp sync`
//! - `mcpp project *` → Add back in v5.1+
//! - `mcpp graph *` → Add back in v5.1+
//! - `mcpp ontology *` → Add back in v5.1+
//! - `mcpp marketplace *` → Add back in v5.1+
//! - `mcpp ai *` → Add back in v5.1+
//! - `mcpp test *` → Add back in v5.1+
//! - `mcpp utils *` → Add back in v5.1+
//! - `mcpp ci *` → Add back in v5.1+
//! - `mcpp workflow *` → Add back in v5.1+

// Shared helpers for command modules
pub mod helpers;

// Core commands: mcpp sync & mcpp init & mcpp wizard
pub mod capability;
pub mod git_hooks;
pub mod init;
pub mod receipt;
pub mod sync;
pub mod wizard;

// Command modules - clap-noun-verb auto-discovery
pub mod ai;
pub mod construct;
pub mod graph;
// marketplace and hook modules removed — migrated to separate project
pub mod mcp;
pub mod ontology;
pub mod packs;
pub mod paper;
pub mod policy;
pub mod project;
pub mod self_play;
pub mod template;
pub mod utils;
pub mod workflow;
pub mod yawl;

use mcpp_utils::error::Result;

/// Setup and run the command router using clap-noun-verb v3.4.0 auto-discovery
pub fn run_cli() -> Result<()> {
    // Handle --version flag before delegating to clap-noun-verb
    let args: Vec<String> = std::env::args().collect();
    if args.iter().any(|arg| arg == "--version" || arg == "-V") {
        log::info!("mcpp {}", env!("CARGO_PKG_VERSION"));
        return Ok(());
    }

    // Use clap-noun-verb's auto-discovery to find all [verb] functions
    clap_noun_verb::run()
        .map_err(|e| mcpp_utils::error::Error::new(&format!("CLI execution failed: {}", e)))?;
    Ok(())
}
