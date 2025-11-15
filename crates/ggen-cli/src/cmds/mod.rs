//! Command Router Module - clap-noun-verb v3.4.0 Auto-Discovery
//!
//! This module provides the entry point for clap-noun-verb auto-discovery.
//! All noun modules with `\[verb\]` functions are automatically discovered and registered.
//!
//! ## Architecture
//! ```text
//! cmds (router) -> auto-discovery -> [verb] functions -> domain (async logic)
//! ```

// Command modules - clap-noun-verb auto-discovery
pub mod ai;
pub mod graph;
pub mod hook;
pub mod marketplace;
pub mod project;
pub mod template;
pub mod utils;
pub mod workflow;

use ggen_utils::error::Result;

/// Setup and run the command router using clap-noun-verb v3.4.0 auto-discovery
pub fn run_cli() -> Result<()> {
    // Handle --version flag before delegating to clap-noun-verb
    let args: Vec<String> = std::env::args().collect();
    if args.iter().any(|arg| arg == "--version" || arg == "-V") {
        println!("ggen {}", env!("CARGO_PKG_VERSION"));
        return Ok(());
    }

    // Use clap-noun-verb's auto-discovery to find all [verb] functions
    clap_noun_verb::run()
        .map_err(|e| ggen_utils::error::Error::new(&format!("CLI execution failed: {}", e)))?;
    Ok(())
}
