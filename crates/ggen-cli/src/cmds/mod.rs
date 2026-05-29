//! Command Router Module - clap-noun-verb v26.5.19 Auto-Discovery
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
//! The following commands were removed in v26.5.19:
//! - `ggen generate` → Use `ggen sync`
//! - `ggen validate` → Use `ggen sync --validate-only`
//! - `ggen template *` → Use `ggen sync`
//! - `ggen project *` → Add back in v26.5.19+
//! - `ggen graph *` → Add back in v26.5.19+
//! - `ggen ontology *` → Add back in v26.5.19+
//! - `ggen marketplace *` → Add back in v26.5.19+
//! - `ggen ai *` → Add back in v26.5.19+
//! - `ggen test *` → Add back in v26.5.19+
//! - `ggen utils *` → Add back in v26.5.19+
//! - `ggen ci *` → Add back in v26.5.19+
//! - `ggen workflow *` → Add back in v26.5.19+

// Shared helpers for command modules
pub mod helpers;

// Core commands: ggen sync & ggen init
pub mod git_hooks;
pub mod init;
pub mod sync;
// ARCHIVED (v26.5.28): ambiguous noun, gated behind default-off `experimental`.
#[cfg(feature = "experimental")]
pub mod wizard;

// Command modules - clap-noun-verb auto-discovery
// ARCHIVED (v26.5.28): a2a/framework/mcp/sigma not provable as finished; gated
// behind default-off `experimental` so they leave the default CLI surface while
// the code is preserved (non-deletion doctrine). See cmds/mod.rs feature note.
#[cfg(feature = "experimental")]
pub mod a2a;
pub mod doctor;
#[cfg(feature = "experimental")]
pub mod framework; // Framework bridge commands (LangChain, etc.)
pub mod graph;
#[cfg(feature = "experimental")]
pub mod mcp; // MCP delivered via `ggen lsp serve --protocol mcp` (lsp feature) + ggen-lsp-mcp binary
#[cfg(feature = "lsp")]
pub mod lsp; // ggen lsp noun (start/check/init/serve/mine/metrics/replay/field-status/emit_pack/verify_pack) — opt-in: --features lsp
pub mod pack; // Singular alias for `packs` noun (golden-path: ggen pack add <name>)
pub mod packs_receipt; // pack-install receipt emitter (full-closure, fail-closed) — invoked by `pack add`
pub mod policy;
#[cfg(feature = "experimental")]
pub mod sigma;
pub mod utils;

use crate::prelude::*;

/// Setup and run the command router using clap-noun-verb v26.5.19 auto-discovery
pub fn run_cli() -> Result<()> {
    // Handle --version flag before delegating to clap-noun-verb
    let args: Vec<String> = std::env::args().collect();
    if args.iter().any(|arg| arg == "--version" || arg == "-V") {
        log::info!("ggen {}", env!("CARGO_PKG_VERSION"));
        return Ok(());
    }

    // Use clap-noun-verb's auto-discovery to find all [verb] functions
    clap_noun_verb::run().map_err(GgenError::from_clap_error)?;
    Ok(())
}
