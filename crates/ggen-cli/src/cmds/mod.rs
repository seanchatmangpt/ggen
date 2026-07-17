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
pub mod inverse_sync;
// ARCHIVED (v26.7.16 routing flip): replaced by ggen-engine's `sync` noun
// (verb `run`). A root FLAT verb named "sync" here and a noun named "sync"
// registered by ggen-engine both become top-level clap subcommands named
// "sync" -- clap_builder's debug_asserts panics on the duplicate name in
// debug builds, and release builds would silently shadow one path with the
// other. File retained on disk at cmds/sync.rs, not deleted, per this
// project's fix-forward doctrine.
// pub mod sync;
// ARCHIVED (v26.5.28): ambiguous noun, gated behind default-off `experimental`.
#[cfg(feature = "experimental")]
pub mod wizard;

// Command modules - clap-noun-verb auto-discovery
// ARCHIVED (v26.5.28): a2a/framework/mcp/sigma not provable as finished; gated
// behind default-off `experimental` so they leave the default CLI surface while
// the code is preserved (non-deletion doctrine). See cmds/mod.rs feature note.
#[cfg(feature = "experimental")]
pub mod a2a;
pub mod agent; // AGI-facing lifecycle surface (`ggen agent <verb>`) over crate::agent::PackAgent (ported from ggen_core, T041)
pub mod capability; // capability surfaces → atomic packs (`ggen capability enable/list/inspect`)
// ARCHIVED (v26.7.16 routing flip): replaced by ggen-engine's `doctor` noun
// (verb `run`). Same root-flat-verb-vs-noun collision as `sync` above. File
// retained on disk at cmds/doctor.rs, not deleted, per this project's
// fix-forward doctrine.
// pub mod doctor;
#[cfg(feature = "experimental")]
pub mod framework; // Framework bridge commands (LangChain, etc.)
// ARCHIVED (v26.7.16 routing flip): replaced by ggen-engine's `graph` noun
// (verb `validate`). File retained on disk at cmds/graph.rs, not deleted,
// per this project's fix-forward doctrine.
// pub mod graph;
#[cfg(feature = "lsp")]
pub mod lsp; // ggen lsp noun (start/check/init/serve/mine/metrics/replay/field-status/emit_pack/verify_pack) — opt-in: --features lsp
#[cfg(feature = "experimental")]
pub mod mcp; // MCP delivered via `ggen lsp serve --protocol mcp` (lsp feature) + ggen-lsp-mcp binary
pub mod ontology; // ggen ontology noun (list/status/info/search) — embedded and marketplace ontology management
pub mod pack; // Singular alias for `packs` noun (golden-path: ggen pack add <name>)
pub mod packs; // lockfile-oriented multi-pack management (`ggen packs install/list/validate/show`)
pub mod packs_receipt; // pack-install receipt emitter (full-closure, fail-closed) — invoked by `pack add`
pub mod policy;
// ARCHIVED (v26.7.16 routing flip): replaced by ggen-engine's `receipt` noun
// (verbs `verify`/`history`). File retained on disk at cmds/receipt.rs, not
// deleted, per this project's fix-forward doctrine.
// pub mod receipt; // ggen receipt verify / info — cryptographic receipt CLI surface (BUG-005)
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
