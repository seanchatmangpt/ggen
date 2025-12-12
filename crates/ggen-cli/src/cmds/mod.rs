//! Command Router Module - clap-noun-verb v5.3.0 Migration
//!
//! This module provides the entry point for clap-noun-verb v5.3.0 with explicit verb registration.
//! All noun modules with `#[verb("verb_name", "noun")]` functions are automatically discovered and registered.
//!
//! ## Architecture: Three-Layer Pattern
//! ```text
//! Layer 3 (CLI): cmds (router) -> explicit verb registration
//! Layer 2 (Integration): auto-discovery -> #[verb] functions -> domain async logic
//! Layer 1 (Domain): pure business logic
//! ```

// Command modules - clap-noun-verb v5.3.0 explicit verb registration
pub mod ai;
pub mod ci;
pub mod graph;
// pub mod hook;        // DISABLED: Deferred to v4.1.0 - Depends on marketplace-v2 feature
pub mod marketplace; // ENABLED: marketplace-v2 CLI integration
                     // pub mod packs;        // DISABLED: Deferred to v4.1.0 - Depends on marketplace-v2 feature
pub mod ontology;
pub mod paper;
pub mod project;
pub mod template;
#[cfg(feature = "test-quality")]
pub mod test; // ENABLED: Feature 004 - Test quality audit and optimization
pub mod utils; // ENABLED: FMEA (Failure Mode and Effects Analysis) utility commands
pub mod workflow;

use ggen_utils::error::Result;
use serde_json::json;

use crate::debug_log;

/// Setup and run the command router using clap-noun-verb v3.4.0 auto-discovery
pub fn run_cli() -> Result<()> {
    // Handle --version flag before delegating to clap-noun-verb
    let args: Vec<String> = std::env::args().collect();
    // #region agent log
    debug_log(
        "H6",
        "cmds/mod.rs:run_cli:entry",
        "run_cli entry with args",
        json!({ "args": args.clone() }),
    );
    // #endregion
    if args.iter().any(|arg| arg == "--version" || arg == "-V") {
        log::info!("ggen {}", env!("CARGO_PKG_VERSION"));
        // #region agent log
        debug_log(
            "H6",
            "cmds/mod.rs:run_cli:version",
            "handled version flag",
            json!({}),
        );
        // #endregion
        return Ok(());
    }

    // Use clap-noun-verb's auto-discovery to find all [verb] functions
    // #region agent log
    debug_log(
        "H7",
        "cmds/mod.rs:run_cli:router",
        "delegating to clap_noun_verb::run",
        json!({}),
    );
    // #endregion
    clap_noun_verb::run()
        .map_err(|e| ggen_utils::error::Error::new(&format!("CLI execution failed: {}", e)))?;
    // #region agent log
    debug_log(
        "H7",
        "cmds/mod.rs:run_cli:router",
        "clap_noun_verb::run completed",
        json!({}),
    );
    // #endregion
    Ok(())
}
