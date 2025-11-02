//! v2.0 Commands Layer - Sync CLI Wrappers
//!
//! This module contains synchronous CLI wrappers that bridge to async domain logic.
//! Each wrapper:
//! - Parses CLI arguments via clap
//! - Validates and transforms input
//! - Spawns Tokio runtime via `runtime::execute`
//! - Calls async domain functions
//! - Handles errors and formats output
//!
//! ## Architecture Pattern
//! ```
//! cmds (clap integration) → commands (sync wrapper) → runtime → domain (async logic)
//! ```

pub mod ai;
pub mod ci;
pub mod graph;
pub mod marketplace;
pub mod project;
pub mod template;
pub mod utils;
