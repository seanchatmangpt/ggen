//! # mcpp-cli-lib - Command-line interface for MCPP (MCP Plus)
//!
//! This crate provides the command-line interface for MCPP, using clap-noun-verb
//! for automatic command discovery and routing. It bridges between MCPP user commands
//! and the domain logic layer (mcpp-core, mcpp-domain).
//!
//! ## Architecture
//!
//! - **Command Discovery**: Uses clap-noun-verb auto-discovery to find all `#[verb]`
//!   functions in the `cmds` module via linkme's distributed_slice
//! - **Integration**: Re-exports all ggen-cli verbs to provide unified ggen+MCPP CLI
//! - **Native MCPP Verbs**: Includes MCPP-specific verbs (autonomics, a2a-control, etc.)
//!
//! ## Features
//!
//! - **Auto-discovery**: Commands are automatically discovered via clap-noun-verb
//! - **Distributed Registry**: Uses linkme distributed_slice for verb registration
//! - **Async support**: Full async/await support for non-blocking operations
//!
//! ## Examples
//!
//! ### Basic CLI Execution
//!
//! ```rust,no_run
//! use mcpp_cli_lib::run_cli;
//!
//! # fn example() -> Result<(), Box<dyn std::error::Error>> {
//! // Execute CLI with auto-discovered commands
//! run_cli()?;
//! # Ok(())
//! # }
//! ```

#![deny(warnings)]
#![allow(non_upper_case_globals)]
#![allow(clippy::unused_unit)]
#![allow(
    clippy::needless_borrows_for_generic_args,
    clippy::needless_question_mark,
    clippy::new_without_default,
    clippy::question_mark,
    clippy::too_many_arguments,
    clippy::unnecessary_lazy_evaluations,
    clippy::unnecessary_map_or,
    clippy::useless_conversion
)]

// Command modules - clap-noun-verb auto-discovery via linkme
pub mod cmds;

#[cfg(test)]
mod tests;

// Re-export clap-noun-verb for auto-discovery
pub use clap_noun_verb::{run, CommandRouter, Result as ClapNounVerbResult};

// Re-export Result type for use in cmds
pub use clap_noun_verb::Result;

/// Main entry point using clap-noun-verb auto-discovery
///
/// This function delegates to clap-noun-verb::run() which automatically discovers
/// all `#[verb]` functions in the cmds module and its submodules via linkme's
/// distributed_slice. This allows MCPP verbs to be auto-registered without the
/// binary needing to explicitly list them.
pub fn run_cli() -> clap_noun_verb::Result<()> {
    // Use clap-noun-verb auto-discovery (handles --version automatically)
    clap_noun_verb::run()
}
