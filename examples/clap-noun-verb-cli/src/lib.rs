//! `clap-noun-verb-cli` library crate — wires the ggen-generated routes and
//! hand-written handlers together, and re-exports `clap_noun_verb::run()`.
//!
//! ```text
//! main.rs
//!    -> clap_noun_verb_cli::run()
//!         -> clap_noun_verb::run()  (auto-discovers all #[verb] commands
//!            via CommandRegistry's distributed_slice entries)
//! ```

use clap_noun_verb::Result;

pub mod verbs;

/// Auto-discover and dispatch every registered `#[verb]` command.
pub fn run() -> Result<()> {
    clap_noun_verb::run()
}
