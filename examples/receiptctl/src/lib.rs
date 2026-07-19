//! `receiptctl` library crate — wires together generated output from all 6
//! packs this example combines:
//!
//! - `clap-noun-verb-pack` -> `src/clap_noun_verb_routes.rs` (CLI routes)
//! - `wasm4pm-algorithms-pack` -> `src/w4pm_algorithms_catalog.rs`
//! - `wasm4pm-cognition-pack` -> `src/w4pm_cognition_{catalog,dispatch}.rs`
//! - `wasm4pm-compat-pack` -> `src/wasm4pm_compat_events.rs`
//! - `chicago-tdd-tools-pack` -> `tests/chicago_tdd_tools_boundary.rs`
//! - `wasm4pm-facts-pack` -> `src/wasm4pm_facts_registry.rs` + registry doc
//!
//! Consumer wiring is now exactly ONE permanent line (the `include!` below):
//! `[templates] aggregate_modules = true` in ggen.toml makes `ggen sync`
//! emit an engine-owned `src/ggen_pack_mods.rs` mounting every generated
//! `src/*.rs` module. This replaced the previous one-mount-per-pack
//! hand-wiring (four `#[path]` mounts + two per-pack `include!`s) that the
//! L5 audits scored as the consumer-effort gap, and that produced the
//! FM-WRITE-008 lib.rs collision when packs tried to generate the wiring
//! themselves.

use clap_noun_verb::Result;

pub mod verbs;

include!("ggen_pack_mods.rs");

/// Auto-discover and dispatch every registered `#[verb]` command.
pub fn run() -> Result<()> {
    clap_noun_verb::run()
}
