//! `receiptctl` library crate — wires together generated output from all 6
//! packs this example combines:
//!
//! - `clap-noun-verb-pack` -> `src/clap_noun_verb_routes.rs` (CLI routes)
//! - `wasm4pm-algorithms-pack` -> `src/w4pm_algorithms_catalog.rs`
//! - `wasm4pm-cognition-pack` -> `src/w4pm_cognition_{catalog,dispatch}.rs`
//! - `wasm4pm-compat-pack` -> `src/wasm4pm_compat_events.rs`
//! - `chicago-tdd-tools-pack` -> `tests/chicago_tdd_tools_boundary.rs`
//! - `wasm4pm-facts-pack` -> `docs/releases/*/BREED_ALGORITHM_REGISTRY.md` (docs only)

use clap_noun_verb::Result;

pub mod verbs;

// GENERATED catalogs/emission surfaces — see each module's own header.
#[path = "w4pm_algorithms_catalog.rs"]
pub mod w4pm_algorithms_catalog;
#[path = "w4pm_cognition_catalog.rs"]
pub mod w4pm_cognition_catalog;
#[path = "w4pm_cognition_dispatch.rs"]
pub mod w4pm_cognition_dispatch;
#[path = "wasm4pm_compat_events.rs"]
pub mod wasm4pm_compat_events;

// Per-pack lib-wiring mounts (each pack's own `_lib_wiring.rs.tmpl` -- see
// that template's own header note for why this is one hand-written
// `include!` per pack rather than a generated multi-pack lib.rs: two
// packs both trying to inject into this same shared file hits the engine's
// own FM-WRITE-008 duplicate-output guard).
include!("wasm4pm_cognition_lib_wiring.rs");
include!("wasm4pm_facts_lib_wiring.rs");

/// Auto-discover and dispatch every registered `#[verb]` command.
pub fn run() -> Result<()> {
    clap_noun_verb::run()
}
