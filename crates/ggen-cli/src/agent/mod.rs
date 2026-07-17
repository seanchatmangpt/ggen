//! Re-export shim over `ggen_marketplace::agent`.
//!
//! The agent-facing packs + marketplace facade (`facade.rs`, `receipt.rs`,
//! `types.rs`) moved to `ggen-marketplace/src/agent/` as tasks T048/T062 of the
//! ggen-core retirement migration (`specs/014-ggen-core-replacement`): every
//! real dependency those three files had was already `ggen_marketplace::*` (or
//! `ed25519_dalek`/`ggen_config::receipt`/`serde`/`std`), so the module now
//! lives alongside the marketplace subsystems it wraps instead of one crate
//! away in `ggen-cli`.
//!
//! This shim exists so every pre-existing `crate::agent::X` reference in
//! `ggen-cli` (`cmds/agent.rs`, `cmds/packs.rs`, `cmds/packs_receipt.rs`)
//! continues to compile unchanged, pointed at the new authoritative location.
//! Do not add new logic here — this crate's `agent` module is a pure re-export,
//! never a second implementation.
pub use ggen_marketplace::agent::*;
