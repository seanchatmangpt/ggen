//! `verbs` module — the hand-written handlers behind the generated routes.
//!
//! The generated `clap_noun_verb_routes.rs` is no longer mounted here: with
//! `[templates] aggregate_modules = true`, the engine-owned
//! `src/ggen_pack_mods.rs` (see lib.rs's single include!) mounts it at the
//! crate root instead. Mounting it in both places would register every
//! `#[verb]` twice via linkme.

pub mod handlers;
