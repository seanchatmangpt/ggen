//! `affidavit-verify` — committed, real consumer of `affidavit-pack`.
//!
//! Wires the pack's generated modules via the single `include!` line the
//! pack's own generated `src/affidavit_lib_wiring.rs` documents as the
//! disclosed remaining consumer-effort step (see that file's header
//! comment, and `packs/affidavit-pack/templates/affidavit_mod_wiring.rs.tmpl`).

include!("affidavit_lib_wiring.rs");
