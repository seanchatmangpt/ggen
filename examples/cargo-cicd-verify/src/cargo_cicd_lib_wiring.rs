// NOTE (fixed during the L5 push's cross-pack verification pass, 2026-07-19):
// this template originally targeted `to: src/lib.rs` with `inject: true` --
// which works in isolation (one pack, its own scratch consumer) but hits this
// engine's own FM-WRITE-008 duplicate-output guard the moment a SECOND pack
// with the same pattern is wired into the same real consumer (confirmed live:
// wasm4pm-cognition-pack and wasm4pm-facts-pack collided on src/lib.rs inside
// examples/receiptctl, which wires both). Retargeted to a pack-unique file;
// the consumer now needs one `include!("<this-file>");` line in their own
// lib.rs to mount it -- a real, disclosed remaining consumer-effort gap, not
// the "consumer wires ggen.toml, done" this template originally claimed.

mod cargo_cicd_catalog;
pub use cargo_cicd_catalog::{
    args_are_consistent, commands_for_noun, distinct_nouns, find_command, is_valid_command,
    parse_args, CargoCicdCommand, CARGO_CICD_COMMANDS,
};

// Handler-gap closure (cap02 / .specify/maturity.ttl's cg_010 mat:d2): a
// real, generated, callable std::process::Command dispatch shim -- see
// cargo_cicd_dispatch.rs.tmpl's module doc for what this does and does not
// close. `pub mod` (not a `pub use` allowlist, unlike the catalog above)
// because the 51 per-command wrapper functions are individually named and
// generated -- callers reach them as `cargo_cicd_dispatch::git_status(&[])`,
// mirroring mcpp-pack's identical `pub mod mcpp_dispatch;` choice in
// mcpp_mod_wiring.rs.tmpl.
pub mod cargo_cicd_dispatch;
