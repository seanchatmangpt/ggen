//! `lsp-max-verify` — committed, real consumer of `lsp-max-pack`.
//!
//! Wires the pack's generated `rules/lsp_max_*.toml` rule-pack files and
//! `docs/lsp_max_rule_pack.md` catalog, exercised entirely by the two
//! generated proof tests under `tests/` (`lsp_max_rule_pack_proof.rs`, the
//! pre-existing hand-transcribed proof, and
//! `lsp_max_rule_pack_sparql_derived_proof.rs`, the SPARQL-derived second
//! proof). Unlike `praxis-core-pack`/`star-toml-pack`, `lsp-max-pack`
//! generates no Rust module for this crate to `include!` -- its output
//! (regex rule-pack TOML + a markdown catalog) is consumed by an external
//! `lsp-max` `RulePackServer`, not by this crate's own Rust code -- so
//! there is nothing to wire here beyond the pack's own `ggen.toml`
//! registration.
