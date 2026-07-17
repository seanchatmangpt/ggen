//! ggen - Ontology-Driven Code Generation
//!
//! This is a workspace crate that provides unified access to all ggen modules.
//! The actual implementation is in the workspace crates under `crates/`.
//!
//! For the CLI tool, use `ggen-cli-lib`.
//! For the core engine library, depend on `ggen-engine` directly from within this
//! workspace/git checkout -- it is NOT re-exported here (see below) and is not
//! usable by external crates.io consumers of the published `ggen` crate.

// T051 originally re-pointed this re-export from `ggen_core` to `ggen_engine`
// (docs/jira/v26.7.16/10-ROOT-PACKAGE-TEST-MIGRATION.md) without running
// `cargo publish --dry-run -p ggen` to check the consequence. Reverted 2026-07-16:
// `ggen-engine` -> `praxis-core`/`praxis-graphlaw` carry hard absolute filesystem
// path dependencies on sibling repos (/Users/sac/bcinr/crates/*,
// /Users/sac/wasm4pm-compat) that only exist on this machine, so `ggen-engine` is
// `publish = false` and can never resolve on crates.io -- a real dependency edge on
// it here broke `cargo publish --dry-run -p ggen` with "no matching package named
// `ggen-engine` found". Confirmed zero internal consumers of this re-export (grep
// for `ggen::core::`/`use ggen::core` across src/tests/examples/benches/crates
// returned nothing) before removing it, so this is a leaf revert with no ripple.
// The published `ggen` crate therefore has no `core` module until `ggen-engine`
// itself becomes independently publishable (a separate, still-open question --
// see docs/jira/v26.7.16/ tickets 01/02) or an alternative non-re-export design is
// chosen.

/// Workspace version information
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
