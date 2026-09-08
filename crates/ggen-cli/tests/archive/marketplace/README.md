# Archived: v1/v2-transitional marketplace test suite

Archived 2026-08-24 (ticket `docs/jira/v26.8.16/04-MARKETPLACE-TEST-SUITE-DISABLED.md`),
closing the two-outcome rule (repair-and-re-enable or formally archive) against these
seven modules, which had been sitting behind commented-out `pub mod` lines in
`crates/ggen-cli/tests/marketplace/mod.rs` since the ggen-core removal
(`docs/jira/v26.7.16/14-GGEN-CORE-REMOVAL-PROPOSAL.md`).

## Why archived, not repaired

Direct inspection (2026-08-24) found every file here targets an architecture that no
longer exists, not one that merely needs its imports repointed:

- `registry_tests.rs`, `unit/{search_ranking,maturity_scoring,package_filtering}_test.rs`,
  `security/consolidated_security.rs`, `integration/cross_backend_test.rs`,
  `performance/consolidated_performance.rs` import `ggen_core::utils::error::Result` and
  `ggen_cli_lib::domain::marketplace::registry::*` — both fully deleted
  (`ggen-core` removed outright per the ticket above; `ggen_cli_lib::domain::marketplace`
  has no such module on disk).
- `integration/{v2_workflows,backward_compat}_test.rs`, `unit/{rdf_mapping,
  adapter_conversion}_test.rs` import `ggen_marketplace_v2`, a crate name that was never
  a workspace member, gated behind `#[cfg(feature = "marketplace-v2")]`, a feature that
  does not exist in `crates/ggen-cli/Cargo.toml` either — dead code even if uncommented.
- `integration/{cli_commands,edge_cases}_test.rs` drive the compiled binary via
  `assert_cmd::Command::cargo_bin("ggen").arg("marketplace")...` — but the live CLI noun
  is `ggen pack <verb>` (`add/list/new/doctor/show/query/remove/search/related`), not
  `ggen marketplace`; these would fail at the first assertion, not at compile time.

Porting any of these to the live `ggen-marketplace`/`ggen pack` surface is a rewrite, not
a repair — the domain types, the CLI noun, and the backend (registry-file vs. RDF/SPARQL)
all changed. Rewriting is tracked as real, new coverage instead: see
`crates/ggen-cli/tests/marketplace/pack_cli_test.rs`, added in the same pass as this
archival, which exercises the live `ggen pack list/search/show/related` verbs
Chicago-style against the compiled binary and the real local pack registry.

## Non-deletion doctrine

These files are preserved, not deleted, per this repo's non-deletion doctrine. They are
excluded from the build (not referenced by any `mod` declaration anywhere), so they
compile to nothing and gate nothing.
