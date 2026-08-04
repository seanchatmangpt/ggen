//! ARCHIVED (2026-08-03): all 8 tests in this file invoked the `ci` noun (`ggen ci
//! validate --workflow ...`), which no longer exists anywhere in the current CLI surface.
//!
//! Confirmed live: `ggen ci validate --workflow nonexistent.yml` fails with `error:
//! unrecognized subcommand 'ci'` (clap even suggests the nearest real noun, `capability`).
//! `ggen --help`'s current noun list has no `ci` entry at all -- this is a real removal
//! from the pre-v26.7.16-CLI-routing-flip design, not a rename, and there is no current
//! GitHub-Actions-workflow-validation command to repoint these 8 tests at.
//!
//! If CI-workflow-file validation is wanted again, it would need to be rebuilt as a real
//! CLI command first -- restoring this file's assertions without that implementation would
//! just recreate the removed-subcommand failures this archival fixes. See also
//! `tests/e2e_github_integration.rs`, which has 8 tests in the same `ci`-noun-removed state
//! (mixed in with other GitHub-integration tests that do still pass).
