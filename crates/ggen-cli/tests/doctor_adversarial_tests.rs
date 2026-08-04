//! ARCHIVED (2026-08-03): these tests exercised `doctor config`, `doctor ontology`, and
//! `doctor security` -- three sabotage-detection subcommands (missing-file checks, a
//! `.specify/ontologies/main.ttl` convention, `.env`-secret scanning, each with specific
//! `"passed":false"`/remediation-string JSON output) that no longer exist anywhere in the
//! real `doctor` noun.
//!
//! Confirmed live (2026-08-03) against the current binary: `ggen doctor --help` lists only
//! `inspect`, `run`, `domain` -- `ggen doctor config`/`ontology`/`security` all fail with
//! `error: unrecognized subcommand`. This is not a rename: `doctor run` (lockfile/pack
//! drift, orphaned artifacts, receipt-vs-disk staleness) and `doctor inspect` (admissible-
//! work program diagnosis) check entirely different things than config/ontology-file
//! existence or secret-scanning -- there is no current equivalent to point these tests at,
//! so rewriting them to call a current subcommand would assert something these tests were
//! never about. The real, current doctor surface already has its own passing coverage: see
//! `crates/ggen-engine/tests/doctor_e2e.rs` (5/5 passing as of this archival, exercising
//! `doctor run`'s actual lockfile-drift/orphaned-artifact/receipt-staleness checks with real
//! sabotage fixtures, the same testing style this file used).
//!
//! `docs/aps/claims.toml`'s `cli.doctor` claim, which used to cite this file as its
//! falsifier, was repointed at `doctor_e2e.rs` in the same pass that archived this file.
//!
//! If config/ontology/`.env`-secret diagnostics are wanted again, they would need to be
//! built as real `doctor` subcommands first -- restoring this file's assertions without
//! that implementation would just recreate the removed-subcommand failures this archival
//! fixes.
