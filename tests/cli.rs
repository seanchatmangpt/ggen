// UPDATED (2026-08-03): this file used to test CLI nouns from before the v26.7.16 CLI-routing
// flip (`market`, `audit`) plus a hardcoded stale version string. Confirmed live against the
// current binary (`ggen --help`): the real noun list is exactly `init, wizard, packs, pack,
// bblock, sync, agent, telco, maximalism, mod, receipt, policy, vision2030, utils, ontology,
// sbb, capability, graph, law, doctor, help` -- no `market`, `audit`, `ci`, `ai`, `hook`,
// `lifecycle`, `project`, `shell`, or `template` noun exists (this is a real removal, not a
// rename -- clap's "did you mean" even suggests unrelated nouns like `agent`/`capability`).
// Tests whose entire intent depended on a removed noun with no current equivalent verb were
// deleted (see per-function comments below for the specific removed-noun evidence); tests
// whose intent is still valid were rewritten against real current nouns/verbs
// (`pack add`/`packs list --format ...`), following the same style as
// `tests/cli_command_tests.rs` (already fixed earlier in this pass) and
// `crates/ggen-cli/tests/doctor_adversarial_tests.rs` (archival-comment precedent).
#![cfg(feature = "integration")]

use assert_cmd::Command;
use predicates::prelude::*;
// use url::Url; // Not available in test dependencies

// NOTE (2026-08-03): the EnvVarGuard helper and `serial_test::serial` import that used to
// live here were only needed by the GGEN_REGISTRY_URL-dependent tests removed in this pass
// (see the removed test_search_command_* and test_cli_output_formats comments below) -- no
// remaining test in this file mutates shared env state, so both were deleted rather than
// left as unused dead code.

#[test]
fn test_cli_basic() {
    // Confirmed live (2026-08-03): a bare `ggen` invocation with no subcommand now prints
    // help and exits 0 by design -- it no longer fails. This mirrors the same real-behavior
    // change already documented in `tests/cli_command_tests.rs`'s `test_missing_required_arg`
    // ("Bare `ggen pack` (no verb) now prints help and exits 0 by design"); here it's the
    // top-level command with zero args, not a specific noun.
    let mut cmd = Command::cargo_bin("ggen").expect("Calling binary failed");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Commands:"));
}

#[test]
fn test_version() {
    // Assert against the real compiled version (env!("CARGO_PKG_VERSION") is baked in from
    // Cargo.toml at compile time) instead of a hardcoded string, so this can never go stale
    // again the way the old "ggen 1.2.0\n" literal did (real version confirmed live: 26.8.4).
    let expected_version = format!("ggen {}\n", env!("CARGO_PKG_VERSION"));
    let mut cmd = Command::cargo_bin("ggen").expect("Calling binary failed");
    cmd.arg("--version")
        .assert()
        .success()
        .stdout(expected_version);
}

// REMOVED (2026-08-03): test_hazard_exit_code and test_hazard_stdout both called
// `ggen audit hazard scan`. Confirmed live: `audit` is not in the current noun list at all
// (`ggen --help` lists only init/wizard/packs/pack/bblock/sync/agent/telco/maximalism/mod/
// receipt/policy/vision2030/utils/ontology/sbb/capability/graph/law/doctor/help) and
// `grep -rn "hazard"` across `crates/` finds zero CLI-command hits -- only unrelated domain
// ontology files (e.g. `doid.owl`) use the word. `test_hazard_exit_code` was passing only
// vacuously (any unrecognized subcommand fails, which is unrelated to hazard-scan behavior);
// `test_hazard_stdout` was genuinely failing since "Scanning" is never printed for an
// unrecognized subcommand. There is no current CLI surface that does hazard/security
// scanning to repoint these at -- security-adjacent checks now live under `doctor run`
// (lockfile/pack drift, orphaned artifacts, receipt staleness; see
// `crates/ggen-engine/tests/doctor_e2e.rs`), which checks entirely different things than a
// hazard scan would.

#[test]
fn test_cli_help_commands() {
    // Batch test all help commands to reduce process spawning.
    //
    // UPDATED (2026-08-03): the original list (market/ai/audit/ci/graph/hook/lifecycle/
    // project/shell/template) is mostly nouns that no longer exist -- confirmed live via
    // `ggen --help`. `graph` is the one entry that IS still real, but its expected text
    // ("RDF graph operations") doesn't match the real help string either. Replaced with 10
    // nouns confirmed live against the current binary, each paired with a substring taken
    // verbatim from that noun's real `--help` first line.
    let commands = [
        ("graph", "Validate RDF/Turtle ontology graphs"),
        ("doctor", "Universal non-actuating diagnostics"),
        ("sync", "Run the ggen code-generation pipeline"),
        ("receipt", "Inspect and verify sync receipt chains"),
        ("packs", "multi-pack project management"),
        ("agent", "AGI-facing CLI surface"),
        ("capability", "resolve and enable capability surfaces"),
        ("law", "Law-state operations on the project graph"),
        ("ontology", "Embedded and Marketplace Ontology Management"),
        ("init", "Initialize a new ggen project"),
    ];

    for (cmd_name, expected_text) in &commands {
        let mut cmd = Command::cargo_bin("ggen").unwrap();
        cmd.arg(cmd_name).arg("--help");
        cmd.assert()
            .success()
            .stdout(predicate::str::contains(*expected_text));
    }
}

// REMOVED (2026-08-03): test_search_command_basic_usage and test_search_command_with_filters
// both called `ggen market search ...` against a local mock registry wired up via the
// GGEN_REGISTRY_URL env var. Confirmed live: `market` is not in the current noun list at all,
// and `grep -rn "GGEN_REGISTRY_URL"` across `crates/` finds zero hits -- the env var isn't
// read by any current code either, so the whole local-mock-registry-search mechanism these
// tests depended on is gone, not just renamed. The closest current noun, `packs`
// (`show`/`validate`/`list`/`install`), has no `search` verb -- it's lockfile-oriented
// (what's already tracked), not registry-query-oriented (what's discoverable), so there is no
// current equivalent to repoint these at. If registry search is wanted again it would need to
// be rebuilt as a real `packs`/`pack` verb first.

// Individual help tests removed - now batched in test_cli_help_commands

#[test]
fn test_cli_error_handling() {
    // Test invalid command (still valid: unrecognized-subcommand handling is unchanged).
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("invalid-command");
    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("unrecognized subcommand"));

    // Test missing required arguments.
    //
    // UPDATED (2026-08-03): was `market add` (removed noun). Rewritten to `pack add` with no
    // <PACK_NAME> positional, matching the real precedent already established in
    // `tests/cli_command_tests.rs`'s `test_missing_required_arg`. Confirmed live: fails with
    // "error: the following required arguments were not provided:\n  <PACK_NAME>".
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("pack").arg("add");
    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("required"));

    // Test invalid arguments.
    //
    // UPDATED (2026-08-03): was `market search --invalid-flag` (removed noun). Rewritten to
    // `packs list --invalid-flag`, a real current verb given a flag it doesn't accept.
    // Confirmed live: fails with "error: unexpected argument '--invalid-flag' found".
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("packs").arg("list").arg("--invalid-flag");
    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("unexpected argument"));
}

#[test]
fn test_cli_output_formats() {
    // UPDATED (2026-08-03): was `market search --json`/`--detailed` against a local mock
    // registry (removed noun, removed mechanism -- see the removed test_search_command_*
    // comment above). Rewritten against the real global `--format` flag (`ggen --help` lists
    // `--format <format>  [possible values: json, json-pretty, yaml, table, plain, tsv,
    // quiet]`) applied to `packs list`, a real current verb. Run from a fresh empty tempdir so
    // the assertion doesn't depend on this repo's own mutable `.ggen/packs.lock` state
    // (confirmed live: `packs list` is lenient by design and returns an empty-but-well-formed
    // result when no lockfile exists, rather than erroring). This mirrors the same
    // `--format json` pattern already used against `packs` verbs in
    // `crates/ggen-cli/tests/proof_packs_test.rs`.
    let temp = tempfile::tempdir().unwrap();

    // Test JSON output.
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("packs")
        .arg("list")
        .arg("--format")
        .arg("json")
        .current_dir(temp.path());
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("\"total\""));

    // Test table output.
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("packs")
        .arg("list")
        .arg("--format")
        .arg("table")
        .current_dir(temp.path());
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("packs"));
}

// REMOVED (2026-08-03): test_cli_environment_variables set GGEN_TRACE across several levels
// and asserted on `ggen audit hazard scan`'s stdout. Confirmed live: `audit` is not a current
// noun (see the removed test_hazard_* comment above), and `grep -rn "GGEN_TRACE"` across
// `crates/` finds zero hits -- the CLI's real tracing is driven by `RUST_LOG`/`EnvFilter`
// (`crates/ggen-cli/src/telemetry.rs`, `src/lib.rs`), not a `GGEN_TRACE` env var, so this
// test's entire premise (that env var, that command) never had a real target to begin with.
// There is no current command whose observable stdout changes shape per trace level to
// repoint this at -- rebuilding it would mean inventing new behavior, not testing existing
// behavior.
