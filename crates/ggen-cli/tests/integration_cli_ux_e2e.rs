#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]

//! End-to-end integration tests for CLI UX and error handling
//!
//! **Chicago TDD Principles**:
//! - REAL CLI process execution
//! - REAL error condition testing
//! - REAL help output verification
//! - REAL state verification for UX elements
//! - NO mocking of CLI framework
//!
//! **Critical User Workflows (80/20)**:
//! 1. Help system (--help, help-me progressive)
//! 2. Error messages and recovery
//! 3. Invalid command handling
//! 4. Doctor command (system checks)
//! 5. Version and info commands

use assert_cmd::Command;
use predicates::prelude::*;

/// Resolve the real `ggen` binary the same way `cli_boundary.rs::ggen_bin`
/// does: `CARGO_BIN_EXE_ggen` (set by `cargo test -p ggen-cli-lib`, never by
/// `-p ggen-engine`/`-p ggen-cli` — the root package is `autobins = false`),
/// then the workspace `target/{debug,release}/ggen`, then `PATH`. Panics
/// (loudly) if no candidate resolves, so failure happens at binary
/// resolution, not at first spawn.
fn ggen_bin() -> std::path::PathBuf {
    if let Ok(path) = std::env::var("CARGO_BIN_EXE_ggen") {
        let p = std::path::PathBuf::from(path);
        if p.exists() {
            return p;
        }
    }

    let target_root = std::env::var_os("CARGO_TARGET_DIR")
        .map(std::path::PathBuf::from)
        .or_else(|| {
            let manifest_dir =
                std::env::var_os("CARGO_MANIFEST_DIR").map(std::path::PathBuf::from)?;
            let mut dir: &std::path::Path = manifest_dir.as_path();
            loop {
                if dir.join("Cargo.lock").exists() {
                    return Some(dir.join("target"));
                }
                match dir.parent() {
                    Some(p) => dir = p,
                    None => return None,
                }
            }
        });

    if let Some(target) = target_root {
        for profile in &["debug", "release"] {
            let candidate = target.join(profile).join("ggen");
            if candidate.is_file() {
                return candidate;
            }
            let candidate_exe = target.join(profile).join("ggen.exe");
            if candidate_exe.is_file() {
                return candidate_exe;
            }
        }
    }

    panic!(
        "could not resolve the `ggen` binary: CARGO_BIN_EXE_ggen unset and no \
         target/debug/ggen found; build it with `cargo build -p ggen-cli-lib --bin ggen`"
    );
}

/// Helper to create ggen command
fn ggen() -> Command {
    Command::new(ggen_bin())
}

#[test]
fn test_cli_help_main() {
    // Chicago TDD: Verify main help output state.
    // Re-pinned 2026-11-08 (lane V8-GGEN-R1): template/market/project nouns
    // removed from the surface; live nouns are pack/sync/doctor/graph.
    ggen()
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("ggen"))
        .stdout(predicate::str::contains("pack"))
        .stdout(predicate::str::contains("sync"))
        .stdout(predicate::str::contains("doctor"))
        .stdout(predicate::str::contains("graph"));
}

#[test]
fn test_cli_help_short_flag() {
    // Chicago TDD: Verify -h flag works
    ggen()
        .arg("-h")
        .assert()
        .success()
        .stdout(predicate::str::contains("Usage").or(predicate::str::contains("ggen")));
}

#[test]
fn test_cli_version() {
    // Chicago TDD: Verify version output state
    ggen()
        .arg("--version")
        .assert()
        .success()
        .stdout(predicate::str::contains("ggen").or(predicate::str::contains(".")));
}

#[test]
fn test_cli_version_short_flag() {
    // Chicago TDD: Verify -V flag works
    ggen()
        .arg("-V")
        .assert()
        .success()
        .stdout(predicate::str::contains("ggen").or(predicate::str::contains(".")));
}

#[test]
fn test_cli_invalid_command() {
    // Chicago TDD: Verify error state for invalid commands
    ggen()
        .arg("nonexistent-command")
        .assert()
        .failure()
        .stderr(predicate::str::contains("error").or(predicate::str::contains("invalid")));
}

#[test]
fn test_cli_invalid_flag() {
    // Chicago TDD: Verify error state for invalid flags
    ggen()
        .arg("--nonexistent-flag")
        .assert()
        .failure()
        .stderr(predicate::str::contains("error").or(predicate::str::contains("unexpected")));
}

#[test]
fn test_cli_missing_required_arg() {
    // Chicago TDD: Verify error state for missing arguments.
    // Re-pinned 2026-11-08: `template show` noun removed; `pack add` without
    // PACK_ID is the live missing-required-arg leg (witnessed clap
    // "required arguments were not provided" + rc=1).
    ggen()
        .arg("pack")
        .arg("add")
        .assert()
        .failure()
        .stderr(predicate::str::contains("required").or(predicate::str::contains("error")));
}

#[test]
fn test_doctor_basic_check() {
    // Chicago TDD: Verify doctor command runs system checks.
    // Re-pinned 2026-11-08: doctor operates on the ggen project in the
    // current directory and requires a ggen.toml there, so it runs against
    // the workspace root (real manifest, witnessed rc=0 with
    // "healthy": true after F1 was fixed in acc295ec6, CLIUX-CWD lane).
    // A temp-dir manifest is insufficient (FM-CONFIG-001 schema requires the
    // full ontology block), so the real repo root IS the fixture.
    let workspace_root = std::path::Path::new(&std::env::var("CARGO_MANIFEST_DIR").unwrap())
        .ancestors()
        .nth(2)
        .expect("workspace root two levels above crates/ggen-cli")
        .to_path_buf();
    ggen()
        .arg("doctor")
        .current_dir(&workspace_root)
        .assert()
        .success()
        .stdout(predicate::str::contains("healthy").or(predicate::str::contains("checks")));
}

// DELETED 2026-11-08 (lane V8-GGEN-R1): test_doctor_verbose_output — the
// `doctor --verbose` flag was removed from the surface (witnessed: clap
// "unexpected argument '--verbose'", only `doctor run|inspect|domain` remain)
// with no verbose equivalent to re-pin to. Loud absence registered in
// docs/jira/v26.11.8/receipts/ggen-e2e-repair-survey.md.

#[test]
fn test_help_progressive_basic() {
    // Chicago TDD: Verify help entry point.
    // Re-pinned 2026-11-08: `help-me` progressive-help subcommand removed;
    // clap's built-in `ggen help` is the live equivalent (rc=0, usage text).
    ggen()
        .arg("help")
        .assert()
        .success()
        .stdout(predicate::str::contains("Usage").or(predicate::str::contains("help")));
}

// DELETED 2026-11-08 (lane V8-GGEN-R1): test_help_progressive_with_level and
// test_help_progressive_topic — the `help-me --level/--topic`
// progressive-help filtering capability was removed outright with the
// `help-me` subcommand; `ggen help` accepts only subcommand names and has no
// level/topic equivalent to re-pin to. Loud absence registered in
// docs/jira/v26.11.8/receipts/ggen-e2e-repair-survey.md.

/// Derive the set of top-level nouns from `ggen --help` itself, rather than
/// hardcoding a list that goes stale every time a noun is added or removed.
/// Subcommand name lines in clap help are indented exactly two spaces; wrapped
/// description lines are indented deeper, so they are skipped.
fn discovered_nouns() -> Vec<String> {
    let output = ggen().arg("--help").output().expect("run ggen --help");
    let stdout = String::from_utf8_lossy(&output.stdout);
    let mut nouns = Vec::new();
    let mut in_commands = false;
    for line in stdout.lines() {
        if line.trim() == "Commands:" {
            in_commands = true;
            continue;
        }
        if !in_commands {
            continue;
        }
        if line.trim().is_empty() {
            break; // the Commands section ends at the first blank line
        }
        let bytes = line.as_bytes();
        let is_name_line = bytes.len() >= 3 && &bytes[0..2] == b"  " && bytes[2] != b' ';
        if !is_name_line {
            continue; // wrapped description continuation line
        }
        if let Some(name) = line.split_whitespace().next() {
            if name != "help" {
                nouns.push(name.to_string());
            }
        }
    }
    nouns
}

#[test]
fn test_all_nouns_have_help() {
    // Chicago TDD: every noun the CLI advertises must have working help.
    let nouns = discovered_nouns();
    assert!(
        !nouns.is_empty(),
        "ggen --help should advertise at least one noun"
    );
    for noun in &nouns {
        ggen()
            .arg(noun)
            .arg("--help")
            .assert()
            .success()
            .stdout(predicate::str::contains(noun.as_str()).or(predicate::str::contains("Usage")));
    }
}

#[test]
fn test_error_suggests_help() {
    // Chicago TDD: Verify errors guide users to help
    let output = ggen().arg("invalid-command").output().unwrap();

    let stderr = String::from_utf8_lossy(&output.stderr);

    // Error message should be helpful
    assert!(
        stderr.contains("help")
            || stderr.contains("--help")
            || stderr.contains("usage")
            || stderr.contains("error"),
        "Error should suggest help: {}",
        stderr
    );
}

#[test]
fn test_cli_no_args_shows_help() {
    // Chicago TDD: Verify running without args shows help
    let output = ggen().output().unwrap();

    // Should show help or error with usage
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        stdout.contains("Usage")
            || stdout.contains("help")
            || stderr.contains("Usage")
            || stderr.contains("help"),
        "Should show usage information"
    );
}

#[test]
fn test_cli_subcommand_no_verb() {
    // Chicago TDD: bare noun behavior.
    // Re-pinned 2026-11-08: bare nouns on the current surface exit 0 and
    // print usage/help (witnessed rc=0 for pack/sync/graph/ontology/packs) —
    // the old premise ("noun without verb fails") is inverted.
    ggen()
        .arg("pack")
        .assert()
        .success()
        .stdout(predicate::str::contains("Usage").or(predicate::str::contains("help")));
}

#[test]
fn test_cli_help_shows_examples() {
    // Chicago TDD: Verify help includes available verbs.
    // Re-pinned 2026-11-08: `template` noun removed; `pack --help` is the
    // live leg asserting verb names in noun help.
    let output = ggen().arg("pack").arg("--help").output().unwrap();

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Help should be comprehensive (contains verb names at minimum)
    assert!(
        stdout.contains("new") || stdout.contains("list") || stdout.contains("add"),
        "Help should show available verbs"
    );
}

#[test]
fn test_cli_consistent_error_format() {
    // Chicago TDD: Verify errors have consistent format
    let outputs = vec![
        ggen().arg("invalid-command").output().unwrap(),
        ggen().arg("template").arg("invalid-verb").output().unwrap(),
        ggen().arg("--invalid-flag").output().unwrap(),
    ];

    for output in outputs {
        let stderr = String::from_utf8_lossy(&output.stderr);

        // All errors should contain "error" or similar indicator
        assert!(
            stderr.contains("error")
                || stderr.contains("Error")
                || stderr.contains("invalid")
                || stderr.contains("unexpected"),
            "Error messages should be clear: {}",
            stderr
        );
    }
}

#[test]
fn test_cli_exit_codes_correct() {
    // Chicago TDD: Verify exit codes are meaningful
    // Success case
    ggen().arg("--help").assert().success();

    // Failure case
    ggen().arg("invalid-command").assert().failure();
}

#[test]
fn test_cli_handles_ctrl_c_gracefully() {
    // Chicago TDD: Verify signal handling (if applicable)
    // Note: This is hard to test in unit tests, but we can verify
    // that commands don't leave zombie processes

    let output = ggen().arg("--help").output().unwrap();
    assert!(output.status.success());

    // No zombie processes should remain (verified by clean exit)
}

#[test]
fn test_cli_colored_output_flag() {
    // Chicago TDD: Verify color control flags work
    let _ = ggen().arg("--help").arg("--color=never").output();

    // Command should complete successfully
    ggen()
        .arg("--help")
        .arg("--color=always")
        .assert()
        .success();
}

#[test]
fn test_cli_json_output_flag() {
    // Chicago TDD: Verify JSON output flag is respected.
    // Re-pinned 2026-11-08: template/market nouns removed; pack legs are the
    // live JSON surface (`pack list --json` witnessed emitting valid JSON).
    let commands_with_json = vec![
        vec!["pack", "list", "--json"],
        vec!["pack", "search", "test", "--json"],
    ];
    // Note: bare `packs --json` deliberately excluded — bare `packs` prints
    // help text (rc=0), it does not accept a --json flag; asserting valid
    // JSON there would pin a non-existent flag.

    for args in commands_with_json {
        let output = ggen().args(&args).output().unwrap();

        // If command succeeds, JSON should be valid
        if output.status.success() {
            let stdout = String::from_utf8_lossy(&output.stdout);
            if !stdout.trim().is_empty() {
                serde_json::from_str::<serde_json::Value>(&stdout)
                    .expect("JSON output should be valid");
            }
        }
    }
}

#[test]
fn test_cli_performance_help_fast() {
    // Chicago TDD: Verify help works and is fast.
    //
    // Flake fix (2026-10-10): a hard latency assert (`< 1000ms`) failed under
    // multi-lane build load (observed 3.486s) purely from process-launch
    // contention, not a real regression. Pattern chosen: (b) soft-assert +
    // (a) retry-min — the hard functional property (exit 0 + usage text) is
    // always asserted; latency is retried (2s settle, min of 3 runs) and only
    // fails if the best of 3 runs still exceeds the budget on a settled
    // machine.
    const HELP_LATENCY_BUDGET_MS: u128 = 1000;

    // Hard functional property: help exits 0 and contains usage text.
    let first = ggen().arg("--help").output().unwrap();
    assert!(first.status.success(), "--help should exit 0");
    let stdout = String::from_utf8_lossy(&first.stdout);
    assert!(
        stdout.contains("Usage") || stdout.contains("usage"),
        "--help should contain usage text"
    );

    // Soft latency check: min of 3 runs with a settle pause.
    let mut durations = Vec::new();
    for attempt in 0..3 {
        if attempt > 0 {
            std::thread::sleep(std::time::Duration::from_secs(2));
        }
        let start = std::time::Instant::now();
        ggen().arg("--help").assert().success();
        durations.push(start.elapsed());
    }

    let best = *durations.iter().min().unwrap();
    assert!(
        best.as_millis() < HELP_LATENCY_BUDGET_MS,
        "Help should be near-instant (best of 3 runs, budget {}ms): {:?}",
        HELP_LATENCY_BUDGET_MS,
        durations
    );
}

#[test]
fn test_cli_doctor_checks_essentials() {
    // Chicago TDD: Verify doctor checks critical dependencies.
    // Re-pinned 2026-11-08: doctor requires a ggen.toml in cwd, so it runs
    // against the workspace root (real manifest; F1 fixed in acc295ec6,
    // CLIUX-CWD lane). The JSON report names its check set and health.
    let workspace_root = std::path::Path::new(&std::env::var("CARGO_MANIFEST_DIR").unwrap())
        .ancestors()
        .nth(2)
        .expect("workspace root two levels above crates/ggen-cli")
        .to_path_buf();
    let output = ggen()
        .arg("doctor")
        .current_dir(&workspace_root)
        .output()
        .unwrap();

    assert!(output.status.success());

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Doctor should report its check set and overall health
    assert!(
        stdout.contains("checks") || stdout.contains("healthy"),
        "Doctor should report checks: {}",
        stdout
    );
}
