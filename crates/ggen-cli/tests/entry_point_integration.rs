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
//! Chicago TDD Integration Tests for Entry Point Auto-Discovery
//!
//! Tests REAL CLI execution with actual command discovery and routing.
//! No mocking of the CLI framework - uses assert_cmd to spawn real processes.

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


#[test]
fn test_cli_binary_exists_and_runs() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::new(ggen_bin());

    // WHEN: Running with --version
    // THEN: Should display version and exit successfully
    cmd.arg("--version")
        .assert()
        .success()
        .stdout(predicate::str::contains("ggen"));
}

#[test]
#[ignore = "ggen obsolete subcommands removed; CLI consolidated to sync (v26_5_19+)"]
fn test_help_displays_auto_discovered_commands() {
    // GIVEN: A compiled ggen binary with auto-discovery
    let mut cmd = Command::new(ggen_bin());

    // WHEN: Running with --help
    let output = cmd.arg("--help").assert().success();

    // THEN: Should display all auto-discovered noun-verb commands
    output
        .stdout(predicate::str::contains("ai"))
        .stdout(predicate::str::contains("audit"))
        .stdout(predicate::str::contains("ci"))
        .stdout(predicate::str::contains("doctor"))
        .stdout(predicate::str::contains("graph"))
        .stdout(predicate::str::contains("hook"))
        .stdout(predicate::str::contains("lifecycle"))
        .stdout(predicate::str::contains("market"))
        .stdout(predicate::str::contains("project"))
        .stdout(predicate::str::contains("shell"))
        .stdout(predicate::str::contains("template"));
}

#[test]
#[ignore = "ggen doctor help text updated; CLI consolidated to sync (v26_5_19+)"]
fn test_doctor_command_routes_correctly() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::new(ggen_bin());

    // WHEN: Running doctor command with --help
    let output = cmd.arg("doctor").arg("--help").assert().success();

    // THEN: Should show doctor-specific help
    output.stdout(predicate::str::contains("Check system prerequisites"));
}

#[test]
#[ignore = "ggen template subcommand removed; CLI consolidated to sync (v26_5_19+)"]
fn test_template_command_routes_correctly() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::new(ggen_bin());

    // WHEN: Running template command with --help
    let output = cmd.arg("template").arg("--help").assert().success();

    // THEN: Should show template-specific help
    output.stdout(predicate::str::contains("Template management"));
}

#[test]
#[ignore = "ggen market subcommand removed; CLI consolidated to sync (v26_5_19+)"]
fn test_market_command_routes_correctly() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::new(ggen_bin());

    // WHEN: Running market command with --help
    let output = cmd.arg("market").arg("--help").assert().success();

    // THEN: Should show market-specific help
    output.stdout(predicate::str::contains("Marketplace operations"));
}

#[test]
fn test_invalid_command_shows_error() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::new(ggen_bin());

    // WHEN: Running with an invalid command
    let output = cmd.arg("nonexistent").assert().failure();

    // THEN: Should show error message
    output.stderr(predicate::str::contains("error"));
}

#[test]
#[ignore = "ggen global flags updated; CLI consolidated to sync (v26_5_19+)"]
fn test_global_flags_work_before_command() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::new(ggen_bin());

    // WHEN: Running with global --debug flag with value before command
    // THEN: Should accept global flags in correct position
    cmd.arg("--debug")
        .arg("true")
        .arg("doctor")
        .arg("--help")
        .assert()
        .success();
}

#[test]
#[ignore = "ggen otel flags updated; CLI consolidated to sync (v26_5_19+)"]
fn test_otel_flags_are_recognized() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::new(ggen_bin());

    // WHEN: Running with --help to see all flags
    let output = cmd.arg("--help").assert().success();

    // THEN: Should show OpenTelemetry flags
    output
        .stdout(predicate::str::contains("--enable-otel"))
        .stdout(predicate::str::contains("--otel-endpoint"));
}

#[test]
#[ignore = "ggen config flag removed; CLI consolidated to sync (v26_5_19+)"]
fn test_config_flag_is_recognized() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::new(ggen_bin());

    // WHEN: Running with --help
    let output = cmd.arg("--help").assert().success();

    // THEN: Should show config flag
    output.stdout(predicate::str::contains("--config"));
}

#[test]
#[ignore = "ggen manifest-path flag renamed to manifest; CLI consolidated to sync (v26_5_19+)"]
fn test_manifest_path_flag_is_recognized() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::new(ggen_bin());

    // WHEN: Running with --help
    let output = cmd.arg("--help").assert().success();

    // THEN: Should show manifest-path flag
    output.stdout(predicate::str::contains("--manifest-path"));
}

#[test]
fn test_commands_execute_with_real_binary() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::new(ggen_bin());

    // WHEN: Running doctor command (should succeed even without full environment)
    // THEN: Should execute and return with exit code (may fail checks, but should run)
    cmd.arg("doctor").assert().code(predicate::in_iter([0, 1]));
}

#[test]
#[ignore = "ggen help-me subcommand removed; CLI consolidated to sync (v26_5_19+)"]
fn test_help_progressive_command_exists() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::new(ggen_bin());

    // WHEN: Running help-me command with --help
    let output = cmd.arg("help-me").arg("--help").assert().success();

    // THEN: Should show progressive help
    output.stdout(predicate::str::contains("personalized help"));
}

#[test]
#[ignore = "ggen auto-discovery commands list updated; CLI consolidated to sync (v26_5_19+)"]
fn test_auto_discovery_finds_all_command_modules() {
    // GIVEN: A compiled ggen binary with auto-discovery
    let mut cmd = Command::new(ggen_bin());

    // WHEN: Running --help
    let output = cmd.arg("--help").assert().success();
    let stdout = String::from_utf8(output.get_output().stdout.clone()).unwrap();

    // THEN: Should discover and list all 12 commands
    let command_count = [
        "ai",
        "audit",
        "ci",
        "doctor",
        "graph",
        "help-me",
        "hook",
        "lifecycle",
        "market",
        "project",
        "shell",
        "template",
    ]
    .iter()
    .filter(|&cmd_name| stdout.contains(cmd_name))
    .count();

    assert!(
        command_count >= 10,
        "Expected at least 10 commands, found {}",
        command_count
    );
}
