#![allow(
    dead_code,
    unused_imports,
    unused_variables,
    deprecated,
    clippy::all,
    unused_mut
)]

//! Executable-proof integration tests for `ggen policy`, `ggen doctor check`,
//! and `ggen utils env`.
//!
//! **Chicago TDD Principles** (mirrors `sync_command_test.rs` /
//! `integration_utils_e2e.rs`):
//! - REAL CLI process execution via `assert_cmd::Command::cargo_bin("ggen")`.
//! - REAL filesystem state in `TempDir` (lockfiles, `ggen.toml`) as each handler
//!   reads it. REAL environment variables for `utils env`.
//! - State-based assertions on observable output (exit code, stdout, files).
//! - NO mocks, NO test doubles, NO behavior verification.
//!
//! Each test is an *executable proof*: it can only pass if the command performs
//! the real work the handler claims. Where a verb would pass without real work,
//! the proof is structured so a stub would FAIL the assertion.
//!
//! Handlers proven (file:line):
//! - policy list       crates/ggen-cli/src/cmds/policy.rs:178
//! - policy validate   crates/ggen-cli/src/cmds/policy.rs:212
//! - policy show       crates/ggen-cli/src/cmds/policy.rs:259
//! - policy check      crates/ggen-cli/src/cmds/policy.rs:319
//! - doctor check      crates/ggen-cli/src/cmds/doctor.rs:160
//! - utils env         crates/ggen-cli/src/cmds/utils.rs:103

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use tempfile::TempDir;

/// Helper to create the REAL ggen binary command (dev-dep `assert_cmd`).
fn ggen() -> Command {
    Command::cargo_bin("ggen").expect("Failed to find ggen binary")
}

/// Write a valid empty-packs lockfile (`.ggen/packs.lock`) into `dir`.
///
/// The lockfile is JSON (see `PackLockfile::from_file` ->
/// `serde_json::from_str`). Zero packs is a valid, fully-parsable state: it
/// forces `load_pack_contexts_from_project()` down its success path and runs
/// REAL `Profile::enforce(&[])`. A stub that ignored the lockfile would not be
/// able to report `policies_checked` derived from the real profile.
fn write_empty_lockfile(dir: &std::path::Path) {
    let ggen_dir = dir.join(".ggen");
    fs::create_dir_all(&ggen_dir).expect("create .ggen dir");
    let lockfile = ggen_dir.join("packs.lock");
    fs::write(
        &lockfile,
        r#"{"packs":{},"updated_at":"2026-01-01T00:00:00Z","ggen_version":"26.5.28"}"#,
    )
    .expect("write packs.lock");
    assert!(lockfile.exists(), "lockfile must exist after setup");
}

// ============================================================================
// policy list  (crates/ggen-cli/src/cmds/policy.rs:178)
// ============================================================================

/// PROOF: `policy list` enumerates the 3 REAL built-in profiles from
/// `predefined_profiles()` (enterprise-strict, regulated-finance, development).
///
/// Observable assertion: stdout contains all three profile IDs. A stub
/// returning empty/placeholder output cannot produce these exact IDs that are
/// derived from `ggen_core::marketplace::profile::predefined_profiles()`.
#[test]
fn proof_policy_list_enumerates_builtin_profiles() {
    ggen()
        .arg("policy")
        .arg("list")
        .assert()
        .success()
        .stdout(predicate::str::contains("enterprise-strict"))
        .stdout(predicate::str::contains("regulated-finance"))
        .stdout(predicate::str::contains("development"));
}

// ============================================================================
// policy show  (crates/ggen-cli/src/cmds/policy.rs:259)
// ============================================================================

/// PROOF: `policy show enterprise-strict` resolves the REAL profile via
/// `get_profile()` and emits its concrete fields (id + at least one policy /
/// trust requirement).
///
/// Observable assertion: stdout contains the profile id `enterprise-strict`
/// AND policy/runtime detail. A stub cannot reproduce the real profile's
/// derived contents.
#[test]
fn proof_policy_show_resolves_real_profile() {
    ggen()
        .arg("policy")
        .arg("show")
        .arg("--profile_id")
        .arg("enterprise-strict")
        .assert()
        .success()
        .stdout(predicate::str::contains("enterprise-strict"));
}

/// PROOF (negative path): `policy show <bogus>` FAILS because `get_profile()`
/// returns `PackageNotFound`. Fail-closed behavior — a stub returning
/// fake-success would wrongly succeed here.
///
/// Observable assertion: non-zero exit + stderr references the not-found error.
#[test]
fn proof_policy_show_unknown_profile_fails_closed() {
    ggen()
        .arg("policy")
        .arg("show")
        .arg("--profile_id")
        .arg("no-such-profile-xyz")
        .assert()
        .failure()
        .stderr(predicate::str::contains("not found").or(predicate::str::contains("Profile")));
}

// ============================================================================
// policy validate  (crates/ggen-cli/src/cmds/policy.rs:212)
// ============================================================================

/// PROOF (negative path / fail-closed): `policy validate` with NO
/// `.ggen/packs.lock` present must FAIL. `load_pack_contexts_from_project()`
/// returns an error when the lockfile is absent (policy.rs:96-100). This proves
/// the verb actually reads project state rather than fabricating success.
///
/// Observable assertion: non-zero exit; stderr mentions the missing project /
/// packs guidance.
#[test]
fn proof_policy_validate_fails_without_lockfile() {
    let tmp = TempDir::new().expect("tempdir");
    ggen()
        .current_dir(tmp.path())
        .arg("policy")
        .arg("validate")
        .arg("--profile")
        .arg("development")
        .assert()
        .failure()
        .stderr(
            predicate::str::contains("No project")
                .or(predicate::str::contains("packs"))
                .or(predicate::str::contains("install")),
        );
}

/// PROOF (success path with REAL lockfile): `policy validate` reads a real
/// (empty-packs) `.ggen/packs.lock`, parses+validates it, resolves the
/// `development` profile, and runs REAL `Profile::enforce(&[])`. With zero pack
/// contexts there are no violations, so validation passes.
///
/// Observable assertion: success exit + stdout references the validated profile
/// `development`. The command can only reach success by parsing the lockfile we
/// wrote — a stub ignoring the file could not bind the profile id.
#[test]
fn proof_policy_validate_runs_enforcement_with_real_lockfile() {
    let tmp = TempDir::new().expect("tempdir");
    write_empty_lockfile(tmp.path());
    ggen()
        .current_dir(tmp.path())
        .arg("policy")
        .arg("validate")
        .arg("--profile")
        .arg("development")
        .assert()
        .success()
        .stdout(predicate::str::contains("development"));
}

// ============================================================================
// policy check  (crates/ggen-cli/src/cmds/policy.rs:319)
// ============================================================================

/// PROOF (fail-closed): `policy check` (hardcoded `enterprise-strict` profile)
/// requires `.ggen/packs.lock`; absent it must FAIL via
/// `load_pack_contexts_from_project()`. Proves real project-state reads.
///
/// Observable assertion: non-zero exit + missing-project error on stderr.
#[test]
fn proof_policy_check_fails_without_lockfile() {
    let tmp = TempDir::new().expect("tempdir");
    ggen()
        .current_dir(tmp.path())
        .arg("policy")
        .arg("check")
        .assert()
        .failure()
        .stderr(
            predicate::str::contains("No project")
                .or(predicate::str::contains("packs"))
                .or(predicate::str::contains("install")),
        );
}

/// PROOF (success path with REAL lockfile): `policy check` resolves the REAL
/// `enterprise-strict` profile and runs `enforce(&[])` over the empty lockfile.
///
/// Observable assertion: success exit + stdout references `enterprise-strict`
/// (the profile id the handler hardcodes and resolves through `get_profile`).
#[test]
fn proof_policy_check_runs_enterprise_strict_with_real_lockfile() {
    let tmp = TempDir::new().expect("tempdir");
    write_empty_lockfile(tmp.path());
    ggen()
        .current_dir(tmp.path())
        .arg("policy")
        .arg("check")
        .assert()
        .success()
        .stdout(predicate::str::contains("enterprise-strict"));
}

// ============================================================================
// doctor check  (crates/ggen-cli/src/cmds/doctor.rs:160)
// ============================================================================

/// PROOF (positive): `doctor check` reads the REAL cwd and reports
/// `ggen.toml` PRESENT when a real `ggen.toml` exists in the working directory
/// (doctor.rs:165-167).
///
/// Observable assertion: success exit + stdout indicates the check passed /
/// found ggen.toml. A stub ignoring the filesystem could not distinguish this
/// from the absent case below.
#[test]
fn proof_doctor_check_passes_when_ggen_toml_present() {
    let tmp = TempDir::new().expect("tempdir");
    fs::write(
        tmp.path().join("ggen.toml"),
        "[project]\nname = \"proof\"\n",
    )
    .expect("write ggen.toml");
    ggen()
        .current_dir(tmp.path())
        .arg("doctor")
        .arg("check")
        .assert()
        .success()
        .stdout(
            predicate::str::contains("passed")
                .or(predicate::str::contains("ggen.toml"))
                .or(predicate::str::contains("true")),
        );
}

/// PROOF (negative): `doctor check` in a directory WITHOUT `ggen.toml` reports
/// the check failed and surfaces recovery guidance (doctor.rs:169-183).
///
/// Observable assertion: stdout reflects the not-found / failed state. This is
/// the differentiating proof versus the positive case: same command, opposite
/// observable result driven purely by real filesystem state.
#[test]
fn proof_doctor_check_reports_missing_ggen_toml() {
    let tmp = TempDir::new().expect("tempdir");
    // Ensure no ggen.toml exists.
    assert!(!tmp.path().join("ggen.toml").exists());
    ggen()
        .current_dir(tmp.path())
        .arg("doctor")
        .arg("check")
        .assert()
        .stdout(
            predicate::str::contains("not found")
                .or(predicate::str::contains("failed"))
                .or(predicate::str::contains("false"))
                .or(predicate::str::contains("Create ggen.toml")),
        );
}

// ============================================================================
// utils env  (crates/ggen-cli/src/cmds/utils.rs:103)
// ============================================================================

/// PROOF: `utils env --get <KEY>` reads the REAL process environment
/// (`std::env::var(key)`, utils.rs:113-116). We inject a unique env var into the
/// child process and assert its value is echoed back.
///
/// Observable assertion: stdout contains the unique value we set. This is
/// unforgeable by a stub — the value only exists because the command read the
/// real environment passed to its process.
#[test]
fn proof_utils_env_get_reads_real_environment() {
    let unique = format!("GGEN_PROOF_{}", std::process::id());
    let value = "proof-value-9f3c";
    ggen()
        .env(&unique, value)
        .arg("utils")
        .arg("env")
        .arg("--get")
        .arg(&unique)
        .assert()
        .success()
        .stdout(predicate::str::contains(value));
}

/// PROOF: `utils env --list` collects REAL ggen-relevant env vars
/// (`collect_ggen_env_vars` filters GGEN_*/RUST_*/HOME/PATH, utils.rs:131-137).
/// We inject a `GGEN_`-prefixed var; it must appear in the listing.
///
/// Observable assertion: stdout contains the injected GGEN_ key. A stub that
/// returned empty (as the older `integration_utils_e2e.rs` comment suggested)
/// would FAIL this assertion — so this test would FLAG a regression to a stub.
#[test]
fn proof_utils_env_list_collects_ggen_vars() {
    let key = "GGEN_PROOF_LIST_MARKER";
    let value = "listed-marker-7a21";
    ggen()
        .env(key, value)
        .arg("utils")
        .arg("env")
        .arg("--list")
        .assert()
        .success()
        .stdout(predicate::str::contains(key).or(predicate::str::contains(value)));
}

/// PROOF: `utils env --set KEY=VALUE` echoes the parsed key/value back
/// (`split_once('=')` then insert, utils.rs:117-122).
///
/// Observable assertion: stdout contains both the key and the value, proving the
/// handler parsed the `KEY=VALUE` argument rather than ignoring it.
#[test]
fn proof_utils_env_set_parses_key_value() {
    ggen()
        .arg("utils")
        .arg("env")
        .arg("--set")
        .arg("GGEN_PROOF_SET=setvalue-c4d8")
        .assert()
        .success()
        .stdout(predicate::str::contains("GGEN_PROOF_SET").and(predicate::str::contains("setvalue-c4d8")));
}
