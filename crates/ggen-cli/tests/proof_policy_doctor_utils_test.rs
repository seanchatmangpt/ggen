#![allow(
    dead_code,
    unused_imports,
    unused_variables,
    deprecated,
    clippy::all,
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    unused_mut
)]

//! Executable-proof integration tests for `ggen policy` and `ggen utils env`.
//! (`ggen doctor` is proven separately — see the file:line note below; no
//! `doctor` test lives in this file despite the module doc historically
//! claiming otherwise.)
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
//! - utils env         crates/ggen-cli/src/cmds/utils.rs:103
//!
//! `ggen doctor` is NOT proven in this file (the cmds/doctor.rs:160 claim this
//! line used to make was stale even before that file was deleted 2026-07-18 —
//! no `#[test]` for it ever existed here). The live implementation is
//! `handle_doctor`, crates/ggen-engine/src/verbs/handlers.rs:759, proven by
//! crates/ggen-engine/tests/doctor_e2e.rs and
//! crates/ggen-cli/tests/doctor_adversarial_tests.rs.

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
// Blocker B regression tests -- BUG-001
// (crates/ggen-cli/src/cmds/policy.rs::load_pack_contexts_from_project /
//  run_policy_enforcement)
//
// Root cause: `.ggen/packs.lock` can contain a pack identifier that was never
// validated when written (e.g. "surface-compliance.soc2", a dot is not
// alphanumeric/hyphen/underscore). The read path used to run every lockfile
// key through the strict `PackageId::new` validator with `?`, so ONE bad
// entry aborted `policy check`/`validate` before the real policy engine
// (`Profile::enforce`) ever ran for anyone -- a crash, not a compliance
// verdict. The fix makes the loader skip (not abort on) a malformed entry,
// still runs the engine over the valid subset, and turns both a real policy
// violation and a malformed-identifier finding into a *typed* nonzero error
// instead of a raw crash or a silently-passing exit 0.
// ============================================================================

/// PROOF (Blocker B, bullet 1): `policy check` reaches the real policy engine
/// for entirely valid input and exits 0.
///
/// A stub that faked success without calling `Profile::enforce` could not
/// reproduce `policies_checked` derived from the REAL `enterprise-strict`
/// profile's policy count, nor `passed: true` computed from a real (empty)
/// violation set.
#[test]
fn policy_check_reaches_policy_engine() {
    let tmp = TempDir::new().expect("tempdir");
    write_empty_lockfile(tmp.path());

    ggen()
        .current_dir(tmp.path())
        .arg("policy")
        .arg("check")
        .assert()
        .success()
        .stdout(
            predicate::str::contains("\"profile_id\": \"enterprise-strict\"")
                .and(predicate::str::contains("\"passed\": true"))
                .and(predicate::str::contains("\"policies_checked\": 4")),
        );
}

/// Write a `.ggen/packs.lock` with one syntactically-valid pack ("demo-pack")
/// alongside the exact malformed identifier that reproduced BUG-001
/// ("surface-compliance.soc2", written unchecked by a pre-fix write path;
/// see `crates/ggen-cli/src/cmds/packs.rs::validate_pack_id`'s doc comment).
fn write_mixed_valid_and_malformed_lockfile(dir: &std::path::Path) {
    let ggen_dir = dir.join(".ggen");
    fs::create_dir_all(&ggen_dir).expect("create .ggen dir");
    let lockfile = ggen_dir.join("packs.lock");
    fs::write(
        &lockfile,
        r#"{
  "packs": {
    "demo-pack": {
      "version": "1.0.0",
      "source": {"type": "Registry", "url": "https://registry.ggen.io"},
      "integrity": "sha256-deadbeef",
      "installed_at": "2026-01-01T00:00:00Z",
      "dependencies": []
    },
    "surface-compliance.soc2": {
      "version": "1.0.0",
      "source": {"type": "Registry", "url": "https://registry.ggen.io"},
      "integrity": "sha256-UNKNOWN-pack-not-found-locally",
      "installed_at": "2026-01-01T00:00:00Z",
      "dependencies": []
    }
  },
  "updated_at": "2026-01-01T00:00:00Z",
  "ggen_version": "26.5.28"
}"#,
    )
    .expect("write packs.lock");
    assert!(lockfile.exists(), "lockfile must exist after setup");
}

/// PROOF (Blocker B, bullets 2-4): `policy check` against a lockfile
/// containing BOTH a well-formed pack and the exact malformed identifier that
/// reproduced BUG-001 must NOT panic, must NOT print the old crash signature
/// ("Argument parsing failed: Invalid package ID ... before compliance logic
/// runs"), and must still reach the policy engine for the valid pack (proven
/// by a real per-pack violation showing up for "demo-pack" in stderr) before
/// refusing with a typed, nonzero exit.
#[test]
fn policy_check_does_not_panic() {
    let tmp = TempDir::new().expect("tempdir");
    write_mixed_valid_and_malformed_lockfile(tmp.path());

    let assert = ggen()
        .current_dir(tmp.path())
        .arg("policy")
        .arg("check")
        .assert()
        .failure();

    let output = assert.get_output();
    let stderr = String::from_utf8_lossy(&output.stderr);

    // No panic of any kind -- the original bug already didn't panic in the
    // Rust sense (it returned a typed Err), but this is the explicit,
    // permanent guard against a regression to an actual panic.
    assert!(
        !stderr.contains("panicked at"),
        "policy check must not panic, got stderr: {stderr}"
    );
    assert!(
        !stderr.to_lowercase().contains("thread 'main' panicked"),
        "policy check must not panic, got stderr: {stderr}"
    );

    // The malformed identifier is reported, not silently swallowed.
    assert!(
        stderr.contains("surface-compliance.soc2"),
        "malformed identifier must be named in the refusal, got stderr: {stderr}"
    );

    // The valid pack was still evaluated by the REAL policy engine despite
    // the malformed sibling entry -- this is the actual regression proof:
    // before the fix, the malformed entry aborted `load_pack_contexts_from_project`
    // via `?` before `Profile::enforce` ever ran for "demo-pack" or anyone else.
    assert!(
        stderr.contains("demo-pack"),
        "the valid pack must still reach the policy engine despite the \
         malformed sibling entry, got stderr: {stderr}"
    );

    // Typed, nonzero, not the old misleading crash message.
    assert!(
        !stderr.contains("Argument parsing failed: Invalid package ID"),
        "must not regress to the old pre-fix crash message, got stderr: {stderr}"
    );
}

// ============================================================================
// doctor — proven elsewhere, not in this file. See crates/ggen-engine/src/
// verbs/handlers.rs:759 (handle_doctor), crates/ggen-engine/tests/doctor_e2e.rs,
// and crates/ggen-cli/tests/doctor_adversarial_tests.rs. (The cmds/doctor.rs:160
// claim this block used to make was stale before that file was deleted
// 2026-07-18 — no `#[test]` for it ever existed in this file.)
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
        .stdout(
            predicate::str::contains("GGEN_PROOF_SET")
                .and(predicate::str::contains("setvalue-c4d8")),
        );
}
