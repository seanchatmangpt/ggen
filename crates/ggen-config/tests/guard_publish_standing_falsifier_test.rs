#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
//! F1 fix evidence (red-team audit, category: fail-open, file:
//! `scripts/ci/guard-publish-standing.sh:50`): the guard validated
//! `docs/aps/claims.toml`'s TOML *shape* only (field presence, standing
//! vocabulary, evidence-dict field presence) and never executed a single
//! claim's `falsifier` command in either mode -- reusing the ledger's own
//! ALIVE/BLOCKED vocabulary as its own pass/fail label reads as falsifier
//! verification but was not one. A claim's `standing` could be flipped from
//! BLOCKED to ALIVE, or any `falsifier` replaced with a command that would
//! fail if run, with zero change in guard behavior.
//!
//! This test proves the fix: in full (non-`--schema-only`) mode the guard
//! now actually executes each claim's `falsifier` as a real subprocess and
//! fails when a claim recorded `standing = "ALIVE"` ("falsifier ran and
//! passed at the recorded evidence coordinate", per `docs/aps/claims.toml`'s
//! own header) has a falsifier that does not currently pass.
//! `--schema-only` mode (the one `just pre-commit` actually calls, via `just
//! guard-claims-schema`) is deliberately left alone -- it is documented as a
//! fast, structure-only check, and this test also pins down that it still
//! never executes any falsifier.
//!
//! Chicago TDD: a real `bash` subprocess running the real, on-disk guard
//! script against a real fabricated `docs/aps/claims.toml` in a temp
//! directory, with real, trivial (`exit 0` / `exit 1`) falsifier commands as
//! the collaborator under test. No mocks.

use std::path::{Path, PathBuf};
use std::process::{Command, Output};

fn workspace_root() -> PathBuf {
    // CARGO_MANIFEST_DIR = <root>/crates/ggen-config
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("crates/")
        .parent()
        .expect("workspace root")
        .to_path_buf()
}

fn guard_script_path() -> PathBuf {
    workspace_root().join("scripts/ci/guard-publish-standing.sh")
}

/// Writes a minimal, schema-valid `docs/aps/claims.toml` into `dir` with
/// exactly one claim, whose `standing` and `falsifier` are controlled by the
/// caller -- everything else is fixed, valid boilerplate so the pre-existing
/// shape checks (id/standing/falsifier/evidence present, standing in
/// vocabulary, evidence dict fields present) never fire, and only the
/// falsifier-execution behavior under test is exercised. `gates = []` (not
/// `["publish"]`) so the BLOCKED-needs-`exception_admitted_by` check never
/// fires either, for the same reason.
fn write_fixture_ledger(dir: &Path, standing: &str, falsifier: &str) {
    let docs_aps = dir.join("docs/aps");
    std::fs::create_dir_all(&docs_aps).expect("mkdir docs/aps");
    let toml = format!(
        r#"schema = "ggen-aps-claims.v1"
toolchain = "test"

[[claims]]
id = "fixture.claim"
standing = "{standing}"
gates = []
falsifier = "{falsifier}"
evidence = {{ commit = "deadbeef1", date = "2026-08-03", method = "fixture" }}
"#
    );
    std::fs::write(docs_aps.join("claims.toml"), toml).expect("write fixture claims.toml");
}

fn run_guard(dir: &Path, mode: Option<&str>) -> Output {
    let mut cmd = Command::new("bash");
    cmd.arg(guard_script_path()).current_dir(dir);
    if let Some(m) = mode {
        cmd.arg(m);
    }
    cmd.output().expect("run guard-publish-standing.sh")
}

#[test]
fn full_mode_passes_when_alive_claims_falsifier_actually_passes() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    write_fixture_ledger(tmp.path(), "ALIVE", "exit 0");

    let out = run_guard(tmp.path(), None);
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        out.status.success(),
        "guard must pass when the ALIVE claim's falsifier really exits 0.\nstdout: {stdout}\nstderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        stdout.contains("RAN: fixture.claim: falsifier exit=0"),
        "guard must report that it actually ran the falsifier (real subprocess execution, not \
         a decorative TOML-shape check); got stdout: {stdout}"
    );
}

/// The core F1 regression witness: a claim recorded `standing = "ALIVE"`
/// whose falsifier does NOT currently pass must fail the guard in full
/// mode. Before the fix, this exact fixture passed (exit 0) unconditionally
/// -- the guard never ran the falsifier at all.
#[test]
fn full_mode_fails_when_alive_claims_falsifier_actually_fails() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    write_fixture_ledger(tmp.path(), "ALIVE", "exit 1");

    let out = run_guard(tmp.path(), None);
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        !out.status.success(),
        "guard must fail (non-zero exit) when an ALIVE claim's falsifier really fails; a \
         guard that passes here is exactly the F1 fail-open finding.\nstdout: {stdout}"
    );
    assert!(
        stdout.contains("BUILD_BROKEN") && stdout.contains("fixture.claim"),
        "guard must name the offending claim in a BUILD_BROKEN line; got stdout: {stdout}"
    );
    assert!(
        stdout.contains("RAN: fixture.claim: falsifier exit=1"),
        "guard must show it actually executed the falsifier (exit=1), not merely trusted the \
         TOML label; got stdout: {stdout}"
    );
}

/// BLOCKED claims make no passing assertion -- a failing falsifier there is
/// expected and must not fail the guard -- but it must still be really
/// executed (visible via the RAN: line), not silently skipped.
#[test]
fn full_mode_tolerates_blocked_claims_failing_falsifier_but_still_runs_it() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    write_fixture_ledger(tmp.path(), "BLOCKED", "exit 1");

    let out = run_guard(tmp.path(), None);
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        out.status.success(),
        "a BLOCKED claim's failing falsifier must not fail the guard (BLOCKED admits \
         failure).\nstdout: {stdout}\nstderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        stdout.contains("RAN: fixture.claim: falsifier exit=1"),
        "BLOCKED claim's falsifier must still be really executed (for real signal), not \
         skipped; got stdout: {stdout}"
    );
}

/// `--schema-only` mode (the fast path `just pre-commit` actually calls, via
/// `just guard-claims-schema`) is documented and intended to stay a
/// structure-only check -- it must NOT execute any falsifier, even for an
/// ALIVE claim whose falsifier would fail. This pins down the fast-path
/// behavior so a future change doesn't silently slow down every commit.
#[test]
fn schema_only_mode_never_executes_any_falsifier() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    write_fixture_ledger(tmp.path(), "ALIVE", "exit 1");

    let out = run_guard(tmp.path(), Some("--schema-only"));
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        out.status.success(),
        "schema-only mode must pass on a schema-valid ledger regardless of falsifier outcome \
         (structure-only by design).\nstdout: {stdout}\nstderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        !stdout.contains("RAN:"),
        "schema-only mode must never execute a falsifier (no RAN: line expected); got stdout: \
         {stdout}"
    );
}
