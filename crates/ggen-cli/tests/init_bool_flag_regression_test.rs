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
//! Regression tests for two `ggen init` bugs (DoD Blockers D+E), both in
//! `crates/ggen-cli/src/cmds/init.rs`:
//!
//! - **Blocker D**: `ggen init --force <malformed-value>` (e.g. `--force garbage`,
//!   `--force ture`) used to be silently coerced to `false` instead of refused.
//!   Root cause: the clap-noun-verb `#[verb]` macro's generated extraction for an
//!   `Option<bool>` parameter is `.and_then(|v| v.parse::<bool>().ok())` --
//!   `.ok()` swallows a parse failure into `None`, and `unwrap_or(false)` then
//!   turned that into a quiet, wrong `false`. Fixed by taking `force`/`skip_hooks`
//!   as raw `Option<String>` and parsing them strictly inside `init()` itself
//!   (`parse_bool_flag`), refusing anything that isn't exactly `"true"`/`"false"`.
//! - **Blocker E**: `ggen init` against an already-initialized directory (without
//!   `--force`) correctly reported `{"status":"error", ...}` on stdout, but the
//!   PROCESS EXIT CODE was still 0 (decorative completion: the payload was
//!   honest, the process lied). Fixed by having the `init()` verb wrapper
//!   promote any `status: "error"` `InitOutput` into a real `Err`, so the process
//!   exit code always agrees with the payload's own status field.
//!
//! **Chicago TDD** (no mocks, no test doubles):
//! - REAL CLI subprocess execution via `assert_cmd::Command::cargo_bin("ggen")`.
//! - REAL filesystem state in a `TempDir`, verified with a REAL recursive
//!   SHA-256 content digest (not just an exit-code check) so a before/after
//!   comparison actually proves "not a single byte changed", not merely "the
//!   files we thought to check are still there".

use assert_cmd::Command;
use serde_json::Value;
use sha2::{Digest, Sha256};
use std::fs;
use std::path::Path;
use tempfile::TempDir;

/// Real `ggen` binary command.
fn ggen() -> Command {
    Command::cargo_bin("ggen").expect("ggen binary not found")
}

/// A real, deterministic content digest of every file under `root` (recursive).
///
/// Hashes `(relative_path, content)` pairs, sorted by relative path, into one
/// SHA-256 so the digest changes if ANY byte of ANY file changes, or if a file
/// is added, removed, or renamed anywhere in the tree -- this is what makes
/// `before == after` a real "the filesystem is byte-for-byte unchanged" proof,
/// not merely "the handful of paths I remembered to `assert!(exists)` are
/// still there".
fn dir_digest(root: &Path) -> String {
    let mut entries: Vec<(String, Vec<u8>)> = Vec::new();
    collect_files(root, root, &mut entries);
    entries.sort_by(|a, b| a.0.cmp(&b.0));

    let mut hasher = Sha256::new();
    for (rel_path, content) in &entries {
        hasher.update(rel_path.as_bytes());
        hasher.update([0u8]);
        hasher.update(content);
        hasher.update([0u8]);
    }
    format!("{:x}", hasher.finalize())
}

fn collect_files(root: &Path, dir: &Path, out: &mut Vec<(String, Vec<u8>)>) {
    let read = fs::read_dir(dir).expect("read_dir");
    for entry in read {
        let entry = entry.expect("dir entry");
        let path = entry.path();
        if path.is_dir() {
            collect_files(root, &path, out);
        } else if path.is_file() {
            let rel = path
                .strip_prefix(root)
                .expect("strip_prefix")
                .to_string_lossy()
                .to_string();
            let content = fs::read(&path).expect("read file");
            out.push((rel, content));
        }
    }
}

/// Parse the JSON document `init` prints on stdout.
///
/// `init` emits one JSON object on stdout, but the process may also print
/// telemetry/log lines around it. So we scan for a line opening a `{` and
/// accumulate until brace depth returns to zero (handles both compact and
/// pretty-printed JSON), returning the first complete object carrying a
/// top-level `status` field. (Same strategy as `proof_init_test.rs::stdout_json`,
/// duplicated here because each file under `tests/` is its own test binary.)
fn stdout_json(bytes: &[u8]) -> Value {
    let s = String::from_utf8_lossy(bytes);
    let lines: Vec<&str> = s.lines().collect();
    for start in 0..lines.len() {
        if !lines[start].trim_start().starts_with('{') {
            continue;
        }
        let mut depth: i32 = 0;
        let mut buf = String::new();
        for line in &lines[start..] {
            buf.push_str(line);
            buf.push('\n');
            depth += line.matches('{').count() as i32;
            depth -= line.matches('}').count() as i32;
            if depth <= 0 {
                break;
            }
        }
        if let Ok(v) = serde_json::from_str::<Value>(buf.trim()) {
            if v.get("status").is_some() {
                return v;
            }
        }
    }
    panic!("init stdout had no JSON object carrying a \"status\" field; got: {s}")
}

// ─────────────────────────────────────────────────────────────────────────────
// Blocker D: `--force <malformed-value>` must be REFUSED, never silently
// coerced to `false`, and must leave the filesystem byte-for-byte unchanged.
// ─────────────────────────────────────────────────────────────────────────────
#[test]
fn init_force_refuses_invalid_value() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    // Arrange: an already-initialized boundary (so --force is actually guarding
    // something real) plus a real marker file, matching the bug report's exact
    // repro shape (`echo MARKER > README.md` before the malformed --force call).
    ggen()
        .current_dir(root)
        .args(["init", "--path", ".", "--skip-hooks", "true"])
        .assert()
        .success();
    fs::write(root.join("README.md"), "MARKER\n").expect("write marker");

    let before = dir_digest(root);

    // Act: `--force garbage` -- a malformed boolean value.
    let assert = ggen()
        .current_dir(root)
        .args([
            "init",
            "--path",
            ".",
            "--force",
            "garbage",
            "--skip-hooks",
            "true",
        ])
        .assert();
    let output = assert.get_output().clone();

    // Assert: refused at the PROCESS level -- non-zero exit code, not a
    // silent `false` coercion that then reports "already initialized".
    assert!(
        !output.status.success(),
        "ggen init --force garbage must be refused (non-zero exit), got status: {:?}, stdout: {}, stderr: {}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    // Assert: the refusal is a TYPED error naming both the invalid value and
    // the accepted values -- not a bare "already initialized" message, which
    // is what the old silent-coercion-to-false bug would have produced.
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("garbage"),
        "refusal must name the invalid value 'garbage': {stderr}"
    );
    assert!(
        stderr.contains("true") && stderr.contains("false"),
        "refusal must name the accepted values (true/false): {stderr}"
    );
    assert!(
        !stderr.contains("already initialized"),
        "a malformed --force value must be refused on its own terms, not fall through \
         to the (coerced-to-false) \"already initialized\" message: {stderr}"
    );

    // Assert: FS_after == FS_before. A refused, malformed --force value must
    // not touch the filesystem at all -- not even to re-check the existing
    // artifacts, since parsing failure is caught before `perform_init` runs.
    let after = dir_digest(root);
    assert_eq!(
        before, after,
        "a refused --force value must leave the filesystem byte-for-byte unchanged \
         (FS_after must equal FS_before)"
    );

    // Also exercise the exact typo'd repro from the bug report ("ture") against
    // a brand-new, not-yet-initialized directory: it must refuse and must not
    // scaffold a single file.
    let dir2 = TempDir::new().expect("tempdir");
    let root2 = dir2.path();
    let assert2 = ggen()
        .current_dir(root2)
        .args(["init", "--path", ".", "--force", "ture"])
        .assert();
    let output2 = assert2.get_output().clone();
    assert!(
        !output2.status.success(),
        "ggen init --force ture (typo) must be refused, not silently treated as false"
    );
    let stderr2 = String::from_utf8_lossy(&output2.stderr);
    assert!(
        stderr2.contains("ture"),
        "refusal must name the invalid value 'ture': {stderr2}"
    );
    assert!(
        !root2.join("ggen.toml").exists(),
        "a refused malformed --force value must not scaffold any files at all"
    );
    assert_eq!(
        fs::read_dir(root2).expect("read fresh dir").count(),
        0,
        "a refused malformed --force value must leave a fresh directory completely empty"
    );
}

/// Bonus coverage: `--skip-hooks` takes the exact same `Option<bool>` ->
/// `Option<String>` path through the `#[verb]` macro as `--force`, so it must
/// be refused the same way on a malformed value (task explicitly asks to check
/// this flag too).
#[test]
fn init_skip_hooks_refuses_invalid_value() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    let assert = ggen()
        .current_dir(root)
        .args(["init", "--path", ".", "--skip-hooks", "garbage"])
        .assert();
    let output = assert.get_output().clone();

    assert!(
        !output.status.success(),
        "ggen init --skip-hooks garbage must be refused (non-zero exit)"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("garbage"),
        "refusal must name the invalid value 'garbage': {stderr}"
    );
    assert!(
        !root.join("ggen.toml").exists(),
        "a refused malformed --skip-hooks value must not scaffold any files at all"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Blocker E: every `ggen init` failure path must return a non-zero process
// exit code -- a `status: "error"` JSON payload must not coexist with exit 0.
// ─────────────────────────────────────────────────────────────────────────────
#[test]
fn init_failure_returns_nonzero() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    // Baseline: a fresh init really does succeed and exit 0 (so the assertions
    // below are contrasting against a real, working success path, not against
    // a command that always fails for unrelated reasons).
    let fresh = ggen()
        .current_dir(root)
        .args(["init", "--path", ".", "--skip-hooks", "true"])
        .assert();
    let fresh_output = fresh.get_output().clone();
    assert!(
        fresh_output.status.success(),
        "fresh init must exit 0: status {:?}, stderr: {}",
        fresh_output.status,
        String::from_utf8_lossy(&fresh_output.stderr)
    );
    assert_eq!(fresh_output.status.code(), Some(0));

    // Act: re-init WITHOUT --force against the now-already-initialized dir --
    // this is `perform_init`'s own "already initialized" refusal branch.
    let assert = ggen()
        .current_dir(root)
        .args(["init", "--path", ".", "--skip-hooks", "true"])
        .assert();
    let output = assert.get_output().clone();

    // Blocker E, the core assertion: a failed init must exit non-zero. Before
    // the fix, this exact scenario exited 0 despite reporting status="error".
    assert!(
        !output.status.success(),
        "re-init without --force must exit non-zero (Blocker E): status {:?}",
        output.status
    );
    assert_ne!(
        output.status.code(),
        Some(0),
        "exit code must not be 0 for a failed init"
    );

    // The structured JSON payload contract is preserved: the process failure
    // is not just a bare non-zero exit with no context -- the same status="error"
    // body the bug report observed is still present on stdout.
    let json = stdout_json(&output.stdout);
    assert_eq!(
        json["status"].as_str(),
        Some("error"),
        "payload must still report status=error: {json}"
    );
    let err_msg = json["error"].as_str().unwrap_or("");
    assert!(
        err_msg.contains("already initialized"),
        "payload must still explain the refusal reason: {err_msg}"
    );

    // And the process-level error text (stderr) independently names the same
    // failure, so a caller that only looks at stderr/exit-code (never parses
    // stdout JSON) still learns why it failed.
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("already initialized"),
        "stderr must also explain the failure: {stderr}"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Blocker E (filesystem side): a failed init must be a real no-op on disk, not
// merely "reported as an error while quietly mutating something".
// ─────────────────────────────────────────────────────────────────────────────
#[test]
fn init_failure_preserves_filesystem() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    ggen()
        .current_dir(root)
        .args(["init", "--path", ".", "--skip-hooks", "true"])
        .assert()
        .success();

    // Mutate a ggen-owned file with a distinctive sentinel so a real
    // byte-for-byte comparison has unambiguous content to check.
    let sentinel = "# USER-EDITED-SENTINEL -- must survive a failed re-init\n";
    fs::write(root.join("ggen.toml"), sentinel).expect("overwrite manifest with sentinel");

    let before = dir_digest(root);

    // Act: re-init without --force -- must fail (Blocker E) and must not touch
    // the filesystem (perform_init's already-initialized branch returns before
    // any fs::create_dir_all / tx.write_file / tx.commit call happens).
    let assert = ggen()
        .current_dir(root)
        .args(["init", "--path", ".", "--skip-hooks", "true"])
        .assert();
    let output = assert.get_output().clone();
    assert!(
        !output.status.success(),
        "a failed re-init must exit non-zero: status {:?}",
        output.status
    );

    let after = dir_digest(root);
    assert_eq!(
        before, after,
        "a refused re-init (no --force) must leave the filesystem byte-for-byte unchanged \
         (FS_after must equal FS_before)"
    );

    // Explicit, human-legible proof of the exact bytes -- not just the digest.
    let manifest_after = fs::read_to_string(root.join("ggen.toml")).expect("read manifest");
    assert_eq!(
        manifest_after, sentinel,
        "the sentinel content must survive the failed re-init byte-for-byte"
    );
}
