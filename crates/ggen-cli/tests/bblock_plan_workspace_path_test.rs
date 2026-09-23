//! Chicago-style integration test for `ggen bblock plan`'s idempotency-check
//! read path (`docs/jira/v26.9.1/03-FORTUNE5-TESTING-BBLOCK-PORTABILITY.md`
//! chaos-suite defect: `fortune5-testing-bblock-pack`'s `chaos` suite failed
//! with `cannot read /workspace/.ggen/bblocks/receipts/aws/testing-plan-result.json:
//! No such file or directory` when the real `ggen` binary was invoked from a
//! real, non-`/workspace` working directory — a literal `/workspace` prefix
//! baked into the receipt-read path while the write path correctly resolved
//! `std::env::current_dir()`).
//!
//! This test runs the real compiled `ggen` binary (`assert_cmd::Command::cargo_bin`)
//! twice from a real temporary directory that is deliberately NOT named or
//! mounted at `/workspace`, first to write the plan receipt, second to read it
//! back for the idempotency (`previous_digest`) check — state-based assertions
//! on real stdout/stderr and the real receipt file on disk. No mocks, no
//! stubs of any collaborator.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use std::fs;

/// Two real `ggen bblock plan` invocations from the same real, non-`/workspace`
/// cwd: the first writes `.ggen/bblocks/receipts/aws/testing-plan-result.json`
/// relative to that cwd; the second must read that same file back successfully
/// for its idempotency (`previous_digest`) check instead of failing with
/// `cannot read /workspace/...`.
#[test]
fn bblock_plan_idempotency_read_resolves_real_cwd_not_literal_workspace() {
    let unique = format!(
        "ggen-bblock-plan-cwd-test-{}-{:?}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::SystemTime::UNIX_EPOCH)
            .expect("time")
    );
    // Deliberately anchored under the real temp dir (e.g. /tmp or
    // /var/folders/... on macOS) so this is provably NOT `/workspace` and
    // NOT the crate's own project root — the exact class of cwd the
    // fortune5-testing-bblock-pack chaos suite runs its subprocess from.
    let root = std::env::temp_dir().join(unique);
    fs::create_dir_all(&root).expect("create real non-/workspace cwd");
    assert_ne!(
        root.to_string_lossy(),
        "/workspace",
        "test fixture must not accidentally be /workspace"
    );

    // First invocation: writes the plan + intent + result receipts relative
    // to the real cwd (GENESIS predecessor digest, since no prior receipt
    // exists yet).
    let first = Command::cargo_bin("ggen")
        .expect("ggen binary must be built for tests")
        .current_dir(&root)
        .args([
            "bblock",
            "plan",
            "--group-id",
            "fortune5-complete",
            "--provider",
            "aws",
        ])
        .output()
        .expect("first ggen bblock plan must execute");
    assert!(
        first.status.success(),
        "first bblock plan invocation failed: stdout={}\nstderr={}",
        String::from_utf8_lossy(&first.stdout),
        String::from_utf8_lossy(&first.stderr)
    );

    let receipt_path = root
        .join(".ggen")
        .join("bblocks")
        .join("receipts")
        .join("aws")
        .join("fortune5-complete-plan-result.json");
    assert!(
        receipt_path.is_file(),
        "first invocation must have written the plan-result receipt at {}",
        receipt_path.display()
    );
    let first_stdout: serde_json::Value =
        serde_json::from_slice(&first.stdout).expect("first invocation must emit valid JSON");

    // Second invocation, same real cwd, same relative receipt path: this is
    // the idempotency-check read. Before the fix this failed with a literal
    // `/workspace/...` ENOENT even though the real receipt sat at
    // `<root>/.ggen/bblocks/receipts/aws/fortune5-complete-plan-result.json`.
    let second = Command::cargo_bin("ggen")
        .expect("ggen binary must be built for tests")
        .current_dir(&root)
        .args([
            "bblock",
            "plan",
            "--group-id",
            "fortune5-complete",
            "--provider",
            "aws",
        ])
        .output()
        .expect("second ggen bblock plan must execute");
    assert!(
        second.status.success(),
        "second bblock plan invocation (idempotency-check read) failed: stdout={}\nstderr={}",
        String::from_utf8_lossy(&second.stdout),
        String::from_utf8_lossy(&second.stderr)
    );
    let stderr = String::from_utf8_lossy(&second.stderr);
    assert!(
        !stderr.contains("/workspace/"),
        "second invocation's stderr must not reference a hardcoded /workspace path: {stderr}"
    );

    let second_stdout: serde_json::Value =
        serde_json::from_slice(&second.stdout).expect("second invocation must emit valid JSON");
    let receipt_digest = second_stdout["receipt_digest"]
        .as_str()
        .expect("response must contain receipt_digest");
    assert!(
        !receipt_digest.is_empty(),
        "receipt_digest must be a real non-empty digest, not a refusal placeholder"
    );

    // The intent receipt's `previous_digest` is computed directly from
    // `previous_receipt_digest(&result_path)` — the exact idempotency-check
    // read this defect broke. On the first invocation it must be the
    // GENESIS placeholder (no prior result existed); on the second
    // invocation it must be the *first* invocation's real result digest,
    // proving the read of the real cwd-relative receipt (not a hardcoded
    // `/workspace`) actually succeeded.
    let intent_path = root
        .join(".ggen")
        .join("bblocks")
        .join("receipts")
        .join("aws")
        .join("fortune5-complete-plan-intent.json");
    let intent_bytes = fs::read(&intent_path).expect("intent receipt file must still be readable");
    let intent: serde_json::Value =
        serde_json::from_slice(&intent_bytes).expect("intent receipt file must contain valid JSON");
    let first_result_digest = first_stdout["receipt_digest"]
        .as_str()
        .expect("first response must contain receipt_digest");
    assert_ne!(
        intent["previous_digest"].as_str(),
        Some("GENESIS"),
        "second invocation's intent receipt must NOT fall back to GENESIS: the idempotency-check \
         read of the first invocation's real cwd-relative receipt must have succeeded"
    );
    assert_eq!(
        intent["previous_digest"].as_str(),
        Some(first_result_digest),
        "second invocation's intent receipt must chain from the first invocation's real result digest"
    );

    fs::remove_dir_all(&root).ok();
}
