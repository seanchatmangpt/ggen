//! Integration tests for the `ggen ea` noun (WO-02 lane contract).
//!
//! Chicago-style: real binary, real kernel, real fixture. The kernel fixture
//! is read BY RELATIVE PATH from the sibling kernel crate
//! (`../ggen-abb-sbb/fixtures/ea-graph.json`, resolved against the ggen-cli
//! cargo manifest dir). Disclosed per the lane contract.

use std::process::Command;

const FIXTURE: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../ggen-abb-sbb/fixtures/ea-graph.json"
);

/// Run the real `ggen` binary with cwd = temp dir so the verbs never see the
/// default `./ea.graph.json` and every flag is exercised explicitly.
fn run_ea(args: &[&str]) -> (i32, String, String) {
    let out = Command::new(env!("CARGO_BIN_EXE_ggen"))
        .args(args)
        .current_dir(std::env::temp_dir())
        .output()
        .expect("spawn ggen binary");
    (
        out.status.code().unwrap_or(-1),
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
}

#[test]
fn plan_on_fixture_returns_decision() {
    let (code, stdout, stderr) = run_ea(&[
        "ea",
        "plan",
        "--graph",
        FIXTURE,
        "--abb",
        "abb:event-ingest",
        "--authority",
        "SELECT",
        "--json",
    ]);
    assert_eq!(code, 0, "plan must exit 0; stderr: {stderr}");
    assert!(
        stdout.contains("\"decision\""),
        "plan must print the Decision; stdout: {stdout}"
    );
    assert!(
        stdout.contains("\"SELECT\""),
        "fixture has a qualified SBB, plan must SELECT; stdout: {stdout}"
    );
}

#[test]
fn admit_at_construct_succeeds() {
    let (code, stdout, stderr) = run_ea(&[
        "ea",
        "admit",
        "--graph",
        FIXTURE,
        "--abb",
        "abb:event-ingest",
        "--sbb",
        "sbb:ingest-0000",
        "--authority",
        "CONSTRUCT",
        "--json",
    ]);
    assert_eq!(code, 0, "admit must exit 0; stderr: {stderr}");
    assert!(stdout.contains("\"ok\":true"), "stdout: {stdout}");
    assert!(stdout.contains("qual:ingest-0000"), "stdout: {stdout}");
}

#[test]
fn tampered_digest_admit_refuses_with_typed_json_and_exit_1() {
    let (code, stdout, stderr) = run_ea(&[
        "ea",
        "admit",
        "--graph",
        FIXTURE,
        "--abb",
        "abb:event-ingest",
        "--sbb",
        "sbb:ingest-0000",
        "--authority",
        "CONSTRUCT",
        "--expected-graph-digest",
        "sha256:0000000000000000000000000000000000000000000000000000000000000000",
        "--json",
    ]);
    assert_eq!(code, 1, "tampered digest must exit 1; stdout: {stdout}");
    assert!(
        stderr.contains("\"refusal\""),
        "typed refusal JSON must be on stderr; stderr: {stderr}"
    );
    assert!(
        stderr.contains("GRAPH_DIGEST_MISMATCH"),
        "typed tag must be on stderr; stderr: {stderr}"
    );
    assert!(
        !stdout.contains("\"ok\":true"),
        "no success envelope on stdout: {stdout}"
    );
}

fn manufacture_receipt(receipt_path: &str, graph_path: &str) -> (i32, String, String) {
    run_ea(&[
        "ea",
        "manufacture",
        "--graph",
        graph_path,
        "--abb",
        "abb:event-ingest",
        "--sbb",
        "sbb:ingest-0000",
        "--authority",
        "CONSTRUCT",
        "--receipt-out",
        receipt_path,
        "--json",
    ])
}

#[test]
fn replay_round_trip() {
    let dir = std::env::temp_dir().join(format!("ggen-ea-rt-{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("mkdir");
    let graph_path = dir.join("ea.graph.json");
    std::fs::copy(FIXTURE, &graph_path).expect("copy fixture");
    let receipt_path = dir.join("receipt.json");

    let (code, stdout, stderr) = manufacture_receipt(
        receipt_path.to_str().expect("utf8"),
        graph_path.to_str().expect("utf8"),
    );
    assert_eq!(code, 0, "manufacture must exit 0; stderr: {stderr}");
    assert!(stdout.contains("receipt_digest"), "stdout: {stdout}");
    assert!(receipt_path.exists(), "receipt must be written");

    let (code, stdout, stderr) = run_ea(&[
        "ea",
        "replay",
        "--graph",
        graph_path.to_str().expect("utf8"),
        "--receipt",
        receipt_path.to_str().expect("utf8"),
        "--json",
    ]);
    assert_eq!(code, 0, "replay round-trip must exit 0; stderr: {stderr}");
    assert!(stdout.contains("\"ok\":true"), "stdout: {stdout}");
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn replay_on_tampered_receipt_refuses() {
    let dir = std::env::temp_dir().join(format!("ggen-ea-tp-{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("mkdir");
    let graph_path = dir.join("ea.graph.json");
    std::fs::copy(FIXTURE, &graph_path).expect("copy fixture");
    let receipt_path = dir.join("receipt.json");

    let (code, _, stderr) = manufacture_receipt(
        receipt_path.to_str().expect("utf8"),
        graph_path.to_str().expect("utf8"),
    );
    assert_eq!(code, 0, "manufacture must exit 0; stderr: {stderr}");

    // Tamper: alter the declared receipt digest; the kernel must refuse.
    let raw = std::fs::read_to_string(&receipt_path).expect("read receipt");
    let tampered = raw.replacen("sha256:", "sha257:", 1);
    std::fs::write(&receipt_path, tampered).expect("write tampered receipt");

    let (code, stdout, stderr) = run_ea(&[
        "ea",
        "replay",
        "--graph",
        graph_path.to_str().expect("utf8"),
        "--receipt",
        receipt_path.to_str().expect("utf8"),
        "--json",
    ]);
    assert_eq!(code, 1, "tampered receipt must exit 1; stdout: {stdout}");
    assert!(
        stderr.contains("RECEIPT_TAMPERED"),
        "typed tag on stderr; stderr: {stderr}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn usage_errors_exit_2() {
    // Unknown authority value: usage error, not kernel refusal.
    let (code, _, stderr) = run_ea(&[
        "ea",
        "plan",
        "--graph",
        FIXTURE,
        "--abb",
        "abb:event-ingest",
        "--authority",
        "TRANSCEND",
        "--json",
    ]);
    assert_eq!(code, 2, "bad authority must exit 2; stderr: {stderr}");
    assert!(stderr.contains("INVALID_INVOCATION"), "stderr: {stderr}");

    // Unreadable graph file: usage error, not kernel refusal.
    let (code, _, stderr) = run_ea(&[
        "ea",
        "plan",
        "--graph",
        "/nonexistent/ea.graph.json",
        "--json",
    ]);
    assert_eq!(code, 2, "missing graph must exit 2; stderr: {stderr}");
    assert!(stderr.contains("INVALID_INVOCATION"), "stderr: {stderr}");
}

#[test]
fn verify_receipt_round_trip_and_tamper() {
    let dir = std::env::temp_dir().join(format!("ggen-ea-vr-{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("mkdir");
    let graph_path = dir.join("ea.graph.json");
    std::fs::copy(FIXTURE, &graph_path).expect("copy fixture");
    let receipt_path = dir.join("receipt.json");

    let (code, _, stderr) = manufacture_receipt(
        receipt_path.to_str().expect("utf8"),
        graph_path.to_str().expect("utf8"),
    );
    assert_eq!(code, 0, "manufacture must exit 0; stderr: {stderr}");

    let (code, stdout, stderr) = run_ea(&[
        "ea",
        "verify-receipt",
        "--receipt",
        receipt_path.to_str().expect("utf8"),
        "--json",
    ]);
    assert_eq!(code, 0, "verify-receipt must exit 0; stderr: {stderr}");
    assert!(stdout.contains("\"ok\":true"), "stdout: {stdout}");

    let raw = std::fs::read_to_string(&receipt_path).expect("read receipt");
    let tampered = raw.replacen("sha256:", "sha257:", 1);
    std::fs::write(&receipt_path, tampered).expect("write tampered receipt");

    let (code, stdout, stderr) = run_ea(&[
        "ea",
        "verify-receipt",
        "--receipt",
        receipt_path.to_str().expect("utf8"),
        "--json",
    ]);
    assert_eq!(code, 1, "tampered receipt must exit 1; stdout: {stdout}");
    assert!(
        stderr.contains("RECEIPT_TAMPERED"),
        "typed tag on stderr; stderr: {stderr}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}
