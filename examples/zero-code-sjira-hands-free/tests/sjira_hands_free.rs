//! Chicago-style court for the hands-free sjira CustomBehavior handlers:
//! real temp files, real `ggen sjira` process, assertions on observable
//! JSON / refusal text. No mocks.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

use serde_json::{json, Map, Value};
use sjira_hands_free::custom_handlers::{issue_admit, system_execute, system_frontier};

fn workspace(label: &str) -> PathBuf {
    let nonce = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock")
        .as_nanos();
    let root = std::env::temp_dir().join(format!(
        "sjira-hands-free-{label}-{}-{nonce}",
        std::process::id()
    ));
    fs::create_dir_all(&root).expect("temp workspace");
    root
}

fn write_json(dir: &PathBuf, name: &str, value: &Value) -> PathBuf {
    let path = dir.join(name);
    fs::write(&path, serde_json::to_vec_pretty(value).unwrap()).unwrap();
    path
}

fn valid_order(identity: &str) -> Value {
    json!({
        "identity": identity,
        "title": "t",
        "description": "d",
        "subject": format!("src/{identity}.rs"),
        "repository": "owner/repo",
        "base_sha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        "standing": "UNKNOWN",
        "evidence_ceiling": "UNVERIFIED",
        "promotion_rule": "courts",
        "replay_identity": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
        "origin_authority": "https://example.test/auth/X1",
        "projections": ["jira"],
        "required_courts": ["c1"],
        "required_evidence": ["e1"],
        "acceptance": ["a1"],
        "falsifiers": ["f1"],
        "path_scope": ["lib/"]
    })
}

fn backlog() -> Value {
    json!([
        {
            "work_order": valid_order("O1"),
            "dependencies": [],
            "path_scope": [],
            "concurrency_key": "global"
        },
        {
            "work_order": valid_order("O2"),
            "dependencies": [
                {"kind": "requiresSemanticIdentity", "upstream": "O1"}
            ],
            "path_scope": [],
            "concurrency_key": "global"
        }
    ])
}

fn inputs(pairs: &[(&str, Value)]) -> Map<String, Value> {
    let mut map = Map::new();
    for (k, v) in pairs {
        map.insert((*k).to_owned(), v.clone());
    }
    map
}

/// Fail-closed: process-adapter courts require a real sjira binary.
/// Soft-skip that returns Ok/pass is forbidden (Skeptic HIGH).
fn require_ggen() {
    let available = if let Ok(bin) = std::env::var("SJIRA_BIN") {
        let bin = bin.trim().to_owned();
        !bin.is_empty()
            && Command::new(&bin)
                .args(["--help"])
                .stdout(std::process::Stdio::null())
                .stderr(std::process::Stdio::null())
                .status()
                .map(|s| s.success())
                .unwrap_or(false)
    } else {
        Command::new(std::env::var("GGEN_BIN").unwrap_or_else(|_| "ggen".into()))
            .args(["sjira", "--help"])
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .status()
            .map(|s| s.success())
            .unwrap_or(false)
    };
    assert!(
        available,
        "fail-closed: ggen sjira (or SJIRA_BIN) required; set GGEN_BIN to the ggen binary. Soft-skip forbidden."
    );
}

#[test]
fn issue_admit_accepts_valid_work_order_via_real_ggen_sjira() {
    require_ggen();
    let dir = workspace("admit-ok");
    let order = write_json(&dir, "order.json", &valid_order("X1"));
    let out = issue_admit(inputs(&[("file", json!(order.to_string_lossy()))]))
        .expect("admit must succeed through real ggen sjira");
    assert_eq!(out["admitted"], json!(true));
    assert_eq!(out["identity"], json!("X1"));
    assert_eq!(out["standing"], json!("UNKNOWN"));
    let _ = fs::remove_dir_all(&dir);
}

#[test]
fn issue_admit_refuses_missing_file_fail_closed() {
    require_ggen();
    let err = issue_admit(inputs(&[(
        "file",
        json!("/nonexistent/sjira-hands-free-order.json"),
    )]))
    .expect_err("missing file must refuse");
    let msg = err.to_string();
    assert!(
        msg.contains("REFUSED:SJIRA_HANDS_FREE"),
        "expected hands-free refusal marker, got {msg}"
    );
}

#[test]
fn issue_admit_refuses_missing_required_arg() {
    let err = issue_admit(Map::new()).expect_err("missing file must refuse before spawn");
    assert!(
        err.to_string().contains("missing required argument `file`"),
        "{}",
        err
    );
}

#[test]
fn system_frontier_without_authority_uses_canonical_index() {
    // Hands-free ggen sjira frontier defaults to the embedded pack index.
    // Non-pack origins (example.test) block as origin_not_admitted.
    require_ggen();
    let dir = workspace("frontier-no-auth");
    let orders = write_json(&dir, "orders.json", &backlog());
    let out = system_frontier(inputs(&[(
        "orders_path",
        json!(orders.to_string_lossy()),
    )]))
    .expect("frontier without authority is a typed JSON answer, not a CLI crash");
    let blocked = out["blocked"].as_array().expect("blocked array");
    assert_eq!(blocked.len(), 2, "frontier dump: {out}");
    for b in blocked {
        assert_eq!(b["reason"], json!("origin_not_admitted"));
    }
    let _ = fs::remove_dir_all(&dir);
}

#[test]
fn system_execute_refuses_missing_authority_file() {
    require_ggen();
    let dir = workspace("execute-missing");
    let orders = write_json(&dir, "orders.json", &backlog());
    let ledger = dir.join("ledger.jsonl");
    fs::write(&ledger, "").unwrap();
    let err = system_execute(inputs(&[
        ("orders_path", json!(orders.to_string_lossy())),
        ("ledger_path", json!(ledger.to_string_lossy())),
        (
            "authority_path",
            json!(dir.join("missing-authority.json").to_string_lossy()),
        ),
        ("max", json!(1)),
    ]))
    .expect_err("missing authority must refuse");
    assert!(
        err.to_string().contains("REFUSED:SJIRA_HANDS_FREE"),
        "{}",
        err
    );
    let _ = fs::remove_dir_all(&dir);
}

#[test]
fn binary_issue_admit_matches_handler_court() {
    require_ggen();
    let bin = env!("CARGO_BIN_EXE_sjira-hands-free");
    let dir = workspace("bin-admit");
    let order = write_json(&dir, "order.json", &valid_order("BIN1"));
    let output = Command::new(bin)
        .current_dir(&dir)
        .args(["issue", "admit", order.to_str().unwrap()])
        .output()
        .expect("spawn generated binary");
    assert!(
        output.status.success(),
        "stdout={:?} stderr={:?}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let value: Value = serde_json::from_slice(&output.stdout).expect("JSON stdout");
    assert_eq!(value["admitted"], json!(true));
    assert_eq!(value["identity"], json!("BIN1"));
    let _ = fs::remove_dir_all(&dir);
}
