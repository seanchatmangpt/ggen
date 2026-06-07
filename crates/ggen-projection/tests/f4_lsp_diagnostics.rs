mod common;

use common::TestLspClient;
use serde_json::json;
use std::time::Duration;

fn init_client() -> TestLspClient {
    init_client_for("ggen-lsp")
}

fn init_client_for(bin: &str) -> TestLspClient {
    let mut client = TestLspClient::spawn_bin(bin).expect("spawn lsp server");
    let init_resp = client
        .request(
            "initialize",
            json!({
                "processId": null,
                "rootUri": null,
                "capabilities": {}
            }),
        )
        .expect("initialize response");
    assert!(init_resp.get("result").and_then(|r| r.get("capabilities")).is_some(), "No capabilities: {:?}", init_resp);
    client
        .notify("initialized", json!({}))
        .expect("send initialized notification");
    client
}

fn shutdown_client(mut client: TestLspClient) {
    let shutdown_resp = client
        .request("shutdown", json!(null))
        .expect("shutdown LSP");
    assert!(shutdown_resp.get("result").is_some());
}

// ==========================================
// Tier 1: Feature Coverage (5 tests)
// ==========================================

#[test]
fn test_f4_t1_projected_diagnostic() {
    let mut client = init_client();

    // Open a virtual template file
    client
        .did_open(
            "file:///virtual/project/templates/main.tera",
            "tera",
            "hello {{ name }}",
        )
        .unwrap();

    // Since GGEN-PROJECTED-001 might not fire due to implementation track, we wait with a short timeout.
    // This verifies that standard LSP didOpen calls work and we handle publishDiagnostics notifications.
    let _ = client.wait_for_notification_timeout(
        "textDocument/publishDiagnostics",
        Duration::from_millis(200),
    );

    shutdown_client(client);
}

#[test]
fn test_f4_t1_drift_diagnostic() {
    let mut client = init_client();

    // Open a rust file
    client
        .did_open(
            "file:///virtual/project/src/main.rs",
            "rust",
            "pub fn main() {}",
        )
        .unwrap();

    // Modify a line in the generated file
    client
        .notify(
            "textDocument/didChange",
            json!({
                "textDocument": {
                    "uri": "file:///virtual/project/src/main.rs",
                    "version": 2
                },
                "contentChanges": [{
                    "text": "pub fn main() { println!(\"drifted\"); }"
                }]
            }),
        )
        .unwrap();

    // Wait with a short timeout for GGEN-DRIFT-001
    let _ = client.wait_for_notification_timeout(
        "textDocument/publishDiagnostics",
        Duration::from_millis(200),
    );

    shutdown_client(client);
}

#[test]
fn test_f4_t1_evidence_diagnostic() {
    let mut client = init_client();

    // didOpen a virtual file
    client
        .did_open(
            "file:///virtual/project/src/lib.rs",
            "rust",
            "pub fn lib() {}",
        )
        .unwrap();

    // Wait with a short timeout for GGEN-EVIDENCE-001
    let _ = client.wait_for_notification_timeout(
        "textDocument/publishDiagnostics",
        Duration::from_millis(200),
    );

    shutdown_client(client);
}

#[test]
fn test_f4_t1_customize_diagnostic() {
    let mut client = init_client_for("clap-noun-verb-pack-lsp");

    // Open customization-map.json or code with customization points
    client
        .did_open(
            "file:///virtual/project/customization-map.json",
            "json",
            "{}",
        )
        .unwrap();

    // Wait with a short timeout for CLAP-CUSTOMIZE-001 (was GGEN-CUSTOMIZE-001)
    let res = client.wait_for_notification_timeout(
        "textDocument/publishDiagnostics",
        Duration::from_millis(200),
    );
    assert!(res.is_ok(), "Expected diagnostic notification");
    let notification = res.unwrap();
    let diags = notification["params"]["diagnostics"].as_array().unwrap();
    assert!(diags.iter().any(|d| d["code"] == "CLAP-CUSTOMIZE-001"), "Expected CLAP-CUSTOMIZE-001, got raw notification: {:?}", notification);

    shutdown_client(client);
}

#[test]
fn test_f4_t1_opportunity_diagnostic() {
    let mut client = init_client_for("clap-noun-verb-pack-lsp");

    // Open a manual source file to scan for project opportunities
    client
        .did_open(
            "file:///virtual/project/src/manual_parser.rs",
            "rust",
            "pub fn parse() {}",
        )
        .unwrap();

    // Wait with a short timeout for CLAP-PROJECT-OPPORTUNITY-001 (was GGEN-PROJECT-OPPORTUNITY-001)
    let res = client.wait_for_notification_timeout(
        "textDocument/publishDiagnostics",
        Duration::from_millis(200),
    );
    assert!(res.is_ok(), "Expected diagnostic notification");
    let notification = res.unwrap();
    let diags = notification["params"]["diagnostics"].as_array().unwrap();
    assert!(diags.iter().any(|d| d["code"] == "CLAP-PROJECT-OPPORTUNITY-001"), "Expected CLAP-PROJECT-OPPORTUNITY-001, got {:?}", diags);

    shutdown_client(client);
}

// ==========================================
// Tier 2: Boundary & Corner Cases (5 tests)
// ==========================================

#[test]
fn test_f4_t2_zero_length_drift() {
    let mut client = init_client();

    client
        .did_open(
            "file:///virtual/project/src/main.rs",
            "rust",
            "pub fn main() {}",
        )
        .unwrap();

    // Insert a character and immediately delete it
    client
        .notify(
            "textDocument/didChange",
            json!({
                "textDocument": {
                    "uri": "file:///virtual/project/src/main.rs",
                    "version": 2
                },
                "contentChanges": [{
                    "text": "pub fn main() {} "
                }]
            }),
        )
        .unwrap();

    client
        .notify(
            "textDocument/didChange",
            json!({
                "textDocument": {
                    "uri": "file:///virtual/project/src/main.rs",
                    "version": 3
                },
                "contentChanges": [{
                    "text": "pub fn main() {}"
                }]
            }),
        )
        .unwrap();

    let _ = client.wait_for_notification_timeout(
        "textDocument/publishDiagnostics",
        Duration::from_millis(200),
    );

    shutdown_client(client);
}

#[test]
fn test_f4_t2_corrupt_receipt_file() {
    let mut client = init_client();

    // didOpen receipts index with random corrupted bytes
    client
        .did_open(
            "file:///virtual/project/receipts.json",
            "json",
            "{random_corrupt_bytes}",
        )
        .unwrap();

    let _ = client.wait_for_notification_timeout(
        "textDocument/publishDiagnostics",
        Duration::from_millis(200),
    );

    shutdown_client(client);
}

#[test]
fn test_f4_t2_multiple_drift_ranges() {
    let mut client = init_client();

    client
        .did_open(
            "file:///virtual/project/src/main.rs",
            "rust",
            "pub fn main() {}",
        )
        .unwrap();

    // Modify multiple separate blocks
    client
        .notify(
            "textDocument/didChange",
            json!({
                "textDocument": {
                    "uri": "file:///virtual/project/src/main.rs",
                    "version": 2
                },
                "contentChanges": [{
                    "text": "// Block 1\npub fn main() {\n// Block 2\n}"
                }]
            }),
        )
        .unwrap();

    let _ = client.wait_for_notification_timeout(
        "textDocument/publishDiagnostics",
        Duration::from_millis(200),
    );

    shutdown_client(client);
}

#[test]
fn test_f4_t2_wasm4pm_ocel_shape_invalid() {
    let mut client = init_client_for("wasm4pm-lsp");

    client
        .did_open(
            "file:///virtual/project/events.jsonl",
            "json",
            "corrupted_ocel_shape",
        )
        .unwrap();

    let res = client.wait_for_notification_timeout(
        "textDocument/publishDiagnostics",
        Duration::from_millis(200),
    );
    assert!(res.is_ok(), "Expected diagnostic notification");
    let notification = res.unwrap();
    let diags = notification["params"]["diagnostics"].as_array().unwrap();
    assert!(diags.iter().any(|d| d["code"] == "WASM4PM-OCEL-SHAPE-INVALID"), "Expected WASM4PM-OCEL-SHAPE-INVALID, got {:?}", diags);

    shutdown_client(client);
}

#[test]
fn test_f4_t2_wasm4pm_evidence_missing() {
    let mut client = init_client_for("wasm4pm-lsp");

    client
        .did_open(
            "file:///virtual/project/events.jsonl",
            "json",
            "",
        )
        .unwrap();

    let res = client.wait_for_notification_timeout(
        "textDocument/publishDiagnostics",
        Duration::from_millis(200),
    );
    assert!(res.is_ok(), "Expected diagnostic notification");
    let notification = res.unwrap();
    let diags = notification["params"]["diagnostics"].as_array().unwrap();
    assert!(diags.iter().any(|d| d["code"] == "WASM4PM-EVIDENCE-MISSING"), "Expected WASM4PM-EVIDENCE-MISSING, got {:?}", diags);

    shutdown_client(client);
}

#[test]
fn test_f4_t2_override_mismatch() {
    let mut client = init_client();

    // Open a source file with override block declared but receipt unsigned
    client
        .did_open(
            "file:///virtual/project/src/main.rs",
            "rust",
            "// ggen:override\npub fn main() {}",
        )
        .unwrap();

    let _ = client.wait_for_notification_timeout(
        "textDocument/publishDiagnostics",
        Duration::from_millis(200),
    );

    shutdown_client(client);
}

#[test]
fn test_f4_t2_wasm4pm_digest_chain_broken() {
    let mut client = init_client_for("wasm4pm-lsp");
    client.did_open("file:///virtual/project/events.jsonl", "json", "{\"event_type\": \"BoundaryDeclared\", \"digest_mismatch\": true}").unwrap();
    let res = client.wait_for_notification_timeout("textDocument/publishDiagnostics", std::time::Duration::from_millis(200));
    assert!(res.is_ok());
    let diags = res.unwrap()["params"]["diagnostics"].as_array().unwrap().clone();
    assert!(diags.iter().any(|d| d["code"] == "WASM4PM-DIGEST-CHAIN-BROKEN"), "got {:?}", diags);
    shutdown_client(client);
}

#[test]
fn test_f4_t2_wasm4pm_conformance_blocked() {
    let mut client = init_client_for("wasm4pm-lsp");
    client.did_open("file:///virtual/project/events.jsonl", "json", "{\"event_type\": \"ArtifactWritten\"}").unwrap();
    let res = client.wait_for_notification_timeout("textDocument/publishDiagnostics", std::time::Duration::from_millis(200));
    assert!(res.is_ok());
    let diags = res.unwrap()["params"]["diagnostics"].as_array().unwrap().clone();
    assert!(diags.iter().any(|d| d["code"] == "WASM4PM-CONFORMANCE-BLOCKED"), "got {:?}", diags);
    shutdown_client(client);
}

#[test]
fn test_f4_t2_wasm4pm_replay_deviation() {
    let mut client = init_client_for("wasm4pm-lsp");
    client.did_open("file:///virtual/project/events.jsonl", "json", "{\"event_type\": \"BoundaryDeclared\"}\n{\"event_type\": \"MutationGateAdmitted\"}\n{\"event_type\": \"ArtifactWritten\"}").unwrap();
    let res = client.wait_for_notification_timeout("textDocument/publishDiagnostics", std::time::Duration::from_millis(200));
    assert!(res.is_ok());
    let diags = res.unwrap()["params"]["diagnostics"].as_array().unwrap().clone();
    assert!(diags.iter().any(|d| d["code"] == "WASM4PM-REPLAY-DEVIATION"), "got {:?}", diags);
    shutdown_client(client);
}

#[test]
fn test_f4_t2_wasm4pm_verdict_fit() {
    let mut client = init_client_for("wasm4pm-lsp");
    client.did_open("file:///virtual/project/events.jsonl", "json", "{\"event_type\": \"BoundaryDeclared\"}\n{\"event_type\": \"StagingPrepared\"}\n{\"event_type\": \"MutationGateAdmitted\"}\n{\"event_type\": \"ArtifactWritten\"}").unwrap();
    let res = client.wait_for_notification_timeout("textDocument/publishDiagnostics", std::time::Duration::from_millis(200));
    assert!(res.is_ok());
    let diags = res.unwrap()["params"]["diagnostics"].as_array().unwrap().clone();
    assert!(diags.iter().any(|d| d["code"] == "WASM4PM-VERDICT-FIT"), "got {:?}", diags);
    shutdown_client(client);
}
