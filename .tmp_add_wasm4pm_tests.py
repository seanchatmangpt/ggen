import re

with open("crates/ggen-projection/tests/f4_lsp_diagnostics.rs", "r") as file:
    content = file.read()

new_tests = """
#[test]
fn test_f4_t2_wasm4pm_digest_chain_broken() {
    let mut client = init_client_for("wasm4pm-lsp");
    client.did_open("file:///virtual/project/events.jsonl", "json", "broken_digest_chain").unwrap();
    let res = client.wait_for_notification_timeout("textDocument/publishDiagnostics", std::time::Duration::from_millis(200));
    assert!(res.is_ok());
    let diags = res.unwrap()["params"]["diagnostics"].as_array().unwrap().clone();
    assert!(diags.iter().any(|d| d["code"] == "WASM4PM-DIGEST-CHAIN-BROKEN"), "got {:?}", diags);
    shutdown_client(client);
}

#[test]
fn test_f4_t2_wasm4pm_conformance_blocked() {
    let mut client = init_client_for("wasm4pm-lsp");
    client.did_open("file:///virtual/project/events.jsonl", "json", "conformance_blocked").unwrap();
    let res = client.wait_for_notification_timeout("textDocument/publishDiagnostics", std::time::Duration::from_millis(200));
    assert!(res.is_ok());
    let diags = res.unwrap()["params"]["diagnostics"].as_array().unwrap().clone();
    assert!(diags.iter().any(|d| d["code"] == "WASM4PM-CONFORMANCE-BLOCKED"), "got {:?}", diags);
    shutdown_client(client);
}

#[test]
fn test_f4_t2_wasm4pm_replay_deviation() {
    let mut client = init_client_for("wasm4pm-lsp");
    client.did_open("file:///virtual/project/events.jsonl", "json", "replay_deviation").unwrap();
    let res = client.wait_for_notification_timeout("textDocument/publishDiagnostics", std::time::Duration::from_millis(200));
    assert!(res.is_ok());
    let diags = res.unwrap()["params"]["diagnostics"].as_array().unwrap().clone();
    assert!(diags.iter().any(|d| d["code"] == "WASM4PM-REPLAY-DEVIATION"), "got {:?}", diags);
    shutdown_client(client);
}

#[test]
fn test_f4_t2_wasm4pm_verdict_fit() {
    let mut client = init_client_for("wasm4pm-lsp");
    client.did_open("file:///virtual/project/events.jsonl", "json", "verdict_fit").unwrap();
    let res = client.wait_for_notification_timeout("textDocument/publishDiagnostics", std::time::Duration::from_millis(200));
    assert!(res.is_ok());
    let diags = res.unwrap()["params"]["diagnostics"].as_array().unwrap().clone();
    assert!(diags.iter().any(|d| d["code"] == "WASM4PM-VERDICT-FIT"), "got {:?}", diags);
    shutdown_client(client);
}
"""

if "test_f4_t2_wasm4pm_digest_chain_broken" not in content:
    content += new_tests

with open("crates/ggen-projection/tests/f4_lsp_diagnostics.rs", "w") as file:
    file.write(content)
