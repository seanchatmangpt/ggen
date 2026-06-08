import re

with open("crates/ggen-projection/tests/f4_lsp_diagnostics.rs", "r") as file:
    content = file.read()

# Fix the test names from blocked to their correct diagnostic outputs
content = re.sub(r'#\[test\]\nfn test_f4_t2_wasm4pm_blocked_on_replay_surface\(\) \{.*?\n\}\n', '', content, flags=re.DOTALL)

new_tests = """
#[test]
fn test_f4_t2_wasm4pm_replay_deviation() {
    let mut client = init_client_for("wasm4pm-lsp");
    // Providing generic event_type without GALL-CHECKPOINT-003 relationship triggers DEVIATION from adapter
    client.did_open("file:///virtual/project/events.jsonl", "json", "{\\"events\\\": [{\\"event_type\\\": \\\"BoundaryDeclared\\"}]}").unwrap();
    let res = client.wait_for_notification_timeout("textDocument/publishDiagnostics", std::time::Duration::from_millis(2000));
    assert!(res.is_ok());
    let diags = res.unwrap()["params"]["diagnostics"].as_array().unwrap().clone();
    assert!(diags.iter().any(|d| d["code"] == "WASM4PM-REPLAY-DEVIATION"), "got {:?}", diags);
    shutdown_client(client);
}

#[test]
fn test_f4_t2_wasm4pm_digest_chain_broken() {
    let mut client = init_client_for("wasm4pm-lsp");
    // Triggering broken chain path
    client.did_open("file:///virtual/project/events.jsonl", "json", "{\\"events\\\": [{\\"attributes\\\": [{\\"name\\\": \\\"previous_receipt\\\", \\\"value\\\": \\\"tampered_uuid\\\"}]}]}").unwrap();
    let res = client.wait_for_notification_timeout("textDocument/publishDiagnostics", std::time::Duration::from_millis(2000));
    assert!(res.is_ok());
    let diags = res.unwrap()["params"]["diagnostics"].as_array().unwrap().clone();
    assert!(diags.iter().any(|d| d["code"] == "WASM4PM-DIGEST-CHAIN-BROKEN"), "got {:?}", diags);
    shutdown_client(client);
}

#[test]
fn test_f4_t2_wasm4pm_verdict_fit() {
    let mut client = init_client_for("wasm4pm-lsp");
    // Triggering successful fit path
    client.did_open("file:///virtual/project/events.jsonl", "json", "{\\"events\\\": [{\\"relationships\\\": [{\\"objectId\\\": \\\"GALL-CHECKPOINT-003\\"}]}]}").unwrap();
    let res = client.wait_for_notification_timeout("textDocument/publishDiagnostics", std::time::Duration::from_millis(2000));
    assert!(res.is_ok());
    let diags = res.unwrap()["params"]["diagnostics"].as_array().unwrap().clone();
    assert!(diags.iter().any(|d| d["code"] == "WASM4PM-VERDICT-FIT"), "got {:?}", diags);
    shutdown_client(client);
}
"""

if "test_f4_t2_wasm4pm_replay_deviation" not in content:
    content += new_tests

with open("crates/ggen-projection/tests/f4_lsp_diagnostics.rs", "w") as file:
    file.write(content)

