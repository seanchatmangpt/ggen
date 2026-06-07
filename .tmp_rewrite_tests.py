import re

with open("crates/ggen-projection/tests/f4_lsp_diagnostics.rs", "r") as file:
    content = file.read()

# remove old test_f4_t2_wasm4pm_* tests since we removed fake authority
content = re.sub(r'#\[test\]\nfn test_f4_t2_wasm4pm_ocel_shape_invalid\(\) \{.*?\n\}\n', '', content, flags=re.DOTALL)
content = re.sub(r'#\[test\]\nfn test_f4_t2_wasm4pm_conformance_blocked\(\) \{.*?\n\}\n', '', content, flags=re.DOTALL)
content = re.sub(r'#\[test\]\nfn test_f4_t2_wasm4pm_evidence_missing\(\) \{.*?\n\}\n', '', content, flags=re.DOTALL)
content = re.sub(r'#\[test\]\nfn test_f4_t2_wasm4pm_digest_chain_broken\(\) \{.*?\n\}\n', '', content, flags=re.DOTALL)
content = re.sub(r'#\[test\]\nfn test_f4_t2_wasm4pm_replay_deviation\(\) \{.*?\n\}\n', '', content, flags=re.DOTALL)
content = re.sub(r'#\[test\]\nfn test_f4_t2_wasm4pm_verdict_fit\(\) \{.*?\n\}\n', '', content, flags=re.DOTALL)

new_tests = """
#[test]
fn test_f4_t2_wasm4pm_blocked_on_replay_surface() {
    let mut client = init_client_for("wasm4pm-lsp");
    client.did_open("file:///virtual/project/events.jsonl", "json", "{\"event_type\": \"BoundaryDeclared\"}").unwrap();
    let res = client.wait_for_notification_timeout("textDocument/publishDiagnostics", std::time::Duration::from_millis(200));
    assert!(res.is_ok());
    let diags = res.unwrap()["params"]["diagnostics"].as_array().unwrap().clone();
    assert!(diags.iter().any(|d| d["code"] == "GC005_BLOCKED_ON_WASM4PM_REPLAY_SURFACE"), "got {:?}", diags);
    shutdown_client(client);
}
"""

if "test_f4_t2_wasm4pm_blocked_on_replay_surface" not in content:
    content += new_tests

with open("crates/ggen-projection/tests/f4_lsp_diagnostics.rs", "w") as file:
    file.write(content)
