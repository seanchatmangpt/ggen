mod common;

use common::lsp_harness::LspHarness;
use serde_json::json;

#[test]
fn test_lsp_harness_operations() {
    // 1. Initialize
    let mut harness = LspHarness::new();
    let init_resp = harness.initialize();
    
    // Verify standard server capabilities
    assert_eq!(init_resp["result"]["serverInfo"]["name"], "ggen-lsp");
    let caps = &init_resp["result"]["capabilities"];
    assert!(!caps["hoverProvider"].is_null());
    assert!(!caps["codeActionProvider"].is_null());

    // 2. did_open
    let uri = "file:///x/query.rq";
    // E0011 is triggered by unconstrained CONSTRUCT
    let text = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\n";
    harness.did_open(uri, text);

    // 3. wait_for_publish_diagnostics
    let diags = harness.wait_for_publish_diagnostics(uri);
    assert!(diags["params"]["diagnostics"].is_array());

    // 4. Assertions (diagnostic code and source id)
    harness.assert_diagnostic_code("E0011");
    harness.assert_source_id("ggen-lsp");
    harness.assert_no_diagnostic_from("nonexistent-source");

    // 5. did_change
    harness.did_change(uri, "# Just a comment update\n");
    let diags2 = harness.wait_for_publish_diagnostics(uri);
    assert!(diags2["params"]["diagnostics"].is_array());

    // 6. request_code_action
    // E0011 is not diagnostic list matching here, but let's query with null range or specific range
    let range = json!({
        "start": { "line": 0, "character": 0 },
        "end": { "line": 0, "character": 10 }
    });
    let actions = harness.request_code_action(uri, range);
    assert!(actions.get("error").is_none());
    assert!(actions.get("result").is_some());

    // 7. execute_command
    // Let's call execute_command with a dummy command
    let cmd_resp = harness.execute_command("dummy.command", json!(null));
    // Since the server doesn't register execute_command_provider, it should return method not found or error.
    // We just assert that it completes and returns a well-formed JSON response (either result or error).
    assert!(cmd_resp.get("error").is_some() || cmd_resp.get("result").is_some());
}
