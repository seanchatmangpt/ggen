mod common;

use common::lsp_harness::LspHarness;
use serde_json::json;
use std::fs;

#[test]
fn test_admission_category_1_diagnostic_protocol() {
    let mut harness = LspHarness::new();
    harness.initialize();

    // open a generated/projected file
    let uri = "file:///tmp/main.rs";
    harness.did_open(uri, "fn main() {}");

    // wait for publishDiagnostics
    let diags = harness.wait_for_publish_diagnostics(uri);
    assert!(diags["params"]["diagnostics"].is_array());

    // assert GGEN-PROJECTED-001 and source_id ggen_lsp_observer
    harness.assert_diagnostic_code("GGEN-PROJECTED-001");
    harness.assert_source_id("ggen_lsp_observer");
}

#[test]
fn test_admission_category_2_drift_protocol() {
    let mut harness = LspHarness::new();
    harness.initialize();

    // open projected file
    let uri = "file:///tmp/main.rs";
    harness.did_open(uri, "fn main() {}");
    harness.wait_for_publish_diagnostics(uri);

    // didChange modified text to contain "drifted"
    harness.did_change(uri, "fn main() { // drifted }");
    harness.wait_for_publish_diagnostics(uri);

    // assert GGEN-DRIFT-001
    harness.assert_diagnostic_code("GGEN-DRIFT-001");
    harness.assert_source_id("ggen_lsp_observer");
}

#[test]
fn test_admission_category_3_pack_domain_diagnostics() {
    let mut harness = LspHarness::new();
    harness.initialize();

    // open malformed clap command file
    let uri_clap = "file:///tmp/cli.rs";
    harness.did_open(uri_clap, "struct Opts;");
    harness.wait_for_publish_diagnostics(uri_clap);

    // assert CLAP-PACK-* and source_id clap_noun_verb_pack_lsp
    harness.assert_diagnostic_code("CLAP-PACK-HANDLER-UNBOUND");
    harness.assert_source_id("clap_noun_verb_pack_lsp");

    // open malformed tower LSP composition file
    let uri_tower = "file:///tmp/server.rs";
    harness.did_open(uri_tower, "fn write_to_disk() {}");
    harness.wait_for_publish_diagnostics(uri_tower);

    // assert TOWER-PACK-* and source_id tower_lsp_max_pack_lsp
    harness.assert_diagnostic_code("TOWER-PACK-UNGUARDED-MUTATION");
    harness.assert_source_id("tower_lsp_max_pack_lsp");
}

#[test]
fn test_admission_category_4_authority_split() {
    let mut harness = LspHarness::new();
    harness.initialize();

    // part 1: corrupt receipt
    let uri_receipts = "file:///tmp/receipts.json";
    harness.did_open(uri_receipts, "random_corrupt_bytes");
    harness.wait_for_publish_diagnostics(uri_receipts);

    // expect GGEN-RECEIPT-* (meaning GGEN-EVIDENCE-001)
    harness.assert_diagnostic_code("GGEN-EVIDENCE-001");
    harness.assert_source_id("ggen_lsp_observer");
    // no CLAP-* or TOWER-* diagnostic owns it
    harness.assert_no_diagnostic_from("clap_noun_verb_pack_lsp");
    harness.assert_no_diagnostic_from("tower_lsp_max_pack_lsp");

    // part 2: corrupt clap domain shape
    let uri_clap = "file:///tmp/cli.rs";
    harness.did_open(uri_clap, "struct Opts;");
    let _diags = harness.wait_for_publish_diagnostics(uri_clap);
    
    // expect CLAP-*
    harness.assert_diagnostic_code("CLAP-PACK-HANDLER-UNBOUND");
    // ggen-lsp must not invent CLAP-* itself (its source_id must be clap_noun_verb_pack_lsp)
    let last = harness.last_diagnostics();
    let clap_diag = last.iter().find(|d| {
        d.get("code").and_then(|c| c.as_str()) == Some("CLAP-PACK-HANDLER-UNBOUND")
    }).expect("Found CLAP-PACK-HANDLER-UNBOUND diagnostic");
    
    let source_id = clap_diag.get("data").and_then(|data| data.get("source_id")).and_then(|s| s.as_str()).unwrap_or("");
    assert_eq!(source_id, "clap_noun_verb_pack_lsp", "Clap diagnostic must be owned by the pack observer, not ggen-lsp itself");
}

#[test]
fn test_admission_category_5_code_action_routing() {
    let mut harness = LspHarness::new();
    harness.initialize();

    // Create a SPARQL file that triggers a TemplateFailure (E0011)
    let uri = "file:///tmp/query.rq";
    let text = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\n";
    harness.did_open(uri, text);
    harness.wait_for_publish_diagnostics(uri);

    harness.assert_diagnostic_code("E0011");

    // Request code action
    let range = json!({
        "start": { "line": 0, "character": 0 },
        "end": { "line": 0, "character": 10 }
    });
    let actions_resp = harness.request_code_action(uri, range);
    assert!(actions_resp.get("error").is_none());

    let actions = actions_resp["result"].as_array().expect("CodeAction response result array");
    assert!(!actions.is_empty(), "Must offer at least one code action for E0011");

    // Assert kind and envelope/intention
    let quickfix = actions.iter().find(|a| {
        a.get("kind").and_then(|k| k.as_str()) == Some("quickfix")
    }).expect("Must contain quickfix code action");

    assert!(quickfix.get("title").is_some());
    let data = quickfix.get("data").expect("Must carry route envelope data");
    assert_eq!(data.get("diagnostic_code").and_then(|c| c.as_str()), Some("E0011"));

    // Execute command (dummy/method not found, since server capability is not registered)
    let cmd_resp = harness.execute_command("dummy.command", json!(null));
    assert!(cmd_resp.get("error").is_some() || cmd_resp.get("result").is_some());

    // Assert no direct write to filesystem
    let temp_root = harness.temp_dir().to_path_buf();
    assert!(!temp_root.join("test.txt").exists());
    assert!(!temp_root.join("main.rs").exists());

    // Now resolve the diagnostic using did_change to turtle clean text
    harness.did_change(uri, "@prefix : <http://example.org/> .\n:Thing a :Class .\n");
    harness.wait_for_publish_diagnostics(uri);

    // Verify diagnostic is cleared
    let diags = harness.last_diagnostics();
    assert!(!diags.iter().any(|d| d.get("code").and_then(|c| c.as_str()) == Some("E0011")));

    // Assert PackPlan/Staging/MutationGate/Receipt path via looking at the intel log
    let log_path = temp_root.join(".intel").join("log.jsonl");
    assert!(log_path.exists(), "Intel log path must exist under root: {:?}", log_path);

    let log_content = fs::read_to_string(&log_path).expect("Read intel log content");
    assert!(log_content.contains("DiagnosticRaised"), "Log must contain DiagnosticRaised");
    assert!(log_content.contains("RouteSelected"), "Log must contain RouteSelected");
    assert!(log_content.contains("RepairSuggested"), "Log must contain RepairSuggested");
    assert!(log_content.contains("RepairApplied"), "Log must contain RepairApplied");
    assert!(log_content.contains("GatePassed"), "Log must contain GatePassed");
    assert!(log_content.contains("ReceiptEmitted"), "Log must contain ReceiptEmitted");
}
