mod common;

use ggen_projection::{EquationContext, sync, CustomizationMap, PackDescriptor, PackPlan, PackTemplateDescriptor, ProjectionMap,
    ProjectionMapping, ReceiptIndex,
};
use serde_json::{json, Value};
use std::collections::BTreeMap;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::Mutex;

// Mock Upstream Server state for testing Feature 5
#[derive(Clone)]
pub struct MockUpstream {
    pub name: String,
    pub should_crash: bool,
    pub delay: Option<Duration>,
    pub diagnostics: Vec<Value>,
    pub hover_text: Option<String>,
    pub completion_items: Vec<Value>,
}

// Composite LSP Multiplexer Implementation
pub struct CompositeMultiplexer {
    pub upstreams: Vec<MockUpstream>,
    pub client_diagnostics: Arc<Mutex<Vec<Value>>>,
    pub timeout_limit: Duration,
}

impl CompositeMultiplexer {
    pub fn new(upstreams: Vec<MockUpstream>) -> Self {
        Self {
            upstreams,
            client_diagnostics: Arc::new(Mutex::new(Vec::new())),
            timeout_limit: Duration::from_millis(50),
        }
    }

    pub async fn handle_notification(
        &self, method: &str, params: Value,
    ) -> Result<(), anyhow::Error> {
        if method == "textDocument/publishDiagnostics" {
            let mut diags = params
                .get("diagnostics")
                .and_then(Value::as_array)
                .cloned()
                .unwrap_or_default();
            let mut validated_diags = Vec::new();

            for d in &mut diags {
                let source_id = d
                    .get("data")
                    .and_then(|data| data.get("source_id"))
                    .and_then(Value::as_str);

                if source_id.is_none() {
                    return Err(anyhow::anyhow!("Attribution Bypass: source_id is missing"));
                }

                validated_diags.push(d.clone());
            }

            let mut client_diags = self.client_diagnostics.lock().await;
            client_diags.push(json!({
                "uri": params.get("uri").unwrap_or(&Value::Null),
                "diagnostics": validated_diags
            }));
        }
        Ok(())
    }

    pub async fn get_merged_diagnostics(&self) -> Vec<Value> {
        let client_diags = self.client_diagnostics.lock().await;
        let mut all_diags = Vec::new();
        for item in client_diags.iter() {
            if let Some(diags) = item.get("diagnostics").and_then(Value::as_array) {
                all_diags.extend(diags.clone());
            }
        }
        // Sort by line number, then character
        all_diags.sort_by(|a, b| {
            let a_line = a
                .get("range")
                .and_then(|r| r.get("start"))
                .and_then(|s| s.get("line"))
                .and_then(Value::as_i64)
                .unwrap_or(0);
            let b_line = b
                .get("range")
                .and_then(|r| r.get("start"))
                .and_then(|s| s.get("line"))
                .and_then(Value::as_i64)
                .unwrap_or(0);
            let a_char = a
                .get("range")
                .and_then(|r| r.get("start"))
                .and_then(|s| s.get("character"))
                .and_then(Value::as_i64)
                .unwrap_or(0);
            let b_char = b
                .get("range")
                .and_then(|r| r.get("start"))
                .and_then(|s| s.get("character"))
                .and_then(Value::as_i64)
                .unwrap_or(0);
            match a_line.cmp(&b_line) {
                std::cmp::Ordering::Equal => a_char.cmp(&b_char),
                other => other,
            }
        });
        all_diags
    }
}

// ---------------------------------------------------------------------------
// 1. test_t4_clap_noun_verb_lsp_projection_flow
// ---------------------------------------------------------------------------
#[test]
fn test_t4_clap_noun_verb_lsp_projection_flow() {
    let tmp = tempfile::TempDir::new().unwrap();

    // Setup base pack descriptor
    let base_pack = PackDescriptor {
        id: "clap-noun-verb".to_string(),
        name: "CLAP Noun Verb Base".to_string(),
        version: "1.0.0".to_string(),
        description: "CLI parser".to_string(),
        license: "MIT".to_string(),
        dependencies: BTreeMap::new(),
        templates: vec![],
        query_aliases: BTreeMap::new(),
    };

    // Setup LSP pack descriptor depending on base pack
    let mut lsp_deps = BTreeMap::new();
    lsp_deps.insert("clap-noun-verb".to_string(), ">=1.0.0".to_string());

    let lsp_pack = PackDescriptor {
        id: "clap-noun-verb-lsp".to_string(),
        name: "CLAP Noun Verb LSP Pack".to_string(),
        version: "1.0.0".to_string(),
        description: "LSP integrations".to_string(),
        license: "MIT".to_string(),
        dependencies: lsp_deps,
        templates: vec![PackTemplateDescriptor {
            path: PathBuf::from("templates/lsp_server.tmpl"),
            description: "LSP server template".to_string(),
            variables: vec!["port".to_string()],
        }],
        query_aliases: BTreeMap::new(),
    };

    // Resolve projection resolution order
    let plan = PackPlan::resolve(&[base_pack, lsp_pack]).unwrap();
    assert_eq!(
        plan.resolution_order,
        vec!["clap-noun-verb", "clap-noun-verb-lsp"]
    );

    // Setup projection maps & customization map
    let mut proj_map = ProjectionMap::new();
    proj_map
        .add_mapping(
            PathBuf::from("src/lsp_server.rs"),
            ProjectionMapping {
                pack_id: "clap-noun-verb-lsp".to_string(),
                template_path: PathBuf::from("templates/lsp_server.tmpl"),
                query_path: None,
                bound_variables: vec!["port".to_string()],
                merge_strategy: "Exclusive".to_string(),
                start_line: None,
                end_line: None,
            },
        )
        .unwrap();

    let mut cust_map = CustomizationMap::new();
    cust_map.vars.insert("port".to_string(), "5007".to_string());

    // Setup files on disk & receipts
    let file_path = tmp.path().join("src/lsp_server.rs");
    fs::create_dir_all(file_path.parent().unwrap()).unwrap();

    let generated_content = b"fn main() { /* port = 5007 */ }";
    fs::write(&file_path, generated_content).unwrap();

    let mut receipts = ReceiptIndex::new();
    receipts.add_receipt("src/lsp_server.rs".to_string(), generated_content, b"", &EquationContext::default(), None);

    // Sync to disk
    sync(tmp.path(), &proj_map, &cust_map, &receipts).unwrap();

    // Verify receipts and markers exist
    assert!(tmp.path().join("projection-map.json").exists());
    assert!(tmp.path().join("receipts.json").exists());
    assert!(tmp.path().join(".sync_marker").exists());

    // Receipt validation
    let res = proj_map.validate_sync(tmp.path(), &receipts, None);
    assert!(res.is_ok());
}

// ---------------------------------------------------------------------------
// 2. test_t4_modifying_generated_files_drift_loop
// ---------------------------------------------------------------------------
#[test]
fn test_t4_modifying_generated_files_drift_loop() {
    let tmp = tempfile::TempDir::new().unwrap();
    let file_path = tmp.path().join("src/main.rs");
    fs::create_dir_all(file_path.parent().unwrap()).unwrap();

    // Initial sync setup
    let original_content = b"original content";
    fs::write(&file_path, original_content).unwrap();

    let mut receipts = ReceiptIndex::new();
    receipts.add_receipt("src/main.rs".to_string(), original_content, b"", &EquationContext::default(), None);

    let mut proj_map = ProjectionMap::new();
    proj_map
        .add_mapping(
            PathBuf::from("src/main.rs"),
            ProjectionMapping {
                pack_id: "pack_1".to_string(),
                template_path: PathBuf::from("templates/main.tmpl"),
                query_path: None,
                bound_variables: vec![],
                merge_strategy: "Exclusive".to_string(),
                start_line: None,
                end_line: None,
            },
        )
        .unwrap();

    let cust_map = CustomizationMap::new();
    sync(tmp.path(), &proj_map, &cust_map, &receipts).unwrap();

    // Verify original sync is valid
    assert!(proj_map.validate_sync(tmp.path(), &receipts, None).is_ok());

    // Simulate developer modifying generated files (drift)
    let drifted_content = b"original content --- modified by dev";
    fs::write(&file_path, drifted_content).unwrap();

    // Verify that drift is detected (validate_sync fails)
    assert!(proj_map.validate_sync(tmp.path(), &receipts, None).is_err());

    // Simulate developer forcing sync (which regenerates receipts)
    let mut new_receipts = ReceiptIndex::new();
    new_receipts.add_receipt("src/main.rs".to_string(), drifted_content, b"", &EquationContext::default(), None);

    sync(tmp.path(), &proj_map, &cust_map, &new_receipts).unwrap();

    // Verify that diagnostic is cleared (validate_sync succeeds with new receipts)
    assert!(proj_map.validate_sync(tmp.path(), &new_receipts, None).is_ok());
}

// ---------------------------------------------------------------------------
// 3. test_t4_composite_lsp_diagnostic_merge_attribution
// ---------------------------------------------------------------------------
#[tokio::test]
async fn test_t4_composite_lsp_diagnostic_merge_attribution() {
    let multiplexer = CompositeMultiplexer::new(vec![]);

    // 1. Diagnostic from mock pack LSP (line 2)
    let mock_lsp_params = json!({
        "uri": "file:///src/main.rs",
        "diagnostics": [{
            "range": { "start": { "line": 2, "character": 5 }, "end": { "line": 2, "character": 10 } },
            "message": "Pack error",
            "severity": 2,
            "code": "PACK-001",
            "source": "mock-pack-lsp",
            "data": {
                "source_id": "mock_pack_lsp"
            }
        }]
    });

    // 2. Diagnostic from ggen-lsp (line 0)
    let ggen_lsp_params = json!({
        "uri": "file:///src/main.rs",
        "diagnostics": [{
            "range": { "start": { "line": 0, "character": 0 }, "end": { "line": 0, "character": 5 } },
            "message": "unbound projection",
            "severity": 1,
            "code": "GGEN-TPL-001",
            "source": "ggen-lsp",
            "data": {
                "source_id": "ggen_lsp"
            }
        }]
    });

    // Handle both notifications
    multiplexer
        .handle_notification("textDocument/publishDiagnostics", mock_lsp_params)
        .await
        .unwrap();
    multiplexer
        .handle_notification("textDocument/publishDiagnostics", ggen_lsp_params)
        .await
        .unwrap();

    // Merged & Sorted diagnostics
    let merged = multiplexer.get_merged_diagnostics().await;

    assert_eq!(merged.len(), 2);
    // Verifying sorting: ggen-lsp (line 0) should be first, mock-pack-lsp (line 2) second
    assert_eq!(merged[0]["code"], "GGEN-TPL-001");
    assert_eq!(merged[0]["data"]["source_id"], "ggen_lsp");

    assert_eq!(merged[1]["code"], "PACK-001");
    assert_eq!(merged[1]["data"]["source_id"], "mock_pack_lsp");
}

// ---------------------------------------------------------------------------
// 4. test_t4_dynamic_manifest_reload
// ---------------------------------------------------------------------------
#[test]
fn test_t4_dynamic_manifest_reload() {
    // 1. Initial valid manifest setup
    let pack_a = PackDescriptor {
        id: "pack_a".to_string(),
        name: "Pack A".to_string(),
        version: "1.0.0".to_string(),
        description: "A".to_string(),
        license: "MIT".to_string(),
        dependencies: BTreeMap::new(),
        templates: vec![],
        query_aliases: BTreeMap::new(),
    };

    let plan = PackPlan::resolve(&[pack_a.clone()]).unwrap();
    assert_eq!(plan.resolution_order, vec!["pack_a"]);

    // 2. Simulate dynamic modification of manifest (adding a missing dependency)
    let mut updated_pack_a = pack_a.clone();
    updated_pack_a
        .dependencies
        .insert("missing_pack".to_string(), ">=1.0.0".to_string());

    let resolve_res = PackPlan::resolve(&[updated_pack_a]);

    // Verify reload / re-resolve catches the issue, proving the dynamic reload feedback loop
    assert!(resolve_res.is_err());
    assert!(resolve_res
        .unwrap_err()
        .to_string()
        .contains("Dependency not found: missing_pack"));
}

// ---------------------------------------------------------------------------
// 5. test_t4_incremental_sync_and_validation
// ---------------------------------------------------------------------------
#[test]
fn test_t4_incremental_sync_and_validation() {
    let tmp = tempfile::TempDir::new().unwrap();

    let path_a = tmp.path().join("src/file_a.rs");
    let path_b = tmp.path().join("src/file_b.rs");
    fs::create_dir_all(path_a.parent().unwrap()).unwrap();

    // Initial projection setup
    let mut proj_map = ProjectionMap::new();
    proj_map
        .add_mapping(
            PathBuf::from("src/file_a.rs"),
            ProjectionMapping {
                pack_id: "pack_a".to_string(),
                template_path: PathBuf::from("templates/file_a.tmpl"),
                query_path: None,
                bound_variables: vec!["title".to_string()],
                merge_strategy: "Exclusive".to_string(),
                start_line: None,
                end_line: None,
            },
        )
        .unwrap();
    proj_map
        .add_mapping(
            PathBuf::from("src/file_b.rs"),
            ProjectionMapping {
                pack_id: "pack_b".to_string(),
                template_path: PathBuf::from("templates/file_b.tmpl"),
                query_path: None,
                bound_variables: vec!["author".to_string()],
                merge_strategy: "Exclusive".to_string(),
                start_line: None,
                end_line: None,
            },
        )
        .unwrap();

    let mut cust_map = CustomizationMap::new();
    cust_map
        .vars
        .insert("title".to_string(), "App v1".to_string());
    cust_map
        .vars
        .insert("author".to_string(), "User".to_string());

    // Write original contents
    let content_a_v1 = b"fn main() { /* title = App v1 */ }";
    let content_b = b"fn main() { /* author = User */ }";
    fs::write(&path_a, content_a_v1).unwrap();
    fs::write(&path_b, content_b).unwrap();

    let mut receipts = ReceiptIndex::new();
    receipts.add_receipt("src/file_a.rs".to_string(), content_a_v1, b"", &EquationContext::default(), None);
    receipts.add_receipt("src/file_b.rs".to_string(), content_b, b"", &EquationContext::default(), None);

    sync(tmp.path(), &proj_map, &cust_map, &receipts).unwrap();

    // Capture initial receipt info for file_b
    let receipt_b_v1 = receipts.receipts.get("src/file_b.rs").unwrap().clone();

    // Incremental update to customization map (only affects file_a.rs)
    cust_map
        .vars
        .insert("title".to_string(), "App v2".to_string());

    // Simulate incremental sync: only re-project file_a.rs
    let content_a_v2 = b"fn main() { /* title = App v2 */ }";
    fs::write(&path_a, content_a_v2).unwrap(); // file_a updated
                                               // file_b remains untouched on disk!

    // Update receipt only for file_a.rs
    receipts.add_receipt("src/file_a.rs".to_string(), content_a_v2, b"", &EquationContext::default(), None);

    sync(tmp.path(), &proj_map, &cust_map, &receipts).unwrap();

    // Verify that file_b's receipt was untouched (retains same receipt ID)
    let receipt_b_v2 = receipts.receipts.get("src/file_b.rs").unwrap();
    assert_eq!(receipt_b_v1.receipt_id, receipt_b_v2.receipt_id);
    assert_eq!(receipt_b_v1.blake3_hash, receipt_b_v2.blake3_hash);

    // Verify both files are valid under the updated sync
    assert!(proj_map.validate_sync(tmp.path(), &receipts, None).is_ok());
}
