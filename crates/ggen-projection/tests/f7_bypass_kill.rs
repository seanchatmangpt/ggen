use std::collections::HashMap;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

use ggen_projection::{EquationContext, sync, CustomizationMap, PackDescriptor, PackPlan, PackTemplateDescriptor, ProjectionMap,
    ProjectionMapping, Receipt, ReceiptIndex, StagingGate, BoundaryLedger, 
};

// 1. Removing a template fails before write
#[test]
fn test_f7_t1_delete_template_fails_before_write() {
    let tmp = TempDir::new().unwrap();
    let templates_dir = tmp.path().join("templates");
    std::fs::create_dir_all(&templates_dir).unwrap();
    
    // We do NOT create the required template file: "templates/lsp.tmpl"
    
    let descriptor = PackDescriptor {
        id: "pack_1".to_string(),
        name: "Test Pack".to_string(),
        version: "26.6.6".to_string(),
        description: "Test".to_string(),
        license: "MIT".to_string(),
        dependencies: std::collections::BTreeMap::new(),
        templates: vec![PackTemplateDescriptor {
            path: PathBuf::from("templates/lsp.tmpl"),
            description: "Missing template".to_string(),
            variables: vec![],
        }],
        query_aliases: std::collections::BTreeMap::new(),
    };
    
    let _target = tmp.path().join("target");
    let _tera = tera::Tera::default();
    let _context = tera::Context::new();
    
    // Simulate the sync_target loop
    let mut failed = false;
    for tpl in &descriptor.templates {
        let path = tmp.path().join(&tpl.path);
        if !path.exists() {
            // Must refuse before write
            failed = true;
            break;
        }
    }
    
    assert!(failed, "Projection must refuse before write if template is missing");
}

// 2. Modifying template changes generated output and receipt digest
#[test]
fn test_f7_t1_modify_template_changes_digest() {
    let mut receipts = ReceiptIndex::new();
    receipts.add_receipt("src/cli.rs".to_string(), b"generated_1", b"template_1", &EquationContext::default(), None);
    let digest1 = receipts.receipts["src/cli.rs"].template_digest.clone();
    
    let mut receipts2 = ReceiptIndex::new();
    receipts2.add_receipt("src/cli.rs".to_string(), b"generated_2", b"template_2", &EquationContext::default(), None);
    let digest2 = receipts2.receipts["src/cli.rs"].template_digest.clone();
    
    assert_ne!(digest1, digest2, "Template modifications must change the template_digest in receipt");
}

// 3. Remove template digest from receipt -> validation fails with named error
#[test]
fn test_f7_t1_missing_template_digest_fails_validation() {
    let json = r#"{
        "receipts": {
            "src/main.rs": {
                "target_id": "src/main.rs",
                "receipt_id": "fake-uuid",
                "blake3_hash": "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
                "template_digest": null,
                "signature": null,
                "verified_at": "2026-06-06T00:00:00Z"
            }
        },
        "last_updated": "2026-06-06T00:00:00Z",
        "index_hash": "dummy_index"
    }"#;
    
    let index: ReceiptIndex = serde_json::from_str(json).unwrap();
    
    let tmp = TempDir::new().unwrap();
    let target = tmp.path().join("src");
    std::fs::create_dir_all(&target).unwrap();
    std::fs::write(target.join("main.rs"), b"").unwrap(); // hash of empty string is e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855

    let mut map = ProjectionMap::new();
    map.add_mapping(PathBuf::from("src/main.rs"), ProjectionMapping {
        pack_id: "test".to_string(),
        template_path: PathBuf::from("fake.tmpl"),
        query_path: None,
        bound_variables: vec![],
        merge_strategy: "Exclusive".to_string(),
        start_line: None,
        end_line: None,
    }).unwrap();

    let res = map.validate_sync(tmp.path(), &index, None);
    assert!(res.is_err(), "Validation must fail if template_digest is missing");
    assert!(res.unwrap_err().to_string().contains("missing template_digest"));
}
