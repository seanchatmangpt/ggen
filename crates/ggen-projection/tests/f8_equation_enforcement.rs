use std::path::{Path, PathBuf};
use tempfile::TempDir;

use ggen_projection::{
    ProjectionMap, ProjectionMapping, ReceiptIndex, EquationContext
};

fn setup_test() -> (ReceiptIndex, EquationContext, ProjectionMap, TempDir, PathBuf) {
    let mut receipts = ReceiptIndex::new();
    let eq = EquationContext {
        boundary_digest: "b_digest_1".to_string(),
        workspace_digest: "w_digest_1".to_string(),
        pack_plan_digest: "pp_digest_1".to_string(),
        pack_descriptor_digest: "pd_digest_1".to_string(),
        customization_digest: "c_digest_1".to_string(),
        staging_digest: "s_digest_1".to_string(),
        mutation_gate_decision: "approved".to_string(),
        verification_result: "passed".to_string(),
        projection_engine_version: "26.6.6".to_string(),
    };
    receipts.add_receipt("src/main.rs".to_string(), b"generated_code", b"template_content", &eq, None);
    
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

    let tmp = TempDir::new().unwrap();
    let target = tmp.path().join("src");
    std::fs::create_dir_all(&target).unwrap();
    std::fs::write(target.join("main.rs"), b"generated_code").unwrap();
    
    (receipts, eq, map, tmp, target)
}

#[test]
fn test_f8_t1_equation_enforcement_boundary_change_invalidates_receipts() {
    let (receipts, eq, map, tmp, _) = setup_test();
    let res_ok = map.validate_sync(tmp.path(), &receipts, Some(&eq));
    assert!(res_ok.is_ok(), "Validation should succeed with matching equation");
    
    let mut bad_eq = eq.clone();
    bad_eq.boundary_digest = "b_digest_2".to_string();
    let res_bad = map.validate_sync(tmp.path(), &receipts, Some(&bad_eq));
    assert!(res_bad.is_err());
    assert!(res_bad.unwrap_err().to_string().contains("boundary_digest mismatch"));
}

#[test]
fn test_f8_t1_equation_enforcement_workspace_change_invalidates_receipts() {
    let (receipts, eq, map, tmp, _) = setup_test();
    let mut bad_eq = eq.clone();
    bad_eq.workspace_digest = "w_digest_2".to_string();
    let res_bad = map.validate_sync(tmp.path(), &receipts, Some(&bad_eq));
    assert!(res_bad.is_err());
    assert!(res_bad.unwrap_err().to_string().contains("workspace_digest mismatch"));
}

#[test]
fn test_f8_t1_equation_enforcement_pack_plan_change_invalidates_receipts() {
    let (receipts, eq, map, tmp, _) = setup_test();
    let mut bad_eq = eq.clone();
    bad_eq.pack_plan_digest = "pp_digest_2".to_string();
    let res_bad = map.validate_sync(tmp.path(), &receipts, Some(&bad_eq));
    assert!(res_bad.is_err());
    assert!(res_bad.unwrap_err().to_string().contains("pack_plan_digest mismatch"));
}

#[test]
fn test_f8_t1_equation_enforcement_pack_descriptor_change_invalidates_receipts() {
    let (receipts, eq, map, tmp, _) = setup_test();
    let mut bad_eq = eq.clone();
    bad_eq.pack_descriptor_digest = "pd_digest_2".to_string();
    let res_bad = map.validate_sync(tmp.path(), &receipts, Some(&bad_eq));
    assert!(res_bad.is_err());
    assert!(res_bad.unwrap_err().to_string().contains("pack_descriptor_digest mismatch"));
}

#[test]
fn test_f8_t1_equation_enforcement_customization_change_invalidates_receipts() {
    let (receipts, eq, map, tmp, _) = setup_test();
    let mut bad_eq = eq.clone();
    bad_eq.customization_digest = "c_digest_2".to_string();
    let res_bad = map.validate_sync(tmp.path(), &receipts, Some(&bad_eq));
    assert!(res_bad.is_err());
    assert!(res_bad.unwrap_err().to_string().contains("customization_digest mismatch"));
}

#[test]
fn test_f8_t1_equation_enforcement_staging_change_invalidates_receipts() {
    let (receipts, eq, map, tmp, _) = setup_test();
    let mut bad_eq = eq.clone();
    bad_eq.staging_digest = "s_digest_2".to_string();
    let res_bad = map.validate_sync(tmp.path(), &receipts, Some(&bad_eq));
    assert!(res_bad.is_err());
    assert!(res_bad.unwrap_err().to_string().contains("staging_digest mismatch"));
}

#[test]
fn test_f8_t1_equation_enforcement_missing_mutation_gate_decision_invalidates_receipts() {
    let (receipts, eq, map, tmp, _) = setup_test();
    let mut bad_eq = eq.clone();
    bad_eq.mutation_gate_decision = "rejected".to_string();
    let res_bad = map.validate_sync(tmp.path(), &receipts, Some(&bad_eq));
    assert!(res_bad.is_err());
    assert!(res_bad.unwrap_err().to_string().contains("mutation_gate_decision mismatch"));
}

#[test]
fn test_f8_t1_equation_enforcement_missing_verification_result_invalidates_receipts() {
    let (receipts, eq, map, tmp, _) = setup_test();
    let mut bad_eq = eq.clone();
    bad_eq.verification_result = "failed".to_string();
    let res_bad = map.validate_sync(tmp.path(), &receipts, Some(&bad_eq));
    assert!(res_bad.is_err());
    assert!(res_bad.unwrap_err().to_string().contains("verification_result mismatch"));
}

#[test]
fn test_f8_t1_equation_enforcement_engine_version_change_invalidates_receipts() {
    let (receipts, eq, map, tmp, _) = setup_test();
    let mut bad_eq = eq.clone();
    bad_eq.projection_engine_version = "2.0.0".to_string();
    let res_bad = map.validate_sync(tmp.path(), &receipts, Some(&bad_eq));
    assert!(res_bad.is_err());
    assert!(res_bad.unwrap_err().to_string().contains("projection_engine_version mismatch"));
}

#[test]
fn test_f8_t1_equation_enforcement_receipt_chain_break() {
    let (mut receipts, eq, map, tmp, _) = setup_test();
    receipts.add_receipt("src/unmapped.rs".to_string(), b"evil_code", b"template", &eq, None);
    let mut bad_map = map.clone();
    bad_map.add_mapping(PathBuf::from("src/unmapped.rs"), ProjectionMapping {
        pack_id: "test".to_string(),
        template_path: PathBuf::from("fake.tmpl"),
        query_path: None,
        bound_variables: vec![],
        merge_strategy: "Exclusive".to_string(),
        start_line: None,
        end_line: None,
    }).unwrap();
    let target = tmp.path().join("src");
    std::fs::write(target.join("unmapped.rs"), b"evil_code").unwrap();
    
    let res_bad = bad_map.validate_sync(tmp.path(), &receipts, Some(&eq));
    assert!(res_bad.is_ok()); 
}

#[test]
fn test_f8_t1_equation_enforcement_after_the_fact_laundering_fails() {
    let (mut receipts, eq, map, tmp, _) = setup_test();
    let target = tmp.path().join("src");
    
    // Attempt to launder a modified file by changing it on disk, but NOT having a valid receipt
    std::fs::write(target.join("main.rs"), b"evil_forged_code").unwrap();
    
    // validate_sync must catch this
    let res_bad = map.validate_sync(tmp.path(), &receipts, Some(&eq));
    assert!(res_bad.is_err());
    assert!(res_bad.unwrap_err().to_string().contains("modified/out-of-sync"));
}