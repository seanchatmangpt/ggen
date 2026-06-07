use std::process::Command;
use std::path::{Path, PathBuf};
use std::fs;
use std::collections::HashMap;
use tempfile::tempdir;
use ggen_projection::{EquationContext, ReceiptIndex, ReceiptValidationError};

#[test]
fn test_gc003_boundary_receipted_equation_enforcement() {
    let tmp = tempdir().unwrap();
    let root = tmp.path();
    let target_dir = root.join(".tmp_gc003/target");
    let staging_dir = root.join(".tmp_gc003/staging");
    let receipt_sink = root.join(".tmp_gc003/receipts");
    
    let cargo_bin = env!("CARGO_BIN_EXE_sync_target");
    let workspace = "/Users/sac/ggen";
    let cnv_pack = "/Users/sac/ggen/crates/ggen-pack-clap-noun-verb";
    let lsp_pack = "/Users/sac/ggen/crates/ggen-pack-tower-lsp-max";

    let run_projection = || -> std::process::Output {
        Command::new(cargo_bin)
            .arg("--workspace").arg(workspace)
            .arg("--target").arg(&target_dir)
            .arg("--staging-dir").arg(&staging_dir)
            .arg("--receipt-sink").arg(&receipt_sink)
            .arg("--pack-roots").arg(cnv_pack).arg(lsp_pack)
            .output()
            .expect("Failed to run sync_target")
    };

    let output = run_projection();
    assert!(output.status.success());
    
    let receipts_json = fs::read_to_string(target_dir.join("receipts.json")).unwrap();
    let mut receipt_index = ReceiptIndex::from_json(&receipts_json).unwrap();
    
    // We should have at least one receipt
    assert!(!receipt_index.receipts.is_empty());
    
    // Find the head of the chain (the one with previous_receipt == None)
    let mut current_id = None;
    for (k, v) in &receipt_index.receipts {
        if v.previous_receipt.is_none() {
            current_id = Some(k.clone());
            break;
        }
    }
    
    let mut ordered_paths = Vec::new();
    let mut expected_previous = None;
    while let Some(path) = current_id {
        ordered_paths.push(path.clone());
        let r = receipt_index.receipts.get(&path).unwrap();
        expected_previous = Some(r.receipt_id.clone());
        
        // Find next
        current_id = None;
        for (k, v) in &receipt_index.receipts {
            if v.previous_receipt == expected_previous {
                current_id = Some(k.clone());
                break;
            }
        }
    }
    
    let first_path = ordered_paths[0].clone();
    let original_receipt = receipt_index.receipts.get(&first_path).unwrap().clone();
    
    let mut expected_eq = EquationContext {
        boundary_digest: original_receipt.boundary_digest.clone(),
        workspace_digest: original_receipt.workspace_digest.clone(),
        pack_plan_digest: original_receipt.pack_plan_digest.clone(),
        pack_descriptor_digest: original_receipt.pack_descriptor_digest.clone(),
        customization_digest: original_receipt.customization_digest.clone(),
        staging_digest: original_receipt.staging_digest.clone(),
        mutation_gate_decision: original_receipt.mutation_gate_decision.clone(),
        verification_result: original_receipt.verification_result.clone(),
        projection_engine_version: original_receipt.projection_engine_version.clone(),
    };
    
    let mut expected_artifacts = HashMap::new();
    let mut expected_templates = HashMap::new();
    
    for (k, v) in &receipt_index.receipts {
        expected_artifacts.insert(k.clone(), v.blake3_hash.clone());
        expected_templates.insert(k.clone(), v.template_digest.clone().unwrap());
    }

    if let Err(e) = receipt_index.validate_sync(&expected_eq, &expected_artifacts, &expected_templates, &ordered_paths) {
        panic!("Initial validation failed: {:?}", e);
    }

    
    // boundary-digest-required
    {
        
            let old = expected_eq.boundary_digest.clone();
            expected_eq.boundary_digest = "tampered".to_string();
            let res = receipt_index.validate_sync(&expected_eq, &expected_artifacts, &expected_templates, &ordered_paths);
            assert!(matches!(res, Err(ReceiptValidationError::BoundaryDigestMismatch)));
            expected_eq.boundary_digest = old;
        
    }
    
    // workspace-digest-required
    {
        
            let old = expected_eq.workspace_digest.clone();
            expected_eq.workspace_digest = "tampered".to_string();
            let res = receipt_index.validate_sync(&expected_eq, &expected_artifacts, &expected_templates, &ordered_paths);
            assert!(matches!(res, Err(ReceiptValidationError::WorkspaceDigestMismatch)));
            expected_eq.workspace_digest = old;
        
    }
    
    // pack-plan-digest-required
    {
        
            let old = expected_eq.pack_plan_digest.clone();
            expected_eq.pack_plan_digest = "tampered".to_string();
            let res = receipt_index.validate_sync(&expected_eq, &expected_artifacts, &expected_templates, &ordered_paths);
            assert!(matches!(res, Err(ReceiptValidationError::PackPlanDigestMismatch)));
            expected_eq.pack_plan_digest = old;
        
    }
    
    // pack-descriptor-digest-required
    {
        
            let old = expected_eq.pack_descriptor_digest.clone();
            expected_eq.pack_descriptor_digest = "tampered".to_string();
            let res = receipt_index.validate_sync(&expected_eq, &expected_artifacts, &expected_templates, &ordered_paths);
            assert!(matches!(res, Err(ReceiptValidationError::PackDescriptorDigestMismatch)));
            expected_eq.pack_descriptor_digest = old;
        
    }
    
    // template-digest-required
    {
        
            let old = expected_templates.get(&first_path).unwrap().clone();
            expected_templates.insert(first_path.clone(), "tampered".to_string());
            let res = receipt_index.validate_sync(&expected_eq, &expected_artifacts, &expected_templates, &ordered_paths);
            assert!(matches!(res, Err(ReceiptValidationError::TemplateDigestMismatch)));
            expected_templates.insert(first_path.clone(), old);
        
    }
    
    // customization-digest-required
    {
        
            let old = expected_eq.customization_digest.clone();
            expected_eq.customization_digest = "tampered".to_string();
            let res = receipt_index.validate_sync(&expected_eq, &expected_artifacts, &expected_templates, &ordered_paths);
            assert!(matches!(res, Err(ReceiptValidationError::CustomizationDigestMismatch)));
            expected_eq.customization_digest = old;
        
    }
    
    // staging-digest-required
    {
        
            let old = expected_eq.staging_digest.clone();
            expected_eq.staging_digest = "tampered".to_string();
            let res = receipt_index.validate_sync(&expected_eq, &expected_artifacts, &expected_templates, &ordered_paths);
            assert!(matches!(res, Err(ReceiptValidationError::StagingDigestMismatch)));
            expected_eq.staging_digest = old;
        
    }
    
    // mutation-gate-decision-required
    {
        
            let old = expected_eq.mutation_gate_decision.clone();
            expected_eq.mutation_gate_decision = "denied".to_string();
            let mut modified_index = receipt_index.clone();
            modified_index.receipts.get_mut(&first_path).unwrap().mutation_gate_decision = "denied".to_string();
            let res = modified_index.validate_sync(&expected_eq, &expected_artifacts, &expected_templates, &ordered_paths);
            assert!(matches!(res, Err(ReceiptValidationError::MutationGateDenied)));
            expected_eq.mutation_gate_decision = old;
        
    }
    
    // verification-result-required
    {
        
            let old = expected_eq.verification_result.clone();
            expected_eq.verification_result = "failed".to_string();
            let mut modified_index = receipt_index.clone();
            modified_index.receipts.get_mut(&first_path).unwrap().verification_result = "failed".to_string();
            let res = modified_index.validate_sync(&expected_eq, &expected_artifacts, &expected_templates, &ordered_paths);
            assert!(matches!(res, Err(ReceiptValidationError::VerificationFailed)));
            expected_eq.verification_result = old;
        
    }
    
    // projection-engine-version-required
    {
        
            let old = expected_eq.projection_engine_version.clone();
            expected_eq.projection_engine_version = "tampered".to_string();
            let res = receipt_index.validate_sync(&expected_eq, &expected_artifacts, &expected_templates, &ordered_paths);
            assert!(matches!(res, Err(ReceiptValidationError::ProjectionEngineMismatch)));
            expected_eq.projection_engine_version = old;
        
    }
    
    // after-the-fact-hash-laundering-refused
    {
        
            let old = expected_artifacts.get(&first_path).unwrap().clone();
            expected_artifacts.insert(first_path.clone(), "tampered".to_string());
            let res = receipt_index.validate_sync(&expected_eq, &expected_artifacts, &expected_templates, &ordered_paths);
            assert!(matches!(res, Err(ReceiptValidationError::AfterTheFactReceiptLaundering)));
            expected_artifacts.insert(first_path.clone(), old);
        
    }
    
    // receipt-chain-broken-refused
    {
        
            if ordered_paths.len() > 1 {
                let second_path = ordered_paths[1].clone();
                let mut modified_index = receipt_index.clone();
                modified_index.receipts.get_mut(&second_path).unwrap().previous_receipt = Some("tampered".to_string());
                let res = modified_index.validate_sync(&expected_eq, &expected_artifacts, &expected_templates, &ordered_paths);
                assert!(matches!(res, Err(ReceiptValidationError::ReceiptChainBroken)));
            }
        
    }
    
}