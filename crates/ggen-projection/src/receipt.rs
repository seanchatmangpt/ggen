use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Receipt {
    pub target_id: String,   // Output file path or Pack ID
    pub receipt_id: String,  // Unique UUID
    
    // GC003 Equation Fields
    #[serde(default)] pub boundary_digest: String,
    #[serde(default)] pub workspace_digest: String,
    #[serde(default)] pub pack_plan_digest: String,
    #[serde(default)] pub pack_descriptor_digest: String,
    #[serde(default)] pub template_digest: Option<String>,
    #[serde(default)] pub customization_digest: String,
    pub blake3_hash: String, // Artifact digest
    #[serde(default)] pub staging_digest: String,
    #[serde(default)] pub mutation_gate_decision: String,
    #[serde(default)] pub verification_result: String,
    #[serde(default)] pub projection_engine_version: String,
    
    pub signature: Option<String>,
    pub verified_at: chrono::DateTime<chrono::Utc>,
    
    #[serde(default)] pub previous_receipt: Option<String>,
}

#[derive(Debug, Clone, Default)]
pub struct EquationContext {
    pub boundary_digest: String,
    pub workspace_digest: String,
    pub pack_plan_digest: String,
    pub pack_descriptor_digest: String,
    pub customization_digest: String,
    pub staging_digest: String,
    pub mutation_gate_decision: String,
    pub verification_result: String,
    pub projection_engine_version: String,
}

pub type CryptographicReceipt = Receipt;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ReceiptIndex {
    pub receipts: HashMap<String, Receipt>,
    pub last_updated: chrono::DateTime<chrono::Utc>,
    pub index_hash: String, // Blake3 hash of the combined index
}

#[derive(Debug, thiserror::Error)]
pub enum ReceiptValidationError {
    #[error("Boundary digest mismatch")] BoundaryDigestMismatch,
    #[error("Workspace digest mismatch")] WorkspaceDigestMismatch,
    #[error("Pack plan digest mismatch")] PackPlanDigestMismatch,
    #[error("Pack descriptor digest mismatch")] PackDescriptorDigestMismatch,
    #[error("Template digest mismatch")] TemplateDigestMismatch,
    #[error("Customization digest mismatch")] CustomizationDigestMismatch,
    #[error("Staging digest mismatch")] StagingDigestMismatch,
    #[error("Mutation gate missing")] MutationGateMissing,
    #[error("Mutation gate denied")] MutationGateDenied,
    #[error("Verification missing")] VerificationMissing,
    #[error("Verification failed")] VerificationFailed,
    #[error("Projection engine mismatch")] ProjectionEngineMismatch,
    #[error("Receipt chain broken")] ReceiptChainBroken,
    #[error("After-the-fact receipt laundering")] AfterTheFactReceiptLaundering,
}

impl Default for ReceiptIndex {
    fn default() -> Self {
        Self::new()
    }
}

impl ReceiptIndex {
    pub fn new() -> Self {
        Self {
            receipts: HashMap::new(),
            last_updated: chrono::Utc::now(),
            index_hash: String::new(),
        }
    }

    pub fn add_receipt(&mut self, path: String, content: &[u8], template_content: &[u8], eq: &EquationContext, previous_receipt: Option<String>) {
        let hash_str = blake3::hash(content).to_hex().to_string();
        let template_hash_str = blake3::hash(template_content).to_hex().to_string();
        let receipt = Receipt {
            target_id: path.clone(),
            receipt_id: uuid::Uuid::new_v4().to_string(),
            blake3_hash: hash_str,
            template_digest: Some(template_hash_str),
            boundary_digest: eq.boundary_digest.clone(),
            workspace_digest: eq.workspace_digest.clone(),
            pack_plan_digest: eq.pack_plan_digest.clone(),
            pack_descriptor_digest: eq.pack_descriptor_digest.clone(),
            customization_digest: eq.customization_digest.clone(),
            staging_digest: eq.staging_digest.clone(),
            mutation_gate_decision: eq.mutation_gate_decision.clone(),
            verification_result: eq.verification_result.clone(),
            projection_engine_version: eq.projection_engine_version.clone(),
            signature: None,
            verified_at: chrono::Utc::now(),
            previous_receipt,
        };
        self.receipts.insert(path, receipt);
        self.update_index_hash();
    }

    pub fn update_index_hash(&mut self) {
        let mut sorted_receipts: Vec<_> = self.receipts.iter().collect();
        sorted_receipts.sort_by(|a, b| a.0.cmp(b.0));
        let mut combined = Vec::new();
        for (k, r) in sorted_receipts {
            combined.extend_from_slice(k.as_bytes());
            combined.extend_from_slice(r.blake3_hash.as_bytes());
            if let Some(td) = &r.template_digest {
                combined.extend_from_slice(td.as_bytes());
            }
            combined.extend_from_slice(r.boundary_digest.as_bytes());
            combined.extend_from_slice(r.workspace_digest.as_bytes());
            combined.extend_from_slice(r.pack_plan_digest.as_bytes());
            combined.extend_from_slice(r.pack_descriptor_digest.as_bytes());
            combined.extend_from_slice(r.customization_digest.as_bytes());
            combined.extend_from_slice(r.staging_digest.as_bytes());
            combined.extend_from_slice(r.mutation_gate_decision.as_bytes());
            combined.extend_from_slice(r.verification_result.as_bytes());
            combined.extend_from_slice(r.projection_engine_version.as_bytes());
            if let Some(pr) = &r.previous_receipt {
                combined.extend_from_slice(pr.as_bytes());
            }
            combined.extend_from_slice(r.receipt_id.as_bytes());
        }
        self.index_hash = blake3::hash(&combined).to_hex().to_string();
        self.last_updated = chrono::Utc::now();
    }

    pub fn validate_sync(&self, expected_eq: &EquationContext, expected_artifact_digests: &HashMap<String, String>, expected_template_digests: &HashMap<String, String>, ordered_paths: &[String]) -> Result<(), ReceiptValidationError> {
        let mut expected_previous: Option<String> = None;
        for path in ordered_paths {
            let receipt = self.receipts.get(path).ok_or(ReceiptValidationError::AfterTheFactReceiptLaundering)?;
            if receipt.boundary_digest != expected_eq.boundary_digest { return Err(ReceiptValidationError::BoundaryDigestMismatch); }
            if receipt.workspace_digest != expected_eq.workspace_digest { return Err(ReceiptValidationError::WorkspaceDigestMismatch); }
            if receipt.pack_plan_digest != expected_eq.pack_plan_digest { return Err(ReceiptValidationError::PackPlanDigestMismatch); }
            if receipt.pack_descriptor_digest != expected_eq.pack_descriptor_digest { return Err(ReceiptValidationError::PackDescriptorDigestMismatch); }
            if receipt.customization_digest != expected_eq.customization_digest { return Err(ReceiptValidationError::CustomizationDigestMismatch); }
            if receipt.staging_digest != expected_eq.staging_digest { return Err(ReceiptValidationError::StagingDigestMismatch); }
            if receipt.projection_engine_version != expected_eq.projection_engine_version { return Err(ReceiptValidationError::ProjectionEngineMismatch); }
            
            if receipt.mutation_gate_decision.is_empty() { return Err(ReceiptValidationError::MutationGateMissing); }
            if receipt.mutation_gate_decision != "admitted" { return Err(ReceiptValidationError::MutationGateDenied); }
            
            if receipt.verification_result.is_empty() { return Err(ReceiptValidationError::VerificationMissing); }
            if receipt.verification_result != "passed" { return Err(ReceiptValidationError::VerificationFailed); }
            
            if let Some(expected_td) = expected_template_digests.get(path) {
                if receipt.template_digest.as_deref() != Some(expected_td.as_str()) {
                    return Err(ReceiptValidationError::TemplateDigestMismatch);
                }
            } else {
                return Err(ReceiptValidationError::TemplateDigestMismatch);
            }
            
            if let Some(expected_ad) = expected_artifact_digests.get(path) {
                if receipt.blake3_hash != *expected_ad {
                    return Err(ReceiptValidationError::AfterTheFactReceiptLaundering);
                }
            } else {
                return Err(ReceiptValidationError::AfterTheFactReceiptLaundering);
            }
            
            if receipt.previous_receipt != expected_previous {
                return Err(ReceiptValidationError::ReceiptChainBroken);
            }
            
            expected_previous = Some(receipt.receipt_id.clone());
        }
        Ok(())
    }

    pub fn from_json(content: &str) -> Result<Self, anyhow::Error> {
        let index: Self = serde_json::from_str(content)?;
        Ok(index)
    }

    pub fn write_to_file(&self, path: &Path) -> Result<(), anyhow::Error> {
        let serialized = serde_json::to_string_pretty(self)?;
        std::fs::write(path, serialized)?;
        Ok(())
    }
}