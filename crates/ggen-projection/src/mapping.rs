use crate::receipt::{ReceiptIndex, EquationContext};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ProjectionMapping {
    pub pack_id: String,
    pub template_path: PathBuf,
    pub query_path: Option<PathBuf>,
    pub bound_variables: Vec<String>,
    pub merge_strategy: String, // E.g., "Exclusive", "Mergeable", "Overlay"
    #[serde(default)]
    pub start_line: Option<usize>,
    #[serde(default)]
    pub end_line: Option<usize>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ProjectionMap {
    pub mappings: HashMap<PathBuf, ProjectionMapping>,
}

impl Default for ProjectionMap {
    fn default() -> Self {
        Self::new()
    }
}

impl ProjectionMap {
    pub fn new() -> Self {
        Self {
            mappings: HashMap::new(),
        }
    }

    pub fn add_mapping(
        &mut self, target_path: PathBuf, mapping: ProjectionMapping,
    ) -> Result<(), anyhow::Error> {
        if let (Some(s1), Some(e1)) = (mapping.start_line, mapping.end_line) {
            for (path, existing) in &self.mappings {
                if path == &target_path {
                    if let (Some(s2), Some(e2)) = (existing.start_line, existing.end_line) {
                        if s1 <= e2 && s2 <= e1 {
                            return Err(anyhow::anyhow!(
                                "Overlapping range conflict in target file {:?}: [{}:{}] overlaps with existing [{}:{}]",
                                target_path, s1, e1, s2, e2
                            ));
                        }
                    }
                }
            }
        }
        self.mappings.insert(target_path, mapping);
        Ok(())
    }

    pub fn validate_sync(
        &self, output_dir: &Path, receipts: &ReceiptIndex, equation: Option<&EquationContext>
    ) -> Result<(), anyhow::Error> {
        for target_path in self.mappings.keys() {
            let full_path = output_dir.join(target_path);
            if !full_path.exists() {
                return Err(anyhow::anyhow!(
                    "Stale projection map: file {:?} does not exist on disk",
                    target_path
                ));
            }
            let content = std::fs::read(&full_path)?;
            let current_hash = blake3::hash(&content).to_hex().to_string();
            let key = target_path.to_string_lossy().into_owned();
            if let Some(receipt) = receipts.receipts.get(&key) {
                if receipt.template_digest.is_none() || receipt.template_digest.as_deref() == Some("") {
                    return Err(anyhow::anyhow!(
                        "Stale projection map: file {:?} has invalid or missing template_digest in receipt",
                        target_path
                    ));
                }
                if receipt.blake3_hash != current_hash {
                    return Err(anyhow::anyhow!(
                        "Stale projection map: file {:?} on disk is modified/out-of-sync (hash {} vs hash {})",
                        target_path, current_hash, receipt.blake3_hash
                    ));
                }
                
                if let Some(eq) = equation {
                    if receipt.boundary_digest != eq.boundary_digest {
                        return Err(anyhow::anyhow!("Equation enforcement failed: boundary_digest mismatch"));
                    }
                    if receipt.workspace_digest != eq.workspace_digest {
                        return Err(anyhow::anyhow!("Equation enforcement failed: workspace_digest mismatch"));
                    }
                    if receipt.pack_plan_digest != eq.pack_plan_digest {
                        return Err(anyhow::anyhow!("Equation enforcement failed: pack_plan_digest mismatch"));
                    }
                    if receipt.pack_descriptor_digest != eq.pack_descriptor_digest {
                        return Err(anyhow::anyhow!("Equation enforcement failed: pack_descriptor_digest mismatch"));
                    }
                    if receipt.customization_digest != eq.customization_digest {
                        return Err(anyhow::anyhow!("Equation enforcement failed: customization_digest mismatch"));
                    }
                    if receipt.staging_digest != eq.staging_digest {
                        return Err(anyhow::anyhow!("Equation enforcement failed: staging_digest mismatch"));
                    }
                    if receipt.mutation_gate_decision != eq.mutation_gate_decision {
                        return Err(anyhow::anyhow!("Equation enforcement failed: mutation_gate_decision mismatch"));
                    }
                    if receipt.verification_result != eq.verification_result {
                        return Err(anyhow::anyhow!("Equation enforcement failed: verification_result mismatch"));
                    }
                    if receipt.projection_engine_version != eq.projection_engine_version {
                        return Err(anyhow::anyhow!("Equation enforcement failed: projection_engine_version mismatch"));
                    }
                }
            } else {
                return Err(anyhow::anyhow!(
                    "Stale projection map: file {:?} has no cryptographic receipt",
                    target_path
                ));
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct CustomizationMap {
    pub vars: HashMap<String, String>, // Variable name -> Override value
    pub file_overrides: HashMap<PathBuf, String>, // File path -> Specific merge rules
}

impl Default for CustomizationMap {
    fn default() -> Self {
        Self::new()
    }
}

impl CustomizationMap {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            file_overrides: HashMap::new(),
        }
    }

    pub fn validate(&self) -> Result<(), anyhow::Error> {
        for key in self.vars.keys() {
            if key.trim().is_empty() {
                return Err(anyhow::anyhow!("Customization point name cannot be empty"));
            }
        }
        Ok(())
    }

    pub fn incomplete_slots(&self) -> Vec<String> {
        let mut incomplete = Vec::new();
        for (k, v) in &self.vars {
            if v.trim().is_empty() || v.contains("TODO") || v.contains("placeholder") {
                incomplete.push(k.clone());
            }
        }
        incomplete
    }
}
