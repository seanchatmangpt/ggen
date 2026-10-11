//! Generated receipt contracts.

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OpenReceipt { pub sequence: u64, pub action_digest: String, pub bundle_digest: String, pub previous_close_digest: Option<String> }

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CloseReceipt { pub sequence: u64, pub open_digest: String, pub action_digest: String, pub bundle_digest: String, pub effect_succeeded: bool, pub postcondition_satisfied: bool, pub final_state_digest: String }

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Level5GenerationReceipt {
    pub source_graph_digest: String,
    pub shapes_digest: String,
    pub query_inventory_digest: String,
    pub template_inventory_digest: String,
    pub ggen_binary_digest: String,
    pub output_inventory_digest: String,
    pub falsifier_report_digest: String,
    pub replay_report_digest: String,
    pub verifier_report_digest: String,
}

pub fn canonical_digest<T: Serialize>(value: &T) -> Result<String, serde_json::Error> {
    let bytes = serde_json::to_vec(value)?;
    Ok(blake3::hash(&bytes).to_hex().to_string())
}
