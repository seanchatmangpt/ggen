use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct CustomizationPoint {
    pub name: String,
    pub description: String,
    pub expected_type: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ProjectionSignature {
    pub pack_id: String,
    pub template_path: String,
    pub expected_hash: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PackActionIntent {
    pub intent_id: String,
    pub title: String,
    pub edits: HashMap<String, String>, // path -> new content (simplified)
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PackFinding {
    pub code: String, // e.g., CLAP-PACK-NOUN-MISSING
    pub message: String,
    pub severity: u8, // 1: Error, 2: Warning, etc.
    pub line: u32,
    pub intents: Vec<PackActionIntent>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PackObservation {
    pub pack_id: String,
    pub source_id: String,
    pub domain: String,
    pub document_uri: String,
    pub findings: Vec<PackFinding>,
    pub projection_signatures: Vec<ProjectionSignature>,
    pub customization_points: Vec<CustomizationPoint>,
    pub action_intents: Vec<PackActionIntent>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct GgenObservedDiagnostic {
    pub source_id: String,
    pub pack_id: String,
    pub domain_code: String,
    pub projection_state: String,
    pub receipt_state: String,
    pub boundary_state: String,
    pub finding: PackFinding,
}
