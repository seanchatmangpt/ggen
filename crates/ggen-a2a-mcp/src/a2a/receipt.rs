use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Avatar8 {
    ProductManager,
    DataScientist,
    SecurityAuditor,
    FrontendDev,
    BackendDev,
    DevOpsEngineer,
    ComplianceOfficer,
    SystemArchitect,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub enum Jtbd8 {
    Analyze_Metrics,
    Audit_Logs,
    Design_Schema,
    Implement_Cache,
    Verify_Compliance,
    Optimize_Frontend,
    Deploy_K8s,
    Setup_Alerts,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum A2AState {
    CreatedOnly,
    ArtifactEmitted,
    Validated,
    Closed,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum A2ARefusalState {
    TaskMissing,
    ArtifactMissing,
    ReceiptMissing,
    RoleBoundaryViolation,
    MCPToolNotAllowed,
    SyntheticTaskClosure,
    RateLimitBlocked,
    OcelMissing,
    OcelHashMismatch,
    OcelPathMismatch,
    ReceiptHashMismatch,
    BoundaryEvidenceMissing,
    ExpectedOCELMissing,
    ToolCallHashMissing,
    ReceiptChainMissing,
    ReceiptScaffoldRefused,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OCELObjectRef {
    pub id: String,
    pub r#type: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub qualifier: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OCELEvent {
    pub id: String,
    pub activity: String,
    pub timestamp: String,
    pub objects: Vec<OCELObjectRef>,
    pub attributes: std::collections::HashMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OCELObject {
    pub id: String,
    pub r#type: String,
}

/// A receipt-bound object-centric trace slice.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReceiptOcelSlice {
    pub schema: String,
    pub events: Vec<OCELEvent>,
    pub objects: Vec<OCELObject>,
    pub canonical_hash: String,
}

impl ReceiptOcelSlice {
    pub fn verify_canonical_hash(&self) -> bool {
        let mut clone_for_hash = self.clone();
        clone_for_hash.canonical_hash = "".to_string(); // Clear before hashing
        let json_bytes = serde_json::to_vec(&clone_for_hash).unwrap_or_default();
        let computed = blake3::hash(&json_bytes).to_hex().to_string();
        computed == self.canonical_hash
    }

    pub fn compute_and_set_hash(&mut self) {
        self.canonical_hash = "".to_string();
        let json_bytes = serde_json::to_vec(&self).unwrap_or_default();
        self.canonical_hash = blake3::hash(&json_bytes).to_hex().to_string();
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpInvocationEvidence {
    pub server: String,
    pub transport: String,
    pub tool_name: String,
    pub tool_call_hash: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExpectedPathEvidence {
    pub route_id: String,
    pub expected_ocel_hash: Option<String>,
    pub expected_artifact: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ObservedPathEvidence {
    pub ocel: ReceiptOcelSlice,
    pub observed_ocel_hash: String,
    pub observed_artifact_hash: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AlignmentEvidence {
    pub expected_vs_observed: String,
    pub missing_events: Vec<String>,
    pub unexpected_events: Vec<String>,
    pub refusal_state: Option<A2ARefusalState>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationResult {
    pub admitted: bool,
    pub refusal_state: Option<A2ARefusalState>,
}

impl ValidationResult {
    pub fn admitted() -> Self {
        Self {
            admitted: true,
            refusal_state: None,
        }
    }

    pub fn refused(state: A2ARefusalState) -> Self {
        Self {
            admitted: false,
            refusal_state: Some(state),
        }
    }
}

/// A2A task receipt with embedded object-centric path evidence.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct A2ATaskReceipt {
    pub receipt_type: String,
    pub receipt_schema: String,
    pub hash_algorithm: String,

    pub avatar: Avatar8,
    pub jtbd: Jtbd8,
    pub task_id: String,
    pub status: A2AState,

    pub mcp: McpInvocationEvidence,
    pub expected_path: ExpectedPathEvidence,
    pub observed_path: ObservedPathEvidence,
    pub alignment: AlignmentEvidence,

    pub refusal_state: Option<A2ARefusalState>,
    pub previous_receipt_hash: Option<String>,
    pub receipt_hash: String,
}

impl A2ATaskReceipt {
    pub fn verify(&self) -> ValidationResult {
        if !self.observed_path.ocel.verify_canonical_hash() {
            return ValidationResult::refused(A2ARefusalState::OcelHashMismatch);
        }

        if self.mcp.tool_call_hash.is_none() {
            return ValidationResult::refused(A2ARefusalState::BoundaryEvidenceMissing);
        }

        if self.expected_path.expected_ocel_hash.is_none() {
            return ValidationResult::refused(A2ARefusalState::ExpectedOCELMissing);
        }

        if self.status == A2AState::Closed && self.observed_path.observed_artifact_hash.is_none() {
            return ValidationResult::refused(A2ARefusalState::ArtifactMissing);
        }

        if !self.verify_receipt_hash() {
            return ValidationResult::refused(A2ARefusalState::ReceiptHashMismatch);
        }

        ValidationResult::admitted()
    }

    pub fn verify_receipt_hash(&self) -> bool {
        let mut clone_for_hash = self.clone();
        clone_for_hash.receipt_hash = "".to_string(); // Clear before hashing
        let json_bytes = serde_json::to_vec(&clone_for_hash).unwrap_or_default();
        let computed = blake3::hash(&json_bytes).to_hex().to_string();
        computed == self.receipt_hash
    }

    pub fn compute_and_set_hash(&mut self) {
        self.receipt_hash = "".to_string();
        let json_bytes = serde_json::to_vec(&self).unwrap_or_default();
        self.receipt_hash = blake3::hash(&json_bytes).to_hex().to_string();
    }
}
