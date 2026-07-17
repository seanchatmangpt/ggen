// Hook verdict and diagnostic records

use crate::term::Triple;
use serde::{Deserialize, Serialize};
use std::fmt;

use super::{EffectKind, HookCondition, HookId};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct HookError {
    pub detail: String,
}

impl fmt::Display for HookError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.detail)
    }
}

impl std::error::Error for HookError {}

impl From<String> for HookError {
    fn from(s: String) -> Self {
        HookError { detail: s }
    }
}

impl From<&str> for HookError {
    fn from(s: &str) -> Self {
        HookError {
            detail: s.to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GraphDelta {
    pub additions: Vec<Triple>,
    pub removals: Vec<Triple>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum HookVerdict {
    Fired,
    NotFired,
    Gated,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DiagnosticDetail {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub focus_node: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub result_path: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub value: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub severity: Option<String>,
    pub message: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TriggerDiagnostic {
    pub hook_iri: String,
    pub conforms: bool,
    pub details: Vec<DiagnosticDetail>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct HookVerdictRecord {
    pub hook_id: HookId,
    pub hook_iri: String,
    pub hook_name: String,
    pub condition_kind: String,
    pub condition_hash: String,
    pub verdict: HookVerdict,
    pub effect: EffectKind,
    pub action_iri: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub diagnostics: Option<TriggerDiagnostic>,
    pub delta_hash: Option<String>,
    pub idempotency_key: Option<String>,
}

impl HookVerdictRecord {
    pub fn delta_hash(&self) -> Option<String> {
        self.delta_hash.clone()
    }

    pub fn idempotency_key(&self) -> Option<String> {
        self.idempotency_key.clone()
    }
}

impl HookCondition {
    pub fn kind(&self) -> &'static str {
        match self {
            Self::Datalog { .. } => "datalog",
            Self::Delta { .. } => "delta",
            Self::Threshold { .. } => "threshold",
            Self::Count { .. } => "count",
            Self::Window { .. } => "window",
            Self::Shacl { .. } => "shacl",
            Self::Shex { .. } => "shex",
            Self::N3 { .. } => "n3",
            Self::Sparql { .. } => "sparql",
        }
    }

    pub fn condition_hash(&self) -> Result<String, String> {
        let json = serde_json::to_string(self).map_err(|e| format!("serialize failed: {}", e))?;
        use sha2::{Digest, Sha256};
        let mut hasher = Sha256::new();
        hasher.update(json.as_bytes());
        let hash = hasher.finalize();
        let mut s = String::new();
        for byte in hash {
            use std::fmt::Write;
            let _ = write!(s, "{:02x}", byte);
        }
        Ok(s)
    }
}

pub fn hook_hash(records: &[HookVerdictRecord]) -> Result<String, String> {
    let json = serde_json::to_string(records).map_err(|e| format!("serialize failed: {}", e))?;
    use sha2::{Digest, Sha256};
    let mut hasher = Sha256::new();
    hasher.update(json.as_bytes());
    let hash = hasher.finalize();
    let mut s = String::new();
    for byte in hash {
        use std::fmt::Write;
        let _ = write!(s, "{:02x}", byte);
    }
    Ok(s)
}
