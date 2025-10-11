//! Common types for governance system

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use super::dashboard::DashboardConfig;
use super::policy::PolicyConfig;
use super::safety::SafetyConfig;
use super::workflow::CriticalityLevel;
use super::workflow::WorkflowConfig;
use crate::types::{DecisionId, RequestId};

/// Main governance configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GovernanceConfig {
    pub audit_db_path: String,
    pub policy_config: PolicyConfig,
    pub dashboard_config: DashboardConfig,
    pub safety_config: SafetyConfig,
    pub workflow_config: WorkflowConfig,
    pub require_approval_for_all: bool,
    pub enable_auto_policies: bool,
}

impl Default for GovernanceConfig {
    fn default() -> Self {
        Self {
            audit_db_path: ".governance/audit.db".to_string(),
            policy_config: PolicyConfig::default(),
            dashboard_config: DashboardConfig::default(),
            safety_config: SafetyConfig::default(),
            workflow_config: WorkflowConfig::default(),
            require_approval_for_all: false,
            enable_auto_policies: true,
        }
    }
}

/// Autonomous system decision
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Decision {
    pub id: String,
    pub action: String,
    pub description: String,
    pub criticality: CriticalityLevel,
    pub timestamp: DateTime<Utc>,
    pub data: serde_json::Value,
    pub metadata: HashMap<String, String>,
}

impl Decision {
    /// Create a new decision
    pub fn new(
        action: impl Into<String>, description: impl Into<String>, criticality: CriticalityLevel,
        data: serde_json::Value,
    ) -> Self {
        Self {
            id: DecisionId::new().to_string(),
            action: action.into(),
            description: description.into(),
            criticality,
            timestamp: Utc::now(),
            data,
            metadata: HashMap::new(),
        }
    }

    /// Create a low-risk decision (auto-approved)
    pub fn new_low_risk(action: impl Into<String>, data: impl Into<String>) -> Self {
        Self::new(
            action,
            "Low risk decision",
            CriticalityLevel::Low,
            serde_json::json!({"data": data.into()}),
        )
    }

    /// Create a high-risk decision (requires approval)
    pub fn new_high_risk(action: impl Into<String>, description: impl Into<String>) -> Self {
        Self::new(
            action,
            description,
            CriticalityLevel::High,
            serde_json::json!({}),
        )
    }

    /// Add metadata
    pub fn with_metadata(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.metadata.insert(key.into(), value.into());
        self
    }
}

/// Outcome of a governance decision
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum DecisionOutcome {
    Approved {
        auto_approved: bool,
        approved_by: String,
    },
    Rejected {
        reason: String,
        requires_review: bool,
    },
    PendingApproval {
        request_id: RequestId,
        submitted_at: DateTime<Utc>,
    },
}

/// Graph evolution event
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphEvolution {
    pub id: String,
    pub timestamp: DateTime<Utc>,
    pub evolution_type: EvolutionType,
    pub before_state: serde_json::Value,
    pub after_state: serde_json::Value,
    pub decision_id: String,
    pub approved_by: Option<String>,
}

/// Type of graph evolution
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum EvolutionType {
    SchemaExtension,
    NodeAddition,
    EdgeAddition,
    NodeModification,
    EdgeModification,
    NodeDeletion,
    EdgeDeletion,
    PropertyUpdate,
}

/// Compliance rule
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceRule {
    pub id: String,
    pub name: String,
    pub description: String,
    pub rule_type: ComplianceType,
    pub validation_script: String,
    pub severity: ComplianceSeverity,
    pub enabled: bool,
}

/// Type of compliance
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ComplianceType {
    DataPrivacy,
    DataRetention,
    AccessControl,
    AuditRequirement,
    SecurityPolicy,
    RegulatoryCompliance,
}

/// Compliance severity
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, PartialOrd)]
#[serde(rename_all = "lowercase")]
pub enum ComplianceSeverity {
    Info,
    Warning,
    Error,
    Critical,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decision_creation() {
        let decision = Decision::new_low_risk("test_action", "test data");
        assert_eq!(decision.action, "test_action");
        assert_eq!(decision.criticality, CriticalityLevel::Low);
    }

    #[test]
    fn test_decision_with_metadata() {
        let decision = Decision::new_high_risk("critical_action", "Critical operation")
            .with_metadata("requester", "admin")
            .with_metadata("reason", "system maintenance");

        assert_eq!(
            decision.metadata.get("requester"),
            Some(&"admin".to_string()),
            "Expected requester metadata to be 'admin'"
        );
        assert_eq!(decision.criticality, CriticalityLevel::High);
    }
}
