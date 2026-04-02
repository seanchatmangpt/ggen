//! Compliance control evaluation

use serde::{Deserialize, Serialize};

/// Control status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ControlStatus {
    /// Control implemented and verified
    Compliant,
    /// Control not implemented or failed verification
    NonCompliant,
    /// Control implementation in progress
    InProgress,
    /// Control not applicable to this system
    NotApplicable,
}

impl std::fmt::Display for ControlStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ControlStatus::Compliant => write!(f, "Compliant"),
            ControlStatus::NonCompliant => write!(f, "Non-Compliant"),
            ControlStatus::InProgress => write!(f, "In Progress"),
            ControlStatus::NotApplicable => write!(f, "Not Applicable"),
        }
    }
}

/// Single compliance control
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Control {
    /// Control ID (e.g., "AC-2" for access control)
    pub id: String,
    /// Control title
    pub title: String,
    /// Detailed description
    pub description: String,
    /// Current status
    pub status: ControlStatus,
    /// Implementation evidence
    pub evidence: Vec<String>,
    /// Last evaluated date (ISO 8601)
    pub last_evaluated: String,
}

impl Control {
    /// Create new control
    pub fn new(id: String, title: String, description: String) -> Self {
        Self {
            id,
            title,
            description,
            status: ControlStatus::NonCompliant,
            evidence: Vec::new(),
            last_evaluated: chrono::Utc::now().to_rfc3339(),
        }
    }

    /// Set control status
    pub fn with_status(mut self, status: ControlStatus) -> Self {
        self.status = status;
        self.last_evaluated = chrono::Utc::now().to_rfc3339();
        self
    }

    /// Add evidence
    pub fn add_evidence(mut self, evidence: String) -> Self {
        self.evidence.push(evidence);
        self
    }

    /// Is control compliant
    pub fn is_compliant(&self) -> bool {
        self.status == ControlStatus::Compliant
    }
}

/// Result of evaluating a control
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ControlResult {
    /// Control ID
    pub control_id: String,
    /// Evaluation status
    pub status: ControlStatus,
    /// Evaluation message
    pub message: String,
    /// Evidence collected
    pub evidence: Vec<String>,
    /// Evaluation timestamp
    pub evaluated_at: String,
}

impl ControlResult {
    /// Create compliant result
    pub fn compliant(control_id: String, message: String) -> Self {
        Self {
            control_id,
            status: ControlStatus::Compliant,
            message,
            evidence: Vec::new(),
            evaluated_at: chrono::Utc::now().to_rfc3339(),
        }
    }

    /// Create non-compliant result
    pub fn non_compliant(control_id: String, message: String) -> Self {
        Self {
            control_id,
            status: ControlStatus::NonCompliant,
            message,
            evidence: Vec::new(),
            evaluated_at: chrono::Utc::now().to_rfc3339(),
        }
    }

    /// Add evidence
    pub fn with_evidence(mut self, evidence: String) -> Self {
        self.evidence.push(evidence);
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_control_creation() {
        let control = Control::new(
            "AC-2".to_string(),
            "Account Management".to_string(),
            "Organization manages information system accounts".to_string(),
        );
        assert_eq!(control.id, "AC-2");
        assert!(!control.is_compliant());
    }

    #[test]
    fn test_control_status_update() {
        let control = Control::new(
            "AC-2".to_string(),
            "Account Management".to_string(),
            "Description".to_string(),
        )
        .with_status(ControlStatus::Compliant);
        assert!(control.is_compliant());
    }

    #[test]
    fn test_control_result_compliant() {
        let result = ControlResult::compliant(
            "AC-2".to_string(),
            "Account management controls verified".to_string(),
        );
        assert_eq!(result.status, ControlStatus::Compliant);
    }

    #[test]
    fn test_control_result_with_evidence() {
        let result = ControlResult::compliant("AC-2".to_string(), "Test".to_string())
            .with_evidence("Log audit verified".to_string());
        assert_eq!(result.evidence.len(), 1);
    }

    #[test]
    fn test_control_status_display() {
        assert_eq!(ControlStatus::Compliant.to_string(), "Compliant");
        assert_eq!(ControlStatus::NonCompliant.to_string(), "Non-Compliant");
        assert_eq!(ControlStatus::InProgress.to_string(), "In Progress");
        assert_eq!(ControlStatus::NotApplicable.to_string(), "Not Applicable");
    }
}
