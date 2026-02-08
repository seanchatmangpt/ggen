//! Compliance evidence collection and correlation

use serde::{Deserialize, Serialize};

/// Compliance evidence for control verification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceEvidence {
    /// Evidence ID
    pub id: String,
    /// Related control ID
    pub control_id: String,
    /// Evidence type (e.g., "log", "configuration", "test", "audit")
    pub evidence_type: String,
    /// Evidence description
    pub description: String,
    /// Evidence content/data
    pub content: String,
    /// Evidence source (e.g., file path, system log, test report)
    pub source: String,
    /// Collection timestamp (ISO 8601)
    pub collected_at: String,
    /// Evidence validity (days until expiry)
    pub valid_for_days: u32,
}

impl ComplianceEvidence {
    /// Create new evidence
    pub fn new(
        control_id: String, evidence_type: String, description: String, content: String,
        source: String,
    ) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            control_id,
            evidence_type,
            description,
            content,
            source,
            collected_at: chrono::Utc::now().to_rfc3339(),
            valid_for_days: 90, // Default 90 days validity
        }
    }

    /// Set validity period
    pub fn with_validity(mut self, days: u32) -> Self {
        self.valid_for_days = days;
        self
    }

    /// Check if evidence is still valid
    pub fn is_valid(&self) -> bool {
        if let Ok(collected) = chrono::DateTime::parse_from_rfc3339(&self.collected_at) {
            let expiry = collected + chrono::Duration::days(self.valid_for_days as i64);
            expiry > chrono::Utc::now()
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_evidence_creation() {
        let evidence = ComplianceEvidence::new(
            "AC-2".to_string(),
            "log".to_string(),
            "Account access logs".to_string(),
            "logs from 2026-01-25".to_string(),
            "/var/log/auth.log".to_string(),
        );
        assert_eq!(evidence.control_id, "AC-2");
        assert!(evidence.is_valid());
    }

    #[test]
    fn test_evidence_with_validity() {
        let evidence = ComplianceEvidence::new(
            "SI-4".to_string(),
            "configuration".to_string(),
            "Monitoring config".to_string(),
            "enabled".to_string(),
            "/etc/monitoring/config.yaml".to_string(),
        )
        .with_validity(365);
        assert_eq!(evidence.valid_for_days, 365);
    }
}
