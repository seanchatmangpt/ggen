// Compliance & Audit System - Rust Implementation
// Enterprise compliance and audit trail management

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use chrono::{DateTime, Utc, Duration};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ComplianceFramework {
    SOX,
    GDPR,
    HIPAA,
    SOC2,
    PCIDSS,
    ISO27001,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum EventType {
    UserAction,
    SystemEvent,
    SecurityEvent,
    DataAccess,
    ConfigChange,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Severity {
    Low,
    Medium,
    High,
    Critical,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditEvent {
    pub event_id: String,
    pub event_type: EventType,
    pub timestamp: DateTime<Utc>,
    pub actor: String,
    pub action: String,
    pub resource: String,
    pub outcome: String,
    pub ip_address: Option<String>,
    pub user_agent: Option<String>,
    pub metadata: HashMap<String, String>,
}

impl AuditEvent {
    pub fn new(
        event_id: String,
        event_type: EventType,
        actor: String,
        action: String,
        resource: String,
    ) -> Self {
        Self {
            event_id,
            event_type,
            timestamp: Utc::now(),
            actor,
            action,
            resource,
            outcome: "success".to_string(),
            ip_address: None,
            user_agent: None,
            metadata: HashMap::new(),
        }
    }

    pub fn with_outcome(mut self, outcome: String) -> Self {
        self.outcome = outcome;
        self
    }

    pub fn with_client_info(mut self, ip: String, user_agent: String) -> Self {
        self.ip_address = Some(ip);
        self.user_agent = Some(user_agent);
        self
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Policy {
    pub policy_id: String,
    pub policy_name: String,
    pub framework: ComplianceFramework,
    pub version: String,
    pub effective_date: DateTime<Utc>,
    pub expiration_date: Option<DateTime<Utc>>,
    pub retention_period_days: Option<u32>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PolicyViolation {
    pub violation_id: String,
    pub policy_id: String,
    pub event_id: String,
    pub severity: Severity,
    pub detected_at: DateTime<Utc>,
    pub resolved_at: Option<DateTime<Utc>>,
    pub description: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceIncident {
    pub incident_id: String,
    pub incident_type: String,
    pub severity: Severity,
    pub discovered_at: DateTime<Utc>,
    pub resolved_at: Option<DateTime<Utc>>,
    pub affected_records: Option<u64>,
    pub description: String,
    pub remediation: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AccessReview {
    pub review_id: String,
    pub review_type: String,
    pub due_date: DateTime<Utc>,
    pub completion_date: Option<DateTime<Utc>>,
    pub reviewer: String,
    pub status: String,
    pub items: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Evidence {
    pub evidence_id: String,
    pub evidence_type: String,
    pub collected_at: DateTime<Utc>,
    pub collected_by: String,
    pub hash: String,
    pub metadata: HashMap<String, String>,
}

pub struct ComplianceAuditSystem {
    events: Vec<AuditEvent>,
    policies: HashMap<String, Policy>,
    violations: Vec<PolicyViolation>,
    incidents: Vec<ComplianceIncident>,
}

impl ComplianceAuditSystem {
    pub fn new() -> Self {
        Self {
            events: Vec::new(),
            policies: HashMap::new(),
            violations: Vec::new(),
            incidents: Vec::new(),
        }
    }

    pub fn log_event(&mut self, event: AuditEvent) {
        self.check_policy_compliance(&event);
        self.events.push(event);
    }

    pub fn register_policy(&mut self, policy: Policy) {
        self.policies.insert(policy.policy_id.clone(), policy);
    }

    fn check_policy_compliance(&mut self, event: &AuditEvent) {
        for policy in self.policies.values() {
            if self.violates_policy(event, policy) {
                let violation = PolicyViolation {
                    violation_id: format!("viol-{}", Utc::now().timestamp()),
                    policy_id: policy.policy_id.clone(),
                    event_id: event.event_id.clone(),
                    severity: Severity::Medium,
                    detected_at: Utc::now(),
                    resolved_at: None,
                    description: format!("Violation of policy: {}", policy.policy_name),
                };
                self.violations.push(violation);
            }
        }
    }

    fn violates_policy(&self, _event: &AuditEvent, _policy: &Policy) -> bool {
        false
    }

    pub fn get_recent_events(&self, since: DateTime<Utc>) -> Vec<&AuditEvent> {
        self.events
            .iter()
            .filter(|e| e.timestamp >= since)
            .collect()
    }

    pub fn get_violations(&self, severity: Option<Severity>) -> Vec<&PolicyViolation> {
        self.violations
            .iter()
            .filter(|v| {
                if let Some(s) = &severity {
                    &v.severity == s
                } else {
                    true
                }
            })
            .collect()
    }

    pub fn create_incident(&mut self, incident: ComplianceIncident) {
        self.incidents.push(incident);
    }

    pub fn get_compliance_score(&self, framework: &ComplianceFramework) -> f64 {
        let total_policies: usize = self
            .policies
            .values()
            .filter(|p| &p.framework == framework)
            .count();

        if total_policies == 0 {
            return 100.0;
        }

        let violations: usize = self
            .violations
            .iter()
            .filter(|v| {
                if let Some(policy) = self.policies.get(&v.policy_id) {
                    &policy.framework == framework
                } else {
                    false
                }
            })
            .count();

        100.0 - (violations as f64 / total_policies as f64 * 100.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_audit_event_creation() {
        let event = AuditEvent::new(
            "evt-001".to_string(),
            EventType::UserAction,
            "user-123".to_string(),
            "login".to_string(),
            "/auth".to_string(),
        );

        assert_eq!(event.event_id, "evt-001");
        assert_eq!(event.actor, "user-123");
    }

    #[test]
    fn test_policy_registration() {
        let mut system = ComplianceAuditSystem::new();

        let policy = Policy {
            policy_id: "pol-001".to_string(),
            policy_name: "Data Retention".to_string(),
            framework: ComplianceFramework::GDPR,
            version: "1.0".to_string(),
            effective_date: Utc::now(),
            expiration_date: None,
            retention_period_days: Some(365),
        };

        system.register_policy(policy);
        assert_eq!(system.policies.len(), 1);
    }

    #[test]
    fn test_compliance_score() {
        let system = ComplianceAuditSystem::new();
        let score = system.get_compliance_score(&ComplianceFramework::SOX);
        assert_eq!(score, 100.0);
    }
}
