//! Audit Trail for Governance
//!
//! Comprehensive logging and querying of all governance decisions and events.

use chrono::{DateTime, Duration, Utc};
use serde::{Deserialize, Serialize};
use std::path::Path;
use std::sync::Arc;
use tokio::sync::RwLock;
use uuid::Uuid;

use super::error::{GovernanceError, Result};
use super::policy::PolicyViolation;
use super::types::Decision;

/// Type of audit event
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum EventType {
    DecisionReceived,
    DecisionApproved,
    DecisionRejected,
    DecisionPending,
    PolicyViolation,
    SafetyViolation,
    EmergencyStop,
    EmergencyResume,
    Rollback,
    PolicyRegistered,
    PolicyUpdated,
    PolicyDeleted,
    ApprovalRequested,
    ApprovalGranted,
    ApprovalDenied,
    ConfigurationChanged,
    SystemStartup,
    SystemShutdown,
}

/// Audit event
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditEvent {
    pub id: String,
    pub event_type: EventType,
    pub timestamp: DateTime<Utc>,
    pub actor: String,
    pub decision_id: Option<String>,
    pub policy_id: Option<String>,
    pub severity: AuditSeverity,
    pub message: String,
    pub details: serde_json::Value,
    pub metadata: serde_json::Value,
}

/// Severity of audit event
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, PartialOrd)]
#[serde(rename_all = "lowercase")]
pub enum AuditSeverity {
    Debug,
    Info,
    Warning,
    Error,
    Critical,
}

/// Query for audit trail
#[derive(Debug, Clone, Default)]
pub struct AuditQuery {
    pub event_types: Option<Vec<EventType>>,
    pub start_time: Option<DateTime<Utc>>,
    pub end_time: Option<DateTime<Utc>>,
    pub actor: Option<String>,
    pub decision_id: Option<String>,
    pub policy_id: Option<String>,
    pub severity: Option<AuditSeverity>,
    pub limit: Option<usize>,
    pub offset: Option<usize>,
}

/// Audit trail storage and query interface
pub struct AuditTrail {
    db_path: String,
    events: Arc<RwLock<Vec<AuditEvent>>>,
    retention_days: i64,
}

impl AuditTrail {
    /// Create a new audit trail
    pub async fn new(db_path: impl AsRef<Path>) -> Result<Self> {
        let db_path = db_path.as_ref().to_string_lossy().to_string();

        // Ensure parent directory exists
        if let Some(parent) = Path::new(&db_path).parent() {
            tokio::fs::create_dir_all(parent).await.map_err(|e| {
                GovernanceError::AuditError(format!("Failed to create audit directory: {}", e))
            })?;
        }

        let trail = Self {
            db_path,
            events: Arc::new(RwLock::new(Vec::new())),
            retention_days: 365, // Default 1 year retention
        };

        // Log startup
        trail
            .log_event(AuditEvent {
                id: Uuid::new_v4().to_string(),
                event_type: EventType::SystemStartup,
                timestamp: Utc::now(),
                actor: "system".to_string(),
                decision_id: None,
                policy_id: None,
                severity: AuditSeverity::Info,
                message: "Audit trail initialized".to_string(),
                details: serde_json::json!({"db_path": trail.db_path}),
                metadata: serde_json::json!({}),
            })
            .await?;

        Ok(trail)
    }

    /// Log a generic event
    pub async fn log_event(&self, event: AuditEvent) -> Result<()> {
        let mut events = self.events.write().await;
        events.push(event.clone());

        // TODO: Persist to SQLite database
        tracing::info!("Audit event: {:?}", event.event_type);

        Ok(())
    }

    /// Log decision received
    pub async fn log_decision_received(&self, decision: &Decision) -> Result<()> {
        self.log_event(AuditEvent {
            id: Uuid::new_v4().to_string(),
            event_type: EventType::DecisionReceived,
            timestamp: Utc::now(),
            actor: "autonomous_system".to_string(),
            decision_id: Some(decision.id.clone()),
            policy_id: None,
            severity: AuditSeverity::Info,
            message: format!("Decision received: {}", decision.action),
            details: serde_json::to_value(decision)
                .map_err(|e| GovernanceError::SerializationError(e.to_string()))?,
            metadata: serde_json::json!({}),
        })
        .await
    }

    /// Log auto-approval
    pub async fn log_auto_approval(&self, decision: &Decision) -> Result<()> {
        self.log_event(AuditEvent {
            id: Uuid::new_v4().to_string(),
            event_type: EventType::DecisionApproved,
            timestamp: Utc::now(),
            actor: "system".to_string(),
            decision_id: Some(decision.id.clone()),
            policy_id: None,
            severity: AuditSeverity::Info,
            message: format!("Decision auto-approved: {}", decision.action),
            details: serde_json::to_value(decision)
                .map_err(|e| GovernanceError::SerializationError(e.to_string()))?,
            metadata: serde_json::json!({"auto_approved": true}),
        })
        .await
    }

    /// Log policy violations
    pub async fn log_policy_violations(
        &self, decision: &Decision, violations: &[PolicyViolation],
    ) -> Result<()> {
        for violation in violations {
            self.log_event(AuditEvent {
                id: Uuid::new_v4().to_string(),
                event_type: EventType::PolicyViolation,
                timestamp: Utc::now(),
                actor: "policy_engine".to_string(),
                decision_id: Some(decision.id.clone()),
                policy_id: Some(violation.policy_id.to_string()),
                severity: match violation.severity {
                    super::policy::Severity::Info => AuditSeverity::Info,
                    super::policy::Severity::Warning => AuditSeverity::Warning,
                    super::policy::Severity::Error => AuditSeverity::Error,
                    super::policy::Severity::Critical => AuditSeverity::Critical,
                },
                message: violation.message.clone(),
                details: serde_json::to_value(violation)
                    .map_err(|e| GovernanceError::SerializationError(e.to_string()))?,
                metadata: serde_json::json!({}),
            })
            .await?;
        }
        Ok(())
    }

    /// Log safety violation
    pub async fn log_safety_violation(&self, decision: &Decision, violation: &str) -> Result<()> {
        self.log_event(AuditEvent {
            id: Uuid::new_v4().to_string(),
            event_type: EventType::SafetyViolation,
            timestamp: Utc::now(),
            actor: "safety_controller".to_string(),
            decision_id: Some(decision.id.clone()),
            policy_id: None,
            severity: AuditSeverity::Critical,
            message: violation.to_string(),
            details: serde_json::to_value(decision)
                .map_err(|e| GovernanceError::SerializationError(e.to_string()))?,
            metadata: serde_json::json!({}),
        })
        .await
    }

    /// Log approval requested
    pub async fn log_approval_requested(
        &self, decision: &Decision, request_id: &crate::types::RequestId,
    ) -> Result<()> {
        self.log_event(AuditEvent {
            id: Uuid::new_v4().to_string(),
            event_type: EventType::ApprovalRequested,
            timestamp: Utc::now(),
            actor: "workflow_engine".to_string(),
            decision_id: Some(decision.id.clone()),
            policy_id: None,
            severity: AuditSeverity::Info,
            message: format!("Approval requested for decision: {}", decision.action),
            details: serde_json::json!({"request_id": request_id.to_string()}),
            metadata: serde_json::json!({}),
        })
        .await
    }

    /// Log emergency stop
    pub async fn log_emergency_stop(&self, reason: &str) -> Result<()> {
        self.log_event(AuditEvent {
            id: Uuid::new_v4().to_string(),
            event_type: EventType::EmergencyStop,
            timestamp: Utc::now(),
            actor: "safety_controller".to_string(),
            decision_id: None,
            policy_id: None,
            severity: AuditSeverity::Critical,
            message: format!("Emergency stop triggered: {}", reason),
            details: serde_json::json!({"reason": reason}),
            metadata: serde_json::json!({}),
        })
        .await
    }

    /// Log system resume
    pub async fn log_resume(&self, approved_by: &str) -> Result<()> {
        self.log_event(AuditEvent {
            id: Uuid::new_v4().to_string(),
            event_type: EventType::EmergencyResume,
            timestamp: Utc::now(),
            actor: approved_by.to_string(),
            decision_id: None,
            policy_id: None,
            severity: AuditSeverity::Warning,
            message: format!("System resumed by {}", approved_by),
            details: serde_json::json!({"approved_by": approved_by}),
            metadata: serde_json::json!({}),
        })
        .await
    }

    /// Log rollback
    pub async fn log_rollback(&self, target_state: &str) -> Result<()> {
        self.log_event(AuditEvent {
            id: Uuid::new_v4().to_string(),
            event_type: EventType::Rollback,
            timestamp: Utc::now(),
            actor: "safety_controller".to_string(),
            decision_id: None,
            policy_id: None,
            severity: AuditSeverity::Warning,
            message: format!("System rolled back to state: {}", target_state),
            details: serde_json::json!({"target_state": target_state}),
            metadata: serde_json::json!({}),
        })
        .await
    }

    /// Query audit trail
    pub async fn query(&self, query: AuditQuery) -> Result<Vec<AuditEvent>> {
        let events = self.events.read().await;

        let mut filtered: Vec<AuditEvent> = events
            .iter()
            .filter(|e| {
                // Filter by event type
                if let Some(ref types) = query.event_types {
                    if !types.contains(&e.event_type) {
                        return false;
                    }
                }

                // Filter by time range
                if let Some(start) = query.start_time {
                    if e.timestamp < start {
                        return false;
                    }
                }
                if let Some(end) = query.end_time {
                    if e.timestamp > end {
                        return false;
                    }
                }

                // Filter by actor
                if let Some(ref actor) = query.actor {
                    if &e.actor != actor {
                        return false;
                    }
                }

                // Filter by decision ID
                if let Some(ref decision_id) = query.decision_id {
                    if e.decision_id.as_ref() != Some(decision_id) {
                        return false;
                    }
                }

                // Filter by policy ID
                if let Some(ref policy_id) = query.policy_id {
                    if e.policy_id.as_ref() != Some(policy_id) {
                        return false;
                    }
                }

                // Filter by severity
                if let Some(ref severity) = query.severity {
                    if &e.severity < severity {
                        return false;
                    }
                }

                true
            })
            .cloned()
            .collect();

        // Sort by timestamp (newest first)
        filtered.sort_by(|a, b| b.timestamp.cmp(&a.timestamp));

        // Apply pagination
        let offset = query.offset.unwrap_or(0);
        let limit = query.limit.unwrap_or(usize::MAX);

        Ok(filtered.into_iter().skip(offset).take(limit).collect())
    }

    /// Clean up old events based on retention policy
    pub async fn cleanup_old_events(&self) -> Result<usize> {
        let cutoff = Utc::now() - Duration::days(self.retention_days);
        let mut events = self.events.write().await;

        let original_count = events.len();
        events.retain(|e| e.timestamp > cutoff);
        let removed = original_count - events.len();

        tracing::info!("Cleaned up {} old audit events", removed);
        Ok(removed)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[tokio::test]
    async fn test_audit_trail_creation() {
        let dir = tempdir().expect("Failed to create temp directory");
        let db_path = dir.path().join("audit.db");
        let trail = AuditTrail::new(db_path)
            .await
            .expect("Failed to create audit trail");

        // Should have startup event
        let events = trail
            .query(AuditQuery::default())
            .await
            .expect("Failed to query audit trail");
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].event_type, EventType::SystemStartup);
    }

    #[tokio::test]
    async fn test_audit_query() {
        let dir = tempdir().expect("Failed to create temp directory");
        let db_path = dir.path().join("audit.db");
        let trail = AuditTrail::new(db_path)
            .await
            .expect("Failed to create audit trail");

        // Log some events
        trail
            .log_emergency_stop("test")
            .await
            .expect("Failed to log emergency stop");

        // Query for emergency stops
        let query = AuditQuery {
            event_types: Some(vec![EventType::EmergencyStop]),
            ..Default::default()
        };

        let events = trail
            .query(query)
            .await
            .expect("Failed to query audit trail");
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].event_type, EventType::EmergencyStop);
    }
}
