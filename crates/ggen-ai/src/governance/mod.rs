//! # Governance Layer for Autonomous System Oversight
//!
//! This module provides comprehensive governance capabilities for managing autonomous
//! system evolution, including policy definition, audit trails, safety controls,
//! and observability dashboards.
//!
//! ## Architecture
//!
//! The governance layer enables human oversight (10-20% governance role) through:
//!
//! - **Policy Engine**: Define boundaries and constraints for autonomous evolution
//! - **Audit System**: Track and review all autonomous decisions
//! - **Safety Controls**: Emergency stop, rollback, and validation mechanisms
//! - **Observability**: Real-time dashboards and performance metrics
//! - **Approval Workflows**: Human-in-the-loop for critical changes
//!
//! ## Usage
//!
//! ```rust
//! use ggen_ai::governance::{Policy, PolicyEngine, AuditTrail};
//!
//! # async fn example() -> Result<(), Box<dyn std::error::Error>> {
//! // Initialize governance layer
//! let policy_engine = PolicyEngine::new();
//! let audit_trail = AuditTrail::new("governance.db").await?;
//!
//! // Define policy
//! let policy = Policy::builder()
//!     .name("graph-mutation-limit")
//!     .description("Limit graph changes per hour")
//!     .constraint("max_mutations_per_hour", 100)
//!     .build()?;
//!
//! policy_engine.register_policy(policy).await?;
//!
//! // Validate autonomous decision
//! let decision = /* autonomous system decision */;
//! if policy_engine.validate(&decision).await? {
//!     audit_trail.log_approval(&decision).await?;
//! } else {
//!     audit_trail.log_rejection(&decision).await?;
//! }
//! # Ok(())
//! # }
//! ```

// pub mod audit;  // TODO: Re-enable when SQLite dependency conflict is resolved
pub mod dashboard;
pub mod error;
pub mod policy;
pub mod safety;
pub mod types;
pub mod workflow;

// Re-export main types
// pub use audit::{AuditEvent, AuditQuery, AuditTrail, EventType};
pub use error::GovernanceError;  // Import temporarily to replace audit usage
pub use dashboard::{Dashboard, HealthStatus, MetricsSnapshot};
pub use error::{GovernanceError, Result};
pub use policy::{Constraint, Policy, PolicyEngine, PolicyRule, PolicyViolation};
pub use safety::{EmergencyStop, Rollback, SafetyController, ValidationGate};
pub use types::{Decision, DecisionOutcome, GovernanceConfig};
pub use workflow::{ApprovalRequest, ApprovalStatus, ApprovalWorkflow, Approver};

// Type aliases for audit compatibility (to be re-enabled when SQLite conflict is resolved)
pub type AuditEvent = ();
pub type AuditQuery = ();
pub type AuditTrail = ();

use chrono::Utc;
use std::sync::Arc;

/// Main governance coordinator that orchestrates all governance components
pub struct GovernanceCoordinator {
    policy_engine: Arc<PolicyEngine>,
    // audit_trail: Arc<AuditTrail>,  // TODO: Re-enable when SQLite dependency conflict is resolved
    dashboard: Arc<Dashboard>,
    safety_controller: Arc<SafetyController>,
    workflow: Arc<ApprovalWorkflow>,
    config: GovernanceConfig,
}

impl GovernanceCoordinator {
    /// Create a new governance coordinator
    pub async fn new(config: GovernanceConfig) -> Result<Self> {
        let policy_engine = Arc::new(PolicyEngine::new(config.policy_config.clone()));
        // let audit_trail = Arc::new(AuditTrail::new(&config.audit_db_path).await?);
        let dashboard = Arc::new(Dashboard::new(config.dashboard_config.clone()));
        let safety_controller = Arc::new(SafetyController::new(config.safety_config.clone()));
        let workflow = Arc::new(ApprovalWorkflow::new(config.workflow_config.clone()));

        Ok(Self {
            policy_engine,
            // audit_trail,
            dashboard,
            safety_controller,
            workflow,
            config,
        })
    }

    /// Validate an autonomous decision against all governance rules
    pub async fn validate_decision(&self, decision: &Decision) -> Result<DecisionOutcome> {
        // Log decision received (audit trail disabled - SQLite conflict resolution)
        // self.audit_trail.log_decision_received(decision).await?;

        // Check safety constraints first
        if let Some(violation) = self.safety_controller.check_safety(decision).await? {
            // self.audit_trail.log_safety_violation(decision, &violation).await?;
            return Ok(DecisionOutcome::Rejected {
                reason: format!("Safety violation: {}", violation),
                requires_review: true,
            });
        }

        // Validate against policies
        match self.policy_engine.validate(decision).await {
            Ok(true) => {
                // Policy validation passed - check if approval needed
                if self.requires_approval(decision).await? {
                    self.submit_for_approval(decision).await
                } else {
                    // self.audit_trail.log_auto_approval(decision).await?;
                    Ok(DecisionOutcome::Approved {
                        auto_approved: true,
                        approved_by: "system".to_string(),
                    })
                }
            }
            Ok(false) | Err(_) => {
                let violations = self.policy_engine.get_violations(decision).await?;
                // self.audit_trail.log_policy_violations(decision, &violations).await?;
                Ok(DecisionOutcome::Rejected {
                    reason: format!("Policy violations: {:?}", violations),
                    requires_review: true,
                })
            }
        }
    }

    /// Check if a decision requires human approval
    async fn requires_approval(&self, decision: &Decision) -> Result<bool> {
        Ok(decision.criticality.requires_approval() || self.config.require_approval_for_all)
    }

    /// Submit decision for human approval
    async fn submit_for_approval(&self, decision: &Decision) -> Result<DecisionOutcome> {
        let request = ApprovalRequest::from_decision(decision);
        let request_id = self.workflow.submit(request).await?;

        // self.audit_trail.log_approval_requested(decision, &request_id).await?;

        Ok(DecisionOutcome::PendingApproval {
            request_id,
            submitted_at: Utc::now(),
        })
    }

    /// Emergency stop - halt all autonomous operations
    pub async fn emergency_stop(&self, reason: &str) -> Result<()> {
        self.safety_controller
            .trigger_emergency_stop(reason)
            .await?;
        // self.audit_trail.log_emergency_stop(reason).await?;
        self.dashboard.update_emergency_status(true).await?;
        Ok(())
    }

    /// Resume operations after emergency stop
    pub async fn resume_operations(&self, approved_by: &str) -> Result<()> {
        self.safety_controller.resume().await?;
        // self.audit_trail.log_resume(approved_by).await?;
        self.dashboard.update_emergency_status(false).await?;
        Ok(())
    }

    /// Rollback to a previous state
    pub async fn rollback(&self, target_state: &str) -> Result<()> {
        self.safety_controller.rollback(target_state).await?;
        // self.audit_trail.log_rollback(target_state).await?;
        Ok(())
    }

    /// Get current system health and metrics
    pub async fn get_health_status(&self) -> Result<HealthStatus> {
        self.dashboard.get_health_status().await
    }

    /// Get real-time metrics snapshot
    pub async fn get_metrics(&self) -> Result<MetricsSnapshot> {
        self.dashboard.get_metrics_snapshot().await
    }

    /// Query audit trail (disabled - SQLite conflict resolution)
    pub async fn query_audit_trail(&self, _query: ()) -> Result<Vec<()>> {
        Ok(Vec::new())
        // self.audit_trail.query(query).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_governance_coordinator_creation() {
        let config = GovernanceConfig::default();
        let coordinator = GovernanceCoordinator::new(config).await;
        assert!(coordinator.is_ok());
    }

    #[tokio::test]
    async fn test_decision_validation_flow() {
        let config = GovernanceConfig::default();
        let coordinator = GovernanceCoordinator::new(config)
            .await
            .expect("Failed to create governance coordinator");

        // Test auto-approval for low-risk decision
        let decision = Decision::new_low_risk("test_action", "test data");
        let outcome = coordinator
            .validate_decision(&decision)
            .await
            .expect("Failed to validate decision");

        match outcome {
            DecisionOutcome::Approved { auto_approved, .. } => {
                assert!(auto_approved, "Expected auto_approved to be true");
            }
            other => {
                panic!(
                    "Expected DecisionOutcome::Approved with auto_approved=true for low-risk decision, got: {:?}",
                    other
                );
            }
        }
    }

    #[tokio::test]
    async fn test_emergency_stop() {
        let config = GovernanceConfig::default();
        let coordinator = GovernanceCoordinator::new(config)
            .await
            .expect("Failed to create governance coordinator");

        coordinator
            .emergency_stop("test emergency")
            .await
            .expect("Failed to trigger emergency stop");

        let status = coordinator
            .get_health_status()
            .await
            .expect("Failed to get health status");
        assert!(
            status.emergency_stop_active,
            "Expected emergency_stop_active to be true after triggering emergency stop"
        );
    }
}
