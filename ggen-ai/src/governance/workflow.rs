//! Approval Workflow for Governance
//!
//! Human-in-the-loop approval system for critical autonomous decisions.

use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use uuid::Uuid;

use super::types::Decision;
use super::error::{GovernanceError, Result};

/// Approval workflow configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowConfig {
    pub default_approvers: Vec<String>,
    pub require_multiple_approvals: bool,
    pub min_approvals_required: usize,
    pub approval_timeout_minutes: u64,
    pub enable_delegation: bool,
}

impl Default for WorkflowConfig {
    fn default() -> Self {
        Self {
            default_approvers: vec!["admin".to_string()],
            require_multiple_approvals: false,
            min_approvals_required: 1,
            approval_timeout_minutes: 60,
            enable_delegation: true,
        }
    }
}

/// Approval request
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApprovalRequest {
    pub id: String,
    pub decision_id: String,
    pub title: String,
    pub description: String,
    pub criticality: CriticalityLevel,
    pub requested_at: DateTime<Utc>,
    pub requested_by: String,
    pub approvers: Vec<String>,
    pub status: ApprovalStatus,
    pub responses: Vec<ApprovalResponse>,
    pub expires_at: DateTime<Utc>,
    pub metadata: HashMap<String, String>,
}

/// Criticality level of a decision
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, PartialOrd)]
#[serde(rename_all = "lowercase")]
pub enum CriticalityLevel {
    Low,
    Medium,
    High,
    Critical,
}

impl CriticalityLevel {
    pub fn requires_approval(&self) -> bool {
        matches!(self, CriticalityLevel::High | CriticalityLevel::Critical)
    }
}

/// Approval status
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum ApprovalStatus {
    Pending,
    Approved,
    Rejected,
    Expired,
    Withdrawn,
}

/// Individual approval response
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApprovalResponse {
    pub approver: String,
    pub decision: ResponseDecision,
    pub responded_at: DateTime<Utc>,
    pub comments: Option<String>,
    pub delegated_from: Option<String>,
}

/// Response decision
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum ResponseDecision {
    Approve,
    Reject,
    Abstain,
}

/// Approver configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Approver {
    pub id: String,
    pub name: String,
    pub email: String,
    pub roles: Vec<String>,
    pub can_delegate: bool,
    pub active: bool,
}

/// Approval workflow engine
pub struct ApprovalWorkflow {
    config: WorkflowConfig,
    requests: Arc<RwLock<HashMap<String, ApprovalRequest>>>,
    approvers: Arc<RwLock<HashMap<String, Approver>>>,
}

impl ApprovalWorkflow {
    /// Create a new approval workflow
    pub fn new(config: WorkflowConfig) -> Self {
        Self {
            config,
            requests: Arc::new(RwLock::new(HashMap::new())),
            approvers: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Register an approver
    pub async fn register_approver(&self, approver: Approver) -> Result<()> {
        let mut approvers = self.approvers.write().await;
        approvers.insert(approver.id.clone(), approver);
        Ok(())
    }

    /// Submit an approval request
    pub async fn submit(&self, mut request: ApprovalRequest) -> Result<String> {
        // Set default approvers if not specified
        if request.approvers.is_empty() {
            request.approvers = self.config.default_approvers.clone();
        }

        // Set expiration
        request.expires_at = Utc::now()
            + chrono::Duration::minutes(self.config.approval_timeout_minutes as i64);

        let request_id = request.id.clone();

        let mut requests = self.requests.write().await;
        requests.insert(request_id.clone(), request.clone());

        // Send notifications to approvers
        self.send_approval_notifications(&request).await?;

        tracing::info!("Approval request submitted: {}", request_id);
        Ok(request_id)
    }

    /// Respond to an approval request
    pub async fn respond(
        &self,
        request_id: &str,
        approver: &str,
        decision: ResponseDecision,
        comments: Option<String>,
    ) -> Result<ApprovalStatus> {
        let mut requests = self.requests.write().await;

        let request = requests
            .get_mut(request_id)
            .ok_or_else(|| GovernanceError::ApprovalRequestNotFound(request_id.to_string()))?;

        // Check if request is still pending
        if request.status != ApprovalStatus::Pending {
            return Err(GovernanceError::ApprovalError(format!(
                "Request is not pending: {:?}",
                request.status
            )));
        }

        // Check if approver is authorized
        if !request.approvers.contains(&approver.to_string()) {
            return Err(GovernanceError::ApprovalError(
                "Approver not authorized for this request".to_string()
            ));
        }

        // Check if already responded
        if request.responses.iter().any(|r| r.approver == approver) {
            return Err(GovernanceError::ApprovalError(
                "Approver has already responded".to_string()
            ));
        }

        // Add response
        request.responses.push(ApprovalResponse {
            approver: approver.to_string(),
            decision: decision.clone(),
            responded_at: Utc::now(),
            comments,
            delegated_from: None,
        });

        // Update status based on responses
        self.update_request_status(request)?;

        tracing::info!(
            "Approval response recorded: {} - {:?}",
            request_id,
            decision
        );

        Ok(request.status.clone())
    }

    /// Update request status based on responses
    fn update_request_status(&self, request: &mut ApprovalRequest) -> Result<()> {
        let approvals = request
            .responses
            .iter()
            .filter(|r| r.decision == ResponseDecision::Approve)
            .count();

        let rejections = request
            .responses
            .iter()
            .filter(|r| r.decision == ResponseDecision::Reject)
            .count();

        // Check if expired
        if Utc::now() > request.expires_at {
            request.status = ApprovalStatus::Expired;
            return Ok(());
        }

        // Check for rejection (any rejection fails the request)
        if rejections > 0 {
            request.status = ApprovalStatus::Rejected;
            return Ok(());
        }

        // Check for approval
        if self.config.require_multiple_approvals {
            if approvals >= self.config.min_approvals_required {
                request.status = ApprovalStatus::Approved;
            }
        } else if approvals > 0 {
            request.status = ApprovalStatus::Approved;
        }

        Ok(())
    }

    /// Get an approval request
    pub async fn get_request(&self, request_id: &str) -> Result<ApprovalRequest> {
        let requests = self.requests.read().await;
        requests
            .get(request_id)
            .cloned()
            .ok_or_else(|| GovernanceError::ApprovalRequestNotFound(request_id.to_string()))
    }

    /// List all approval requests
    pub async fn list_requests(&self, status: Option<ApprovalStatus>) -> Result<Vec<ApprovalRequest>> {
        let requests = self.requests.read().await;

        let filtered: Vec<ApprovalRequest> = requests
            .values()
            .filter(|r| status.as_ref().map(|s| &r.status == s).unwrap_or(true))
            .cloned()
            .collect();

        Ok(filtered)
    }

    /// Withdraw an approval request
    pub async fn withdraw(&self, request_id: &str, withdrawn_by: &str) -> Result<()> {
        let mut requests = self.requests.write().await;

        let request = requests
            .get_mut(request_id)
            .ok_or_else(|| GovernanceError::ApprovalRequestNotFound(request_id.to_string()))?;

        if request.status != ApprovalStatus::Pending {
            return Err(GovernanceError::ApprovalError(
                "Only pending requests can be withdrawn".to_string()
            ));
        }

        if request.requested_by != withdrawn_by {
            return Err(GovernanceError::ApprovalError(
                "Only the requester can withdraw".to_string()
            ));
        }

        request.status = ApprovalStatus::Withdrawn;
        tracing::info!("Approval request withdrawn: {}", request_id);

        Ok(())
    }

    /// Delegate approval to another approver
    pub async fn delegate(
        &self,
        request_id: &str,
        from_approver: &str,
        to_approver: &str,
    ) -> Result<()> {
        if !self.config.enable_delegation {
            return Err(GovernanceError::ApprovalError(
                "Delegation is disabled".to_string()
            ));
        }

        let approvers = self.approvers.read().await;
        let approver = approvers
            .get(from_approver)
            .ok_or_else(|| GovernanceError::ApproverNotFound(from_approver.to_string()))?;

        if !approver.can_delegate {
            return Err(GovernanceError::ApprovalError(
                "Approver does not have delegation permission".to_string()
            ));
        }

        // Update request approvers
        let mut requests = self.requests.write().await;
        let request = requests
            .get_mut(request_id)
            .ok_or_else(|| GovernanceError::ApprovalRequestNotFound(request_id.to_string()))?;

        if let Some(pos) = request.approvers.iter().position(|a| a == from_approver) {
            request.approvers[pos] = to_approver.to_string();
            request.metadata.insert(
                "delegation".to_string(),
                format!("{} -> {}", from_approver, to_approver),
            );

            tracing::info!("Approval delegated: {} -> {}", from_approver, to_approver);
        }

        Ok(())
    }

    /// Clean up expired requests
    pub async fn cleanup_expired(&self) -> Result<usize> {
        let mut requests = self.requests.write().await;
        let now = Utc::now();

        let mut expired_count = 0;

        for request in requests.values_mut() {
            if request.status == ApprovalStatus::Pending && now > request.expires_at {
                request.status = ApprovalStatus::Expired;
                expired_count += 1;
            }
        }

        if expired_count > 0 {
            tracing::info!("Marked {} requests as expired", expired_count);
        }

        Ok(expired_count)
    }

    /// Send notifications to approvers for a new approval request
    async fn send_approval_notifications(&self, request: &ApprovalRequest) -> Result<()> {
        tracing::info!(
            request_id = %request.id,
            approvers = request.approvers.len(),
            "Sending approval notifications"
        );

        let approvers = self.approvers.read().await;

        for approver_id in &request.approvers {
            if let Some(approver) = approvers.get(approver_id) {
                if !approver.active {
                    tracing::warn!(
                        approver_id = %approver_id,
                        "Skipping inactive approver"
                    );
                    continue;
                }

                // Send notification based on approver preferences
                match self.send_notification_to_approver(approver, request).await {
                    Ok(_) => {
                        tracing::debug!(
                            approver_id = %approver_id,
                            approver_email = %approver.email,
                            request_id = %request.id,
                            "Notification sent successfully"
                        );
                    }
                    Err(e) => {
                        tracing::error!(
                            approver_id = %approver_id,
                            error = %e,
                            "Failed to send notification"
                        );
                        // Continue with other approvers even if one fails
                    }
                }
            } else {
                tracing::warn!(
                    approver_id = %approver_id,
                    request_id = %request.id,
                    "Approver not found in registry"
                );
            }
        }

        Ok(())
    }

    /// Send notification to a specific approver
    async fn send_notification_to_approver(
        &self,
        approver: &Approver,
        request: &ApprovalRequest,
    ) -> Result<()> {
        // In a production system, this would integrate with:
        // - Email service (SMTP, SendGrid, etc.)
        // - Slack/Teams integration
        // - SMS service
        // - Push notifications
        // - Webhook callbacks

        tracing::debug!(
            approver_name = %approver.name,
            approver_email = %approver.email,
            request_title = %request.title,
            criticality = ?request.criticality,
            expires_at = %request.expires_at,
            "Preparing notification"
        );

        // Create notification message
        let message = self.format_notification_message(approver, request);

        // Log the notification (in production, this would actually send)
        tracing::info!(
            to = %approver.email,
            subject = format!("Approval Required: {}", request.title),
            message_preview = %message.chars().take(100).collect::<String>(),
            "Notification prepared (would be sent in production)"
        );

        // In a real implementation:
        // 1. Queue the notification for delivery
        // 2. Track delivery status
        // 3. Implement retry logic for failures
        // 4. Support multiple notification channels based on criticality
        // 5. Handle approver preferences (email, slack, etc.)

        Ok(())
    }

    /// Format notification message for approver
    fn format_notification_message(&self, approver: &Approver, request: &ApprovalRequest) -> String {
        format!(
            r#"Hello {},

An approval request requires your attention:

Title: {}
Criticality: {:?}
Requested by: {}
Requested at: {}
Expires at: {}

Description:
{}

Please review and respond to this approval request.

Request ID: {}
"#,
            approver.name,
            request.title,
            request.criticality,
            request.requested_by,
            request.requested_at.format("%Y-%m-%d %H:%M:%S UTC"),
            request.expires_at.format("%Y-%m-%d %H:%M:%S UTC"),
            request.description,
            request.id
        )
    }
}

impl ApprovalRequest {
    /// Create an approval request from a decision
    pub fn from_decision(decision: &Decision) -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            decision_id: decision.id.clone(),
            title: format!("Approval required: {}", decision.action),
            description: decision.description.clone(),
            criticality: decision.criticality.clone(),
            requested_at: Utc::now(),
            requested_by: "autonomous_system".to_string(),
            approvers: Vec::new(),
            status: ApprovalStatus::Pending,
            responses: Vec::new(),
            expires_at: Utc::now() + chrono::Duration::hours(1),
            metadata: decision.metadata.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_approval_workflow() {
        let workflow = ApprovalWorkflow::new(WorkflowConfig::default());

        // Register approver
        let approver = Approver {
            id: "user1".to_string(),
            name: "User One".to_string(),
            email: "user1@example.com".to_string(),
            roles: vec!["admin".to_string()],
            can_delegate: true,
            active: true,
        };
        workflow.register_approver(approver)
            .await
            .expect("Failed to register approver");

        // Submit request
        let request = ApprovalRequest {
            id: Uuid::new_v4().to_string(),
            decision_id: "test-decision".to_string(),
            title: "Test Request".to_string(),
            description: "Test".to_string(),
            criticality: CriticalityLevel::High,
            requested_at: Utc::now(),
            requested_by: "system".to_string(),
            approvers: vec!["user1".to_string()],
            status: ApprovalStatus::Pending,
            responses: Vec::new(),
            expires_at: Utc::now() + chrono::Duration::hours(1),
            metadata: HashMap::new(),
        };

        let request_id = workflow.submit(request)
            .await
            .expect("Failed to submit approval request");

        // Approve
        let status = workflow
            .respond(&request_id, "user1", ResponseDecision::Approve, None)
            .await
            .expect("Failed to respond to approval request");

        assert_eq!(status, ApprovalStatus::Approved);
    }

    #[tokio::test]
    async fn test_approval_rejection() {
        let workflow = ApprovalWorkflow::new(WorkflowConfig::default());

        let request = ApprovalRequest {
            id: Uuid::new_v4().to_string(),
            decision_id: "test-decision".to_string(),
            title: "Test Request".to_string(),
            description: "Test".to_string(),
            criticality: CriticalityLevel::High,
            requested_at: Utc::now(),
            requested_by: "system".to_string(),
            approvers: vec!["user1".to_string()],
            status: ApprovalStatus::Pending,
            responses: Vec::new(),
            expires_at: Utc::now() + chrono::Duration::hours(1),
            metadata: HashMap::new(),
        };

        let request_id = workflow.submit(request)
            .await
            .expect("Failed to submit approval request");

        let status = workflow
            .respond(&request_id, "user1", ResponseDecision::Reject, Some("Not safe".to_string()))
            .await
            .expect("Failed to respond to approval request");

        assert_eq!(status, ApprovalStatus::Rejected);
    }
}
