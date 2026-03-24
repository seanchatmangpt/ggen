//! CLM (Customer Lifecycle Management) proxy pass-off surface.
//!
//! All human interaction routes through this proxy.
//! Type-first design: Interaction state is encoded in types.

use crate::auth::OAuth2Manager;
use crate::error::{MarketplaceError, Result};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// Interaction type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum InteractionType {
    /// Support request
    Support,
    /// Sales inquiry
    Sales,
    /// Feature request
    Feature,
    /// Bug report
    Bug,
    /// Billing question
    Billing,
    /// General inquiry
    General,
}

/// Interaction priority.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, PartialOrd, Ord)]
#[serde(rename_all = "lowercase")]
pub enum Priority {
    /// Low priority
    Low,
    /// Normal priority
    Normal,
    /// High priority
    High,
    /// Critical priority
    Critical,
}

/// Interaction status.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum InteractionStatus {
    /// Interaction is open
    Open,
    /// Interaction is in progress
    InProgress,
    /// Interaction is waiting for response
    Waiting,
    /// Interaction is resolved
    Resolved,
    /// Interaction is closed
    Closed,
}

/// Human interaction request.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InteractionRequest {
    /// Interaction ID
    pub id: String,
    /// User ID requesting interaction
    pub user_id: String,
    /// Account ID
    pub account_id: String,
    /// Interaction type
    pub interaction_type: InteractionType,
    /// Priority
    pub priority: Priority,
    /// Subject
    pub subject: String,
    /// Message body
    pub body: String,
    /// Context metadata
    pub context: serde_json::Value,
    /// Creation time
    pub created_at: DateTime<Utc>,
}

impl InteractionRequest {
    /// Create a new interaction request.
    #[must_use]
    pub fn new(
        user_id: String,
        account_id: String,
        interaction_type: InteractionType,
        priority: Priority,
        subject: String,
        body: String,
        context: serde_json::Value,
    ) -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            user_id,
            account_id,
            interaction_type,
            priority,
            subject,
            body,
            context,
            created_at: Utc::now(),
        }
    }
}

/// Interaction response from CLM system.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InteractionResponse {
    /// Interaction ID
    pub interaction_id: String,
    /// Response ID
    pub response_id: String,
    /// Agent/representative ID
    pub agent_id: String,
    /// Response message
    pub message: String,
    /// Status after response
    pub status: InteractionStatus,
    /// Response time
    pub responded_at: DateTime<Utc>,
}

/// CLM proxy for routing human interactions.
pub struct ClmProxy {
    client: reqwest::Client,
    auth_manager: OAuth2Manager,
    clm_base_url: String,
}

impl ClmProxy {
    /// Create a new CLM proxy.
    #[must_use]
    pub fn new(auth_manager: OAuth2Manager, clm_base_url: String) -> Self {
        Self {
            client: reqwest::Client::new(),
            auth_manager,
            clm_base_url,
        }
    }

    /// Submit an interaction request to CLM system.
    ///
    /// # Errors
    /// Returns error if request fails.
    pub async fn submit_interaction(&self, request: InteractionRequest) -> Result<String> {
        let token = self.auth_manager.get_valid_token().await?;
        let url = format!("{}/interactions", self.clm_base_url);

        let response = self
            .client
            .post(&url)
            .bearer_auth(&token.access_token)
            .json(&request)
            .send()
            .await?;

        if !response.status().is_success() {
            return Err(MarketplaceError::ClmProxyError(format!(
                "Failed to submit interaction: {}",
                response.status()
            )));
        }

        let response_data: serde_json::Value = response.json().await?;
        let interaction_id = response_data["id"]
            .as_str()
            .ok_or_else(|| {
                MarketplaceError::ClmProxyError("Missing interaction ID in response".to_string())
            })?
            .to_string();

        Ok(interaction_id)
    }

    /// Get interaction status.
    ///
    /// # Errors
    /// Returns error if request fails.
    pub async fn get_interaction_status(&self, interaction_id: &str) -> Result<InteractionStatus> {
        let token = self.auth_manager.get_valid_token().await?;
        let url = format!("{}/interactions/{interaction_id}", self.clm_base_url);

        let response = self
            .client
            .get(&url)
            .bearer_auth(&token.access_token)
            .send()
            .await?;

        if response.status() == reqwest::StatusCode::NOT_FOUND {
            return Err(MarketplaceError::NotFound(format!(
                "Interaction {interaction_id} not found"
            )));
        }

        if !response.status().is_success() {
            return Err(MarketplaceError::ClmProxyError(format!(
                "Failed to get interaction status: {}",
                response.status()
            )));
        }

        let data: serde_json::Value = response.json().await?;
        let status_str = data["status"]
            .as_str()
            .ok_or_else(|| {
                MarketplaceError::ClmProxyError("Missing status in response".to_string())
            })?;

        let status: InteractionStatus = serde_json::from_value(serde_json::Value::String(
            status_str.to_string(),
        ))?;

        Ok(status)
    }

    /// List responses for an interaction.
    ///
    /// # Errors
    /// Returns error if request fails.
    pub async fn list_responses(
        &self,
        interaction_id: &str,
    ) -> Result<Vec<InteractionResponse>> {
        let token = self.auth_manager.get_valid_token().await?;
        let url = format!("{}/interactions/{interaction_id}/responses", self.clm_base_url);

        let response = self
            .client
            .get(&url)
            .bearer_auth(&token.access_token)
            .send()
            .await?;

        if !response.status().is_success() {
            return Err(MarketplaceError::ClmProxyError(format!(
                "Failed to list responses: {}",
                response.status()
            )));
        }

        let responses: Vec<InteractionResponse> = response.json().await?;
        Ok(responses)
    }

    /// Submit user response to an interaction.
    ///
    /// # Errors
    /// Returns error if request fails.
    pub async fn submit_response(
        &self,
        interaction_id: &str,
        message: String,
    ) -> Result<String> {
        let token = self.auth_manager.get_valid_token().await?;
        let url = format!("{}/interactions/{interaction_id}/responses", self.clm_base_url);

        let request_body = serde_json::json!({
            "message": message,
            "timestamp": Utc::now().to_rfc3339(),
        });

        let response = self
            .client
            .post(&url)
            .bearer_auth(&token.access_token)
            .json(&request_body)
            .send()
            .await?;

        if !response.status().is_success() {
            return Err(MarketplaceError::ClmProxyError(format!(
                "Failed to submit response: {}",
                response.status()
            )));
        }

        let response_data: serde_json::Value = response.json().await?;
        let response_id = response_data["response_id"]
            .as_str()
            .ok_or_else(|| {
                MarketplaceError::ClmProxyError("Missing response ID in response".to_string())
            })?
            .to_string();

        Ok(response_id)
    }

    /// Close an interaction.
    ///
    /// # Errors
    /// Returns error if request fails.
    pub async fn close_interaction(&self, interaction_id: &str) -> Result<()> {
        let token = self.auth_manager.get_valid_token().await?;
        let url = format!("{}/interactions/{interaction_id}/close", self.clm_base_url);

        let response = self
            .client
            .post(&url)
            .bearer_auth(&token.access_token)
            .send()
            .await?;

        if !response.status().is_success() {
            return Err(MarketplaceError::ClmProxyError(format!(
                "Failed to close interaction: {}",
                response.status()
            )));
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_interaction_request_creation() {
        // Arrange & Act
        let request = InteractionRequest::new(
            "user-1".to_string(),
            "account-1".to_string(),
            InteractionType::Support,
            Priority::High,
            "Need help".to_string(),
            "I have a problem".to_string(),
            serde_json::json!({"context": "test"}),
        );

        // Assert
        assert!(!request.id.is_empty());
        assert_eq!(request.user_id, "user-1");
        assert_eq!(request.interaction_type, InteractionType::Support);
        assert_eq!(request.priority, Priority::High);
    }

    #[test]
    fn test_priority_ordering() {
        // Arrange & Act & Assert
        assert!(Priority::Low < Priority::Normal);
        assert!(Priority::Normal < Priority::High);
        assert!(Priority::High < Priority::Critical);
    }

    #[test]
    fn test_interaction_type_equality() {
        // Arrange & Act & Assert
        assert_eq!(InteractionType::Support, InteractionType::Support);
        assert_ne!(InteractionType::Support, InteractionType::Sales);
    }

    #[test]
    fn test_interaction_status_equality() {
        // Arrange & Act & Assert
        assert_eq!(InteractionStatus::Open, InteractionStatus::Open);
        assert_ne!(InteractionStatus::Open, InteractionStatus::Closed);
    }

    #[test]
    fn test_interaction_serialization() {
        // Arrange
        let request = InteractionRequest::new(
            "user-1".to_string(),
            "account-1".to_string(),
            InteractionType::Bug,
            Priority::Critical,
            "Critical bug".to_string(),
            "System crash".to_string(),
            serde_json::json!({}),
        );

        // Act
        let serialized = serde_json::to_string(&request).unwrap();
        let deserialized: InteractionRequest = serde_json::from_str(&serialized).unwrap();

        // Assert
        assert_eq!(request.id, deserialized.id);
        assert_eq!(request.user_id, deserialized.user_id);
        assert_eq!(request.interaction_type, deserialized.interaction_type);
    }
}
