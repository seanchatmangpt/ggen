//! Workspace add-on integration.
//!
//! Type-first design: Workspace state and operations are strongly typed.

use crate::auth::OAuth2Manager;
use crate::error::{MarketplaceError, Result};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// Workspace add-on status.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum AddonStatus {
    /// Add-on is installed and active
    Active,
    /// Add-on is pending installation
    Pending,
    /// Add-on is disabled
    Disabled,
    /// Add-on is being removed
    Removing,
}

/// Workspace add-on configuration.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AddonConfig {
    /// Add-on ID
    pub addon_id: String,
    /// Workspace ID
    pub workspace_id: String,
    /// Status
    pub status: AddonStatus,
    /// Configuration parameters
    pub config: serde_json::Value,
    /// Installation time
    pub installed_at: DateTime<Utc>,
    /// Last update time
    pub updated_at: DateTime<Utc>,
}

/// Workspace resource limits.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceLimits {
    /// Maximum users
    pub max_users: u32,
    /// Maximum storage (bytes)
    pub max_storage_bytes: u64,
    /// Maximum API calls per day
    pub max_api_calls_daily: u64,
    /// Maximum concurrent operations
    pub max_concurrent_ops: u32,
}

impl Default for ResourceLimits {
    fn default() -> Self {
        Self {
            max_users: 10,
            max_storage_bytes: 1024 * 1024 * 1024, // 1GB
            max_api_calls_daily: 10_000,
            max_concurrent_ops: 5,
        }
    }
}

/// Workspace metadata.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Workspace {
    /// Workspace ID
    pub id: String,
    /// Workspace name
    pub name: String,
    /// Owner account ID
    pub owner_id: String,
    /// Resource limits
    pub limits: ResourceLimits,
    /// Installed add-ons
    pub addons: Vec<AddonConfig>,
    /// Creation time
    pub created_at: DateTime<Utc>,
    /// Last update time
    pub updated_at: DateTime<Utc>,
}

impl Workspace {
    /// Check if add-on is installed.
    #[must_use]
    pub fn has_addon(&self, addon_id: &str) -> bool {
        self.addons
            .iter()
            .any(|a| a.addon_id == addon_id && a.status == AddonStatus::Active)
    }

    /// Get add-on configuration.
    #[must_use]
    pub fn get_addon(&self, addon_id: &str) -> Option<&AddonConfig> {
        self.addons
            .iter()
            .find(|a| a.addon_id == addon_id && a.status == AddonStatus::Active)
    }
}

/// Workspace service for managing workspace add-ons.
pub struct WorkspaceService {
    client: reqwest::Client,
    auth_manager: OAuth2Manager,
    api_base_url: String,
}

impl WorkspaceService {
    /// Create a new workspace service.
    #[must_use]
    pub fn new(auth_manager: OAuth2Manager, api_base_url: String) -> Self {
        Self {
            client: reqwest::Client::new(),
            auth_manager,
            api_base_url,
        }
    }

    /// Get workspace by ID.
    ///
    /// # Errors
    /// Returns error if request fails or workspace not found.
    pub async fn get_workspace(&self, workspace_id: &str) -> Result<Workspace> {
        let token = self.auth_manager.get_valid_token().await?;
        let url = format!("{}/workspaces/{workspace_id}", self.api_base_url);

        let response = self
            .client
            .get(&url)
            .bearer_auth(&token.access_token)
            .send()
            .await?;

        if response.status() == reqwest::StatusCode::NOT_FOUND {
            return Err(MarketplaceError::NotFound(format!(
                "Workspace {workspace_id} not found"
            )));
        }

        if !response.status().is_success() {
            return Err(MarketplaceError::WorkspaceError(format!(
                "Failed to get workspace: {}",
                response.status()
            )));
        }

        let workspace: Workspace = response.json().await?;
        Ok(workspace)
    }

    /// Create a new workspace.
    ///
    /// # Errors
    /// Returns error if request fails.
    pub async fn create_workspace(
        &self,
        name: String,
        owner_id: String,
        limits: Option<ResourceLimits>,
    ) -> Result<Workspace> {
        let token = self.auth_manager.get_valid_token().await?;
        let url = format!("{}/workspaces", self.api_base_url);

        let workspace_id = Uuid::new_v4().to_string();

        let request_body = serde_json::json!({
            "id": workspace_id,
            "name": name,
            "owner_id": owner_id,
            "limits": limits.unwrap_or_default(),
        });

        let response = self
            .client
            .post(&url)
            .bearer_auth(&token.access_token)
            .json(&request_body)
            .send()
            .await?;

        if !response.status().is_success() {
            return Err(MarketplaceError::WorkspaceError(format!(
                "Failed to create workspace: {}",
                response.status()
            )));
        }

        let workspace: Workspace = response.json().await?;
        Ok(workspace)
    }

    /// Install add-on to workspace.
    ///
    /// # Errors
    /// Returns error if request fails or addon already installed.
    pub async fn install_addon(
        &self,
        workspace_id: &str,
        addon_id: &str,
        config: serde_json::Value,
    ) -> Result<AddonConfig> {
        let token = self.auth_manager.get_valid_token().await?;
        let url = format!("{}/workspaces/{workspace_id}/addons", self.api_base_url);

        let request_body = serde_json::json!({
            "addon_id": addon_id,
            "config": config,
        });

        let response = self
            .client
            .post(&url)
            .bearer_auth(&token.access_token)
            .json(&request_body)
            .send()
            .await?;

        if response.status() == reqwest::StatusCode::CONFLICT {
            return Err(MarketplaceError::WorkspaceError(format!(
                "Add-on {addon_id} already installed in workspace {workspace_id}"
            )));
        }

        if !response.status().is_success() {
            return Err(MarketplaceError::WorkspaceError(format!(
                "Failed to install add-on: {}",
                response.status()
            )));
        }

        let addon: AddonConfig = response.json().await?;
        Ok(addon)
    }

    /// Uninstall add-on from workspace.
    ///
    /// # Errors
    /// Returns error if request fails.
    pub async fn uninstall_addon(&self, workspace_id: &str, addon_id: &str) -> Result<()> {
        let token = self.auth_manager.get_valid_token().await?;
        let url = format!(
            "{}/workspaces/{workspace_id}/addons/{addon_id}",
            self.api_base_url
        );

        let response = self
            .client
            .delete(&url)
            .bearer_auth(&token.access_token)
            .send()
            .await?;

        if !response.status().is_success() {
            return Err(MarketplaceError::WorkspaceError(format!(
                "Failed to uninstall add-on: {}",
                response.status()
            )));
        }

        Ok(())
    }

    /// Update add-on configuration.
    ///
    /// # Errors
    /// Returns error if request fails.
    pub async fn update_addon_config(
        &self,
        workspace_id: &str,
        addon_id: &str,
        config: serde_json::Value,
    ) -> Result<AddonConfig> {
        let token = self.auth_manager.get_valid_token().await?;
        let url = format!(
            "{}/workspaces/{workspace_id}/addons/{addon_id}",
            self.api_base_url
        );

        let request_body = serde_json::json!({
            "config": config,
        });

        let response = self
            .client
            .patch(&url)
            .bearer_auth(&token.access_token)
            .json(&request_body)
            .send()
            .await?;

        if !response.status().is_success() {
            return Err(MarketplaceError::WorkspaceError(format!(
                "Failed to update add-on config: {}",
                response.status()
            )));
        }

        let addon: AddonConfig = response.json().await?;
        Ok(addon)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_workspace_has_addon() {
        // Arrange
        let workspace = Workspace {
            id: "ws-1".to_string(),
            name: "Test Workspace".to_string(),
            owner_id: "owner-1".to_string(),
            limits: ResourceLimits::default(),
            addons: vec![AddonConfig {
                addon_id: "addon-1".to_string(),
                workspace_id: "ws-1".to_string(),
                status: AddonStatus::Active,
                config: serde_json::json!({}),
                installed_at: Utc::now(),
                updated_at: Utc::now(),
            }],
            created_at: Utc::now(),
            updated_at: Utc::now(),
        };

        // Act & Assert
        assert!(workspace.has_addon("addon-1"));
        assert!(!workspace.has_addon("addon-2"));
    }

    #[test]
    fn test_workspace_get_addon() {
        // Arrange
        let now = Utc::now();
        let addon = AddonConfig {
            addon_id: "addon-1".to_string(),
            workspace_id: "ws-1".to_string(),
            status: AddonStatus::Active,
            config: serde_json::json!({"key": "value"}),
            installed_at: now,
            updated_at: now,
        };

        let workspace = Workspace {
            id: "ws-1".to_string(),
            name: "Test Workspace".to_string(),
            owner_id: "owner-1".to_string(),
            limits: ResourceLimits::default(),
            addons: vec![addon.clone()],
            created_at: now,
            updated_at: now,
        };

        // Act
        let retrieved = workspace.get_addon("addon-1");

        // Assert
        assert!(retrieved.is_some());
        assert_eq!(retrieved.unwrap().addon_id, "addon-1");
    }

    #[test]
    fn test_resource_limits_default() {
        // Arrange & Act
        let limits = ResourceLimits::default();

        // Assert
        assert_eq!(limits.max_users, 10);
        assert_eq!(limits.max_storage_bytes, 1024 * 1024 * 1024);
        assert_eq!(limits.max_api_calls_daily, 10_000);
        assert_eq!(limits.max_concurrent_ops, 5);
    }

    #[test]
    fn test_addon_status_equality() {
        // Arrange & Act & Assert
        assert_eq!(AddonStatus::Active, AddonStatus::Active);
        assert_ne!(AddonStatus::Active, AddonStatus::Pending);
    }
}
