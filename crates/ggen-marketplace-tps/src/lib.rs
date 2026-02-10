//! Marketplace/workspace economy integration.
//!
//! This crate provides integration with GCP Marketplace, workspace add-ons,
//! and CLM (Customer Lifecycle Management) proxy for routing human interactions.
//!
//! # Architecture
//!
//! - `auth`: OAuth2 authentication manager
//! - `entitlements`: GCP Marketplace entitlement checks
//! - `workspace`: Workspace add-on integration
//! - `clm_proxy`: CLM pass-off surface for human interaction
//! - `error`: Error types and result definitions
//!
//! # Type-First Design
//!
//! All state is encoded in types. The compiler enforces correctness.
//! No `unwrap()` or `expect()` - all operations return `Result<T, E>`.

#![deny(
    missing_docs,
    unsafe_code,
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic
)]

pub mod auth;
pub mod clm_proxy;
pub mod entitlements;
pub mod error;
pub mod workspace;

use auth::{OAuth2Config, OAuth2Manager};
use clm_proxy::ClmProxy;
use entitlements::EntitlementService;
use error::Result;
use workspace::WorkspaceService;

/// Marketplace client configuration.
#[derive(Debug, Clone)]
pub struct MarketplaceConfig {
    /// OAuth2 configuration
    pub oauth2: OAuth2Config,
    /// GCP Marketplace API base URL
    pub marketplace_api_url: String,
    /// Workspace API base URL
    pub workspace_api_url: String,
    /// CLM API base URL
    pub clm_api_url: String,
}

/// Marketplace client for managing all marketplace operations.
///
/// This is the main entry point for marketplace integration.
pub struct MarketplaceClient {
    auth_manager: OAuth2Manager,
    entitlement_service: EntitlementService,
    workspace_service: WorkspaceService,
    clm_proxy: ClmProxy,
}

impl MarketplaceClient {
    /// Create a new marketplace client.
    #[must_use]
    pub fn new(config: MarketplaceConfig) -> Self {
        let auth_manager = OAuth2Manager::new(config.oauth2);
        let entitlement_service =
            EntitlementService::new(auth_manager.clone(), config.marketplace_api_url);
        let workspace_service =
            WorkspaceService::new(auth_manager.clone(), config.workspace_api_url);
        let clm_proxy = ClmProxy::new(auth_manager.clone(), config.clm_api_url);

        Self {
            auth_manager,
            entitlement_service,
            workspace_service,
            clm_proxy,
        }
    }

    /// Get auth manager.
    #[must_use]
    pub const fn auth(&self) -> &OAuth2Manager {
        &self.auth_manager
    }

    /// Get entitlement service.
    #[must_use]
    pub const fn entitlements(&self) -> &EntitlementService {
        &self.entitlement_service
    }

    /// Get workspace service.
    #[must_use]
    pub const fn workspace(&self) -> &WorkspaceService {
        &self.workspace_service
    }

    /// Get CLM proxy.
    #[must_use]
    pub const fn clm(&self) -> &ClmProxy {
        &self.clm_proxy
    }

    /// Initialize authentication flow.
    ///
    /// # Errors
    /// Returns error if URL generation fails.
    pub fn start_auth(&self, state: &str) -> Result<String> {
        self.auth_manager.get_authorization_url(state)
    }

    /// Complete authentication with authorization code.
    ///
    /// # Errors
    /// Returns error if token exchange fails.
    pub async fn complete_auth(&self, code: &str) -> Result<()> {
        self.auth_manager.exchange_code(code).await?;
        Ok(())
    }

    /// Check if client is authenticated.
    ///
    /// # Errors
    /// Returns error if token validation fails.
    pub async fn is_authenticated(&self) -> Result<bool> {
        match self.auth_manager.get_valid_token().await {
            Ok(_) => Ok(true),
            Err(_) => Ok(false),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_marketplace_client_creation() {
        // Arrange
        let config = MarketplaceConfig {
            oauth2: OAuth2Config {
                client_id: "test".to_string(),
                client_secret: "secret".to_string(),
                auth_url: "https://example.com/auth".to_string(),
                token_url: "https://example.com/token".to_string(),
                redirect_uri: "https://app.example.com/callback".to_string(),
                scopes: vec!["read".to_string()],
            },
            marketplace_api_url: "https://marketplace.example.com".to_string(),
            workspace_api_url: "https://workspace.example.com".to_string(),
            clm_api_url: "https://clm.example.com".to_string(),
        };

        // Act
        let client = MarketplaceClient::new(config);

        // Assert
        assert!(client.start_auth("test_state").is_ok());
    }

    #[tokio::test]
    async fn test_marketplace_client_auth_check() {
        // Arrange
        let config = MarketplaceConfig {
            oauth2: OAuth2Config {
                client_id: "test".to_string(),
                client_secret: "secret".to_string(),
                auth_url: "https://example.com/auth".to_string(),
                token_url: "https://example.com/token".to_string(),
                redirect_uri: "https://app.example.com/callback".to_string(),
                scopes: vec!["read".to_string()],
            },
            marketplace_api_url: "https://marketplace.example.com".to_string(),
            workspace_api_url: "https://workspace.example.com".to_string(),
            clm_api_url: "https://clm.example.com".to_string(),
        };

        let client = MarketplaceClient::new(config);

        // Act
        let is_authed = client.is_authenticated().await.unwrap();

        // Assert - should not be authenticated without token
        assert!(!is_authed);
    }
}
