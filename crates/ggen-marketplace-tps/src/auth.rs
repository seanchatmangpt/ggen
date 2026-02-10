//! OAuth2 authentication for marketplace integration.
//!
//! Type-first design: Authentication state is encoded in types.

use crate::error::{MarketplaceError, Result};
use chrono::{DateTime, Duration, Utc};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::sync::RwLock;

/// OAuth2 token with metadata.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OAuth2Token {
    /// Access token
    pub access_token: String,
    /// Token type (usually "Bearer")
    pub token_type: String,
    /// Expiration time
    pub expires_at: DateTime<Utc>,
    /// Refresh token (optional)
    pub refresh_token: Option<String>,
    /// Scopes granted
    pub scopes: Vec<String>,
}

impl OAuth2Token {
    /// Check if token is expired.
    #[must_use]
    pub fn is_expired(&self) -> bool {
        Utc::now() >= self.expires_at
    }

    /// Check if token is valid (not expired).
    #[must_use]
    pub fn is_valid(&self) -> bool {
        !self.is_expired()
    }

    /// Time until expiration.
    #[must_use]
    pub fn time_until_expiry(&self) -> Duration {
        self.expires_at - Utc::now()
    }
}

/// OAuth2 client configuration.
#[derive(Debug, Clone)]
pub struct OAuth2Config {
    /// Client ID
    pub client_id: String,
    /// Client secret
    pub client_secret: String,
    /// Authorization endpoint
    pub auth_url: String,
    /// Token endpoint
    pub token_url: String,
    /// Redirect URI
    pub redirect_uri: String,
    /// Scopes to request
    pub scopes: Vec<String>,
}

/// OAuth2 authentication manager.
#[derive(Clone)]
pub struct OAuth2Manager {
    config: OAuth2Config,
    client: reqwest::Client,
    token: Arc<RwLock<Option<OAuth2Token>>>,
}

impl OAuth2Manager {
    /// Create a new OAuth2 manager.
    #[must_use]
    pub fn new(config: OAuth2Config) -> Self {
        Self {
            config,
            client: reqwest::Client::new(),
            token: Arc::new(RwLock::new(None)),
        }
    }

    /// Get authorization URL for user consent.
    ///
    /// # Errors
    /// Returns error if URL construction fails.
    pub fn get_authorization_url(&self, state: &str) -> Result<String> {
        let mut url = url::Url::parse(&self.config.auth_url)?;
        url.query_pairs_mut()
            .append_pair("client_id", &self.config.client_id)
            .append_pair("redirect_uri", &self.config.redirect_uri)
            .append_pair("response_type", "code")
            .append_pair("state", state)
            .append_pair("scope", &self.config.scopes.join(" "));

        Ok(url.to_string())
    }

    /// Exchange authorization code for access token.
    ///
    /// # Errors
    /// Returns error if token exchange fails.
    pub async fn exchange_code(&self, code: &str) -> Result<OAuth2Token> {
        let params = [
            ("grant_type", "authorization_code"),
            ("code", code),
            ("redirect_uri", &self.config.redirect_uri),
            ("client_id", &self.config.client_id),
            ("client_secret", &self.config.client_secret),
        ];

        let response = self
            .client
            .post(&self.config.token_url)
            .form(&params)
            .send()
            .await?;

        if !response.status().is_success() {
            return Err(MarketplaceError::AuthError(format!(
                "Token exchange failed: {}",
                response.status()
            )));
        }

        let token_response: TokenResponse = response.json().await?;
        let token = self.create_token_from_response(token_response);

        // Store token
        let mut token_guard = self.token.write().await;
        *token_guard = Some(token.clone());

        Ok(token)
    }

    /// Refresh access token using refresh token.
    ///
    /// # Errors
    /// Returns error if refresh fails or no refresh token available.
    pub async fn refresh_token(&self) -> Result<OAuth2Token> {
        let refresh_token = {
            let current_token = self.token.read().await;
            current_token
                .as_ref()
                .and_then(|t| t.refresh_token.as_ref())
                .cloned()
                .ok_or_else(|| MarketplaceError::AuthError("No refresh token available".to_string()))?
        };

        let params = [
            ("grant_type", "refresh_token"),
            ("refresh_token", &refresh_token),
            ("client_id", &self.config.client_id),
            ("client_secret", &self.config.client_secret),
        ];

        let response = self
            .client
            .post(&self.config.token_url)
            .form(&params)
            .send()
            .await?;

        if !response.status().is_success() {
            return Err(MarketplaceError::AuthError(format!(
                "Token refresh failed: {}",
                response.status()
            )));
        }

        let token_response: TokenResponse = response.json().await?;
        let token = self.create_token_from_response(token_response);

        // Store new token
        let mut token_guard = self.token.write().await;
        *token_guard = Some(token.clone());

        Ok(token)
    }

    /// Get current valid token, refreshing if necessary.
    ///
    /// # Errors
    /// Returns error if no token available or refresh fails.
    pub async fn get_valid_token(&self) -> Result<OAuth2Token> {
        let token_guard = self.token.read().await;

        if let Some(token) = token_guard.as_ref() {
            if token.is_valid() {
                return Ok(token.clone());
            }
        }
        drop(token_guard);

        // Token expired or missing, try to refresh
        self.refresh_token().await
    }

    fn create_token_from_response(&self, response: TokenResponse) -> OAuth2Token {
        let expires_at = Utc::now() + Duration::seconds(response.expires_in);
        OAuth2Token {
            access_token: response.access_token,
            token_type: response.token_type,
            expires_at,
            refresh_token: response.refresh_token,
            scopes: response.scope.split_whitespace().map(String::from).collect(),
        }
    }
}

#[derive(Debug, Deserialize)]
struct TokenResponse {
    access_token: String,
    token_type: String,
    expires_in: i64,
    refresh_token: Option<String>,
    scope: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_token_expiration() {
        // Arrange
        let expired_token = OAuth2Token {
            access_token: "test".to_string(),
            token_type: "Bearer".to_string(),
            expires_at: Utc::now() - Duration::seconds(60),
            refresh_token: None,
            scopes: vec!["read".to_string()],
        };

        let valid_token = OAuth2Token {
            access_token: "test".to_string(),
            token_type: "Bearer".to_string(),
            expires_at: Utc::now() + Duration::seconds(3600),
            refresh_token: None,
            scopes: vec!["read".to_string()],
        };

        // Act & Assert
        assert!(expired_token.is_expired());
        assert!(!expired_token.is_valid());
        assert!(!valid_token.is_expired());
        assert!(valid_token.is_valid());
    }

    #[test]
    fn test_authorization_url_generation() {
        // Arrange
        let config = OAuth2Config {
            client_id: "test_client".to_string(),
            client_secret: "secret".to_string(),
            auth_url: "https://example.com/oauth/authorize".to_string(),
            token_url: "https://example.com/oauth/token".to_string(),
            redirect_uri: "https://app.example.com/callback".to_string(),
            scopes: vec!["read".to_string(), "write".to_string()],
        };

        let manager = OAuth2Manager::new(config);

        // Act
        let url = manager.get_authorization_url("test_state").unwrap();

        // Assert
        assert!(url.contains("client_id=test_client"));
        assert!(url.contains("state=test_state"));
        assert!(url.contains("response_type=code"));
        assert!(url.contains("scope=read+write"));
    }

    #[tokio::test]
    async fn test_manager_creation() {
        // Arrange
        let config = OAuth2Config {
            client_id: "test".to_string(),
            client_secret: "secret".to_string(),
            auth_url: "https://example.com/auth".to_string(),
            token_url: "https://example.com/token".to_string(),
            redirect_uri: "https://app.example.com/callback".to_string(),
            scopes: vec!["read".to_string()],
        };

        // Act
        let manager = OAuth2Manager::new(config);

        // Assert
        assert!(manager.token.read().await.is_none());
    }
}
