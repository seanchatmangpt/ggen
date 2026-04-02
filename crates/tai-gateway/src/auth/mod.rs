//! Authentication and authorization layer
//!
//! Supports OAuth2/OIDC, JWT validation, and Vault backend integration

use crate::error::{GatewayError, GatewayResult};
use chrono::{Duration, Utc};
use jsonwebtoken::{decode, decode_key, Algorithm, DecodingKey, Validation};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use uuid::Uuid;

/// Authentication token with metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuthToken {
    /// Token identifier (UUID)
    pub id: String,
    /// Token type (Bearer, API Key, etc.)
    pub token_type: String,
    /// Token value (JWT, secret key, etc.)
    pub value: String,
    /// Subject (user ID, service ID)
    pub subject: String,
    /// Scopes/permissions
    pub scopes: Vec<String>,
    /// Issued at timestamp
    pub issued_at: i64,
    /// Expires at timestamp
    pub expires_at: i64,
    /// Custom claims
    pub claims: HashMap<String, String>,
}

impl AuthToken {
    /// Check if token is expired
    pub fn is_expired(&self) -> bool {
        Utc::now().timestamp() > self.expires_at
    }

    /// Check if token has a specific scope
    pub fn has_scope(&self, scope: &str) -> bool {
        self.scopes.contains(&scope.to_string())
    }
}

/// JWT claims structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JwtClaims {
    pub sub: String,
    pub iss: String,
    pub aud: Vec<String>,
    pub exp: i64,
    pub iat: i64,
    pub scopes: Vec<String>,
    #[serde(flatten)]
    pub extra: HashMap<String, serde_json::Value>,
}

/// Authorization context for a request
#[derive(Debug, Clone)]
pub struct AuthContext {
    /// Authenticated subject
    pub subject: String,
    /// Token scopes
    pub scopes: Vec<String>,
    /// Custom claims
    pub claims: HashMap<String, String>,
}

impl AuthContext {
    /// Create new auth context
    pub fn new(subject: impl Into<String>, scopes: Vec<String>) -> Self {
        Self {
            subject: subject.into(),
            scopes,
            claims: HashMap::new(),
        }
    }

    /// Check if context has required scopes
    pub fn requires_scopes(&self, required: &[&str]) -> bool {
        required.iter().all(|scope| self.scopes.contains(&scope.to_string()))
    }
}

/// Authentication provider trait
#[async_trait::async_trait]
pub trait AuthProvider: Send + Sync {
    /// Authenticate a request and return auth context
    async fn authenticate(&self, token: &str) -> GatewayResult<AuthContext>;

    /// Validate scopes
    async fn validate_scopes(&self, context: &AuthContext, required: &[&str]) -> GatewayResult<()>;
}

/// JWT-based authentication provider
#[derive(Debug, Clone)]
pub struct JwtAuthProvider {
    // Issuer URL
    issuer: String,
    // Decoding key (HMAC secret or RSA public key)
    decoding_key: Arc<DecodingKey>,
    // Allowed audiences
    audiences: Vec<String>,
}

impl JwtAuthProvider {
    /// Create a new JWT auth provider with HMAC secret
    pub fn new_hmac(issuer: impl Into<String>, secret: &[u8], audiences: Vec<String>) -> Self {
        Self {
            issuer: issuer.into(),
            decoding_key: Arc::new(DecodingKey::from_secret(secret)),
            audiences,
        }
    }

    /// Validate JWT token
    fn validate_jwt(&self, token: &str) -> GatewayResult<JwtClaims> {
        let mut validation = Validation::new(Algorithm::HS256);
        validation.set_audience(&self.audiences.iter().map(|s| s.as_str()).collect::<Vec<_>>());
        validation.set_issuer(&[self.issuer.as_str()]);

        decode::<JwtClaims>(token, &self.decoding_key, &validation)
            .map(|data| data.claims)
            .map_err(|e| GatewayError::AuthenticationFailed(format!("JWT validation failed: {}", e)))
    }
}

#[async_trait::async_trait]
impl AuthProvider for JwtAuthProvider {
    async fn authenticate(&self, token: &str) -> GatewayResult<AuthContext> {
        let claims = self.validate_jwt(token)?;
        let mut ctx_claims = HashMap::new();

        // Extract standard claims
        for (key, value) in claims.extra {
            if let Some(s) = value.as_str() {
                ctx_claims.insert(key, s.to_string());
            }
        }

        Ok(AuthContext {
            subject: claims.sub,
            scopes: claims.scopes,
            claims: ctx_claims,
        })
    }

    async fn validate_scopes(&self, context: &AuthContext, required: &[&str]) -> GatewayResult<()> {
        if context.requires_scopes(required) {
            Ok(())
        } else {
            Err(GatewayError::AuthorizationFailed(
                "Insufficient scopes".to_string(),
            ))
        }
    }
}

/// In-memory token store for testing
#[derive(Debug, Clone)]
pub struct InMemoryTokenStore {
    tokens: Arc<RwLock<HashMap<String, AuthToken>>>,
}

impl InMemoryTokenStore {
    /// Create a new token store
    pub fn new() -> Self {
        Self {
            tokens: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Store a token
    pub async fn store(&self, token: AuthToken) -> GatewayResult<()> {
        let mut tokens = self.tokens.write().await;
        tokens.insert(token.value.clone(), token);
        Ok(())
    }

    /// Retrieve a token
    pub async fn get(&self, value: &str) -> GatewayResult<AuthToken> {
        let tokens = self.tokens.read().await;
        tokens
            .get(value)
            .cloned()
            .ok_or_else(|| GatewayError::AuthenticationFailed("Token not found".to_string()))
    }

    /// Revoke a token
    pub async fn revoke(&self, value: &str) -> GatewayResult<()> {
        let mut tokens = self.tokens.write().await;
        tokens
            .remove(value)
            .ok_or_else(|| GatewayError::AuthenticationFailed("Token not found".to_string()))?;
        Ok(())
    }

    /// Validate and get auth context
    pub async fn validate(&self, value: &str) -> GatewayResult<AuthContext> {
        let token = self.get(value).await?;

        if token.is_expired() {
            self.revoke(value).await?;
            return Err(GatewayError::AuthenticationFailed("Token expired".to_string()));
        }

        Ok(AuthContext {
            subject: token.subject,
            scopes: token.scopes,
            claims: token.claims,
        })
    }
}

impl Default for InMemoryTokenStore {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait::async_trait]
impl AuthProvider for InMemoryTokenStore {
    async fn authenticate(&self, token: &str) -> GatewayResult<AuthContext> {
        self.validate(token).await
    }

    async fn validate_scopes(&self, context: &AuthContext, required: &[&str]) -> GatewayResult<()> {
        if context.requires_scopes(required) {
            Ok(())
        } else {
            Err(GatewayError::AuthorizationFailed(
                "Insufficient scopes".to_string(),
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_auth_token_expiration() {
        let now = Utc::now().timestamp();
        let token = AuthToken {
            id: Uuid::new_v4().to_string(),
            token_type: "Bearer".to_string(),
            value: "test-token".to_string(),
            subject: "user-123".to_string(),
            scopes: vec!["read".to_string()],
            issued_at: now,
            expires_at: now - 3600,
            claims: HashMap::new(),
        };

        assert!(token.is_expired());
    }

    #[test]
    fn test_auth_token_scope() {
        let now = Utc::now().timestamp();
        let token = AuthToken {
            id: Uuid::new_v4().to_string(),
            token_type: "Bearer".to_string(),
            value: "test-token".to_string(),
            subject: "user-123".to_string(),
            scopes: vec!["read".to_string(), "write".to_string()],
            issued_at: now,
            expires_at: now + 3600,
            claims: HashMap::new(),
        };

        assert!(token.has_scope("read"));
        assert!(token.has_scope("write"));
        assert!(!token.has_scope("delete"));
    }

    #[test]
    fn test_auth_context_requires_scopes() {
        let context = AuthContext::new("user-123", vec!["read".to_string(), "write".to_string()]);

        assert!(context.requires_scopes(&["read", "write"]));
        assert!(!context.requires_scopes(&["read", "delete"]));
    }

    #[tokio::test]
    async fn test_in_memory_token_store() {
        let store = InMemoryTokenStore::new();
        let now = Utc::now().timestamp();

        let token = AuthToken {
            id: Uuid::new_v4().to_string(),
            token_type: "Bearer".to_string(),
            value: "test-token".to_string(),
            subject: "user-123".to_string(),
            scopes: vec!["read".to_string()],
            issued_at: now,
            expires_at: now + 3600,
            claims: HashMap::new(),
        };

        store.store(token.clone()).await.unwrap();
        let retrieved = store.get("test-token").await.unwrap();
        assert_eq!(retrieved.subject, "user-123");
    }

    #[tokio::test]
    async fn test_in_memory_token_store_revoke() {
        let store = InMemoryTokenStore::new();
        let now = Utc::now().timestamp();

        let token = AuthToken {
            id: Uuid::new_v4().to_string(),
            token_type: "Bearer".to_string(),
            value: "test-token".to_string(),
            subject: "user-123".to_string(),
            scopes: vec!["read".to_string()],
            issued_at: now,
            expires_at: now + 3600,
            claims: HashMap::new(),
        };

        store.store(token).await.unwrap();
        store.revoke("test-token").await.unwrap();

        let result = store.get("test-token").await;
        assert!(result.is_err());
    }
}
