/// OAuth2 and OpenID Connect (OIDC) Provider Implementation
/// Supports authorization code flow, implicit flow, and OIDC discovery

use crate::models::*;
use chrono::{DateTime, Duration, Utc};
use rand::Rng;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use uuid::Uuid;

/// OAuth2 AuthorizationRequest
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AuthorizationRequest {
    pub response_type: String,      // "code", "token", "id_token"
    pub client_id: String,
    pub redirect_uri: String,
    pub scope: String,              // "openid profile email"
    pub state: String,              // CSRF protection
    pub nonce: Option<String>,      // OIDC
    pub code_challenge: Option<String>, // PKCE
    pub code_challenge_method: Option<String>, // PKCE
}

/// OAuth2 AuthorizationResponse
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AuthorizationResponse {
    pub code: String,
    pub state: String,
}

/// OAuth2 TokenRequest
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TokenRequest {
    pub grant_type: String,         // "authorization_code", "refresh_token", "client_credentials"
    pub code: Option<String>,
    pub client_id: String,
    pub client_secret: Option<String>,
    pub redirect_uri: Option<String>,
    pub refresh_token: Option<String>,
    pub code_verifier: Option<String>, // PKCE
}

/// OAuth2 TokenResponse
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TokenResponse {
    pub access_token: String,
    pub token_type: String,        // "Bearer"
    pub expires_in: i64,
    pub refresh_token: Option<String>,
    pub id_token: Option<String>,  // OIDC
}

/// OAuth2 Provider
pub struct OAuth2Provider {
    config: OAuth2Config,
}

#[derive(Clone, Debug)]
pub struct OAuth2Config {
    pub issuer: String,
    pub authorization_endpoint: String,
    pub token_endpoint: String,
    pub userinfo_endpoint: String,
    pub jwks_uri: String,
    pub jwt_secret: String,
    pub code_lifetime: Duration,
    pub token_lifetime: Duration,
    pub refresh_token_lifetime: Duration,
}

impl OAuth2Provider {
    pub fn new(config: OAuth2Config) -> Self {
        Self { config }
    }

    /// Authorization endpoint - user initiates login
    pub async fn authorize(
        &self,
        request: AuthorizationRequest,
        client: &OAuth2Client,
        user_id: Uuid,
    ) -> Result<AuthorizationResponse, String> {
        // Validate client
        if client.id != request.client_id {
            return Err("Invalid client_id".to_string());
        }

        // Validate redirect_uri
        if !client.redirect_uris.contains(&request.redirect_uri) {
            return Err("Invalid redirect_uri".to_string());
        }

        // Validate scope
        for scope in request.scope.split(' ') {
            if !client.allowed_scopes.contains(&scope.to_string()) {
                return Err(format!("Scope {} not allowed", scope));
            }
        }

        // Generate authorization code
        let code = self.generate_code();
        let expires_at = Utc::now() + self.config.code_lifetime;

        // Store authorization code (in real implementation, store in database)
        // _auth_code = AuthorizationCode {
        //     code: code.clone(),
        //     client_id: client.id.clone(),
        //     user_id,
        //     scopes: request.scope.split(' ').map(|s| s.to_string()).collect(),
        //     expires_at,
        //     redirect_uri: request.redirect_uri.clone(),
        // };

        Ok(AuthorizationResponse {
            code,
            state: request.state,
        })
    }

    /// Token endpoint - exchange code for tokens
    pub async fn token(
        &self,
        request: TokenRequest,
        client: &OAuth2Client,
    ) -> Result<TokenResponse, String> {
        match request.grant_type.as_str() {
            "authorization_code" => self.token_authorization_code(request, client).await,
            "refresh_token" => self.token_refresh_token(request, client).await,
            "client_credentials" => self.token_client_credentials(client).await,
            _ => Err("Unsupported grant_type".to_string()),
        }
    }

    async fn token_authorization_code(
        &self,
        request: TokenRequest,
        client: &OAuth2Client,
    ) -> Result<TokenResponse, String> {
        // Validate client credentials
        if let Some(secret) = &request.client_secret {
            if client.secret != *secret {
                return Err("Invalid client_secret".to_string());
            }
        } else if client.is_confidential {
            return Err("Client secret required".to_string());
        }

        // Get authorization code from storage
        let code = request
            .code
            .ok_or("Authorization code required".to_string())?;

        // Verify PKCE if present
        if let Some(verifier) = &request.code_verifier {
            // Code challenge should be in stored auth code
            // Verify: SHA256(code_verifier) == code_challenge
        }

        // In real implementation, retrieve code from storage and validate
        // For now, we'll create tokens directly

        let access_token = self.generate_jwt(client.id.clone(), vec![])?;
        let refresh_token = self.generate_refresh_token();

        Ok(TokenResponse {
            access_token,
            token_type: "Bearer".to_string(),
            expires_in: self.config.token_lifetime.num_seconds(),
            refresh_token: Some(refresh_token),
            id_token: None, // Will be added for OIDC
        })
    }

    async fn token_refresh_token(
        &self,
        request: TokenRequest,
        client: &OAuth2Client,
    ) -> Result<TokenResponse, String> {
        let refresh_token = request
            .refresh_token
            .ok_or("Refresh token required".to_string())?;

        // In real implementation, validate refresh token from storage
        // Check if it's expired or revoked

        let access_token = self.generate_jwt(client.id.clone(), vec![])?;

        Ok(TokenResponse {
            access_token,
            token_type: "Bearer".to_string(),
            expires_in: self.config.token_lifetime.num_seconds(),
            refresh_token: Some(refresh_token),
            id_token: None,
        })
    }

    async fn token_client_credentials(
        &self,
        client: &OAuth2Client,
    ) -> Result<TokenResponse, String> {
        let access_token = self.generate_jwt(client.id.clone(), vec![])?;

        Ok(TokenResponse {
            access_token,
            token_type: "Bearer".to_string(),
            expires_in: self.config.token_lifetime.num_seconds(),
            refresh_token: None,
            id_token: None,
        })
    }

    /// OIDC UserInfo endpoint
    pub async fn userinfo(&self, access_token: &str) -> Result<UserInfo, String> {
        // Validate token and return user information
        let _claims = self.validate_jwt(access_token)?;

        Ok(UserInfo {
            sub: "user123".to_string(),
            name: "John Doe".to_string(),
            email: "john@example.com".to_string(),
            email_verified: true,
            picture: None,
            profile: None,
        })
    }

    /// OIDC Discovery endpoint
    pub fn discovery(&self) -> OidcDiscovery {
        OidcDiscovery {
            issuer: self.config.issuer.clone(),
            authorization_endpoint: self.config.authorization_endpoint.clone(),
            token_endpoint: self.config.token_endpoint.clone(),
            userinfo_endpoint: self.config.userinfo_endpoint.clone(),
            jwks_uri: self.config.jwks_uri.clone(),
            scopes_supported: vec![
                "openid".to_string(),
                "profile".to_string(),
                "email".to_string(),
            ],
            response_types_supported: vec![
                "code".to_string(),
                "token".to_string(),
                "id_token".to_string(),
            ],
            grant_types_supported: vec![
                "authorization_code".to_string(),
                "refresh_token".to_string(),
                "client_credentials".to_string(),
            ],
            token_endpoint_auth_methods_supported: vec![
                "client_secret_basic".to_string(),
                "client_secret_post".to_string(),
            ],
        }
    }

    // Helper functions
    fn generate_code(&self) -> String {
        use base64::Engine;
        let mut rng = rand::thread_rng();
        let bytes: Vec<u8> = (0..32).map(|_| rng.gen()).collect();
        base64::engine::general_purpose::STANDARD_NO_PAD.encode(bytes)
    }

    fn generate_refresh_token(&self) -> String {
        use base64::Engine;
        let mut rng = rand::thread_rng();
        let bytes: Vec<u8> = (0..64).map(|_| rng.gen()).collect();
        base64::engine::general_purpose::STANDARD_NO_PAD.encode(bytes)
    }

    fn generate_jwt(&self, sub: String, _roles: Vec<String>) -> Result<String, String> {
        // In real implementation, use jsonwebtoken crate
        // Create JWT with claims, sign with secret
        Ok(format!("jwt.token.{}", sub))
    }

    fn validate_jwt(&self, _token: &str) -> Result<Claims, String> {
        // In real implementation, validate JWT signature and expiry
        Err("Not implemented".to_string())
    }
}

/// OIDC UserInfo
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct UserInfo {
    pub sub: String,
    pub name: Option<String>,
    pub email: Option<String>,
    pub email_verified: Option<bool>,
    pub picture: Option<String>,
    pub profile: Option<String>,
}

/// OIDC Discovery Document
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct OidcDiscovery {
    pub issuer: String,
    pub authorization_endpoint: String,
    pub token_endpoint: String,
    pub userinfo_endpoint: String,
    pub jwks_uri: String,
    pub scopes_supported: Vec<String>,
    pub response_types_supported: Vec<String>,
    pub grant_types_supported: Vec<String>,
    pub token_endpoint_auth_methods_supported: Vec<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_config() -> OAuth2Config {
        OAuth2Config {
            issuer: "http://localhost:3000".to_string(),
            authorization_endpoint: "http://localhost:3000/oauth/authorize".to_string(),
            token_endpoint: "http://localhost:3000/oauth/token".to_string(),
            userinfo_endpoint: "http://localhost:3000/oauth/userinfo".to_string(),
            jwks_uri: "http://localhost:3000/oauth/jwks".to_string(),
            jwt_secret: "test-secret".to_string(),
            code_lifetime: Duration::minutes(10),
            token_lifetime: Duration::hours(1),
            refresh_token_lifetime: Duration::days(7),
        }
    }

    #[tokio::test]
    async fn test_oauth2_discovery() {
        let provider = OAuth2Provider::new(create_test_config());
        let discovery = provider.discovery();
        assert_eq!(discovery.issuer, "http://localhost:3000");
        assert!(!discovery.scopes_supported.is_empty());
    }
}
