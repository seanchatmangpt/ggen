//! JWT token generation and verification

use chrono::{Duration, Utc};
use jsonwebtoken::{decode, encode, DecodingKey, EncodingKey, Header, Validation};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::{errors::AuthError, AuthResult};

/// JWT token claims
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TokenClaims {
    pub sub: String, // Subject (user ID)
    pub email: String,
    pub tier: String,
    pub iat: i64,    // Issued at
    pub exp: i64,    // Expiration
    pub jti: String, // JWT ID (for revocation)
}

/// JWT manager for token operations
pub struct JwtManager {
    secret: String,
    expiration_secs: u64,
}

impl JwtManager {
    /// Create a new JWT manager
    pub fn new(secret: String, expiration_secs: u64) -> Self {
        Self {
            secret,
            expiration_secs,
        }
    }

    /// Generate a new JWT token
    pub fn generate_token(&self, user_id: &str, email: &str, tier: &str) -> AuthResult<String> {
        let now = Utc::now();
        let exp = now + Duration::seconds(self.expiration_secs as i64);

        let claims = TokenClaims {
            sub: user_id.to_string(),
            email: email.to_string(),
            tier: tier.to_string(),
            iat: now.timestamp(),
            exp: exp.timestamp(),
            jti: Uuid::new_v4().to_string(),
        };

        let key = EncodingKey::from_secret(self.secret.as_bytes());
        encode(&Header::default(), &claims, &key)
            .map_err(|e| AuthError::TokenGenerationFailed(e.to_string()))
    }

    /// Verify and decode a JWT token
    pub fn verify_token(&self, token: &str) -> AuthResult<TokenClaims> {
        let key = DecodingKey::from_secret(self.secret.as_bytes());
        let validation = Validation::default();

        decode::<TokenClaims>(token, &key, &validation)
            .map(|data| data.claims)
            .map_err(|e| {
                if e.to_string().contains("ExpiredSignature") {
                    AuthError::TokenExpired
                } else {
                    AuthError::InvalidToken
                }
            })
    }

    /// Check if a token is expired
    pub fn is_expired(token: &str) -> bool {
        if let Ok(claims) = jsonwebtoken::decode::<TokenClaims>(
            token,
            &DecodingKey::from_secret(b""),
            &Validation::default(),
        ) {
            claims.claims.exp < Utc::now().timestamp()
        } else {
            true
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_and_verify_token() {
        let manager = JwtManager::new("secret".to_string(), 3600);
        let token = manager
            .generate_token("user123", "user@example.com", "free")
            .unwrap();
        assert!(!token.is_empty());
    }

    #[test]
    fn test_verify_invalid_token() {
        let manager = JwtManager::new("secret".to_string(), 3600);
        let result = manager.verify_token("invalid.token.here");
        assert!(result.is_err());
    }
}
