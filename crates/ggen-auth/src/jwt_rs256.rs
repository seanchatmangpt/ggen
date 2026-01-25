//! RS256 JWT token generation and verification with asymmetric key pairs

use chrono::{Duration, Utc};
use jsonwebtoken::{decode, encode, Algorithm, DecodingKey, EncodingKey, Header, Validation};
use rsa::{RsaPrivateKey, RsaPublicKey};
use rsa::pkcs8::{EncodePrivateKey, EncodePublicKey, DecodePrivateKey, DecodePublicKey, LineEnding};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use uuid::Uuid;

use crate::{errors::AuthError, AuthResult};

/// Token type enumeration
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    Access,
    Refresh,
}

/// JWT token claims with RS256
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Rs256TokenClaims {
    pub sub: String,        // Subject (user ID)
    pub email: String,
    pub tier: String,
    pub scopes: Vec<String>,
    pub token_type: String, // "access" or "refresh"
    pub iat: i64,          // Issued at
    pub exp: i64,          // Expiration
    pub jti: String,       // JWT ID (for revocation)
}

/// Token pair (access + refresh)
#[derive(Debug, Clone)]
pub struct TokenPair {
    pub access_token: String,
    pub refresh_token: String,
    pub access_expires_in: i64,
    pub refresh_expires_in: i64,
}

/// RS256 JWT manager with asymmetric key pairs
pub struct Rs256JwtManager {
    private_key: Arc<EncodingKey>,
    public_key: Arc<DecodingKey>,
    access_token_ttl: i64,
    refresh_token_ttl: i64,
}

impl Rs256JwtManager {
    /// Create a new RS256 JWT manager with PEM-encoded keys
    ///
    /// # Errors
    /// Returns error if keys cannot be parsed
    pub fn new(
        private_key_pem: &str,
        public_key_pem: &str,
        access_token_ttl_secs: i64,
        refresh_token_ttl_secs: i64,
    ) -> AuthResult<Self> {
        let private_key = EncodingKey::from_rsa_pem(private_key_pem.as_bytes())
            .map_err(|e| AuthError::CryptoError(format!("Invalid private key: {}", e)))?;

        let public_key = DecodingKey::from_rsa_pem(public_key_pem.as_bytes())
            .map_err(|e| AuthError::CryptoError(format!("Invalid public key: {}", e)))?;

        Ok(Self {
            private_key: Arc::new(private_key),
            public_key: Arc::new(public_key),
            access_token_ttl: access_token_ttl_secs,
            refresh_token_ttl: refresh_token_ttl_secs,
        })
    }

    /// Generate a 4096-bit RSA key pair (private and public keys in PEM format)
    ///
    /// # Errors
    /// Returns error if key generation fails
    pub fn generate_key_pair() -> AuthResult<(String, String)> {
        use rand::rngs::OsRng;

        let mut rng = OsRng;
        let bits = 4096;

        let private_key = RsaPrivateKey::new(&mut rng, bits)
            .map_err(|e| AuthError::CryptoError(format!("Failed to generate private key: {}", e)))?;

        let public_key = RsaPublicKey::from(&private_key);

        let private_pem = private_key
            .to_pkcs8_pem(LineEnding::LF)
            .map_err(|e| AuthError::CryptoError(format!("Failed to encode private key: {}", e)))?
            .to_string();

        let public_pem = public_key
            .to_public_key_pem(LineEnding::LF)
            .map_err(|e| AuthError::CryptoError(format!("Failed to encode public key: {}", e)))?;

        Ok((private_pem, public_pem))
    }

    /// Generate a token pair (access + refresh tokens)
    ///
    /// # Errors
    /// Returns error if token generation fails
    pub fn generate_token_pair(
        &self,
        user_id: &str,
        email: &str,
        tier: &str,
        scopes: Vec<String>,
    ) -> AuthResult<TokenPair> {
        let access_token = self.generate_token(
            user_id,
            email,
            tier,
            scopes.clone(),
            TokenType::Access,
        )?;

        let refresh_token = self.generate_token(
            user_id,
            email,
            tier,
            scopes,
            TokenType::Refresh,
        )?;

        Ok(TokenPair {
            access_token,
            refresh_token,
            access_expires_in: self.access_token_ttl,
            refresh_expires_in: self.refresh_token_ttl,
        })
    }

    /// Generate a JWT token with RS256 algorithm
    ///
    /// # Errors
    /// Returns error if encoding fails
    fn generate_token(
        &self,
        user_id: &str,
        email: &str,
        tier: &str,
        scopes: Vec<String>,
        token_type: TokenType,
    ) -> AuthResult<String> {
        let now = Utc::now();
        let ttl = match token_type {
            TokenType::Access => self.access_token_ttl,
            TokenType::Refresh => self.refresh_token_ttl,
        };
        let exp = now + Duration::seconds(ttl);

        let claims = Rs256TokenClaims {
            sub: user_id.to_string(),
            email: email.to_string(),
            tier: tier.to_string(),
            scopes,
            token_type: match token_type {
                TokenType::Access => "access".to_string(),
                TokenType::Refresh => "refresh".to_string(),
            },
            iat: now.timestamp(),
            exp: exp.timestamp(),
            jti: Uuid::new_v4().to_string(),
        };

        let header = Header::new(Algorithm::RS256);

        encode(&header, &claims, &self.private_key)
            .map_err(|e| AuthError::TokenGenerationFailed(e.to_string()))
    }

    /// Verify and decode a JWT token
    ///
    /// # Errors
    /// Returns error if token is invalid or expired
    pub fn verify_token(&self, token: &str) -> AuthResult<Rs256TokenClaims> {
        let mut validation = Validation::new(Algorithm::RS256);
        validation.validate_exp = true;
        validation.validate_nbf = false;

        decode::<Rs256TokenClaims>(token, &self.public_key, &validation)
            .map(|data| data.claims)
            .map_err(|e| {
                let error_msg = e.to_string();
                if error_msg.contains("ExpiredSignature") {
                    AuthError::TokenExpired
                } else {
                    AuthError::InvalidToken
                }
            })
    }

    /// Verify a refresh token and generate a new token pair
    ///
    /// # Errors
    /// Returns error if refresh token is invalid or expired
    pub fn refresh_token_pair(&self, refresh_token: &str) -> AuthResult<TokenPair> {
        let claims = self.verify_token(refresh_token)?;

        // Verify this is actually a refresh token
        if claims.token_type != "refresh" {
            return Err(AuthError::InvalidToken);
        }

        // Generate new token pair
        self.generate_token_pair(&claims.sub, &claims.email, &claims.tier, claims.scopes)
    }

    /// Extract claims without verification (for debugging/logging only)
    ///
    /// # Errors
    /// Returns error if token cannot be decoded
    pub fn decode_unverified(&self, token: &str) -> AuthResult<Rs256TokenClaims> {
        let mut validation = Validation::new(Algorithm::RS256);
        validation.insecure_disable_signature_validation();
        validation.validate_exp = false;

        decode::<Rs256TokenClaims>(token, &self.public_key, &validation)
            .map(|data| data.claims)
            .map_err(|_| AuthError::InvalidToken)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_manager() -> Rs256JwtManager {
        let (private_pem, public_pem) = Rs256JwtManager::generate_key_pair().unwrap();
        Rs256JwtManager::new(&private_pem, &public_pem, 900, 604800).unwrap()
    }

    #[test]
    fn test_generate_key_pair() {
        // Arrange + Act
        let result = Rs256JwtManager::generate_key_pair();

        // Assert
        assert!(result.is_ok());
        let (private_pem, public_pem) = result.unwrap();
        assert!(private_pem.contains("BEGIN PRIVATE KEY"));
        assert!(public_pem.contains("BEGIN PUBLIC KEY"));
    }

    #[test]
    fn test_generate_token_pair() {
        // Arrange
        let manager = create_test_manager();
        let scopes = vec!["read".to_string(), "write".to_string()];

        // Act
        let result = manager.generate_token_pair("user123", "user@example.com", "pro", scopes);

        // Assert
        assert!(result.is_ok());
        let pair = result.unwrap();
        assert!(!pair.access_token.is_empty());
        assert!(!pair.refresh_token.is_empty());
        assert_eq!(pair.access_expires_in, 900);
        assert_eq!(pair.refresh_expires_in, 604800);
    }

    #[test]
    fn test_verify_access_token() {
        // Arrange
        let manager = create_test_manager();
        let scopes = vec!["read".to_string()];
        let pair = manager.generate_token_pair("user123", "user@example.com", "free", scopes).unwrap();

        // Act
        let result = manager.verify_token(&pair.access_token);

        // Assert
        assert!(result.is_ok());
        let claims = result.unwrap();
        assert_eq!(claims.sub, "user123");
        assert_eq!(claims.email, "user@example.com");
        assert_eq!(claims.tier, "free");
        assert_eq!(claims.token_type, "access");
    }

    #[test]
    fn test_verify_invalid_token() {
        // Arrange
        let manager = create_test_manager();

        // Act
        let result = manager.verify_token("invalid.token.here");

        // Assert
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), AuthError::InvalidToken));
    }

    #[test]
    fn test_refresh_token_rotation() {
        // Arrange
        let manager = create_test_manager();
        let scopes = vec!["read".to_string(), "write".to_string()];
        let pair = manager.generate_token_pair("user123", "user@example.com", "enterprise", scopes).unwrap();

        // Act
        let result = manager.refresh_token_pair(&pair.refresh_token);

        // Assert
        assert!(result.is_ok());
        let new_pair = result.unwrap();
        assert_ne!(new_pair.access_token, pair.access_token);
        assert_ne!(new_pair.refresh_token, pair.refresh_token);
    }

    #[test]
    fn test_cannot_refresh_with_access_token() {
        // Arrange
        let manager = create_test_manager();
        let scopes = vec!["read".to_string()];
        let pair = manager.generate_token_pair("user123", "user@example.com", "pro", scopes).unwrap();

        // Act
        let result = manager.refresh_token_pair(&pair.access_token);

        // Assert
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), AuthError::InvalidToken));
    }

    #[test]
    fn test_decode_unverified() {
        // Arrange
        let manager = create_test_manager();
        let scopes = vec!["read".to_string()];
        let pair = manager.generate_token_pair("user123", "user@example.com", "free", scopes).unwrap();

        // Act
        let result = manager.decode_unverified(&pair.access_token);

        // Assert
        assert!(result.is_ok());
        let claims = result.unwrap();
        assert_eq!(claims.sub, "user123");
    }
}
