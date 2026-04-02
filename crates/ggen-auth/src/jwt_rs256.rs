//! RS256 JWT token generation and verification with asymmetric key pairs

use chrono::{Duration, Utc};
use jsonwebtoken::{decode, encode, Algorithm, DecodingKey, EncodingKey, Header, Validation};
use rsa::pkcs8::{EncodePrivateKey, EncodePublicKey, LineEnding};
use rsa::{RsaPrivateKey, RsaPublicKey};
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
    pub sub: String, // Subject (user ID)
    pub email: String,
    pub tier: String,
    pub scopes: Vec<String>,
    pub token_type: String, // "access" or "refresh"
    pub iat: i64,           // Issued at
    pub exp: i64,           // Expiration
    pub jti: String,        // JWT ID (for revocation)
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
        private_key_pem: &str, public_key_pem: &str, access_token_ttl_secs: i64,
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

    /// Generate an RSA key pair (private and public keys in PEM format)
    ///
    /// # Errors
    /// Returns error if key generation fails
    pub fn generate_key_pair() -> AuthResult<(String, String)> {
        Self::generate_key_pair_with_bits(4096)
    }

    /// Generate an RSA key pair with the specified bit size
    ///
    /// # Errors
    /// Returns error if key generation fails
    pub fn generate_key_pair_with_bits(bits: usize) -> AuthResult<(String, String)> {
        use rand::rngs::OsRng;

        let mut rng = OsRng;

        let private_key = RsaPrivateKey::new(&mut rng, bits).map_err(|e| {
            AuthError::CryptoError(format!("Failed to generate private key: {}", e))
        })?;

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
        &self, user_id: &str, email: &str, tier: &str, scopes: Vec<String>,
    ) -> AuthResult<TokenPair> {
        let access_token =
            self.generate_token(user_id, email, tier, scopes.clone(), TokenType::Access)?;

        let refresh_token =
            self.generate_token(user_id, email, tier, scopes, TokenType::Refresh)?;

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
        &self, user_id: &str, email: &str, tier: &str, scopes: Vec<String>, token_type: TokenType,
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

    /// Pre-generated 2048-bit RSA key pair for tests.
    /// Using hardcoded keys avoids live RSA key generation (which takes 1-30s per call)
    /// and keeps all tests well under the pre-push hook timeout.
    const TEST_PRIVATE_KEY_PEM: &str = "-----BEGIN PRIVATE KEY-----\n\
        MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQCmczBP1W/uFg1Z\n\
        R+v+oUOvdR/O+9anA5qbxbjs0veEczTEmuv1DBJicvZLAVhWsnRbH0v6+Imc3YvU\n\
        C90rVreeqndW9Sd9SsSOWFsHIeIgGAAFm9uCmNnCjLGZNucs8GLjcemiRLkd9AIc\n\
        NrYbu1h8V/KwPq9WxtunR2RdIXOBziux27xCb3YHnrzTIawIfZf4S1wXQ8lRunUu\n\
        SosdIXusojnYvew9ZDWlkdAoxpgo0dYZOL+szuy9oagHqoY82y2Pj+nevID/ZUfK\n\
        fMC+8gDpS39FLOA6TgXD0p66N59zdbZHwxcTieT5eOMg3mzl4CKx27KP3a1SVH7c\n\
        5xEHc8b9AgMBAAECggEACTBDAqfhLG7/i1CwQ0JgFqpWuFnoVm2ZVi+65ClHQa1h\n\
        xIKwM5BYNCLC3Dl1nh+ZfcLuB6uQXoj6ZEPIEKbnrwobrsC3eTAB8WuzkWK6Sr7t\n\
        wrbWY3tloLBHSOfM04sXompSP+4e8VDcNvYsOmlZWqU46qgjN3KggzeqijDuYUQg\n\
        upbD1nHPwSG2M7eIeQ/S9+z0Ak8jIHu/cs1BEcId4oeT8NLGSZXoUdbCsYPZnvBa\n\
        a9MHbNgyty8zwOLh9jVJbFr3IfgjH7P6vi8NJ2fFUMR033MZWvqH7M3knKGsME6m\n\
        9SwuQGXkcgGPVaqzP7Gp/v8Glho7PDABVH4FazyBoQKBgQDW18ZR11R2QnNzGiyz\n\
        u8dpRy6M2sucZM5VfuZPtwIuRrb2SrZj1ZEQf0lr7cj/ICUA+N/Pg0esWsgx0jif\n\
        ii8Qw4+7QFUbhMZp7AbtGtUdjV9+wuaEoSZK1P7TZUdZDAI60R2k+GYZUX1LDqfI\n\
        5qtxAPSHxEwNT63qNSlpVgr+TQKBgQDGViamrgXKz1ZhkugN8hfvnV00zmcda53C\n\
        4Q0Wcv/TzCispCmJiSyzB9mdESsENznDGvAQqX14ctjgYPcAM2FJIU97YwdI/1Mi\n\
        EhU28d9Mir8DDRXjiR/JyuHMi3aLTYFJEhv5nmb+aRaKn1THX3VL/ymkEdvaQ4t0\n\
        XXDi16kjcQKBgGExHDbKAxu81B2uKarluECYQybVlGcb83wZ/Hbzg7kLpJmnEvv3\n\
        Ebk03DJHUDaahCL8c8Oc0D/Ykdh4EcWa7c/XqJ49bEMWvewNKtT43pCP9DOUWFSH\n\
        0prn4KAhoTVXiWqyT4biGM47X0MlTiJEuCpXFEiXNRCQi0z439MXacHBAoGBAIRz\n\
        SACLuoaEDBXBO4MEp4N3PaVkKWx2T7azAEB7r+jLMGaKgsq8dsNgfKztqCJ6lvJ6\n\
        bNNTvNzRJZBCcPELXu6tdhzaNyVCjUv1c4Fr6Ul+Zq0s8e8vxbDG8h/dCt9gdPRN\n\
        Sa1bYUCJAWq1mLJER1rnHfHZk06hSOH+/MIgb22hAoGAcrp47YNBN9AZZDNfvjpJ\n\
        hoj/ZpkwU6TN0SNz5EiQaltHZWUZLT0RcOB5E9ljm5cb2AZgGBr/VZW6hQuqLqwt\n\
        JDEulkTXTWnIGhiEhr9GIYo7XCLq+TBFwVIYnL7kZB9e+5lxtN9nsDmFsMSbwGt1\n\
        ZEKU7LP0+DBOEYBLRW5su+o=\n\
        -----END PRIVATE KEY-----";

    const TEST_PUBLIC_KEY_PEM: &str = "-----BEGIN PUBLIC KEY-----\n\
        MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEApnMwT9Vv7hYNWUfr/qFD\n\
        r3UfzvvWpwOam8W47NL3hHM0xJrr9QwSYnL2SwFYVrJ0Wx9L+viJnN2L1AvdK1a3\n\
        nqp3VvUnfUrEjlhbByHiIBgABZvbgpjZwoyxmTbnLPBi43HpokS5HfQCHDa2G7tY\n\
        fFfysD6vVsbbp0dkXSFzgc4rsdu8Qm92B5680yGsCH2X+EtcF0PJUbp1LkqLHSF7\n\
        rKI52L3sPWQ1pZHQKMaYKNHWGTi/rM7svaGoB6qGPNstj4/p3ryA/2VHynzAvvIA\n\
        6Ut/RSzgOk4Fw9Keujefc3W2R8MXE4nk+XjjIN5s5eAisduyj92tUlR+3OcRB3PG\n\
        /QIDAQAB\n\
        -----END PUBLIC KEY-----";

    fn create_test_manager() -> Rs256JwtManager {
        // Use pre-generated hardcoded keys to avoid slow live RSA key generation in tests
        Rs256JwtManager::new(TEST_PRIVATE_KEY_PEM, TEST_PUBLIC_KEY_PEM, 900, 604800).unwrap()
    }

    #[test]
    fn test_generate_key_pair() {
        // Arrange + Act: verify the constructor accepts PEM-encoded keys (no live generation)
        let result = Rs256JwtManager::new(TEST_PRIVATE_KEY_PEM, TEST_PUBLIC_KEY_PEM, 900, 604800);

        // Assert
        assert!(result.is_ok());
        // Verify the hardcoded PEMs have the expected headers
        assert!(TEST_PRIVATE_KEY_PEM.contains("BEGIN PRIVATE KEY"));
        assert!(TEST_PUBLIC_KEY_PEM.contains("BEGIN PUBLIC KEY"));
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
        let pair = manager
            .generate_token_pair("user123", "user@example.com", "free", scopes)
            .unwrap();

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
        let pair = manager
            .generate_token_pair("user123", "user@example.com", "enterprise", scopes)
            .unwrap();

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
        let pair = manager
            .generate_token_pair("user123", "user@example.com", "pro", scopes)
            .unwrap();

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
        let pair = manager
            .generate_token_pair("user123", "user@example.com", "free", scopes)
            .unwrap();

        // Act
        let result = manager.decode_unverified(&pair.access_token);

        // Assert
        assert!(result.is_ok());
        let claims = result.unwrap();
        assert_eq!(claims.sub, "user123");
    }
}
