//! Security error types
//!
//! This module defines all error types for security operations including
//! Vault, Cloud KMS, certificate management, and encryption operations.

use std::fmt;
use thiserror::Error;

/// Security operation result type
pub type Result<T> = std::result::Result<T, SecurityError>;

/// Comprehensive error type for security operations
#[derive(Error, Debug, Clone)]
pub enum SecurityError {
    /// Vault connection or operation failed
    #[error("Vault error: {message}")]
    VaultError { message: String },

    /// Cloud KMS operation failed
    #[error("Cloud KMS error: {message}")]
    CloudKmsError { message: String },

    /// Certificate operation failed
    #[error("Certificate error: {message}")]
    CertificateError { message: String },

    /// mTLS configuration or validation failed
    #[error("mTLS error: {message}")]
    MtlsError { message: String },

    /// Secret rotation operation failed
    #[error("Secret rotation error: {message}")]
    SecretRotationError { message: String },

    /// Encryption/decryption operation failed
    #[error("Encryption error: {message}")]
    EncryptionError { message: String },

    /// Key management operation failed
    #[error("Key management error: {message}")]
    KeyManagementError { message: String },

    /// Authentication failed
    #[error("Authentication error: {message}")]
    AuthenticationError { message: String },

    /// Authorization failed (insufficient permissions)
    #[error("Authorization error: {message}")]
    AuthorizationError { message: String },

    /// Invalid secret or lease state
    #[error("Invalid secret state: {message}")]
    InvalidSecretState { message: String },

    /// Lease management error
    #[error("Lease error: {message}")]
    LeaseError { message: String },

    /// Serialization/deserialization error
    #[error("Serialization error: {message}")]
    SerializationError { message: String },

    /// Cryptographic operation failed
    #[error("Cryptographic error: {message}")]
    CryptoError { message: String },

    /// Configuration error
    #[error("Configuration error: {message}")]
    ConfigurationError { message: String },

    /// Timeout during operation
    #[error("Operation timeout: {message}")]
    TimeoutError { message: String },

    /// Resource not found
    #[error("Not found: {message}")]
    NotFound { message: String },

    /// Generic security error
    #[error("{message}")]
    Other { message: String },
}

impl SecurityError {
    /// Create a Vault error
    pub fn vault(msg: impl Into<String>) -> Self {
        Self::VaultError {
            message: msg.into(),
        }
    }

    /// Create a Cloud KMS error
    pub fn cloud_kms(msg: impl Into<String>) -> Self {
        Self::CloudKmsError {
            message: msg.into(),
        }
    }

    /// Create a certificate error
    pub fn certificate(msg: impl Into<String>) -> Self {
        Self::CertificateError {
            message: msg.into(),
        }
    }

    /// Create an mTLS error
    pub fn mtls(msg: impl Into<String>) -> Self {
        Self::MtlsError {
            message: msg.into(),
        }
    }

    /// Create a secret rotation error
    pub fn secret_rotation(msg: impl Into<String>) -> Self {
        Self::SecretRotationError {
            message: msg.into(),
        }
    }

    /// Create an encryption error
    pub fn encryption(msg: impl Into<String>) -> Self {
        Self::EncryptionError {
            message: msg.into(),
        }
    }

    /// Create a key management error
    pub fn key_management(msg: impl Into<String>) -> Self {
        Self::KeyManagementError {
            message: msg.into(),
        }
    }

    /// Create an authentication error
    pub fn authentication(msg: impl Into<String>) -> Self {
        Self::AuthenticationError {
            message: msg.into(),
        }
    }

    /// Create an authorization error
    pub fn authorization(msg: impl Into<String>) -> Self {
        Self::AuthorizationError {
            message: msg.into(),
        }
    }

    /// Create an invalid secret state error
    pub fn invalid_secret_state(msg: impl Into<String>) -> Self {
        Self::InvalidSecretState {
            message: msg.into(),
        }
    }

    /// Create a lease error
    pub fn lease(msg: impl Into<String>) -> Self {
        Self::LeaseError {
            message: msg.into(),
        }
    }

    /// Create a serialization error
    pub fn serialization(msg: impl Into<String>) -> Self {
        Self::SerializationError {
            message: msg.into(),
        }
    }

    /// Create a crypto error
    pub fn crypto(msg: impl Into<String>) -> Self {
        Self::CryptoError {
            message: msg.into(),
        }
    }

    /// Create a configuration error
    pub fn configuration(msg: impl Into<String>) -> Self {
        Self::ConfigurationError {
            message: msg.into(),
        }
    }

    /// Create a timeout error
    pub fn timeout(msg: impl Into<String>) -> Self {
        Self::TimeoutError {
            message: msg.into(),
        }
    }

    /// Create a not found error
    pub fn not_found(msg: impl Into<String>) -> Self {
        Self::NotFound {
            message: msg.into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_creation() {
        let err = SecurityError::vault("Connection failed");
        assert_eq!(
            err.to_string(),
            "Vault error: Connection failed"
        );

        let err = SecurityError::encryption("Invalid key");
        assert_eq!(err.to_string(), "Encryption error: Invalid key");
    }

    #[test]
    fn test_error_variants() {
        let vault_err = SecurityError::vault("test");
        assert!(matches!(vault_err, SecurityError::VaultError { .. }));

        let kms_err = SecurityError::cloud_kms("test");
        assert!(matches!(kms_err, SecurityError::CloudKmsError { .. }));

        let cert_err = SecurityError::certificate("test");
        assert!(matches!(cert_err, SecurityError::CertificateError { .. }));

        let mtls_err = SecurityError::mtls("test");
        assert!(matches!(mtls_err, SecurityError::MtlsError { .. }));

        let rotation_err = SecurityError::secret_rotation("test");
        assert!(matches!(rotation_err, SecurityError::SecretRotationError { .. }));

        let enc_err = SecurityError::encryption("test");
        assert!(matches!(enc_err, SecurityError::EncryptionError { .. }));

        let key_err = SecurityError::key_management("test");
        assert!(matches!(key_err, SecurityError::KeyManagementError { .. }));

        let auth_err = SecurityError::authentication("test");
        assert!(matches!(auth_err, SecurityError::AuthenticationError { .. }));

        let authz_err = SecurityError::authorization("test");
        assert!(matches!(authz_err, SecurityError::AuthorizationError { .. }));

        let state_err = SecurityError::invalid_secret_state("test");
        assert!(matches!(state_err, SecurityError::InvalidSecretState { .. }));

        let lease_err = SecurityError::lease("test");
        assert!(matches!(lease_err, SecurityError::LeaseError { .. }));

        let ser_err = SecurityError::serialization("test");
        assert!(matches!(ser_err, SecurityError::SerializationError { .. }));

        let crypto_err = SecurityError::crypto("test");
        assert!(matches!(crypto_err, SecurityError::CryptoError { .. }));

        let config_err = SecurityError::configuration("test");
        assert!(matches!(config_err, SecurityError::ConfigurationError { .. }));

        let timeout_err = SecurityError::timeout("test");
        assert!(matches!(timeout_err, SecurityError::TimeoutError { .. }));

        let not_found_err = SecurityError::not_found("test");
        assert!(matches!(not_found_err, SecurityError::NotFound { .. }));
    }
}
