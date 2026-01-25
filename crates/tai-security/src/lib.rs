//! Production-grade security infrastructure for ggen
//!
//! This crate provides comprehensive security features:
//!
//! - **Vault Integration**: HashiCorp Vault for secret management, dynamic credentials, encryption-as-a-service
//! - **Cloud KMS**: Google Cloud KMS for key management, encryption/decryption, key rotation
//! - **mTLS**: Mutual TLS certificate management, provisioning, renewal, pinning
//! - **Secret Rotation**: Automatic secret rotation with audit trails and rollback
//! - **Encryption**: Data encryption at rest and in transit, key derivation, HMAC
//!
//! ## Quick Start
//!
//! ### Vault Integration
//!
//! ```rust,no_run
//! use tai_security::vault_client::{VaultClient, VaultConfig};
//! use std::time::Duration;
//!
//! # async fn example() {
//! let config = VaultConfig {
//!     url: "https://vault.example.com".to_string(),
//!     token: "s.xxxxxxxxxxxxxxxxxxxx".to_string(),
//!     timeout: Duration::from_secs(10),
//!     namespace: None,
//!     tls_verify: true,
//!     ca_cert: None,
//!     renewal_threshold: 0.75,
//! };
//!
//! let client = VaultClient::new(config).await.unwrap();
//! let secret = client.get_secret("database/credentials").await.unwrap();
//! println!("Database credentials: {:?}", secret);
//! # }
//! ```
//!
//! ### Cloud KMS
//!
//! ```rust,no_run
//! use tai_security::cloud_kms::{CloudKmsClient, KmsConfig};
//! use std::time::Duration;
//!
//! # async fn example() {
//! let config = KmsConfig {
//!     project_id: "my-project".to_string(),
//!     location: "us-central1".to_string(),
//!     credentials_path: Some("/path/to/credentials.json".to_string()),
//!     timeout: Duration::from_secs(30),
//!     audit_logging: true,
//! };
//!
//! let client = CloudKmsClient::new(config).await.unwrap();
//! let ring = client.create_key_ring("production").await.unwrap();
//! let key = client.create_key("production", "app-key", "AES_256_GCM").await.unwrap();
//! # }
//! ```
//!
//! ### mTLS Certificates
//!
//! ```rust,no_run
//! use tai_security::mtls::{MtlsManager, RenewalConfig, CertificateProvider};
//! use std::path::Path;
//!
//! # async fn example() {
//! let renewal_config = RenewalConfig {
//!     renew_before_days: 30,
//!     provider: CertificateProvider::Vault,
//!     provider_config: Default::default(),
//!     auto_renewal: true,
//!     check_interval: 24,
//! };
//!
//! let manager = MtlsManager::new(renewal_config);
//! let cert = manager.load_certificate_from_file(Path::new("/path/to/cert.pem")).await.unwrap();
//! println!("Certificate: CN={}", cert.common_name);
//! # }
//! ```
//!
//! ### Secret Rotation
//!
//! ```rust,no_run
//! use tai_security::secret_rotation::{SecretRotationManager, SecretType};
//!
//! # async fn example() {
//! let manager = SecretRotationManager::new();
//! manager.register_secret(
//!     "db-password".to_string(),
//!     SecretType::DatabaseCredential,
//!     90,
//! ).await.unwrap();
//!
//! let new_version = manager.rotate_secret(
//!     "db-password",
//!     "new_password_hash".to_string(),
//!     "Scheduled rotation".to_string(),
//! ).await.unwrap();
//!
//! println!("Rotated to version: {}", new_version);
//! # }
//! ```
//!
//! ### Encryption
//!
//! ```rust
//! use tai_security::encryption::{EncryptionManager, EncryptionAlgorithm, KeyDerivationFunction};
//!
//! let manager = EncryptionManager::new(
//!     EncryptionAlgorithm::Aes256Gcm,
//!     KeyDerivationFunction::Pbkdf2,
//! );
//!
//! // Generate key
//! let key = manager.generate_key("app-key".to_string()).unwrap();
//!
//! // Encrypt data
//! let plaintext = b"sensitive data";
//! let encrypted = manager.encrypt_aes256(plaintext, &key).unwrap();
//! println!("Encrypted: {}", encrypted.ciphertext);
//!
//! // Decrypt data
//! let decrypted = manager.decrypt_aes256(&encrypted, &key).unwrap();
//! assert_eq!(decrypted, plaintext);
//! ```
//!
//! ## Security Principles
//!
//! - **Least Privilege**: All credentials rotated regularly, services have minimal permissions
//! - **Defense in Depth**: Multiple layers - encryption, TLS, access control, audit logging
//! - **Secrets Never in Code**: All secrets loaded from Vault or KMS, never in source code
//! - **Automatic Rotation**: Secrets rotated before expiry, with audit trail and rollback capability
//! - **Observable**: All operations logged with timestamps, actors, and outcomes
//! - **Type-Safe**: Rust's type system ensures secrets aren't accidentally logged or leaked
//!
//! ## Features
//!
//! - Vault integration with dynamic secrets and lease renewal
//! - Google Cloud KMS with key rotation and IAM
//! - mTLS certificate management with automatic renewal
//! - Secret rotation with graceful transition and rollback
//! - AES-256-GCM encryption with key derivation
//! - HMAC-SHA256 for authentication
//! - Comprehensive audit logging

pub mod cloud_kms;
pub mod encryption;
pub mod error;
pub mod mtls;
pub mod secret_rotation;
pub mod vault_client;

// Re-export commonly used types
pub use error::{Result, SecurityError};
pub use vault_client::VaultClient;
pub use cloud_kms::CloudKmsClient;
pub use mtls::MtlsManager;
pub use secret_rotation::SecretRotationManager;
pub use encryption::EncryptionManager;

/// Security module result type
pub type SecurityResult<T> = Result<T>;
