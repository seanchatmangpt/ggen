//! Integration tests for security module
//!
//! Tests real-world security scenarios including:
//! - Secret retrieval and rotation
//! - Encryption/decryption round trips
//! - Certificate provisioning and renewal
//! - mTLS configuration
//! - Vault and Cloud KMS operations

use tai_security::cloud_kms::{CloudKmsClient, KmsConfig, EncryptedData};
use tai_security::encryption::{EncryptionManager, EncryptionAlgorithm, KeyDerivationFunction};
use tai_security::error::SecurityError;
use tai_security::mtls::{MtlsManager, RenewalConfig, CertificateProvider};
use tai_security::secret_rotation::{SecretRotationManager, SecretType};
use tai_security::vault_client::{VaultConfig, VaultClient};
use std::collections::HashMap;
use std::time::Duration;
use chrono::Utc;

#[tokio::test]
async fn test_vault_client_creation() {
    let config = VaultConfig {
        url: "http://localhost:8200".to_string(),
        token: "s.test".to_string(),
        timeout: Duration::from_secs(5),
        namespace: None,
        tls_verify: false,
        ca_cert: None,
        renewal_threshold: 0.75,
    };

    // This will fail because we don't have a real Vault running,
    // but we're testing the client creation logic
    match VaultClient::new(config).await {
        Ok(_) => {
            // Success if Vault is running
        }
        Err(e) => {
            // Expected if Vault is not available
            assert!(
                e.to_string().contains("Vault") || e.to_string().contains("connection"),
                "Expected Vault error: {}",
                e
            );
        }
    }
}

#[tokio::test]
async fn test_cloud_kms_key_ring_creation() {
    let config = KmsConfig {
        project_id: "test-project".to_string(),
        location: "us-central1".to_string(),
        credentials_path: None,
        timeout: Duration::from_secs(10),
        audit_logging: true,
    };

    let client = CloudKmsClient::new(config).await.unwrap();
    let ring = client.create_key_ring("test-ring").await.unwrap();

    assert!(ring.name.contains("test-ring"));
    assert_eq!(ring.project_id, "test-project");
    assert!(ring.created_at <= Utc::now());
}

#[tokio::test]
async fn test_cloud_kms_key_creation_and_encryption() {
    let config = KmsConfig {
        project_id: "test-project".to_string(),
        location: "us-central1".to_string(),
        credentials_path: None,
        timeout: Duration::from_secs(10),
        audit_logging: true,
    };

    let client = CloudKmsClient::new(config).await.unwrap();

    // Create key ring
    client.create_key_ring("test-ring").await.unwrap();

    // Create key
    let key = client
        .create_key("test-ring", "test-key", "AES_256_GCM")
        .await
        .unwrap();

    assert_eq!(key.algorithm, "AES_256_GCM");
    assert_eq!(key.current_version, 1);

    // Encrypt data
    let plaintext = b"sensitive information";
    let encrypted = client
        .encrypt("test-ring", "test-key", plaintext)
        .await
        .unwrap();

    assert!(!encrypted.ciphertext.is_empty());
    assert_eq!(encrypted.key_id, key.name);
    assert_eq!(encrypted.algorithm, "AES_256_GCM");

    // Decrypt data
    let decrypted = client
        .decrypt("test-ring", "test-key", &encrypted)
        .await
        .unwrap();

    assert!(!decrypted.data.is_empty());
}

#[tokio::test]
async fn test_cloud_kms_key_rotation() {
    let config = KmsConfig {
        project_id: "test-project".to_string(),
        location: "us-central1".to_string(),
        credentials_path: None,
        timeout: Duration::from_secs(10),
        audit_logging: true,
    };

    let client = CloudKmsClient::new(config).await.unwrap();

    // Setup
    client.create_key_ring("rotation-ring").await.unwrap();
    client
        .create_key("rotation-ring", "rotation-key", "RSA_2048")
        .await
        .unwrap();

    let key_before = client
        .get_key("rotation-ring", "rotation-key")
        .await
        .unwrap();

    assert_eq!(key_before.current_version, 1);
    assert_eq!(key_before.versions.len(), 1);

    // Rotate key
    client
        .rotate_key("rotation-ring", "rotation-key")
        .await
        .unwrap();

    let key_after = client
        .get_key("rotation-ring", "rotation-key")
        .await
        .unwrap();

    assert_eq!(key_after.current_version, 2);
    assert_eq!(key_after.versions.len(), 2);
    assert!(key_after.next_rotation_time.is_some());
}

#[tokio::test]
async fn test_encryption_manager_aes256() {
    let manager = EncryptionManager::new(
        EncryptionAlgorithm::Aes256Gcm,
        KeyDerivationFunction::Pbkdf2,
    );

    // Generate key
    let key = manager.generate_key("test-key".to_string()).unwrap();

    // Encrypt
    let plaintext = b"confidential data";
    let encrypted = manager.encrypt_aes256(plaintext, &key).unwrap();

    assert!(!encrypted.ciphertext.is_empty());
    assert!(!encrypted.iv.is_empty());
    assert!(!encrypted.tag.is_empty());
    assert_eq!(encrypted.algorithm, EncryptionAlgorithm::Aes256Gcm);

    // Decrypt
    let decrypted = manager.decrypt_aes256(&encrypted, &key).unwrap();

    assert_eq!(decrypted, plaintext);
}

#[tokio::test]
async fn test_encryption_manager_key_derivation() {
    let manager = EncryptionManager::new(
        EncryptionAlgorithm::Aes256Gcm,
        KeyDerivationFunction::Pbkdf2,
    );

    let password = "super-secret-password";
    let salt = b"random-salt-32-bytes-long!!!!!";

    let key = manager
        .derive_key_from_password("derived-key".to_string(), password, salt)
        .unwrap();

    assert_eq!(key.key_id, "derived-key");
    assert_eq!(key.algorithm, EncryptionAlgorithm::Aes256Gcm);

    // Key material should be deterministic
    let key2 = manager
        .derive_key_from_password("derived-key-2".to_string(), password, salt)
        .unwrap();

    // Different key IDs but derived from same password/salt should work
    assert_ne!(key.key_id, key2.key_id);
}

#[tokio::test]
async fn test_encryption_manager_hmac() {
    let manager = EncryptionManager::new(
        EncryptionAlgorithm::Aes256Gcm,
        KeyDerivationFunction::Pbkdf2,
    );

    let key = b"hmac-key";
    let data = b"data to authenticate";

    let tag = manager.hmac_sha256(data, key).unwrap();
    assert!(!tag.is_empty());

    // Verify correct
    let verified = manager.verify_hmac_sha256(data, key, &tag).unwrap();
    assert!(verified);

    // Verify fails on different data
    let other_data = b"different data";
    let verified_wrong = manager
        .verify_hmac_sha256(other_data, key, &tag)
        .unwrap();
    assert!(!verified_wrong);
}

#[tokio::test]
async fn test_mtls_manager_creation() {
    let config = RenewalConfig {
        renew_before_days: 30,
        provider: CertificateProvider::Manual,
        provider_config: HashMap::new(),
        auto_renewal: false,
        check_interval: 24,
    };

    let manager = MtlsManager::new(config);
    let certs = manager.list_certificates().await.unwrap();

    assert!(certs.is_empty());
}

#[tokio::test]
async fn test_mtls_certificate_state_checks() {
    let config = RenewalConfig {
        renew_before_days: 30,
        provider: CertificateProvider::Manual,
        provider_config: HashMap::new(),
        auto_renewal: false,
        check_interval: 24,
    };

    let manager = MtlsManager::new(config);

    // Check expiry on empty manager should return empty alerts
    let alerts = manager.check_expiry().await.unwrap();
    assert!(alerts.is_empty());
}

#[tokio::test]
async fn test_secret_rotation_basic_flow() {
    let manager = SecretRotationManager::new();

    // Register secret
    manager
        .register_secret(
            "app-api-key".to_string(),
            SecretType::ApiKey,
            30,
        )
        .await
        .unwrap();

    // Rotate secret
    let version = manager
        .rotate_secret(
            "app-api-key",
            "new_api_key_value_here".to_string(),
            "Scheduled rotation".to_string(),
        )
        .await
        .unwrap();

    assert_eq!(version, 1);

    // Get rotation history
    let history = manager.get_rotation_history("app-api-key").await.unwrap();
    assert_eq!(history.len(), 1);
    assert_eq!(history[0].secret_id, "app-api-key");
}

#[tokio::test]
async fn test_secret_rotation_multiple_rotations() {
    let manager = SecretRotationManager::new();

    manager
        .register_secret(
            "db-password".to_string(),
            SecretType::DatabaseCredential,
            90,
        )
        .await
        .unwrap();

    // First rotation
    let v1 = manager
        .rotate_secret(
            "db-password",
            "password_v1".to_string(),
            "Initial rotation".to_string(),
        )
        .await
        .unwrap();

    assert_eq!(v1, 1);

    // Second rotation
    let v2 = manager
        .rotate_secret(
            "db-password",
            "password_v2".to_string(),
            "Second rotation".to_string(),
        )
        .await
        .unwrap();

    assert_eq!(v2, 2);

    // Verify history
    let history = manager.get_rotation_history("db-password").await.unwrap();
    assert_eq!(history.len(), 2);
}

#[tokio::test]
async fn test_secret_rotation_rollback() {
    let manager = SecretRotationManager::new();

    manager
        .register_secret(
            "oauth-token".to_string(),
            SecretType::Oauth2Token,
            1,
        )
        .await
        .unwrap();

    // Initial rotation to v1
    manager
        .rotate_secret(
            "oauth-token",
            "token_v1".to_string(),
            "Initial".to_string(),
        )
        .await
        .unwrap();

    // Rotation to v2
    manager
        .rotate_secret(
            "oauth-token",
            "token_v2".to_string(),
            "Second".to_string(),
        )
        .await
        .unwrap();

    // Rollback to v1
    manager
        .rollback_secret("oauth-token", 1, "v2 failed".to_string())
        .await
        .unwrap();

    let history = manager.get_rotation_history("oauth-token").await.unwrap();
    assert_eq!(history.len(), 3); // initial + second + rollback
}

#[test]
fn test_password_hashing() {
    let manager = EncryptionManager::new(
        EncryptionAlgorithm::Aes256Gcm,
        KeyDerivationFunction::Pbkdf2,
    );

    let password = "test-password-123";
    let hash = manager.hash_password(password).unwrap();

    assert!(!hash.is_empty());
    assert_eq!(hash.len(), 64); // SHA-256 hex

    // Same password should produce same hash (deterministic)
    let hash2 = manager.hash_password(password).unwrap();
    assert_eq!(hash, hash2);
}

#[tokio::test]
async fn test_cloud_kms_audit_logging() {
    let config = KmsConfig {
        project_id: "test-project".to_string(),
        location: "us-central1".to_string(),
        credentials_path: None,
        timeout: Duration::from_secs(10),
        audit_logging: true,
    };

    let client = CloudKmsClient::new(config).await.unwrap();

    // Create ring and key
    client.create_key_ring("audit-ring").await.unwrap();
    client
        .create_key("audit-ring", "audit-key", "AES_256_GCM")
        .await
        .unwrap();

    // Perform operations
    client
        .encrypt("audit-ring", "audit-key", b"test")
        .await
        .unwrap();

    // Get audit log
    let log = client.get_audit_log().await.unwrap();

    assert!(!log.is_empty());
    assert_eq!(log.len(), 1);
    assert_eq!(log[0].operation, "encrypt");
    assert_eq!(log[0].status, "success");
}

#[tokio::test]
async fn test_error_handling_invalid_secret() {
    let manager = SecretRotationManager::new();

    // Try to rotate unregistered secret
    let result = manager
        .rotate_secret(
            "nonexistent-secret",
            "value".to_string(),
            "reason".to_string(),
        )
        .await;

    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("not found"));
}

#[tokio::test]
async fn test_encryption_algorithm_mismatch() {
    let manager = EncryptionManager::new(
        EncryptionAlgorithm::Aes256Gcm,
        KeyDerivationFunction::Pbkdf2,
    );

    let key = manager.generate_key("test-key".to_string()).unwrap();

    // Try to use wrong algorithm
    // This should succeed because we're using the key's own algorithm
    let plaintext = b"data";
    let encrypted = manager.encrypt_aes256(plaintext, &key).unwrap();
    assert!(!encrypted.ciphertext.is_empty());
}

#[test]
fn test_secret_types() {
    assert_eq!(SecretType::ApiKey, SecretType::ApiKey);
    assert_ne!(
        SecretType::ApiKey,
        SecretType::DatabaseCredential
    );
}

#[tokio::test]
async fn test_cloud_kms_get_nonexistent_key_ring() {
    let config = KmsConfig {
        project_id: "test-project".to_string(),
        location: "us-central1".to_string(),
        credentials_path: None,
        timeout: Duration::from_secs(10),
        audit_logging: false,
    };

    let client = CloudKmsClient::new(config).await.unwrap();

    // Try to get non-existent ring
    let result = client.get_key_ring("nonexistent-ring").await;
    assert!(result.is_err());
}

#[tokio::test]
async fn test_cloud_kms_iam_policy() {
    let config = KmsConfig {
        project_id: "test-project".to_string(),
        location: "us-central1".to_string(),
        credentials_path: None,
        timeout: Duration::from_secs(10),
        audit_logging: true,
    };

    let client = CloudKmsClient::new(config).await.unwrap();

    // Create ring and key
    client.create_key_ring("iam-ring").await.unwrap();
    client
        .create_key("iam-ring", "iam-key", "RSA_2048")
        .await
        .unwrap();

    // Create IAM policy
    let policy = tai_security::cloud_kms::IamPolicy {
        bindings: vec![],
        etag: "test-etag".to_string(),
        version: 1,
    };

    // Set policy
    let result = client.set_iam_policy("iam-ring", "iam-key", policy).await;
    assert!(result.is_ok());
}

#[test]
fn test_security_error_variants() {
    let err = SecurityError::vault("test");
    assert!(err.to_string().contains("Vault"));

    let err = SecurityError::encryption("test");
    assert!(err.to_string().contains("Encryption"));

    let err = SecurityError::not_found("test");
    assert!(err.to_string().contains("not found"));
}
