//! Security tests for secrets management
//!
//! These tests verify security properties including:
//! - Key exposure prevention
//! - Timing attack resistance
//! - Memory safety
//! - Cryptographic strength
//! - Access control

use ggen_utils::error::Result;
use ggen_utils::secrets::{EncryptionProvider, SecretType, SecretsManager};
use std::time::Instant;

// ============================================================================
// Security Tests: Key Exposure Prevention (Chicago TDD - AAA Pattern)
// ============================================================================

#[test]
fn test_encryption_key_not_leaked_in_debug() {
    // Arrange
    let key = b"12345678901234567890123456789012";
    let provider = EncryptionProvider::new(key).expect("Failed to create provider");

    // Act
    let debug_output = format!("{:?}", provider);

    // Assert - key should not appear in debug output
    assert!(!debug_output.contains("12345678901234567890123456789012"));
}

#[test]
fn test_encrypted_data_differs_from_plaintext() {
    // Arrange
    let key = b"12345678901234567890123456789012";
    let provider = EncryptionProvider::new(key).expect("Failed to create provider");
    let plaintext = "sensitive_secret";

    // Act
    let encrypted = provider.encrypt(plaintext).expect("Encryption failed");

    // Assert - ciphertext should not contain plaintext
    let encrypted_str = String::from_utf8_lossy(&encrypted);
    assert!(!encrypted_str.contains(plaintext));
}

#[test]
fn test_different_keys_produce_different_ciphertext() {
    // Arrange
    let key1 = b"12345678901234567890123456789012";
    let key2 = b"abcdefghijklmnopqrstuvwxyz123456";
    let provider1 = EncryptionProvider::new(key1).expect("Failed to create provider");
    let provider2 = EncryptionProvider::new(key2).expect("Failed to create provider");
    let plaintext = "secret";

    // Act
    let encrypted1 = provider1.encrypt(plaintext).expect("Encryption failed");
    let encrypted2 = provider2.encrypt(plaintext).expect("Encryption failed");

    // Assert - different keys should produce different ciphertext
    assert_ne!(encrypted1, encrypted2);
}

#[test]
fn test_decryption_with_wrong_key_fails() {
    // Arrange
    let key1 = b"12345678901234567890123456789012";
    let key2 = b"abcdefghijklmnopqrstuvwxyz123456";
    let provider1 = EncryptionProvider::new(key1).expect("Failed to create provider");
    let provider2 = EncryptionProvider::new(key2).expect("Failed to create provider");
    let plaintext = "secret";

    let encrypted = provider1.encrypt(plaintext).expect("Encryption failed");

    // Act
    let result = provider2.decrypt(&encrypted);

    // Assert - wrong key should fail decryption
    assert!(result.is_err());
}

#[test]
fn test_nonce_uniqueness() {
    // Arrange
    let key = b"12345678901234567890123456789012";
    let provider = EncryptionProvider::new(key).expect("Failed to create provider");
    let plaintext = "secret";

    // Act - encrypt same plaintext multiple times
    let mut nonces = Vec::new();
    for _ in 0..10 {
        let encrypted = provider.encrypt(plaintext).expect("Encryption failed");
        // Extract nonce (first 12 bytes)
        nonces.push(encrypted[0..12].to_vec());
    }

    // Assert - all nonces should be unique
    let unique_nonces: std::collections::HashSet<_> = nonces.iter().collect();
    assert_eq!(unique_nonces.len(), nonces.len());
}

#[test]
fn test_authentication_tag_verification() {
    // Arrange
    let key = b"12345678901234567890123456789012";
    let provider = EncryptionProvider::new(key).expect("Failed to create provider");
    let plaintext = "secret";
    let mut encrypted = provider.encrypt(plaintext).expect("Encryption failed");

    // Act - tamper with authentication tag (last 16 bytes)
    let len = encrypted.len();
    encrypted[len - 1] ^= 0xFF;

    let result = provider.decrypt(&encrypted);

    // Assert - tampering should be detected
    assert!(result.is_err());
}

#[test]
fn test_ciphertext_modification_detected() {
    // Arrange
    let key = b"12345678901234567890123456789012";
    let provider = EncryptionProvider::new(key).expect("Failed to create provider");
    let plaintext = "secret_message";
    let mut encrypted = provider.encrypt(plaintext).expect("Encryption failed");

    // Act - modify ciphertext (not nonce, not tag)
    if encrypted.len() > 28 {
        encrypted[20] ^= 0xFF;
    }

    let result = provider.decrypt(&encrypted);

    // Assert - modification should be detected
    assert!(result.is_err());
}

// ============================================================================
// Security Tests: Timing Attack Resistance
// ============================================================================

#[test]
fn test_decryption_timing_constant() {
    // Arrange
    let key = b"12345678901234567890123456789012";
    let provider = EncryptionProvider::new(key).expect("Failed to create provider");

    let valid_encrypted = provider.encrypt("secret").expect("Encryption failed");
    let mut invalid_encrypted = valid_encrypted.clone();
    invalid_encrypted[20] ^= 0xFF;

    // Act - measure decryption time for valid and invalid ciphertexts
    let iterations = 1000;

    let start_valid = Instant::now();
    for _ in 0..iterations {
        let _ = provider.decrypt(&valid_encrypted);
    }
    let duration_valid = start_valid.elapsed();

    let start_invalid = Instant::now();
    for _ in 0..iterations {
        let _ = provider.decrypt(&invalid_encrypted);
    }
    let duration_invalid = start_invalid.elapsed();

    // Assert - timing difference should be minimal (< 20% variance)
    let ratio = duration_valid.as_nanos() as f64 / duration_invalid.as_nanos() as f64;
    assert!(
        ratio > 0.8 && ratio < 1.2,
        "Timing variance too high: {}",
        ratio
    );
}

#[test]
fn test_key_derivation_timing_consistent() {
    // Arrange
    let password = "password";
    let salt = b"salt12345678901234567890";

    // Act - measure key derivation time multiple times
    let mut durations = Vec::new();
    for _ in 0..5 {
        let start = Instant::now();
        let _ = EncryptionProvider::derive_key_from_password(password, salt);
        durations.push(start.elapsed());
    }

    // Assert - timing should be consistent (< 10% variance)
    let avg: std::time::Duration =
        durations.iter().sum::<std::time::Duration>() / durations.len() as u32;
    for duration in &durations {
        let ratio = duration.as_nanos() as f64 / avg.as_nanos() as f64;
        assert!(
            ratio > 0.9 && ratio < 1.1,
            "Timing variance too high: {}",
            ratio
        );
    }
}

// ============================================================================
// Security Tests: Cryptographic Strength
// ============================================================================

#[test]
fn test_encryption_output_entropy() {
    // Arrange
    let key = b"12345678901234567890123456789012";
    let provider = EncryptionProvider::new(key).expect("Failed to create provider");
    let plaintext = "a".repeat(100);

    // Act
    let encrypted = provider.encrypt(&plaintext).expect("Encryption failed");

    // Assert - ciphertext should have high entropy (no obvious patterns)
    // Count unique bytes
    let unique_bytes: std::collections::HashSet<_> = encrypted.iter().collect();
    assert!(
        unique_bytes.len() > 50,
        "Low entropy: only {} unique bytes",
        unique_bytes.len()
    );
}

#[test]
fn test_key_derivation_iterations() {
    // Arrange
    let password = "weak_password";
    let salt = b"salt12345678901234567890";

    // Act - measure key derivation time (should take reasonable time)
    let start = Instant::now();
    let _ = EncryptionProvider::derive_key_from_password(password, salt);
    let duration = start.elapsed();

    // Assert - should take at least 10ms (100k iterations)
    assert!(
        duration.as_millis() >= 10,
        "Key derivation too fast: {:?}",
        duration
    );
}

#[test]
fn test_password_salt_independence() {
    // Arrange
    let password = "password";
    let salt1 = b"salt1111111111111111111";
    let salt2 = b"salt2222222222222222222";

    // Act
    let provider1 =
        EncryptionProvider::derive_key_from_password(password, salt1).expect("Derivation failed");
    let provider2 =
        EncryptionProvider::derive_key_from_password(password, salt2).expect("Derivation failed");

    let plaintext = "test";
    let encrypted1 = provider1.encrypt(plaintext).expect("Encryption failed");

    // Assert - different salts should produce incompatible keys
    let result = provider2.decrypt(&encrypted1);
    assert!(result.is_err());
}

// ============================================================================
// Security Tests: SecretsManager Access Control
// ============================================================================

#[tokio::test]
async fn test_secrets_manager_nonexistent_key() {
    // Arrange
    let key = b"12345678901234567890123456789012";
    let manager = SecretsManager::with_encryption(key).expect("Failed to create manager");

    // Act
    let result = manager.get_secret("nonexistent").await;

    // Assert - should fail gracefully
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("not found"));
}

#[tokio::test]
async fn test_secrets_manager_audit_trail() {
    // Arrange
    let key = b"12345678901234567890123456789012";
    let manager = SecretsManager::with_encryption(key).expect("Failed to create manager");

    // Act - perform operations
    manager
        .store_secret("audit_key", "value", SecretType::ApiKey)
        .await
        .expect("Store failed");

    manager.get_secret("audit_key").await.expect("Get failed");

    // Assert - audit log should contain entries
    let audit_log = manager
        .get_audit_log(10)
        .await
        .expect("Get audit log failed");
    assert!(audit_log.len() >= 2);
}

#[tokio::test]
async fn test_secrets_manager_rotation_versioning() {
    // Arrange
    let key = b"12345678901234567890123456789012";
    let manager = SecretsManager::with_encryption(key).expect("Failed to create manager");

    manager
        .store_secret("version_test", "v1", SecretType::SigningKey)
        .await
        .expect("Store failed");

    // Act
    manager
        .rotate_secret("version_test", "v2")
        .await
        .expect("Rotation failed");

    let secret = manager
        .get_secret("version_test")
        .await
        .expect("Get failed");

    // Assert - version should increment
    assert_eq!(secret.metadata().version, 2);
    assert!(secret.metadata().last_rotated.is_some());
}

#[tokio::test]
async fn test_secrets_manager_delete_removes_secret() {
    // Arrange
    let key = b"12345678901234567890123456789012";
    let manager = SecretsManager::with_encryption(key).expect("Failed to create manager");

    manager
        .store_secret("delete_test", "value", SecretType::Generic)
        .await
        .expect("Store failed");

    // Act
    manager
        .delete_secret("delete_test")
        .await
        .expect("Delete failed");

    // Assert - secret should no longer be accessible
    let result = manager.get_secret("delete_test").await;
    assert!(result.is_err());
}

// ============================================================================
// Security Tests: Memory Safety
// ============================================================================

#[tokio::test]
async fn test_secrets_manager_concurrent_access_safe() {
    // Arrange
    let key = b"12345678901234567890123456789012";
    let manager = std::sync::Arc::new(
        SecretsManager::with_encryption(key).expect("Failed to create manager"),
    );

    manager
        .store_secret("concurrent", "value", SecretType::Generic)
        .await
        .expect("Store failed");

    // Act - concurrent reads
    let handles: Vec<_> = (0..10)
        .map(|_| {
            let mgr = manager.clone();
            tokio::spawn(async move { mgr.get_secret("concurrent").await })
        })
        .collect();

    // Assert - all reads should succeed
    for handle in handles {
        let result = handle.await.expect("Task panicked");
        assert!(result.is_ok());
    }
}

#[test]
fn test_encryption_provider_clone_safety() {
    // Arrange
    let key = b"12345678901234567890123456789012";
    let provider = EncryptionProvider::new(key).expect("Failed to create provider");

    // Act - clone provider
    let cloned = provider.clone();

    // Assert - both should work independently
    let plaintext = "test";
    let encrypted1 = provider.encrypt(plaintext).expect("Encryption failed");
    let encrypted2 = cloned.encrypt(plaintext).expect("Encryption failed");

    // Can decrypt with either provider
    assert!(provider.decrypt(&encrypted2).is_ok());
    assert!(cloned.decrypt(&encrypted1).is_ok());
}

#[test]
fn test_large_secret_encryption() {
    // Arrange
    let key = b"12345678901234567890123456789012";
    let provider = EncryptionProvider::new(key).expect("Failed to create provider");
    let large_secret = "x".repeat(1_000_000);

    // Act
    let encrypted = provider.encrypt(&large_secret).expect("Encryption failed");
    let decrypted = provider.decrypt(&encrypted).expect("Decryption failed");

    // Assert
    assert_eq!(decrypted, large_secret);
}

#[test]
fn test_special_characters_in_secrets() {
    // Arrange
    let key = b"12345678901234567890123456789012";
    let provider = EncryptionProvider::new(key).expect("Failed to create provider");
    let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?`~\n\t\r";

    // Act
    let encrypted = provider.encrypt(special_chars).expect("Encryption failed");
    let decrypted = provider.decrypt(&encrypted).expect("Decryption failed");

    // Assert
    assert_eq!(decrypted, special_chars);
}
