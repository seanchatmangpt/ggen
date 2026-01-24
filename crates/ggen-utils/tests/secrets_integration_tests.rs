//! Integration tests for secrets management with Vault backend
//!
//! These tests verify the complete integration with HashiCorp Vault,
//! including connection, storage, retrieval, and rotation.
//!
//! Note: These tests require a running Vault instance.
//! Run with: `docker run -p 8200:8200 -e VAULT_DEV_ROOT_TOKEN_ID=root vault`

use ggen_utils::error::Result;
use ggen_utils::secrets::{SecretsManager, SecretType, VaultConfig};
use std::time::Duration;
use tokio::time::sleep;

// ============================================================================
// Helper Functions
// ============================================================================

/// Check if Vault is available for testing
async fn is_vault_available() -> bool {
    let client = reqwest::Client::new();
    client
        .get("http://localhost:8200/v1/sys/health")
        .send()
        .await
        .is_ok()
}

/// Create test Vault configuration
fn create_test_vault_config() -> VaultConfig {
    VaultConfig {
        address: "http://localhost:8200".to_string(),
        token: "root".to_string(),
        mount_path: "secret".to_string(),
    }
}

// ============================================================================
// Integration Tests: Vault Backend (Chicago TDD - AAA Pattern)
// ============================================================================

#[tokio::test]
async fn test_vault_store_and_retrieve_secret() -> Result<()> {
    // Arrange
    if !is_vault_available().await {
        eprintln!("Skipping Vault test: Vault not available");
        return Ok(());
    }

    let config = create_test_vault_config();
    let manager = SecretsManager::with_vault(config).await?;

    // Act
    manager
        .store_secret("test_api_key", "sk_test_12345", SecretType::ApiKey)
        .await?;

    let secret = manager.get_secret("test_api_key").await?;

    // Assert
    assert_eq!(secret.value(), "sk_test_12345");
    assert_eq!(secret.metadata().secret_type, SecretType::ApiKey);

    // Cleanup
    manager.delete_secret("test_api_key").await?;

    Ok(())
}

#[tokio::test]
async fn test_vault_store_database_credentials() -> Result<()> {
    // Arrange
    if !is_vault_available().await {
        eprintln!("Skipping Vault test: Vault not available");
        return Ok(());
    }

    let config = create_test_vault_config();
    let manager = SecretsManager::with_vault(config).await?;

    // Act
    manager
        .store_secret(
            "db_password",
            "postgres_secure_password",
            SecretType::DatabaseCredential,
        )
        .await?;

    let secret = manager.get_secret("db_password").await?;

    // Assert
    assert_eq!(secret.value(), "postgres_secure_password");
    assert_eq!(secret.metadata().secret_type, SecretType::DatabaseCredential);

    // Cleanup
    manager.delete_secret("db_password").await?;

    Ok(())
}

#[tokio::test]
async fn test_vault_rotate_secret() -> Result<()> {
    // Arrange
    if !is_vault_available().await {
        eprintln!("Skipping Vault test: Vault not available");
        return Ok(());
    }

    let config = create_test_vault_config();
    let manager = SecretsManager::with_vault(config).await?;

    manager
        .store_secret("rotation_test", "old_value", SecretType::SigningKey)
        .await?;

    // Act
    manager.rotate_secret("rotation_test", "new_value").await?;

    let secret = manager.get_secret("rotation_test").await?;

    // Assert
    assert_eq!(secret.value(), "new_value");
    assert_eq!(secret.metadata().version, 2);

    // Cleanup
    manager.delete_secret("rotation_test").await?;

    Ok(())
}

#[tokio::test]
async fn test_vault_delete_secret() -> Result<()> {
    // Arrange
    if !is_vault_available().await {
        eprintln!("Skipping Vault test: Vault not available");
        return Ok(());
    }

    let config = create_test_vault_config();
    let manager = SecretsManager::with_vault(config).await?;

    manager
        .store_secret("delete_test", "temporary_value", SecretType::Generic)
        .await?;

    // Act
    manager.delete_secret("delete_test").await?;

    // Assert - should fail to retrieve deleted secret
    let result = manager.get_secret("delete_test").await;
    assert!(result.is_err());

    Ok(())
}

#[tokio::test]
async fn test_vault_get_nonexistent_secret() -> Result<()> {
    // Arrange
    if !is_vault_available().await {
        eprintln!("Skipping Vault test: Vault not available");
        return Ok(());
    }

    let config = create_test_vault_config();
    let manager = SecretsManager::with_vault(config).await?;

    // Act
    let result = manager.get_secret("nonexistent_key").await;

    // Assert
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("not found"));

    Ok(())
}

#[tokio::test]
async fn test_vault_multiple_secrets() -> Result<()> {
    // Arrange
    if !is_vault_available().await {
        eprintln!("Skipping Vault test: Vault not available");
        return Ok(());
    }

    let config = create_test_vault_config();
    let manager = SecretsManager::with_vault(config).await?;

    // Act - store multiple secrets
    manager
        .store_secret("key1", "value1", SecretType::ApiKey)
        .await?;
    manager
        .store_secret("key2", "value2", SecretType::DatabaseCredential)
        .await?;
    manager
        .store_secret("key3", "value3", SecretType::SigningKey)
        .await?;

    // Assert - retrieve all secrets
    let secret1 = manager.get_secret("key1").await?;
    let secret2 = manager.get_secret("key2").await?;
    let secret3 = manager.get_secret("key3").await?;

    assert_eq!(secret1.value(), "value1");
    assert_eq!(secret2.value(), "value2");
    assert_eq!(secret3.value(), "value3");

    // Cleanup
    manager.delete_secret("key1").await?;
    manager.delete_secret("key2").await?;
    manager.delete_secret("key3").await?;

    Ok(())
}

#[tokio::test]
async fn test_vault_secret_versioning() -> Result<()> {
    // Arrange
    if !is_vault_available().await {
        eprintln!("Skipping Vault test: Vault not available");
        return Ok(());
    }

    let config = create_test_vault_config();
    let manager = SecretsManager::with_vault(config).await?;

    manager
        .store_secret("versioned_key", "v1", SecretType::SigningKey)
        .await?;

    // Act - multiple rotations
    manager.rotate_secret("versioned_key", "v2").await?;
    manager.rotate_secret("versioned_key", "v3").await?;
    manager.rotate_secret("versioned_key", "v4").await?;

    let secret = manager.get_secret("versioned_key").await?;

    // Assert
    assert_eq!(secret.value(), "v4");
    assert_eq!(secret.metadata().version, 4);

    // Cleanup
    manager.delete_secret("versioned_key").await?;

    Ok(())
}

#[tokio::test]
async fn test_vault_audit_logging() -> Result<()> {
    // Arrange
    if !is_vault_available().await {
        eprintln!("Skipping Vault test: Vault not available");
        return Ok(());
    }

    let config = create_test_vault_config();
    let manager = SecretsManager::with_vault(config).await?;

    // Act - perform multiple operations
    manager
        .store_secret("audit_test", "value1", SecretType::ApiKey)
        .await?;
    manager.get_secret("audit_test").await?;
    manager.rotate_secret("audit_test", "value2").await?;
    manager.delete_secret("audit_test").await?;

    // Small delay to ensure audit logs are written
    sleep(Duration::from_millis(100)).await;

    // Assert - check audit log
    let audit_entries = manager.get_audit_log(10).await?;
    assert!(audit_entries.len() >= 4);

    Ok(())
}

#[tokio::test]
async fn test_vault_concurrent_access() -> Result<()> {
    // Arrange
    if !is_vault_available().await {
        eprintln!("Skipping Vault test: Vault not available");
        return Ok(());
    }

    let config = create_test_vault_config();
    let manager = std::sync::Arc::new(SecretsManager::with_vault(config).await?);

    // Store initial secret
    manager
        .store_secret("concurrent_test", "initial", SecretType::Generic)
        .await?;

    // Act - concurrent reads
    let manager1 = manager.clone();
    let manager2 = manager.clone();
    let manager3 = manager.clone();

    let (result1, result2, result3) = tokio::join!(
        manager1.get_secret("concurrent_test"),
        manager2.get_secret("concurrent_test"),
        manager3.get_secret("concurrent_test"),
    );

    // Assert - all reads succeed
    assert!(result1.is_ok());
    assert!(result2.is_ok());
    assert!(result3.is_ok());

    // Cleanup
    manager.delete_secret("concurrent_test").await?;

    Ok(())
}

#[tokio::test]
async fn test_vault_unicode_secrets() -> Result<()> {
    // Arrange
    if !is_vault_available().await {
        eprintln!("Skipping Vault test: Vault not available");
        return Ok(());
    }

    let config = create_test_vault_config();
    let manager = SecretsManager::with_vault(config).await?;

    let unicode_secret = "パスワード🔒";

    // Act
    manager
        .store_secret("unicode_test", unicode_secret, SecretType::Generic)
        .await?;

    let secret = manager.get_secret("unicode_test").await?;

    // Assert
    assert_eq!(secret.value(), unicode_secret);

    // Cleanup
    manager.delete_secret("unicode_test").await?;

    Ok(())
}
