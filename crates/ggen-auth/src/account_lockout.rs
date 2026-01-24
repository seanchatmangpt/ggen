//! Account lockout mechanism for failed authentication attempts

use async_trait::async_trait;
use chrono::Utc;
use redis::{aio::ConnectionManager, AsyncCommands};
use std::sync::Arc;

use crate::{errors::AuthError, AuthResult};

/// Account lockout configuration
#[derive(Debug, Clone)]
pub struct LockoutConfig {
    pub max_failed_attempts: u32,
    pub lockout_duration_seconds: i64,
    pub attempt_window_seconds: i64,
}

impl Default for LockoutConfig {
    fn default() -> Self {
        Self {
            max_failed_attempts: 5,
            lockout_duration_seconds: 1800, // 30 minutes
            attempt_window_seconds: 300,    // 5 minutes
        }
    }
}

/// Account lockout status
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LockoutStatus {
    NotLocked,
    Locked { until: i64, attempts: u32 },
}

/// Account lockout manager trait
#[async_trait]
pub trait LockoutManager: Send + Sync {
    /// Record a failed login attempt
    async fn record_failed_attempt(&self, identifier: &str) -> AuthResult<LockoutStatus>;

    /// Reset failed attempts (on successful login)
    async fn reset_failed_attempts(&self, identifier: &str) -> AuthResult<()>;

    /// Check if account is locked
    async fn is_locked(&self, identifier: &str) -> AuthResult<bool>;

    /// Get lockout status
    async fn get_lockout_status(&self, identifier: &str) -> AuthResult<LockoutStatus>;

    /// Manually lock an account (for admin actions)
    async fn lock_account(&self, identifier: &str, duration_seconds: i64) -> AuthResult<()>;

    /// Manually unlock an account (for admin actions)
    async fn unlock_account(&self, identifier: &str) -> AuthResult<()>;
}

/// Redis-backed account lockout manager
pub struct RedisLockoutManager {
    conn: Arc<ConnectionManager>,
    config: LockoutConfig,
}

impl RedisLockoutManager {
    /// Create a new Redis lockout manager
    ///
    /// # Errors
    /// Returns error if connection cannot be established
    pub async fn new(redis_url: &str, config: LockoutConfig) -> AuthResult<Self> {
        let client = redis::Client::open(redis_url)
            .map_err(|e| AuthError::ConfigError(format!("Invalid Redis URL: {}", e)))?;

        let conn = ConnectionManager::new(client)
            .await
            .map_err(|e| AuthError::DatabaseError(format!("Failed to connect to Redis: {}", e)))?;

        Ok(Self {
            conn: Arc::new(conn),
            config,
        })
    }

    /// Create with default configuration
    ///
    /// # Errors
    /// Returns error if connection fails
    pub async fn with_defaults(redis_url: &str) -> AuthResult<Self> {
        Self::new(redis_url, LockoutConfig::default()).await
    }

    fn failed_attempts_key(identifier: &str) -> String {
        format!("account_lockout:attempts:{}", identifier)
    }

    fn lockout_key(identifier: &str) -> String {
        format!("account_lockout:locked:{}", identifier)
    }
}

#[async_trait]
impl LockoutManager for RedisLockoutManager {
    async fn record_failed_attempt(&self, identifier: &str) -> AuthResult<LockoutStatus> {
        // First check if already locked
        if self.is_locked(identifier).await? {
            return self.get_lockout_status(identifier).await;
        }

        let mut conn = self.conn.as_ref().clone();

        // Increment failed attempts counter
        let attempts: u32 = conn
            .incr(Self::failed_attempts_key(identifier), 1)
            .await
            .map_err(|e| AuthError::DatabaseError(format!("Failed to increment attempts: {}", e)))?;

        // Set expiration on first attempt
        if attempts == 1 {
            let _: () = conn
                .expire(
                    Self::failed_attempts_key(identifier),
                    self.config.attempt_window_seconds,
                )
                .await
                .map_err(|e| AuthError::DatabaseError(format!("Failed to set expiration: {}", e)))?;
        }

        // Lock account if max attempts reached
        if attempts >= self.config.max_failed_attempts {
            let lockout_until = Utc::now().timestamp() + self.config.lockout_duration_seconds;

            let _: () = conn
                .set_ex(
                    Self::lockout_key(identifier),
                    lockout_until,
                    self.config.lockout_duration_seconds as u64,
                )
                .await
                .map_err(|e| AuthError::DatabaseError(format!("Failed to set lockout: {}", e)))?;

            return Ok(LockoutStatus::Locked {
                until: lockout_until,
                attempts,
            });
        }

        Ok(LockoutStatus::NotLocked)
    }

    async fn reset_failed_attempts(&self, identifier: &str) -> AuthResult<()> {
        let mut conn = self.conn.as_ref().clone();

        // Delete failed attempts counter
        let _: () = conn
            .del(Self::failed_attempts_key(identifier))
            .await
            .map_err(|e| AuthError::DatabaseError(format!("Failed to delete attempts: {}", e)))?;

        Ok(())
    }

    async fn is_locked(&self, identifier: &str) -> AuthResult<bool> {
        let mut conn = self.conn.as_ref().clone();

        let locked: bool = conn
            .exists(Self::lockout_key(identifier))
            .await
            .map_err(|e| AuthError::DatabaseError(format!("Failed to check lockout: {}", e)))?;

        Ok(locked)
    }

    async fn get_lockout_status(&self, identifier: &str) -> AuthResult<LockoutStatus> {
        let mut conn = self.conn.as_ref().clone();

        let lockout_until: Option<i64> = conn
            .get(Self::lockout_key(identifier))
            .await
            .map_err(|e| AuthError::DatabaseError(format!("Failed to get lockout: {}", e)))?;

        if let Some(until) = lockout_until {
            let attempts: Option<u32> = conn
                .get(Self::failed_attempts_key(identifier))
                .await
                .map_err(|e| AuthError::DatabaseError(format!("Failed to get attempts: {}", e)))?;

            Ok(LockoutStatus::Locked {
                until,
                attempts: attempts.unwrap_or(0),
            })
        } else {
            Ok(LockoutStatus::NotLocked)
        }
    }

    async fn lock_account(&self, identifier: &str, duration_seconds: i64) -> AuthResult<()> {
        let mut conn = self.conn.as_ref().clone();

        let lockout_until = Utc::now().timestamp() + duration_seconds;

        let _: () = conn
            .set_ex(
                Self::lockout_key(identifier),
                lockout_until,
                duration_seconds as u64,
            )
            .await
            .map_err(|e| AuthError::DatabaseError(format!("Failed to lock account: {}", e)))?;

        Ok(())
    }

    async fn unlock_account(&self, identifier: &str) -> AuthResult<()> {
        let mut conn = self.conn.as_ref().clone();

        // Delete lockout key
        let _: () = conn
            .del(Self::lockout_key(identifier))
            .await
            .map_err(|e| AuthError::DatabaseError(format!("Failed to unlock account: {}", e)))?;

        // Also reset failed attempts
        self.reset_failed_attempts(identifier).await?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    async fn create_test_manager() -> RedisLockoutManager {
        let redis_url = std::env::var("REDIS_URL")
            .unwrap_or_else(|_| "redis://127.0.0.1:6379".to_string());

        RedisLockoutManager::with_defaults(&redis_url)
            .await
            .unwrap()
    }

    #[tokio::test]
    #[ignore] // Requires Redis
    async fn test_account_not_locked_initially() {
        // Arrange
        let manager = create_test_manager().await;
        let identifier = "user@example.com";

        // Act
        let is_locked = manager.is_locked(identifier).await.unwrap();

        // Assert
        assert!(!is_locked);

        // Cleanup
        let _ = manager.unlock_account(identifier).await;
    }

    #[tokio::test]
    #[ignore] // Requires Redis
    async fn test_record_failed_attempts() {
        // Arrange
        let manager = create_test_manager().await;
        let identifier = "user@example.com";

        // Act
        for _ in 0..3 {
            let _ = manager.record_failed_attempt(identifier).await;
        }
        let status = manager.get_lockout_status(identifier).await.unwrap();

        // Assert
        assert_eq!(status, LockoutStatus::NotLocked);

        // Cleanup
        let _ = manager.unlock_account(identifier).await;
    }

    #[tokio::test]
    #[ignore] // Requires Redis
    async fn test_lockout_after_max_attempts() {
        // Arrange
        let manager = create_test_manager().await;
        let identifier = "user@example.com";

        // Act - record 5 failed attempts
        let mut status = LockoutStatus::NotLocked;
        for _ in 0..5 {
            status = manager.record_failed_attempt(identifier).await.unwrap();
        }
        let is_locked = manager.is_locked(identifier).await.unwrap();

        // Assert
        assert!(is_locked);
        assert!(matches!(status, LockoutStatus::Locked { .. }));

        // Cleanup
        let _ = manager.unlock_account(identifier).await;
    }

    #[tokio::test]
    #[ignore] // Requires Redis
    async fn test_reset_failed_attempts() {
        // Arrange
        let manager = create_test_manager().await;
        let identifier = "user@example.com";
        for _ in 0..3 {
            let _ = manager.record_failed_attempt(identifier).await;
        }

        // Act
        let _ = manager.reset_failed_attempts(identifier).await;
        let status = manager.get_lockout_status(identifier).await.unwrap();

        // Assert
        assert_eq!(status, LockoutStatus::NotLocked);
    }

    #[tokio::test]
    #[ignore] // Requires Redis
    async fn test_manual_lock_unlock() {
        // Arrange
        let manager = create_test_manager().await;
        let identifier = "user@example.com";

        // Act - manually lock
        let _ = manager.lock_account(identifier, 600).await;
        let is_locked_before = manager.is_locked(identifier).await.unwrap();

        // Act - manually unlock
        let _ = manager.unlock_account(identifier).await;
        let is_locked_after = manager.is_locked(identifier).await.unwrap();

        // Assert
        assert!(is_locked_before);
        assert!(!is_locked_after);
    }

    #[tokio::test]
    #[ignore] // Requires Redis
    async fn test_lockout_status_details() {
        // Arrange
        let manager = create_test_manager().await;
        let identifier = "user@example.com";

        // Act - lock account
        for _ in 0..5 {
            let _ = manager.record_failed_attempt(identifier).await;
        }
        let status = manager.get_lockout_status(identifier).await.unwrap();

        // Assert
        match status {
            LockoutStatus::Locked { until, attempts } => {
                assert!(until > Utc::now().timestamp());
                assert_eq!(attempts, 5);
            }
            LockoutStatus::NotLocked => {
                panic!("Account should be locked");
            }
        }

        // Cleanup
        let _ = manager.unlock_account(identifier).await;
    }
}
