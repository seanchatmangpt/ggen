//! Rate limiting for authentication endpoints

use async_trait::async_trait;
use redis::{aio::ConnectionManager, AsyncCommands};
use std::sync::Arc;

use crate::{errors::AuthError, AuthResult};

/// Rate limit configuration
#[derive(Debug, Clone)]
pub struct RateLimitConfig {
    pub max_attempts: u32,
    pub window_seconds: i64,
}

impl RateLimitConfig {
    /// Default configuration for login attempts
    pub fn login_default() -> Self {
        Self {
            max_attempts: 5,
            window_seconds: 300, // 5 minutes
        }
    }

    /// Strict configuration for sensitive operations
    pub fn strict() -> Self {
        Self {
            max_attempts: 3,
            window_seconds: 600, // 10 minutes
        }
    }
}

/// Rate limiter trait
#[async_trait]
pub trait RateLimiter: Send + Sync {
    /// Check if identifier is rate limited
    async fn is_rate_limited(&self, identifier: &str) -> AuthResult<bool>;

    /// Record an attempt
    async fn record_attempt(&self, identifier: &str) -> AuthResult<()>;

    /// Reset attempts for identifier
    async fn reset_attempts(&self, identifier: &str) -> AuthResult<()>;

    /// Get remaining attempts
    async fn remaining_attempts(&self, identifier: &str) -> AuthResult<u32>;

    /// Get time until reset (in seconds)
    async fn time_until_reset(&self, identifier: &str) -> AuthResult<Option<i64>>;
}

/// Redis-backed rate limiter
pub struct RedisRateLimiter {
    conn: Arc<ConnectionManager>,
    config: RateLimitConfig,
}

impl RedisRateLimiter {
    /// Create a new Redis rate limiter
    ///
    /// # Errors
    /// Returns error if connection cannot be established
    pub async fn new(redis_url: &str, config: RateLimitConfig) -> AuthResult<Self> {
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

    fn attempts_key(identifier: &str) -> String {
        format!("rate_limit:attempts:{}", identifier)
    }

    fn lockout_key(identifier: &str) -> String {
        format!("rate_limit:lockout:{}", identifier)
    }
}

#[async_trait]
impl RateLimiter for RedisRateLimiter {
    async fn is_rate_limited(&self, identifier: &str) -> AuthResult<bool> {
        let mut conn = self.conn.as_ref().clone();

        // Check if locked out
        let is_locked: bool = conn
            .exists(Self::lockout_key(identifier))
            .await
            .map_err(|e| AuthError::DatabaseError(format!("Failed to check lockout: {}", e)))?;

        if is_locked {
            return Ok(true);
        }

        // Check attempt count
        let attempts: Option<u32> = conn
            .get(Self::attempts_key(identifier))
            .await
            .map_err(|e| AuthError::DatabaseError(format!("Failed to get attempts: {}", e)))?;

        Ok(attempts.unwrap_or(0) >= self.config.max_attempts)
    }

    async fn record_attempt(&self, identifier: &str) -> AuthResult<()> {
        let mut conn = self.conn.as_ref().clone();

        // Increment attempt counter
        let attempts: u32 = conn
            .incr(Self::attempts_key(identifier), 1)
            .await
            .map_err(|e| AuthError::DatabaseError(format!("Failed to increment attempts: {}", e)))?;

        // Set expiration on first attempt
        if attempts == 1 {
            let _: () = conn
                .expire(Self::attempts_key(identifier), self.config.window_seconds)
                .await
                .map_err(|e| AuthError::DatabaseError(format!("Failed to set expiration: {}", e)))?;
        }

        // Lock out if max attempts reached
        if attempts >= self.config.max_attempts {
            let _: () = conn
                .set_ex(
                    Self::lockout_key(identifier),
                    "locked",
                    self.config.window_seconds as u64,
                )
                .await
                .map_err(|e| AuthError::DatabaseError(format!("Failed to set lockout: {}", e)))?;
        }

        Ok(())
    }

    async fn reset_attempts(&self, identifier: &str) -> AuthResult<()> {
        let mut conn = self.conn.as_ref().clone();

        // Delete both attempts and lockout keys
        let _: () = conn
            .del(Self::attempts_key(identifier))
            .await
            .map_err(|e| AuthError::DatabaseError(format!("Failed to delete attempts: {}", e)))?;

        let _: () = conn
            .del(Self::lockout_key(identifier))
            .await
            .map_err(|e| AuthError::DatabaseError(format!("Failed to delete lockout: {}", e)))?;

        Ok(())
    }

    async fn remaining_attempts(&self, identifier: &str) -> AuthResult<u32> {
        let mut conn = self.conn.as_ref().clone();

        let attempts: Option<u32> = conn
            .get(Self::attempts_key(identifier))
            .await
            .map_err(|e| AuthError::DatabaseError(format!("Failed to get attempts: {}", e)))?;

        let used = attempts.unwrap_or(0);
        let remaining = self.config.max_attempts.saturating_sub(used);

        Ok(remaining)
    }

    async fn time_until_reset(&self, identifier: &str) -> AuthResult<Option<i64>> {
        let mut conn = self.conn.as_ref().clone();

        // Check TTL on lockout key
        let ttl: i64 = conn
            .ttl(Self::lockout_key(identifier))
            .await
            .map_err(|e| AuthError::DatabaseError(format!("Failed to get TTL: {}", e)))?;

        if ttl > 0 {
            Ok(Some(ttl))
        } else {
            Ok(None)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    async fn create_test_limiter() -> RedisRateLimiter {
        let redis_url = std::env::var("REDIS_URL")
            .unwrap_or_else(|_| "redis://127.0.0.1:6379".to_string());

        RedisRateLimiter::new(&redis_url, RateLimitConfig::login_default())
            .await
            .unwrap()
    }

    #[tokio::test]
    #[ignore] // Requires Redis
    async fn test_rate_limit_not_exceeded() {
        // Arrange
        let limiter = create_test_limiter().await;
        let identifier = "user@example.com";

        // Act
        let result = limiter.is_rate_limited(identifier).await;

        // Assert
        assert!(result.is_ok());
        assert!(!result.unwrap());

        // Cleanup
        let _ = limiter.reset_attempts(identifier).await;
    }

    #[tokio::test]
    #[ignore] // Requires Redis
    async fn test_record_attempts() {
        // Arrange
        let limiter = create_test_limiter().await;
        let identifier = "user@example.com";

        // Act
        for _ in 0..3 {
            let _ = limiter.record_attempt(identifier).await;
        }
        let remaining = limiter.remaining_attempts(identifier).await.unwrap();

        // Assert
        assert_eq!(remaining, 2); // 5 max - 3 used = 2 remaining

        // Cleanup
        let _ = limiter.reset_attempts(identifier).await;
    }

    #[tokio::test]
    #[ignore] // Requires Redis
    async fn test_lockout_after_max_attempts() {
        // Arrange
        let limiter = create_test_limiter().await;
        let identifier = "user@example.com";

        // Act - record max attempts
        for _ in 0..5 {
            let _ = limiter.record_attempt(identifier).await;
        }
        let is_limited = limiter.is_rate_limited(identifier).await.unwrap();
        let remaining = limiter.remaining_attempts(identifier).await.unwrap();

        // Assert
        assert!(is_limited);
        assert_eq!(remaining, 0);

        // Cleanup
        let _ = limiter.reset_attempts(identifier).await;
    }

    #[tokio::test]
    #[ignore] // Requires Redis
    async fn test_reset_attempts() {
        // Arrange
        let limiter = create_test_limiter().await;
        let identifier = "user@example.com";
        for _ in 0..5 {
            let _ = limiter.record_attempt(identifier).await;
        }

        // Act
        let _ = limiter.reset_attempts(identifier).await;
        let is_limited = limiter.is_rate_limited(identifier).await.unwrap();
        let remaining = limiter.remaining_attempts(identifier).await.unwrap();

        // Assert
        assert!(!is_limited);
        assert_eq!(remaining, 5);
    }

    #[tokio::test]
    #[ignore] // Requires Redis
    async fn test_time_until_reset() {
        // Arrange
        let limiter = create_test_limiter().await;
        let identifier = "user@example.com";
        for _ in 0..5 {
            let _ = limiter.record_attempt(identifier).await;
        }

        // Act
        let ttl = limiter.time_until_reset(identifier).await.unwrap();

        // Assert
        assert!(ttl.is_some());
        assert!(ttl.unwrap() > 0);

        // Cleanup
        let _ = limiter.reset_attempts(identifier).await;
    }
}
