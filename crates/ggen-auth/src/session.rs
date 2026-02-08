//! Session management with Redis backend

use async_trait::async_trait;
use chrono::Utc;
use redis::{aio::ConnectionManager, AsyncCommands};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use uuid::Uuid;

use crate::{errors::AuthError, AuthResult};

/// Session data stored in Redis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SessionData {
    pub user_id: String,
    pub email: String,
    pub tier: String,
    pub scopes: Vec<String>,
    pub created_at: i64,
    pub last_accessed: i64,
    pub ip_address: Option<String>,
    pub user_agent: Option<String>,
}

/// Session configuration
#[derive(Debug, Clone)]
pub struct SessionConfig {
    pub ttl_seconds: i64,
    pub refresh_threshold_seconds: i64,
}

impl Default for SessionConfig {
    fn default() -> Self {
        Self {
            ttl_seconds: 86400,              // 24 hours
            refresh_threshold_seconds: 3600, // Refresh if < 1 hour remaining
        }
    }
}

/// Session manager trait
#[async_trait]
pub trait SessionManager: Send + Sync {
    /// Create a new session
    async fn create_session(
        &self, user_id: &str, email: &str, tier: &str, scopes: Vec<String>,
        ip_address: Option<String>, user_agent: Option<String>,
    ) -> AuthResult<String>;

    /// Get session data
    async fn get_session(&self, session_id: &str) -> AuthResult<SessionData>;

    /// Update session last accessed time
    async fn touch_session(&self, session_id: &str) -> AuthResult<()>;

    /// Delete session (logout)
    async fn delete_session(&self, session_id: &str) -> AuthResult<()>;

    /// Delete all sessions for a user
    async fn delete_user_sessions(&self, user_id: &str) -> AuthResult<()>;

    /// Check if session exists
    async fn session_exists(&self, session_id: &str) -> AuthResult<bool>;
}

/// Redis-backed session manager
pub struct RedisSessionManager {
    conn: Arc<ConnectionManager>,
    config: SessionConfig,
}

impl RedisSessionManager {
    /// Create a new Redis session manager
    ///
    /// # Errors
    /// Returns error if connection cannot be established
    pub async fn new(redis_url: &str, config: SessionConfig) -> AuthResult<Self> {
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
        Self::new(redis_url, SessionConfig::default()).await
    }

    fn session_key(session_id: &str) -> String {
        format!("session:{}", session_id)
    }

    fn user_sessions_key(user_id: &str) -> String {
        format!("user_sessions:{}", user_id)
    }
}

#[async_trait]
impl SessionManager for RedisSessionManager {
    async fn create_session(
        &self, user_id: &str, email: &str, tier: &str, scopes: Vec<String>,
        ip_address: Option<String>, user_agent: Option<String>,
    ) -> AuthResult<String> {
        let session_id = Uuid::new_v4().to_string();
        let now = Utc::now().timestamp();

        let session_data = SessionData {
            user_id: user_id.to_string(),
            email: email.to_string(),
            tier: tier.to_string(),
            scopes,
            created_at: now,
            last_accessed: now,
            ip_address,
            user_agent,
        };

        let session_json =
            serde_json::to_string(&session_data).map_err(|_| AuthError::InternalError)?;

        let mut conn = self.conn.as_ref().clone();

        // Store session data with TTL
        let _: () = conn
            .set_ex(
                Self::session_key(&session_id),
                session_json,
                self.config.ttl_seconds as u64,
            )
            .await
            .map_err(|e| AuthError::DatabaseError(format!("Failed to create session: {}", e)))?;

        // Add to user's session set
        let _: () = conn
            .sadd(Self::user_sessions_key(user_id), &session_id)
            .await
            .map_err(|e| {
                AuthError::DatabaseError(format!("Failed to track user session: {}", e))
            })?;

        Ok(session_id)
    }

    async fn get_session(&self, session_id: &str) -> AuthResult<SessionData> {
        let mut conn = self.conn.as_ref().clone();

        let session_json: Option<String> = conn
            .get(Self::session_key(session_id))
            .await
            .map_err(|e| AuthError::DatabaseError(format!("Failed to get session: {}", e)))?;

        session_json.map_or(Err(AuthError::InvalidToken), |json| {
            serde_json::from_str(&json).map_err(|_| AuthError::InternalError)
        })
    }

    async fn touch_session(&self, session_id: &str) -> AuthResult<()> {
        let mut session_data = self.get_session(session_id).await?;
        session_data.last_accessed = Utc::now().timestamp();

        let session_json =
            serde_json::to_string(&session_data).map_err(|_| AuthError::InternalError)?;

        let mut conn = self.conn.as_ref().clone();

        // Update session data and refresh TTL
        let _: () = conn
            .set_ex(
                Self::session_key(session_id),
                session_json,
                self.config.ttl_seconds as u64,
            )
            .await
            .map_err(|e| AuthError::DatabaseError(format!("Failed to touch session: {}", e)))?;

        Ok(())
    }

    async fn delete_session(&self, session_id: &str) -> AuthResult<()> {
        // Get session to find user_id
        let session_data = self.get_session(session_id).await?;

        let mut conn = self.conn.as_ref().clone();

        // Delete session
        let _: () = conn
            .del(Self::session_key(session_id))
            .await
            .map_err(|e| AuthError::DatabaseError(format!("Failed to delete session: {}", e)))?;

        // Remove from user's session set
        let _: () = conn
            .srem(Self::user_sessions_key(&session_data.user_id), session_id)
            .await
            .map_err(|e| {
                AuthError::DatabaseError(format!("Failed to remove user session: {}", e))
            })?;

        Ok(())
    }

    async fn delete_user_sessions(&self, user_id: &str) -> AuthResult<()> {
        let mut conn = self.conn.as_ref().clone();

        // Get all session IDs for user
        let session_ids: Vec<String> = conn
            .smembers(Self::user_sessions_key(user_id))
            .await
            .map_err(|e| AuthError::DatabaseError(format!("Failed to get user sessions: {}", e)))?;

        // Delete all sessions
        for session_id in &session_ids {
            let _: () = conn.del(Self::session_key(session_id)).await.map_err(|e| {
                AuthError::DatabaseError(format!("Failed to delete session: {}", e))
            })?;
        }

        // Delete user's session set
        let _: () = conn
            .del(Self::user_sessions_key(user_id))
            .await
            .map_err(|e| {
                AuthError::DatabaseError(format!("Failed to delete user session set: {}", e))
            })?;

        Ok(())
    }

    async fn session_exists(&self, session_id: &str) -> AuthResult<bool> {
        let mut conn = self.conn.as_ref().clone();

        let exists: bool = conn
            .exists(Self::session_key(session_id))
            .await
            .map_err(|e| AuthError::DatabaseError(format!("Failed to check session: {}", e)))?;

        Ok(exists)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Note: These tests require a Redis instance running
    // In CI/CD, use testcontainers to spin up Redis automatically

    async fn create_test_manager() -> RedisSessionManager {
        let redis_url =
            std::env::var("REDIS_URL").unwrap_or_else(|_| "redis://127.0.0.1:6379".to_string());

        RedisSessionManager::with_defaults(&redis_url)
            .await
            .unwrap()
    }

    #[tokio::test]
    #[ignore] // Requires Redis
    async fn test_create_session() {
        // Arrange
        let manager = create_test_manager().await;
        let scopes = vec!["read".to_string(), "write".to_string()];

        // Act
        let result = manager
            .create_session(
                "user123",
                "user@example.com",
                "pro",
                scopes,
                Some("127.0.0.1".to_string()),
                Some("Test-Agent/1.0".to_string()),
            )
            .await;

        // Assert
        assert!(result.is_ok());
        let session_id = result.unwrap();
        assert!(!session_id.is_empty());

        // Cleanup
        let _ = manager.delete_session(&session_id).await;
    }

    #[tokio::test]
    #[ignore] // Requires Redis
    async fn test_get_session() {
        // Arrange
        let manager = create_test_manager().await;
        let scopes = vec!["read".to_string()];
        let session_id = manager
            .create_session("user123", "user@example.com", "free", scopes, None, None)
            .await
            .unwrap();

        // Act
        let result = manager.get_session(&session_id).await;

        // Assert
        assert!(result.is_ok());
        let session_data = result.unwrap();
        assert_eq!(session_data.user_id, "user123");
        assert_eq!(session_data.email, "user@example.com");
        assert_eq!(session_data.tier, "free");

        // Cleanup
        let _ = manager.delete_session(&session_id).await;
    }

    #[tokio::test]
    #[ignore] // Requires Redis
    async fn test_delete_session() {
        // Arrange
        let manager = create_test_manager().await;
        let session_id = manager
            .create_session("user123", "user@example.com", "pro", vec![], None, None)
            .await
            .unwrap();

        // Act
        let delete_result = manager.delete_session(&session_id).await;
        let exists = manager.session_exists(&session_id).await.unwrap();

        // Assert
        assert!(delete_result.is_ok());
        assert!(!exists);
    }

    #[tokio::test]
    #[ignore] // Requires Redis
    async fn test_delete_user_sessions() {
        // Arrange
        let manager = create_test_manager().await;
        let session1 = manager
            .create_session("user123", "user@example.com", "pro", vec![], None, None)
            .await
            .unwrap();
        let session2 = manager
            .create_session("user123", "user@example.com", "pro", vec![], None, None)
            .await
            .unwrap();

        // Act
        let result = manager.delete_user_sessions("user123").await;
        let exists1 = manager.session_exists(&session1).await.unwrap();
        let exists2 = manager.session_exists(&session2).await.unwrap();

        // Assert
        assert!(result.is_ok());
        assert!(!exists1);
        assert!(!exists2);
    }
}
