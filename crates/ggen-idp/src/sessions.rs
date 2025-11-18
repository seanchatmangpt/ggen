/// Session Management - handles user sessions, token lifecycle, and revocation
use crate::models::Session;
use async_trait::async_trait;
use chrono::{DateTime, Duration, Utc};
use uuid::Uuid;

/// Session Store interface
#[async_trait]
pub trait SessionStore: Send + Sync {
    /// Create a new session
    async fn create_session(&self, session: Session) -> Result<Session, String>;

    /// Get session by ID
    async fn get_session(&self, session_id: Uuid) -> Result<Option<Session>, String>;

    /// Get session by token
    async fn get_session_by_token(&self, token: &str) -> Result<Option<Session>, String>;

    /// Update session
    async fn update_session(&self, session: Session) -> Result<(), String>;

    /// Revoke session
    async fn revoke_session(&self, session_id: Uuid) -> Result<(), String>;

    /// Revoke all sessions for a user
    async fn revoke_user_sessions(&self, user_id: Uuid) -> Result<(), String>;

    /// Clean up expired sessions
    async fn cleanup_expired_sessions(&self) -> Result<usize, String>;

    /// List active sessions for a user
    async fn list_user_sessions(&self, user_id: Uuid) -> Result<Vec<Session>, String>;
}

/// In-memory session store (for testing)
pub struct InMemorySessionStore {
    sessions: std::sync::Arc<tokio::sync::RwLock<Vec<Session>>>,
}

impl InMemorySessionStore {
    pub fn new() -> Self {
        Self {
            sessions: std::sync::Arc::new(tokio::sync::RwLock::new(Vec::new())),
        }
    }
}

impl Default for InMemorySessionStore {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait::async_trait]
impl SessionStore for InMemorySessionStore {
    async fn create_session(&self, session: Session) -> Result<Session, String> {
        let mut sessions = self.sessions.write().await;
        sessions.push(session.clone());
        Ok(session)
    }

    async fn get_session(&self, session_id: Uuid) -> Result<Option<Session>, String> {
        let sessions = self.sessions.read().await;
        Ok(sessions.iter().find(|s| s.id == session_id).cloned())
    }

    async fn get_session_by_token(&self, token: &str) -> Result<Option<Session>, String> {
        let sessions = self.sessions.read().await;
        Ok(sessions
            .iter()
            .find(|s| s.access_token == token)
            .cloned())
    }

    async fn update_session(&self, session: Session) -> Result<(), String> {
        let mut sessions = self.sessions.write().await;
        if let Some(pos) = sessions.iter().position(|s| s.id == session.id) {
            sessions[pos] = session;
            Ok(())
        } else {
            Err("Session not found".to_string())
        }
    }

    async fn revoke_session(&self, session_id: Uuid) -> Result<(), String> {
        let mut sessions = self.sessions.write().await;
        if let Some(pos) = sessions.iter().position(|s| s.id == session_id) {
            sessions[pos].is_revoked = true;
            Ok(())
        } else {
            Err("Session not found".to_string())
        }
    }

    async fn revoke_user_sessions(&self, user_id: Uuid) -> Result<(), String> {
        let mut sessions = self.sessions.write().await;
        for session in sessions.iter_mut() {
            if session.user_id == user_id {
                session.is_revoked = true;
            }
        }
        Ok(())
    }

    async fn cleanup_expired_sessions(&self) -> Result<usize, String> {
        let mut sessions = self.sessions.write().await;
        let now = Utc::now();
        let initial_count = sessions.len();
        sessions.retain(|s| s.expires_at > now);
        Ok(initial_count - sessions.len())
    }

    async fn list_user_sessions(&self, user_id: Uuid) -> Result<Vec<Session>, String> {
        let sessions = self.sessions.read().await;
        Ok(sessions
            .iter()
            .filter(|s| s.user_id == user_id && !s.is_revoked)
            .cloned()
            .collect())
    }
}

/// Session Manager - orchestrates session lifecycle
pub struct SessionManager {
    store: std::sync::Arc<dyn SessionStore>,
    token_lifetime: Duration,
}

impl SessionManager {
    pub fn new(store: std::sync::Arc<dyn SessionStore>, token_lifetime: Duration) -> Self {
        Self { store, token_lifetime }
    }

    /// Create a new session for a user
    pub async fn create_user_session(
        &self,
        user_id: Uuid,
        ip_address: Option<String>,
        user_agent: Option<String>,
    ) -> Result<Session, String> {
        let session_id = Uuid::new_v4();
        let access_token = self.generate_token();
        let refresh_token = self.generate_token();
        let expires_at = Utc::now() + self.token_lifetime;

        let session = Session {
            id: session_id,
            user_id,
            access_token,
            refresh_token,
            expires_at,
            created_at: Utc::now(),
            ip_address,
            user_agent,
            is_revoked: false,
        };

        self.store.create_session(session).await
    }

    /// Validate session token
    pub async fn validate_token(&self, token: &str) -> Result<Session, String> {
        let session = self
            .store
            .get_session_by_token(token)
            .await?
            .ok_or("Session not found".to_string())?;

        if session.is_revoked {
            return Err("Session revoked".to_string());
        }

        if session.expires_at < Utc::now() {
            return Err("Session expired".to_string());
        }

        Ok(session)
    }

    /// Refresh access token
    pub async fn refresh_access_token(&self, session_id: Uuid) -> Result<String, String> {
        let mut session = self
            .store
            .get_session(session_id)
            .await?
            .ok_or("Session not found".to_string())?;

        if session.is_revoked {
            return Err("Session revoked".to_string());
        }

        if session.expires_at < Utc::now() {
            return Err("Session expired".to_string());
        }

        // Generate new access token
        let new_token = self.generate_token();
        session.access_token = new_token.clone();
        session.expires_at = Utc::now() + self.token_lifetime;

        self.store.update_session(session).await?;

        Ok(new_token)
    }

    /// Revoke a session
    pub async fn revoke_session(&self, session_id: Uuid) -> Result<(), String> {
        self.store.revoke_session(session_id).await
    }

    /// Revoke all user sessions (logout all devices)
    pub async fn revoke_all_user_sessions(&self, user_id: Uuid) -> Result<(), String> {
        self.store.revoke_user_sessions(user_id).await
    }

    /// List user's active sessions
    pub async fn list_user_sessions(&self, user_id: Uuid) -> Result<Vec<Session>, String> {
        self.store.list_user_sessions(user_id).await
    }

    /// Generate a secure random token
    fn generate_token(&self) -> String {
        use base64::Engine;
        let mut rng = rand::thread_rng();
        let bytes: Vec<u8> = (0..32).map(|_| rand::random()).collect();
        base64::engine::general_purpose::STANDARD_NO_PAD.encode(bytes)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_create_session() {
        let store = std::sync::Arc::new(InMemorySessionStore::new());
        let manager = SessionManager::new(store, Duration::hours(1));

        let user_id = Uuid::new_v4();
        let session = manager
            .create_user_session(user_id, None, None)
            .await
            .unwrap();

        assert_eq!(session.user_id, user_id);
        assert!(!session.is_revoked);
    }

    #[tokio::test]
    async fn test_validate_token() {
        let store = std::sync::Arc::new(InMemorySessionStore::new());
        let manager = SessionManager::new(store, Duration::hours(1));

        let user_id = Uuid::new_v4();
        let session = manager
            .create_user_session(user_id, None, None)
            .await
            .unwrap();

        let validated = manager
            .validate_token(&session.access_token)
            .await
            .unwrap();
        assert_eq!(validated.user_id, user_id);
    }

    #[tokio::test]
    async fn test_revoke_session() {
        let store = std::sync::Arc::new(InMemorySessionStore::new());
        let manager = SessionManager::new(store, Duration::hours(1));

        let user_id = Uuid::new_v4();
        let session = manager
            .create_user_session(user_id, None, None)
            .await
            .unwrap();

        manager.revoke_session(session.id).await.unwrap();

        let result = manager.validate_token(&session.access_token).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_revoke_all_user_sessions() {
        let store = std::sync::Arc::new(InMemorySessionStore::new());
        let manager = SessionManager::new(store, Duration::hours(1));

        let user_id = Uuid::new_v4();
        let session1 = manager
            .create_user_session(user_id, None, None)
            .await
            .unwrap();
        let session2 = manager
            .create_user_session(user_id, None, None)
            .await
            .unwrap();

        manager.revoke_all_user_sessions(user_id).await.unwrap();

        assert!(manager.validate_token(&session1.access_token).await.is_err());
        assert!(manager.validate_token(&session2.access_token).await.is_err());
    }
}
