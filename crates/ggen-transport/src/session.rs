use crate::error::{Result, TransportError};
use chrono::{DateTime, Duration, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use uuid::Uuid;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SessionId(String);

impl SessionId {
    pub fn new() -> Self {
        Self(Uuid::new_v4().to_string())
    }

    pub fn from_string(s: String) -> Self {
        Self(s)
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl Default for SessionId {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResumeCursor {
    pub session_id: SessionId,
    pub position: u64,
    pub timestamp: DateTime<Utc>,
    pub metadata: HashMap<String, String>,
}

impl ResumeCursor {
    pub fn new(session_id: SessionId, position: u64) -> Self {
        Self {
            session_id,
            position,
            timestamp: Utc::now(),
            metadata: HashMap::new(),
        }
    }

    pub fn with_metadata(mut self, key: String, value: String) -> Self {
        self.metadata.insert(key, value);
        self
    }

    pub fn to_string(&self) -> Result<String> {
        serde_json::to_string(self).map_err(TransportError::from)
    }

    pub fn from_string(s: &str) -> Result<Self> {
        serde_json::from_str(s).map_err(TransportError::from)
    }
}

#[derive(Debug, Clone)]
pub struct Session {
    pub id: SessionId,
    pub created_at: DateTime<Utc>,
    pub last_accessed: DateTime<Utc>,
    pub expires_at: DateTime<Utc>,
    pub cursor: Option<ResumeCursor>,
    pub metadata: HashMap<String, String>,
}

impl Session {
    pub fn new(ttl_seconds: i64) -> Self {
        let id = SessionId::new();
        let now = Utc::now();
        Self {
            id,
            created_at: now,
            last_accessed: now,
            expires_at: now + Duration::seconds(ttl_seconds),
            cursor: None,
            metadata: HashMap::new(),
        }
    }

    pub fn is_expired(&self) -> bool {
        Utc::now() > self.expires_at
    }

    pub fn touch(&mut self, ttl_seconds: i64) {
        self.last_accessed = Utc::now();
        self.expires_at = self.last_accessed + Duration::seconds(ttl_seconds);
    }

    pub fn set_cursor(&mut self, cursor: ResumeCursor) {
        self.cursor = Some(cursor);
    }

    pub fn get_cursor(&self) -> Option<&ResumeCursor> {
        self.cursor.as_ref()
    }
}

#[derive(Debug, Clone)]
pub struct SessionManager {
    sessions: Arc<RwLock<HashMap<String, Session>>>,
    default_ttl_seconds: i64,
}

impl SessionManager {
    pub fn new(default_ttl_seconds: i64) -> Self {
        Self {
            sessions: Arc::new(RwLock::new(HashMap::new())),
            default_ttl_seconds,
        }
    }

    pub async fn create_session(&self) -> Session {
        let session = Session::new(self.default_ttl_seconds);
        let mut sessions = self.sessions.write().await;
        let id = session.id.as_str().to_string();
        sessions.insert(id, session.clone());
        session
    }

    pub async fn get_session(&self, id: &SessionId) -> Result<Session> {
        let sessions = self.sessions.read().await;
        let session = sessions
            .get(id.as_str())
            .ok_or_else(|| TransportError::SessionNotFound(id.as_str().to_string()))?;

        if session.is_expired() {
            return Err(TransportError::SessionExpired(id.as_str().to_string()));
        }

        Ok(session.clone())
    }

    pub async fn update_session(&self, id: &SessionId, session: Session) -> Result<()> {
        let mut sessions = self.sessions.write().await;
        if !sessions.contains_key(id.as_str()) {
            return Err(TransportError::SessionNotFound(id.as_str().to_string()));
        }
        sessions.insert(id.as_str().to_string(), session);
        Ok(())
    }

    pub async fn touch_session(&self, id: &SessionId) -> Result<()> {
        let mut sessions = self.sessions.write().await;
        let session = sessions
            .get_mut(id.as_str())
            .ok_or_else(|| TransportError::SessionNotFound(id.as_str().to_string()))?;

        if session.is_expired() {
            return Err(TransportError::SessionExpired(id.as_str().to_string()));
        }

        session.touch(self.default_ttl_seconds);
        Ok(())
    }

    pub async fn delete_session(&self, id: &SessionId) -> Result<()> {
        let mut sessions = self.sessions.write().await;
        sessions
            .remove(id.as_str())
            .ok_or_else(|| TransportError::SessionNotFound(id.as_str().to_string()))?;
        Ok(())
    }

    pub async fn cleanup_expired(&self) -> usize {
        let mut sessions = self.sessions.write().await;
        let expired: Vec<String> = sessions
            .iter()
            .filter(|(_, s)| s.is_expired())
            .map(|(k, _)| k.clone())
            .collect();

        let count = expired.len();
        for key in expired {
            sessions.remove(&key);
        }
        count
    }

    pub async fn set_cursor(&self, id: &SessionId, cursor: ResumeCursor) -> Result<()> {
        let mut sessions = self.sessions.write().await;
        let session = sessions
            .get_mut(id.as_str())
            .ok_or_else(|| TransportError::SessionNotFound(id.as_str().to_string()))?;

        session.set_cursor(cursor);
        Ok(())
    }

    pub async fn get_cursor(&self, id: &SessionId) -> Result<Option<ResumeCursor>> {
        let sessions = self.sessions.read().await;
        let session = sessions
            .get(id.as_str())
            .ok_or_else(|| TransportError::SessionNotFound(id.as_str().to_string()))?;

        Ok(session.get_cursor().cloned())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_session_creation() {
        let manager = SessionManager::new(3600);
        let session = manager.create_session().await;
        assert!(!session.is_expired());
    }

    #[tokio::test]
    async fn test_session_retrieval() {
        let manager = SessionManager::new(3600);
        let session = manager.create_session().await;
        let retrieved = manager.get_session(&session.id).await.unwrap();
        assert_eq!(retrieved.id.as_str(), session.id.as_str());
    }

    #[tokio::test]
    async fn test_cursor_management() {
        let manager = SessionManager::new(3600);
        let session = manager.create_session().await;
        let cursor = ResumeCursor::new(session.id.clone(), 42);

        manager.set_cursor(&session.id, cursor.clone()).await.unwrap();
        let retrieved = manager.get_cursor(&session.id).await.unwrap();

        assert!(retrieved.is_some());
        assert_eq!(retrieved.unwrap().position, 42);
    }

    #[tokio::test]
    async fn test_session_touch() {
        let manager = SessionManager::new(3600);
        let session = manager.create_session().await;
        tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
        manager.touch_session(&session.id).await.unwrap();

        let updated = manager.get_session(&session.id).await.unwrap();
        assert!(updated.last_accessed > session.last_accessed);
    }
}
