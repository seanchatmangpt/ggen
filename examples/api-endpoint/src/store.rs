use crate::error::ApiError;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use uuid::Uuid;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct User {
    pub id: Uuid,
    pub name: String,
    pub email: String,
    pub active: bool,
}

pub struct InMemoryUserStore {
    users: Arc<RwLock<HashMap<Uuid, User>>>,
}

impl InMemoryUserStore {
    pub fn new() -> Self {
        Self {
            users: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    pub async fn list(&self) -> Result<Vec<User>, ApiError> {
        let users = self.users.read().await;
        Ok(users.values().cloned().collect())
    }

    pub async fn get(&self, id: Uuid) -> Result<User, ApiError> {
        let users = self.users.read().await;
        users.get(&id).cloned().ok_or(ApiError::NotFound)
    }

    pub async fn create(&self, user: User) -> Result<User, ApiError> {
        let mut users = self.users.write().await;

        // Check for duplicate email
        if users.values().any(|u| u.email == user.email) {
            return Err(ApiError::DuplicateEmail);
        }

        users.insert(user.id, user.clone());
        Ok(user)
    }

    pub async fn delete(&self, id: Uuid) -> Result<(), ApiError> {
        let mut users = self.users.write().await;
        users.remove(&id).ok_or(ApiError::NotFound)?;
        Ok(())
    }

    pub fn validate_name(&self, name: &str) -> Result<(), ApiError> {
        if name.is_empty() || name.len() > 100 {
            return Err(ApiError::InvalidName(
                "Name must be between 1 and 100 characters".to_string(),
            ));
        }
        Ok(())
    }

    pub fn validate_email(&self, email: &str) -> Result<(), ApiError> {
        if !email.contains('@') || email.len() < 5 {
            return Err(ApiError::InvalidEmail(
                "Invalid email format".to_string(),
            ));
        }
        Ok(())
    }
}

impl Default for InMemoryUserStore {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_create_and_get_user() {
        let store = InMemoryUserStore::new();
        let user = User {
            id: Uuid::new_v4(),
            name: "John Doe".to_string(),
            email: "john@example.com".to_string(),
            active: true,
        };

        let created = store.create(user.clone()).await.unwrap();
        assert_eq!(created.id, user.id);

        let retrieved = store.get(user.id).await.unwrap();
        assert_eq!(retrieved.email, "john@example.com");
    }

    #[tokio::test]
    async fn test_list_users() {
        let store = InMemoryUserStore::new();
        let user1 = User {
            id: Uuid::new_v4(),
            name: "User 1".to_string(),
            email: "user1@example.com".to_string(),
            active: true,
        };
        let user2 = User {
            id: Uuid::new_v4(),
            name: "User 2".to_string(),
            email: "user2@example.com".to_string(),
            active: true,
        };

        store.create(user1).await.unwrap();
        store.create(user2).await.unwrap();

        let users = store.list().await.unwrap();
        assert_eq!(users.len(), 2);
    }

    #[tokio::test]
    async fn test_delete_user() {
        let store = InMemoryUserStore::new();
        let user = User {
            id: Uuid::new_v4(),
            name: "John Doe".to_string(),
            email: "john@example.com".to_string(),
            active: true,
        };

        store.create(user.clone()).await.unwrap();
        store.delete(user.id).await.unwrap();

        let result = store.get(user.id).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_duplicate_email() {
        let store = InMemoryUserStore::new();
        let user1 = User {
            id: Uuid::new_v4(),
            name: "User 1".to_string(),
            email: "same@example.com".to_string(),
            active: true,
        };
        let user2 = User {
            id: Uuid::new_v4(),
            name: "User 2".to_string(),
            email: "same@example.com".to_string(),
            active: true,
        };

        store.create(user1).await.unwrap();
        let result = store.create(user2).await;
        assert!(matches!(result, Err(ApiError::DuplicateEmail)));
    }

    #[test]
    fn test_validate_name() {
        let store = InMemoryUserStore::new();
        assert!(store.validate_name("Valid Name").is_ok());
        assert!(store.validate_name("").is_err());
        assert!(store.validate_name(&"x".repeat(101)).is_err());
    }

    #[test]
    fn test_validate_email() {
        let store = InMemoryUserStore::new();
        assert!(store.validate_email("user@example.com").is_ok());
        assert!(store.validate_email("invalid").is_err());
        assert!(store.validate_email("bad@").is_err());
    }
}
