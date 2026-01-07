use serde::{Deserialize, Serialize};
use uuid::Uuid;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct User {
    pub id: Uuid,
    pub name: String,
    pub email: String,
}

#[derive(Debug, Clone)]
pub struct UserStore {
    users: Vec<User>,
}

impl Default for UserStore {
    fn default() -> Self {
        Self::new()
    }
}

impl UserStore {
    pub fn new() -> Self {
        Self {
            users: Vec::new(),
        }
    }

    pub fn add_user(&mut self, user: User) -> Uuid {
        self.users.push(user.clone());
        user.id
    }

    pub fn users(&self) -> &[User] {
        &self.users
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_user_store_creation() {
        let store = UserStore::new();
        assert_eq!(store.users().len(), 0);
    }

    #[test]
    fn test_add_user() {
        let mut store = UserStore::new();
        let user = User {
            id: Uuid::new_v4(),
            name: "Alice".to_string(),
            email: "alice@example.com".to_string(),
        };
        store.add_user(user);
        assert_eq!(store.users().len(), 1);
    }
}
