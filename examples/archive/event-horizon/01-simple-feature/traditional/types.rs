// Traditional Approach: Hand-written types
// File: types.rs (52 LOC)

use serde::{Deserialize, Serialize};
use std::fmt;

/// Represents a user in the system
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct User {
    pub id: String,
    pub email: String,
    pub password_hash: String,
    pub created_at: i64,
    pub updated_at: i64,
}

impl User {
    /// Create a new user (password should already be hashed)
    pub fn new(id: String, email: String, password_hash: String) -> Self {
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs() as i64;

        Self {
            id,
            email,
            password_hash,
            created_at: now,
            updated_at: now,
        }
    }

    /// Validate email format (basic check)
    pub fn validate_email(email: &str) -> bool {
        email.contains('@') && email.contains('.')
    }
}

/// Represents an active session
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Session {
    pub token: String,
    pub user_id: String,
    pub created_at: i64,
    pub expires_at: i64,
}

impl Session {
    /// Create a new session with 24-hour expiration
    pub fn new(user_id: String) -> Self {
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs() as i64;

        let token = format!("{}_{}", user_id, now); // Simplified token generation

        Self {
            token,
            user_id,
            created_at: now,
            expires_at: now + 86400, // 24 hours
        }
    }

    /// Check if session is still valid
    pub fn is_valid(&self) -> bool {
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs() as i64;

        now < self.expires_at
    }
}

impl fmt::Display for User {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "User(id={}, email={})", self.id, self.email)
    }
}

impl fmt::Display for Session {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Session(user_id={}, valid={})", self.user_id, self.is_valid())
    }
}
