// Traditional Approach: Hand-written error types
// File: errors.rs (50 LOC)

use std::fmt;

/// Authentication-related errors
#[derive(Debug, Clone, PartialEq)]
pub enum AuthError {
    /// Invalid email format
    InvalidEmail(String),
    /// Password too weak
    WeakPassword(String),
    /// User already exists
    UserExists(String),
    /// User not found
    UserNotFound(String),
    /// Invalid credentials
    InvalidCredentials,
    /// Session expired or invalid
    InvalidSession(String),
    /// Password hashing failed
    HashingError(String),
}

impl fmt::Display for AuthError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AuthError::InvalidEmail(email) => {
                write!(f, "Invalid email format: {}", email)
            }
            AuthError::WeakPassword(reason) => {
                write!(f, "Password too weak: {}", reason)
            }
            AuthError::UserExists(email) => {
                write!(f, "User already exists: {}", email)
            }
            AuthError::UserNotFound(email) => {
                write!(f, "User not found: {}", email)
            }
            AuthError::InvalidCredentials => {
                write!(f, "Invalid email or password")
            }
            AuthError::InvalidSession(token) => {
                write!(f, "Invalid or expired session: {}", token)
            }
            AuthError::HashingError(msg) => {
                write!(f, "Password hashing failed: {}", msg)
            }
        }
    }
}

impl std::error::Error for AuthError {}

// Manual mapping for external errors
impl From<bcrypt::BcryptError> for AuthError {
    fn from(err: bcrypt::BcryptError) -> Self {
        AuthError::HashingError(err.to_string())
    }
}
