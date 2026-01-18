//! Authentication error types

use thiserror::Error;

#[derive(Error, Debug)]
pub enum AuthError {
    #[error("Invalid credentials")]
    InvalidCredentials,

    #[error("Token expired")]
    TokenExpired,

    #[error("Invalid token")]
    InvalidToken,

    #[error("Token generation failed: {0}")]
    TokenGenerationFailed(String),

    #[error("Key not found")]
    KeyNotFound,

    #[error("Invalid API key")]
    InvalidApiKey,

    #[error("OAuth error: {0}")]
    OAuthError(String),

    #[error("User not found")]
    UserNotFound,

    #[error("User already exists")]
    UserAlreadyExists,

    #[error("Password too weak")]
    WeakPassword,

    #[error("Configuration error: {0}")]
    ConfigError(String),

    #[error("Database error: {0}")]
    DatabaseError(String),

    #[error("Cryptographic error: {0}")]
    CryptoError(String),

    #[error("Internal server error")]
    InternalError,
}
