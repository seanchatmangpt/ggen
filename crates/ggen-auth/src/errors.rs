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

    // RBAC errors
    #[error("Permission denied")]
    PermissionDenied,

    #[error("Insufficient permissions")]
    InsufficientPermissions,

    #[error("Role not found: {0}")]
    RoleNotFound(String),

    #[error("Invalid role assignment")]
    InvalidRoleAssignment,

    #[error("Resource not found: {0}")]
    ResourceNotFound(String),

    #[error("Policy evaluation failed: {0}")]
    PolicyEvaluationFailed(String),

    #[error("Authorization failed: {0}")]
    AuthorizationFailed(String),

    #[error("Privilege escalation attempt detected")]
    PrivilegeEscalation,

    #[error("Resource ownership required")]
    OwnershipRequired,

    #[error("Invalid policy configuration: {0}")]
    InvalidPolicyConfiguration(String),

    // Week 5: Authentication hardening errors
    #[error("Account locked until {0}")]
    AccountLocked(String),

    #[error("Rate limit exceeded")]
    RateLimitExceeded,

    #[error("Session not found")]
    SessionNotFound,

    #[error("Session expired")]
    SessionExpired,
}
