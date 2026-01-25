//! Authentication system for ggen: OAuth2, JWT, and API key management

pub mod jwt;
pub mod oauth;
pub mod api_keys;
pub mod errors;
pub mod claims;
pub mod rbac;

// Week 5: Authentication hardening modules
pub mod jwt_rs256;
pub mod password;
pub mod session;
pub mod rate_limit;
pub mod account_lockout;

// Legacy JWT (HS256 - deprecated, use RS256 instead)
pub use jwt::{JwtManager, TokenClaims};

// RS256 JWT (recommended for production)
pub use jwt_rs256::{Rs256JwtManager, Rs256TokenClaims, TokenPair, TokenType};

// OAuth
pub use oauth::{OAuthConfig, OAuthProvider};

// API Keys
pub use api_keys::{ApiKeyManager, ApiKeyHash};

// Errors
pub use errors::AuthError;

// Claims
pub use claims::Claims;

// RBAC
pub use rbac::{
    authorize, AuthorizationContext, AuthorizationDecision, AuthorizationRequest, Effect,
    Permission, Permissions, Policy, PolicyEngine, PolicyRule, Resource, ResourceOwner,
    ResourceType, Role, RoleHierarchy, RoleLevel, UserRole,
};

// Password hashing
pub use password::{PasswordHasher, PasswordRequirements, SecurePassword};

// Session management
pub use session::{SessionData, SessionManager, RedisSessionManager, SessionConfig};

// Rate limiting
pub use rate_limit::{RateLimiter, RedisRateLimiter, RateLimitConfig};

// Account lockout
pub use account_lockout::{LockoutManager, RedisLockoutManager, LockoutConfig, LockoutStatus};

/// Result type for auth operations
pub type AuthResult<T> = Result<T, AuthError>;

/// Result type for RBAC operations
pub type RbacResult<T> = Result<T, AuthError>;
