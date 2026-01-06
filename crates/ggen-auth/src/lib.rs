//! Authentication system for ggen: OAuth2, JWT, and API key management

pub mod jwt;
pub mod oauth;
pub mod api_keys;
pub mod errors;
pub mod claims;

pub use jwt::{JwtManager, TokenClaims};
pub use oauth::{OAuthConfig, OAuthProvider};
pub use api_keys::{ApiKeyManager, ApiKeyHash};
pub use errors::AuthError;
pub use claims::Claims;

/// Result type for auth operations
pub type AuthResult<T> = Result<T, AuthError>;
