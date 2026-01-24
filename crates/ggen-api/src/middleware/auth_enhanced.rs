//! Enhanced authentication middleware with RS256 JWT and session management

use axum::{
    body::Body,
    extract::{Request, State},
    http::{header, StatusCode},
    middleware::Next,
    response::{IntoResponse, Response},
};
use ggen_auth::{Rs256JwtManager, SessionManager};
use std::sync::Arc;

use crate::error::ApiError;

/// User context extracted from authentication
#[derive(Debug, Clone)]
pub struct AuthenticatedUser {
    pub user_id: String,
    pub email: String,
    pub tier: String,
    pub scopes: Vec<String>,
    pub session_id: Option<String>,
}

/// Authentication state shared across middleware
#[derive(Clone)]
pub struct AuthState {
    pub jwt_manager: Arc<Rs256JwtManager>,
    pub session_manager: Option<Arc<dyn SessionManager>>,
}

impl AuthState {
    /// Create new authentication state
    pub fn new(
        jwt_manager: Rs256JwtManager,
        session_manager: Option<Arc<dyn SessionManager>>,
    ) -> Self {
        Self {
            jwt_manager: Arc::new(jwt_manager),
            session_manager,
        }
    }
}

/// Extract token from Authorization header
fn extract_bearer_token(request: &Request) -> Result<String, ApiError> {
    let auth_header = request
        .headers()
        .get(header::AUTHORIZATION)
        .and_then(|h| h.to_str().ok())
        .ok_or(ApiError::Unauthorized("Missing Authorization header".to_string()))?;

    if !auth_header.starts_with("Bearer ") {
        return Err(ApiError::Unauthorized("Invalid Authorization header format".to_string()));
    }

    Ok(auth_header[7..].to_string())
}

/// Middleware to verify JWT tokens with RS256
pub async fn verify_jwt_rs256(
    State(auth_state): State<AuthState>,
    mut request: Request,
    next: Next,
) -> Result<Response, ApiError> {
    // Extract token
    let token = extract_bearer_token(&request)?;

    // Verify token signature and expiration
    let claims = auth_state
        .jwt_manager
        .verify_token(&token)
        .map_err(|e| match e {
            ggen_auth::AuthError::TokenExpired => {
                ApiError::Unauthorized("Token expired".to_string())
            }
            ggen_auth::AuthError::InvalidToken => {
                ApiError::Unauthorized("Invalid token".to_string())
            }
            _ => ApiError::Unauthorized("Token verification failed".to_string()),
        })?;

    // Verify this is an access token
    if claims.token_type != "access" {
        return Err(ApiError::Unauthorized("Invalid token type".to_string()));
    }

    // Create authenticated user context
    let user = AuthenticatedUser {
        user_id: claims.sub.clone(),
        email: claims.email.clone(),
        tier: claims.tier.clone(),
        scopes: claims.scopes.clone(),
        session_id: None,
    };

    // Add user to request extensions
    request.extensions_mut().insert(user);

    Ok(next.run(request).await)
}

/// Middleware to verify session (if session manager is configured)
pub async fn verify_session(
    State(auth_state): State<AuthState>,
    mut request: Request,
    next: Next,
) -> Result<Response, ApiError> {
    // Extract session ID from cookie or header
    let session_id = request
        .headers()
        .get("X-Session-ID")
        .and_then(|h| h.to_str().ok())
        .ok_or(ApiError::Unauthorized("Missing session ID".to_string()))?;

    // Verify session exists (if session manager is configured)
    if let Some(ref session_manager) = auth_state.session_manager {
        let session_data = session_manager
            .get_session(session_id)
            .await
            .map_err(|e| match e {
                ggen_auth::AuthError::InvalidToken => {
                    ApiError::Unauthorized("Invalid session".to_string())
                }
                _ => ApiError::InternalServerError("Session verification failed".to_string()),
            })?;

        // Update last accessed time
        let _ = session_manager.touch_session(session_id).await;

        // Create authenticated user from session
        let user = AuthenticatedUser {
            user_id: session_data.user_id,
            email: session_data.email,
            tier: session_data.tier,
            scopes: session_data.scopes,
            session_id: Some(session_id.to_string()),
        };

        // Add user to request extensions
        request.extensions_mut().insert(user);
    }

    Ok(next.run(request).await)
}

/// Middleware to require authentication (checks for user in extensions)
pub async fn require_auth(
    request: Request,
    next: Next,
) -> Result<Response, ApiError> {
    // Check if user is in request extensions
    if request.extensions().get::<AuthenticatedUser>().is_none() {
        return Err(ApiError::Unauthorized("Authentication required".to_string()));
    }

    Ok(next.run(request).await)
}

/// Middleware to require specific scope
pub fn require_scope(required_scope: &'static str) -> impl Fn(Request, Next) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<Response, ApiError>> + Send>> + Clone {
    move |request: Request, next: Next| {
        Box::pin(async move {
            let user = request
                .extensions()
                .get::<AuthenticatedUser>()
                .ok_or(ApiError::Unauthorized("Authentication required".to_string()))?;

            if !user.scopes.contains(&required_scope.to_string()) {
                return Err(ApiError::Forbidden("Insufficient permissions".to_string()));
            }

            Ok(next.run(request).await)
        })
    }
}

/// Middleware to require specific tier or higher
pub fn require_tier(required_tier: &'static str) -> impl Fn(Request, Next) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<Response, ApiError>> + Send>> + Clone {
    move |request: Request, next: Next| {
        Box::pin(async move {
            let user = request
                .extensions()
                .get::<AuthenticatedUser>()
                .ok_or(ApiError::Unauthorized("Authentication required".to_string()))?;

            let tier_level = |tier: &str| -> u32 {
                match tier {
                    "free" => 1,
                    "pro" => 2,
                    "enterprise" => 3,
                    _ => 0,
                }
            };

            let required_level = tier_level(required_tier);
            let user_level = tier_level(&user.tier);

            if user_level < required_level {
                return Err(ApiError::Forbidden(
                    format!("Tier {} or higher required", required_tier),
                ));
            }

            Ok(next.run(request).await)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use axum::body::Body;
    use axum::http::Request;
    use ggen_auth::Rs256JwtManager;

    fn create_test_auth_state() -> AuthState {
        let (private_pem, public_pem) = Rs256JwtManager::generate_key_pair().unwrap();
        let jwt_manager = Rs256JwtManager::new(&private_pem, &public_pem, 900, 604800).unwrap();
        AuthState::new(jwt_manager, None)
    }

    #[test]
    fn test_extract_bearer_token() {
        // Arrange
        let request = Request::builder()
            .header("Authorization", "Bearer test_token_123")
            .body(Body::empty())
            .unwrap();

        // Act
        let result = extract_bearer_token(&request);

        // Assert
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), "test_token_123");
    }

    #[test]
    fn test_extract_bearer_token_missing_header() {
        // Arrange
        let request = Request::builder()
            .body(Body::empty())
            .unwrap();

        // Act
        let result = extract_bearer_token(&request);

        // Assert
        assert!(result.is_err());
    }

    #[test]
    fn test_extract_bearer_token_invalid_format() {
        // Arrange
        let request = Request::builder()
            .header("Authorization", "Basic dGVzdDp0ZXN0")
            .body(Body::empty())
            .unwrap();

        // Act
        let result = extract_bearer_token(&request);

        // Assert
        assert!(result.is_err());
    }
}
