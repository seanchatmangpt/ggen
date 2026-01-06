//! Authentication middleware

use axum::{
    extract::{Request, State},
    middleware::Next,
    response::Response,
};
use std::sync::Arc;

use crate::state::AppState;

/// User information extracted from JWT token
#[derive(Debug, Clone)]
pub struct User {
    pub id: String,
    pub email: String,
    pub tier: String,
}

/// Middleware to verify JWT tokens
pub async fn verify_jwt(
    State(_state): State<AppState>,
    mut request: Request,
    next: Next,
) -> Response {
    // TODO: Extract Authorization header
    // TODO: Verify JWT signature
    // TODO: Check expiration
    // TODO: Extract claims
    // TODO: Add User to request extensions

    next.run(request).await
}

/// Middleware to require authentication
pub async fn require_auth(
    State(_state): State<AppState>,
    request: Request,
    next: Next,
) -> Response {
    // TODO: Check if User is in request extensions
    // TODO: Return 401 if not authenticated

    next.run(request).await
}

/// Middleware to require specific tier
pub async fn require_tier(min_tier: &str) -> impl Fn(Request, Next) -> futures::future::BoxFuture<'static, Response> + Clone {
    move |request: Request, next: Next| {
        let tier = min_tier.to_string();
        Box::pin(async move {
            // TODO: Get User from request extensions
            // TODO: Check tier hierarchy: free < pro < enterprise
            // TODO: Return 403 if tier insufficient

            next.run(request).await
        })
    }
}
