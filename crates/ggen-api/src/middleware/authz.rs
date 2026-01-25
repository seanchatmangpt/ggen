//! Authorization middleware for RBAC enforcement

use axum::{
    body::Body,
    extract::{Request, State},
    http::StatusCode,
    middleware::Next,
    response::{IntoResponse, Response},
    Json,
};
use ggen_auth::{
    authorize, AuthError, AuthorizationContext, Permission, PolicyEngine, Resource, ResourceOwner,
    ResourceType, Role, RoleHierarchy,
};
use serde::Serialize;
use std::sync::Arc;

use crate::error::ApiError;
use crate::state::AppState;

/// User information from authentication
#[derive(Debug, Clone)]
pub struct AuthenticatedUser {
    pub id: String,
    pub email: String,
    pub role_ids: Vec<String>,
}

/// Error response for authorization failures
#[derive(Debug, Serialize)]
pub struct AuthzErrorResponse {
    pub error: String,
    pub message: String,
    pub status: u16,
}

/// Extract authenticated user from request extensions
fn get_authenticated_user(request: &Request) -> Result<AuthenticatedUser, ApiError> {
    request
        .extensions()
        .get::<AuthenticatedUser>()
        .cloned()
        .ok_or_else(|| ApiError::Unauthorized("User not authenticated".to_string()))
}

/// Authorization middleware - checks if user has required permission
pub async fn require_permission(
    permission: Permission,
    resource_type: ResourceType,
) -> impl Fn(State<AppState>, Request, Next) -> std::pin::Pin<
    Box<dyn std::future::Future<Output = Result<Response, Response>> + Send>,
> + Clone {
    move |state: State<AppState>, request: Request, next: Next| {
        let perm = permission;
        let res_type = resource_type.clone();

        Box::pin(async move {
            // Get authenticated user
            let user = match get_authenticated_user(&request) {
                Ok(u) => u,
                Err(e) => return Err(authorization_error(StatusCode::UNAUTHORIZED, &e.to_string())),
            };

            // Get user roles from role hierarchy
            let role_hierarchy = RoleHierarchy::new();
            let roles: Vec<Role> = user
                .role_ids
                .iter()
                .filter_map(|id| role_hierarchy.get_role(id).cloned())
                .collect();

            if roles.is_empty() {
                return Err(authorization_error(
                    StatusCode::FORBIDDEN,
                    "User has no valid roles",
                ));
            }

            // Create dummy resource for permission check (actual resource would come from path)
            let resource = Resource {
                id: "temp".to_string(),
                resource_type: res_type,
                owner: ResourceOwner::new(&user.id),
                permissions: ggen_auth::Permissions::empty(),
            };

            // Check authorization
            let policy_engine = PolicyEngine::new();
            match authorize(&user.id, &roles, &resource, perm, &policy_engine) {
                Ok(true) => Ok(next.run(request).await),
                Ok(false) => Err(authorization_error(
                    StatusCode::FORBIDDEN,
                    "Insufficient permissions",
                )),
                Err(e) => Err(authorization_error(
                    StatusCode::INTERNAL_SERVER_ERROR,
                    &format!("Authorization check failed: {}", e),
                )),
            }
        })
    }
}

/// Authorization middleware - checks if user has specific role
pub async fn require_role(
    required_role: String,
) -> impl Fn(State<AppState>, Request, Next) -> std::pin::Pin<
    Box<dyn std::future::Future<Output = Result<Response, Response>> + Send>,
> + Clone {
    move |_state: State<AppState>, request: Request, next: Next| {
        let role = required_role.clone();

        Box::pin(async move {
            // Get authenticated user
            let user = match get_authenticated_user(&request) {
                Ok(u) => u,
                Err(e) => return Err(authorization_error(StatusCode::UNAUTHORIZED, &e.to_string())),
            };

            // Check if user has required role
            if !user.role_ids.contains(&role) {
                return Err(authorization_error(
                    StatusCode::FORBIDDEN,
                    &format!("Role required: {}", role),
                ));
            }

            Ok(next.run(request).await)
        })
    }
}

/// Authorization middleware - checks resource ownership
pub async fn require_ownership(
    resource_extractor: impl Fn(&Request) -> Option<(ResourceType, String)> + Clone + Send + 'static,
) -> impl Fn(State<AppState>, Request, Next) -> std::pin::Pin<
    Box<dyn std::future::Future<Output = Result<Response, Response>> + Send>,
> + Clone {
    move |_state: State<AppState>, request: Request, next: Next| {
        let extractor = resource_extractor.clone();

        Box::pin(async move {
            // Get authenticated user
            let user = match get_authenticated_user(&request) {
                Ok(u) => u,
                Err(e) => return Err(authorization_error(StatusCode::UNAUTHORIZED, &e.to_string())),
            };

            // Extract resource information from request
            let (resource_type, resource_id) = match extractor(&request) {
                Some(r) => r,
                None => {
                    return Err(authorization_error(
                        StatusCode::BAD_REQUEST,
                        "Could not extract resource information",
                    ))
                }
            };

            // TODO: Load actual resource from database
            // For now, create a dummy resource
            let resource = Resource {
                id: resource_id.clone(),
                resource_type,
                owner: ResourceOwner::new(&user.id),
                permissions: ggen_auth::Permissions::empty(),
            };

            // Check ownership
            if !resource.is_owner(&user.id) {
                return Err(authorization_error(
                    StatusCode::FORBIDDEN,
                    "Resource ownership required",
                ));
            }

            Ok(next.run(request).await)
        })
    }
}

/// Create authorization error response
fn authorization_error(status: StatusCode, message: &str) -> Response {
    let error_response = AuthzErrorResponse {
        error: "Authorization Error".to_string(),
        message: message.to_string(),
        status: status.as_u16(),
    };

    (status, Json(error_response)).into_response()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_authz_error_response_serialization() {
        // Arrange
        let error = AuthzErrorResponse {
            error: "Authorization Error".to_string(),
            message: "Insufficient permissions".to_string(),
            status: 403,
        };

        // Act
        let json = serde_json::to_string(&error);

        // Assert
        assert!(json.is_ok());
        let json_str = json.unwrap();
        assert!(json_str.contains("Authorization Error"));
        assert!(json_str.contains("Insufficient permissions"));
    }

    #[test]
    fn test_authenticated_user_clone() {
        // Arrange
        let user = AuthenticatedUser {
            id: "user123".to_string(),
            email: "user@example.com".to_string(),
            role_ids: vec!["role_user".to_string()],
        };

        // Act
        let cloned = user.clone();

        // Assert
        assert_eq!(cloned.id, user.id);
        assert_eq!(cloned.email, user.email);
        assert_eq!(cloned.role_ids, user.role_ids);
    }
}
