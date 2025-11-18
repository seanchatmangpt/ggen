/// HTTP Handlers for IDP API endpoints
/// Built with Actix-web

use crate::auth::AuthService;
use crate::models::*;
use crate::oauth2::*;
use crate::rbac::RbacEngine;
use crate::sessions::SessionManager;
use crate::audit::AuditLogger;
use crate::errors::{IdpError, IdpResult};
use actix_web::{web, HttpRequest, HttpResponse};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

// Request/Response DTOs
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RegisterRequest {
    pub username: String,
    pub email: String,
    pub password: String,
    pub display_name: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct LoginRequest {
    pub username: String,
    pub password: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct LoginResponse {
    pub access_token: String,
    pub refresh_token: String,
    pub expires_in: i64,
    pub token_type: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RefreshTokenRequest {
    pub refresh_token: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ChangePasswordRequest {
    pub old_password: String,
    pub new_password: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RequestPasswordResetRequest {
    pub email: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ResetPasswordRequest {
    pub token: String,
    pub new_password: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CreateRoleRequest {
    pub name: String,
    pub description: Option<String>,
    pub permissions: Vec<Permission>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AssignRoleRequest {
    pub role_id: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CheckPermissionRequest {
    pub resource: String,
    pub action: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CheckPermissionResponse {
    pub allowed: bool,
    pub reason: Option<String>,
}

// Middleware for extracting user from token
pub struct AuthenticatedUser {
    pub user_id: Uuid,
    pub org_id: Uuid,
    pub claims: Claims,
}

/// Authentication Handlers
pub mod auth_handlers {
    use super::*;

    /// Register a new user
    pub async fn register(
        req: web::Json<RegisterRequest>,
        auth_service: web::Data<AuthService>,
        org_id: web::Path<String>,
    ) -> IdpResult<HttpResponse> {
        let org_uuid =
            Uuid::parse_str(&org_id).map_err(|_| IdpError::BadRequest("Invalid org_id".to_string()))?;

        let _user = auth_service
            .register(&req.username, &req.email, &req.password, org_uuid)
            .await?;

        Ok(HttpResponse::Created().json(serde_json::json!({
            "message": "User registered successfully. Please verify your email.",
            "email": req.email
        })))
    }

    /// Login with credentials
    pub async fn login(
        req: web::Json<LoginRequest>,
        auth_service: web::Data<AuthService>,
        session_manager: web::Data<SessionManager>,
        org_id: web::Path<String>,
        http_req: HttpRequest,
    ) -> IdpResult<HttpResponse> {
        let org_uuid = Uuid::parse_str(&org_id)
            .map_err(|_| IdpError::BadRequest("Invalid org_id".to_string()))?;

        // Authenticate user
        let user = auth_service
            .authenticate(&req.username, &req.password, org_uuid)
            .await?;

        // Create session
        let ip = http_req
            .connection_info()
            .peer_addr()
            .map(|s| s.to_string());
        let user_agent = http_req
            .headers()
            .get("user-agent")
            .and_then(|h| h.to_str().ok())
            .map(|s| s.to_string());

        let session = session_manager
            .create_user_session(user.id, ip, user_agent)
            .await?;

        Ok(HttpResponse::Ok().json(LoginResponse {
            access_token: session.access_token,
            refresh_token: session.refresh_token,
            expires_in: 3600,
            token_type: "Bearer".to_string(),
        }))
    }

    /// Refresh access token
    pub async fn refresh_token(
        req: web::Json<RefreshTokenRequest>,
        session_manager: web::Data<SessionManager>,
    ) -> IdpResult<HttpResponse> {
        let session = session_manager
            .validate_token(&req.refresh_token)
            .await?;

        let new_token = session_manager
            .refresh_access_token(session.id)
            .await?;

        Ok(HttpResponse::Ok().json(LoginResponse {
            access_token: new_token,
            refresh_token: req.refresh_token.clone(),
            expires_in: 3600,
            token_type: "Bearer".to_string(),
        }))
    }

    /// Logout (revoke session)
    pub async fn logout(
        auth_user: web::Path<Uuid>,
        session_manager: web::Data<SessionManager>,
    ) -> IdpResult<HttpResponse> {
        session_manager
            .revoke_session(auth_user.into_inner())
            .await?;

        Ok(HttpResponse::Ok().json(serde_json::json!({
            "message": "Logged out successfully"
        })))
    }

    /// Change password
    pub async fn change_password(
        req: web::Json<ChangePasswordRequest>,
        user_id: web::Path<Uuid>,
        auth_service: web::Data<AuthService>,
    ) -> IdpResult<HttpResponse> {
        auth_service
            .change_password(user_id.into_inner(), &req.old_password, &req.new_password)
            .await?;

        Ok(HttpResponse::Ok().json(serde_json::json!({
            "message": "Password changed successfully"
        })))
    }
}

/// RBAC Handlers
pub mod rbac_handlers {
    use super::*;

    /// Check if user has permission
    pub async fn check_permission(
        user_id: web::Path<Uuid>,
        req: web::Json<CheckPermissionRequest>,
        rbac_engine: web::Data<RbacEngine>,
    ) -> IdpResult<HttpResponse> {
        // Get user's org_id (in real implementation)
        let org_id = Uuid::new_v4();

        let allowed = rbac_engine
            .has_permission(
                user_id.into_inner(),
                org_id,
                &req.resource,
                &req.action,
            )
            .await?;

        Ok(HttpResponse::Ok().json(CheckPermissionResponse {
            allowed,
            reason: if allowed {
                None
            } else {
                Some("Permission denied".to_string())
            },
        }))
    }

    /// Assign role to user
    pub async fn assign_role(
        params: web::Path<(Uuid, Uuid)>,
        req: web::Json<AssignRoleRequest>,
        rbac_engine: web::Data<RbacEngine>,
    ) -> IdpResult<HttpResponse> {
        let (user_id, org_id) = params.into_inner();
        let role_id =
            Uuid::parse_str(&req.role_id).map_err(|_| IdpError::BadRequest("Invalid role_id".to_string()))?;

        rbac_engine
            .grant_role(user_id, role_id, org_id)
            .await?;

        Ok(HttpResponse::Ok().json(serde_json::json!({
            "message": "Role assigned successfully"
        })))
    }

    /// Revoke role from user
    pub async fn revoke_role(
        params: web::Path<(Uuid, Uuid)>,
        _req: web::Json<AssignRoleRequest>,
        rbac_engine: web::Data<RbacEngine>,
    ) -> IdpResult<HttpResponse> {
        let (user_id, role_id) = params.into_inner();

        rbac_engine
            .revoke_role(user_id, role_id)
            .await?;

        Ok(HttpResponse::Ok().json(serde_json::json!({
            "message": "Role revoked successfully"
        })))
    }

    /// List user's roles
    pub async fn list_user_roles(
        params: web::Path<(Uuid, Uuid)>,
        rbac_engine: web::Data<RbacEngine>,
    ) -> IdpResult<HttpResponse> {
        let (user_id, org_id) = params.into_inner();

        let roles = rbac_engine
            .store
            .get_user_roles(user_id, org_id)
            .await?;

        Ok(HttpResponse::Ok().json(serde_json::json!({
            "roles": roles
        })))
    }
}

/// OAuth2 Handlers
pub mod oauth2_handlers {
    use super::*;

    /// Authorization endpoint
    pub async fn authorize(
        query: web::Query<std::collections::HashMap<String, String>>,
        oauth2_provider: web::Data<OAuth2Provider>,
    ) -> IdpResult<HttpResponse> {
        let client_id = query
            .get("client_id")
            .ok_or(IdpError::BadRequest("Missing client_id".to_string()))?;
        let redirect_uri = query
            .get("redirect_uri")
            .ok_or(IdpError::BadRequest("Missing redirect_uri".to_string()))?;
        let state = query
            .get("state")
            .ok_or(IdpError::BadRequest("Missing state".to_string()))?;
        let scope = query
            .get("scope")
            .map(|s| s.clone())
            .unwrap_or_else(|| "openid".to_string());

        let auth_request = AuthorizationRequest {
            response_type: "code".to_string(),
            client_id: client_id.clone(),
            redirect_uri: redirect_uri.clone(),
            scope,
            state: state.clone(),
            nonce: query.get("nonce").cloned(),
            code_challenge: query.get("code_challenge").cloned(),
            code_challenge_method: query.get("code_challenge_method").cloned(),
        };

        // TODO: Create mock client and user for demo
        let client = OAuth2Client {
            id: client_id.clone(),
            secret: "secret".to_string(),
            name: "Test Client".to_string(),
            organization_id: Uuid::new_v4(),
            redirect_uris: vec![redirect_uri.clone()],
            allowed_scopes: vec!["openid".to_string(), "profile".to_string(), "email".to_string()],
            is_confidential: true,
            created_at: chrono::Utc::now(),
        };

        let user_id = Uuid::new_v4();

        let response = oauth2_provider
            .authorize(auth_request, &client, user_id)
            .await?;

        Ok(HttpResponse::Found()
            .insert_header(("location", format!("{}?code={}&state={}", redirect_uri, response.code, response.state)))
            .finish())
    }

    /// Token endpoint
    pub async fn token(
        req: web::Json<TokenRequest>,
        oauth2_provider: web::Data<OAuth2Provider>,
    ) -> IdpResult<HttpResponse> {
        // TODO: Create mock client for demo
        let client = OAuth2Client {
            id: req.client_id.clone(),
            secret: "secret".to_string(),
            name: "Test Client".to_string(),
            organization_id: Uuid::new_v4(),
            redirect_uris: vec!["http://localhost:3000/callback".to_string()],
            allowed_scopes: vec!["openid".to_string(), "profile".to_string(), "email".to_string()],
            is_confidential: true,
            created_at: chrono::Utc::now(),
        };

        let response = oauth2_provider
            .token(req.into_inner(), &client)
            .await?;

        Ok(HttpResponse::Ok().json(response))
    }

    /// OIDC Discovery endpoint
    pub async fn discovery(
        oauth2_provider: web::Data<OAuth2Provider>,
    ) -> IdpResult<HttpResponse> {
        let discovery = oauth2_provider.discovery();
        Ok(HttpResponse::Ok().json(discovery))
    }

    /// OIDC UserInfo endpoint
    pub async fn userinfo(
        auth_header: web::Header<actix_web::http::header::Authorization<actix_web::http::header::Bearer>>,
        oauth2_provider: web::Data<OAuth2Provider>,
    ) -> IdpResult<HttpResponse> {
        let token = auth_header.as_ref().token();
        let userinfo = oauth2_provider.userinfo(token).await?;
        Ok(HttpResponse::Ok().json(userinfo))
    }
}

/// Audit Log Handlers
pub mod audit_handlers {
    use super::*;

    /// Get audit logs for organization
    pub async fn get_org_audit_logs(
        org_id: web::Path<Uuid>,
        audit_logger: web::Data<AuditLogger>,
    ) -> IdpResult<HttpResponse> {
        let logs = audit_logger
            .get_org_audit_trail(org_id.into_inner(), 100)
            .await?;

        Ok(HttpResponse::Ok().json(serde_json::json!({
            "logs": logs
        })))
    }

    /// Get audit logs for user
    pub async fn get_user_audit_logs(
        user_id: web::Path<Uuid>,
        audit_logger: web::Data<AuditLogger>,
    ) -> IdpResult<HttpResponse> {
        // TODO: Get user's org_id
        let org_id = Uuid::new_v4();

        let logs = audit_logger
            .get_org_audit_trail(org_id, 100)
            .await?;

        let filtered: Vec<_> = logs
            .into_iter()
            .filter(|l| l.user_id == Some(user_id.into_inner()))
            .collect();

        Ok(HttpResponse::Ok().json(serde_json::json!({
            "logs": filtered
        })))
    }
}

/// Health check
pub async fn health() -> HttpResponse {
    HttpResponse::Ok().json(serde_json::json!({
        "status": "healthy",
        "service": "ggen-idp"
    }))
}
