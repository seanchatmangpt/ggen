//! ggen-idp: Innovative Identity Provider System for BPMN.js Marketplace
//!
//! A production-grade identity and access management system featuring:
//! - BPMN workflow-based authentication flows
//! - Relation-Based Access Control (ReBAC) - inspired by Google Zanzibar
//! - Full OAuth2 and OpenID Connect (OIDC) provider
//! - Multi-tenant architecture
//! - Session management with token lifecycle
//! - Comprehensive audit logging
//! - MFA/2FA support
//! - RDF-backed identity graph
//!
//! # Architecture Overview
//!
//! ```text
//! ┌─────────────────────────────────────┐
//! │  HTTP API (Actix-web handlers)      │
//! └─────────────────────────────────────┘
//!               ↓
//! ┌─────────────────────────────────────┐
//! │  Core Services:                     │
//! │  - Auth Service (login/register)   │
//! │  - RBAC Engine                      │
//! │  - Session Manager                  │
//! │  - Audit Logger                     │
//! │  - OAuth2/OIDC Provider             │
//! └─────────────────────────────────────┘
//!               ↓
//! ┌─────────────────────────────────────┐
//! │  Workflow Engine:                   │
//! │  - BPMN-based Auth Flows           │
//! │  - Flow Executor                    │
//! └─────────────────────────────────────┘
//!               ↓
//! ┌─────────────────────────────────────┐
//! │  Storage Layer:                     │
//! │  - RDF Store (Identity Graph)       │
//! │  - PostgreSQL (Sessions, Audit)     │
//! └─────────────────────────────────────┘
//! ```

pub mod auth;
pub mod audit;
pub mod errors;
pub mod handlers;
pub mod models;
pub mod oauth2;
pub mod rbac;
pub mod sessions;
pub mod workflows;

// Re-export key types and traits
pub use auth::AuthService;
pub use audit::{AuditLogger, AuditLogStore};
pub use errors::{IdpError, IdpResult};
pub use handlers::*;
pub use models::*;
pub use oauth2::{OAuth2Client, OAuth2Provider};
pub use rbac::RbacEngine;
pub use sessions::{SessionManager, SessionStore};
pub use workflows::{AuthFlow, FlowExecutor, WorkflowExecutor};

use actix_web::{web, App, HttpServer, middleware};
use std::sync::Arc;

/// IDP Application Configuration
#[derive(Clone, Debug)]
pub struct IdpConfig {
    pub jwt_secret: String,
    pub jwt_expiry_secs: i64,
    pub refresh_token_expiry_secs: i64,
    pub database_url: String,
    pub rdf_store_path: String,
    pub oauth2_redirect_base_url: String,
    pub server_host: String,
    pub server_port: u16,
}

impl Default for IdpConfig {
    fn default() -> Self {
        Self {
            jwt_secret: "change-me-in-production".to_string(),
            jwt_expiry_secs: 3600,
            refresh_token_expiry_secs: 604800, // 7 days
            database_url: "postgres://localhost/ggen_idp".to_string(),
            rdf_store_path: "./data/rdf".to_string(),
            oauth2_redirect_base_url: "http://localhost:3000".to_string(),
            server_host: "localhost".to_string(),
            server_port: 8000,
        }
    }
}

/// Initialize and run the IDP application
pub async fn run_idp_server(config: IdpConfig) -> std::io::Result<()> {
    // Initialize services
    let auth_service = web::Data::new(AuthService::new());
    let rbac_engine = web::Data::new(RbacEngine::new(rbac::RbacStore::new()));

    let session_store = Arc::new(sessions::InMemorySessionStore::new());
    let session_manager = web::Data::new(SessionManager::new(
        session_store,
        chrono::Duration::hours(1),
    ));

    let audit_store = Arc::new(audit::InMemoryAuditLogStore::new());
    let audit_logger = web::Data::new(AuditLogger::new(audit_store));

    let oauth2_config = oauth2::OAuth2Config {
        issuer: format!("http://{}:{}", config.server_host, config.server_port),
        authorization_endpoint: format!("http://{}:{}/oauth/authorize", config.server_host, config.server_port),
        token_endpoint: format!("http://{}:{}/oauth/token", config.server_host, config.server_port),
        userinfo_endpoint: format!("http://{}:{}/oauth/userinfo", config.server_host, config.server_port),
        jwks_uri: format!("http://{}:{}/oauth/jwks", config.server_host, config.server_port),
        jwt_secret: config.jwt_secret.clone(),
        code_lifetime: chrono::Duration::minutes(10),
        token_lifetime: chrono::Duration::hours(1),
        refresh_token_lifetime: chrono::Duration::days(7),
    };
    let oauth2_provider = web::Data::new(OAuth2Provider::new(oauth2_config));

    let listen_addr = format!("{}:{}", config.server_host, config.server_port);

    println!("🚀 Starting ggen-idp server on {}", listen_addr);

    HttpServer::new(move || {
        App::new()
            // Logging and middleware
            .wrap(middleware::Logger::default())
            .wrap(middleware::Compress::default())
            // Shared state
            .app_data(auth_service.clone())
            .app_data(rbac_engine.clone())
            .app_data(session_manager.clone())
            .app_data(audit_logger.clone())
            .app_data(oauth2_provider.clone())
            // Health check
            .route("/health", web::get().to(handlers::health))
            // Authentication endpoints
            .service(
                web::scope("/auth/{org_id}")
                    .route("/register", web::post().to(handlers::auth_handlers::register))
                    .route("/login", web::post().to(handlers::auth_handlers::login))
                    .route("/refresh", web::post().to(handlers::auth_handlers::refresh_token))
                    .route("/logout/{session_id}", web::post().to(handlers::auth_handlers::logout))
                    .route("/change-password/{user_id}", web::post().to(handlers::auth_handlers::change_password))
            )
            // RBAC endpoints
            .service(
                web::scope("/rbac")
                    .route("/check/{user_id}", web::post().to(handlers::rbac_handlers::check_permission))
                    .route("/assign/{user_id}/{org_id}", web::post().to(handlers::rbac_handlers::assign_role))
                    .route("/revoke/{user_id}/{role_id}", web::post().to(handlers::rbac_handlers::revoke_role))
                    .route("/roles/{user_id}/{org_id}", web::get().to(handlers::rbac_handlers::list_user_roles))
            )
            // OAuth2 / OIDC endpoints
            .service(
                web::scope("/oauth")
                    .route("/authorize", web::get().to(handlers::oauth2_handlers::authorize))
                    .route("/token", web::post().to(handlers::oauth2_handlers::token))
                    .route("/.well-known/openid-configuration", web::get().to(handlers::oauth2_handlers::discovery))
                    .route("/userinfo", web::get().to(handlers::oauth2_handlers::userinfo))
            )
            // Audit endpoints
            .service(
                web::scope("/audit")
                    .route("/org/{org_id}", web::get().to(handlers::audit_handlers::get_org_audit_logs))
                    .route("/user/{user_id}", web::get().to(handlers::audit_handlers::get_user_audit_logs))
            )
    })
    .bind(&listen_addr)?
    .run()
    .await
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = IdpConfig::default();
        assert_eq!(config.server_host, "localhost");
        assert_eq!(config.server_port, 8000);
        assert_eq!(config.jwt_expiry_secs, 3600);
    }
}
