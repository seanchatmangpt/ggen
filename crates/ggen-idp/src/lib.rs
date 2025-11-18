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

// FOCUS: 2028 Features for AI Agent Swarm Identity Infrastructure
pub mod features_2028;     // 2028 Features: DIDs, ZK Proofs, Autonomous Agents, Quantum-Safe Crypto, Swarm Coordination, Reputation, Biometrics

// Existing IDP modules (disabled for 2028 features focus)
// pub mod auth;
// pub mod audit;
// pub mod errors;
// pub mod handlers;
// pub mod models;
// pub mod oauth2;
// pub mod rbac;
// pub mod sessions;
// pub mod workflows;

// Re-export 2028 features
pub use features_2028::*;

// Re-export key types and traits (disabled - uncomment when other modules are re-enabled)
// pub use auth::AuthService;
// pub use audit::{AuditLogger, AuditLogStore};
// pub use errors::{IdpError, IdpResult};
// pub use handlers::*;
// pub use models::*;
// pub use oauth2::{OAuth2Client, OAuth2Provider};
// pub use rbac::RbacEngine;
// pub use sessions::{SessionManager, SessionStore};
// pub use workflows::{AuthFlow, FlowExecutor, WorkflowExecutor};

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
/// (Disabled for 2028 features focus - re-enable when other modules are re-enabled)
/*
pub async fn run_idp_server(config: IdpConfig) -> std::io::Result<()> {
    // Initialize services
    let auth_service = web::Data::new(AuthService::new());
    let rbac_engine = web::Data::new(RbacEngine::new(rbac::RbacStore::new()));
    // ... rest of the function disabled
}
*/

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
