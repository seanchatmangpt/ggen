/// ggen-idp CLI Server
///
/// Usage:
///   cargo run --bin ggen-idp-server
///
/// Environment Variables:
///   GGEN_IDP_HOST=localhost
///   GGEN_IDP_PORT=8000
///   GGEN_IDP_JWT_SECRET=your-secret-key
///   DATABASE_URL=postgres://localhost/ggen_idp

use ggen_idp::IdpConfig;
use std::env;

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    // Initialize logging
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("info"));

    // Load configuration from environment
    let config = IdpConfig {
        jwt_secret: env::var("GGEN_IDP_JWT_SECRET")
            .unwrap_or_else(|_| "change-me-in-production".to_string()),
        jwt_expiry_secs: env::var("GGEN_IDP_JWT_EXPIRY")
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(3600),
        refresh_token_expiry_secs: env::var("GGEN_IDP_REFRESH_TOKEN_EXPIRY")
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(604800),
        database_url: env::var("DATABASE_URL")
            .unwrap_or_else(|_| "postgres://localhost/ggen_idp".to_string()),
        rdf_store_path: env::var("GGEN_IDP_RDF_STORE")
            .unwrap_or_else(|_| "./data/rdf".to_string()),
        oauth2_redirect_base_url: env::var("GGEN_IDP_OAUTH_BASE")
            .unwrap_or_else(|_| "http://localhost:3000".to_string()),
        server_host: env::var("GGEN_IDP_HOST").unwrap_or_else(|_| "localhost".to_string()),
        server_port: env::var("GGEN_IDP_PORT")
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(8000),
    };

    println!("═══════════════════════════════════════════════════════");
    println!("🔐 ggen-idp: Identity Provider System");
    println!("═══════════════════════════════════════════════════════");
    println!("\n📋 Configuration:");
    println!("  Host: {}", config.server_host);
    println!("  Port: {}", config.server_port);
    println!("  Database: {}", config.database_url);
    println!("\n📍 Key Endpoints:");
    println!("  Health: http://{}:{}/health", config.server_host, config.server_port);
    println!("  Auth: http://{}:{}/auth/{{org_id}}/login", config.server_host, config.server_port);
    println!("  OAuth2: http://{}:{}/oauth/authorize", config.server_host, config.server_port);
    println!("  OIDC Discovery: http://{}:{}/oauth/.well-known/openid-configuration", config.server_host, config.server_port);
    println!("\n🚀 Starting server...\n");

    ggen_idp::run_idp_server(config).await
}
