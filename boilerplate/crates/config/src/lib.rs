use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::env;

/// Layered application config: defaults → env vars → CLI overrides.
///
/// Precedence (highest wins): CLI flags > env vars > defaults.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AppConfig {
    pub bind_addr: String,
    pub database_url: String,
    pub log_level: String,
    pub mcp_transport: String,
}

impl Default for AppConfig {
    fn default() -> Self {
        Self {
            bind_addr: "0.0.0.0:8080".into(),
            database_url: "app.db".into(),
            log_level: "info".into(),
            mcp_transport: "stdio".into(),
        }
    }
}

impl AppConfig {
    /// Load from environment variables with `.env` file support.
    pub fn from_env() -> Result<Self> {
        // Load .env file if present (silently ignore if absent).
        let _ = dotenvy::dotenv();

        Ok(Self {
            bind_addr: env::var("BIND_ADDR").unwrap_or_else(|_| "0.0.0.0:8080".into()),
            database_url: env::var("DATABASE_URL").unwrap_or_else(|_| "app.db".into()),
            log_level: env::var("LOG_LEVEL").unwrap_or_else(|_| "info".into()),
            mcp_transport: env::var("MCP_TRANSPORT").unwrap_or_else(|_| "stdio".into()),
        })
    }
}
