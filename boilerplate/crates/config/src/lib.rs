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

#[cfg(test)]
mod tests {
    use super::*;

    /// Real env call — no mocks. `from_env()` uses `unwrap_or_else` fallbacks so
    /// it succeeds regardless of whether the env vars are set.
    #[test]
    fn from_env_succeeds_with_defaults() {
        let config = AppConfig::from_env().expect("from_env must succeed with defaults");
        assert!(!config.bind_addr.is_empty(), "bind_addr must be non-empty");
        assert!(!config.database_url.is_empty(), "database_url must be non-empty");
        assert!(!config.log_level.is_empty(), "log_level must be non-empty");
        assert!(!config.mcp_transport.is_empty(), "mcp_transport must be non-empty");
    }

    #[test]
    fn from_env_respects_bind_addr_env_var() {
        // Arrange: set a real env var before calling from_env
        std::env::set_var("BIND_ADDR", "127.0.0.1:9999");

        // Act: real env call
        let config = AppConfig::from_env().expect("from_env must succeed");

        // Assert: observable state — bind_addr reflects the env var
        assert_eq!(config.bind_addr, "127.0.0.1:9999");

        // Cleanup
        std::env::remove_var("BIND_ADDR");
    }

    #[test]
    fn default_bind_addr_is_all_interfaces() {
        let config = AppConfig::default();
        assert_eq!(config.bind_addr, "0.0.0.0:8080");
    }
}
