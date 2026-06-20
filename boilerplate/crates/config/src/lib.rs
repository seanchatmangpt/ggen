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
    /// Validate all fields and return an error describing the first violation.
    pub fn validate(&self) -> anyhow::Result<()> {
        // 1. bind_addr must parse as a valid SocketAddr
        self.bind_addr
            .parse::<std::net::SocketAddr>()
            .map_err(|e| anyhow::anyhow!("invalid bind_addr '{}': {}", self.bind_addr, e))?;

        // 2. database_url must be non-empty
        if self.database_url.trim().is_empty() {
            return Err(anyhow::anyhow!("database_url must not be empty"));
        }

        // 3. log_level must be one of: trace, debug, info, warn, error
        let valid_levels = ["trace", "debug", "info", "warn", "error"];
        if !valid_levels.contains(&self.log_level.to_lowercase().as_str()) {
            return Err(anyhow::anyhow!(
                "invalid log_level '{}': must be one of {:?}",
                self.log_level,
                valid_levels
            ));
        }

        // 4. mcp_transport must be "stdio" or "http"
        if !matches!(self.mcp_transport.as_str(), "stdio" | "http") {
            return Err(anyhow::anyhow!(
                "invalid mcp_transport '{}': must be 'stdio' or 'http'",
                self.mcp_transport
            ));
        }

        Ok(())
    }

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
        assert!(
            !config.database_url.is_empty(),
            "database_url must be non-empty"
        );
        assert!(!config.log_level.is_empty(), "log_level must be non-empty");
        assert!(
            !config.mcp_transport.is_empty(),
            "mcp_transport must be non-empty"
        );
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

    // --- validate() tests (Chicago TDD: real state, no mocks) ---

    #[test]
    fn validate_default_config_passes() {
        AppConfig::default().validate().expect("default config must be valid");
    }

    #[test]
    fn validate_rejects_invalid_bind_addr() {
        let config = AppConfig {
            bind_addr: "not-an-addr".into(),
            ..AppConfig::default()
        };
        let err = config.validate().expect_err("must reject invalid bind_addr");
        assert!(
            err.to_string().contains("invalid bind_addr"),
            "error message should mention 'invalid bind_addr', got: {err}"
        );
    }

    #[test]
    fn validate_rejects_empty_database_url() {
        let config = AppConfig {
            database_url: "".into(),
            ..AppConfig::default()
        };
        let err = config.validate().expect_err("must reject empty database_url");
        assert!(
            err.to_string().contains("database_url"),
            "error message should mention 'database_url', got: {err}"
        );
    }

    #[test]
    fn validate_rejects_unknown_log_level() {
        let config = AppConfig {
            log_level: "verbose".into(),
            ..AppConfig::default()
        };
        let err = config.validate().expect_err("must reject unknown log_level");
        assert!(
            err.to_string().contains("invalid log_level"),
            "error message should mention 'invalid log_level', got: {err}"
        );
    }

    #[test]
    fn validate_rejects_unknown_mcp_transport() {
        let config = AppConfig {
            mcp_transport: "websocket".into(),
            ..AppConfig::default()
        };
        let err = config
            .validate()
            .expect_err("must reject unknown mcp_transport");
        assert!(
            err.to_string().contains("invalid mcp_transport"),
            "error message should mention 'invalid mcp_transport', got: {err}"
        );
    }

    #[test]
    fn validate_accepts_http_transport() {
        let config = AppConfig {
            mcp_transport: "http".into(),
            ..AppConfig::default()
        };
        config
            .validate()
            .expect("'http' must be a valid mcp_transport");
    }
}
