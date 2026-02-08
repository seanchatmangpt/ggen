//! Shared application state

use ggen_core::telemetry::TelemetryConfig;
use std::sync::Arc;

/// Shared application state
#[derive(Clone)]
pub struct AppState {
    /// Configuration
    pub config: Arc<ApiConfig>,
    /// Telemetry configuration
    pub telemetry: Arc<TelemetryConfig>,
}

/// API configuration
#[derive(Debug, Clone)]
pub struct ApiConfig {
    pub host: String,
    pub port: u16,
    pub jwt_secret: String,
    pub stripe_key: Option<String>,
    pub max_requests_per_minute: u32,
    pub max_api_calls_per_minute: u32,
    pub base_url: String,
}

impl Default for ApiConfig {
    fn default() -> Self {
        Self {
            host: "127.0.0.1".to_string(),
            port: 3000,
            jwt_secret: "change-me-in-production".to_string(),
            stripe_key: None,
            max_requests_per_minute: 100,
            max_api_calls_per_minute: 1000,
            base_url: "http://localhost:3000".to_string(),
        }
    }
}

impl AppState {
    pub fn new(config: ApiConfig, telemetry: TelemetryConfig) -> Self {
        Self {
            config: Arc::new(config),
            telemetry: Arc::new(telemetry),
        }
    }
}
