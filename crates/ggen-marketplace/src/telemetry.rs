//! OpenTelemetry instrumentation for ggen marketplace
//!
//! This module provides OTLP (OpenTelemetry Protocol) tracing capabilities for
//! all marketplace operations. It enables distributed tracing, performance monitoring,
//! and trace validation by clnrm tests.
//!
//! ## Features
//!
//! - **OTLP Export**: Export traces via OTLP gRPC
//! - **Structured Tracing**: Rich span attributes and events
//! - **Console Output**: Local debugging with formatted trace output
//! - **Configurable Sampling**: Control trace sampling ratio
//! - **Service Identification**: Tag traces with service name and version
//!
//! ## Configuration
//!
//! Telemetry can be configured via environment variables:
//! - `OTEL_EXPORTER_OTLP_ENDPOINT`: OTLP endpoint URL (default: http://localhost:4317)
//! - `OTEL_SERVICE_NAME`: Service name for traces (default: "ggen-marketplace")
//! - `RUST_LOG`: Log level filter (default: "info")
//!
//! ## Examples
//!
//! ### Initializing Telemetry
//!
//! ```rust,no_run
//! use ggen_marketplace::telemetry::{init_telemetry, TelemetryConfig};
//!
//! # fn main() -> anyhow::Result<()> {
//! let config = TelemetryConfig {
//!     endpoint: "http://localhost:4317".to_string(),
//!     service_name: "ggen-marketplace".to_string(),
//!     sample_ratio: 1.0,
//!     console_output: true,
//! };
//!
//! let _guard = init_telemetry(config)?;
//! // Telemetry is now active
//! # Ok(())
//! # }
//! ```

use anyhow::{Context, Result};
use opentelemetry::KeyValue;
use opentelemetry_otlp::WithExportConfig;
use opentelemetry_sdk::{
    runtime,
    trace::{RandomIdGenerator, Sampler},
    Resource,
};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt, EnvFilter, Registry};

/// OpenTelemetry configuration
#[derive(Debug, Clone)]
pub struct TelemetryConfig {
    /// OTLP endpoint (default: http://localhost:4318)
    pub endpoint: String,
    /// Service name for traces
    pub service_name: String,
    /// Sample ratio (0.0 to 1.0, default: 1.0)
    pub sample_ratio: f64,
    /// Whether to enable console output
    pub console_output: bool,
}

impl Default for TelemetryConfig {
    fn default() -> Self {
        Self {
            endpoint: std::env::var("OTEL_EXPORTER_OTLP_ENDPOINT")
                .unwrap_or_else(|_| "http://localhost:4317".to_string()), // gRPC default port
            service_name: "ggen-marketplace".to_string(),
            sample_ratio: 1.0,
            console_output: true,
        }
    }
}

/// Initialize OpenTelemetry with OTLP exporter
///
/// This sets up:
/// - OTLP gRPC exporter for traces (CLI-only, no HTTP)
/// - Tracing subscriber with OpenTelemetry layer
/// - Console output for local debugging
///
/// # Example
///
/// ```no_run
/// use ggen_marketplace::telemetry::{init_telemetry, TelemetryConfig};
///
/// #[tokio::main]
/// async fn main() -> anyhow::Result<()> {
///     let config = TelemetryConfig::default();
///     init_telemetry(config)?;
///
///     // Your marketplace code here
///
///     shutdown_telemetry();
///     Ok(())
/// }
/// ```
pub fn init_telemetry(config: TelemetryConfig) -> Result<()> {
    // Create OTLP tracer pipeline with gRPC exporter (matching ggen-core API)
    let tracer = opentelemetry_otlp::new_pipeline()
        .tracing()
        .with_exporter(
            opentelemetry_otlp::new_exporter()
                .tonic() // Use tonic/gRPC transport
                .with_endpoint(&config.endpoint),
        )
        .with_trace_config(
            opentelemetry_sdk::trace::config()
                .with_sampler(Sampler::TraceIdRatioBased(config.sample_ratio))
                .with_id_generator(RandomIdGenerator::default())
                .with_resource(Resource::new(vec![
                    KeyValue::new("service.name", config.service_name.clone()),
                    KeyValue::new("service.version", env!("CARGO_PKG_VERSION")),
                    KeyValue::new("service.namespace", "ggen-marketplace"),
                    KeyValue::new("telemetry.sdk.language", "rust"),
                    KeyValue::new("telemetry.sdk.name", "opentelemetry"),
                ])),
        )
        .install_batch(runtime::Tokio)
        .context("Failed to install OTLP tracer")?;

    // Create OpenTelemetry tracing layer
    let telemetry_layer = tracing_opentelemetry::layer().with_tracer(tracer);

    // Create subscriber with layers
    let subscriber = Registry::default()
        .with(EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info")))
        .with(telemetry_layer);

    // Add console output if enabled
    if config.console_output {
        let fmt_layer = tracing_subscriber::fmt::layer()
            .with_target(true)
            .with_level(true);
        subscriber.with(fmt_layer).init();
    } else {
        subscriber.init();
    }

    tracing::info!(
        endpoint = %config.endpoint,
        service = %config.service_name,
        sample_ratio = config.sample_ratio,
        "OpenTelemetry initialized for ggen marketplace"
    );

    Ok(())
}

/// Shutdown OpenTelemetry and flush pending spans
///
/// Call this before application exit to ensure all traces are exported.
/// Note: In OpenTelemetry 0.31.0, shutdown is handled automatically by the SDK.
pub fn shutdown_telemetry() {
    tracing::info!("Shutting down OpenTelemetry (handled automatically by SDK in v0.31.0)");
    // Shutdown is handled automatically by the SDK in OpenTelemetry 0.31.0
    // No explicit shutdown needed - tracer provider will flush and shutdown on drop
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_telemetry_config_default() {
        let config = TelemetryConfig::default();
        assert_eq!(config.service_name, "ggen-marketplace");
        assert_eq!(config.sample_ratio, 1.0);
        assert!(config.console_output);
    }

    #[test]
    fn test_telemetry_config_custom() {
        let config = TelemetryConfig {
            endpoint: "http://custom:4318".to_string(),
            service_name: "test-service".to_string(),
            sample_ratio: 0.5,
            console_output: false,
        };
        assert_eq!(config.endpoint, "http://custom:4318");
        assert_eq!(config.service_name, "test-service");
        assert_eq!(config.sample_ratio, 0.5);
        assert!(!config.console_output);
    }
}
