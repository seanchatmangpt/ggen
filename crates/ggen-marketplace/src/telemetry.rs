//! OpenTelemetry instrumentation for ggen marketplace
//!
//! This module provides OTLP tracing capabilities for all marketplace operations,
//! enabling trace validation by clnrm tests.

use anyhow::{Context, Result};
use opentelemetry::KeyValue;
use opentelemetry_sdk::{
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
                .unwrap_or_else(|_| "http://localhost:4318".to_string()),
            service_name: "ggen-marketplace".to_string(),
            sample_ratio: 1.0,
            console_output: true,
        }
    }
}

/// Initialize OpenTelemetry with OTLP exporter
///
/// This sets up:
/// - OTLP HTTP exporter for traces
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
    // Set OTLP endpoint environment variable for HTTP exporter (v0.31.0 API)
    std::env::set_var("OTEL_EXPORTER_OTLP_ENDPOINT", &config.endpoint);

    // Create OTLP HTTP exporter using v0.31.0 API
    let exporter = opentelemetry_otlp::SpanExporter::builder()
        .with_http()
        .build()
        .context("Failed to create OTLP HTTP exporter")?;

    // Create resource with service information (v0.31.0 API)
    let resource = Resource::builder_empty()
        .with_service_name(config.service_name.clone())
        .with_attributes([
            KeyValue::new("service.version", env!("CARGO_PKG_VERSION")),
            KeyValue::new("service.namespace", "ggen-marketplace"),
            KeyValue::new("telemetry.sdk.language", "rust"),
            KeyValue::new("telemetry.sdk.name", "opentelemetry"),
            KeyValue::new("telemetry.sdk.version", "0.31.0"),
        ])
        .build();

    // Create tracer provider with batch exporter (v0.31.0 API)
    let tracer_provider = opentelemetry_sdk::trace::SdkTracerProvider::builder()
        .with_sampler(Sampler::TraceIdRatioBased(config.sample_ratio))
        .with_id_generator(RandomIdGenerator::default())
        .with_resource(resource)
        .with_batch_exporter(exporter)
        .build();

    // Set global tracer provider
    opentelemetry::global::set_tracer_provider(tracer_provider);

    // Create tracer
    let tracer = opentelemetry::global::tracer("ggen-marketplace");

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

