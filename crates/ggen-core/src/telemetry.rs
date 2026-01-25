//! OpenTelemetry instrumentation for ggen
//!
//! This module provides OTLP (OpenTelemetry Protocol) tracing capabilities for
//! all ggen operations. It enables distributed tracing, performance monitoring,
//! and trace validation by clnrm tests.
//!
//! **NOTE**: OpenTelemetry support is optional. Enable with `--features otel` when building.
//!
//! ## Features
//!
//! - **OTLP Export**: Export traces via OTLP HTTP/gRPC
//! - **Structured Tracing**: Rich span attributes and events
//! - **Console Output**: Local debugging with formatted trace output
//! - **Configurable Sampling**: Control trace sampling ratio
//! - **Service Identification**: Tag traces with service name and version
//!
//! ## Configuration
//!
//! Telemetry can be configured via environment variables:
//! - `OTEL_EXPORTER_OTLP_ENDPOINT`: OTLP endpoint URL (default: http://localhost:4318)
//! - `OTEL_SERVICE_NAME`: Service name for traces (default: "ggen")
//! - `RUST_LOG`: Log level filter (default: "info")
//!
//! ## Examples
//!
//! ### Initializing Telemetry (with "otel" feature enabled)
//!
//! ```rust,no_run
//! use ggen_core::telemetry::{init_telemetry, TelemetryConfig};
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let config = TelemetryConfig {
//!     endpoint: "http://localhost:4318".to_string(),
//!     service_name: "ggen".to_string(),
//!     sample_ratio: 1.0,
//!     console_output: true,
//! };
//!
//! let _guard = init_telemetry(config)?;
//! // Telemetry is now active
//! # Ok(())
//! # }
//! ```

use ggen_utils::error::Result;

// OpenTelemetry implementation (only when "otel" feature is enabled)
#[cfg(feature = "otel")]
use ggen_utils::error::Error;
#[cfg(feature = "otel")]
use opentelemetry::{global, KeyValue};
#[cfg(feature = "otel")]
use opentelemetry_otlp::WithExportConfig;
#[cfg(feature = "otel")]
use opentelemetry_sdk::{
    runtime,
    trace::{RandomIdGenerator, Sampler},
    Resource,
};
#[cfg(feature = "otel")]
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
        #[cfg(feature = "otel")]
        {
            Self {
                endpoint: std::env::var("OTEL_EXPORTER_OTLP_ENDPOINT")
                    .unwrap_or_else(|_| "http://localhost:4318".to_string()),
                service_name: "ggen".to_string(),
                sample_ratio: 1.0,
                console_output: true,
            }
        }
        #[cfg(not(feature = "otel"))]
        {
            Self {
                endpoint: String::new(),
                service_name: "ggen".to_string(),
                sample_ratio: 0.0,
                console_output: false,
            }
        }
    }
}

/// Initialize OpenTelemetry with OTLP exporter
///
/// **NOTE**: This function requires the "otel" feature to be enabled.
/// When the "otel" feature is disabled, this function becomes a no-op.
///
/// This sets up:
/// - OTLP HTTP exporter for traces
/// - Tracing subscriber with OpenTelemetry layer
/// - Console output for local debugging
///
/// # Example
///
/// ```no_run
/// use ggen_core::telemetry::{init_telemetry, TelemetryConfig};
///
/// #[tokio::main]
/// async fn main() -> ggen_utils::error::Result<()> {
///     let config = TelemetryConfig::default();
///     init_telemetry(config)?;
///
///     // Your application code here
///
///     shutdown_telemetry();
///     Ok(())
/// }
/// ```
#[cfg(feature = "otel")]
pub fn init_telemetry(config: TelemetryConfig) -> Result<()> {
    // Create OTLP tracer pipeline with HTTP exporter
    let tracer = opentelemetry_otlp::new_pipeline()
        .tracing()
        .with_exporter(
            opentelemetry_otlp::new_exporter()
                .tonic() // Use tonic/grpc transport
                .with_endpoint(&config.endpoint),
        )
        .with_trace_config(
            opentelemetry_sdk::trace::config()
                .with_sampler(Sampler::TraceIdRatioBased(config.sample_ratio))
                .with_id_generator(RandomIdGenerator::default())
                .with_resource(Resource::new(vec![
                    KeyValue::new("service.name", config.service_name.clone()),
                    KeyValue::new("service.version", env!("CARGO_PKG_VERSION")),
                ])),
        )
        .install_batch(runtime::Tokio)
        .map_err(|e| Error::with_context("Failed to install OTLP tracer", &e.to_string()))?;

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
        "OpenTelemetry initialized"
    );

    Ok(())
}

/// No-op implementation when "otel" feature is disabled
#[cfg(not(feature = "otel"))]
pub fn init_telemetry(_config: TelemetryConfig) -> Result<()> {
    // No-op: OpenTelemetry is disabled
    Ok(())
}

/// Shutdown OpenTelemetry and flush pending spans
///
/// **NOTE**: This function requires the "otel" feature to be enabled.
/// When the "otel" feature is disabled, this function becomes a no-op.
///
/// Call this before application exit to ensure all traces are exported.
#[cfg(feature = "otel")]
pub fn shutdown_telemetry() {
    tracing::info!("Shutting down OpenTelemetry");
    global::shutdown_tracer_provider();
}

/// No-op implementation when "otel" feature is disabled
#[cfg(not(feature = "otel"))]
pub fn shutdown_telemetry() {
    // No-op: OpenTelemetry is disabled
}

/// Create a telemetry context with common attributes
///
/// This is useful for adding consistent context to spans across operations.
#[macro_export]
macro_rules! telemetry_context {
    ($($key:expr => $value:expr),* $(,)?) => {
        {
            let span = tracing::Span::current();
            $(
                span.record($key, &tracing::field::display($value));
            )*
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_telemetry_config_default() {
        let config = TelemetryConfig::default();
        assert_eq!(config.service_name, "ggen");
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
