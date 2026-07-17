//! OpenTelemetry instrumentation for ggen, native to `ggen-cli`.
//!
//! Ported from `ggen-core/src/telemetry.rs` during the ggen-core retirement migration
//! (`specs/014-ggen-core-replacement`, task T034): this module has zero counterpart in
//! `ggen-config`/`ggen-marketplace`/`ggen-engine`, so it moves into its sole remaining
//! consumer (`lib.rs`'s `cli_match()`) rather than being invented as new shared surface.
//!
//! **Behavior-preserving simplification**: the original module gated its real
//! implementation behind `#[cfg(feature = "otel")]`, but `ggen-cli`'s own
//! `Cargo.toml` unconditionally requested `ggen-core`'s `otel` feature
//! (`[dependencies.ggen-core] features = ["otel"]`) — so in every build of
//! `ggen-cli` that has ever existed, the "otel" branch was the one compiled in.
//! This port keeps that same always-on behavior directly (no feature gate), rather
//! than fabricating a new opt-out toggle nothing has ever exercised.
//!
//! It enables distributed tracing, performance monitoring, and trace validation by
//! clnrm tests.
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
//! ### Initializing Telemetry
//!
//! ```rust,no_run
//! use ggen_cli_lib::telemetry::{init_telemetry, TelemetryConfig};
//!
//! # fn main() -> ggen_cli_lib::utils::error::Result<()> {
//! let config = TelemetryConfig {
//!     endpoint: "http://localhost:4317".to_string(),
//!     service_name: "ggen".to_string(),
//!     console_output: true,
//! };
//!
//! let _guard = init_telemetry(config)?;
//! // Telemetry is now active
//! # Ok(())
//! # }
//! ```

use crate::utils::error::{Error, Result};

use opentelemetry::{global, KeyValue};
use opentelemetry_sdk::trace::SdkTracerProvider;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt, EnvFilter, Registry};

/// OpenTelemetry configuration
#[derive(Debug, Clone)]
pub struct TelemetryConfig {
    /// OTLP endpoint (default: http://localhost:4317)
    pub endpoint: String,
    /// Service name for traces
    pub service_name: String,
    /// Whether to enable console output
    pub console_output: bool,
}

impl Default for TelemetryConfig {
    fn default() -> Self {
        Self {
            endpoint: std::env::var("OTEL_EXPORTER_OTLP_ENDPOINT")
                .unwrap_or_else(|_| "http://localhost:4317".to_string()),
            service_name: "ggen".to_string(),
            console_output: true,
        }
    }
}

/// Guard that shuts down the tracer provider on drop.
pub struct TelemetryGuard {
    provider: SdkTracerProvider,
}

impl Drop for TelemetryGuard {
    fn drop(&mut self) {
        let _ = self.provider.shutdown();
    }
}

/// Initialize OpenTelemetry with OTLP gRPC exporter
///
/// This sets up:
/// - OTLP gRPC exporter for traces
/// - Tracing subscriber with OpenTelemetry layer
/// - Console output for local debugging
///
/// # Example
///
/// ```no_run
/// use ggen_cli_lib::telemetry::{init_telemetry, TelemetryConfig};
///
/// #[tokio::main]
/// async fn main() -> ggen_cli_lib::utils::error::Result<()> {
///     let config = TelemetryConfig::default();
///     let _guard = init_telemetry(config)?;
///
///     // Your application code here
///
///     Ok(())
/// }
/// ```
pub fn init_telemetry(config: TelemetryConfig) -> Result<TelemetryGuard> {
    use opentelemetry_otlp::WithExportConfig;
    use opentelemetry_sdk::Resource;

    let exporter = opentelemetry_otlp::SpanExporter::builder()
        .with_tonic()
        .with_endpoint(&config.endpoint)
        .build()
        .map_err(|e| Error::with_context("Failed to create OTLP exporter", &e.to_string()))?;

    let resource = Resource::builder_empty()
        .with_attributes([
            KeyValue::new("service.name", config.service_name.clone()),
            KeyValue::new("service.version", env!("CARGO_PKG_VERSION")),
        ])
        .build();

    let provider = SdkTracerProvider::builder()
        .with_batch_exporter(exporter)
        .with_resource(resource)
        .build();

    global::set_tracer_provider(provider.clone());

    let tracer = global::tracer("ggen");
    let telemetry_layer = tracing_opentelemetry::layer().with_tracer(tracer);

    let subscriber = Registry::default()
        .with(EnvFilter::try_from_default_env().unwrap_or_else(|_| {
            EnvFilter::new("info,opentelemetry=off,opentelemetry_sdk=off,opentelemetry_otlp=off")
        }))
        .with(telemetry_layer);

    if config.console_output {
        let fmt_layer = tracing_subscriber::fmt::layer()
            .with_target(true)
            .with_level(true);
        let _ = subscriber.with(fmt_layer).try_init();
    } else {
        let _ = subscriber.try_init();
    }

    tracing::info!(
        endpoint = %config.endpoint,
        service = %config.service_name,
        "OpenTelemetry initialized"
    );

    Ok(TelemetryGuard { provider })
}

/// Shutdown OpenTelemetry and flush pending spans
///
/// Call this before application exit to ensure all traces are exported.
/// Prefer using the `TelemetryGuard` returned by `init_telemetry()` instead,
/// which shuts down automatically on drop.
pub fn shutdown_telemetry() {
    tracing::info!("OpenTelemetry shutdown requested — use TelemetryGuard for automatic flush");
    // In opentelemetry 0.31, shutdown is via SdkTracerProvider::shutdown().
    // Use the TelemetryGuard returned by init_telemetry() instead.
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
        assert!(config.console_output);
    }

    #[test]
    fn test_telemetry_config_custom() {
        let config = TelemetryConfig {
            endpoint: "http://custom:4317".to_string(),
            service_name: "test-service".to_string(),
            console_output: false,
        };
        assert_eq!(config.endpoint, "http://custom:4317");
        assert_eq!(config.service_name, "test-service");
        assert!(!config.console_output);
    }
}
