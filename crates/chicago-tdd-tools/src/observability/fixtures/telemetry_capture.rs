//! Telemetry capture helpers for Weaver-integrated tests.
//!
//! These helpers spin up OTLP exporters that stream spans directly to the
//! Weaver live-check endpoint used by the [`super::WeaverTestFixture`].  The exported
//! tracers automatically flush on drop to ensure telemetry reaches Weaver
//! before validation runs.

#![cfg(all(feature = "weaver", feature = "otel"))]

use std::sync::{Arc, Mutex};

use opentelemetry::trace::TracerProvider as _;
use opentelemetry_otlp::WithExportConfig;
use opentelemetry_sdk::{
    resource::Resource,
    trace::{self, SdkTracerProvider},
};

use crate::observability::{ObservabilityError, ObservabilityResult};

/// Helper that provisions tracers which automatically export spans to Weaver.
#[derive(Debug)]
pub struct TelemetryCapture {
    endpoint: String,
    tracers: Mutex<Vec<Arc<TelemetryTracerInner>>>,
    _rt: std::sync::Arc<tokio::runtime::Runtime>,
}

impl TelemetryCapture {
    /// Create a new telemetry capture for the given endpoint.
    #[must_use]
    pub fn new(endpoint: impl Into<String>) -> Self {
        let rt = tokio::runtime::Runtime::new().unwrap();
        Self {
            endpoint: endpoint.into(),
            tracers: Mutex::new(Vec::new()),
            _rt: std::sync::Arc::new(rt),
        }
    }

    /// Provision a tracer that exports to the Weaver endpoint.
    ///
    /// The returned [`TelemetryTracer`] guarantees that telemetry is flushed and
    /// the provider shut down when dropped.
    ///
    /// # Panics
    ///
    /// Panics if the mutex is poisoned (should never happen in normal operation).
    ///
    /// # Errors
    ///
    /// Returns an error if the OTLP exporter cannot be created.
    pub fn tracer(
        &self,
        instrumentation_name: &str,
        service_name: &str,
    ) -> ObservabilityResult<TelemetryTracer> {
        let _guard = self._rt.enter();
        let exporter = opentelemetry_otlp::SpanExporter::builder()
            .with_tonic()
            .with_endpoint(self.endpoint.clone())
            .build()
            .map_err(|err| {
                ObservabilityError::ValidationFailed(format!(
                    "Failed to create OTLP exporter for Weaver: {err}"
                ))
            })?;

        let resource = Resource::builder().with_service_name(service_name.to_string()).build();

        let provider = SdkTracerProvider::builder()
            .with_simple_exporter(exporter)
            .with_resource(resource)
            .build();

        // **Kaizen improvement**: Convert to owned String to avoid lifetime issues
        let instrumentation_name_owned = instrumentation_name.to_string();
        let tracer = provider.tracer(instrumentation_name_owned);

        let inner = Arc::new(TelemetryTracerInner { provider, tracer });

        #[allow(clippy::expect_used)] // Mutex should never be poisoned in normal operation
        self.tracers
            .lock()
            .expect("TelemetryCapture mutex poisoned")
            .push(inner.clone());

        Ok(TelemetryTracer { inner })
    }

    /// Force flush all tracers provisioned by this capture.
    ///
    /// # Panics
    ///
    /// Panics if the mutex is poisoned (should never happen in normal operation).
    ///
    /// # Errors
    ///
    /// Returns an error if flushing any tracer fails.
    pub fn flush(&self) -> ObservabilityResult<()> {
        #[allow(clippy::expect_used)] // Mutex should never be poisoned in normal operation
        for tracer in self.tracers.lock().expect("TelemetryCapture mutex poisoned").iter() {
            tracer.force_flush()?;
        }

        Ok(())
    }
}

#[derive(Debug)]
struct TelemetryTracerInner {
    provider: SdkTracerProvider,
    tracer: trace::Tracer,
}

impl TelemetryTracerInner {
    fn force_flush(&self) -> ObservabilityResult<()> {
        self.provider.force_flush().map_err(|err| {
            ObservabilityError::ValidationFailed(format!("Failed to flush tracer: {err}"))
        })
    }
}

impl Drop for TelemetryTracerInner {
    fn drop(&mut self) {
        let _ = self.provider.shutdown();
    }
}

/// Handle for a tracer exporting spans to Weaver live-check.
#[derive(Debug, Clone)]
pub struct TelemetryTracer {
    inner: Arc<TelemetryTracerInner>,
}

impl TelemetryTracer {
    /// Access the OpenTelemetry tracer.
    #[must_use]
    pub fn tracer(&self) -> &trace::Tracer {
        &self.inner.tracer
    }

    /// Clone the underlying tracer instance.
    #[must_use]
    pub fn clone_tracer(&self) -> trace::Tracer {
        self.inner.tracer.clone()
    }

    /// Force flush any pending telemetry.
    ///
    /// # Errors
    ///
    /// Returns an error if flushing fails.
    pub fn force_flush(&self) -> ObservabilityResult<()> {
        self.inner.force_flush()
    }
}
