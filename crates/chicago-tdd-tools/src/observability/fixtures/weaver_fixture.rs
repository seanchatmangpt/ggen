//! High level Weaver fixture that wires live-check into Chicago TDD tests.

#![cfg(all(feature = "weaver", feature = "otel"))]

use std::path::Path;
use std::sync::{Arc, Mutex};

use tempfile::TempDir;

use crate::observability::unified::TestConfig;
use crate::observability::{ObservabilityError, ObservabilityResult, ObservabilityTest};

use super::{TelemetryCapture, TelemetryTracer, ValidationResults};

/// Fixture that manages Weaver live-check lifecycle and telemetry capture.
pub struct WeaverTestFixture {
    observability: ObservabilityTest,
    capture: TelemetryCapture,
    output_dir: TempDir,
}

impl WeaverTestFixture {
    /// Create a new Weaver test fixture with zero configuration.
    ///
    /// # Errors
    ///
    /// Returns an error if Weaver cannot be started or configured.
    pub fn new() -> ObservabilityResult<Self> {
        Self::with_config(TestConfig::default())
    }

    /// Create a new Weaver test fixture with custom configuration.
    ///
    /// # Errors
    ///
    /// Returns an error if Weaver cannot be started or configured.
    pub fn with_config(mut config: TestConfig) -> ObservabilityResult<Self> {
        config.weaver_enabled = true;

        if let Some(ref path) = config.registry_path {
            if !path.exists() {
                return Err(ObservabilityError::RegistryNotFound(format!(
                    "Semantic conventions registry not found at {}. Run `cargo make weaver-bootstrap` or provide an existing registry path.",
                    path.display()
                )));
            }
        }

        let output_dir = TempDir::new().map_err(|err| {
            ObservabilityError::ValidationFailed(format!(
                "Failed to create temporary Weaver report directory: {err}"
            ))
        })?;

        config.weaver_output_dir = Some(output_dir.path().to_path_buf());

        let observability = ObservabilityTest::with_config(config)?;

        let capture = TelemetryCapture::new(observability.otlp_endpoint());

        Ok(Self { observability, capture, output_dir })
    }

    /// Acquire a tracer that exports spans to the Weaver live-check instance.
    ///
    /// The tracer should be used to instrument the code under test.  When the
    /// fixture is finished all pending telemetry is flushed automatically.
    ///
    /// # Errors
    ///
    /// Returns an error if the tracer cannot be created.
    pub fn tracer(
        &self,
        instrumentation_name: &str,
        service_name: &str,
    ) -> ObservabilityResult<TelemetryTracer> {
        self.capture.tracer(instrumentation_name, service_name)
    }

    /// Access the underlying observability test helper for advanced scenarios.
    #[must_use]
    pub const fn observability(&self) -> &ObservabilityTest {
        &self.observability
    }

    /// Location of the Weaver report directory for this fixture.
    #[must_use]
    pub fn output_dir(&self) -> &Path {
        self.output_dir.path()
    }

    /// Flush captured telemetry, wait for Weaver to produce reports, and parse
    /// the validation output.
    ///
    /// **Use this method in blocking contexts** (synchronous tests, non-async code).
    /// For async contexts, use `finish_async()` instead.
    ///
    /// # Errors
    ///
    /// Returns an error if telemetry capture fails, Weaver processing fails,
    /// or validation results cannot be parsed.
    pub fn finish(&mut self) -> ObservabilityResult<ValidationResults> {
        self.capture.flush()?;
        #[cfg(feature = "weaver")]
        {
            self.observability.stop_weaver_process();
        }

        ValidationResults::from_report_dir(self.output_dir())
    }

    /// Flush captured telemetry, wait for Weaver to produce reports, and parse
    /// the validation output (async-aware version).
    ///
    /// **Use this method in async contexts** (tokio tests, async functions).
    /// This method automatically handles async/blocking context switching using
    /// `tokio::task::spawn_blocking`, eliminating the need for manual thread spawning.
    ///
    /// **TRIZ Solution**: Resolves async/blocking contradiction by handling context
    /// switching internally (Principle #13: The Other Way Round, Principle #24: Intermediary).
    ///
    /// # Errors
    ///
    /// For blocking contexts, use `finish()` instead.
    ///
    /// # Errors
    ///
    /// Returns an error if telemetry capture fails, Weaver processing fails,
    /// or validation results cannot be parsed.
    pub async fn finish_async(&mut self) -> ObservabilityResult<ValidationResults> {
        self.capture.flush()?;

        let observability = Arc::new(Mutex::new(std::mem::replace(
            &mut self.observability,
            ObservabilityTest::with_config(TestConfig::default()).map_err(|e| {
                ObservabilityError::ValidationFailed(format!(
                    "Failed to create temporary ObservabilityTest for finish_async: {e}"
                ))
            })?,
        )));

        let observability_clone = Arc::clone(&observability);
        let stop_result = tokio::task::spawn_blocking(move || {
            #[cfg(feature = "weaver")]
            {
                let mut obs = observability_clone.lock().map_err(|e| {
                    ObservabilityError::ValidationFailed(format!(
                        "Failed to acquire lock on ObservabilityTest: {e}"
                    ))
                })?;
                obs.stop_weaver_process();
            }
            Ok(())
        })
        .await
        .map_err(|e| {
            ObservabilityError::ValidationFailed(format!(
                "Failed to execute blocking operation in async context: {e}"
            ))
        })?;

        stop_result?;

        // Restore observability (though fixture is typically dropped after this)
        // **Kaizen improvement**: Consistent error handling pattern (map_err instead of unwrap)
        self.observability = Arc::try_unwrap(observability)
            .map_err(|_| {
                ObservabilityError::ValidationFailed(
                    "ObservabilityTest Arc should have single owner after finish_async()"
                        .to_string(),
                )
            })?
            .into_inner()
            .map_err(|e| {
                ObservabilityError::ValidationFailed(format!(
                    "Failed to extract ObservabilityTest from Mutex: {e}"
                ))
            })?;

        // Parse validation results (this is also blocking, but lightweight)
        ValidationResults::from_report_dir(self.output_dir())
    }
}
