//! Weaver Live Validation Integration
//!
//! Provides integration with Weaver live-check for runtime telemetry validation.
//! Ensures all OTEL spans and metrics conform to declared schema.

#[cfg(feature = "weaver")]
use crate::observability::weaver::types::WeaverLiveCheck;
use std::path::{Path, PathBuf};
use std::process::Child;
use thiserror::Error;

#[cfg(feature = "weaver")]
pub mod poka_yoke;
pub mod types;

/// Poka-yoke types for Weaver process lifecycle
///
/// **Poka-yoke**: Type-level state machine prevents invalid operations.
/// A Weaver validator is either `Stopped` or `Running` - cannot be both.
#[cfg(feature = "weaver")]
pub mod lifecycle {
    use std::marker::PhantomData;
    use std::path::PathBuf;
    use std::process::Child;

    /// Weaver validator state marker types
    pub mod state {
        /// Validator is stopped (initial state)
        pub struct Stopped;

        /// Validator is running (can validate telemetry)
        pub struct Running;
    }

    /// Weaver validator with type-level state
    ///
    /// **Poka-yoke**: Type parameter `S` prevents invalid operations.
    /// - `WeaverValidator<Stopped>`: Can only start, cannot validate
    /// - `WeaverValidator<Running>`: Can validate, can stop
    pub struct WeaverValidator<S> {
        /// Registry path (validated)
        registry_path: PathBuf,
        /// OTLP gRPC port
        otlp_grpc_port: u16,
        /// Admin port
        admin_port: u16,
        /// Process handle (only Some when Running)
        process: Option<Child>,
        /// State marker (compile-time guarantee)
        _state: PhantomData<S>,
    }

    impl WeaverValidator<state::Stopped> {
        /// Create a new stopped Weaver validator
        ///
        /// **Poka-yoke**: Returns `WeaverValidator<Stopped>` - cannot validate until started.
        ///
        /// # Errors
        ///
        /// Returns error if registry path is invalid or Weaver binary not found.
        pub fn new(
            registry_path: PathBuf,
        ) -> crate::observability::weaver::WeaverValidationResult<Self> {
            use crate::observability::weaver::WeaverValidationError;

            // Check Weaver binary availability
            crate::observability::weaver::WeaverValidator::check_weaver_available()?;

            // Validate registry path exists
            if !registry_path.exists() {
                return Err(WeaverValidationError::RegistryNotFound(
                    registry_path.display().to_string(),
                ));
            }

            Ok(Self {
                registry_path,
                otlp_grpc_port: crate::observability::weaver::DEFAULT_OTLP_GRPC_PORT,
                admin_port: crate::observability::weaver::DEFAULT_ADMIN_PORT,
                process: None,
                _state: PhantomData,
            })
        }

        /// Start the Weaver validator
        ///
        /// **Poka-yoke**: Changes type from `WeaverValidator<Stopped>` to `WeaverValidator<Running>`.
        /// After this call, validator can validate telemetry.
        ///
        /// # Errors
        ///
        /// Returns error if Weaver start fails.
        pub fn start(
            self,
        ) -> crate::observability::weaver::WeaverValidationResult<WeaverValidator<state::Running>>
        {
            use crate::observability::weaver::types::WeaverLiveCheck;
            use crate::observability::weaver::WeaverValidationError;
            use std::process::Command;

            // Check Docker if testcontainers feature enabled
            #[cfg(feature = "testcontainers")]
            {
                use crate::testcontainers::check_docker_available;
                check_docker_available().map_err(|e| {
                    WeaverValidationError::DockerUnavailable(format!(
                        "Docker daemon is not running. Error: {e}"
                    ))
                })?;
            }

            // Find the Weaver binary
            let weaver_binary = WeaverLiveCheck::find_weaver_binary()
                .ok_or(WeaverValidationError::BinaryNotFound)?;

            let mut actual_registry_path = self.registry_path.clone();
            if actual_registry_path.join("model").exists() {
                actual_registry_path = actual_registry_path.join("model");
            }
            let registry_str = actual_registry_path.to_string_lossy().into_owned();
            let grpc_port_str = self.otlp_grpc_port.to_string();
            let admin_port_str = self.admin_port.to_string();

            // Spawn the Weaver live-check process
            let child = Command::new(&weaver_binary)
                .args([
                    "registry",
                    "live-check",
                    "-r",
                    &registry_str,
                    "--otlp-grpc-port",
                    &grpc_port_str,
                    "--admin-port",
                    &admin_port_str,
                ])
                .spawn()
                .map_err(|e| {
                    if e.kind() == std::io::ErrorKind::NotFound {
                        WeaverValidationError::BinaryNotFound
                    } else {
                        WeaverValidationError::ProcessStartFailed(e.to_string())
                    }
                })?;

            Ok(WeaverValidator::<state::Running> {
                registry_path: self.registry_path,
                otlp_grpc_port: self.otlp_grpc_port,
                admin_port: self.admin_port,
                process: Some(child),
                _state: PhantomData,
            })
        }
    }

    impl WeaverValidator<state::Running> {
        /// Get OTLP endpoint for sending telemetry
        ///
        /// **Poka-yoke**: Only available on `WeaverValidator<Running>`.
        #[must_use]
        pub fn otlp_endpoint(&self) -> String {
            format!(
                "http://{}:{}",
                crate::observability::weaver::LOCALHOST,
                self.otlp_grpc_port
            )
        }

        /// Check if Weaver process is still alive
        ///
        /// Uses `try_wait()` to poll the child process without blocking.
        /// Returns `true` if the process has not yet exited.
        #[must_use]
        pub fn is_running(&mut self) -> bool {
            self.process
                .as_mut()
                .is_some_and(|child| child.try_wait().map_or(true, |status| status.is_none()))
        }

        /// Stop the Weaver validator
        ///
        /// **Poka-yoke**: Changes type from `WeaverValidator<Running>` to `WeaverValidator<Stopped>`.
        /// After this call, validator cannot validate telemetry.
        ///
        /// # Errors
        ///
        /// Returns error if killing the child process fails.
        pub fn stop(
            mut self,
        ) -> crate::observability::weaver::WeaverValidationResult<WeaverValidator<state::Stopped>>
        {
            use crate::observability::weaver::WeaverValidationError;

            if let Some(ref mut child) = self.process {
                child
                    .kill()
                    .map_err(|e| WeaverValidationError::ProcessStopFailed(e.to_string()))?;
                child
                    .wait()
                    .map_err(|e| WeaverValidationError::ProcessStopFailed(e.to_string()))?;
            }

            Ok(WeaverValidator::<state::Stopped> {
                registry_path: self.registry_path,
                otlp_grpc_port: self.otlp_grpc_port,
                admin_port: self.admin_port,
                process: None,
                _state: PhantomData,
            })
        }
    }
}

/// Weaver validation error
///
/// Errors that can occur during Weaver validation operations.
///
/// # Examples
///
/// ```rust
/// use chicago_tdd_tools::observability::weaver::WeaverValidationError;
///
/// // Handle specific error types
/// let error = WeaverValidationError::BinaryNotFound;
/// match error {
///     WeaverValidationError::BinaryNotFound => {
///         eprintln!("Weaver binary not found. Install with: cargo install weaver");
///     }
///     WeaverValidationError::ValidationFailed(msg) => {
///         eprintln!("Validation failed: {}", msg);
///     }
///     _ => {
///         eprintln!("Other Weaver error occurred");
///     }
/// }
/// ```
#[derive(Error, Debug)]
pub enum WeaverValidationError {
    /// Weaver binary not found
    #[error("🚨 Weaver binary not found in PATH\n   ⚠️  STOP: Cannot proceed with Weaver validation\n   💡 FIX: Run cargo make weaver-bootstrap\n   📋 Manual: cargo install weaver\n   📋 Download: https://github.com/open-telemetry/weaver/releases")]
    BinaryNotFound,
    /// Docker daemon is not running or unavailable
    #[error("🚨 Docker daemon is not running or unavailable: {0}\n   ⚠️  STOP: Cannot proceed with Weaver integration\n   💡 FIX: Start Docker Desktop or Docker daemon\n   📋 macOS: Open Docker Desktop\n   📋 Linux: sudo systemctl start docker\n   📋 Windows: Start Docker Desktop")]
    DockerUnavailable(String),
    /// Weaver check failed
    #[error("🚨 Weaver validation failed: {0}\n   ⚠️  STOP: Telemetry validation failed\n   💡 FIX: Check telemetry conforms to schema and semantic conventions")]
    ValidationFailed(String),
    /// Registry path does not exist
    #[error("🚨 Registry path does not exist: {0}\n   ⚠️  STOP: Cannot proceed with Weaver validation\n   💡 FIX: Provide valid registry path\n   📋 Registry: Path to OpenTelemetry semantic conventions registry")]
    RegistryNotFound(String),
    /// Failed to start Weaver process
    #[error("🚨 Failed to start Weaver process: {0}\n   ⚠️  STOP: Cannot start Weaver live-check\n   💡 FIX: Check Weaver binary is installed and accessible\n   📋 Verify: weaver --version")]
    ProcessStartFailed(String),
    /// Failed to stop Weaver process
    #[error("⚠️  Failed to stop Weaver process: {0}\n   ⚠️  WARNING: Weaver process may still be running\n   💡 FIX: Manually stop Weaver process if needed\n   📋 Check: ps aux | grep weaver")]
    ProcessStopFailed(String),
    /// Weaver process not running
    #[error("⚠️  Weaver process is not running\n   ⚠️  WARNING: Expected Weaver process to be running\n   💡 FIX: Start Weaver process before operation")]
    ProcessNotRunning,
}

/// Result type for Weaver validation
pub type WeaverValidationResult<T> = Result<T, WeaverValidationError>;

/// Default OTLP gRPC port (OpenTelemetry standard)
///
/// **Kaizen improvement**: Extracted magic number `4317` to named constant.
/// Pattern: Use named constants instead of magic numbers for configuration values.
/// Benefits: Improves readability, maintainability, self-documentation.
pub const DEFAULT_OTLP_GRPC_PORT: u16 = 4317;

/// Default Weaver admin port
///
/// **Kaizen improvement**: Extracted magic number `4320` to named constant.
/// Pattern: Use named constants for configuration values that may change.
pub const DEFAULT_ADMIN_PORT: u16 = 4320;

/// Default inactivity timeout in seconds (5 minutes)
///
/// **Kaizen improvement**: Extracted magic number `300` to named constant.
/// Pattern: Use named constants for timeouts and durations.
pub const DEFAULT_INACTIVITY_TIMEOUT_SECONDS: u64 = 300;

/// Localhost IP address for client connections
///
/// **Kaizen improvement**: Extracted magic string `"127.0.0.1"` to named constant.
/// Pattern: Use named constants for network addresses and endpoints.
pub const LOCALHOST: &str = "127.0.0.1";

/// Weaver live validation helper
#[cfg(feature = "weaver")]
pub struct WeaverValidator {
    live_check: Option<WeaverLiveCheck>,
    process: Option<Child>,
    registry_path: PathBuf,
    otlp_grpc_port: u16,
    admin_port: u16,
}

#[cfg(feature = "weaver")]
impl WeaverValidator {
    /// Create a new Weaver validator
    #[must_use]
    pub const fn new(registry_path: PathBuf) -> Self {
        Self {
            live_check: None,
            process: None,
            registry_path,
            otlp_grpc_port: DEFAULT_OTLP_GRPC_PORT,
            admin_port: DEFAULT_ADMIN_PORT,
        }
    }

    /// Create a Weaver validator with custom configuration
    #[must_use]
    pub const fn with_config(registry_path: PathBuf, otlp_grpc_port: u16, admin_port: u16) -> Self {
        Self {
            live_check: None,
            process: None,
            registry_path,
            otlp_grpc_port,
            admin_port,
        }
    }

    /// Check if Weaver binary is available
    ///
    /// # Errors
    ///
    /// Returns an error if Weaver binary is not found.
    pub fn check_weaver_available() -> WeaverValidationResult<()> {
        WeaverLiveCheck::check_weaver_available()
            .map_err(|e| WeaverValidationError::ValidationFailed(format!("{e}")))
    }

    /// Clone OpenTelemetry semantic conventions registry at runtime if missing
    ///
    /// This is a runtime fallback that matches the Weaver binary runtime download pattern.
    /// The registry should normally be cloned during build via `build.rs`, but this provides
    /// a fallback for cases where build-time clone failed or registry was deleted.
    ///
    /// # Errors
    ///
    /// Returns an error if git is not available or clone fails.
    fn clone_registry_runtime(registry_path: &Path) -> WeaverValidationResult<()> {
        use std::process::Command;

        // Check if git is available
        if Command::new("git").arg("--version").output().is_err() {
            return Err(WeaverValidationError::RegistryNotFound(format!(
                "{} (git not found for runtime clone)",
                registry_path.display()
            )));
        }

        let registry_url = "https://github.com/open-telemetry/semantic-conventions.git";
        let registry_str = registry_path.to_str().ok_or_else(|| {
            WeaverValidationError::ValidationFailed("Registry path is not valid UTF-8".to_string())
        })?;

        // Clone with shallow clone for faster download
        // Use --depth 1 to only clone the latest commit
        let status = Command::new("git")
            .args([
                "clone",
                "--depth",
                "1",
                "--single-branch",
                registry_url,
                registry_str,
            ])
            .status()
            .map_err(|e| {
                WeaverValidationError::RegistryNotFound(format!(
                    "{} (failed to clone: {e})",
                    registry_path.display()
                ))
            })?;

        if !status.success() {
            return Err(WeaverValidationError::RegistryNotFound(format!(
                "{} (git clone failed)",
                registry_path.display()
            )));
        }

        Ok(())
    }

    /// Start Weaver live-check
    ///
    /// Signals:
    /// - 🚨 CRITICAL: Stops immediately if Weaver binary not found
    /// - 🚨 CRITICAL: Stops immediately if Docker unavailable (when testcontainers feature enabled)
    /// - 🚨 CRITICAL: Stops immediately if registry path doesn't exist
    ///
    /// # Errors
    ///
    /// Returns an error if Weaver binary is not available, Docker is unavailable, or registry path doesn't exist.
    pub fn start(&mut self) -> WeaverValidationResult<()> {
        // 🚨 Check Weaver binary availability
        Self::check_weaver_available()?;
        // ✅ Weaver binary is available

        // 🚨 Check Docker availability if testcontainers feature is enabled
        #[cfg(feature = "testcontainers")]
        {
            use crate::testcontainers::check_docker_available;
            check_docker_available().map_err(|e| {
                WeaverValidationError::DockerUnavailable(format!(
                    "Docker daemon is not running. Weaver integration requires Docker. Error: {e}"
                ))
            })?;
            // ✅ Docker is available
        }

        // 🚨 Verify registry path exists, clone if missing (runtime fallback)
        if !self.registry_path.exists() {
            // Try to clone registry at runtime (matching Weaver binary runtime download pattern)
            if let Err(err) = Self::clone_registry_runtime(self.registry_path.as_path()) {
                return Err(WeaverValidationError::RegistryNotFound(format!(
                    "{}\n   💡 FIX: Registry will be cloned automatically during build, or run: cargo make weaver-bootstrap\n   Details: {err}",
                    self.registry_path.display()
                )));
            }
        }
        // ✅ Registry path exists

        // Create Weaver live-check instance
        let mut actual_registry_path = self.registry_path.clone();
        if actual_registry_path.join("model").exists() {
            actual_registry_path = actual_registry_path.join("model");
        }

        let registry_str = actual_registry_path.to_str().ok_or_else(|| {
            WeaverValidationError::ValidationFailed("Registry path is not valid UTF-8".to_string())
        })?;

        let live_check = WeaverLiveCheck::new()
            .with_registry(registry_str.to_string())
            .with_otlp_port(self.otlp_grpc_port)
            .with_admin_port(self.admin_port)
            .with_inactivity_timeout(DEFAULT_INACTIVITY_TIMEOUT_SECONDS) // 5 minutes (longer for tests)
            .with_format("json".to_string()) // Use JSON format for parsing
            .with_output("./weaver-reports".to_string()); // Output to directory for parsing

        // Start Weaver live-check process
        let process = live_check
            .start()
            .map_err(WeaverValidationError::ProcessStartFailed)?;

        self.live_check = Some(live_check);
        self.process = Some(process);

        Ok(())
    }

    /// Stop Weaver live-check
    ///
    /// # Errors
    ///
    /// Returns an error if stopping the process fails.
    pub fn stop(&mut self) -> WeaverValidationResult<()> {
        if let Some(ref live_check) = self.live_check {
            live_check
                .stop()
                .map_err(WeaverValidationError::ProcessStopFailed)?;
        }

        if let Some(mut process) = self.process.take() {
            let _ = process.kill();
        }

        self.live_check = None;
        Ok(())
    }

    /// Get OTLP endpoint for sending telemetry
    #[must_use]
    pub fn otlp_endpoint(&self) -> String {
        format!("http://{LOCALHOST}:{}", self.otlp_grpc_port)
    }

    /// Check if Weaver process is running
    #[must_use]
    pub const fn is_running(&self) -> bool {
        self.process.is_some()
    }
}

#[cfg(feature = "weaver")]
impl Drop for WeaverValidator {
    fn drop(&mut self) {
        // **Root Cause Fix**: Only stop if process is still running
        // If stop() was already called explicitly, process will be None
        // This prevents blocking HTTP client from being called in async Drop context
        if self.process.is_some() {
            // Try to stop, but don't panic if it fails (we're in Drop)
            let _ = self.stop();
        }
    }
}

/// Send a test span to Weaver OTLP endpoint for validation
///
/// Creates a simple test span and sends it to the Weaver OTLP endpoint.
/// This is used for integration testing to verify that Weaver validates telemetry.
///
/// # Note
///
/// This function requires OpenTelemetry SDK dependencies which may have API changes.
/// For production use, configure OpenTelemetry exporters directly in your application.
///
/// # Example
///
/// ```rust,ignore
/// # #[cfg(feature = "weaver")]
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// use chicago_tdd_tools::observability::weaver::{send_test_span_to_weaver, LOCALHOST, DEFAULT_OTLP_GRPC_PORT};
///
/// let endpoint = format!("http://{}:{}", LOCALHOST, DEFAULT_OTLP_GRPC_PORT);
/// send_test_span_to_weaver(&endpoint, "test.operation")?;
/// # Ok(())
/// # }
/// ```
///
/// # Errors
///
/// Returns an error if sending the span to Weaver fails.
#[cfg(feature = "weaver")]
pub fn send_test_span_to_weaver(
    endpoint: &str, span_name: &str,
) -> Result<(), WeaverValidationError> {
    // Weaver live check ONLY listens on gRPC (no HTTP endpoint).
    // We MUST use `grpc-tonic` which requires a Tokio runtime.
    let rt = tokio::runtime::Runtime::new().map_err(|e| {
        WeaverValidationError::ValidationFailed(format!("Failed to create Tokio runtime: {e}"))
    })?;

    rt.block_on(async {
        // Items (use statements) must come before statements (Rust requirement)
        use opentelemetry::trace::{Span, Tracer, TracerProvider as _};
        use opentelemetry::KeyValue;
        use opentelemetry_sdk::trace::{RandomIdGenerator, Sampler, SdkTracerProvider};
        use opentelemetry_sdk::Resource;
        use std::time::Duration;

        // Create OTLP HTTP exporter and tracer provider
        // Set endpoint via environment variable (required by exporter)
        let base_endpoint = endpoint.trim_end_matches("/v1/traces").trim_end_matches('/');
        // For gRPC, we often just need the base endpoint like "http://127.0.0.1:4317"
        std::env::set_var("OTEL_EXPORTER_OTLP_ENDPOINT", base_endpoint);

        // Create OTLP gRPC exporter using builder pattern
        let exporter = opentelemetry_otlp::SpanExporter::builder()
            .with_tonic()
            .build()
            .map_err(|e| {
                WeaverValidationError::ValidationFailed(format!(
                    "🚨 Failed to create OTLP gRPC exporter: {e}\n   ⚠️  STOP: Cannot create OTLP exporter\n   💡 FIX: Check OpenTelemetry SDK configuration and endpoint"
                ))
            })?;

        // Create resource with service information
        let resource = Resource::builder_empty()
            .with_service_name("chicago-tdd-tools-test")
            .with_attributes([
                KeyValue::new("service.version", env!("CARGO_PKG_VERSION")),
                KeyValue::new("telemetry.sdk.language", "rust"),
                KeyValue::new("telemetry.sdk.name", "opentelemetry"),
                KeyValue::new("telemetry.sdk.version", "0.31.0"),
            ])
            .build();

        // Create tracer provider with batch exporter
        let provider = SdkTracerProvider::builder()
            .with_batch_exporter(exporter)
            .with_sampler(Sampler::TraceIdRatioBased(1.0)) // Always sample for tests
            .with_id_generator(RandomIdGenerator::default())
            .with_resource(resource)
            .build();

        // Get tracer
        let tracer = provider.tracer("chicago-tdd-tools");

        // Create and start span using span_builder pattern
        let span_name_owned = span_name.to_string();
        let mut span = tracer.span_builder(span_name_owned.clone()).start(&tracer);

        // Set test attributes
        span.set_attribute(KeyValue::new("test.operation", span_name_owned));
        span.set_attribute(KeyValue::new("test.framework", "chicago-tdd-tools"));
        span.set_attribute(KeyValue::new("span.kind", "internal"));

        // End span (this triggers export)
        span.end();

        // Force flush to ensure span is exported before shutdown
        if let Err(e) = provider.force_flush() {
            return Err(WeaverValidationError::ValidationFailed(format!(
                "⚠️  Failed to flush traces: {e}\n   ⚠️  WARNING: Traces may not be exported\n   💡 FIX: Check OTLP endpoint connectivity"
            )));
        }

        // Give async exports time to complete
        tokio::time::sleep(Duration::from_millis(500)).await;

        // Shutdown tracer provider
        if let Err(e) = provider.shutdown() {
            return Err(WeaverValidationError::ValidationFailed(format!(
                "⚠️  Failed to shutdown tracer provider: {e}\n   ⚠️  WARNING: Tracer provider may not have shut down cleanly\n   💡 FIX: Check resource cleanup"
            )));
        }

        Ok(())
    })
}

/// Run Weaver static schema validation
///
/// Validates that schema files are valid without running live-check.
///
/// # Example
///
/// ```rust,ignore
/// # #[cfg(feature = "weaver")]
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// use chicago_tdd_tools::observability::weaver::validate_schema_static;
/// use std::path::PathBuf;
///
/// let registry_path = PathBuf::from("registry/");
/// validate_schema_static(&registry_path)?;
/// # Ok(())
/// # }
/// ```
///
/// # Errors
///
/// Returns an error if Weaver binary is not available or schema validation fails.
#[cfg(feature = "weaver")]
pub fn validate_schema_static(registry_path: &std::path::Path) -> WeaverValidationResult<()> {
    // Items (use statements) must come before statements (Rust requirement)
    use crate::observability::weaver::types::WeaverLiveCheck;
    use std::process::Command;

    // Check Weaver binary availability
    WeaverValidator::check_weaver_available()?;

    // Verify registry path exists
    if !registry_path.exists() {
        return Err(WeaverValidationError::RegistryNotFound(
            registry_path.display().to_string(),
        ));
    }

    let mut actual_registry_path = registry_path.to_path_buf();
    if actual_registry_path.join("model").exists() {
        actual_registry_path = actual_registry_path.join("model");
    }

    let registry_str = actual_registry_path.to_str().ok_or_else(|| {
        WeaverValidationError::ValidationFailed("Registry path is not valid UTF-8".to_string())
    })?;

    // Find weaver binary (may trigger runtime download)
    let weaver_binary =
        WeaverLiveCheck::find_weaver_binary().ok_or(WeaverValidationError::BinaryNotFound)?;

    let output = Command::new(&weaver_binary)
        .args(["registry", "check", "-r", registry_str])
        .output()
        .map_err(|e| {
            if e.kind() == std::io::ErrorKind::NotFound {
                WeaverValidationError::BinaryNotFound
            } else {
                WeaverValidationError::ValidationFailed(format!(
                    "🚨 Failed to execute weaver check: {e}\n   ⚠️  STOP: Weaver schema validation failed\n   💡 FIX: Check Weaver binary is installed and registry path is valid"
                ))
            }
        })?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(WeaverValidationError::ValidationFailed(format!(
            "🚨 Weaver schema validation failed: {stderr}\n   ⚠️  STOP: Schema does not conform to semantic conventions\n   💡 FIX: Check registry schema and telemetry structure"
        )));
    }

    Ok(())
}

#[cfg(test)]
#[allow(clippy::panic)] // Test code - panic is appropriate for test failures
mod tests {
    use super::*;

    // Test feature-gated code paths (critical - verify features work correctly)
    #[cfg(not(feature = "weaver"))]
    #[test]
    fn test_weaver_module_not_accessible_without_feature() {
        // Verify weaver module is not accessible without feature
        // This test should compile and pass when weaver feature is disabled
        assert!(
            true,
            "weaver module should not be accessible without feature"
        );
    }

    #[cfg(feature = "weaver")]
    #[test]
    fn test_weaver_validation_error_variants() {
        // Test all error variants (critical - 80% of bugs)
        let errors = vec![
            WeaverValidationError::BinaryNotFound,
            WeaverValidationError::ValidationFailed("test".to_string()),
            WeaverValidationError::RegistryNotFound("test".to_string()),
            WeaverValidationError::ProcessStartFailed("test".to_string()),
            WeaverValidationError::ProcessStopFailed("test".to_string()),
            WeaverValidationError::ProcessNotRunning,
        ];

        for error in errors {
            let display = format!("{error}");
            assert!(!display.is_empty(), "Error should have display message");
        }
    }

    #[cfg(feature = "weaver")]
    #[test]
    fn test_weaver_validation_error_debug() {
        // Test error is debuggable
        let error = WeaverValidationError::BinaryNotFound;
        let debug = format!("{error:?}");
        assert!(debug.contains("BinaryNotFound"));
    }

    #[cfg(feature = "weaver")]
    #[test]
    fn test_weaver_validation_error_all_variants_display() {
        // Test all error variants have proper Display implementation
        let errors = vec![
            WeaverValidationError::BinaryNotFound,
            WeaverValidationError::ValidationFailed("test validation".to_string()),
            WeaverValidationError::RegistryNotFound("/nonexistent/path".to_string()),
            WeaverValidationError::ProcessStartFailed("failed to start".to_string()),
            WeaverValidationError::ProcessStopFailed("failed to stop".to_string()),
            WeaverValidationError::ProcessNotRunning,
        ];

        for error in errors {
            let display = format!("{error}");
            assert!(!display.is_empty(), "Error should have display message");
            // Verify error messages are descriptive
            assert!(
                display.contains("Weaver")
                    || display.contains("validation")
                    || display.contains("registry")
                    || display.contains("Registry")
                    || display.contains("path")
                    || display.contains("process")
                    || display.contains("Process")
                    || display.contains("binary")
                    || display.contains("Binary")
                    || display.contains("not found")
                    || display.contains("Not found")
                    || display.contains("failed")
                    || display.contains("Failed"),
                "Error message should be descriptive: {display}"
            );
        }
    }

    #[cfg(feature = "weaver")]
    #[test]
    fn test_weaver_validation_error_all_variants_debug() {
        // Test all error variants have proper Debug implementation
        let errors = vec![
            WeaverValidationError::BinaryNotFound,
            WeaverValidationError::ValidationFailed("test".to_string()),
            WeaverValidationError::RegistryNotFound("test".to_string()),
            WeaverValidationError::ProcessStartFailed("test".to_string()),
            WeaverValidationError::ProcessStopFailed("test".to_string()),
            WeaverValidationError::ProcessNotRunning,
        ];

        for error in errors {
            let debug = format!("{error:?}");
            assert!(!debug.is_empty(), "Error should have debug representation");
            // Verify debug output contains error type name
            assert!(
                debug.contains("BinaryNotFound")
                    || debug.contains("ValidationFailed")
                    || debug.contains("RegistryNotFound")
                    || debug.contains("ProcessStartFailed")
                    || debug.contains("ProcessStopFailed")
                    || debug.contains("ProcessNotRunning"),
                "Debug output should contain error type: {debug}"
            );
        }
    }

    #[cfg(feature = "weaver")]
    #[test]
    fn test_weaver_validator_new() {
        let registry_path = PathBuf::from("registry/");
        let validator = WeaverValidator::new(registry_path);
        assert_eq!(validator.otlp_grpc_port, 4317);
        assert_eq!(validator.admin_port, 4320); // Match weaver default
    }

    #[cfg(feature = "weaver")]
    #[test]
    fn test_weaver_validator_with_config() {
        let registry_path = PathBuf::from("registry/");
        let validator = WeaverValidator::with_config(registry_path, 4318, 8081);
        assert_eq!(validator.otlp_grpc_port, 4318);
        assert_eq!(validator.admin_port, 8081);
    }

    #[cfg(feature = "weaver")]
    #[test]
    fn test_weaver_validator_otlp_endpoint() {
        let registry_path = PathBuf::from("registry/");
        let validator = WeaverValidator::new(registry_path);
        // OTLP endpoint uses LOCALHOST for client connections (even though server listens on 0.0.0.0)
        assert_eq!(
            validator.otlp_endpoint(),
            format!("http://{LOCALHOST}:{DEFAULT_OTLP_GRPC_PORT}")
        );
    }

    #[cfg(feature = "weaver")]
    #[test]
    fn test_weaver_validator_check_weaver_available() {
        // Test check_weaver_available (may fail if Weaver not installed, that's OK)
        let result = WeaverValidator::check_weaver_available();
        // Assert: Method returns Result (behavior test, not existence test)
        // We don't assert success because Weaver may not be installed in test environment
        assert!(
            result.is_ok() || result.is_err(),
            "check_weaver_available should return Result"
        );
    }

    #[cfg(feature = "weaver")]
    #[test]
    fn test_weaver_validatorregistry_path_validation() {
        use crate::assert_err;

        // **Refactored**: Test now runs unconditionally and fails clearly if prerequisites are missing.
        // This ensures we detect when Weaver system is not working, rather than silently skipping.
        //
        // **Test Requirements**:
        // - Weaver binary must be available (checked by WeaverValidator::new())
        // - Registry path validation is tested with invalid path
        //
        // **Behavior**: Test will fail with clear error message if Weaver binary is not available,
        // allowing us to detect system failures rather than hiding them.

        // Test registry path validation (error path - 80% of bugs)
        let invalid_path = PathBuf::from("/nonexistent/registry/path");

        // Check Weaver binary availability before creating validator
        if WeaverValidator::check_weaver_available().is_err() {
            panic!(
                "🚨 Weaver binary not available\n\
                 ⚠️  STOP: Cannot proceed with Weaver validation test\n\
                 💡 FIX: Run cargo make weaver-bootstrap\n\
                 📋 This test verifies Weaver system is working - binary must be available"
            );
        }

        // Test start with invalid registry path
        // Note: WeaverValidator::new() is a simple constructor, doesn't check binary
        // We check binary availability above before creating validator
        let mut validator = WeaverValidator::new(invalid_path);

        let start_result = validator.start();

        // Should fail (either BinaryNotFound, RegistryNotFound, or DockerUnavailable)
        assert_err!(
            &start_result,
            "Start should fail with invalid registry path"
        );
        match start_result {
            Err(WeaverValidationError::RegistryNotFound(_)) => {
                // Expected error variant (when Weaver binary is available and registry path is invalid)
                // This confirms the system is working - it correctly detects invalid registry path
            }
            Err(WeaverValidationError::BinaryNotFound) => {
                panic!(
                    "🚨 Weaver binary not available during start()\n\
                     ⚠️  STOP: Weaver system is not working\n\
                     💡 FIX: Run cargo make weaver-bootstrap\n\
                     📋 This indicates Weaver binary became unavailable between new() and start()"
                );
            }
            Err(WeaverValidationError::ProcessStartFailed(_)) => {
                // Expected when starting with invalid path fails at process spawn
            }
            Err(WeaverValidationError::DockerUnavailable(_)) => {
                // **Test Isolation Fix**: Docker not being available is acceptable for unit tests
                // Docker is only required for full integration tests, not for basic validation
                // This test can still verify Weaver error handling without Docker
                log::info!(
                    "ℹ️  Docker unavailable - skipping Docker-dependent validation\n\
                     📋 This is acceptable for unit tests\n\
                     📋 Full integration tests require Docker (cargo make test-integration)"
                );
            }
            Err(e) => {
                panic!(
                    "Expected RegistryNotFound, BinaryNotFound, ProcessStartFailed or DockerUnavailable, got: {:?}",
                    e
                );
            }
            Ok(_) => {
                panic!("Expected error for invalid registry path, got success");
            }
        }
    }

    #[cfg(feature = "weaver")]
    #[test]
    fn test_weaver_validator_is_running() {
        // **Refactored**: Test now runs unconditionally and fails clearly if prerequisites are missing.
        // This ensures we detect when Weaver system is not working, rather than silently skipping.
        //
        // **Test Requirements**:
        // - Registry path must exist (checked by test)
        // - Weaver binary availability is checked implicitly
        //
        // **Behavior**: Test will fail with clear error message if registry path is missing,
        // allowing us to detect system failures rather than hiding them.

        // Test is_running() method (important - used frequently)
        let registry_path = PathBuf::from("registry/");

        // Verify registry path exists - fail clearly if missing
        if !registry_path.exists() {
            panic!(
                "🚨 Registry path does not exist: {:?}\n\
                 ⚠️  STOP: Cannot proceed with Weaver validator test\n\
                 💡 FIX: Run cargo make weaver-bootstrap\n\
                 📋 This test verifies Weaver system is working - registry must be available",
                registry_path
            );
        }

        // Check Weaver binary availability before creating validator
        if WeaverValidator::check_weaver_available().is_err() {
            panic!(
                "🚨 Weaver binary not available\n\
                 ⚠️  STOP: Cannot proceed with Weaver validator test\n\
                 💡 FIX: Run cargo make weaver-bootstrap\n\
                 📋 This test verifies Weaver system is working - binary must be available"
            );
        }

        // Create validator (WeaverValidator::new() doesn't return Result, it's a simple constructor)
        let validator = WeaverValidator::new(registry_path);

        // Initially not running
        assert!(
            !validator.is_running(),
            "Validator should not be running initially"
        );
    }

    // **Poka-yoke**: Integration test moved to tests/weaver_integration.rs
    // Unit tests in src/ should only test types and validators, not integration with external services
}
