# Validation Framework Implementation Design

## Overview

This document provides the detailed Rust implementation design for the multi-layered validation framework. The design follows Rust best practices with strong typing, error handling, and async support.

---

## Module Structure

```
src/validation/
├── mod.rs                      # Module root and exports
├── traits.rs                   # Core traits (Validator, AsyncValidator)
├── types.rs                    # Result types and reports
├── registry.rs                 # ValidationRegistry
├── pre_test/                   # Layer 1: Pre-Test Validation
│   ├── mod.rs
│   ├── daemon_check.rs         # Docker daemon health
│   ├── socket_check.rs         # Socket accessibility
│   ├── resource_check.rs       # Resource availability
│   └── network_check.rs        # Network availability
├── runtime/                    # Layer 2: Runtime Monitoring
│   ├── mod.rs
│   ├── container_tracker.rs   # Container creation tracking
│   ├── port_monitor.rs        # Port binding monitoring
│   ├── resource_monitor.rs    # Resource usage tracking
│   └── api_interceptor.rs     # Docker API interception
├── post_test/                  # Layer 3: Post-Test Validation
│   ├── mod.rs
│   ├── lifecycle_verifier.rs  # Container lifecycle
│   ├── log_analyzer.rs        # Log file analysis
│   ├── cleanup_verifier.rs    # Cleanup verification
│   └── leak_detector.rs       # Resource leak detection
├── service_level/              # Layer 4: Service-Level Validation
│   ├── mod.rs
│   ├── connection_tester.rs   # Database connections
│   ├── operation_tester.rs    # Service operations
│   ├── persistence_tester.rs  # Data persistence
│   └── performance_validator.rs # Performance characteristics
└── negative/                   # Layer 5: Negative Testing
    ├── mod.rs
    ├── fail_case_tester.rs    # Fail-case validation
    ├── error_checker.rs       # Error message verification
    ├── degradation_tester.rs  # Graceful degradation
    └── retry_validator.rs     # Retry logic validation
```

---

## Core Traits and Types

### src/validation/traits.rs

```rust
use async_trait::async_trait;
use crate::validation::types::ValidationResult;

/// Core validator trait for synchronous validation
pub trait Validator: Send + Sync {
    /// Get validator name for reporting
    fn name(&self) -> &str;

    /// Perform validation
    fn validate(&self) -> ValidationResult;

    /// Optional: Get validator description
    fn description(&self) -> Option<&str> {
        None
    }

    /// Optional: Check if validator is enabled
    fn is_enabled(&self) -> bool {
        true
    }
}

/// Async validator trait for asynchronous validation
#[async_trait]
pub trait AsyncValidator: Send + Sync {
    /// Get validator name for reporting
    fn name(&self) -> &str;

    /// Perform async validation
    async fn validate(&self) -> ValidationResult;

    /// Optional: Get validator description
    fn description(&self) -> Option<&str> {
        None
    }

    /// Optional: Check if validator is enabled
    fn is_enabled(&self) -> bool {
        true
    }
}

/// Validator that can be configured
pub trait ConfigurableValidator: Validator {
    type Config;

    /// Create validator with configuration
    fn with_config(config: Self::Config) -> anyhow::Result<Self>
    where
        Self: Sized;
}
```

### src/validation/types.rs

```rust
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::Duration;

/// Result of a validation check
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ValidationResult {
    /// Validation passed
    Success,
    /// Validation passed with warnings
    Warning { message: String, details: Option<String> },
    /// Validation failed
    Failure { message: String, details: Option<String>, error: Option<String> },
}

impl ValidationResult {
    /// Create success result
    pub fn success() -> Self {
        ValidationResult::Success
    }

    /// Create warning result
    pub fn warning(message: impl Into<String>) -> Self {
        ValidationResult::Warning {
            message: message.into(),
            details: None,
        }
    }

    /// Create warning with details
    pub fn warning_with_details(message: impl Into<String>, details: impl Into<String>) -> Self {
        ValidationResult::Warning {
            message: message.into(),
            details: Some(details.into()),
        }
    }

    /// Create failure result
    pub fn failure(message: impl Into<String>) -> Self {
        ValidationResult::Failure {
            message: message.into(),
            details: None,
            error: None,
        }
    }

    /// Create failure with details
    pub fn failure_with_details(message: impl Into<String>, details: impl Into<String>) -> Self {
        ValidationResult::Failure {
            message: message.into(),
            details: Some(details.into()),
            error: None,
        }
    }

    /// Create failure from error
    pub fn from_error(message: impl Into<String>, error: &dyn std::error::Error) -> Self {
        ValidationResult::Failure {
            message: message.into(),
            details: None,
            error: Some(error.to_string()),
        }
    }

    /// Check if validation succeeded
    pub fn is_success(&self) -> bool {
        matches!(self, ValidationResult::Success)
    }

    /// Check if validation has warnings
    pub fn is_warning(&self) -> bool {
        matches!(self, ValidationResult::Warning { .. })
    }

    /// Check if validation failed
    pub fn is_failure(&self) -> bool {
        matches!(self, ValidationResult::Failure { .. })
    }

    /// Assert success (panic on failure)
    pub fn assert_success(&self) {
        match self {
            ValidationResult::Success => {}
            ValidationResult::Warning { message, .. } => {
                eprintln!("Warning: {}", message);
            }
            ValidationResult::Failure { message, details, error } => {
                let mut msg = format!("Validation failed: {}", message);
                if let Some(d) = details {
                    msg.push_str(&format!("\nDetails: {}", d));
                }
                if let Some(e) = error {
                    msg.push_str(&format!("\nError: {}", e));
                }
                panic!("{}", msg);
            }
        }
    }

    /// Aggregate multiple results
    pub fn aggregate(results: Vec<ValidationResult>) -> ValidationResult {
        let mut warnings = Vec::new();

        for result in results {
            match result {
                ValidationResult::Failure { .. } => return result,
                ValidationResult::Warning { message, .. } => warnings.push(message),
                ValidationResult::Success => {}
            }
        }

        if warnings.is_empty() {
            ValidationResult::Success
        } else {
            ValidationResult::warning(warnings.join("; "))
        }
    }
}

/// Comprehensive validation report
#[derive(Debug, Serialize, Deserialize)]
pub struct ValidationReport {
    /// Results by validator name
    pub results: HashMap<String, ValidationResult>,
    /// Timestamp when report was generated
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Total validation duration
    pub duration: Duration,
    /// Additional metadata
    pub metadata: HashMap<String, String>,
}

impl ValidationReport {
    /// Create new validation report
    pub fn new() -> Self {
        Self {
            results: HashMap::new(),
            timestamp: chrono::Utc::now(),
            duration: Duration::from_secs(0),
            metadata: HashMap::new(),
        }
    }

    /// Add validation result
    pub fn add_result(&mut self, name: impl Into<String>, result: ValidationResult) {
        self.results.insert(name.into(), result);
    }

    /// Add metadata
    pub fn add_metadata(&mut self, key: impl Into<String>, value: impl Into<String>) {
        self.metadata.insert(key.into(), value.into());
    }

    /// Set duration
    pub fn set_duration(&mut self, duration: Duration) {
        self.duration = duration;
    }

    /// Check if all validations passed
    pub fn all_passed(&self) -> bool {
        self.results.values().all(|r| r.is_success())
    }

    /// Check if any validations failed
    pub fn has_failures(&self) -> bool {
        self.results.values().any(|r| r.is_failure())
    }

    /// Get all failures
    pub fn failures(&self) -> Vec<(&str, &str)> {
        self.results
            .iter()
            .filter_map(|(name, result)| {
                if let ValidationResult::Failure { message, .. } = result {
                    Some((name.as_str(), message.as_str()))
                } else {
                    None
                }
            })
            .collect()
    }

    /// Get all warnings
    pub fn warnings(&self) -> Vec<(&str, &str)> {
        self.results
            .iter()
            .filter_map(|(name, result)| {
                if let ValidationResult::Warning { message, .. } = result {
                    Some((name.as_str(), message.as_str()))
                } else {
                    None
                }
            })
            .collect()
    }

    /// Get summary statistics
    pub fn summary(&self) -> ValidationSummary {
        let total = self.results.len();
        let passed = self.results.values().filter(|r| r.is_success()).count();
        let warnings = self.results.values().filter(|r| r.is_warning()).count();
        let failures = self.results.values().filter(|r| r.is_failure()).count();

        ValidationSummary {
            total,
            passed,
            warnings,
            failures,
        }
    }
}

impl Default for ValidationReport {
    fn default() -> Self {
        Self::new()
    }
}

/// Summary statistics for validation report
#[derive(Debug, Serialize, Deserialize)]
pub struct ValidationSummary {
    pub total: usize,
    pub passed: usize,
    pub warnings: usize,
    pub failures: usize,
}
```

---

## Validator Registry

### src/validation/registry.rs

```rust
use crate::validation::traits::{Validator, AsyncValidator};
use crate::validation::types::{ValidationReport, ValidationResult};
use std::sync::Arc;
use std::time::Instant;

/// Registry for managing validators
pub struct ValidationRegistry {
    /// Synchronous validators
    validators: Vec<Box<dyn Validator>>,
    /// Asynchronous validators
    async_validators: Vec<Arc<dyn AsyncValidator>>,
    /// Registry metadata
    metadata: HashMap<String, String>,
}

impl ValidationRegistry {
    /// Create new empty registry
    pub fn new() -> Self {
        Self {
            validators: Vec::new(),
            async_validators: Vec::new(),
            metadata: HashMap::new(),
        }
    }

    /// Add synchronous validator
    pub fn add_validator(&mut self, validator: Box<dyn Validator>) {
        self.validators.push(validator);
    }

    /// Add asynchronous validator
    pub fn add_async_validator(&mut self, validator: Arc<dyn AsyncValidator>) {
        self.async_validators.push(validator);
    }

    /// Add metadata
    pub fn add_metadata(&mut self, key: impl Into<String>, value: impl Into<String>) {
        self.metadata.insert(key.into(), value.into());
    }

    /// Validate all synchronous validators
    pub fn validate_all(&self) -> ValidationReport {
        let start = Instant::now();
        let mut report = ValidationReport::new();

        // Add registry metadata
        for (key, value) in &self.metadata {
            report.add_metadata(key, value);
        }

        // Run all validators
        for validator in &self.validators {
            if validator.is_enabled() {
                let result = validator.validate();
                report.add_result(validator.name(), result);
            }
        }

        report.set_duration(start.elapsed());
        report
    }

    /// Validate all asynchronous validators
    pub async fn validate_all_async(&self) -> ValidationReport {
        let start = Instant::now();
        let mut report = ValidationReport::new();

        // Add registry metadata
        for (key, value) in &self.metadata {
            report.add_metadata(key, value);
        }

        // Run all async validators
        for validator in &self.async_validators {
            if validator.is_enabled() {
                let result = validator.validate().await;
                report.add_result(validator.name(), result);
            }
        }

        report.set_duration(start.elapsed());
        report
    }

    /// Validate both sync and async validators
    pub async fn validate_all_combined(&self) -> ValidationReport {
        let start = Instant::now();
        let mut report = ValidationReport::new();

        // Add registry metadata
        for (key, value) in &self.metadata {
            report.add_metadata(key, value);
        }

        // Run all synchronous validators
        for validator in &self.validators {
            if validator.is_enabled() {
                let result = validator.validate();
                report.add_result(validator.name(), result);
            }
        }

        // Run all async validators
        for validator in &self.async_validators {
            if validator.is_enabled() {
                let result = validator.validate().await;
                report.add_result(validator.name(), result);
            }
        }

        report.set_duration(start.elapsed());
        report
    }

    /// Get count of registered validators
    pub fn validator_count(&self) -> usize {
        self.validators.len() + self.async_validators.len()
    }

    /// Get list of validator names
    pub fn validator_names(&self) -> Vec<String> {
        let mut names = Vec::new();

        for validator in &self.validators {
            names.push(validator.name().to_string());
        }

        for validator in &self.async_validators {
            names.push(validator.name().to_string());
        }

        names
    }
}

impl Default for ValidationRegistry {
    fn default() -> Self {
        Self::new()
    }
}
```

---

## Example Layer Implementation: Pre-Test Validation

### src/validation/pre_test/daemon_check.rs

```rust
use crate::validation::traits::Validator;
use crate::validation::types::ValidationResult;
use std::process::Command;
use std::time::{Duration, Instant};

/// Docker daemon health check validator
pub struct DockerDaemonCheck {
    /// Maximum time to wait for Docker response
    timeout: Duration,
}

impl DockerDaemonCheck {
    /// Create new daemon check with default timeout
    pub fn new() -> Self {
        Self {
            timeout: Duration::from_secs(2),
        }
    }

    /// Create with custom timeout
    pub fn with_timeout(timeout: Duration) -> Self {
        Self { timeout }
    }

    /// Check if Docker daemon is running
    fn check_daemon(&self) -> anyhow::Result<DaemonInfo> {
        let start = Instant::now();

        let output = Command::new("docker")
            .arg("info")
            .arg("--format")
            .arg("{{.ServerVersion}}")
            .output()
            .map_err(|e| anyhow::anyhow!("Failed to execute docker command: {}", e))?;

        let elapsed = start.elapsed();

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(anyhow::anyhow!("Docker daemon not running: {}", stderr));
        }

        let version = String::from_utf8(output.stdout)
            .map_err(|e| anyhow::anyhow!("Invalid UTF-8 in docker output: {}", e))?
            .trim()
            .to_string();

        if version.is_empty() {
            return Err(anyhow::anyhow!("Docker returned empty version"));
        }

        Ok(DaemonInfo {
            version,
            response_time: elapsed,
        })
    }
}

impl Validator for DockerDaemonCheck {
    fn name(&self) -> &str {
        "docker_daemon_check"
    }

    fn description(&self) -> Option<&str> {
        Some("Verifies Docker daemon is running and responsive")
    }

    fn validate(&self) -> ValidationResult {
        match self.check_daemon() {
            Ok(info) => {
                if info.response_time > self.timeout {
                    ValidationResult::warning_with_details(
                        "Docker daemon slow to respond",
                        format!(
                            "Response time: {:?} (timeout: {:?})",
                            info.response_time, self.timeout
                        ),
                    )
                } else {
                    ValidationResult::success()
                }
            }
            Err(e) => ValidationResult::from_error("Docker daemon check failed", &*e),
        }
    }
}

impl Default for DockerDaemonCheck {
    fn default() -> Self {
        Self::new()
    }
}

/// Information about Docker daemon
#[derive(Debug)]
struct DaemonInfo {
    version: String,
    response_time: Duration,
}
```

---

## Validator Pseudocode

### Layer 1: Pre-Test Validators

#### DockerDaemonValidator
```
FUNCTION validate_docker_daemon():
    TRY:
        output = execute("docker info --format {{.ServerVersion}}")
        IF output.exit_code != 0:
            RETURN Failure("Docker daemon not running")

        IF output.response_time > 2 seconds:
            RETURN Warning("Docker daemon slow to respond")

        IF output.version is empty:
            RETURN Failure("Docker returned empty version")

        RETURN Success
    CATCH error:
        RETURN Failure("Docker daemon check failed: " + error)
```

#### DockerSocketValidator
```
FUNCTION validate_docker_socket():
    socket_path = "/var/run/docker.sock"

    IF NOT file_exists(socket_path):
        RETURN Failure("Docker socket not found")

    IF NOT can_read_write(socket_path):
        RETURN Failure("Insufficient permissions for Docker socket")

    TRY:
        connection = open_unix_socket(socket_path)
        connection.close()
        RETURN Success
    CATCH error:
        RETURN Failure("Cannot connect to Docker socket: " + error)
```

#### ResourceAvailabilityValidator
```
FUNCTION validate_resources():
    TRY:
        system_info = execute("docker info --format json")

        available_memory = system_info.MemTotal - system_info.MemUsed
        IF available_memory < 2GB:
            RETURN Failure("Insufficient memory: " + available_memory)

        available_disk = get_disk_space("/var/lib/docker")
        IF available_disk < 10GB:
            RETURN Failure("Insufficient disk space: " + available_disk)

        cpu_usage = get_cpu_usage()
        IF cpu_usage > 95%:
            RETURN Warning("CPU highly saturated: " + cpu_usage + "%")

        RETURN Success
    CATCH error:
        RETURN Failure("Resource check failed: " + error)
```

### Layer 2: Runtime Validators

#### ContainerTrackerValidator
```
FUNCTION track_container_creation():
    containers_created = []

    HOOK on_container_create(container_id, image, name):
        containers_created.append({
            id: container_id,
            image: image,
            name: name,
            timestamp: now()
        })

    FUNCTION validate():
        IF containers_created is empty:
            RETURN Failure("No containers created during test")

        FOR EACH container IN containers_created:
            IF NOT container_exists(container.id):
                RETURN Warning("Container " + container.id + " not found")

        RETURN Success
```

#### PortBindingMonitorValidator
```
FUNCTION monitor_port_bindings():
    port_bindings = []

    HOOK on_port_bind(container_id, host_port, container_port):
        port_bindings.append({
            container: container_id,
            host_port: host_port,
            container_port: container_port,
            timestamp: now()
        })

    FUNCTION validate():
        IF port_bindings is empty:
            RETURN Warning("No ports bound during test")

        FOR EACH binding IN port_bindings:
            IF NOT is_port_listening(binding.host_port):
                RETURN Failure("Port " + binding.host_port + " not listening")

        RETURN Success
```

### Layer 3: Post-Test Validators

#### ContainerLifecycleVerifier
```
FUNCTION verify_container_lifecycle():
    TRY:
        containers = execute("docker ps -a --filter label=test-id=<test_id>")

        IF containers is empty:
            RETURN Failure("No containers found in Docker history")

        FOR EACH container IN containers:
            IF container.status != "exited":
                RETURN Warning("Container " + container.id + " still running")

            IF container.exit_code != 0:
                logs = get_container_logs(container.id)
                RETURN Failure("Container exited with error: " + logs)

        RETURN Success
    CATCH error:
        RETURN Failure("Lifecycle verification failed: " + error)
```

#### LogAnalyzerValidator
```
FUNCTION analyze_container_logs():
    TRY:
        containers = get_test_containers()

        FOR EACH container IN containers:
            logs = get_container_logs(container.id)

            IF logs is empty:
                RETURN Failure("Container " + container.id + " has no logs")

            IF logs.contains("MOCK") OR logs.contains("FAKE"):
                RETURN Failure("Mock implementation detected in logs")

            IF NOT logs.contains_expected_output():
                RETURN Warning("Unexpected log output")

        RETURN Success
    CATCH error:
        RETURN Failure("Log analysis failed: " + error)
```

### Layer 4: Service-Level Validators

#### DatabaseConnectionTester
```
FUNCTION test_database_connection(container):
    TRY:
        host = container.get_host()
        port = container.get_port()

        connection = connect_database(host, port, "testuser", "testpass")

        IF connection is null:
            RETURN Failure("Failed to connect to database")

        IF connection.host != expected_host:
            RETURN Failure("Connected to wrong host: " + connection.host)

        connection.close()
        RETURN Success
    CATCH error:
        RETURN Failure("Database connection failed: " + error)
```

#### ServiceOperationTester
```
FUNCTION test_service_operations(container):
    TRY:
        connection = connect_database(container)

        # Test INSERT
        start_time = now()
        result = connection.execute("INSERT INTO test VALUES (1, 'test')")
        insert_duration = now() - start_time

        IF insert_duration < 1ms:
            RETURN Failure("Insert too fast (likely mock): " + insert_duration)

        # Test SELECT
        result = connection.execute("SELECT * FROM test WHERE id = 1")
        IF result is empty:
            RETURN Failure("Data not persisted")

        # Test DELETE
        connection.execute("DELETE FROM test WHERE id = 1")
        result = connection.execute("SELECT * FROM test WHERE id = 1")
        IF NOT result is empty:
            RETURN Failure("Delete failed")

        connection.close()
        RETURN Success
    CATCH error:
        RETURN Failure("Service operations failed: " + error)
```

### Layer 5: Negative Validators

#### FailCaseTester
```
FUNCTION test_fail_case():
    # Stop Docker daemon
    stop_docker_daemon()

    TRY:
        result = run_test_without_docker()

        IF result.passed:
            RETURN Failure("Test passed without Docker (false positive)")

        IF NOT result.error_message.contains("Docker"):
            RETURN Warning("Error message doesn't mention Docker")

        RETURN Success
    FINALLY:
        start_docker_daemon()
```

---

## Integration Hooks

### Test Lifecycle Hooks

```rust
/// Hooks for integrating validation into test lifecycle
pub struct ValidationHooks {
    pre_validators: ValidationRegistry,
    runtime_monitor: RuntimeMonitor,
    post_validators: ValidationRegistry,
}

impl ValidationHooks {
    /// Run before test starts
    pub fn pre_test(&self) -> ValidationReport {
        self.pre_validators.validate_all()
    }

    /// Start monitoring during test
    pub fn start_monitoring(&mut self) {
        self.runtime_monitor.start();
    }

    /// Stop monitoring after test
    pub fn stop_monitoring(&mut self) -> ValidationReport {
        self.runtime_monitor.stop();
        self.runtime_monitor.report()
    }

    /// Run after test completes
    pub fn post_test(&self) -> ValidationReport {
        self.post_validators.validate_all()
    }

    /// Full validation lifecycle
    pub async fn full_validation<F, Fut>(&mut self, test_fn: F) -> ValidationReport
    where
        F: FnOnce() -> Fut,
        Fut: std::future::Future<Output = ()>,
    {
        let mut combined_report = ValidationReport::new();

        // Pre-test validation
        let pre_report = self.pre_test();
        combined_report.merge(pre_report);

        if combined_report.has_failures() {
            return combined_report;
        }

        // Start monitoring
        self.start_monitoring();

        // Run test
        test_fn().await;

        // Stop monitoring
        let runtime_report = self.stop_monitoring();
        combined_report.merge(runtime_report);

        // Post-test validation
        let post_report = self.post_test();
        combined_report.merge(post_report);

        combined_report
    }
}
```

---

## Configuration

### Validation Configuration

```rust
/// Configuration for validation framework
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationConfig {
    /// Enable pre-test validation
    pub enable_pre_test: bool,
    /// Enable runtime monitoring
    pub enable_runtime_monitoring: bool,
    /// Enable post-test validation
    pub enable_post_test: bool,
    /// Enable service-level validation
    pub enable_service_level: bool,
    /// Enable negative testing
    pub enable_negative_testing: bool,
    /// Timeout for validators (seconds)
    pub validator_timeout: u64,
    /// Fail on warnings
    pub fail_on_warnings: bool,
    /// Docker daemon timeout (seconds)
    pub docker_daemon_timeout: u64,
}

impl Default for ValidationConfig {
    fn default() -> Self {
        Self {
            enable_pre_test: true,
            enable_runtime_monitoring: true,
            enable_post_test: true,
            enable_service_level: true,
            enable_negative_testing: false, // Expensive
            validator_timeout: 30,
            fail_on_warnings: false,
            docker_daemon_timeout: 2,
        }
    }
}
```

---

## Usage Examples

### Basic Usage

```rust
use cleanroom::validation::{ValidationRegistry, ValidationConfig};
use cleanroom::validation::pre_test::{DockerDaemonCheck, DockerSocketCheck};

#[tokio::test]
async fn test_with_validation() {
    // Create validation registry
    let mut registry = ValidationRegistry::new();
    registry.add_validator(Box::new(DockerDaemonCheck::new()));
    registry.add_validator(Box::new(DockerSocketCheck::new()));

    // Run validation
    let report = registry.validate_all();
    report.assert_all_passed();

    // Run actual test
    // ...
}
```

### Full Lifecycle Validation

```rust
#[tokio::test]
async fn test_with_full_validation() {
    let mut hooks = ValidationHooks::new(ValidationConfig::default());

    let report = hooks.full_validation(|| async {
        // Your test code here
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        // ...
    }).await;

    // Assert all validations passed
    report.assert_all_passed();
}
```

---

## Conclusion

This implementation design provides a **production-ready validation framework** with:

1. **Strong Typing**: Rust's type system ensures correctness
2. **Async Support**: Full async/await support for modern Rust
3. **Composability**: Validators can be combined and configured
4. **Observability**: Comprehensive reporting and logging
5. **Extensibility**: Easy to add new validators

**Next Steps**: Begin implementation starting with core traits and types.
