# Error Examples and Usage Patterns

## Error Examples Overview

This document provides comprehensive examples of how to create, handle, and manage errors in the Cleanroom framework.

## Basic Error Creation

### Creating Simple Errors

```rust
use cleanroom::error::{CleanroomError, ErrorKind};

// Create a basic error
let error = CleanroomError::new(ErrorKind::ContainerError, "Container failed to start");

// Create error with context
let error = CleanroomError::container_error("PostgreSQL container failed")
    .with_context("Database initialization");

// Create error with source
let error = CleanroomError::network_error("Connection timeout")
    .with_source("Docker daemon");

// Create error with both context and source
let error = CleanroomError::timeout_error("Operation timed out")
    .with_context("Database connection attempt")
    .with_source("PostgreSQL service");
```

### Using Error Helper Functions

```rust
use cleanroom::error::CleanroomError;

// Container errors
let container_error = CleanroomError::container_error("Container failed to start");
let image_error = CleanroomError::container_error("Failed to pull image");

// Network errors
let network_error = CleanroomError::network_error("Network unreachable");
let connection_error = CleanroomError::network_error("Connection refused");

// Resource errors
let memory_error = CleanroomError::resource_limit_exceeded("Memory limit exceeded");
let cpu_error = CleanroomError::resource_limit_exceeded("CPU limit exceeded");

// Timeout errors
let timeout_error = CleanroomError::timeout_error("Operation timed out");
let startup_timeout = CleanroomError::timeout_error("Container startup timeout");

// Configuration errors
let config_error = CleanroomError::configuration_error("Invalid configuration");
let validation_error = CleanroomError::validation_error("Validation failed");

// Policy errors
let policy_error = CleanroomError::policy_violation_error("Policy violation detected");
let security_error = CleanroomError::policy_violation_error("Security policy violated");

// Feature-specific errors
let coverage_error = CleanroomError::coverage_error("Coverage tracking failed");
let snapshot_error = CleanroomError::snapshot_error("Snapshot validation failed");
let tracing_error = CleanroomError::tracing_error("Tracing initialization failed");
let redaction_error = CleanroomError::redaction_error("Data redaction failed");
let report_error = CleanroomError::report_error("Report generation failed");

// System errors
let io_error = CleanroomError::io_error("File operation failed");
let serialization_error = CleanroomError::serialization_error("Serialization failed");
let internal_error = CleanroomError::internal_error("Internal system error");
```

## Specialized Error Types

### BackendError Examples

```rust
use cleanroom::error::BackendError;

// Runtime errors
let runtime_error = BackendError::Runtime("Runtime execution failed".to_string());
let command_error = BackendError::CommandExecution("Command execution failed".to_string());

// Container errors
let startup_error = BackendError::ContainerStartup("Container startup failed".to_string());
let communication_error = BackendError::ContainerCommunication("Container communication failed".to_string());

// Image errors
let pull_error = BackendError::ImagePull("Image pull failed".to_string());
let build_error = BackendError::ImageBuild("Image build failed".to_string());

// Feature errors
let unsupported_error = BackendError::UnsupportedFeature("Feature not supported".to_string());
```

### PolicyError Examples

```rust
use cleanroom::error::PolicyError;

// Policy configuration errors
let invalid_policy = PolicyError::InvalidPolicy("Invalid policy configuration".to_string());
let policy_violation = PolicyError::PolicyViolation("Policy violation detected".to_string());
let unsupported_feature = PolicyError::UnsupportedFeature("Policy feature not supported".to_string());
```

### ScenarioError Examples

```rust
use cleanroom::error::ScenarioError;

// Scenario definition errors
let invalid_scenario = ScenarioError::InvalidScenario("Invalid scenario definition".to_string());
let step_error = ScenarioError::StepExecutionFailed("Step execution failed".to_string());
let timeout_error = ScenarioError::ScenarioTimeout("Scenario timeout".to_string());
let concurrent_error = ScenarioError::ConcurrentExecution("Concurrent execution error".to_string());
```

### ServiceError Examples

```rust
use cleanroom::error::ServiceError;

// Service connection errors
let connection_error = ServiceError::ConnectionFailed("Service connection failed".to_string());
let startup_error = ServiceError::StartupFailed("Service startup failed".to_string());
let health_error = ServiceError::HealthCheckFailed("Health check failed".to_string());

// Service configuration errors
let config_error = ServiceError::Configuration("Service configuration error".to_string());
let operation_error = ServiceError::UnsupportedOperation("Unsupported operation".to_string());
```

### ConfigError Examples

```rust
use cleanroom::error::ConfigError;

// Configuration file errors
let invalid_file = ConfigError::InvalidFile("Invalid configuration file".to_string());
let missing_value = ConfigError::MissingValue("Missing configuration value".to_string());
let invalid_value = ConfigError::InvalidValue("Invalid configuration value".to_string());
let invalid_pattern = ConfigError::InvalidPattern("Invalid pattern".to_string(), "Pattern details".to_string());
```

## Error Conversion Examples

### Converting External Errors

```rust
use cleanroom::error::{CleanroomError, Result};
use std::fs::File;

// Convert IO errors
fn read_config_file(path: &str) -> Result<String> {
    let content = std::fs::read_to_string(path)?; // Automatically converts std::io::Error
    Ok(content)
}

// Convert JSON errors
fn parse_config_json(json_str: &str) -> Result<serde_json::Value> {
    let value = serde_json::from_str(json_str)?; // Automatically converts serde_json::Error
    Ok(value)
}

// Convert testcontainers errors
fn create_container() -> Result<()> {
    let container = testcontainers::Container::new("postgres:13")?; // Automatically converts TestcontainersError
    Ok(())
}

// Manual error conversion with context
fn process_file(path: &str) -> Result<String> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| CleanroomError::io_error("Failed to read file")
            .with_context(format!("File: {}", path))
            .with_source(e.to_string()))?;
    Ok(content)
}
```

### Error Chain Preservation

```rust
use cleanroom::error::{CleanroomError, Result};

fn complex_operation() -> Result<String> {
    let result = step1()?;
    let result = step2(result)?;
    let result = step3(result)?;
    Ok(result)
}

fn step1() -> Result<String> {
    Err(CleanroomError::io_error("Step 1 failed")
        .with_context("Reading input file")
        .with_source("File system"))
}

fn step2(input: String) -> Result<String> {
    Err(CleanroomError::network_error("Step 2 failed")
        .with_context("Connecting to service")
        .with_source("Network layer"))
}

fn step3(input: String) -> Result<String> {
    Err(CleanroomError::timeout_error("Step 3 failed")
        .with_context("Processing data")
        .with_source("Processing engine"))
}
```

## Error Handling Examples

### Basic Error Handling

```rust
use cleanroom::error::{CleanroomError, ErrorKind, Result};

fn handle_basic_errors() -> Result<()> {
    let result = risky_operation();
    
    match result {
        Ok(value) => {
            println!("Operation succeeded: {:?}", value);
            Ok(())
        },
        Err(error) => {
            eprintln!("Operation failed: {}", error);
            Err(error)
        }
    }
}

fn risky_operation() -> Result<String> {
    Err(CleanroomError::container_error("Container failed"))
}
```

### Specific Error Type Handling

```rust
use cleanroom::error::{CleanroomError, ErrorKind, Result};

fn handle_specific_errors() -> Result<()> {
    let result = risky_operation();
    
    match result {
        Ok(value) => {
            println!("Operation succeeded: {:?}", value);
            Ok(())
        },
        Err(CleanroomError { kind: ErrorKind::ContainerError, .. }) => {
            eprintln!("Container error occurred");
            // Handle container-specific error
            Err(CleanroomError::container_error("Container handling failed"))
        },
        Err(CleanroomError { kind: ErrorKind::NetworkError, .. }) => {
            eprintln!("Network error occurred");
            // Handle network-specific error
            Err(CleanroomError::network_error("Network handling failed"))
        },
        Err(CleanroomError { kind: ErrorKind::Timeout, .. }) => {
            eprintln!("Timeout occurred");
            // Handle timeout-specific error
            Err(CleanroomError::timeout_error("Timeout handling failed"))
        },
        Err(error) => {
            eprintln!("Unexpected error: {}", error);
            // Handle unexpected error
            Err(error)
        }
    }
}
```

### Error Recovery Examples

```rust
use cleanroom::error::{CleanroomError, Result};
use std::time::Duration;
use tokio::time::sleep;

async fn retry_operation() -> Result<String> {
    let max_retries = 3;
    let mut last_error = None;
    
    for attempt in 1..=max_retries {
        match perform_operation().await {
            Ok(result) => return Ok(result),
            Err(error) => {
                last_error = Some(error);
                if attempt < max_retries {
                    eprintln!("Attempt {} failed, retrying...", attempt);
                    sleep(Duration::from_secs(2_u64.pow(attempt))).await; // Exponential backoff
                }
            }
        }
    }
    
    Err(last_error.unwrap_or_else(|| {
        CleanroomError::internal_error("All retry attempts failed")
    }))
}

async fn perform_operation() -> Result<String> {
    // Simulate operation that might fail
    Err(CleanroomError::network_error("Network operation failed"))
}
```

### Error Context Preservation

```rust
use cleanroom::error::{CleanroomError, Result};

fn preserve_error_context() -> Result<()> {
    let result = outer_operation();
    
    match result {
        Ok(value) => Ok(value),
        Err(error) => {
            // Preserve the original error context
            Err(error.with_context("Outer operation failed"))
        }
    }
}

fn outer_operation() -> Result<()> {
    let result = inner_operation();
    
    match result {
        Ok(value) => Ok(value),
        Err(error) => {
            // Add layer-specific context
            Err(error.with_context("Inner operation failed"))
        }
    }
}

fn inner_operation() -> Result<()> {
    Err(CleanroomError::io_error("File not found")
        .with_context("Reading configuration file")
        .with_source("File system"))
}
```

## Error Testing Examples

### Testing Error Creation

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use cleanroom::error::{CleanroomError, ErrorKind};

    #[test]
    fn test_error_creation() {
        let error = CleanroomError::new(ErrorKind::ContainerError, "test message");
        assert_eq!(error.message, "test message");
        assert!(matches!(error.kind, ErrorKind::ContainerError));
    }

    #[test]
    fn test_error_with_context() {
        let error = CleanroomError::new(ErrorKind::ContainerError, "test message")
            .with_context("test context");
        assert_eq!(error.message, "test message");
        assert_eq!(error.context, Some("test context".to_string()));
    }

    #[test]
    fn test_error_with_source() {
        let error = CleanroomError::new(ErrorKind::ContainerError, "test message")
            .with_source("test source");
        assert_eq!(error.message, "test message");
        assert_eq!(error.source, Some("test source".to_string()));
    }

    #[test]
    fn test_error_display() {
        let error = CleanroomError::new(ErrorKind::Timeout, "test message");
        let display = format!("{}", error);
        assert!(display.contains("Timeout"));
        assert!(display.contains("test message"));
    }
}
```

### Testing Error Conversion

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_from_io() {
        let io_error = std::io::Error::new(std::io::ErrorKind::NotFound, "test");
        let error: CleanroomError = io_error.into();
        assert!(matches!(error.kind, ErrorKind::IoError));
    }

    #[test]
    fn test_error_from_json() {
        let json_error = serde_json::from_str::<serde_json::Value>("invalid json");
        let error: CleanroomError = json_error.unwrap_err().into();
        assert!(matches!(error.kind, ErrorKind::SerializationError));
    }

    #[test]
    fn test_helper_functions() {
        let container_error = CleanroomError::container_error("container failed");
        assert!(matches!(container_error.kind, ErrorKind::ContainerError));

        let network_error = CleanroomError::network_error("network failed");
        assert!(matches!(network_error.kind, ErrorKind::NetworkError));

        let timeout_error = CleanroomError::timeout_error("timeout occurred");
        assert!(matches!(timeout_error.kind, ErrorKind::Timeout));
    }
}
```

### Testing Error Handling

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_handling() {
        let result = Err(CleanroomError::container_error("test error"));
        
        match result {
            Err(CleanroomError { kind: ErrorKind::ContainerError, .. }) => {
                // Test passed - error was handled correctly
            },
            _ => panic!("Expected container error"),
        }
    }

    #[test]
    fn test_error_recovery() {
        let mut attempts = 0;
        let result = std::panic::catch_unwind(|| {
            attempts += 1;
            if attempts < 3 {
                Err(CleanroomError::network_error("network failed"))
            } else {
                Ok("success")
            }
        });
        
        assert!(result.is_ok());
        assert_eq!(attempts, 3);
    }
}
```

## Error Monitoring Examples

### Error Metrics Collection

```rust
use cleanroom::error::{CleanroomError, ErrorKind};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

#[derive(Debug, Default)]
struct ErrorMetrics {
    error_counts: HashMap<ErrorKind, u64>,
    error_rate: f64,
    last_error_time: Option<chrono::DateTime<chrono::Utc>>,
}

impl ErrorMetrics {
    fn record_error(&mut self, error: &CleanroomError) {
        *self.error_counts.entry(error.kind.clone()).or_insert(0) += 1;
        self.last_error_time = Some(error.timestamp);
        self.calculate_error_rate();
    }
    
    fn calculate_error_rate(&mut self) {
        // Calculate error rate based on time window
        // Implementation depends on specific requirements
        self.error_rate = 0.0; // Placeholder
    }
    
    fn get_error_count(&self, kind: &ErrorKind) -> u64 {
        self.error_counts.get(kind).copied().unwrap_or(0)
    }
}

#[derive(Debug)]
struct ErrorMonitor {
    metrics: Arc<RwLock<ErrorMetrics>>,
}

impl ErrorMonitor {
    fn new() -> Self {
        Self {
            metrics: Arc::new(RwLock::new(ErrorMetrics::default())),
        }
    }
    
    async fn record_error(&self, error: &CleanroomError) {
        let mut metrics = self.metrics.write().await;
        metrics.record_error(error);
    }
    
    async fn get_error_count(&self, kind: &ErrorKind) -> u64 {
        let metrics = self.metrics.read().await;
        metrics.get_error_count(kind)
    }
    
    async fn get_error_rate(&self) -> f64 {
        let metrics = self.metrics.read().await;
        metrics.error_rate
    }
}
```

### Error Alerting

```rust
use cleanroom::error::{CleanroomError, ErrorKind};
use std::sync::Arc;
use tokio::sync::RwLock;

#[derive(Debug)]
struct ErrorAlert {
    kind: ErrorKind,
    threshold: u64,
    current_count: u64,
    alert_sent: bool,
}

impl ErrorAlert {
    fn new(kind: ErrorKind, threshold: u64) -> Self {
        Self {
            kind,
            threshold,
            current_count: 0,
            alert_sent: false,
        }
    }
    
    fn check_threshold(&mut self) -> bool {
        if self.current_count >= self.threshold && !self.alert_sent {
            self.alert_sent = true;
            true
        } else {
            false
        }
    }
    
    fn increment(&mut self) {
        self.current_count += 1;
    }
}

#[derive(Debug)]
struct ErrorAlerter {
    alerts: Arc<RwLock<Vec<ErrorAlert>>>,
}

impl ErrorAlerter {
    fn new() -> Self {
        Self {
            alerts: Arc::new(RwLock::new(Vec::new())),
        }
    }
    
    async fn add_alert(&self, kind: ErrorKind, threshold: u64) {
        let mut alerts = self.alerts.write().await;
        alerts.push(ErrorAlert::new(kind, threshold));
    }
    
    async fn process_error(&self, error: &CleanroomError) {
        let mut alerts = self.alerts.write().await;
        
        for alert in alerts.iter_mut() {
            if alert.kind == error.kind {
                alert.increment();
                if alert.check_threshold() {
                    self.send_alert(&error).await;
                }
            }
        }
    }
    
    async fn send_alert(&self, error: &CleanroomError) {
        eprintln!("ALERT: Error threshold exceeded for {:?}", error.kind);
        eprintln!("Error: {}", error.message);
        if let Some(context) = &error.context {
            eprintln!("Context: {}", context);
        }
        if let Some(source) = &error.source {
            eprintln!("Source: {}", source);
        }
    }
}
```

## Error Best Practices Examples

### Proper Error Context

```rust
use cleanroom::error::{CleanroomError, Result};

// Good: Provide meaningful context
fn create_user_error() -> Result<()> {
    Err(CleanroomError::validation_error("Invalid email format")
        .with_context("User registration")
        .with_source("Email validator"))
}

// Avoid: Minimal context
fn create_bad_error() -> Result<()> {
    Err(CleanroomError::validation_error("Invalid"))
}

// Good: Layer-specific context
fn process_user_data(user_data: &str) -> Result<String> {
    let result = validate_user_data(user_data);
    
    match result {
        Ok(data) => Ok(data),
        Err(error) => Err(error.with_context("User data processing")),
    }
}

fn validate_user_data(data: &str) -> Result<String> {
    if data.is_empty() {
        Err(CleanroomError::validation_error("Empty user data")
            .with_context("Data validation")
            .with_source("Input validator"))
    } else {
        Ok(data.to_string())
    }
}
```

### Error Recovery Patterns

```rust
use cleanroom::error::{CleanroomError, Result};
use std::time::Duration;
use tokio::time::sleep;

// Good: Implement retry with exponential backoff
async fn retry_with_backoff<F, T>(mut operation: F, max_retries: u32) -> Result<T>
where
    F: FnMut() -> Result<T>,
{
    let mut last_error = None;
    
    for attempt in 1..=max_retries {
        match operation() {
            Ok(result) => return Ok(result),
            Err(error) => {
                last_error = Some(error);
                if attempt < max_retries {
                    let delay = Duration::from_secs(2_u64.pow(attempt - 1));
                    sleep(delay).await;
                }
            }
        }
    }
    
    Err(last_error.unwrap_or_else(|| {
        CleanroomError::internal_error("All retry attempts failed")
    }))
}

// Good: Implement fallback mechanism
async fn operation_with_fallback() -> Result<String> {
    // Try primary operation
    match primary_operation().await {
        Ok(result) => Ok(result),
        Err(_) => {
            // Fall back to secondary operation
            match secondary_operation().await {
                Ok(result) => Ok(result),
                Err(error) => Err(error.with_context("Both primary and secondary operations failed")),
            }
        }
    }
}

async fn primary_operation() -> Result<String> {
    Err(CleanroomError::network_error("Primary service unavailable"))
}

async fn secondary_operation() -> Result<String> {
    Ok("Fallback result".to_string())
}
```

### Error Propagation Best Practices

```rust
use cleanroom::error::{CleanroomError, Result};

// Good: Use ? operator for error propagation
fn process_data() -> Result<String> {
    let data = read_file("data.txt")?; // Error automatically propagated
    let processed = transform_data(data)?; // Error automatically propagated
    Ok(processed)
}

// Good: Add context at each layer
fn process_data_with_context() -> Result<String> {
    let data = read_file("data.txt")
        .map_err(|e| e.with_context("Reading input file"))?;
    
    let processed = transform_data(data)
        .map_err(|e| e.with_context("Transforming data"))?;
    
    Ok(processed)
}

// Avoid: Swallowing errors
fn bad_process_data() -> Result<String> {
    let data = match read_file("data.txt") {
        Ok(d) => d,
        Err(_) => return Err(CleanroomError::io_error("File read failed")),
    };
    Ok(data)
}

fn read_file(path: &str) -> Result<String> {
    Err(CleanroomError::io_error("File not found")
        .with_context(format!("Reading file: {}", path))
        .with_source("File system"))
}

fn transform_data(data: String) -> Result<String> {
    Err(CleanroomError::internal_error("Transformation failed")
        .with_context("Data transformation")
        .with_source("Processing engine"))
}
```
