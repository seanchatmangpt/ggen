# Unified Error Handling Strategy

## Overview

The error handling strategy ensures consistent error reporting, automatic fallback, and graceful degradation across marketplace-v1 and marketplace-v2 backends.

## Error Type Hierarchy

```rust
// ggen-domain/src/marketplace/error.rs

use thiserror::Error;

/// Unified marketplace error type
#[derive(Error, Debug)]
pub enum MarketplaceError {
    // Backend-specific errors
    #[error("V1 backend error: {0}")]
    V1Error(#[from] V1BackendError),

    #[error("V2 backend error: {0}")]
    V2Error(#[from] V2BackendError),

    // Conversion errors
    #[error("Failed to convert package format: {0}")]
    ConversionError(String),

    #[error("Invalid package ID format: {0}")]
    InvalidPackageId(String),

    // Search errors
    #[error("Search query failed: {0}")]
    SearchError(String),

    #[error("Invalid search query: {0}")]
    InvalidSearchQuery(String),

    // Installation errors
    #[error("Installation failed: {0}")]
    InstallationError(String),

    #[error("Package not found: {0}")]
    PackageNotFound(String),

    #[error("Dependency resolution failed: {0}")]
    DependencyResolutionError(String),

    // Publishing errors
    #[error("Publishing failed: {0}")]
    PublishError(String),

    #[error("Invalid manifest: {0}")]
    InvalidManifest(String),

    // Cryptographic errors (V2-specific)
    #[error("Signature verification failed")]
    SignatureVerificationFailed,

    #[error("Invalid signature format: {0}")]
    InvalidSignature(String),

    #[error("Key not found: {0}")]
    KeyNotFound(String),

    #[error("Cryptographic operation failed: {0}")]
    CryptoError(String),

    // RDF errors (V2-specific)
    #[error("SPARQL query failed: {0}")]
    SparqlError(String),

    #[error("Invalid RDF data: {0}")]
    InvalidRdf(String),

    #[error("Ontology validation failed: {0}")]
    OntologyValidationError(String),

    // Backend availability errors
    #[error("Backend not available: {0}")]
    BackendNotAvailable(String),

    #[error("Feature not supported by backend: {0}")]
    FeatureNotSupported(String),

    // Configuration errors
    #[error("Configuration error: {0}")]
    ConfigError(String),

    #[error("Invalid backend type: {0}")]
    InvalidBackendType(String),

    // Network errors
    #[error("Network error: {0}")]
    NetworkError(String),

    #[error("Timeout: {0}")]
    Timeout(String),

    // IO errors
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),

    // Generic errors
    #[error("Internal error: {0}")]
    Internal(String),
}

pub type Result<T> = std::result::Result<T, MarketplaceError>;

/// V1 backend errors
#[derive(Error, Debug)]
pub enum V1BackendError {
    #[error("Tantivy index error: {0}")]
    IndexError(String),

    #[error("Registry error: {0}")]
    RegistryError(String),

    #[error(transparent)]
    TantivyError(#[from] tantivy::TantivyError),

    #[error(transparent)]
    IoError(#[from] std::io::Error),
}

/// V2 backend errors
#[derive(Error, Debug)]
pub enum V2BackendError {
    #[error("RDF store error: {0}")]
    RdfStoreError(String),

    #[error("SPARQL error: {0}")]
    SparqlError(String),

    #[error("Cryptographic error: {0}")]
    CryptoError(String),

    #[error(transparent)]
    OxigraphError(#[from] oxigraph::store::StoreError),

    #[error(transparent)]
    Ed25519Error(#[from] ed25519_dalek::ed25519::Error),

    #[error(transparent)]
    IoError(#[from] std::io::Error),
}
```

## Error Conversion Strategy

### V1 → Unified Error Conversion

```rust
// ggen-domain/src/marketplace/v1_adapter.rs

impl From<ggen_marketplace::Error> for MarketplaceError {
    fn from(e: ggen_marketplace::Error) -> Self {
        match e {
            ggen_marketplace::Error::PackageNotFound(id) => {
                MarketplaceError::PackageNotFound(id)
            }
            ggen_marketplace::Error::SearchError(msg) => {
                MarketplaceError::SearchError(msg)
            }
            ggen_marketplace::Error::InstallationError(msg) => {
                MarketplaceError::InstallationError(msg)
            }
            ggen_marketplace::Error::TantivyError(e) => {
                MarketplaceError::V1Error(V1BackendError::TantivyError(e))
            }
            // ... other conversions
            _ => MarketplaceError::V1Error(V1BackendError::RegistryError(e.to_string())),
        }
    }
}
```

### V2 → Unified Error Conversion

```rust
// ggen-domain/src/marketplace/v2_adapter.rs

impl From<ggen_marketplace_v2::Error> for MarketplaceError {
    fn from(e: ggen_marketplace_v2::Error) -> Self {
        match e {
            ggen_marketplace_v2::Error::PackageNotFound(id) => {
                MarketplaceError::PackageNotFound(id)
            }
            ggen_marketplace_v2::Error::SparqlError(msg) => {
                MarketplaceError::SparqlError(msg)
            }
            ggen_marketplace_v2::Error::SignatureVerificationFailed => {
                MarketplaceError::SignatureVerificationFailed
            }
            ggen_marketplace_v2::Error::OxigraphError(e) => {
                MarketplaceError::V2Error(V2BackendError::OxigraphError(e))
            }
            // ... other conversions
            _ => MarketplaceError::V2Error(V2BackendError::RdfStoreError(e.to_string())),
        }
    }
}
```

## Fallback Strategy

### Automatic Fallback in Dual Backend

```rust
// ggen-domain/src/marketplace/dual_adapter.rs

impl DualBackendAdapter {
    /// Execute operation with automatic fallback
    async fn execute_with_fallback<T, F1, F2>(
        &self,
        v1_operation: F1,
        v2_operation: F2,
        operation_name: &str,
    ) -> Result<T>
    where
        F1: Future<Output = Result<T>>,
        F2: Future<Output = Result<T>>,
    {
        match self.strategy {
            BackendStrategy::V2WithFallback => {
                match v2_operation.await {
                    Ok(result) => {
                        tracing::info!("V2 {} succeeded", operation_name);
                        self.record_success(BackendType::V2);
                        Ok(result)
                    }
                    Err(e) => {
                        tracing::warn!(
                            "V2 {} failed, falling back to V1: {}",
                            operation_name,
                            e
                        );
                        self.record_fallback(operation_name, &e);

                        match v1_operation.await {
                            Ok(result) => {
                                self.record_success(BackendType::V1);
                                Ok(result)
                            }
                            Err(v1_error) => {
                                tracing::error!(
                                    "V1 fallback also failed for {}: {}",
                                    operation_name,
                                    v1_error
                                );
                                // Return composite error
                                Err(MarketplaceError::Internal(format!(
                                    "Both backends failed. V2: {}, V1: {}",
                                    e, v1_error
                                )))
                            }
                        }
                    }
                }
            }

            BackendStrategy::V1Only => v1_operation.await,
            BackendStrategy::V2Only => v2_operation.await,

            BackendStrategy::ABTest { .. } => {
                let selected = self.select_backend();
                match selected {
                    BackendType::V1 => v1_operation.await,
                    BackendType::V2 => v2_operation.await,
                    _ => unreachable!(),
                }
            }

            BackendStrategy::Compare => {
                // Execute both and compare (for testing)
                let (v1_result, v2_result) = tokio::join!(v1_operation, v2_operation);
                self.compare_and_log_results(operation_name, &v1_result, &v2_result);
                v2_result.or(v1_result)
            }
        }
    }

    fn record_fallback(&self, operation: &str, error: &MarketplaceError) {
        let mut metrics = self.metrics.lock().unwrap();
        metrics.v2_fallbacks += 1;

        tracing::warn!(
            "Fallback triggered for {}: {}",
            operation,
            error
        );

        // Emit OpenTelemetry metric
        #[cfg(feature = "telemetry")]
        {
            use opentelemetry::metrics::Counter;
            let counter = metrics::meter("marketplace")
                .u64_counter("fallback_count")
                .init();
            counter.add(1, &[("operation", operation.to_string())]);
        }
    }
}
```

### Fallback Configuration

```yaml
# ~/.config/ggen/config.yaml

marketplace:
  fallback:
    enabled: true

    # Fallback to V1 if V2 fails
    fallback_to_v1: true

    # Number of retry attempts before fallback
    retry_attempts: 3

    # Retry delay (exponential backoff)
    retry_delay_ms: 100
    max_retry_delay_ms: 5000

    # Operations that always use V2 (no fallback)
    v2_only_operations:
      - "publish_with_signature"
      - "verify_signature"

    # Log fallback events
    log_fallbacks: true

    # Alert on high fallback rate
    alert_threshold: 10  # Alert if >10% operations fall back
```

## Error Recovery Strategies

### Strategy 1: Retry with Exponential Backoff

```rust
// ggen-domain/src/marketplace/retry.rs

use tokio::time::{sleep, Duration};

pub async fn retry_with_backoff<T, E, F, Fut>(
    mut operation: F,
    max_attempts: u32,
    initial_delay_ms: u64,
    max_delay_ms: u64,
) -> std::result::Result<T, E>
where
    F: FnMut() -> Fut,
    Fut: Future<Output = std::result::Result<T, E>>,
    E: std::fmt::Display,
{
    let mut attempt = 0;
    let mut delay_ms = initial_delay_ms;

    loop {
        attempt += 1;

        match operation().await {
            Ok(result) => {
                if attempt > 1 {
                    tracing::info!("Operation succeeded after {} attempts", attempt);
                }
                return Ok(result);
            }
            Err(e) => {
                if attempt >= max_attempts {
                    tracing::error!(
                        "Operation failed after {} attempts: {}",
                        max_attempts,
                        e
                    );
                    return Err(e);
                }

                tracing::warn!(
                    "Attempt {}/{} failed: {}. Retrying in {}ms...",
                    attempt,
                    max_attempts,
                    e,
                    delay_ms
                );

                sleep(Duration::from_millis(delay_ms)).await;

                // Exponential backoff
                delay_ms = (delay_ms * 2).min(max_delay_ms);
            }
        }
    }
}
```

### Strategy 2: Circuit Breaker

```rust
// ggen-domain/src/marketplace/circuit_breaker.rs

use std::sync::atomic::{AtomicU64, AtomicBool, Ordering};
use std::sync::Arc;

#[derive(Clone)]
pub struct CircuitBreaker {
    failure_count: Arc<AtomicU64>,
    is_open: Arc<AtomicBool>,
    threshold: u64,
    reset_timeout_ms: u64,
}

impl CircuitBreaker {
    pub fn new(threshold: u64, reset_timeout_ms: u64) -> Self {
        Self {
            failure_count: Arc::new(AtomicU64::new(0)),
            is_open: Arc::new(AtomicBool::new(false)),
            threshold,
            reset_timeout_ms,
        }
    }

    pub async fn execute<T, F, Fut>(&self, operation: F) -> Result<T>
    where
        F: FnOnce() -> Fut,
        Fut: Future<Output = Result<T>>,
    {
        // Check if circuit is open
        if self.is_open.load(Ordering::Relaxed) {
            return Err(MarketplaceError::BackendNotAvailable(
                "Circuit breaker is open".to_string(),
            ));
        }

        // Execute operation
        match operation().await {
            Ok(result) => {
                // Reset failure count on success
                self.failure_count.store(0, Ordering::Relaxed);
                Ok(result)
            }
            Err(e) => {
                // Increment failure count
                let failures = self.failure_count.fetch_add(1, Ordering::Relaxed) + 1;

                // Open circuit if threshold exceeded
                if failures >= self.threshold {
                    tracing::error!(
                        "Circuit breaker opened after {} failures",
                        failures
                    );
                    self.is_open.store(true, Ordering::Relaxed);

                    // Schedule circuit reset
                    let is_open = self.is_open.clone();
                    let timeout = self.reset_timeout_ms;
                    tokio::spawn(async move {
                        sleep(Duration::from_millis(timeout)).await;
                        is_open.store(false, Ordering::Relaxed);
                        tracing::info!("Circuit breaker reset after {}ms", timeout);
                    });
                }

                Err(e)
            }
        }
    }
}
```

## User-Facing Error Messages

### Error Context for Users

```rust
// ggen-domain/src/marketplace/error.rs

impl MarketplaceError {
    /// Get user-friendly error message
    pub fn user_message(&self) -> String {
        match self {
            MarketplaceError::PackageNotFound(id) => {
                format!(
                    "Package '{}' not found in marketplace. Try searching with 'ggen marketplace search'",
                    id
                )
            }
            MarketplaceError::SignatureVerificationFailed => {
                "Package signature verification failed. This package may have been tampered with.".to_string()
            }
            MarketplaceError::DependencyResolutionError(msg) => {
                format!(
                    "Failed to resolve dependencies: {}. Try running with --no-dependencies",
                    msg
                )
            }
            MarketplaceError::NetworkError(msg) => {
                format!(
                    "Network error: {}. Check your internet connection and try again",
                    msg
                )
            }
            MarketplaceError::Timeout(msg) => {
                format!(
                    "Operation timed out: {}. The server may be slow or unreachable",
                    msg
                )
            }
            _ => self.to_string(),
        }
    }

    /// Get suggested action for user
    pub fn suggested_action(&self) -> Option<String> {
        match self {
            MarketplaceError::PackageNotFound(_) => {
                Some("Search for similar packages with 'ggen marketplace search <query>'".to_string())
            }
            MarketplaceError::SignatureVerificationFailed => {
                Some("Install without signature verification using --no-verify (not recommended)".to_string())
            }
            MarketplaceError::DependencyResolutionError(_) => {
                Some("Install without dependencies using --no-dependencies".to_string())
            }
            MarketplaceError::NetworkError(_) => {
                Some("Check your network connection and proxy settings".to_string())
            }
            _ => None,
        }
    }

    /// Check if error is retryable
    pub fn is_retryable(&self) -> bool {
        matches!(
            self,
            MarketplaceError::NetworkError(_)
                | MarketplaceError::Timeout(_)
                | MarketplaceError::BackendNotAvailable(_)
        )
    }
}
```

### CLI Error Display

```rust
// ggen-cli/src/cmds/marketplace.rs

use colored::Colorize;

fn display_marketplace_error(error: &MarketplaceError) {
    eprintln!("{} {}", "Error:".red().bold(), error.user_message());

    if let Some(action) = error.suggested_action() {
        eprintln!("\n{} {}", "Suggestion:".yellow().bold(), action);
    }

    if error.is_retryable() {
        eprintln!("\n{} This operation can be retried", "Note:".cyan().bold());
    }

    // Log full error for debugging
    tracing::error!("Marketplace error: {:?}", error);
}
```

## Error Metrics and Monitoring

### OpenTelemetry Error Tracking

```rust
// ggen-domain/src/marketplace/metrics.rs

use opentelemetry::{
    metrics::{Counter, Histogram},
    KeyValue,
};

pub struct MarketplaceMetrics {
    error_counter: Counter<u64>,
    fallback_counter: Counter<u64>,
    latency_histogram: Histogram<f64>,
}

impl MarketplaceMetrics {
    pub fn new() -> Self {
        let meter = opentelemetry::global::meter("marketplace");

        Self {
            error_counter: meter
                .u64_counter("marketplace.errors")
                .with_description("Number of marketplace errors")
                .init(),
            fallback_counter: meter
                .u64_counter("marketplace.fallbacks")
                .with_description("Number of V2→V1 fallbacks")
                .init(),
            latency_histogram: meter
                .f64_histogram("marketplace.operation.duration")
                .with_description("Operation latency in milliseconds")
                .init(),
        }
    }

    pub fn record_error(&self, error: &MarketplaceError, backend: BackendType) {
        let error_type = match error {
            MarketplaceError::PackageNotFound(_) => "package_not_found",
            MarketplaceError::SearchError(_) => "search_error",
            MarketplaceError::InstallationError(_) => "installation_error",
            MarketplaceError::SignatureVerificationFailed => "signature_verification_failed",
            _ => "other",
        };

        self.error_counter.add(
            1,
            &[
                KeyValue::new("error_type", error_type),
                KeyValue::new("backend", backend.as_str()),
            ],
        );
    }

    pub fn record_fallback(&self, operation: &str) {
        self.fallback_counter.add(
            1,
            &[KeyValue::new("operation", operation.to_string())],
        );
    }

    pub fn record_latency(&self, operation: &str, backend: BackendType, latency_ms: f64) {
        self.latency_histogram.record(
            latency_ms,
            &[
                KeyValue::new("operation", operation.to_string()),
                KeyValue::new("backend", backend.as_str()),
            ],
        );
    }
}
```

## Error Handling Test Cases

```rust
// ggen-domain/tests/error_handling_tests.rs

#[tokio::test]
async fn test_fallback_on_v2_failure() {
    let adapter = DualBackendAdapter::new(
        v1_config(),
        v2_config(),
        BackendStrategy::V2WithFallback,
    ).unwrap();

    // Simulate V2 failure
    mock_v2_to_fail();

    // Should automatically fall back to V1
    let result = adapter.search(&test_query()).await;
    assert!(result.is_ok());
    assert_eq!(result.unwrap().backend_used, BackendType::V1);
}

#[tokio::test]
async fn test_both_backends_fail() {
    let adapter = DualBackendAdapter::new(
        v1_config(),
        v2_config(),
        BackendStrategy::V2WithFallback,
    ).unwrap();

    // Simulate both backends failing
    mock_v1_to_fail();
    mock_v2_to_fail();

    let result = adapter.search(&test_query()).await;
    assert!(result.is_err());

    // Should return composite error
    let error = result.unwrap_err();
    assert!(error.to_string().contains("Both backends failed"));
}

#[tokio::test]
async fn test_retry_with_backoff() {
    let mut attempt = 0;
    let result = retry_with_backoff(
        || async {
            attempt += 1;
            if attempt < 3 {
                Err(MarketplaceError::NetworkError("Temporary failure".to_string()))
            } else {
                Ok("Success")
            }
        },
        5,
        100,
        1000,
    ).await;

    assert!(result.is_ok());
    assert_eq!(attempt, 3);
}

#[tokio::test]
async fn test_circuit_breaker() {
    let breaker = CircuitBreaker::new(3, 1000);

    // First 3 failures should execute
    for _ in 0..3 {
        let result = breaker.execute(|| async {
            Err::<(), _>(MarketplaceError::NetworkError("Failure".to_string()))
        }).await;
        assert!(result.is_err());
    }

    // 4th attempt should be rejected (circuit open)
    let result = breaker.execute(|| async {
        Ok::<_, MarketplaceError>(())
    }).await;
    assert!(result.is_err());
    assert!(matches!(result.unwrap_err(), MarketplaceError::BackendNotAvailable(_)));
}
```

## Success Criteria

| Criterion | Target | Validation |
|-----------|--------|------------|
| Automatic fallback success | >99% | V2 failures don't break workflows |
| Error message clarity | 100% | All errors have user-friendly messages |
| Retryable error detection | 100% | Network/timeout errors retry automatically |
| Circuit breaker protection | 100% | High failure rate opens circuit |
| Error metrics coverage | 100% | All error types tracked in OpenTelemetry |
| Rollback on error | <1 hour | Config change restores service |
