//! GCP integration tests for production-grade service clients
//!
//! These tests verify the TAI GCP integration with real GCP services.
//! Run with: `cargo test --test gcp_integration_tests -- --test-threads=1 --nocapture`

use tai_gcp::gcp_auth::{AdcAuth, GcpAuthStrategy};
use tai_gcp::gcp_config::GcpConfig;
use tai_gcp::gcp_errors::{GcpError, GcpErrorKind};

/// Test that we can load configuration from environment
#[tokio::test]
async fn test_load_config_from_environment() {
    // Set required environment variables
    std::env::set_var("GCP_PROJECT_ID", "test-project");
    std::env::set_var("GCP_REGION", "us-central1");

    let result = GcpConfig::from_environment();
    assert!(result.is_ok(), "Failed to load config from environment");

    let config = result.unwrap();
    assert_eq!(config.project_id, "test-project");
    assert_eq!(config.region, "us-central1");
}

/// Test that missing GCP_PROJECT_ID returns appropriate error
#[tokio::test]
async fn test_missing_project_id() {
    // Ensure GCP_PROJECT_ID is not set
    std::env::remove_var("GCP_PROJECT_ID");

    let result = GcpConfig::from_environment();
    assert!(result.is_err(), "Should fail without GCP_PROJECT_ID");

    let error = result.unwrap_err();
    assert_eq!(error.kind(), GcpErrorKind::Permanent);
}

/// Test that configuration can be serialized and deserialized
#[test]
fn test_config_serde() {
    let config = GcpConfig::new("test-project".to_string(), "europe-west1".to_string());

    let json = serde_json::to_string(&config).expect("serialization failed");
    let deserialized: GcpConfig = serde_json::from_str(&json).expect("deserialization failed");

    assert_eq!(deserialized.project_id, config.project_id);
    assert_eq!(deserialized.region, config.region);
}

/// Test configuration timeout settings
#[test]
fn test_config_timeouts() {
    let mut config = GcpConfig::default();

    assert_eq!(config.get_timeout("run").as_secs(), 30);
    assert_eq!(config.get_timeout("firestore").as_secs(), 5);

    config.set_timeout("run", std::time::Duration::from_secs(60));
    assert_eq!(config.get_timeout("run").as_secs(), 60);
}

/// Test error kind classification
#[test]
fn test_error_classification() {
    let error = GcpError::transient("service unavailable")
        .with_gcp_code("UNAVAILABLE".to_string());

    assert_eq!(error.kind(), GcpErrorKind::Transient);
    assert!(error.is_retryable());

    let error = GcpError::permanent("not found")
        .with_gcp_code("NOT_FOUND".to_string());

    assert!(!error.is_retryable());
}

/// Test error context enrichment
#[test]
fn test_error_context() {
    use tai_gcp::gcp_errors::GcpErrorContext;

    let context = GcpErrorContext::default()
        .with_project("my-project".to_string())
        .with_operation("firestore.write".to_string())
        .with_resource("/documents/user-123".to_string())
        .with_attempt(1)
        .with_http_status(503);

    let error = GcpError::transient("service unavailable").with_context(context);

    let error_str = format!("{}", error);
    assert!(error_str.contains("my-project"));
    assert!(error_str.contains("firestore.write"));
    assert!(error_str.contains("503"));
}

/// Test ADC authentication strategy creation
#[test]
fn test_adc_auth_creation() {
    let adc = AdcAuth::new();
    assert_eq!(adc.metadata_endpoint, "http://metadata.google.internal/computeMetadata/v1");
}

/// Test ADC with custom key path
#[test]
fn test_adc_auth_with_key_path() {
    std::env::set_var("GOOGLE_APPLICATION_CREDENTIALS", "/path/to/key.json");
    let adc = AdcAuth::with_key_path("/path/to/key.json".to_string());
    assert_eq!(adc.key_path, Some("/path/to/key.json".to_string()));
}

/// Test configuration failover regions
#[test]
fn test_config_failover_regions() {
    let mut config = GcpConfig::default();

    config.add_failover_region("us-east1".to_string());
    config.add_failover_region("europe-west1".to_string());

    assert_eq!(config.failover_regions.len(), 2);

    // Adding duplicate should not increase count
    config.add_failover_region("us-east1".to_string());
    assert_eq!(config.failover_regions.len(), 2);
}

/// Test configuration metrics enablement
#[test]
fn test_config_metrics() {
    let mut config = GcpConfig::default();
    assert!(!config.metrics_enabled);

    config.enable_metrics();
    assert!(config.metrics_enabled);
}

/// Test configuration debug logging
#[test]
fn test_config_debug_logging() {
    let mut config = GcpConfig::default();
    assert!(!config.debug_logging);

    config.enable_debug_logging();
    assert!(config.debug_logging);
}

/// Test HTTP status code classification
#[test]
fn test_http_status_classification() {
    use tai_gcp::gcp_errors::classify_http_status;

    assert_eq!(classify_http_status(401), GcpErrorKind::AuthenticationFailure);
    assert_eq!(classify_http_status(429), GcpErrorKind::RateLimited);
    assert_eq!(classify_http_status(503), GcpErrorKind::Transient);
    assert_eq!(classify_http_status(404), GcpErrorKind::Permanent);
    assert_eq!(classify_http_status(500), GcpErrorKind::Transient);
}

/// Test GCP error code classification
#[test]
fn test_gcp_error_code_classification() {
    use tai_gcp::gcp_errors::classify_gcp_error;

    let (kind, _msg) = classify_gcp_error("UNAUTHENTICATED", "auth failed");
    assert_eq!(kind, GcpErrorKind::AuthenticationFailure);

    let (kind, _msg) = classify_gcp_error("UNAVAILABLE", "service down");
    assert_eq!(kind, GcpErrorKind::Transient);

    let (kind, _msg) = classify_gcp_error("NOT_FOUND", "resource missing");
    assert_eq!(kind, GcpErrorKind::Permanent);

    let (kind, _msg) = classify_gcp_error("RESOURCE_EXHAUSTED", "quota exceeded");
    assert_eq!(kind, GcpErrorKind::RateLimited);
}

/// Test error kind display
#[test]
fn test_error_kind_display() {
    assert_eq!(format!("{}", GcpErrorKind::Transient), "transient");
    assert_eq!(format!("{}", GcpErrorKind::Permanent), "permanent");
    assert_eq!(format!("{}", GcpErrorKind::RateLimited), "rate_limited");
    assert_eq!(format!("{}", GcpErrorKind::AuthenticationFailure), "authentication_failure");
    assert_eq!(format!("{}", GcpErrorKind::QuotaExceeded), "quota_exceeded");
}

/// Test error creation and display
#[test]
fn test_error_display() {
    let error = GcpError::transient("service unavailable")
        .with_gcp_code("UNAVAILABLE".to_string());

    let display = format!("{}", error);
    assert!(display.contains("transient"));
    assert!(display.contains("service unavailable"));
    assert!(display.contains("UNAVAILABLE"));
}

/// Test workload identity configuration creation
#[test]
fn test_workload_identity_config_creation() {
    use tai_gcp::gcp_config::WorkloadIdentityConfig;

    let wi = WorkloadIdentityConfig {
        ksa_name: "default".to_string(),
        gsa_email: "gsa@my-project.iam.gserviceaccount.com".to_string(),
    };

    assert_eq!(wi.ksa_name, "default");
    assert_eq!(wi.gsa_email, "gsa@my-project.iam.gserviceaccount.com");
}

/// Test configuration TOML serialization
#[test]
fn test_config_toml_serde() {
    let config = GcpConfig::new("test-project".to_string(), "us-central1".to_string());

    let toml = toml::to_string(&config).expect("toml serialization failed");
    let deserialized: GcpConfig = toml::from_str(&toml).expect("toml deserialization failed");

    assert_eq!(deserialized.project_id, config.project_id);
    assert_eq!(deserialized.region, config.region);
}

/// Test error retryability
#[test]
fn test_error_retryability() {
    assert!(GcpError::transient("error").is_retryable());
    assert!(GcpError::quota_exceeded("error").is_retryable());
    assert!(GcpError::rate_limited("error").is_retryable());
    assert!(!GcpError::permanent("error").is_retryable());
    assert!(!GcpError::authentication_failure("error").is_retryable());
}

/// Test error context builder pattern
#[test]
fn test_error_context_builder() {
    use tai_gcp::gcp_errors::GcpErrorContext;

    let context = GcpErrorContext::with_project("my-project".to_string())
        .with_operation("firestore.write".to_string())
        .with_resource("/documents/user-123".to_string());

    assert_eq!(context.project_id, Some("my-project".to_string()));
    assert_eq!(context.operation, Some("firestore.write".to_string()));
    assert_eq!(context.resource, Some("/documents/user-123".to_string()));
}

/// Test multiple error kinds are correctly classified
#[test]
fn test_multiple_error_kinds() {
    let transient = GcpError::transient("error");
    let permanent = GcpError::permanent("error");
    let quota = GcpError::quota_exceeded("error");
    let rate = GcpError::rate_limited("error");
    let auth = GcpError::authentication_failure("error");

    assert_eq!(transient.kind(), GcpErrorKind::Transient);
    assert_eq!(permanent.kind(), GcpErrorKind::Permanent);
    assert_eq!(quota.kind(), GcpErrorKind::QuotaExceeded);
    assert_eq!(rate.kind(), GcpErrorKind::RateLimited);
    assert_eq!(auth.kind(), GcpErrorKind::AuthenticationFailure);
}

/// Test configuration with environment variable override for timeouts
#[tokio::test]
async fn test_config_timeout_from_env() {
    std::env::set_var("GCP_PROJECT_ID", "test-project");
    std::env::set_var("GCP_TIMEOUT_RUN", "60");
    std::env::set_var("GCP_TIMEOUT_FIRESTORE", "10");

    let config = GcpConfig::from_environment().expect("failed to load config");
    assert_eq!(config.get_timeout("run").as_secs(), 60);
    assert_eq!(config.get_timeout("firestore").as_secs(), 10);
}

#[cfg(test)]
mod unit_tests {
    use super::*;

    /// Test that error context displays correctly
    #[test]
    fn test_empty_context_display() {
        use tai_gcp::gcp_errors::GcpErrorContext;

        let context = GcpErrorContext::default();
        let display = format!("{}", context);
        assert_eq!(display, "(no context)");
    }

    /// Test error context with single field
    #[test]
    fn test_context_single_field() {
        use tai_gcp::gcp_errors::GcpErrorContext;

        let context = GcpErrorContext::default()
            .with_project("my-project".to_string());

        let display = format!("{}", context);
        assert!(display.contains("project_id=my-project"));
    }

    /// Test error context with multiple fields
    #[test]
    fn test_context_multiple_fields() {
        use tai_gcp::gcp_errors::GcpErrorContext;

        let context = GcpErrorContext::default()
            .with_project("my-project".to_string())
            .with_operation("firestore.write".to_string())
            .with_http_status(503);

        let display = format!("{}", context);
        assert!(display.contains("project_id=my-project"));
        assert!(display.contains("operation=firestore.write"));
        assert!(display.contains("http_status=503"));
    }
}
