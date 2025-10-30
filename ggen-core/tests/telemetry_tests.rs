//! Tests for OpenTelemetry instrumentation
//!
//! These tests verify that ggen operations generate correct spans
//! that can be validated by clnrm tests.

use ggen_core::telemetry::{init_telemetry, shutdown_telemetry, TelemetryConfig};
use ggen_core::{RegistryClient, SearchParams};

#[tokio::test]
async fn test_telemetry_initialization() {
    let config = TelemetryConfig {
        endpoint: "http://localhost:4318".to_string(),
        service_name: "ggen-test".to_string(),
        sample_ratio: 1.0,
        console_output: false,
    };

    // Should initialize without error
    let result = init_telemetry(config);
    assert!(result.is_ok());

    shutdown_telemetry();
}

#[tokio::test]
#[ignore] // Requires OTLP collector running
async fn test_registry_search_generates_spans() {
    // Initialize telemetry
    let config = TelemetryConfig {
        endpoint: "http://localhost:4318".to_string(),
        service_name: "ggen-test".to_string(),
        sample_ratio: 1.0,
        console_output: false,
    };
    init_telemetry(config).expect("Failed to init telemetry");

    // Create registry client and perform search
    let client = RegistryClient::new().expect("Failed to create client");

    // This should generate a "ggen.market.search" span
    let _results = client.search("rust").await;

    shutdown_telemetry();

    // Note: Actual span verification would be done by clnrm tests
    // by querying the OTLP collector for trace data
}

#[tokio::test]
#[ignore] // Requires OTLP collector running
async fn test_registry_resolve_generates_spans() {
    let config = TelemetryConfig {
        endpoint: "http://localhost:4318".to_string(),
        service_name: "ggen-test".to_string(),
        sample_ratio: 1.0,
        console_output: false,
    };
    init_telemetry(config).expect("Failed to init telemetry");

    let client = RegistryClient::new().expect("Failed to create client");

    // This should generate a "ggen.market.resolve" span
    let _resolved = client.resolve("test-pack", None).await;

    shutdown_telemetry();
}

#[tokio::test]
#[ignore] // Requires OTLP collector running
async fn test_advanced_search_generates_spans() {
    let config = TelemetryConfig {
        endpoint: "http://localhost:4318".to_string(),
        service_name: "ggen-test".to_string(),
        sample_ratio: 1.0,
        console_output: false,
    };
    init_telemetry(config).expect("Failed to init telemetry");

    let client = RegistryClient::new().expect("Failed to create client");

    let params = SearchParams {
        query: "rust",
        category: Some("web"),
        keyword: None,
        author: None,
        stable_only: false,
        limit: 10,
    };

    // This should generate a "ggen.market.advanced_search" span
    let _results = client.advanced_search(&params).await;

    shutdown_telemetry();
}

#[test]
fn test_telemetry_config_from_env() {
    // Test that endpoint can be read from environment
    std::env::set_var("OTEL_EXPORTER_OTLP_ENDPOINT", "http://custom:4318");

    let config = TelemetryConfig::default();
    assert_eq!(config.endpoint, "http://custom:4318");

    std::env::remove_var("OTEL_EXPORTER_OTLP_ENDPOINT");
}

#[test]
fn test_telemetry_config_custom_sample_ratio() {
    let config = TelemetryConfig {
        endpoint: "http://localhost:4318".to_string(),
        service_name: "test".to_string(),
        sample_ratio: 0.1,
        console_output: false,
    };

    assert_eq!(config.sample_ratio, 0.1);
}

/// Integration test: Verify span attributes
#[tokio::test]
#[ignore] // Requires OTLP collector and manual verification
async fn test_span_attributes_correctness() {
    let config = TelemetryConfig {
        endpoint: "http://localhost:4318".to_string(),
        service_name: "ggen-test".to_string(),
        sample_ratio: 1.0,
        console_output: true, // Enable console for debugging
    };
    init_telemetry(config).expect("Failed to init telemetry");

    let client = RegistryClient::new().expect("Failed to create client");

    // Perform search - should create span with:
    // - span name: "ggen.market.search"
    // - attributes: query, result_count
    let results = client.search("rust").await;

    if let Ok(results) = results {
        println!("Search returned {} results", results.len());
    }

    // Perform resolve - should create span with:
    // - span name: "ggen.market.resolve"
    // - attributes: pack_id, version, resolved_version
    let _resolved = client.resolve("test-pack", Some("1.0.0")).await;

    shutdown_telemetry();

    // Manual verification:
    // 1. Check OTLP collector (e.g., Jaeger UI at http://localhost:16686)
    // 2. Verify spans have correct names and attributes
    // 3. Verify span hierarchy (fetch_index as child of search/resolve)
}
