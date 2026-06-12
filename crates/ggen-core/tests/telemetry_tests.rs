#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]
#![allow(
    dead_code,
    unused_imports,
    unused_variables,
    deprecated,
    clippy::all,
    unused_mut
)]

//! Tests for OpenTelemetry instrumentation
//!
//! These tests verify that ggen operations generate correct spans
//! that can be validated by clnrm tests.

use ggen_core::registry::{RegistryClient, SearchParams};
use ggen_core::telemetry::{init_telemetry, shutdown_telemetry, TelemetryConfig};

#[tokio::test]
async fn test_telemetry_initialization() {
    let config = TelemetryConfig {
        endpoint: "http://localhost:4318".to_string(),
        service_name: "ggen-test".to_string(),
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

// Endpoint is read from `OTEL_EXPORTER_OTLP_ENDPOINT` only when the `otel`
// feature is enabled; without it `TelemetryConfig::default()` yields an empty
// endpoint (see `impl Default for TelemetryConfig` in src/telemetry.rs).
#[cfg(feature = "otel")]
#[test]
fn test_telemetry_config_from_env() {
    // Test that endpoint can be read from environment
    std::env::set_var("OTEL_EXPORTER_OTLP_ENDPOINT", "http://custom:4318");

    let config = TelemetryConfig::default();
    assert_eq!(config.endpoint, "http://custom:4318");

    std::env::remove_var("OTEL_EXPORTER_OTLP_ENDPOINT");
}

#[test]
fn test_telemetry_config_custom_service_name() {
    let config = TelemetryConfig {
        endpoint: "http://localhost:4318".to_string(),
        service_name: "custom-service".to_string(),
        console_output: true,
    };

    assert_eq!(config.service_name, "custom-service");
    assert_eq!(config.console_output, true);
}

/// Integration test: Verify span attributes
#[tokio::test]
#[ignore] // Requires OTLP collector and manual verification
async fn test_span_attributes_correctness() {
    let config = TelemetryConfig {
        endpoint: "http://localhost:4318".to_string(),
        service_name: "ggen-test".to_string(),
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
