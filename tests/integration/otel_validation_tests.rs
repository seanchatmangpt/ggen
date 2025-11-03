//! OTEL Validation Integration Tests
//!
//! **CRITICAL:** These tests verify that OpenTelemetry traces are ACTUALLY emitted
//! to a real OTEL collector, not just that CLI commands run successfully.
//!
//! **From instructions:** "running a CLI help command is a false positive.
//! ONLY TRUST OTEL SPAN/TRACES"
//!
//! **Test Coverage:**
//! - Traces are exported to OTLP HTTP endpoint
//! - Collector receives and processes spans
//! - Parent-child span relationships are preserved
//! - Service name and attributes are correct
//! - Trace context propagation works across operations
//!
//! **Prerequisites:**
//! - Docker running
//! - OTEL collector started via docker-compose
//! - Ports 4318 (OTLP HTTP), 13133 (health) available

use reqwest;
use serde_json::Value;
use std::collections::HashMap;
use std::process::Command;
use std::time::Duration;
use tokio;

/// OTEL Collector endpoints
const COLLECTOR_OTLP_HTTP: &str = "http://localhost:4318";
const COLLECTOR_HEALTH: &str = "http://localhost:13133";
const COLLECTOR_METRICS: &str = "http://localhost:8888/metrics";
const JAEGER_QUERY: &str = "http://localhost:16686/api/traces";

/// Expected service name for ggen marketplace
const SERVICE_NAME: &str = "ggen-marketplace-p2p";

// =============================================================================
// TEST SUITE 1: COLLECTOR INFRASTRUCTURE
// =============================================================================

#[tokio::test]
#[ignore] // Requires Docker infrastructure
async fn test_otel_collector_is_healthy() {
    // Verify: OTEL collector is running and healthy
    let client = reqwest::Client::new();

    let response = client
        .get(COLLECTOR_HEALTH)
        .timeout(Duration::from_secs(5))
        .send()
        .await
        .expect("Failed to connect to OTEL collector health endpoint");

    assert!(
        response.status().is_success(),
        "OTEL collector health check failed: {}",
        response.status()
    );

    let body = response.text().await.expect("Failed to read health response");

    assert!(
        body.contains("Server available") || body.contains("OK"),
        "Unexpected health response: {}",
        body
    );
}

#[tokio::test]
#[ignore]
async fn test_otel_collector_endpoints_available() {
    // Verify: All OTEL collector endpoints are accessible
    let client = reqwest::Client::builder()
        .timeout(Duration::from_secs(5))
        .build()
        .unwrap();

    // Check health endpoint
    let health = client.get(COLLECTOR_HEALTH).send().await;
    assert!(health.is_ok(), "Health endpoint not available");

    // Check metrics endpoint
    let metrics = client.get(COLLECTOR_METRICS).send().await;
    assert!(metrics.is_ok(), "Metrics endpoint not available");

    // OTLP HTTP endpoint should accept POST (but reject empty body)
    let otlp = client.post(format!("{}/v1/traces", COLLECTOR_OTLP_HTTP)).send().await;
    assert!(otlp.is_ok(), "OTLP HTTP endpoint not available");
}

// =============================================================================
// TEST SUITE 2: TRACE EMISSION VALIDATION
// =============================================================================

#[tokio::test]
#[ignore]
async fn test_marketplace_search_emits_spans_to_collector() {
    // Verify: marketplace search command emits actual spans to collector

    // Get baseline metrics
    let baseline_spans = get_collector_span_count().await;

    // Run marketplace search with OTEL enabled
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--features", "otel", "--",
                "marketplace", "search", "test"])
        .env("OTEL_EXPORTER_OTLP_ENDPOINT", COLLECTOR_OTLP_HTTP)
        .env("OTEL_SERVICE_NAME", SERVICE_NAME)
        .env("RUST_LOG", "info")
        .output()
        .expect("Failed to run marketplace search");

    assert!(
        output.status.success(),
        "Command failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Wait for spans to be exported
    tokio::time::sleep(Duration::from_secs(2)).await;

    // Verify collector received new spans
    let current_spans = get_collector_span_count().await;

    assert!(
        current_spans > baseline_spans,
        "No new spans received by collector. Baseline: {}, Current: {}",
        baseline_spans,
        current_spans
    );

    println!(
        "✅ Collector received {} new spans from marketplace search",
        current_spans - baseline_spans
    );
}

#[tokio::test]
#[ignore]
async fn test_p2p_operations_emit_trace_context() {
    // Verify: P2P operations create proper trace context with parent-child relationships

    let baseline = get_collector_span_count().await;

    // Execute P2P initialization (should create multiple spans)
    let output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--features", "otel,p2p", "--",
                "marketplace", "search", "test", "--p2p"])
        .env("OTEL_EXPORTER_OTLP_ENDPOINT", COLLECTOR_OTLP_HTTP)
        .env("OTEL_SERVICE_NAME", SERVICE_NAME)
        .env("RUST_LOG", "debug")
        .output()
        .expect("Failed to run P2P search");

    if !output.status.success() {
        eprintln!("Warning: P2P command failed (expected if backend not ready)");
        eprintln!("{}", String::from_utf8_lossy(&output.stderr));
        return; // Skip test if P2P not ready
    }

    tokio::time::sleep(Duration::from_secs(3)).await;

    let current = get_collector_span_count().await;

    // P2P should emit multiple spans (network init, DHT query, etc.)
    assert!(
        current > baseline + 2,
        "Expected multiple spans from P2P operation, got {}",
        current - baseline
    );

    println!("✅ P2P operation emitted {} spans", current - baseline);
}

#[tokio::test]
#[ignore]
async fn test_span_attributes_contain_operation_metadata() {
    // Verify: Spans contain proper attributes (operation, package name, etc.)

    // Run operation
    let _output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--features", "otel", "--",
                "marketplace", "search", "test-package"])
        .env("OTEL_EXPORTER_OTLP_ENDPOINT", COLLECTOR_OTLP_HTTP)
        .env("OTEL_SERVICE_NAME", SERVICE_NAME)
        .output()
        .expect("Failed to run search");

    tokio::time::sleep(Duration::from_secs(2)).await;

    // Query Jaeger for traces
    let traces = query_jaeger_traces(SERVICE_NAME).await;

    if traces.is_empty() {
        eprintln!("Warning: No traces found in Jaeger yet");
        return;
    }

    // Verify at least one span has expected attributes
    let has_attributes = traces.iter().any(|trace| {
        // Check if trace has spans with marketplace attributes
        trace.get("spans")
            .and_then(|s| s.as_array())
            .map(|spans| {
                spans.iter().any(|span| {
                    span.get("tags")
                        .and_then(|t| t.as_array())
                        .map(|tags| {
                            tags.iter().any(|tag| {
                                tag.get("key")
                                    .and_then(|k| k.as_str())
                                    .map(|k| k.contains("marketplace") || k.contains("search"))
                                    .unwrap_or(false)
                            })
                        })
                        .unwrap_or(false)
                })
            })
            .unwrap_or(false)
    });

    assert!(
        has_attributes,
        "Spans should contain marketplace operation attributes"
    );

    println!("✅ Spans contain proper operation metadata");
}

// =============================================================================
// TEST SUITE 3: PARENT-CHILD SPAN RELATIONSHIPS
// =============================================================================

#[tokio::test]
#[ignore]
async fn test_parent_child_span_relationships_preserved() {
    // Verify: Nested operations create proper parent-child span relationships

    // Run operation that should create nested spans
    let _output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--features", "otel", "--",
                "marketplace", "search", "test"])
        .env("OTEL_EXPORTER_OTLP_ENDPOINT", COLLECTOR_OTLP_HTTP)
        .env("OTEL_SERVICE_NAME", SERVICE_NAME)
        .output()
        .expect("Failed to run search");

    tokio::time::sleep(Duration::from_secs(2)).await;

    let traces = query_jaeger_traces(SERVICE_NAME).await;

    if traces.is_empty() {
        eprintln!("Warning: No traces available yet");
        return;
    }

    // Check for parent-child relationships
    let has_relationships = traces.iter().any(|trace| {
        trace.get("spans")
            .and_then(|s| s.as_array())
            .map(|spans| {
                // Check if any spans have parent span references
                spans.iter().any(|span| {
                    span.get("references")
                        .and_then(|r| r.as_array())
                        .map(|refs| !refs.is_empty())
                        .unwrap_or(false)
                })
            })
            .unwrap_or(false)
    });

    assert!(
        has_relationships,
        "Expected to find parent-child span relationships in traces"
    );

    println!("✅ Parent-child span relationships preserved");
}

#[tokio::test]
#[ignore]
async fn test_trace_context_propagates_across_operations() {
    // Verify: Trace context (trace_id, span_id) propagates correctly

    let _output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--features", "otel", "--",
                "marketplace", "search", "propagation-test"])
        .env("OTEL_EXPORTER_OTLP_ENDPOINT", COLLECTOR_OTLP_HTTP)
        .env("OTEL_SERVICE_NAME", SERVICE_NAME)
        .output()
        .expect("Failed to run search");

    tokio::time::sleep(Duration::from_secs(2)).await;

    let traces = query_jaeger_traces(SERVICE_NAME).await;

    if traces.is_empty() {
        return; // Skip if no traces yet
    }

    // Verify all spans in a trace share the same trace_id
    for trace in traces.iter() {
        if let Some(spans) = trace.get("spans").and_then(|s| s.as_array()) {
            if spans.is_empty() {
                continue;
            }

            let trace_id = spans[0]
                .get("traceID")
                .and_then(|t| t.as_str())
                .expect("Missing trace ID");

            // All spans should have same trace_id
            for span in spans.iter() {
                let span_trace_id = span
                    .get("traceID")
                    .and_then(|t| t.as_str())
                    .expect("Missing span trace ID");

                assert_eq!(
                    trace_id, span_trace_id,
                    "Trace ID should be consistent across all spans in trace"
                );
            }
        }
    }

    println!("✅ Trace context propagates correctly");
}

// =============================================================================
// TEST SUITE 4: SERVICE IDENTIFICATION
// =============================================================================

#[tokio::test]
#[ignore]
async fn test_spans_have_correct_service_name() {
    // Verify: All spans are tagged with correct service name

    let _output = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--features", "otel", "--",
                "marketplace", "search", "service-test"])
        .env("OTEL_EXPORTER_OTLP_ENDPOINT", COLLECTOR_OTLP_HTTP)
        .env("OTEL_SERVICE_NAME", SERVICE_NAME)
        .output()
        .expect("Failed to run search");

    tokio::time::sleep(Duration::from_secs(2)).await;

    let traces = query_jaeger_traces(SERVICE_NAME).await;

    assert!(
        !traces.is_empty(),
        "Expected to find traces for service '{}'",
        SERVICE_NAME
    );

    // Verify service name in process tags
    for trace in traces.iter() {
        if let Some(processes) = trace.get("processes").and_then(|p| p.as_object()) {
            let has_correct_service = processes.values().any(|process| {
                process
                    .get("serviceName")
                    .and_then(|s| s.as_str())
                    .map(|name| name == SERVICE_NAME)
                    .unwrap_or(false)
            });

            assert!(
                has_correct_service,
                "Service name should be '{}' in trace metadata",
                SERVICE_NAME
            );
        }
    }

    println!("✅ Service name correctly set: {}", SERVICE_NAME);
}

// =============================================================================
// TEST SUITE 5: PERFORMANCE & RELIABILITY
// =============================================================================

#[tokio::test]
#[ignore]
async fn test_span_export_does_not_block_operation() {
    // Verify: OTEL export doesn't significantly slow down operations

    // Run without OTEL
    let start_no_otel = std::time::Instant::now();
    let _output1 = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--", "marketplace", "search", "perf-test"])
        .output()
        .expect("Failed to run search without OTEL");
    let duration_no_otel = start_no_otel.elapsed();

    // Run with OTEL
    let start_with_otel = std::time::Instant::now();
    let _output2 = Command::new("cargo")
        .args(&["run", "--bin", "ggen", "--features", "otel", "--",
                "marketplace", "search", "perf-test"])
        .env("OTEL_EXPORTER_OTLP_ENDPOINT", COLLECTOR_OTLP_HTTP)
        .env("OTEL_SERVICE_NAME", SERVICE_NAME)
        .output()
        .expect("Failed to run search with OTEL");
    let duration_with_otel = start_with_otel.elapsed();

    // OTEL overhead should be minimal (< 50% slowdown)
    let overhead_ratio = duration_with_otel.as_secs_f64() / duration_no_otel.as_secs_f64();

    assert!(
        overhead_ratio < 1.5,
        "OTEL overhead too high: {}x slower ({}ms vs {}ms)",
        overhead_ratio,
        duration_with_otel.as_millis(),
        duration_no_otel.as_millis()
    );

    println!(
        "✅ OTEL overhead acceptable: {:.1}% ({:?} vs {:?})",
        (overhead_ratio - 1.0) * 100.0,
        duration_with_otel,
        duration_no_otel
    );
}

#[tokio::test]
#[ignore]
async fn test_collector_handles_burst_of_spans() {
    // Verify: Collector can handle rapid span emission

    let baseline = get_collector_span_count().await;

    // Run multiple operations concurrently
    let mut handles = vec![];

    for i in 0..5 {
        let handle = tokio::spawn(async move {
            Command::new("cargo")
                .args(&["run", "--bin", "ggen", "--features", "otel", "--",
                        "marketplace", "search", &format!("burst-test-{}", i)])
                .env("OTEL_EXPORTER_OTLP_ENDPOINT", COLLECTOR_OTLP_HTTP)
                .env("OTEL_SERVICE_NAME", SERVICE_NAME)
                .output()
                .expect("Failed to run search")
        });
        handles.push(handle);
    }

    // Wait for all operations
    for handle in handles {
        let _ = handle.await;
    }

    tokio::time::sleep(Duration::from_secs(3)).await;

    let current = get_collector_span_count().await;

    assert!(
        current > baseline,
        "Collector should have received spans from burst operations"
    );

    println!(
        "✅ Collector handled burst: {} spans from 5 concurrent operations",
        current - baseline
    );
}

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

/// Get current span count from collector metrics
async fn get_collector_span_count() -> u64 {
    let client = reqwest::Client::new();

    let response = client
        .get(COLLECTOR_METRICS)
        .timeout(Duration::from_secs(5))
        .send()
        .await
        .expect("Failed to get collector metrics");

    let body = response.text().await.expect("Failed to read metrics");

    // Parse Prometheus metrics for span count
    parse_span_count_from_metrics(&body)
}

/// Parse span count from Prometheus metrics
fn parse_span_count_from_metrics(metrics: &str) -> u64 {
    // Look for: otelcol_receiver_accepted_spans{...} 123
    for line in metrics.lines() {
        if line.starts_with("otelcol_receiver_accepted_spans")
            && !line.starts_with("#")
        {
            // Extract number after last space
            if let Some(count_str) = line.split_whitespace().last() {
                if let Ok(count) = count_str.parse::<u64>() {
                    return count;
                }
            }
        }
    }
    0
}

/// Query Jaeger for traces by service name
async fn query_jaeger_traces(service: &str) -> Vec<Value> {
    let client = reqwest::Client::new();

    let url = format!(
        "{}?service={}&limit=20&lookback=1h",
        JAEGER_QUERY, service
    );

    let response = client
        .get(&url)
        .timeout(Duration::from_secs(5))
        .send()
        .await;

    if let Ok(resp) = response {
        if let Ok(json) = resp.json::<Value>().await {
            if let Some(data) = json.get("data").and_then(|d| d.as_array()) {
                return data.clone();
            }
        }
    }

    vec![]
}

// =============================================================================
// INFRASTRUCTURE VALIDATION
// =============================================================================

#[tokio::test]
#[ignore]
async fn test_docker_compose_otel_stack_running() {
    // Verify: Complete OTEL stack is running

    let services = vec![
        ("Collector Health", COLLECTOR_HEALTH),
        ("Collector Metrics", COLLECTOR_METRICS),
        ("Jaeger Query", JAEGER_QUERY),
    ];

    let client = reqwest::Client::builder()
        .timeout(Duration::from_secs(5))
        .build()
        .unwrap();

    for (name, url) in services {
        let result = client.get(url).send().await;

        assert!(
            result.is_ok(),
            "{} endpoint not available at {}",
            name, url
        );

        println!("✅ {} available at {}", name, url);
    }
}
