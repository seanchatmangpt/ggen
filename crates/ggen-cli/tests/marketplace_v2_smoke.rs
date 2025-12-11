//! Smoke tests for marketplace-v2 CLI wiring.
//!
//! These tests validate that the marketplace noun is registered and routes to
//! the ggen-marketplace-v2 backend without panicking, providing basic,
//! behavior-focused assertions on the returned data.

use ggen_cli_lib::cmds::marketplace;

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn marketplace_search_returns_no_results_on_fresh_registry() {
    let output = marketplace::search(
        "hello".to_string(),
        None,
        None,
        None,
        None,
        None,
        Some(5),
        Some(0),
    )
    .expect("marketplace search should succeed on empty registry");

    // Behavior: empty registry yields zero results but echoes query and filters.
    assert_eq!(output.query, "hello");
    assert!(output.results.is_empty());
    assert_eq!(output.total, 0);
    assert_eq!(output.filters_applied.category, None);
    assert_eq!(output.filters_applied.author, None);
    assert_eq!(output.filters_applied.license, None);
    assert_eq!(output.filters_applied.min_quality, None);
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn marketplace_metrics_can_be_queried() {
    // Query metrics to ensure the noun is registered and collector is available.
    let metrics = marketplace::metrics(None).expect("metrics should be accessible");

    // Behavior: metrics fields are non-negative and structurally valid.
    assert!(metrics.searches.total >= 0);
    assert!(metrics.searches.successful <= metrics.searches.total);
    assert!(metrics.installations.total >= 0);
    assert!(metrics.validations >= 0);
    assert!(metrics.signature_verifications >= 0);
}
