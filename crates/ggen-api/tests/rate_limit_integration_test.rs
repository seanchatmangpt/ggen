//! Integration tests for rate limiting middleware
//!
//! Chicago TDD: State-based testing with real collaborators and behavior verification

use axum::{
    extract::State,
    http::StatusCode,
    routing::get,
    Router,
};
use ggen_api::middleware::{rate_limit_middleware, RateLimitConfig, RateLimiter, RateLimitBackend};
use std::sync::Arc;
use std::time::Duration;
use tokio::time::sleep;
use tower::ServiceExt;

async fn handler() -> &'static str {
    "OK"
}

fn create_test_app(limiter: Arc<RateLimiter>) -> Router {
    Router::new()
        .route("/api/test", get(handler))
        .layer(axum::middleware::from_fn_with_state(
            limiter.clone(),
            rate_limit_middleware,
        ))
        .with_state(limiter)
}

#[tokio::test]
async fn test_middleware_allows_requests_within_limit() {
    // Arrange: Create app with generous limits
    let config = RateLimitConfig {
        requests_per_second: 10,
        burst_size: 10,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = Arc::new(RateLimiter::new(config).await.unwrap());
    let app = create_test_app(limiter);

    // Act: Send request within limits
    let request = axum::http::Request::builder()
        .uri("/api/test")
        .header("X-API-Key", "test-key-123")
        .body(axum::body::Body::empty())
        .unwrap();

    let response = app.oneshot(request).await.unwrap();

    // Assert: Should succeed
    assert_eq!(response.status(), StatusCode::OK);
}

#[tokio::test]
async fn test_middleware_blocks_requests_exceeding_limit() {
    // Arrange: Create app with very restrictive limits
    let config = RateLimitConfig {
        requests_per_second: 1,
        burst_size: 1,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = Arc::new(RateLimiter::new(config).await.unwrap());

    // Act: Send first request (should succeed)
    let app1 = create_test_app(limiter.clone());
    let request1 = axum::http::Request::builder()
        .uri("/api/test")
        .header("X-API-Key", "test-key-456")
        .body(axum::body::Body::empty())
        .unwrap();
    let response1 = app1.oneshot(request1).await.unwrap();

    // Assert: First request succeeds
    assert_eq!(response1.status(), StatusCode::OK);

    // Act: Send second request immediately (should be blocked)
    let app2 = create_test_app(limiter.clone());
    let request2 = axum::http::Request::builder()
        .uri("/api/test")
        .header("X-API-Key", "test-key-456")
        .body(axum::body::Body::empty())
        .unwrap();
    let response2 = app2.oneshot(request2).await.unwrap();

    // Assert: Second request is rate limited
    assert_eq!(response2.status(), StatusCode::TOO_MANY_REQUESTS);
    assert!(response2.headers().contains_key("Retry-After"));
}

#[tokio::test]
async fn test_middleware_separates_limits_by_api_key() {
    // Arrange: Create app with single request limit
    let config = RateLimitConfig {
        requests_per_second: 1,
        burst_size: 1,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = Arc::new(RateLimiter::new(config).await.unwrap());

    // Act: Exhaust limit for first API key
    let app1 = create_test_app(limiter.clone());
    let request1 = axum::http::Request::builder()
        .uri("/api/test")
        .header("X-API-Key", "key-alpha")
        .body(axum::body::Body::empty())
        .unwrap();
    let response1 = app1.oneshot(request1).await.unwrap();
    assert_eq!(response1.status(), StatusCode::OK);

    // Act: Try different API key
    let app2 = create_test_app(limiter.clone());
    let request2 = axum::http::Request::builder()
        .uri("/api/test")
        .header("X-API-Key", "key-beta")
        .body(axum::body::Body::empty())
        .unwrap();
    let response2 = app2.oneshot(request2).await.unwrap();

    // Assert: Different API key has independent limit
    assert_eq!(response2.status(), StatusCode::OK);
}

#[tokio::test]
async fn test_middleware_refills_tokens_after_delay() {
    // Arrange: Create app with fast refill
    let config = RateLimitConfig {
        requests_per_second: 10, // 10 per second = 100ms per token
        burst_size: 1,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = Arc::new(RateLimiter::new(config).await.unwrap());

    // Act: Exhaust limit
    let app1 = create_test_app(limiter.clone());
    let request1 = axum::http::Request::builder()
        .uri("/api/test")
        .header("X-API-Key", "refill-test")
        .body(axum::body::Body::empty())
        .unwrap();
    let response1 = app1.oneshot(request1).await.unwrap();
    assert_eq!(response1.status(), StatusCode::OK);

    // Verify limit is exhausted
    let app2 = create_test_app(limiter.clone());
    let request2 = axum::http::Request::builder()
        .uri("/api/test")
        .header("X-API-Key", "refill-test")
        .body(axum::body::Body::empty())
        .unwrap();
    let response2 = app2.oneshot(request2).await.unwrap();
    assert_eq!(response2.status(), StatusCode::TOO_MANY_REQUESTS);

    // Wait for refill (200ms = 2 tokens at 10/sec)
    sleep(Duration::from_millis(200)).await;

    // Act: Try again after refill
    let app3 = create_test_app(limiter.clone());
    let request3 = axum::http::Request::builder()
        .uri("/api/test")
        .header("X-API-Key", "refill-test")
        .body(axum::body::Body::empty())
        .unwrap();
    let response3 = app3.oneshot(request3).await.unwrap();

    // Assert: Should succeed after refill
    assert_eq!(response3.status(), StatusCode::OK);
}

#[tokio::test]
async fn test_middleware_response_includes_rate_limit_headers() {
    // Arrange: Create app with limit of 1
    let config = RateLimitConfig {
        requests_per_second: 1,
        burst_size: 1,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = Arc::new(RateLimiter::new(config).await.unwrap());

    // Act: Exhaust limit
    let app1 = create_test_app(limiter.clone());
    let request1 = axum::http::Request::builder()
        .uri("/api/test")
        .header("X-API-Key", "header-test")
        .body(axum::body::Body::empty())
        .unwrap();
    app1.oneshot(request1).await.unwrap();

    // Act: Send request that exceeds limit
    let app2 = create_test_app(limiter.clone());
    let request2 = axum::http::Request::builder()
        .uri("/api/test")
        .header("X-API-Key", "header-test")
        .body(axum::body::Body::empty())
        .unwrap();
    let response = app2.oneshot(request2).await.unwrap();

    // Assert: Check for rate limit headers
    assert_eq!(response.status(), StatusCode::TOO_MANY_REQUESTS);
    let headers = response.headers();

    assert!(headers.contains_key("Retry-After"));
    assert!(headers.contains_key("X-RateLimit-Limit"));
    assert!(headers.contains_key("X-RateLimit-Remaining"));

    // Verify Retry-After is a positive integer
    if let Some(retry_after) = headers.get("Retry-After") {
        let retry_value: u64 = retry_after.to_str().unwrap().parse().unwrap();
        assert!(retry_value > 0);
    }
}

#[tokio::test]
async fn test_burst_handling_allows_rapid_requests() {
    // Arrange: Create app with burst capacity
    let config = RateLimitConfig {
        requests_per_second: 2,
        burst_size: 5,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = Arc::new(RateLimiter::new(config).await.unwrap());

    // Act: Send burst of 5 requests rapidly
    let mut success_count = 0;
    for i in 0..5 {
        let app = create_test_app(limiter.clone());
        let request = axum::http::Request::builder()
            .uri("/api/test")
            .header("X-API-Key", "burst-test")
            .body(axum::body::Body::empty())
            .unwrap();

        let response = app.oneshot(request).await.unwrap();
        if response.status() == StatusCode::OK {
            success_count += 1;
        }
    }

    // Assert: All 5 burst requests should succeed
    assert_eq!(success_count, 5, "Expected all burst requests to succeed");

    // Act: 6th request should be blocked
    let app_extra = create_test_app(limiter.clone());
    let request_extra = axum::http::Request::builder()
        .uri("/api/test")
        .header("X-API-Key", "burst-test")
        .body(axum::body::Body::empty())
        .unwrap();
    let response_extra = app_extra.oneshot(request_extra).await.unwrap();

    // Assert: Should be rate limited
    assert_eq!(response_extra.status(), StatusCode::TOO_MANY_REQUESTS);
}
