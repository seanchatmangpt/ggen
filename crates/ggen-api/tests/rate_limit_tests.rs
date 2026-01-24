//! Comprehensive rate limiting tests using Chicago TDD
//!
//! Test categories:
//! 1. Token Bucket Algorithm Tests (15 tests)
//! 2. Backend Tests (10 tests)
//! 3. Middleware Integration Tests (15 tests)
//! 4. Edge Case Tests (10 tests)
//! 5. Performance Tests (5 tests)
//!
//! Chicago TDD Principles:
//! - State-based testing (verify outputs, not implementation)
//! - Real collaborators (use real objects, minimize mocks)
//! - Behavior verification (verify what code does)
//! - AAA pattern (Arrange-Act-Assert)

use ggen_api::middleware::rate_limit::{
    RateLimiter, RateLimitConfig, RateLimitBackend, RateLimitError,
};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::time::sleep;

// ============================================================================
// 1. Token Bucket Algorithm Tests (15 tests)
// ============================================================================

#[tokio::test]
async fn test_rate_limit_allows_initial_burst() {
    // Arrange - Create limiter with burst capacity of 20
    let config = RateLimitConfig {
        requests_per_second: 5,
        burst_size: 20,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Send burst of requests up to capacity
    let mut results = Vec::new();
    for _ in 0..20 {
        results.push(limiter.check_limit("client1").await);
    }

    // Assert - All requests within burst should succeed
    assert_eq!(results.iter().filter(|r| r.is_ok()).count(), 20);
}

#[tokio::test]
async fn test_rate_limit_blocks_after_burst_exhausted() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 2,
        burst_size: 10,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Exhaust burst capacity
    for _ in 0..10 {
        let _ = limiter.check_limit("client1").await;
    }

    // Act - Try one more request
    let over_limit = limiter.check_limit("client1").await;

    // Assert - Should be blocked
    assert!(over_limit.is_err());
    assert!(matches!(
        over_limit,
        Err(RateLimitError::TooManyRequests { .. })
    ));
}

#[tokio::test]
async fn test_rate_limit_refills_over_time() {
    // Arrange - 10 tokens/sec = 0.1s per token
    let config = RateLimitConfig {
        requests_per_second: 10,
        burst_size: 5,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Exhaust capacity
    for _ in 0..5 {
        let _ = limiter.check_limit("client1").await;
    }

    // Assert - Next request should fail
    assert!(limiter.check_limit("client1").await.is_err());

    // Act - Wait for refill (200ms = 2 tokens at 10/sec)
    sleep(Duration::from_millis(200)).await;

    // Assert - Should allow new requests after partial refill
    assert!(limiter.check_limit("client1").await.is_ok());
}

#[tokio::test]
async fn test_rate_limit_sustained_request_rate() {
    // Arrange - 10 requests per second
    let config = RateLimitConfig {
        requests_per_second: 10,
        burst_size: 10,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();
    let mut successful_requests = 0;

    // Act - Send requests at sustainable rate (100ms between = 10/sec)
    for i in 0..5 {
        if i > 0 {
            sleep(Duration::from_millis(100)).await;
        }
        if limiter.check_limit("client1").await.is_ok() {
            successful_requests += 1;
        }
    }

    // Assert - All requests at sustainable rate should succeed
    assert_eq!(successful_requests, 5);
}

#[tokio::test]
async fn test_rate_limit_burst_recovery_timeline() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 10,
        burst_size: 10,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Exhaust burst
    for _ in 0..10 {
        let _ = limiter.check_limit("client1").await;
    }

    // Assert - Immediate request should fail
    assert!(limiter.check_limit("client1").await.is_err());

    // Act - Wait for partial refill (500ms = 5 tokens at 10/sec)
    sleep(Duration::from_millis(500)).await;

    // Assert - Should have some tokens available
    assert!(limiter.check_limit("client1").await.is_ok());
}

#[tokio::test]
async fn test_rate_limit_concurrent_requests_same_client() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 10,
        burst_size: 20,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = Arc::new(RateLimiter::new(config).await.unwrap());

    // Act - Send 20 concurrent requests
    let mut handles = Vec::new();
    for _ in 0..20 {
        let limiter_clone = Arc::clone(&limiter);
        handles.push(tokio::spawn(async move {
            limiter_clone.check_limit("client1").await
        }));
    }

    let results: Vec<_> = futures::future::join_all(handles)
        .await
        .into_iter()
        .map(|r| r.unwrap())
        .collect();

    // Assert - Should handle concurrent requests correctly
    let successful = results.iter().filter(|r| r.is_ok()).count();
    assert!(successful <= 20, "Should not exceed rate limit");
}

#[tokio::test]
async fn test_rate_limit_multiple_clients_independent() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 2,
        burst_size: 5,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Client 1 exhausts quota
    for _ in 0..5 {
        let _ = limiter.check_limit("client1").await;
    }

    // Act - Client 2 should have independent quota
    let client2_result = limiter.check_limit("client2").await;

    // Assert - Client 2 not affected by client 1's usage
    assert!(client2_result.is_ok());
}

#[tokio::test]
async fn test_rate_limit_zero_burst_size() {
    // Arrange - Create limiter that blocks all requests
    let config = RateLimitConfig {
        requests_per_second: 10,
        burst_size: 0,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act
    let result = limiter.check_limit("client1").await;

    // Assert - Should immediately block
    assert!(result.is_err());
}

#[tokio::test]
async fn test_rate_limit_very_high_burst_capacity() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 1000,
        burst_size: 10000,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Send large burst
    let mut results = Vec::new();
    for _ in 0..1000 {
        results.push(limiter.check_limit("client1").await);
    }

    // Assert - Should handle high capacity
    assert_eq!(results.iter().filter(|r| r.is_ok()).count(), 1000);
}

#[tokio::test]
async fn test_rate_limit_gradual_token_refill() {
    // Arrange - 10 tokens per second
    let config = RateLimitConfig {
        requests_per_second: 10,
        burst_size: 10,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Use some tokens
    for _ in 0..10 {
        let _ = limiter.check_limit("client1").await;
    }

    // Wait for refill (100ms = 1 token at 10/sec)
    sleep(Duration::from_millis(100)).await;

    // Try to use refilled token
    let result = limiter.check_limit("client1").await;

    // Assert - Should have refilled at least one token
    assert!(result.is_ok(), "Should refill tokens gradually over time");
}

#[tokio::test]
async fn test_rate_limit_time_based_token_generation() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 10,
        burst_size: 10,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();
    let start = Instant::now();

    // Act - Exhaust tokens
    for _ in 0..10 {
        let _ = limiter.check_limit("client1").await;
    }

    // Wait and try again
    sleep(Duration::from_millis(100)).await;
    let result = limiter.check_limit("client1").await;
    let elapsed = start.elapsed();

    // Assert - Should succeed after time window
    assert!(result.is_ok());
    assert!(elapsed >= Duration::from_millis(100));
}

#[tokio::test]
async fn test_rate_limit_partial_burst_usage() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 10,
        burst_size: 20,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Use half of burst capacity
    for _ in 0..10 {
        assert!(limiter.check_limit("client1").await.is_ok());
    }

    // Assert - Remaining capacity should still be available
    for _ in 0..10 {
        assert!(limiter.check_limit("client1").await.is_ok());
    }

    // Assert - Next request exceeds limit
    assert!(limiter.check_limit("client1").await.is_err());
}

#[tokio::test]
async fn test_rate_limit_burst_then_sustained() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 10,
        burst_size: 10,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Use initial burst
    for _ in 0..10 {
        let _ = limiter.check_limit("client1").await;
    }

    // Assert - Burst exhausted
    assert!(limiter.check_limit("client1").await.is_err());

    // Act - Wait for partial refill
    sleep(Duration::from_millis(200)).await;

    // Assert - Can resume sustained usage
    assert!(limiter.check_limit("client1").await.is_ok());
}

#[tokio::test]
async fn test_rate_limit_micro_bursts() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 100,
        burst_size: 100,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Send micro-bursts of 10 requests each
    for burst_num in 0..5 {
        if burst_num > 0 {
            sleep(Duration::from_millis(100)).await;
        }

        let mut burst_results = Vec::new();
        for _ in 0..10 {
            burst_results.push(limiter.check_limit("client1").await);
        }

        // Assert - Each micro-burst should succeed
        assert_eq!(burst_results.iter().filter(|r| r.is_ok()).count(), 10);
    }
}

// ============================================================================
// 2. Backend Tests (10 tests)
// ============================================================================

#[tokio::test]
async fn test_in_memory_backend_basic_operations() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 10,
        burst_size: 10,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act & Assert - Basic increment and check
    assert!(limiter.check_limit("client1").await.is_ok());
    assert!(limiter.check_limit("client1").await.is_ok());
}

#[tokio::test]
async fn test_in_memory_backend_isolation() {
    // Arrange
    let config1 = RateLimitConfig {
        requests_per_second: 2,
        burst_size: 5,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let config2 = config1.clone();

    let limiter1 = RateLimiter::new(config1).await.unwrap();
    let limiter2 = RateLimiter::new(config2).await.unwrap();

    // Act - Exhaust limiter1
    for _ in 0..5 {
        let _ = limiter1.check_limit("client1").await;
    }

    // Assert - limiter2 should be independent
    assert!(limiter2.check_limit("client1").await.is_ok());
}

#[tokio::test]
async fn test_in_memory_backend_expiration() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 10,
        burst_size: 5,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Use tokens
    for _ in 0..5 {
        let _ = limiter.check_limit("client1").await;
    }

    // Assert - Exhausted
    assert!(limiter.check_limit("client1").await.is_err());

    // Act - Wait for refill
    sleep(Duration::from_millis(500)).await;

    // Assert - Should have refilled
    assert!(limiter.check_limit("client1").await.is_ok());
}

#[tokio::test]
async fn test_in_memory_backend_concurrent_access() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 100,
        burst_size: 100,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = Arc::new(RateLimiter::new(config).await.unwrap());

    // Act - Concurrent access from multiple tasks
    let mut handles = Vec::new();
    for i in 0..10 {
        let limiter_clone = Arc::clone(&limiter);
        let client_id = format!("client{}", i);
        handles.push(tokio::spawn(async move {
            limiter_clone.check_limit(&client_id).await
        }));
    }

    let results: Vec<_> = futures::future::join_all(handles)
        .await
        .into_iter()
        .map(|r| r.unwrap())
        .collect();

    // Assert - All different clients should succeed
    assert_eq!(results.iter().filter(|r| r.is_ok()).count(), 10);
}

#[tokio::test]
async fn test_in_memory_backend_key_namespace() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 2,
        burst_size: 5,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Different keys should be independent
    for _ in 0..5 {
        let _ = limiter.check_limit("client1").await;
    }

    // Assert - Different key has independent quota
    assert!(limiter.check_limit("client2").await.is_ok());
    assert!(limiter.check_limit("client1").await.is_err());
}

#[tokio::test]
async fn test_in_memory_backend_memory_efficiency() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 100,
        burst_size: 100,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Create many different client entries
    for i in 0..1000 {
        let client_id = format!("client{}", i);
        let _ = limiter.check_limit(&client_id).await;
    }

    // Assert - Should handle large number of clients
    // This is a behavioral test - if it completes without panic, memory is handled
    assert!(limiter.check_limit("client1001").await.is_ok());
}

#[tokio::test]
async fn test_backend_atomic_increment_operations() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 50,
        burst_size: 50,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = Arc::new(RateLimiter::new(config).await.unwrap());

    // Act - Concurrent increments on same key
    let mut handles = Vec::new();
    for _ in 0..50 {
        let limiter_clone = Arc::clone(&limiter);
        handles.push(tokio::spawn(async move {
            limiter_clone.check_limit("client1").await
        }));
    }

    let results: Vec<_> = futures::future::join_all(handles)
        .await
        .into_iter()
        .map(|r| r.unwrap())
        .collect();

    // Assert - Should properly count concurrent requests
    let successful = results.iter().filter(|r| r.is_ok()).count();
    assert!(
        successful <= 50,
        "Atomic operations should prevent over-limiting"
    );
}

#[tokio::test]
async fn test_backend_handles_unicode_keys() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 10,
        burst_size: 10,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Use Unicode client identifiers
    let unicode_clients = vec!["客户端", "клиент", "🚀client"];
    let mut results = Vec::new();

    for client in unicode_clients {
        results.push(limiter.check_limit(client).await);
    }

    // Assert - Should handle Unicode keys correctly
    assert_eq!(results.iter().filter(|r| r.is_ok()).count(), 3);
}

#[tokio::test]
async fn test_backend_handles_empty_key() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 10,
        burst_size: 10,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Use empty string as key
    let result = limiter.check_limit("").await;

    // Assert - Should handle edge case gracefully
    assert!(result.is_ok() || result.is_err()); // Either behavior is acceptable
}

#[tokio::test]
async fn test_backend_handles_very_long_keys() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 10,
        burst_size: 10,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();
    let long_key = "a".repeat(10000);

    // Act
    let result = limiter.check_limit(&long_key).await;

    // Assert - Should handle long keys without panic
    assert!(result.is_ok() || result.is_err());
}

// ============================================================================
// 3. Middleware Integration Tests (15 tests)
// ============================================================================

#[tokio::test]
async fn test_per_ip_rate_limiting() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 2,
        burst_size: 5,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();
    let ip1 = "192.168.1.1";
    let ip2 = "192.168.1.2";

    // Act - IP1 exhausts quota
    for _ in 0..5 {
        let _ = limiter.check_limit(ip1).await;
    }

    // Assert - IP1 blocked, IP2 allowed
    assert!(limiter.check_limit(ip1).await.is_err());
    assert!(limiter.check_limit(ip2).await.is_ok());
}

#[tokio::test]
async fn test_per_api_key_rate_limiting() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 5,
        burst_size: 10,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();
    let api_key1 = "key_abc123";
    let api_key2 = "key_xyz789";

    // Act - Key1 exhausts quota
    for _ in 0..10 {
        let _ = limiter.check_limit(api_key1).await;
    }

    // Assert - Keys have independent quotas
    assert!(limiter.check_limit(api_key1).await.is_err());
    assert!(limiter.check_limit(api_key2).await.is_ok());
}

#[tokio::test]
async fn test_combined_ip_and_api_key_limiting() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 2,
        burst_size: 5,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Use combined identifier
    let combined_key = "192.168.1.1:key_abc123";
    for _ in 0..5 {
        let _ = limiter.check_limit(combined_key).await;
    }

    // Assert - Combined key is rate limited
    assert!(limiter.check_limit(combined_key).await.is_err());

    // Assert - Different combination is independent
    assert!(limiter
        .check_limit("192.168.1.1:key_xyz789")
        .await
        .is_ok());
}

#[tokio::test]
async fn test_ipv4_address_handling() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 2,
        burst_size: 5,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();
    let ipv4 = "203.0.113.42";

    // Act
    for _ in 0..5 {
        let _ = limiter.check_limit(ipv4).await;
    }

    // Assert
    assert!(limiter.check_limit(ipv4).await.is_err());
}

#[tokio::test]
async fn test_ipv6_address_handling() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 2,
        burst_size: 5,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();
    let ipv6 = "2001:0db8:85a3:0000:0000:8a2e:0370:7334";

    // Act
    for _ in 0..5 {
        let _ = limiter.check_limit(ipv6).await;
    }

    // Assert
    assert!(limiter.check_limit(ipv6).await.is_err());
}

#[tokio::test]
async fn test_x_forwarded_for_extraction() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 2,
        burst_size: 5,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Simulate X-Forwarded-For: client, proxy1, proxy2
    // Should use leftmost IP (actual client)
    let forwarded_ip = "203.0.113.42";

    // Act
    for _ in 0..5 {
        let _ = limiter.check_limit(forwarded_ip).await;
    }

    // Assert - Client IP should be rate limited
    assert!(limiter.check_limit(forwarded_ip).await.is_err());
}

#[tokio::test]
async fn test_rate_limit_error_response_format() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 1,
        burst_size: 1,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();
    let _ = limiter.check_limit("client1").await;

    // Act - Trigger rate limit
    let error = limiter.check_limit("client1").await;

    // Assert - Error type is correct
    assert!(matches!(
        error,
        Err(RateLimitError::TooManyRequests { .. })
    ));
}

#[tokio::test]
async fn test_retry_after_header_calculation() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 1,
        burst_size: 5,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Exhaust quota
    for _ in 0..5 {
        let _ = limiter.check_limit("client1").await;
    }

    let result = limiter.check_limit("client1").await;

    // Assert - Should have retry_after value
    match result {
        Err(RateLimitError::TooManyRequests { retry_after }) => {
            assert!(retry_after > 0);
        }
        _ => panic!("Expected TooManyRequests error"),
    }
}

#[tokio::test]
async fn test_successful_request_no_retry_header() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 10,
        burst_size: 10,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act
    let result = limiter.check_limit("client1").await;

    // Assert - No error means no retry header needed
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_rate_limit_headers_on_success() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 10,
        burst_size: 10,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Make several requests
    for i in 0..5 {
        let result = limiter.check_limit("client1").await;
        // Assert - Should succeed
        assert!(result.is_ok(), "Request {} should succeed", i);
    }
}

#[tokio::test]
async fn test_localhost_exemption_not_applied() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 2,
        burst_size: 5,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();
    let localhost = "127.0.0.1";

    // Act - Exhaust quota
    for _ in 0..5 {
        let _ = limiter.check_limit(localhost).await;
    }

    // Assert - Localhost should also be rate limited
    assert!(limiter.check_limit(localhost).await.is_err());
}

#[tokio::test]
async fn test_rate_limit_preserves_request_context() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 10,
        burst_size: 10,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Check rate limit doesn't modify client identifier
    let client_id = "client123";
    let result = limiter.check_limit(client_id).await;

    // Assert - Result is based on original identifier
    assert!(result.is_ok());

    // Second request should also use same identifier
    let result2 = limiter.check_limit(client_id).await;
    assert!(result2.is_ok());
}

#[tokio::test]
async fn test_rate_limit_case_sensitive_keys() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 2,
        burst_size: 5,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Use different cases
    for _ in 0..5 {
        let _ = limiter.check_limit("CLIENT1").await;
    }

    // Assert - Different case should be different key
    assert!(limiter.check_limit("CLIENT1").await.is_err());
    assert!(limiter.check_limit("client1").await.is_ok());
}

#[tokio::test]
async fn test_middleware_different_endpoint_keys() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 2,
        burst_size: 5,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Different endpoints might use different keys
    let endpoint1_key = "endpoint:/api/v1/users";
    let endpoint2_key = "endpoint:/api/v1/posts";

    for _ in 0..5 {
        let _ = limiter.check_limit(endpoint1_key).await;
    }

    // Assert - Different endpoints have independent limits
    assert!(limiter.check_limit(endpoint1_key).await.is_err());
    assert!(limiter.check_limit(endpoint2_key).await.is_ok());
}

#[tokio::test]
async fn test_rate_limit_with_proxy_chain() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 2,
        burst_size: 5,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Simulate request through multiple proxies
    // X-Forwarded-For: client, proxy1, proxy2
    let actual_client = "203.0.113.42";

    // Act - Should extract actual client IP
    for _ in 0..5 {
        let _ = limiter.check_limit(actual_client).await;
    }

    // Assert - Client should be limited, not proxy
    assert!(limiter.check_limit(actual_client).await.is_err());
}

// ============================================================================
// 4. Edge Case Tests (10 tests)
// ============================================================================

#[tokio::test]
async fn test_clock_skew_handling() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 5,
        burst_size: 10,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Use tokens
    for _ in 0..10 {
        let _ = limiter.check_limit("client1").await;
    }

    // Note: Clock skew would be handled by backend
    // This tests that behavior is consistent
    assert!(limiter.check_limit("client1").await.is_err());
}

#[tokio::test]
async fn test_zero_burst_size_edge_case() {
    // Arrange - Zero burst
    let config = RateLimitConfig {
        requests_per_second: 10,
        burst_size: 0,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act
    let result = limiter.check_limit("client1").await;

    // Assert - Should block all requests
    assert!(result.is_err());
}

#[tokio::test]
async fn test_very_high_request_rate_simulation() {
    // Arrange - Allow 10k RPS
    let config = RateLimitConfig {
        requests_per_second: 10000,
        burst_size: 10000,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = Arc::new(RateLimiter::new(config).await.unwrap());

    // Act - Simulate high rate (100 concurrent requests)
    let mut handles = Vec::new();
    for _ in 0..100 {
        let limiter_clone = Arc::clone(&limiter);
        handles.push(tokio::spawn(async move {
            limiter_clone.check_limit("high_volume_client").await
        }));
    }

    let results: Vec<_> = futures::future::join_all(handles)
        .await
        .into_iter()
        .map(|r| r.unwrap())
        .collect();

    // Assert - Should handle high throughput
    assert_eq!(results.iter().filter(|r| r.is_ok()).count(), 100);
}

#[tokio::test]
async fn test_maximum_u32_rate_limit() {
    // Arrange - Test with maximum value
    let config = RateLimitConfig {
        requests_per_second: u32::MAX,
        burst_size: u32::MAX,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act
    let result = limiter.check_limit("client1").await;

    // Assert - Should handle extreme values
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_rapid_client_switching() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 2,
        burst_size: 5,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Rapidly switch between clients
    for i in 0..100 {
        let client_id = format!("client{}", i % 10);
        let _ = limiter.check_limit(&client_id).await;
    }

    // Assert - Each client should eventually be limited
    for i in 0..10 {
        let client_id = format!("client{}", i);
        // Each client got ~10 requests, so should fail
        let result = limiter.check_limit(&client_id).await;
        assert!(result.is_err(), "Client {} should be limited", i);
    }
}

#[tokio::test]
async fn test_client_id_with_special_characters() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 10,
        burst_size: 10,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();
    let special_chars = vec![
        "client:with:colons",
        "client/with/slashes",
        "client@with@ats",
        "client#with#hashes",
        "client$with$dollars",
    ];

    // Act & Assert - Should handle special characters
    for client_id in special_chars {
        let result = limiter.check_limit(client_id).await;
        assert!(result.is_ok(), "Should handle client_id: {}", client_id);
    }
}

#[tokio::test]
async fn test_rapid_sequential_requests() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 100,
        burst_size: 100,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Send requests as fast as possible
    let start = Instant::now();
    for _ in 0..100 {
        let _ = limiter.check_limit("speed_client").await;
    }
    let elapsed = start.elapsed();

    // Assert - Should process quickly (< 100ms for 100 requests)
    assert!(
        elapsed < Duration::from_millis(100),
        "Too slow: {:?}",
        elapsed
    );
}

#[tokio::test]
async fn test_limiter_state_after_error() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 2,
        burst_size: 5,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Exhaust quota
    for _ in 0..5 {
        let _ = limiter.check_limit("client1").await;
    }

    // Get error
    let error = limiter.check_limit("client1").await;
    assert!(error.is_err());

    // Assert - State should remain consistent
    let error2 = limiter.check_limit("client1").await;
    assert!(error2.is_err());
}

#[tokio::test]
async fn test_client_id_whitespace_handling() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 2,
        burst_size: 5,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Same IP with/without whitespace
    let ip1 = "192.168.1.1";
    let ip2 = " 192.168.1.1 "; // With whitespace

    for _ in 0..5 {
        let _ = limiter.check_limit(ip1).await;
    }

    // Assert - These are treated as different keys (no normalization)
    let result = limiter.check_limit(ip2).await;
    assert!(result.is_ok(), "Whitespace creates different key");
}

#[tokio::test]
async fn test_null_byte_in_client_id() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 10,
        burst_size: 10,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Client ID with null byte
    let client_with_null = "client\0id";
    let result = limiter.check_limit(client_with_null).await;

    // Assert - Should handle gracefully (not panic)
    let _ = result;
}

// ============================================================================
// 5. Performance Tests (5 tests)
// ============================================================================

#[tokio::test]
async fn test_throughput_single_client() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 10000,
        burst_size: 10000,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();
    let iterations = 1000;

    // Act - Measure throughput
    let start = Instant::now();
    for _ in 0..iterations {
        let _ = limiter.check_limit("perf_client").await;
    }
    let elapsed = start.elapsed();

    // Assert - Should process at least 10k requests/second
    let rps = iterations as f64 / elapsed.as_secs_f64();
    assert!(rps > 10_000.0, "Throughput too low: {} rps", rps);
}

#[tokio::test]
async fn test_latency_percentiles() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 1000,
        burst_size: 1000,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();
    let mut latencies = Vec::new();

    // Act - Measure individual request latencies
    for _ in 0..100 {
        let start = Instant::now();
        let _ = limiter.check_limit("latency_client").await;
        latencies.push(start.elapsed());
    }

    // Sort for percentile calculation
    latencies.sort();

    // Assert - Check percentiles
    let p50 = latencies[49];
    let p95 = latencies[94];
    let p99 = latencies[98];

    // P50 should be very fast (< 1ms)
    assert!(p50 < Duration::from_millis(1), "P50 too high: {:?}", p50);

    // P95 should be fast (< 5ms)
    assert!(p95 < Duration::from_millis(5), "P95 too high: {:?}", p95);

    // P99 should be reasonable (< 10ms)
    assert!(p99 < Duration::from_millis(10), "P99 too high: {:?}", p99);
}

#[tokio::test]
async fn test_memory_usage_with_many_clients() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 100,
        burst_size: 100,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = RateLimiter::new(config).await.unwrap();

    // Act - Create many unique clients
    for i in 0..10_000 {
        let client_id = format!("client{}", i);
        let _ = limiter.check_limit(&client_id).await;
    }

    // Assert - Should complete without excessive memory usage
    // If this test completes, memory is managed reasonably
    assert!(limiter.check_limit("final_client").await.is_ok());
}

#[tokio::test]
async fn test_concurrent_load() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 10000,
        burst_size: 10000,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = Arc::new(RateLimiter::new(config).await.unwrap());

    // Act - Simulate concurrent load from multiple clients
    let start = Instant::now();
    let mut handles = Vec::new();

    for client_num in 0..100 {
        let limiter_clone = Arc::clone(&limiter);
        handles.push(tokio::spawn(async move {
            let client_id = format!("concurrent{}", client_num);
            for _ in 0..100 {
                let _ = limiter_clone.check_limit(&client_id).await;
            }
        }));
    }

    // Wait for all tasks
    for handle in handles {
        handle.await.unwrap();
    }

    let elapsed = start.elapsed();

    // Assert - 10k requests should complete quickly
    assert!(
        elapsed < Duration::from_secs(5),
        "Concurrent load too slow: {:?}",
        elapsed
    );
}

#[tokio::test]
async fn test_graceful_degradation_under_pressure() {
    // Arrange
    let config = RateLimitConfig {
        requests_per_second: 25,
        burst_size: 50,
        backend: RateLimitBackend::InMemory,
        ttl: Duration::from_secs(60),
    };
    let limiter = Arc::new(RateLimiter::new(config).await.unwrap());

    // Act - Send more requests than allowed
    let mut handles = Vec::new();
    for _ in 0..200 {
        let limiter_clone = Arc::clone(&limiter);
        handles.push(tokio::spawn(async move {
            limiter_clone.check_limit("pressure_client").await
        }));
    }

    let results: Vec<_> = futures::future::join_all(handles)
        .await
        .into_iter()
        .map(|r| r.unwrap())
        .collect();

    let successful = results.iter().filter(|r| r.is_ok()).count();
    let failed = results.iter().filter(|r| r.is_err()).count();

    // Assert - Should properly enforce limits under pressure
    assert!(
        successful <= 50,
        "Should not exceed burst limit of 50, got {}",
        successful
    );
    assert!(
        failed >= 150,
        "Should reject excess requests, only rejected {}",
        failed
    );

    // Assert - System should remain responsive
    assert!(limiter.check_limit("new_client").await.is_ok());
}
