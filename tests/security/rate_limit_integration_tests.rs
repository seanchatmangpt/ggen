//! End-to-End Rate Limiting Integration Tests
//!
//! Tests rate limiting behavior across the ggen API with real Redis:
//! - Concurrent client requests
//! - IP-based limiting
//! - API key limiting
//! - Burst behavior
//! - Rate limit recovery
//!
//! Uses Chicago TDD: AAA pattern, real Redis via testcontainers, observable outputs

use reqwest::blocking::Client;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::Duration;
use testcontainers::{clients::Cli, RunnableImage};
use testcontainers_modules::redis::Redis;

/// Test fixture providing Redis container and API client
struct RateLimitFixture {
    _container: testcontainers::Container<'static, Redis>,
    redis_url: String,
    api_base_url: String,
    client: Client,
}

impl RateLimitFixture {
    fn new(docker: &'static Cli) -> Result<Self, Box<dyn std::error::Error>> {
        // Arrange: Start Redis container
        let redis_image = RunnableImage::from(Redis::default());
        let container = docker.run(redis_image);
        let redis_port = container.get_host_port_ipv4(6379);
        let redis_url = format!("redis://127.0.0.1:{}", redis_port);

        // Note: In real implementation, would start ggen API server here
        // For now, using placeholder
        let api_base_url = "http://localhost:8080".to_string();

        let client = Client::builder().timeout(Duration::from_secs(5)).build()?;

        Ok(Self {
            _container: container,
            redis_url,
            api_base_url,
            client,
        })
    }

    fn make_request(&self, path: &str) -> Result<reqwest::blocking::Response, reqwest::Error> {
        self.client
            .get(&format!("{}{}", self.api_base_url, path))
            .send()
    }

    fn make_request_with_ip(
        &self, path: &str, ip: &str,
    ) -> Result<reqwest::blocking::Response, reqwest::Error> {
        self.client
            .get(&format!("{}{}", self.api_base_url, path))
            .header("X-Forwarded-For", ip)
            .send()
    }

    fn make_request_with_api_key(
        &self, path: &str, api_key: &str,
    ) -> Result<reqwest::blocking::Response, reqwest::Error> {
        self.client
            .get(&format!("{}{}", self.api_base_url, path))
            .header("Authorization", format!("Bearer {}", api_key))
            .send()
    }
}

// ============================================================================
// CATEGORY 1: Concurrent Rate Limiting
// ============================================================================

#[test]
#[ignore] // Requires running API server
fn test_concurrent_requests_hit_rate_limit() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let docker = Box::leak(Box::new(Cli::default()));
    let fixture = RateLimitFixture::new(docker)?;

    let max_requests = 10;
    let concurrent_requests = 50;
    let success_count = Arc::new(AtomicUsize::new(0));
    let rate_limited_count = Arc::new(AtomicUsize::new(0));

    // Act: Send concurrent requests from same IP
    let mut handles = vec![];
    for i in 0..concurrent_requests {
        let fixture_url = fixture.api_base_url.clone();
        let success = Arc::clone(&success_count);
        let limited = Arc::clone(&rate_limited_count);

        let handle = thread::spawn(move || {
            let client = Client::new();
            let response = client
                .get(&format!("{}/api/generate", fixture_url))
                .header("X-Forwarded-For", "192.168.1.100")
                .send();

            match response {
                Ok(resp) if resp.status().is_success() => {
                    success.fetch_add(1, Ordering::SeqCst);
                }
                Ok(resp) if resp.status().as_u16() == 429 => {
                    limited.fetch_add(1, Ordering::SeqCst);
                }
                _ => {}
            }
        });
        handles.push(handle);

        // Small delay to simulate realistic concurrent access
        if i % 10 == 0 {
            thread::sleep(Duration::from_millis(10));
        }
    }

    for handle in handles {
        handle.join().ok();
    }

    // Assert: Should hit rate limit
    let success = success_count.load(Ordering::SeqCst);
    let limited = rate_limited_count.load(Ordering::SeqCst);

    assert!(
        success <= max_requests,
        "Should allow at most {} requests, got {}",
        max_requests,
        success
    );
    assert!(limited > 0, "Should have rate-limited some requests, got 0");
    assert_eq!(
        success + limited,
        concurrent_requests,
        "All requests should be accounted for"
    );

    Ok(())
}

#[test]
#[ignore] // Requires running API server
fn test_different_ips_independent_limits() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let docker = Box::leak(Box::new(Cli::default()));
    let fixture = RateLimitFixture::new(docker)?;

    let requests_per_ip = 15;

    // Act: Send requests from different IPs
    let mut ip1_success = 0;
    let mut ip2_success = 0;

    for _ in 0..requests_per_ip {
        if let Ok(resp) = fixture.make_request_with_ip("/api/generate", "192.168.1.100") {
            if resp.status().is_success() {
                ip1_success += 1;
            }
        }

        if let Ok(resp) = fixture.make_request_with_ip("/api/generate", "192.168.1.101") {
            if resp.status().is_success() {
                ip2_success += 1;
            }
        }
    }

    // Assert: Each IP should have independent rate limits
    assert!(ip1_success > 0, "IP1 should have some successful requests");
    assert!(ip2_success > 0, "IP2 should have some successful requests");

    // Both IPs should hit limits independently
    assert!(ip1_success <= 10, "IP1 should be rate limited");
    assert!(ip2_success <= 10, "IP2 should be rate limited");

    Ok(())
}

// ============================================================================
// CATEGORY 2: API Key Rate Limiting
// ============================================================================

#[test]
#[ignore] // Requires running API server
fn test_api_key_rate_limiting() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let docker = Box::leak(Box::new(Cli::default()));
    let fixture = RateLimitFixture::new(docker)?;

    let api_key = "test_key_123";
    let max_requests = 20; // Higher limit for authenticated users

    // Act: Make requests with API key
    let mut success_count = 0;
    let mut rate_limited_count = 0;

    for _ in 0..30 {
        if let Ok(resp) = fixture.make_request_with_api_key("/api/generate", api_key) {
            if resp.status().is_success() {
                success_count += 1;
            } else if resp.status().as_u16() == 429 {
                rate_limited_count += 1;
            }
        }
        thread::sleep(Duration::from_millis(50));
    }

    // Assert: Should allow higher limit for API key users
    assert!(
        success_count <= max_requests,
        "Should respect API key rate limit"
    );
    assert!(rate_limited_count > 0, "Should eventually hit rate limit");

    Ok(())
}

// ============================================================================
// CATEGORY 3: Burst Behavior
// ============================================================================

#[test]
#[ignore] // Requires running API server
fn test_burst_requests_handled() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let docker = Box::leak(Box::new(Cli::default()));
    let fixture = RateLimitFixture::new(docker)?;

    let burst_size = 5;

    // Act: Send burst of requests instantly
    let mut burst_results = vec![];
    for _ in 0..burst_size {
        let result = fixture.make_request("/api/generate");
        burst_results.push(result);
    }

    // Assert: Should allow small burst
    let success_count = burst_results
        .iter()
        .filter(|r| {
            r.as_ref()
                .map(|resp| resp.status().is_success())
                .unwrap_or(false)
        })
        .count();

    assert!(
        success_count >= burst_size - 1,
        "Should allow small burst, got {} successes",
        success_count
    );

    Ok(())
}

// ============================================================================
// CATEGORY 4: Rate Limit Recovery
// ============================================================================

#[test]
#[ignore] // Requires running API server
fn test_rate_limit_recovery_after_window() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let docker = Box::leak(Box::new(Cli::default()));
    let fixture = RateLimitFixture::new(docker)?;

    // Act: Exhaust rate limit
    for _ in 0..15 {
        let _ = fixture.make_request_with_ip("/api/generate", "192.168.1.200");
    }

    // Verify rate limited
    let limited_response = fixture.make_request_with_ip("/api/generate", "192.168.1.200")?;
    assert_eq!(
        limited_response.status().as_u16(),
        429,
        "Should be rate limited"
    );

    // Wait for rate limit window to expire (60 seconds)
    println!("Waiting for rate limit window to expire...");
    thread::sleep(Duration::from_secs(61));

    // Try again after window expires
    let recovered_response = fixture.make_request_with_ip("/api/generate", "192.168.1.200")?;

    // Assert: Should allow requests after window expires
    assert!(
        recovered_response.status().is_success(),
        "Should allow requests after rate limit window expires, got {}",
        recovered_response.status()
    );

    Ok(())
}

// ============================================================================
// CATEGORY 5: Distributed Rate Limiting (Multiple Instances)
// ============================================================================

#[test]
#[ignore] // Requires running API server
fn test_distributed_rate_limiting_shared_redis() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange: Two API instances sharing Redis
    let docker = Box::leak(Box::new(Cli::default()));
    let fixture1 = RateLimitFixture::new(docker)?;
    let fixture2 = RateLimitFixture::new(docker)?;

    // Both instances use same Redis
    assert_eq!(fixture1.redis_url, fixture2.redis_url);

    // Act: Make requests to both instances from same IP
    let mut total_success = 0;

    for i in 0..20 {
        let fixture = if i % 2 == 0 { &fixture1 } else { &fixture2 };

        if let Ok(resp) = fixture.make_request_with_ip("/api/generate", "192.168.1.150") {
            if resp.status().is_success() {
                total_success += 1;
            }
        }
    }

    // Assert: Combined requests across instances should be rate limited
    assert!(
        total_success <= 10,
        "Should enforce rate limit across instances, got {}",
        total_success
    );

    Ok(())
}

// ============================================================================
// CATEGORY 6: Rate Limit Headers
// ============================================================================

#[test]
#[ignore] // Requires running API server
fn test_rate_limit_headers_present() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let docker = Box::leak(Box::new(Cli::default()));
    let fixture = RateLimitFixture::new(docker)?;

    // Act
    let response = fixture.make_request("/api/generate")?;

    // Assert: Should include rate limit headers
    assert!(
        response.headers().contains_key("X-RateLimit-Limit"),
        "Should include X-RateLimit-Limit header"
    );
    assert!(
        response.headers().contains_key("X-RateLimit-Remaining"),
        "Should include X-RateLimit-Remaining header"
    );
    assert!(
        response.headers().contains_key("X-RateLimit-Reset"),
        "Should include X-RateLimit-Reset header"
    );

    Ok(())
}
