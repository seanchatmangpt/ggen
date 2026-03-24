//! E2E tests for backpressure and λ > μ rejection

use ggen_backpressure::{
    limiter::{RateLimiter, RateLimiterConfig},
    AdmissionController,
};
use std::time::Duration;
use tokio::time::sleep;

fn test_config(concurrent: usize, rps: f64) -> RateLimiterConfig {
    RateLimiterConfig {
        max_rps: rps,
        max_concurrent: concurrent,
        burst_size: concurrent,
    }
}

#[tokio::test]
async fn test_lambda_exceeds_mu_rejection() {
    let config = test_config(10, 10.0);
    let limiter = RateLimiter::new(config);

    let mut acquired = Vec::new();
    let mut rejected = 0;

    for _ in 0..20 {
        match limiter.try_acquire() {
            Ok(Some(token)) => acquired.push(token),
            Ok(None) => rejected += 1,
            Err(_) => rejected += 1,
        }
    }

    assert!(rejected > 0, "Expected rejections when λ > μ");
    assert!(acquired.len() <= 10);
}

#[tokio::test]
async fn test_wip_token_lifecycle() {
    let config = test_config(5, 100.0);
    let limiter = RateLimiter::new(config);

    let token = limiter.try_acquire().expect("Failed").expect("None");
    let initial_in_flight = limiter.in_flight();
    assert_eq!(initial_in_flight, 1);

    drop(token);
    sleep(Duration::from_millis(10)).await;

    let token2 = limiter.try_acquire().expect("Failed");
    assert!(token2.is_some());
}

#[tokio::test]
async fn test_capacity_enforcement() {
    let capacity = 3;
    let config = test_config(capacity, 1000.0);
    let limiter = RateLimiter::new(config);

    let mut tokens = Vec::new();
    for i in 0..capacity {
        let token = limiter.try_acquire().unwrap_or_else(|e| panic!("Token {}: {}", i, e));
        assert!(token.is_some());
        tokens.push(token);
    }

    assert_eq!(limiter.in_flight(), capacity);
    assert_eq!(limiter.utilization(), 1.0);

    // Try to exceed capacity - may get rate limit error or None
    let overflow = limiter.try_acquire();
    assert!(overflow.is_err() || overflow.ok().unwrap().is_none());

    tokens.pop();
    sleep(Duration::from_millis(10)).await;
}

#[tokio::test]
async fn test_utilization_metrics() {
    let capacity = 10;
    let config = test_config(capacity, 1000.0);
    let limiter = RateLimiter::new(config);

    assert_eq!(limiter.capacity(), capacity);
    assert_eq!(limiter.in_flight(), 0);
    assert_eq!(limiter.utilization(), 0.0);

    let mut tokens = Vec::new();
    for _ in 0..5 {
        tokens.push(limiter.try_acquire().expect("Failed").expect("None"));
    }

    assert_eq!(limiter.in_flight(), 5);
    assert_eq!(limiter.utilization(), 0.5);

    for _ in 0..5 {
        tokens.push(limiter.try_acquire().expect("Failed").expect("None"));
    }

    assert_eq!(limiter.in_flight(), 10);
    assert_eq!(limiter.utilization(), 1.0);

    tokens.clear();
    sleep(Duration::from_millis(10)).await;
    assert_eq!(limiter.utilization(), 0.0);
}
