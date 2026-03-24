//! Rate limiter implementing λ ≤ μ enforcement
//!
//! Ensures arrival rate (λ) does not exceed service rate (μ) to prevent
//! queue buildup and maintain system stability.

use crate::{AdmissionController, BackpressureError, Result, WIPToken};
use async_trait::async_trait;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::Mutex;

/// Configuration for rate limiter
#[derive(Debug, Clone)]
pub struct RateLimiterConfig {
    /// Maximum requests per second (μ - service rate)
    pub max_rps: f64,

    /// Maximum concurrent requests (WIP limit)
    pub max_concurrent: usize,

    /// Burst capacity (allows temporary spikes)
    pub burst_size: usize,
}

impl Default for RateLimiterConfig {
    fn default() -> Self {
        Self {
            max_rps: 100.0,
            max_concurrent: 10,
            burst_size: 20,
        }
    }
}

/// Token bucket rate limiter with WIP limits
///
/// Implements λ ≤ μ by:
/// 1. Token bucket for rate limiting (prevents sustained overload)
/// 2. Semaphore for WIP limits (bounds concurrent work)
/// 3. Admission rejection when capacity exhausted
#[derive(Debug)]
pub struct RateLimiter {
    config: RateLimiterConfig,
    pool: crate::token::TokenPool,
    bucket: Arc<Mutex<TokenBucket>>,
}

#[derive(Debug)]
struct TokenBucket {
    tokens: f64,
    last_refill: Instant,
    capacity: f64,
    refill_rate: f64,
}

impl TokenBucket {
    fn new(capacity: f64, refill_rate: f64) -> Self {
        Self {
            tokens: capacity,
            last_refill: Instant::now(),
            capacity,
            refill_rate,
        }
    }

    fn refill(&mut self) {
        let now = Instant::now();
        let elapsed = now.duration_since(self.last_refill).as_secs_f64();

        let new_tokens = elapsed * self.refill_rate;
        self.tokens = (self.tokens + new_tokens).min(self.capacity);
        self.last_refill = now;
    }

    fn try_consume(&mut self) -> bool {
        self.refill();

        if self.tokens >= 1.0 {
            self.tokens -= 1.0;
            true
        } else {
            false
        }
    }

    async fn consume(&mut self) -> Result<()> {
        loop {
            self.refill();

            if self.tokens >= 1.0 {
                self.tokens -= 1.0;
                return Ok(());
            }

            // Wait for next refill
            let wait_time = Duration::from_secs_f64(1.0 / self.refill_rate);
            tokio::time::sleep(wait_time).await;
        }
    }
}

impl RateLimiter {
    /// Create a new rate limiter with the given configuration
    pub fn new(config: RateLimiterConfig) -> Self {
        let bucket = TokenBucket::new(config.burst_size as f64, config.max_rps);

        Self {
            pool: crate::token::TokenPool::new(config.max_concurrent),
            bucket: Arc::new(Mutex::new(bucket)),
            config,
        }
    }

    /// Create with default configuration
    pub fn with_defaults() -> Self {
        Self::new(RateLimiterConfig::default())
    }

    /// Check if rate limit would be exceeded
    pub async fn check_rate(&self) -> bool {
        let mut bucket = self.bucket.lock().await;
        bucket.refill();
        bucket.tokens >= 1.0
    }
}

#[async_trait]
impl AdmissionController for RateLimiter {
    async fn acquire(&self) -> Result<WIPToken> {
        // First check rate limit
        {
            let mut bucket = self.bucket.lock().await;
            bucket.consume().await?;
        }

        // Then acquire WIP token
        self.pool.acquire().await
    }

    fn try_acquire(&self) -> Result<Option<WIPToken>> {
        // Check rate limit without blocking
        let mut bucket = self.bucket.try_lock()
            .map_err(|_| BackpressureError::RateLimitExceeded("rate check in progress".to_string()))?;

        if !bucket.try_consume() {
            return Err(BackpressureError::RateLimitExceeded(
                format!("rate limit exceeded: {} rps", self.config.max_rps)
            ));
        }

        // Try to acquire WIP token
        self.pool.try_acquire()
    }

    fn utilization(&self) -> f64 {
        self.pool.utilization()
    }

    fn capacity(&self) -> usize {
        self.pool.capacity()
    }

    fn in_flight(&self) -> usize {
        self.pool.in_flight()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_rate_limiter_respects_wip_limit() {
        let config = RateLimiterConfig {
            max_rps: 1000.0, // High rate to focus on WIP
            max_concurrent: 2,
            burst_size: 100,
        };

        let limiter = RateLimiter::new(config);

        let t1 = limiter.acquire().await.unwrap();
        let t2 = limiter.acquire().await.unwrap();

        // Third should fail (WIP limit)
        assert!(limiter.try_acquire().unwrap().is_none());

        drop(t1);

        // Now should succeed
        let t3 = limiter.try_acquire().unwrap();
        assert!(t3.is_some());

        drop(t2);
        drop(t3);
    }

    #[tokio::test]
    async fn test_rate_limiter_enforces_rate() {
        let config = RateLimiterConfig {
            max_rps: 10.0, // 10 per second
            max_concurrent: 100, // High WIP to focus on rate
            burst_size: 2, // Small burst
        };

        let limiter = RateLimiter::new(config);

        // Should allow burst
        let _t1 = limiter.acquire().await.unwrap();
        let _t2 = limiter.acquire().await.unwrap();

        // Third should fail rate limit
        let result = limiter.try_acquire();
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_rate_limiter_utilization() {
        let config = RateLimiterConfig {
            max_rps: 1000.0,
            max_concurrent: 10,
            burst_size: 100,
        };

        let limiter = RateLimiter::new(config);

        assert_eq!(limiter.utilization(), 0.0);
        assert_eq!(limiter.capacity(), 10);
        assert_eq!(limiter.in_flight(), 0);

        let _t1 = limiter.acquire().await.unwrap();
        assert_eq!(limiter.in_flight(), 1);
        assert!((limiter.utilization() - 0.1).abs() < 0.01);
    }

    #[tokio::test]
    async fn test_token_bucket_refill() {
        let mut bucket = TokenBucket::new(10.0, 5.0); // 5 tokens per second

        // Consume all tokens
        for _ in 0..10 {
            assert!(bucket.try_consume());
        }

        // Should be empty
        assert!(!bucket.try_consume());

        // Wait for refill
        tokio::time::sleep(Duration::from_millis(500)).await;
        bucket.refill();

        // Should have ~2.5 tokens
        assert!(bucket.try_consume());
        assert!(bucket.try_consume());
    }

    #[tokio::test]
    async fn test_strict_lambda_leq_mu_enforcement() {
        let config = RateLimiterConfig {
            max_rps: 5.0,     // μ = 5 rps
            max_concurrent: 3,
            burst_size: 5,
        };

        let limiter = RateLimiter::new(config);

        // Exhaust burst capacity
        for _ in 0..5 {
            let _token = limiter.acquire().await.unwrap();
        }

        // Next acquisition should fail - λ would exceed μ
        let result = limiter.try_acquire();
        assert!(result.is_err());
    }
}
