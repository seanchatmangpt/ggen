//! Rate limiting strategy implementations

use super::{ClientId, RateLimitDecision, RateLimitStrategy};
use crate::error::{GatewayError, GatewayResult};
use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use tokio::sync::RwLock;

/// Token bucket rate limiter with burst capacity
#[derive(Debug, Clone)]
pub struct TokenBucketLimiter {
    // Tokens per second
    rate: f64,
    // Maximum tokens in bucket
    capacity: u32,
    // Per-client bucket state: (tokens, last_refill_time)
    buckets: Arc<RwLock<HashMap<ClientId, (f64, u64)>>>,
}

impl TokenBucketLimiter {
    /// Create a new token bucket limiter
    pub fn new(rate: f64, capacity: u32) -> Self {
        Self {
            rate,
            capacity,
            buckets: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Check if request is allowed
    pub async fn check(&self, client: &ClientId) -> GatewayResult<RateLimitDecision> {
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs();

        let mut buckets = self.buckets.write().await;
        let (tokens, last_refill) = buckets
            .entry(client.clone())
            .or_insert((self.capacity as f64, now));

        // Refill tokens based on time elapsed
        let elapsed = (now - last_refill) as f64;
        let refill_amount = elapsed * self.rate;
        *tokens = (*tokens + refill_amount).min(self.capacity as f64);

        if *tokens >= 1.0 {
            *tokens -= 1.0;
            *last_refill = now;

            let remaining = *tokens as u32;
            Ok(RateLimitDecision::allowed(remaining, 1))
        } else {
            Err(GatewayError::RateLimitExceeded)
        }
    }

    /// Get current bucket state for a client
    pub async fn get_bucket_state(&self, client: &ClientId) -> Option<(f64, u64)> {
        let buckets = self.buckets.read().await;
        buckets.get(client).copied()
    }
}

/// Sliding window rate limiter (counts requests in fixed time windows)
#[derive(Debug, Clone)]
pub struct SlidingWindowLimiter {
    // Max requests per window
    limit: u32,
    // Window size in seconds
    window_seconds: u64,
    // Per-client window data: (request_times, oldest_request_time)
    windows: Arc<RwLock<HashMap<ClientId, Vec<u64>>>>,
}

impl SlidingWindowLimiter {
    /// Create a new sliding window limiter
    pub fn new(limit: u32, window_seconds: u64) -> Self {
        Self {
            limit,
            window_seconds,
            windows: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Check if request is allowed
    pub async fn check(&self, client: &ClientId) -> GatewayResult<RateLimitDecision> {
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs();

        let mut windows = self.windows.write().await;
        let times = windows.entry(client.clone()).or_insert_with(Vec::new);

        // Remove old requests outside the window
        times.retain(|t| now - t < self.window_seconds);

        if times.len() < self.limit as usize {
            times.push(now);
            let remaining = self.limit - times.len() as u32;
            Ok(RateLimitDecision::allowed(remaining, self.window_seconds))
        } else {
            // Calculate reset time based on oldest request
            let oldest = times[0];
            let reset_after = (oldest + self.window_seconds).saturating_sub(now);
            Err(GatewayError::RateLimitExceeded)
        }
    }

    /// Get current window state for a client
    pub async fn get_window_state(&self, client: &ClientId) -> Option<Vec<u64>> {
        let windows = self.windows.read().await;
        windows.get(client).cloned()
    }
}

/// Adaptive rate limiter that adjusts limits based on system load
#[derive(Debug, Clone)]
pub struct AdaptiveRateLimiter {
    // Base limit
    base_limit: u32,
    // Window size
    window_seconds: u64,
    // Current system load (0.0 to 1.0)
    load: Arc<AtomicU64>,
    // Per-client sliding window
    windows: Arc<RwLock<HashMap<ClientId, Vec<u64>>>>,
}

impl AdaptiveRateLimiter {
    /// Create a new adaptive rate limiter
    pub fn new(base_limit: u32, window_seconds: u64) -> Self {
        Self {
            base_limit,
            window_seconds,
            load: Arc::new(AtomicU64::new(0)),
            windows: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Update system load (0.0 to 1.0, encoded as u64 in fixed-point)
    pub fn set_load(&self, load: f64) {
        let encoded = (load * 1_000_000.0) as u64;
        self.load.store(encoded, Ordering::Relaxed);
    }

    /// Get current effective limit based on load
    fn get_effective_limit(&self) -> u32 {
        let encoded = self.load.load(Ordering::Relaxed);
        let load = encoded as f64 / 1_000_000.0;

        // Reduce limit as load increases
        let factor = 1.0 - (load * 0.5); // Reduce by up to 50% at 100% load
        (self.base_limit as f64 * factor.max(0.5)) as u32
    }

    /// Check if request is allowed
    pub async fn check(&self, client: &ClientId) -> GatewayResult<RateLimitDecision> {
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs();

        let effective_limit = self.get_effective_limit();

        let mut windows = self.windows.write().await;
        let times = windows.entry(client.clone()).or_insert_with(Vec::new);

        // Remove old requests outside the window
        times.retain(|t| now - t < self.window_seconds);

        if times.len() < effective_limit as usize {
            times.push(now);
            let remaining = effective_limit - times.len() as u32;
            Ok(RateLimitDecision::allowed(remaining, self.window_seconds))
        } else {
            let oldest = times[0];
            let reset_after = (oldest + self.window_seconds).saturating_sub(now);
            Err(GatewayError::RateLimitExceeded)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_token_bucket_limiter_allows_requests() {
        let limiter = TokenBucketLimiter::new(10.0, 100);
        let client = ClientId::from_ip("127.0.0.1");

        // First request should be allowed
        let result = limiter.check(&client).await;
        assert!(result.is_ok());
        let decision = result.unwrap();
        assert!(decision.allowed);
    }

    #[tokio::test]
    async fn test_token_bucket_limiter_refills() {
        let limiter = TokenBucketLimiter::new(10.0, 1);
        let client = ClientId::from_ip("127.0.0.1");

        // Consume the one token
        limiter.check(&client).await.unwrap();

        // Next request should be rate limited
        let result = limiter.check(&client).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_sliding_window_limiter() {
        let limiter = SlidingWindowLimiter::new(3, 60);
        let client = ClientId::from_ip("127.0.0.1");

        // Allow 3 requests
        limiter.check(&client).await.unwrap();
        limiter.check(&client).await.unwrap();
        limiter.check(&client).await.unwrap();

        // 4th request should be rate limited
        let result = limiter.check(&client).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_sliding_window_window_reset() {
        let limiter = SlidingWindowLimiter::new(1, 1);
        let client = ClientId::from_ip("127.0.0.1");

        limiter.check(&client).await.unwrap();

        // Wait for window to expire
        tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;

        // Next request should be allowed
        let result = limiter.check(&client).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_adaptive_limiter_reduces_with_load() {
        let limiter = AdaptiveRateLimiter::new(100, 60);
        let client = ClientId::from_ip("127.0.0.1");

        // With 0% load, should allow requests
        assert_eq!(limiter.get_effective_limit(), 100);

        // With 50% load, limit should be reduced
        limiter.set_load(0.5);
        assert_eq!(limiter.get_effective_limit(), 75);

        // With 100% load, limit should be further reduced
        limiter.set_load(1.0);
        assert_eq!(limiter.get_effective_limit(), 50);
    }

    #[test]
    fn test_client_id_equality() {
        let client1 = ClientId::from_ip("192.168.1.1");
        let client2 = ClientId::from_ip("192.168.1.1");
        assert_eq!(client1, client2);
    }
}
