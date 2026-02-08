//! Production-ready rate limiting middleware with token bucket algorithm
//!
//! Provides per-IP and per-API-key rate limiting with:
//! - Token bucket algorithm for burst and sustained rate control
//! - Redis backend for distributed limiting
//! - In-memory fallback for graceful degradation
//! - Configurable limits and burst sizes
//! - Comprehensive observability via tracing

use async_trait::async_trait;
use axum::{
    extract::{Request, State},
    http::{HeaderMap, HeaderValue, StatusCode},
    middleware::Next,
    response::{IntoResponse, Response},
};
use std::net::IpAddr;
use std::sync::Arc;
use std::time::{Duration, SystemTime};
use thiserror::Error;
use tracing::{debug, error, warn};

/// Rate limiting configuration
#[derive(Debug, Clone)]
pub struct RateLimitConfig {
    /// Maximum requests per second (sustained rate)
    pub requests_per_second: u32,
    /// Maximum burst size (token bucket capacity)
    pub burst_size: u32,
    /// Backend storage implementation
    pub backend: RateLimitBackend,
    /// Time to live for rate limit data
    pub ttl: Duration,
}

impl Default for RateLimitConfig {
    fn default() -> Self {
        Self {
            requests_per_second: 10,
            burst_size: 20,
            backend: RateLimitBackend::InMemory,
            ttl: Duration::from_secs(60),
        }
    }
}

/// Backend storage for rate limiting
#[derive(Debug, Clone)]
pub enum RateLimitBackend {
    /// In-memory storage (single instance, no distribution)
    InMemory,
    /// Redis storage (distributed, multi-instance)
    Redis { url: String },
}

/// Rate limiting errors
#[derive(Error, Debug)]
pub enum RateLimitError {
    #[error("Rate limit exceeded. Retry after {retry_after} seconds")]
    TooManyRequests { retry_after: u64 },

    #[error("Backend error: {0}")]
    BackendError(String),

    #[error("Invalid configuration: {0}")]
    InvalidConfig(String),
}

impl IntoResponse for RateLimitError {
    fn into_response(self) -> Response {
        match self {
            RateLimitError::TooManyRequests { retry_after } => {
                let mut headers = HeaderMap::new();
                if let Ok(value) = retry_after.to_string().parse() {
                    headers.insert("Retry-After", value);
                }
                // HeaderValue doesn't implement Default, so use from_static directly
                headers.insert("X-RateLimit-Limit", HeaderValue::from_static("0"));
                headers.insert("X-RateLimit-Remaining", HeaderValue::from_static("0"));

                (
                    StatusCode::TOO_MANY_REQUESTS,
                    headers,
                    format!("Rate limit exceeded. Retry after {} seconds", retry_after),
                )
                    .into_response()
            }
            RateLimitError::BackendError(msg) => {
                error!("Rate limit backend error: {}", msg);
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    "Rate limiting temporarily unavailable",
                )
                    .into_response()
            }
            RateLimitError::InvalidConfig(msg) => {
                error!("Rate limit configuration error: {}", msg);
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    "Rate limiting configuration error",
                )
                    .into_response()
            }
        }
    }
}

/// Token bucket state
#[derive(Debug, Clone)]
struct TokenBucket {
    /// Available tokens
    tokens: f64,
    /// Last refill timestamp
    last_refill: SystemTime,
}

impl TokenBucket {
    fn new(capacity: u32) -> Self {
        Self {
            tokens: capacity as f64,
            last_refill: SystemTime::now(),
        }
    }

    /// Attempt to consume a token, returns true if successful
    fn try_consume(&mut self, refill_rate: f64, capacity: u32) -> bool {
        // Refill tokens based on elapsed time
        let now = SystemTime::now();
        if let Ok(elapsed) = now.duration_since(self.last_refill) {
            let refill_amount = elapsed.as_secs_f64() * refill_rate;
            self.tokens = (self.tokens + refill_amount).min(capacity as f64);
            self.last_refill = now;
        }

        // Try to consume a token
        if self.tokens >= 1.0 {
            self.tokens -= 1.0;
            true
        } else {
            false
        }
    }

    /// Calculate seconds until next token is available
    fn retry_after(&self, refill_rate: f64) -> u64 {
        if self.tokens >= 1.0 {
            0
        } else {
            ((1.0 - self.tokens) / refill_rate).ceil() as u64
        }
    }
}

/// Backend trait for rate limiting storage
#[async_trait]
pub trait RateLimitBackendTrait: Send + Sync {
    /// Check if request is allowed and record it
    async fn check_limit(
        &self, key: &str, config: &RateLimitConfig,
    ) -> Result<bool, RateLimitError>;

    /// Get retry-after duration in seconds
    async fn retry_after(&self, key: &str, config: &RateLimitConfig)
        -> Result<u64, RateLimitError>;
}

/// In-memory rate limiting backend
struct InMemoryBackend {
    buckets: Arc<tokio::sync::RwLock<std::collections::HashMap<String, TokenBucket>>>,
}

impl InMemoryBackend {
    fn new() -> Self {
        Self {
            buckets: Arc::new(tokio::sync::RwLock::new(std::collections::HashMap::new())),
        }
    }
}

#[async_trait]
impl RateLimitBackendTrait for InMemoryBackend {
    async fn check_limit(
        &self, key: &str, config: &RateLimitConfig,
    ) -> Result<bool, RateLimitError> {
        let mut buckets = self.buckets.write().await;
        let bucket = buckets
            .entry(key.to_string())
            .or_insert_with(|| TokenBucket::new(config.burst_size));

        let refill_rate = config.requests_per_second as f64;
        Ok(bucket.try_consume(refill_rate, config.burst_size))
    }

    async fn retry_after(
        &self, key: &str, config: &RateLimitConfig,
    ) -> Result<u64, RateLimitError> {
        let buckets = self.buckets.read().await;
        let refill_rate = config.requests_per_second as f64;

        Ok(buckets
            .get(key)
            .map(|b| b.retry_after(refill_rate))
            .unwrap_or(0))
    }
}

/// Redis rate limiting backend
struct RedisBackend {
    pool: bb8::Pool<bb8_redis::RedisConnectionManager>,
}

impl RedisBackend {
    async fn new(url: &str) -> Result<Self, RateLimitError> {
        let manager = bb8_redis::RedisConnectionManager::new(url)
            .map_err(|e| RateLimitError::BackendError(format!("Redis connection failed: {}", e)))?;

        let pool = bb8::Pool::builder()
            .max_size(15)
            .build(manager)
            .await
            .map_err(|e| {
                RateLimitError::BackendError(format!("Redis pool creation failed: {}", e))
            })?;

        Ok(Self { pool })
    }

    async fn execute_lua_script(
        &self, key: &str, config: &RateLimitConfig,
    ) -> Result<(i64, f64), RateLimitError> {
        use redis::AsyncCommands;

        let mut conn = self.pool.get().await.map_err(|e| {
            RateLimitError::BackendError(format!("Failed to get Redis connection: {}", e))
        })?;

        // Lua script for atomic token bucket algorithm
        let script = r#"
            local key = KEYS[1]
            local capacity = tonumber(ARGV[1])
            local refill_rate = tonumber(ARGV[2])
            local now = tonumber(ARGV[3])
            local ttl = tonumber(ARGV[4])

            local bucket = redis.call('HMGET', key, 'tokens', 'last_refill')
            local tokens = tonumber(bucket[1]) or capacity
            local last_refill = tonumber(bucket[2]) or now

            -- Refill tokens
            local elapsed = now - last_refill
            local refill_amount = elapsed * refill_rate
            tokens = math.min(tokens + refill_amount, capacity)

            -- Try to consume
            local allowed = 0
            if tokens >= 1.0 then
                tokens = tokens - 1.0
                allowed = 1
            end

            -- Update state
            redis.call('HMSET', key, 'tokens', tokens, 'last_refill', now)
            redis.call('EXPIRE', key, ttl)

            return {allowed, tokens}
        "#;

        let now = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs_f64();

        let result: (i64, f64) = redis::Script::new(script)
            .key(key)
            .arg(config.burst_size)
            .arg(config.requests_per_second)
            .arg(now)
            .arg(config.ttl.as_secs())
            .invoke_async(&mut *conn)
            .await
            .map_err(|e| {
                RateLimitError::BackendError(format!("Redis script execution failed: {}", e))
            })?;

        Ok(result)
    }
}

#[async_trait]
impl RateLimitBackendTrait for RedisBackend {
    async fn check_limit(
        &self, key: &str, config: &RateLimitConfig,
    ) -> Result<bool, RateLimitError> {
        match self.execute_lua_script(key, config).await {
            Ok((allowed, _)) => Ok(allowed == 1),
            Err(e) => {
                warn!("Redis rate limit check failed: {}", e);
                Err(e)
            }
        }
    }

    async fn retry_after(
        &self, key: &str, config: &RateLimitConfig,
    ) -> Result<u64, RateLimitError> {
        match self.execute_lua_script(key, config).await {
            Ok((_, tokens)) => {
                let refill_rate = config.requests_per_second as f64;
                if tokens >= 1.0 {
                    Ok(0)
                } else {
                    Ok(((1.0 - tokens) / refill_rate).ceil() as u64)
                }
            }
            Err(e) => {
                warn!("Redis retry-after calculation failed: {}", e);
                Err(e)
            }
        }
    }
}

/// Main rate limiter
pub struct RateLimiter {
    config: RateLimitConfig,
    backend: Arc<dyn RateLimitBackendTrait>,
    fallback: Arc<InMemoryBackend>,
}

impl RateLimiter {
    /// Create a new rate limiter with the given configuration
    pub async fn new(config: RateLimitConfig) -> Result<Self, RateLimitError> {
        let backend: Arc<dyn RateLimitBackendTrait> = match &config.backend {
            RateLimitBackend::InMemory => Arc::new(InMemoryBackend::new()),
            RateLimitBackend::Redis { url } => Arc::new(RedisBackend::new(url).await?),
        };

        let fallback = Arc::new(InMemoryBackend::new());

        Ok(Self {
            config,
            backend,
            fallback,
        })
    }

    /// Check rate limit for a given key
    pub async fn check_limit(&self, key: &str) -> Result<(), RateLimitError> {
        debug!("Checking rate limit for key: {}", key);

        // Try primary backend
        match self.backend.check_limit(key, &self.config).await {
            Ok(true) => {
                debug!("Rate limit check passed for key: {}", key);
                Ok(())
            }
            Ok(false) => {
                let retry_after = self.retry_after(key).await?;
                warn!(
                    "Rate limit exceeded for key: {}, retry after: {}s",
                    key, retry_after
                );
                Err(RateLimitError::TooManyRequests { retry_after })
            }
            Err(e) => {
                // Graceful degradation to in-memory backend
                warn!("Primary backend failed, falling back to in-memory: {}", e);
                match self.fallback.check_limit(key, &self.config).await {
                    Ok(true) => Ok(()),
                    Ok(false) => {
                        let retry_after = self.fallback.retry_after(key, &self.config).await?;
                        Err(RateLimitError::TooManyRequests { retry_after })
                    }
                    Err(fallback_error) => {
                        error!("Fallback backend also failed: {}", fallback_error);
                        Err(e)
                    }
                }
            }
        }
    }

    /// Record a request (same as check_limit for token bucket)
    pub async fn record_request(&self, key: &str) -> Result<(), RateLimitError> {
        self.check_limit(key).await
    }

    /// Get retry-after duration for a key
    async fn retry_after(&self, key: &str) -> Result<u64, RateLimitError> {
        self.backend.retry_after(key, &self.config).await
    }
}

/// Extract client identifier from request
fn extract_client_id(headers: &HeaderMap, client_ip: Option<IpAddr>) -> String {
    // Prefer API key if present
    if let Some(api_key) = headers.get("X-API-Key") {
        if let Ok(key) = api_key.to_str() {
            return format!("api:{}", key);
        }
    }

    // Fall back to IP address
    if let Some(ip) = client_ip {
        return format!("ip:{}", ip);
    }

    // Last resort: use a default key (will rate limit all anonymous traffic together)
    "anonymous".to_string()
}

/// Axum middleware for rate limiting
pub async fn rate_limit_middleware(
    State(limiter): State<Arc<RateLimiter>>, request: Request, next: Next,
) -> Result<Response, RateLimitError> {
    // Extract client identifier
    let client_id = extract_client_id(
        request.headers(),
        request
            .extensions()
            .get::<std::net::SocketAddr>()
            .map(|addr| addr.ip()),
    );

    // Check rate limit
    limiter.check_limit(&client_id).await?;

    // Continue to next handler
    Ok(next.run(request).await)
}

#[cfg(test)]
mod tests {
    use super::*;
    use tokio::time::sleep;

    // Chicago TDD: State-based testing with real collaborators

    #[tokio::test]
    async fn test_token_bucket_allows_burst() {
        // Arrange: Create in-memory rate limiter with burst capacity
        let config = RateLimitConfig {
            requests_per_second: 2,
            burst_size: 5,
            backend: RateLimitBackend::InMemory,
            ttl: Duration::from_secs(60),
        };
        let limiter = RateLimiter::new(config).await.unwrap();

        // Act: Send burst of requests up to burst_size
        let mut results = Vec::new();
        for _ in 0..5 {
            results.push(limiter.check_limit("test-key").await.is_ok());
        }

        // Assert: All burst requests should succeed
        assert_eq!(results.iter().filter(|&&r| r).count(), 5);

        // Act: Send one more request beyond burst
        let beyond_burst = limiter.check_limit("test-key").await;

        // Assert: Should be rate limited
        assert!(beyond_burst.is_err());
        match beyond_burst {
            Err(RateLimitError::TooManyRequests { retry_after }) => {
                assert!(retry_after > 0);
            }
            _ => panic!("Expected TooManyRequests error"),
        }
    }

    #[tokio::test]
    async fn test_token_bucket_refills_over_time() {
        // Arrange: Create limiter with slow refill rate
        let config = RateLimitConfig {
            requests_per_second: 10, // 10 tokens per second = 1 token per 100ms
            burst_size: 2,
            backend: RateLimitBackend::InMemory,
            ttl: Duration::from_secs(60),
        };
        let limiter = RateLimiter::new(config).await.unwrap();

        // Act: Consume all tokens
        assert!(limiter.check_limit("refill-test").await.is_ok());
        assert!(limiter.check_limit("refill-test").await.is_ok());
        assert!(limiter.check_limit("refill-test").await.is_err());

        // Wait for refill (200ms = 2 tokens at 10/sec)
        sleep(Duration::from_millis(200)).await;

        // Act: Try again after refill
        let result = limiter.check_limit("refill-test").await;

        // Assert: Should succeed after refill
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_separate_keys_have_independent_limits() {
        // Arrange: Create shared limiter
        let config = RateLimitConfig {
            requests_per_second: 1,
            burst_size: 1,
            backend: RateLimitBackend::InMemory,
            ttl: Duration::from_secs(60),
        };
        let limiter = RateLimiter::new(config).await.unwrap();

        // Act: Exhaust limit for key1
        assert!(limiter.check_limit("key1").await.is_ok());
        assert!(limiter.check_limit("key1").await.is_err());

        // Act: Try different key
        let key2_result = limiter.check_limit("key2").await;

        // Assert: key2 should have independent limit
        assert!(key2_result.is_ok());
    }

    #[tokio::test]
    async fn test_record_request_same_as_check_limit() {
        // Arrange
        let config = RateLimitConfig {
            requests_per_second: 1,
            burst_size: 1,
            backend: RateLimitBackend::InMemory,
            ttl: Duration::from_secs(60),
        };
        let limiter = RateLimiter::new(config).await.unwrap();

        // Act: Use record_request
        let record_result = limiter.record_request("record-test").await;

        // Assert: Should succeed
        assert!(record_result.is_ok());

        // Act: Try check_limit immediately after
        let check_result = limiter.check_limit("record-test").await;

        // Assert: Should be rate limited (tokens exhausted)
        assert!(check_result.is_err());
    }

    #[tokio::test]
    async fn test_default_config_values() {
        // Arrange & Act
        let config = RateLimitConfig::default();

        // Assert: Verify sensible defaults
        assert_eq!(config.requests_per_second, 10);
        assert_eq!(config.burst_size, 20);
        assert_eq!(config.ttl, Duration::from_secs(60));
        assert!(matches!(config.backend, RateLimitBackend::InMemory));
    }

    #[tokio::test]
    async fn test_extract_client_id_prefers_api_key() {
        // Arrange
        let mut headers = HeaderMap::new();
        headers.insert("X-API-Key", "test-api-key-123".parse().unwrap());
        let ip = Some("192.168.1.1".parse().unwrap());

        // Act
        let client_id = extract_client_id(&headers, ip);

        // Assert: Should use API key, not IP
        assert_eq!(client_id, "api:test-api-key-123");
    }

    #[tokio::test]
    async fn test_extract_client_id_falls_back_to_ip() {
        // Arrange
        let headers = HeaderMap::new();
        let ip = Some("192.168.1.1".parse().unwrap());

        // Act
        let client_id = extract_client_id(&headers, ip);

        // Assert: Should use IP
        assert_eq!(client_id, "ip:192.168.1.1");
    }

    #[tokio::test]
    async fn test_extract_client_id_uses_anonymous_as_last_resort() {
        // Arrange
        let headers = HeaderMap::new();
        let ip = None;

        // Act
        let client_id = extract_client_id(&headers, ip);

        // Assert: Should use anonymous
        assert_eq!(client_id, "anonymous");
    }

    #[tokio::test]
    async fn test_retry_after_calculation() {
        // Arrange
        let config = RateLimitConfig {
            requests_per_second: 1, // 1 token per second
            burst_size: 1,
            backend: RateLimitBackend::InMemory,
            ttl: Duration::from_secs(60),
        };
        let limiter = RateLimiter::new(config).await.unwrap();

        // Act: Exhaust tokens
        limiter.check_limit("retry-test").await.unwrap();
        let result = limiter.check_limit("retry-test").await;

        // Assert: Should have retry-after value
        match result {
            Err(RateLimitError::TooManyRequests { retry_after }) => {
                assert!(retry_after > 0);
                assert!(retry_after <= 1); // Should be at most 1 second
            }
            _ => panic!("Expected TooManyRequests error with retry_after"),
        }
    }

    #[tokio::test]
    async fn test_high_concurrency_rate_limiting() {
        // Arrange: Create limiter with small burst
        let config = RateLimitConfig {
            requests_per_second: 10,
            burst_size: 5,
            backend: RateLimitBackend::InMemory,
            ttl: Duration::from_secs(60),
        };
        let limiter = Arc::new(RateLimiter::new(config).await.unwrap());

        // Act: Spawn concurrent requests
        let mut handles = Vec::new();
        for _ in 0..20 {
            let limiter_clone = Arc::clone(&limiter);
            let handle =
                tokio::spawn(
                    async move { limiter_clone.check_limit("concurrent-test").await.is_ok() },
                );
            handles.push(handle);
        }

        // Collect results
        let mut success_count = 0;
        for handle in handles {
            if handle.await.unwrap() {
                success_count += 1;
            }
        }

        // Assert: Should allow burst_size requests, block the rest
        assert!(
            success_count <= 5,
            "Expected at most 5 successful requests, got {}",
            success_count
        );
        assert!(
            success_count >= 3,
            "Expected at least some requests to succeed, got {}",
            success_count
        );
    }

    #[tokio::test]
    async fn test_error_response_includes_retry_after_header() {
        // Arrange
        let error = RateLimitError::TooManyRequests { retry_after: 42 };

        // Act
        let response = error.into_response();

        // Assert: Check status code and headers
        assert_eq!(response.status(), StatusCode::TOO_MANY_REQUESTS);

        let headers = response.headers();
        assert!(headers.contains_key("Retry-After"));
        if let Some(retry_after) = headers.get("Retry-After") {
            assert_eq!(retry_after.to_str().unwrap(), "42");
        }
    }

    #[tokio::test]
    async fn test_backend_error_returns_500() {
        // Arrange
        let error = RateLimitError::BackendError("Redis connection failed".to_string());

        // Act
        let response = error.into_response();

        // Assert: Should return 500
        assert_eq!(response.status(), StatusCode::INTERNAL_SERVER_ERROR);
    }
}
