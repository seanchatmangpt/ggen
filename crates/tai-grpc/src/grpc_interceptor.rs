//! Request/response interceptors for middleware functionality

use std::sync::Arc;

use parking_lot::RwLock;
use tonic::service::Interceptor;
use tonic::{Request, Status};
use tracing::{debug, warn};

/// Request metrics
#[derive(Debug, Clone, Default)]
pub struct RequestMetrics {
    /// Total requests processed
    pub total_requests: u64,
    /// Total successful responses
    pub successful_responses: u64,
    /// Total failed responses
    pub failed_responses: u64,
    /// Total request latency in microseconds
    pub total_latency_us: u64,
}

impl RequestMetrics {
    /// Calculate average latency
    pub fn avg_latency_us(&self) -> u64 {
        if self.total_requests == 0 {
            0
        } else {
            self.total_latency_us / self.total_requests
        }
    }

    /// Calculate success rate as percentage
    pub fn success_rate(&self) -> f64 {
        if self.total_requests == 0 {
            0.0
        } else {
            (self.successful_responses as f64 / self.total_requests as f64) * 100.0
        }
    }
}

/// Request interceptor trait
pub trait RequestInterceptor: Send + Sync {
    /// Called before request processing
    fn before_request(&self, method: &str);

    /// Called after request processing
    fn after_request(&self, method: &str, success: bool, latency_us: u64);

    /// Get current metrics
    fn metrics(&self) -> RequestMetrics;
}

/// Logging interceptor implementation
#[derive(Debug, Clone)]
pub struct LoggingInterceptor {
    metrics: Arc<RwLock<RequestMetrics>>,
}

impl LoggingInterceptor {
    /// Create a new logging interceptor
    pub fn new() -> Self {
        Self {
            metrics: Arc::new(RwLock::new(RequestMetrics::default())),
        }
    }
}

impl Default for LoggingInterceptor {
    fn default() -> Self {
        Self::new()
    }
}

impl RequestInterceptor for LoggingInterceptor {
    fn before_request(&self, method: &str) {
        debug!("RPC call started: {}", method);
    }

    fn after_request(&self, method: &str, success: bool, latency_us: u64) {
        let mut metrics = self.metrics.write();
        metrics.total_requests += 1;
        metrics.total_latency_us += latency_us;

        if success {
            metrics.successful_responses += 1;
            debug!("RPC call succeeded: {} ({}µs)", method, latency_us);
        } else {
            metrics.failed_responses += 1;
            warn!("RPC call failed: {} ({}µs)", method, latency_us);
        }
    }

    fn metrics(&self) -> RequestMetrics {
        self.metrics.read().clone()
    }
}

/// Authentication interceptor for JWT validation
#[derive(Debug, Clone)]
pub struct AuthInterceptor {
    /// Enable JWT validation
    pub enabled: bool,
}

impl AuthInterceptor {
    /// Create a new auth interceptor
    pub fn new(enabled: bool) -> Self {
        Self { enabled }
    }

    /// Validate JWT token
    pub fn validate_token(&self, token: &str) -> Result<(), Status> {
        if !self.enabled {
            return Ok(());
        }

        // Simple token validation (in production, use proper JWT library)
        if token.is_empty() {
            return Err(Status::unauthenticated("Missing authentication token"));
        }

        if !token.starts_with("Bearer ") {
            return Err(Status::unauthenticated("Invalid token format"));
        }

        Ok(())
    }
}

impl Interceptor for AuthInterceptor {
    fn call(&mut self, mut request: Request<()>) -> Result<Request<()>, Status> {
        let metadata = request.metadata();

        if self.enabled {
            if let Some(token) = metadata.get("authorization") {
                let token_str = token.to_str().map_err(|_| {
                    Status::unauthenticated("Failed to parse authorization header")
                })?;

                self.validate_token(token_str)?;
                debug!("Authentication successful");
            } else {
                return Err(Status::unauthenticated("Missing authorization metadata"));
            }
        }

        Ok(request)
    }
}

/// Metrics collection interceptor
#[derive(Debug, Clone)]
pub struct MetricsInterceptor {
    metrics: Arc<RwLock<RequestMetrics>>,
}

impl MetricsInterceptor {
    /// Create a new metrics interceptor
    pub fn new() -> Self {
        Self {
            metrics: Arc::new(RwLock::new(RequestMetrics::default())),
        }
    }

    /// Record request metrics
    pub fn record_request(&self, success: bool, latency_us: u64) {
        let mut metrics = self.metrics.write();
        metrics.total_requests += 1;
        metrics.total_latency_us += latency_us;

        if success {
            metrics.successful_responses += 1;
        } else {
            metrics.failed_responses += 1;
        }
    }

    /// Get current metrics
    pub fn metrics(&self) -> RequestMetrics {
        self.metrics.read().clone()
    }
}

impl Default for MetricsInterceptor {
    fn default() -> Self {
        Self::new()
    }
}

impl Interceptor for MetricsInterceptor {
    fn call(&mut self, request: Request<()>) -> Result<Request<()>, Status> {
        let metadata = request.metadata();
        if let Some(user_agent) = metadata.get("user-agent") {
            debug!("Request from: {:?}", user_agent);
        }
        Ok(request)
    }
}

/// Rate limiting interceptor
#[derive(Debug, Clone)]
pub struct RateLimitInterceptor {
    /// Maximum requests per second
    max_requests_per_sec: u32,
    /// Current request count
    request_count: Arc<RwLock<u32>>,
    /// Last reset time
    last_reset: Arc<RwLock<std::time::Instant>>,
}

impl RateLimitInterceptor {
    /// Create a new rate limit interceptor
    pub fn new(max_requests_per_sec: u32) -> Self {
        Self {
            max_requests_per_sec,
            request_count: Arc::new(RwLock::new(0)),
            last_reset: Arc::new(RwLock::new(std::time::Instant::now())),
        }
    }

    /// Check if request is allowed
    pub fn is_allowed(&self) -> Result<(), Status> {
        let mut last_reset = self.last_reset.write();
        let mut count = self.request_count.write();

        // Reset counter every second
        if last_reset.elapsed() > std::time::Duration::from_secs(1) {
            *count = 0;
            *last_reset = std::time::Instant::now();
        }

        *count += 1;
        if *count > self.max_requests_per_sec {
            return Err(Status::resource_exhausted("Rate limit exceeded"));
        }

        Ok(())
    }
}

impl Interceptor for RateLimitInterceptor {
    fn call(&mut self, request: Request<()>) -> Result<Request<()>, Status> {
        self.is_allowed()?;
        Ok(request)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_logging_interceptor() {
        let interceptor = LoggingInterceptor::new();

        interceptor.before_request("test_method");
        interceptor.after_request("test_method", true, 100);

        let metrics = interceptor.metrics();
        assert_eq!(metrics.total_requests, 1);
        assert_eq!(metrics.successful_responses, 1);
        assert_eq!(metrics.failed_responses, 0);
        assert_eq!(metrics.total_latency_us, 100);
    }

    #[test]
    fn test_logging_interceptor_success_rate() {
        let interceptor = LoggingInterceptor::new();

        for _ in 0..10 {
            interceptor.after_request("test", true, 100);
        }

        for _ in 0..5 {
            interceptor.after_request("test", false, 100);
        }

        let metrics = interceptor.metrics();
        assert_eq!(metrics.total_requests, 15);
        assert_eq!(metrics.successful_responses, 10);
        assert_eq!(metrics.failed_responses, 5);
        assert!((metrics.success_rate() - 66.66666).abs() < 0.01);
    }

    #[test]
    fn test_auth_interceptor_validation() {
        let interceptor = AuthInterceptor::new(true);

        // Valid token
        assert!(interceptor.validate_token("Bearer valid_token").is_ok());

        // Invalid token format
        assert!(interceptor.validate_token("invalid_token").is_err());

        // Empty token
        assert!(interceptor.validate_token("").is_err());
    }

    #[test]
    fn test_rate_limit_interceptor() {
        let interceptor = RateLimitInterceptor::new(5);

        // Should allow first 5 requests
        for _ in 0..5 {
            assert!(interceptor.is_allowed().is_ok());
        }

        // Should reject 6th request
        assert!(interceptor.is_allowed().is_err());
    }
}
