//! Rate limiting middleware

use axum::{
    extract::Request,
    http::StatusCode,
    middleware::Next,
    response::{IntoResponse, Response},
};
use moka::future::Cache;
use std::sync::Arc;
use std::time::Duration;

/// Rate limiter for API requests
pub struct RateLimiter {
    // Request count per IP per minute
    request_counts: Arc<Cache<String, u32>>,
    max_requests_per_minute: u32,
}

impl RateLimiter {
    pub fn new(max_requests_per_minute: u32) -> Self {
        Self {
            request_counts: Arc::new(
                Cache::builder()
                    .time_to_live(Duration::from_secs(60))
                    .build(),
            ),
            max_requests_per_minute,
        }
    }

    pub async fn check_rate_limit(&self, ip: &str) -> Result<(), RateLimitError> {
        let current = self
            .request_counts
            .try_get_with(ip.to_string(), async { Ok::<u32, std::io::Error>(0) })
            .await
            .unwrap_or(0);

        if current >= self.max_requests_per_minute {
            return Err(RateLimitError::TooManyRequests);
        }

        self.request_counts
            .insert(ip.to_string(), current + 1)
            .await;

        Ok(())
    }
}

#[derive(Debug)]
pub enum RateLimitError {
    TooManyRequests,
}

impl IntoResponse for RateLimitError {
    fn into_response(self) -> Response {
        match self {
            RateLimitError::TooManyRequests => (
                StatusCode::TOO_MANY_REQUESTS,
                "Rate limit exceeded",
            )
                .into_response(),
        }
    }
}

/// Apply rate limiting to requests
pub async fn rate_limit(
    request: Request,
    next: Next,
) -> Response {
    // TODO: Extract client IP from request
    // TODO: Apply rate limiter
    // TODO: Return 429 if limit exceeded

    next.run(request).await
}
