//! Rate limiting strategies for request throttling
//!
//! This module provides multiple rate limiting algorithms:
//! - Token bucket: Smooth rate limiting with burst capacity
//! - Sliding window: Per-time-window request counting
//! - Adaptive: Dynamic limits based on system load

use crate::error::{GatewayError, GatewayResult};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};
use tokio::sync::RwLock;

pub mod strategies;

pub use strategies::{AdaptiveRateLimiter, SlidingWindowLimiter, TokenBucketLimiter};

/// Rate limiting configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RateLimitConfig {
    /// Request limit per window
    pub limit: u32,
    /// Time window in seconds
    pub window_seconds: u64,
    /// Rate limiting strategy
    pub strategy: RateLimitStrategy,
}

/// Available rate limiting strategies
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum RateLimitStrategy {
    TokenBucket,
    SlidingWindow,
    Adaptive,
}

/// Rate limit decision with metadata
#[derive(Debug, Clone)]
pub struct RateLimitDecision {
    /// Whether the request is allowed
    pub allowed: bool,
    /// Remaining requests in current window
    pub remaining: u32,
    /// Time to reset (seconds)
    pub reset_after_seconds: u64,
    /// Retry after (seconds, if rejected)
    pub retry_after_seconds: Option<u64>,
}

impl RateLimitDecision {
    /// Create an allowed decision
    pub fn allowed(remaining: u32, reset_after: u64) -> Self {
        Self {
            allowed: true,
            remaining,
            reset_after_seconds: reset_after,
            retry_after_seconds: None,
        }
    }

    /// Create a rejected decision
    pub fn rejected(reset_after: u64) -> Self {
        Self {
            allowed: false,
            remaining: 0,
            reset_after_seconds: reset_after,
            retry_after_seconds: Some(1),
        }
    }
}

/// Client identifier for rate limiting (IP, API key, user ID, etc.)
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct ClientId(String);

impl ClientId {
    /// Create from an IP address
    pub fn from_ip(ip: &str) -> Self {
        Self(format!("ip:{}", ip))
    }

    /// Create from an API key
    pub fn from_api_key(key: &str) -> Self {
        Self(format!("key:{}", key))
    }

    /// Create from a user ID
    pub fn from_user_id(user_id: &str) -> Self {
        Self(format!("user:{}", user_id))
    }
}

impl std::fmt::Display for ClientId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rate_limit_config_creation() {
        let config = RateLimitConfig {
            limit: 100,
            window_seconds: 60,
            strategy: RateLimitStrategy::TokenBucket,
        };

        assert_eq!(config.limit, 100);
        assert_eq!(config.window_seconds, 60);
    }

    #[test]
    fn test_client_id_from_ip() {
        let client = ClientId::from_ip("192.168.1.1");
        assert_eq!(client.to_string(), "ip:192.168.1.1");
    }

    #[test]
    fn test_client_id_from_api_key() {
        let client = ClientId::from_api_key("sk-123456");
        assert_eq!(client.to_string(), "key:sk-123456");
    }

    #[test]
    fn test_rate_limit_decision_allowed() {
        let decision = RateLimitDecision::allowed(50, 30);
        assert!(decision.allowed);
        assert_eq!(decision.remaining, 50);
        assert_eq!(decision.reset_after_seconds, 30);
        assert!(decision.retry_after_seconds.is_none());
    }

    #[test]
    fn test_rate_limit_decision_rejected() {
        let decision = RateLimitDecision::rejected(30);
        assert!(!decision.allowed);
        assert_eq!(decision.remaining, 0);
        assert_eq!(decision.retry_after_seconds, Some(1));
    }
}
