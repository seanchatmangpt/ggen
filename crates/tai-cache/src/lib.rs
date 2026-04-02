#![warn(missing_docs)]
//! tai-cache: Advanced caching library with Redis, Memcached, multi-tier strategies, and metrics
//!
//! This crate provides enterprise-grade caching capabilities:
//! - Redis integration with connection pooling and async operations
//! - Memcached for simple stateless caching
//! - Multi-tier caching with L1 (in-memory), L2 (Redis), L3 (persistent storage)
//! - Cache invalidation patterns (time-based, event-based, cascading)
//! - Distributed caching with write-through/write-behind strategies
//! - Comprehensive metrics collection and SLO monitoring

pub mod cache_invalidation;
pub mod cache_metrics;
pub mod distributed_caching;
pub mod memcached_cache;
pub mod redis_cache;

pub use cache_invalidation::{
    BroadcastInvalidator, DependencyGraph, InvalidationAuditLog, InvalidationEvent,
    InvalidationManager, InvalidationReceipt, InvalidationStrategy,
};
pub use cache_metrics::{CacheMetrics, MetricsCollector, SloCheckResult, SloChecker, TimeSeriesMetrics};
pub use distributed_caching::{
    CacheCoherence, CacheStats, CacheWarmer, InMemoryCacheTier, MultiTierCache, ReadStrategy,
    WriteStrategy,
};
pub use memcached_cache::MemcachedCache;
pub use redis_cache::{RedisCache, CacheOps};

/// Cache library version
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Default cache configuration
pub mod config {
    use std::time::Duration;

    /// Default Redis connection timeout
    pub const DEFAULT_REDIS_TIMEOUT: Duration = Duration::from_secs(30);

    /// Default cache TTL
    pub const DEFAULT_TTL: Duration = Duration::from_secs(3600); // 1 hour

    /// Default cache eviction check interval
    pub const DEFAULT_EVICTION_CHECK_INTERVAL: Duration = Duration::from_secs(60);

    /// Default memcached connection timeout
    pub const DEFAULT_MEMCACHED_TIMEOUT: Duration = Duration::from_secs(5);

    /// Default min hit rate SLO (80%)
    pub const DEFAULT_MIN_HIT_RATE: f64 = 80.0;

    /// Default max hit latency SLO (100 microseconds)
    pub const DEFAULT_MAX_HIT_LATENCY_US: u64 = 100;

    /// Default max miss latency SLO (1000 microseconds)
    pub const DEFAULT_MAX_MISS_LATENCY_US: u64 = 1000;

    /// Default max eviction rate SLO (5%)
    pub const DEFAULT_MAX_EVICTION_RATE: f64 = 5.0;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version_is_set() {
        assert!(!VERSION.is_empty());
    }

    #[test]
    fn test_config_defaults() {
        assert_eq!(config::DEFAULT_MIN_HIT_RATE, 80.0);
        assert_eq!(config::DEFAULT_MAX_HIT_LATENCY_US, 100);
    }
}
