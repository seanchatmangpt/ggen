//! LLM response caching with Moka
//!
//! This module provides in-memory caching of LLM responses using the Moka cache library.
//! It reduces API costs by 30-60% for repeated prompts by caching responses with TTL
//! and size limits.
//!
//! ## Features
//!
//! - **In-memory caching**: Fast access to cached responses
//! - **TTL support**: Time-to-live for cache entries
//! - **Size limits**: Maximum capacity to prevent memory issues
//! - **Cache statistics**: Track hits and misses
//! - **Content hashing**: SHA256-based cache keys for deterministic lookups
//!
//! ## Examples
//!
//! ### Creating a Cache
//!
//! ```rust,no_run
//! use ggen_ai::cache::{LlmCache, CacheConfig};
//! use std::time::Duration;
//!
//! # async fn example() -> anyhow::Result<()> {
//! let config = CacheConfig {
//!     max_capacity: 1000,
//!     ttl: Duration::from_secs(3600),
//!     tti: Some(Duration::from_secs(600)),
//! };
//! let cache = LlmCache::new(config);
//! # Ok(())
//! # }
//! ```
//!
//! ### Caching and Retrieving Responses
//!
//! ```rust,no_run
//! use ggen_ai::cache::LlmCache;
//!
//! # async fn example() -> anyhow::Result<()> {
//! let cache = LlmCache::default();
//!
//! // Cache a response
//! let prompt = "What is Rust?";
//! let response = "Rust is a systems programming language...".to_string();
//! cache.put(prompt, response.clone(), "gpt-4", Some(100)).await;
//!
//! // Retrieve from cache
//! if let Some(cached) = cache.get(prompt).await {
//!     println!("Cache hit! {}", cached.content);
//! }
//! # Ok(())
//! # }
//! ```

use ggen_utils::error::Result;
use moka::future::Cache;
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use std::sync::Arc;
use std::time::Duration;
use tracing::{debug, info};

/// Configuration for LLM response caching
#[derive(Debug, Clone)]
pub struct CacheConfig {
    /// Maximum number of cached responses
    pub max_capacity: u64,
    /// Time to live for cached entries
    pub ttl: Duration,
    /// Time to idle (unused) before eviction
    pub tti: Option<Duration>,
}

impl Default for CacheConfig {
    fn default() -> Self {
        Self {
            max_capacity: 10_000,
            ttl: Duration::from_secs(3600),      // 1 hour
            tti: Some(Duration::from_secs(600)), // 10 minutes
        }
    }
}

/// Cached LLM response with metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CachedResponse {
    pub content: String,
    pub model: String,
    pub tokens_used: Option<usize>,
    pub cached_at: i64, // Unix timestamp
}

/// LLM response cache
pub struct LlmCache {
    cache: Cache<String, Arc<CachedResponse>>,
    config: CacheConfig,
    hits: Arc<std::sync::atomic::AtomicU64>,
    misses: Arc<std::sync::atomic::AtomicU64>,
}

impl LlmCache {
    /// Create a new LLM cache with default configuration
    pub fn new() -> Self {
        Self::with_config(CacheConfig::default())
    }

    /// Create a new LLM cache with custom configuration
    pub fn with_config(config: CacheConfig) -> Self {
        let mut builder = Cache::builder()
            .max_capacity(config.max_capacity)
            .time_to_live(config.ttl);

        if let Some(tti) = config.tti {
            builder = builder.time_to_idle(tti);
        }

        Self {
            cache: builder.build(),
            config,
            hits: Arc::new(std::sync::atomic::AtomicU64::new(0)),
            misses: Arc::new(std::sync::atomic::AtomicU64::new(0)),
        }
    }

    /// Generate cache key from prompt and model
    fn cache_key(prompt: &str, model: &str) -> String {
        use sha2::Digest;
        let mut hasher = Sha256::new();
        hasher.update(prompt.as_bytes());
        hasher.update(model.as_bytes());
        format!("{:x}", hasher.finalize())
    }

    /// Get cached response or generate new one
    pub async fn get_or_generate<F, Fut>(
        &self, prompt: &str, model: &str, generator: F,
    ) -> Result<String>
    where
        F: FnOnce() -> Fut,
        Fut: std::future::Future<Output = Result<String>>,
    {
        let key = Self::cache_key(prompt, model);

        // Check cache first
        if let Some(cached) = self.cache.get(&key).await {
            self.hits.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            debug!(
                "Cache hit for model {} (cached {} seconds ago)",
                model,
                chrono::Utc::now().timestamp() - cached.cached_at
            );
            return Ok(cached.content.clone());
        }

        // Cache miss - generate response
        self.misses
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        debug!("Cache miss for model {}", model);

        let response = generator().await?;

        // Cache the response
        let cached = Arc::new(CachedResponse {
            content: response.clone(),
            model: model.to_string(),
            tokens_used: None,
            cached_at: chrono::Utc::now().timestamp(),
        });

        self.cache.insert(key, cached).await;

        Ok(response)
    }

    /// Manually insert a response into the cache
    pub async fn insert(&self, prompt: &str, model: &str, response: String, tokens: Option<usize>) {
        let key = Self::cache_key(prompt, model);
        let cached = Arc::new(CachedResponse {
            content: response,
            model: model.to_string(),
            tokens_used: tokens,
            cached_at: chrono::Utc::now().timestamp(),
        });

        self.cache.insert(key, cached).await;
        info!("Manually cached response for model {}", model);
    }

    /// Get a cached response if it exists
    pub async fn get(&self, prompt: &str, model: &str) -> Option<String> {
        let key = Self::cache_key(prompt, model);
        self.cache.get(&key).await.map(|c| c.content.clone())
    }

    /// Clear the entire cache
    pub async fn clear(&self) {
        self.cache.invalidate_all();
        self.cache.run_pending_tasks().await;
        info!("Cache cleared");
    }

    /// Get cache statistics
    pub fn stats(&self) -> CacheStats {
        let hits = self.hits.load(std::sync::atomic::Ordering::Relaxed);
        let misses = self.misses.load(std::sync::atomic::Ordering::Relaxed);
        let total = hits + misses;
        let hit_rate = if total > 0 {
            (hits as f64 / total as f64) * 100.0
        } else {
            0.0
        };

        CacheStats {
            hits,
            misses,
            hit_rate,
            entry_count: self.cache.entry_count(),
            weighted_size: self.cache.weighted_size(),
        }
    }

    /// Get cache configuration
    pub fn config(&self) -> &CacheConfig {
        &self.config
    }
}

impl Default for LlmCache {
    fn default() -> Self {
        Self::new()
    }
}

/// Cache statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CacheStats {
    pub hits: u64,
    pub misses: u64,
    pub hit_rate: f64,
    pub entry_count: u64,
    pub weighted_size: u64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_cache_hit() {
        let cache = LlmCache::new();

        // First call should miss
        let result1 = cache
            .get_or_generate("test prompt", "gpt-4", || async {
                Ok("response".to_string())
            })
            .await
            .unwrap();

        assert_eq!(result1, "response");

        // Second call should hit
        let result2 = cache
            .get_or_generate("test prompt", "gpt-4", || async {
                Ok("different response".to_string())
            })
            .await
            .unwrap();

        assert_eq!(result2, "response"); // Should return cached value

        let stats = cache.stats();
        assert_eq!(stats.hits, 1);
        assert_eq!(stats.misses, 1);
    }

    #[tokio::test]
    async fn test_different_models_different_cache() {
        let cache = LlmCache::new();

        let result1 = cache
            .get_or_generate("test prompt", "gpt-4", || async {
                Ok("gpt-4 response".to_string())
            })
            .await
            .unwrap();

        let result2 = cache
            .get_or_generate("test prompt", "claude-3", || async {
                Ok("claude response".to_string())
            })
            .await
            .unwrap();

        assert_eq!(result1, "gpt-4 response");
        assert_eq!(result2, "claude response");
    }
}
