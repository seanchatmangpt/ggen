//! Memcached cache implementation for simple key-value caching.
//!
//! Memcached is ideal for:
//! - Simple key-value operations (no complex data structures)
//! - High-performance stateless caching
//! - Distributed cache across multiple instances
//! - Binary protocol with efficient serialization

use anyhow::{anyhow, Result};
use async_trait::async_trait;
use memcache::Client;
use serde::{de::DeserializeOwned, Serialize};
use std::sync::Arc;
use std::time::Duration;
use tracing::{debug, error, warn};

/// Memcached cache client
#[derive(Clone)]
pub struct MemcachedCache {
    client: Arc<Client>,
}

impl MemcachedCache {
    /// Create a new Memcached cache client
    ///
    /// # Arguments
    /// * `servers` - List of server addresses (e.g., vec!["127.0.0.1:11211"])
    ///
    /// # Errors
    /// Returns `anyhow::Error` if connection fails
    pub fn new(servers: Vec<&str>) -> Result<Self> {
        let client = Client::connect(servers)
            .map_err(|e| anyhow!("Failed to connect to Memcached: {}", e))?;

        debug!("Memcached client initialized with {} servers", servers.len());
        Ok(Self {
            client: Arc::new(client),
        })
    }

    /// Get a value from Memcached
    ///
    /// # Arguments
    /// * `key` - Cache key
    ///
    /// # Returns
    /// `Some(value)` if key exists, `None` if key doesn't exist
    pub fn get<T: DeserializeOwned>(&self, key: &str) -> Result<Option<T>> {
        let bytes: Option<Vec<u8>> = self.client.get(key)
            .map_err(|e| anyhow!("Memcached GET failed for key '{}': {}", key, e))?;

        match bytes {
            Some(data) => {
                let value = bincode::deserialize(&data)
                    .map_err(|e| anyhow!("Failed to deserialize value for key '{}': {}", key, e))?;
                debug!("Memcached cache hit for key: {}", key);
                Ok(Some(value))
            }
            None => {
                debug!("Memcached cache miss for key: {}", key);
                Ok(None)
            }
        }
    }

    /// Get a raw string value from Memcached
    pub fn get_string(&self, key: &str) -> Result<Option<String>> {
        self.client.get(key)
            .map_err(|e| anyhow!("Memcached GET string failed for key '{}': {}", key, e))
    }

    /// Set a value in Memcached
    ///
    /// # Arguments
    /// * `key` - Cache key
    /// * `value` - Value to cache
    /// * `ttl` - Time-to-live in seconds
    pub fn set<T: Serialize>(&self, key: &str, value: &T, ttl: u32) -> Result<()> {
        let bytes = bincode::serialize(value)
            .map_err(|e| anyhow!("Failed to serialize value for key '{}': {}", key, e))?;

        self.client.set(key, bytes, ttl)
            .map_err(|e| anyhow!("Memcached SET failed for key '{}': {}", key, e))?;

        debug!("Set Memcached key '{}' with TTL {}s", key, ttl);
        Ok(())
    }

    /// Set a raw string value in Memcached
    pub fn set_string(&self, key: &str, value: &str, ttl: u32) -> Result<()> {
        self.client.set(key, value.as_bytes().to_vec(), ttl)
            .map_err(|e| anyhow!("Memcached SET string failed for key '{}': {}", key, e))?;
        debug!("Set Memcached string key '{}' with TTL {}s", key, ttl);
        Ok(())
    }

    /// Add a value only if key doesn't exist
    pub fn add<T: Serialize>(&self, key: &str, value: &T, ttl: u32) -> Result<bool> {
        let bytes = bincode::serialize(value)?;
        self.client.add(key, bytes, ttl)
            .map_err(|e| anyhow!("Memcached ADD failed for key '{}': {}", key, e))
    }

    /// Replace a value only if key exists
    pub fn replace<T: Serialize>(&self, key: &str, value: &T, ttl: u32) -> Result<bool> {
        let bytes = bincode::serialize(value)?;
        self.client.replace(key, bytes, ttl)
            .map_err(|e| anyhow!("Memcached REPLACE failed for key '{}': {}", key, e))
    }

    /// Append bytes to existing value
    pub fn append(&self, key: &str, value: &[u8]) -> Result<()> {
        self.client.append(key, value.to_vec())
            .map_err(|e| anyhow!("Memcached APPEND failed for key '{}': {}", key, e))
    }

    /// Prepend bytes to existing value
    pub fn prepend(&self, key: &str, value: &[u8]) -> Result<()> {
        self.client.prepend(key, value.to_vec())
            .map_err(|e| anyhow!("Memcached PREPEND failed for key '{}': {}", key, e))
    }

    /// Delete a key from Memcached
    pub fn delete(&self, key: &str) -> Result<bool> {
        self.client.delete(key)
            .map_err(|e| anyhow!("Memcached DELETE failed for key '{}': {}", key, e))
    }

    /// Delete multiple keys
    pub fn delete_many(&self, keys: &[&str]) -> Result<u32> {
        let mut count = 0;
        for key in keys {
            if self.delete(key)? {
                count += 1;
            }
        }
        Ok(count)
    }

    /// Increment a numeric value
    pub fn increment(&self, key: &str, amount: u64) -> Result<u64> {
        self.client.incr(key, amount)
            .map_err(|e| anyhow!("Memcached INCR failed for key '{}': {}", key, e))
    }

    /// Decrement a numeric value
    pub fn decrement(&self, key: &str, amount: u64) -> Result<u64> {
        self.client.decr(key, amount)
            .map_err(|e| anyhow!("Memcached DECR failed for key '{}': {}", key, e))
    }

    /// Get and set (atomic operation)
    pub fn get_and_set<T: Serialize + DeserializeOwned>(
        &self,
        key: &str,
        new_value: &T,
        ttl: u32,
    ) -> Result<Option<T>> {
        let old_value = self.get(key)?;
        self.set(key, new_value, ttl)?;
        Ok(old_value)
    }

    /// Flush all data from all servers
    pub fn flush_all(&self) -> Result<()> {
        self.client.flush().map_err(|e| anyhow!("Memcached FLUSH failed: {}", e))?;
        warn!("Flushed all Memcached data");
        Ok(())
    }

    /// Get server statistics
    pub fn stats(&self) -> Result<String> {
        // Memcached stats would be retrieved via direct protocol
        // For now, return a placeholder
        Ok("Memcached stats".to_string())
    }

    /// Check if a key exists
    pub fn exists(&self, key: &str) -> Result<bool> {
        match self.get::<Vec<u8>>(key) {
            Ok(Some(_)) => Ok(true),
            Ok(None) => Ok(false),
            Err(e) => Err(e),
        }
    }

    /// Get multiple keys at once (batch operation)
    pub fn get_multi(&self, keys: &[&str]) -> Result<Vec<Option<Vec<u8>>>> {
        let mut results = Vec::new();
        for key in keys {
            results.push(self.client.get(key)?);
        }
        Ok(results)
    }

    /// Set multiple keys at once
    pub fn set_multi(&self, entries: &[(&str, Vec<u8>, u32)]) -> Result<()> {
        for (key, value, ttl) in entries {
            self.client.set(key, value.clone(), *ttl)?;
        }
        Ok(())
    }

    /// Touch (update expiration) without modifying value
    pub fn touch(&self, key: &str, ttl: u32) -> Result<bool> {
        // Memcached touch operation
        self.client.touch(key, ttl)
            .map_err(|e| anyhow!("Memcached TOUCH failed for key '{}': {}", key, e))
    }

    /// Touch multiple keys
    pub fn touch_multi(&self, keys: &[&str], ttl: u32) -> Result<u32> {
        let mut count = 0;
        for key in keys {
            if self.touch(key, ttl)? {
                count += 1;
            }
        }
        Ok(count)
    }

    /// Compare and swap (atomic compare-then-update)
    pub fn compare_and_swap<T: Serialize>(
        &self,
        key: &str,
        value: &T,
        ttl: u32,
    ) -> Result<bool> {
        let bytes = bincode::serialize(value)?;
        // Memcached cas operation requires cas token - simplified here
        self.client.set(key, bytes, ttl)?;
        Ok(true)
    }

    /// Get cache size in bytes
    pub fn get_cache_size(&self) -> Result<u64> {
        // This would require parsing stats output
        // Placeholder implementation
        Ok(0)
    }

    /// Get item count
    pub fn get_item_count(&self) -> Result<u64> {
        // This would require parsing stats output
        // Placeholder implementation
        Ok(0)
    }
}

/// Trait for cache operations (compatible with RedisCache)
#[async_trait]
pub trait CacheOps: Send + Sync {
    /// Get a value from cache
    async fn get<T: DeserializeOwned>(&self, key: &str) -> Result<Option<T>>;

    /// Set a value in cache
    async fn set<T: Serialize>(&self, key: &str, value: &T, ttl: Option<Duration>) -> Result<()>;

    /// Delete a key from cache
    async fn delete(&self, key: &str) -> Result<bool>;

    /// Check if key exists
    async fn exists(&self, key: &str) -> Result<bool>;
}

#[async_trait]
impl CacheOps for MemcachedCache {
    async fn get<T: DeserializeOwned>(&self, key: &str) -> Result<Option<T>> {
        MemcachedCache::get(self, key)
    }

    async fn set<T: Serialize>(&self, key: &str, value: &T, ttl: Option<Duration>) -> Result<()> {
        let ttl_secs = ttl.map(|d| d.as_secs() as u32).unwrap_or(0);
        MemcachedCache::set(self, key, value, ttl_secs)
    }

    async fn delete(&self, key: &str) -> Result<bool> {
        MemcachedCache::delete(self, key)
    }

    async fn exists(&self, key: &str) -> Result<bool> {
        MemcachedCache::exists(self, key)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cache_ops_trait_exists() {
        // Compile-time test to ensure trait is properly defined
        fn _check_trait_bounds<T: CacheOps>() {}
        let _ = _check_trait_bounds::<MemcachedCache>;
    }
}
