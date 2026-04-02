//! Redis cache implementation with async operations, connection pooling, and data structure operations.
//!
//! This module provides a high-level abstraction over Redis operations with:
//! - Automatic connection pooling for efficient resource usage
//! - Async/await support via Tokio integration
//! - Support for all major Redis data structures (String, List, Hash, Set, Sorted Set)
//! - Expiration policies and TTL management
//! - Type-safe serialization/deserialization

use anyhow::{anyhow, Result};
use async_trait::async_trait;
use redis::aio::ConnectionManager;
use redis::AsyncCommands;
use serde::{de::DeserializeOwned, Serialize};
use std::sync::Arc;
use std::time::Duration;
use tracing::{debug, error, warn};

/// Redis cache client with connection pooling
#[derive(Clone)]
pub struct RedisCache {
    client: Arc<ConnectionManager>,
}

impl RedisCache {
    /// Create a new Redis cache client
    ///
    /// # Arguments
    /// * `redis_url` - Connection string (e.g., "redis://127.0.0.1:6379")
    ///
    /// # Errors
    /// Returns `anyhow::Error` if connection fails
    pub async fn new(redis_url: &str) -> Result<Self> {
        let client = redis::Client::open(redis_url)
            .map_err(|e| anyhow!("Failed to create Redis client: {}", e))?;

        let manager = ConnectionManager::new(client)
            .await
            .map_err(|e| anyhow!("Failed to create connection manager: {}", e))?;

        debug!("Redis cache initialized at {}", redis_url);
        Ok(Self {
            client: Arc::new(manager),
        })
    }

    /// Get a value from cache
    ///
    /// # Arguments
    /// * `key` - Cache key
    ///
    /// # Returns
    /// `Some(value)` if key exists, `None` if key doesn't exist
    pub async fn get<T: DeserializeOwned>(&self, key: &str) -> Result<Option<T>> {
        let bytes: Option<Vec<u8>> = self.client.get(key).await
            .map_err(|e| anyhow!("Redis GET failed for key '{}': {}", key, e))?;

        match bytes {
            Some(data) => {
                let value = bincode::deserialize(&data)
                    .map_err(|e| anyhow!("Failed to deserialize value for key '{}': {}", key, e))?;
                debug!("Cache hit for key: {}", key);
                Ok(Some(value))
            }
            None => {
                debug!("Cache miss for key: {}", key);
                Ok(None)
            }
        }
    }

    /// Get a raw string value from cache
    pub async fn get_string(&self, key: &str) -> Result<Option<String>> {
        self.client.get(key)
            .await
            .map_err(|e| anyhow!("Redis GET string failed for key '{}': {}", key, e))
    }

    /// Set a value in cache with optional expiration
    ///
    /// # Arguments
    /// * `key` - Cache key
    /// * `value` - Value to cache
    /// * `ttl` - Optional time-to-live duration
    pub async fn set<T: Serialize>(&self, key: &str, value: &T, ttl: Option<Duration>) -> Result<()> {
        let bytes = bincode::serialize(value)
            .map_err(|e| anyhow!("Failed to serialize value for key '{}': {}", key, e))?;

        match ttl {
            Some(duration) => {
                self.client.set_ex(key, &bytes, duration.as_secs() as usize)
                    .await
                    .map_err(|e| anyhow!("Redis SET with TTL failed for key '{}': {}", key, e))?;
                debug!("Set cache key '{}' with TTL {:?}", key, duration);
            }
            None => {
                self.client.set(key, &bytes)
                    .await
                    .map_err(|e| anyhow!("Redis SET failed for key '{}': {}", key, e))?;
                debug!("Set cache key '{}' without TTL", key);
            }
        }
        Ok(())
    }

    /// Set a raw string value in cache
    pub async fn set_string(&self, key: &str, value: &str, ttl: Option<Duration>) -> Result<()> {
        match ttl {
            Some(duration) => {
                self.client.set_ex(key, value, duration.as_secs() as usize)
                    .await
                    .map_err(|e| anyhow!("Redis SET string with TTL failed for key '{}': {}", key, e))?;
            }
            None => {
                self.client.set(key, value)
                    .await
                    .map_err(|e| anyhow!("Redis SET string failed for key '{}': {}", key, e))?;
            }
        }
        Ok(())
    }

    /// Delete a key from cache
    pub async fn delete(&self, key: &str) -> Result<bool> {
        let deleted: bool = self.client.del(key)
            .await
            .map_err(|e| anyhow!("Redis DEL failed for key '{}': {}", key, e))?;
        if deleted {
            debug!("Deleted cache key: {}", key);
        }
        Ok(deleted)
    }

    /// Delete multiple keys from cache
    pub async fn delete_many(&self, keys: &[&str]) -> Result<u32> {
        let count: u32 = self.client.del(keys)
            .await
            .map_err(|e| anyhow!("Redis DEL many failed: {}", e))?;
        debug!("Deleted {} cache keys", count);
        Ok(count)
    }

    /// Check if a key exists in cache
    pub async fn exists(&self, key: &str) -> Result<bool> {
        self.client.exists(key)
            .await
            .map_err(|e| anyhow!("Redis EXISTS failed for key '{}': {}", key, e))
    }

    /// Set expiration time for a key
    pub async fn expire(&self, key: &str, ttl: Duration) -> Result<bool> {
        self.client.expire(key, ttl.as_secs() as usize)
            .await
            .map_err(|e| anyhow!("Redis EXPIRE failed for key '{}': {}", key, e))
    }

    /// Get remaining time to live for a key in seconds (-1 if no TTL, -2 if doesn't exist)
    pub async fn ttl(&self, key: &str) -> Result<i64> {
        self.client.ttl(key)
            .await
            .map_err(|e| anyhow!("Redis TTL failed for key '{}': {}", key, e))
    }

    // List operations

    /// Push value to the head of a list
    pub async fn lpush(&self, key: &str, value: &[u8]) -> Result<usize> {
        self.client.lpush(key, value)
            .await
            .map_err(|e| anyhow!("Redis LPUSH failed for key '{}': {}", key, e))
    }

    /// Push value to the tail of a list
    pub async fn rpush(&self, key: &str, value: &[u8]) -> Result<usize> {
        self.client.rpush(key, value)
            .await
            .map_err(|e| anyhow!("Redis RPUSH failed for key '{}': {}", key, e))
    }

    /// Pop value from the head of a list
    pub async fn lpop(&self, key: &str, count: Option<u32>) -> Result<Option<Vec<Vec<u8>>>> {
        match count {
            Some(c) => {
                self.client.lpop(key, Some(c as usize))
                    .await
                    .map_err(|e| anyhow!("Redis LPOP failed for key '{}': {}", key, e))
            }
            None => {
                let value: Option<Vec<u8>> = self.client.lpop(key, None)
                    .await
                    .map_err(|e| anyhow!("Redis LPOP failed for key '{}': {}", key, e))?;
                Ok(value.map(|v| vec![v]))
            }
        }
    }

    /// Pop value from the tail of a list
    pub async fn rpop(&self, key: &str, count: Option<u32>) -> Result<Option<Vec<Vec<u8>>>> {
        match count {
            Some(c) => {
                self.client.rpop(key, Some(c as usize))
                    .await
                    .map_err(|e| anyhow!("Redis RPOP failed for key '{}': {}", key, e))
            }
            None => {
                let value: Option<Vec<u8>> = self.client.rpop(key, None)
                    .await
                    .map_err(|e| anyhow!("Redis RPOP failed for key '{}': {}", key, e))?;
                Ok(value.map(|v| vec![v]))
            }
        }
    }

    /// Get list length
    pub async fn llen(&self, key: &str) -> Result<usize> {
        self.client.llen(key)
            .await
            .map_err(|e| anyhow!("Redis LLEN failed for key '{}': {}", key, e))
    }

    /// Get range of values from list
    pub async fn lrange(&self, key: &str, start: isize, stop: isize) -> Result<Vec<Vec<u8>>> {
        self.client.lrange(key, start, stop)
            .await
            .map_err(|e| anyhow!("Redis LRANGE failed for key '{}': {}", key, e))
    }

    // Hash operations

    /// Set field in hash
    pub async fn hset(&self, key: &str, field: &str, value: &[u8]) -> Result<bool> {
        self.client.hset(key, field, value)
            .await
            .map_err(|e| anyhow!("Redis HSET failed for key '{}': {}", key, e))
    }

    /// Get field from hash
    pub async fn hget(&self, key: &str, field: &str) -> Result<Option<Vec<u8>>> {
        self.client.hget(key, field)
            .await
            .map_err(|e| anyhow!("Redis HGET failed for key '{}': {}", key, e))
    }

    /// Get all fields and values from hash
    pub async fn hgetall(&self, key: &str) -> Result<std::collections::HashMap<String, Vec<u8>>> {
        self.client.hgetall(key)
            .await
            .map_err(|e| anyhow!("Redis HGETALL failed for key '{}': {}", key, e))
    }

    /// Delete field from hash
    pub async fn hdel(&self, key: &str, field: &str) -> Result<bool> {
        self.client.hdel(key, field)
            .await
            .map_err(|e| anyhow!("Redis HDEL failed for key '{}': {}", key, e))
    }

    /// Check if field exists in hash
    pub async fn hexists(&self, key: &str, field: &str) -> Result<bool> {
        self.client.hexists(key, field)
            .await
            .map_err(|e| anyhow!("Redis HEXISTS failed for key '{}': {}", key, e))
    }

    // Set operations

    /// Add member to set
    pub async fn sadd(&self, key: &str, member: &[u8]) -> Result<bool> {
        self.client.sadd(key, member)
            .await
            .map_err(|e| anyhow!("Redis SADD failed for key '{}': {}", key, e))
    }

    /// Remove member from set
    pub async fn srem(&self, key: &str, member: &[u8]) -> Result<bool> {
        self.client.srem(key, member)
            .await
            .map_err(|e| anyhow!("Redis SREM failed for key '{}': {}", key, e))
    }

    /// Check if member exists in set
    pub async fn sismember(&self, key: &str, member: &[u8]) -> Result<bool> {
        self.client.sismember(key, member)
            .await
            .map_err(|e| anyhow!("Redis SISMEMBER failed for key '{}': {}", key, e))
    }

    /// Get all members from set
    pub async fn smembers(&self, key: &str) -> Result<Vec<Vec<u8>>> {
        self.client.smembers(key)
            .await
            .map_err(|e| anyhow!("Redis SMEMBERS failed for key '{}': {}", key, e))
    }

    /// Get set size
    pub async fn scard(&self, key: &str) -> Result<usize> {
        self.client.scard(key)
            .await
            .map_err(|e| anyhow!("Redis SCARD failed for key '{}': {}", key, e))
    }

    // Sorted set operations

    /// Add member to sorted set with score
    pub async fn zadd(&self, key: &str, score: f64, member: &[u8]) -> Result<bool> {
        self.client.zadd(key, member, score)
            .await
            .map_err(|e| anyhow!("Redis ZADD failed for key '{}': {}", key, e))
    }

    /// Remove member from sorted set
    pub async fn zrem(&self, key: &str, member: &[u8]) -> Result<bool> {
        self.client.zrem(key, member)
            .await
            .map_err(|e| anyhow!("Redis ZREM failed for key '{}': {}", key, e))
    }

    /// Get score of member in sorted set
    pub async fn zscore(&self, key: &str, member: &[u8]) -> Result<Option<f64>> {
        self.client.zscore(key, member)
            .await
            .map_err(|e| anyhow!("Redis ZSCORE failed for key '{}': {}", key, e))
    }

    /// Get range of members from sorted set by rank (low to high)
    pub async fn zrange(&self, key: &str, start: isize, stop: isize) -> Result<Vec<Vec<u8>>> {
        self.client.zrange(key, start, stop)
            .await
            .map_err(|e| anyhow!("Redis ZRANGE failed for key '{}': {}", key, e))
    }

    /// Get range of members from sorted set by rank (high to low)
    pub async fn zrevrange(&self, key: &str, start: isize, stop: isize) -> Result<Vec<Vec<u8>>> {
        self.client.zrevrange(key, start, stop)
            .await
            .map_err(|e| anyhow!("Redis ZREVRANGE failed for key '{}': {}", key, e))
    }

    /// Get sorted set size
    pub async fn zcard(&self, key: &str) -> Result<usize> {
        self.client.zcard(key)
            .await
            .map_err(|e| anyhow!("Redis ZCARD failed for key '{}': {}", key, e))
    }

    /// Increment value by specified amount
    pub async fn incr(&self, key: &str, amount: i64) -> Result<i64> {
        if amount == 1 {
            self.client.incr(key, 1)
                .await
                .map_err(|e| anyhow!("Redis INCR failed for key '{}': {}", key, e))
        } else {
            self.client.incr(key, amount)
                .await
                .map_err(|e| anyhow!("Redis INCRBY failed for key '{}': {}", key, e))
        }
    }

    /// Flush all keys from the current database
    pub async fn flush_db(&self) -> Result<()> {
        redis::cmd("FLUSHDB")
            .query_async(&mut self.client.clone())
            .await
            .map_err(|e| anyhow!("Redis FLUSHDB failed: {}", e))?;
        warn!("Flushed entire Redis database");
        Ok(())
    }

    /// Get database statistics
    pub async fn info(&self) -> Result<String> {
        redis::cmd("INFO")
            .query_async(&mut self.client.clone())
            .await
            .map_err(|e| anyhow!("Redis INFO failed: {}", e))
    }

    /// Get all keys matching pattern
    pub async fn keys(&self, pattern: &str) -> Result<Vec<String>> {
        redis::cmd("KEYS")
            .arg(pattern)
            .query_async(&mut self.client.clone())
            .await
            .map_err(|e| anyhow!("Redis KEYS failed: {}", e))
    }

    /// Get the size in bytes of all values stored under a key
    pub async fn memory_usage(&self, key: &str) -> Result<Option<u64>> {
        redis::cmd("MEMORY")
            .arg("USAGE")
            .arg(key)
            .query_async(&mut self.client.clone())
            .await
            .map_err(|e| anyhow!("Redis MEMORY USAGE failed for key '{}': {}", key, e))
    }
}

/// Trait for cache operations
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
impl CacheOps for RedisCache {
    async fn get<T: DeserializeOwned>(&self, key: &str) -> Result<Option<T>> {
        RedisCache::get(self, key).await
    }

    async fn set<T: Serialize>(&self, key: &str, value: &T, ttl: Option<Duration>) -> Result<()> {
        RedisCache::set(self, key, value, ttl).await
    }

    async fn delete(&self, key: &str) -> Result<bool> {
        RedisCache::delete(self, key).await
    }

    async fn exists(&self, key: &str) -> Result<bool> {
        RedisCache::exists(self, key).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cache_ops_trait_exists() {
        // Compile-time test to ensure trait is properly defined
        fn _check_trait_bounds<T: CacheOps>() {}
        let _ = _check_trait_bounds::<RedisCache>;
    }
}
