//! Idempotency handling for distributed requests
//!
//! Ensures requests are processed exactly once, even if retried.
//! Uses UUID-based idempotency keys and event log storage.

use chrono::{DateTime, Duration, Utc};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use uuid::Uuid;

/// Uniquely identifies a request for idempotency
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct IdempotencyKey(Uuid);

impl IdempotencyKey {
    /// Generate a new random idempotency key
    pub fn generate() -> Self {
        Self(Uuid::new_v4())
    }

    /// Create from an existing UUID
    pub fn from_uuid(uuid: Uuid) -> Self {
        Self(uuid)
    }

    /// Get the inner UUID
    pub fn as_uuid(&self) -> Uuid {
        self.0
    }
}

impl std::fmt::Display for IdempotencyKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<Uuid> for IdempotencyKey {
    fn from(uuid: Uuid) -> Self {
        Self(uuid)
    }
}

/// Result of a processed request
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IdempotencyRecord {
    /// Unique key for this request
    pub key: IdempotencyKey,
    /// When the request was first processed
    pub processed_at: DateTime<Utc>,
    /// The result returned to the client
    pub result: Value,
    /// Request metadata for audit trail
    pub metadata: HashMap<String, Value>,
}

/// Event log for tracking request processing
#[derive(Debug, Clone)]
pub struct IdempotencyLog {
    records: Arc<RwLock<HashMap<IdempotencyKey, IdempotencyRecord>>>,
    retention_secs: u64,
}

impl IdempotencyLog {
    /// Create a new idempotency log with retention period
    pub fn new(retention_secs: u64) -> Self {
        Self {
            records: Arc::new(RwLock::new(HashMap::new())),
            retention_secs,
        }
    }

    /// Check if a key has been processed
    pub fn contains(&self, key: IdempotencyKey) -> bool {
        let records = self
            .records
            .read()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        records.contains_key(&key)
    }

    /// Get the result of a previously processed request
    pub fn get(&self, key: IdempotencyKey) -> Option<IdempotencyRecord> {
        let records = self
            .records
            .read()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        records.get(&key).cloned()
    }

    /// Record a processed request
    pub fn record(
        &self, key: IdempotencyKey, result: Value, metadata: HashMap<String, Value>,
    ) -> Result<(), String> {
        let mut records = self
            .records
            .write()
            .unwrap_or_else(|poisoned| poisoned.into_inner());

        if records.contains_key(&key) {
            return Err("Request already processed".to_string());
        }

        records.insert(
            key,
            IdempotencyRecord {
                key,
                processed_at: Utc::now(),
                result,
                metadata,
            },
        );

        Ok(())
    }

    /// Clean up expired entries
    pub fn cleanup_expired(&self) {
        let mut records = self
            .records
            .write()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let retention_duration = Duration::seconds(self.retention_secs as i64);
        let cutoff = Utc::now() - retention_duration;

        records.retain(|_, record| record.processed_at > cutoff);
    }

    /// Get total number of recorded requests
    pub fn len(&self) -> usize {
        let records = self
            .records
            .read()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        records.len()
    }

    /// Check if log is empty
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl Default for IdempotencyLog {
    fn default() -> Self {
        Self::new(3600) // 1 hour retention
    }
}

/// Manager for idempotent request handling
#[derive(Debug, Clone)]
pub struct IdempotencyManager {
    log: IdempotencyLog,
    auto_cleanup_interval: u64,
}

impl IdempotencyManager {
    /// Create a new idempotency manager
    pub fn new(retention_secs: u64) -> Self {
        Self {
            log: IdempotencyLog::new(retention_secs),
            auto_cleanup_interval: retention_secs / 10,
        }
    }

    /// Process a request with idempotency
    pub fn process_request<F>(&self, key: IdempotencyKey, f: F) -> Result<Value, String>
    where
        F: FnOnce() -> Result<Value, String>,
    {
        // Check if already processed
        if let Some(record) = self.log.get(key) {
            return Ok(record.result);
        }

        // Process the request
        let result = f()?;

        // Record the result
        self.log.record(key, result.clone(), HashMap::new())?;

        Ok(result)
    }

    /// Get the underlying log
    pub fn log(&self) -> &IdempotencyLog {
        &self.log
    }
}

impl Default for IdempotencyManager {
    fn default() -> Self {
        Self::new(3600)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_idempotency_key_generation() {
        let key1 = IdempotencyKey::generate();
        let key2 = IdempotencyKey::generate();
        assert_ne!(key1, key2);
    }

    #[test]
    fn test_idempotency_log_recording() {
        let log = IdempotencyLog::new(3600);
        let key = IdempotencyKey::generate();
        let result = Value::String("success".to_string());

        assert!(!log.contains(key));

        log.record(key, result.clone(), HashMap::new()).unwrap();

        assert!(log.contains(key));
        let record = log.get(key).unwrap();
        assert_eq!(record.result, result);
    }

    #[test]
    fn test_idempotency_log_duplicate_handling() {
        let log = IdempotencyLog::new(3600);
        let key = IdempotencyKey::generate();
        let result = Value::String("success".to_string());

        log.record(key, result.clone(), HashMap::new()).unwrap();

        // Attempting to record the same key again should fail
        let result2 = log.record(key, Value::String("different".to_string()), HashMap::new());
        assert!(result2.is_err());
    }

    #[test]
    fn test_idempotency_manager_deduplication() {
        let manager = IdempotencyManager::new(3600);
        let key = IdempotencyKey::generate();

        let mut call_count = 0;
        let result1 = manager.process_request(key, || {
            call_count += 1;
            Ok(Value::String(format!("call_{}", call_count)))
        });

        let result2 = manager.process_request(key, || {
            call_count += 1;
            Ok(Value::String(format!("call_{}", call_count)))
        });

        // Both should return the same result
        assert_eq!(result1.unwrap(), result2.unwrap());
        // Function should only be called once
        assert_eq!(call_count, 1);
    }

    #[test]
    fn test_idempotency_log_cleanup() {
        let log = IdempotencyLog::new(1);
        let key = IdempotencyKey::generate();

        log.record(key, Value::String("test".to_string()), HashMap::new())
            .unwrap();
        assert_eq!(log.len(), 1);

        // Wait for record to expire
        std::thread::sleep(std::time::Duration::from_secs(2));
        log.cleanup_expired();

        assert_eq!(log.len(), 0);
    }
}
