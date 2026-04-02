//! Idempotency handling for replication events
//!
//! Ensures replication events are processed exactly once, even if retried.
//! Uses UUID-based idempotency keys and LRU cache with TTL for expired event IDs.
//!
//! # Architecture
//!
//! ```text
//! ReplicationEventIdempotency
//!   LRU Cache (UUID → EventRecord)
//!     |
//!     v
//!   Event Bus Integration
//!     Deduplicate before publish
//!     Exactly-once processing guarantees
//! ```
//!
//! # Features
//!
//! - **UUID-based deduplication**: Each event gets unique identifier
//! - **LRU cache with TTL**: Automatic expiration of old event IDs
//! - **Event bus integration**: Seamless deduplication before publishing
//! - **Exactly-once semantics**: Guaranteed processing even with retries
//! - **Thread-safe**: Arc<RwLock> for concurrent access
//!
//! # Usage
//!
//! ```ignore
//! let idempotency = ReplicationEventIdempotency::new(1000, 3600);
//!
//! // Check if event was already processed
//! let event_id = EventId::generate();
//! if !idempotency.contains(event_id) {
//!     // Process event
//!     bus.publish(event).await?;
//!     idempotency.record(event_id, event_metadata)?;
//! }
//!
//! // Or use process_event for automatic deduplication
//! idempotency.process_event(event_id, || async {
//!     // Event processing logic
//!     bus.publish(event).await
//! }).await?;
//! ```

use crate::error::{OSIRISError, Result};
use crate::replication::event_bus::StreamingEvent;
use chrono::{DateTime, Duration, Utc};
use lru::LruCache;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::num::NonZeroUsize;
use std::sync::Arc;
use tokio::sync::RwLock;
use uuid::Uuid;

/// Uniquely identifies a replication event for idempotency
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct EventId(Uuid);

impl EventId {
    /// Generate a new random event ID
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

    /// Create from string (for parsing from external sources)
    pub fn from_str(s: &str) -> Result<Self> {
        let uuid = Uuid::parse_str(s)
            .map_err(|e| OSIRISError::ConfigurationError(format!("Invalid UUID: {}", e)))?;
        Ok(Self(uuid))
    }
}

impl std::fmt::Display for EventId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<Uuid> for EventId {
    fn from(uuid: Uuid) -> Self {
        Self(uuid)
    }
}

/// Metadata about a processed replication event
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EventRecord {
    /// Unique event ID
    pub event_id: EventId,
    /// When the event was first processed
    pub processed_at: DateTime<Utc>,
    /// Event type for categorization
    pub event_type: String,
    /// Region that generated the event
    pub region_id: Option<String>,
    /// Event metadata for audit trail
    pub metadata: HashMap<String, String>,
}

/// Idempotency cache for replication events
///
/// Uses LRU cache with TTL-based expiration to track processed events.
/// Provides exactly-once processing guarantees for replication events.
#[derive(Clone)]
pub struct ReplicationEventIdempotency {
    /// LRU cache of event records
    cache: Arc<RwLock<LruCache<EventId, EventRecord>>>,
    /// Retention period in seconds (TTL for event records)
    retention_secs: u64,
    /// Cache capacity (max number of event IDs to track)
    capacity: usize,
}

impl ReplicationEventIdempotency {
    /// Create a new idempotency cache with specified capacity and retention
    ///
    /// # Arguments
    /// * `capacity` - Maximum number of event IDs to track (LRU eviction)
    /// * `retention_secs` - Time-to-live for event records in seconds
    ///
    /// # Example
    /// ```ignore
    /// // Track up to 10,000 events for 1 hour
    /// let idempotency = ReplicationEventIdempotency::new(10000, 3600);
    /// ```
    pub fn new(capacity: usize, retention_secs: u64) -> Self {
        let non_zero_capacity = NonZeroUsize::new(capacity).unwrap_or_else(|| NonZeroUsize::new(1000).unwrap());
        Self {
            cache: Arc::new(RwLock::new(LruCache::new(non_zero_capacity))),
            retention_secs,
            capacity,
        }
    }

    /// Create with default settings (1000 capacity, 1 hour retention)
    pub fn default() -> Self {
        Self::new(1000, 3600)
    }

    /// Check if an event has been processed
    ///
    /// Returns true if the event ID exists in the cache and hasn't expired.
    pub async fn contains(&self, event_id: EventId) -> bool {
        let mut cache = self.cache.write().await;
        if let Some(record) = cache.get(&event_id) {
            // Check if record has expired
            if self.is_expired(record) {
                cache.pop(&event_id);
                return false;
            }
            true
        } else {
            false
        }
    }

    /// Get the record of a previously processed event
    ///
    /// Returns None if the event hasn't been processed or has expired.
    pub async fn get(&self, event_id: EventId) -> Option<EventRecord> {
        let mut cache = self.cache.write().await;
        if let Some(record) = cache.get(&event_id) {
            // Check if record has expired
            if self.is_expired(record) {
                cache.pop(&event_id);
                return None;
            }
            Some(record.clone())
        } else {
            None
        }
    }

    /// Record a processed event
    ///
    /// Returns an error if the event was already recorded.
    pub async fn record(
        &self,
        event_id: EventId,
        event_type: String,
        region_id: Option<String>,
        metadata: HashMap<String, String>,
    ) -> Result<()> {
        let mut cache = self.cache.write().await;

        // Check if already exists
        if cache.contains(&event_id) {
            return Err(OSIRISError::ConfigurationError(format!(
                "Event {} already processed",
                event_id
            )));
        }

        // Create event record
        let record = EventRecord {
            event_id,
            processed_at: Utc::now(),
            event_type,
            region_id,
            metadata,
        };

        // Insert into cache
        cache.put(event_id, record);

        Ok(())
    }

    /// Record a streaming event with automatic metadata extraction
    ///
    /// Convenience method that extracts event type and region from the event.
    pub async fn record_streaming_event(
        &self,
        event_id: EventId,
        event: &StreamingEvent,
        metadata: HashMap<String, String>,
    ) -> Result<()> {
        let event_type = format!("{:?}", event.event_type());
        let region_id = event.region_id().map(|s| s.to_string());

        self.record(event_id, event_type, region_id, metadata).await
    }

    /// Process an event with automatic deduplication
    ///
    /// If the event was already processed, returns the cached result without
    /// executing the processing function. Otherwise, executes the function
    /// and records the result.
    ///
    /// # Example
    /// ```ignore
    /// idempotency.process_event(event_id, || async {
    ///     // Event processing logic
    ///     bus.publish(event).await?;
    ///     Ok(())
    /// }).await?;
    /// ```
    pub async fn process_event<F, Fut, T>(
        &self,
        event_id: EventId,
        f: F,
    ) -> Result<T>
    where
        F: FnOnce() -> Fut,
        Fut: std::future::Future<Output = Result<T>>,
    {
        // Check if already processed
        if self.contains(event_id).await {
            return Err(OSIRISError::ConfigurationError(format!(
                "Event {} already processed",
                event_id
            )));
        }

        // Process the event
        let result = f().await?;

        // Record the event (minimal metadata)
        self.record(event_id, "processed".to_string(), None, HashMap::new())
            .await?;

        Ok(result)
    }

    /// Clean up expired entries
    ///
    /// Removes records that have exceeded the retention period.
    /// This is automatically called during contains/get operations,
    /// but can be called manually for proactive cleanup.
    pub async fn cleanup_expired(&self) -> usize {
        let mut cache = self.cache.write().await;
        let cutoff = Utc::now() - Duration::seconds(self.retention_secs as i64);

        let mut expired_keys = Vec::new();
        for (event_id, record) in cache.iter() {
            if record.processed_at < cutoff {
                expired_keys.push(*event_id);
            }
        }

        for key in expired_keys.into_iter() {
            cache.pop(&key);
        }

        expired_keys.len()
    }

    /// Get total number of recorded events
    pub async fn len(&self) -> usize {
        let cache = self.cache.read().await;
        cache.len()
    }

    /// Check if cache is empty
    pub async fn is_empty(&self) -> bool {
        self.len().await == 0
    }

    /// Get cache capacity
    pub fn capacity(&self) -> usize {
        self.capacity
    }

    /// Get retention period in seconds
    pub fn retention_secs(&self) -> u64 {
        self.retention_secs
    }

    /// Check if a record has expired
    fn is_expired(&self, record: &EventRecord) -> bool {
        let cutoff = Utc::now() - Duration::seconds(self.retention_secs as i64);
        record.processed_at < cutoff
    }

    /// Get cache statistics
    pub async fn stats(&self) -> IdempotencyStats {
        let cache = self.cache.read().await;
        let now = Utc::now();
        let cutoff = now - Duration::seconds(self.retention_secs as i64);

        let mut events_by_type = HashMap::new();
        let mut events_by_region = HashMap::new();
        let mut expired_count = 0;

        for record in cache.iter() {
            if record.1.processed_at < cutoff {
                expired_count += 1;
            }

            *events_by_type.entry(record.1.event_type.clone()).or_insert(0) += 1;
            if let Some(region) = &record.1.region_id {
                *events_by_region.entry(region.clone()).or_insert(0) += 1;
            }
        }

        IdempotencyStats {
            total_events: cache.len(),
            expired_count,
            capacity: self.capacity,
            events_by_type,
            events_by_region,
        }
    }
}

/// Statistics about the idempotency cache
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IdempotencyStats {
    /// Total number of events in cache
    pub total_events: usize,
    /// Number of expired events (not yet cleaned up)
    pub expired_count: usize,
    /// Cache capacity
    pub capacity: usize,
    /// Event counts by type
    pub events_by_type: HashMap<String, usize>,
    /// Event counts by region
    pub events_by_region: HashMap<String, usize>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::replication::event_bus::{HealthStatusEvent, RegionUpdateEvent};
    use crate::replication::manager::RegionHealth;
    use std::collections::HashMap;

    // ========== EventId Tests ==========

    #[test]
    fn test_event_id_generation() {
        let id1 = EventId::generate();
        let id2 = EventId::generate();
        assert_ne!(id1, id2);
    }

    #[test]
    fn test_event_id_from_uuid() {
        let uuid = Uuid::new_v4();
        let event_id = EventId::from_uuid(uuid);
        assert_eq!(event_id.as_uuid(), uuid);
    }

    #[test]
    fn test_event_id_display() {
        let uuid = Uuid::parse_str("936DA01F-9ABD-4D9D-80C7-02AF85C822A8").unwrap();
        let event_id = EventId::from_uuid(uuid);
        assert_eq!(event_id.to_string(), "936da01f-9abd-4d9d-80c7-02af85c822a8");
    }

    #[test]
    fn test_event_id_from_str_valid() {
        let event_id = EventId::from_str("936DA01F-9ABD-4D9D-80C7-02AF85C822A8").unwrap();
        assert_eq!(event_id.to_string(), "936da01f-9abd-4d9d-80c7-02af85c822a8");
    }

    #[test]
    fn test_event_id_from_str_invalid() {
        let result = EventId::from_str("invalid-uuid");
        assert!(result.is_err());
    }

    // ========== Basic Idempotency Tests ==========

    #[tokio::test]
    async fn test_idempotency_creation() {
        let idempotency = ReplicationEventIdempotency::new(100, 3600);
        assert_eq!(idempotency.capacity(), 100);
        assert_eq!(idempotency.retention_secs(), 3600);
        assert!(idempotency.is_empty().await);
    }

    #[tokio::test]
    async fn test_idempotency_default() {
        let idempotency = ReplicationEventIdempotency::default();
        assert_eq!(idempotency.capacity(), 1000);
        assert_eq!(idempotency.retention_secs(), 3600);
    }

    #[tokio::test]
    async fn test_record_and_contains() {
        let idempotency = ReplicationEventIdempotency::new(100, 3600);
        let event_id = EventId::generate();

        assert!(!idempotency.contains(event_id).await);

        idempotency
            .record(
                event_id,
                "test_event".to_string(),
                Some("us-east".to_string()),
                HashMap::new(),
            )
            .await
            .unwrap();

        assert!(idempotency.contains(event_id).await);
    }

    #[tokio::test]
    async fn test_record_duplicate_fails() {
        let idempotency = ReplicationEventIdempotency::new(100, 3600);
        let event_id = EventId::generate();

        idempotency
            .record(
                event_id,
                "test_event".to_string(),
                Some("us-east".to_string()),
                HashMap::new(),
            )
            .await
            .unwrap();

        let result = idempotency
            .record(
                event_id,
                "test_event".to_string(),
                Some("us-east".to_string()),
                HashMap::new(),
            )
            .await;

        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_get_event_record() {
        let idempotency = ReplicationEventIdempotency::new(100, 3600);
        let event_id = EventId::generate();

        idempotency
            .record(
                event_id,
                "test_event".to_string(),
                Some("us-west".to_string()),
                HashMap::new(),
            )
            .await
            .unwrap();

        let record = idempotency.get(event_id).await.unwrap();
        assert_eq!(record.event_id, event_id);
        assert_eq!(record.event_type, "test_event");
        assert_eq!(record.region_id, Some("us-west".to_string()));
    }

    #[tokio::test]
    async fn test_get_nonexistent_returns_none() {
        let idempotency = ReplicationEventIdempotency::new(100, 3600);
        let event_id = EventId::generate();

        let result = idempotency.get(event_id).await;
        assert!(result.is_none());
    }

    // ========== LRU Eviction Tests ==========

    #[tokio::test]
    async fn test_lru_eviction() {
        let idempotency = ReplicationEventIdempotency::new(3, 3600); // Small capacity

        // Fill cache
        let id1 = EventId::generate();
        let id2 = EventId::generate();
        let id3 = EventId::generate();
        let id4 = EventId::generate();

        idempotency
            .record(id1, "event1".to_string(), None, HashMap::new())
            .await
            .unwrap();
        idempotency
            .record(id2, "event2".to_string(), None, HashMap::new())
            .await
            .unwrap();
        idempotency
            .record(id3, "event3".to_string(), None, HashMap::new())
            .await
            .unwrap();

        assert_eq!(idempotency.len().await, 3);

        // Add one more - should evict id1 (least recently used)
        idempotency
            .record(id4, "event4".to_string(), None, HashMap::new())
            .await
            .unwrap();

        assert_eq!(idempotency.len().await, 3);
        assert!(!idempotency.contains(id1).await); // Evicted
        assert!(idempotency.contains(id2).await);
        assert!(idempotency.contains(id3).await);
        assert!(idempotency.contains(id4).await);
    }

    #[tokio::test]
    async fn test_lru_recency_update() {
        let idempotency = ReplicationEventIdempotency::new(3, 3600);

        let id1 = EventId::generate();
        let id2 = EventId::generate();
        let id3 = EventId::generate();

        idempotency
            .record(id1, "event1".to_string(), None, HashMap::new())
            .await
            .unwrap();
        idempotency
            .record(id2, "event2".to_string(), None, HashMap::new())
            .await
            .unwrap();
        idempotency
            .record(id3, "event3".to_string(), None, HashMap::new())
            .await
            .unwrap();

        // Access id1 to make it more recent
        idempotency.contains(id1).await;

        // Add id4 - should evict id2 (now least recently used)
        let id4 = EventId::generate();
        idempotency
            .record(id4, "event4".to_string(), None, HashMap::new())
            .await
            .unwrap();

        assert!(!idempotency.contains(id2).await); // Evicted
        assert!(idempotency.contains(id1).await); // Still present
        assert!(idempotency.contains(id3).await);
        assert!(idempotency.contains(id4).await);
    }

    // ========== TTL/Expiration Tests ==========

    #[tokio::test]
    async fn test_expiration_cleanup() {
        let idempotency = ReplicationEventIdempotency::new(100, 1); // 1 second retention

        let event_id = EventId::generate();

        idempotency
            .record(
                event_id,
                "test_event".to_string(),
                None,
                HashMap::new(),
            )
            .await
            .unwrap();

        assert!(idempotency.contains(event_id).await);

        // Wait for expiration
        tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

        // Should be expired now
        assert!(!idempotency.contains(event_id).await);
    }

    #[tokio::test]
    async fn test_cleanup_expired_count() {
        let idempotency = ReplicationEventIdempotency::new(100, 1);

        let id1 = EventId::generate();
        let id2 = EventId::generate();

        idempotency
            .record(id1, "event1".to_string(), None, HashMap::new())
            .await
            .unwrap();

        // Wait for first event to expire
        tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

        idempotency
            .record(id2, "event2".to_string(), None, HashMap::new())
            .await
            .unwrap();

        let cleaned = idempotency.cleanup_expired().await;
        assert_eq!(cleaned, 1);
        assert_eq!(idempotency.len().await, 1);
    }

    #[tokio::test]
    async fn test_get_returns_none_for_expired() {
        let idempotency = ReplicationEventIdempotency::new(100, 1);

        let event_id = EventId::generate();

        idempotency
            .record(
                event_id,
                "test_event".to_string(),
                None,
                HashMap::new(),
            )
            .await
            .unwrap();

        // Wait for expiration
        tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

        let result = idempotency.get(event_id).await;
        assert!(result.is_none());
    }

    // ========== process_event Tests ==========

    #[tokio::test]
    async fn test_process_event_success() {
        let idempotency = ReplicationEventIdempotency::new(100, 3600);
        let event_id = EventId::generate();

        let mut call_count = 0;
        let result = idempotency
            .process_event(event_id, || async {
                call_count += 1;
                Ok::<_, OSIRISError>(format!("call_{}", call_count))
            })
            .await;

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), "call_1");
        assert_eq!(call_count, 1);
        assert!(idempotency.contains(event_id).await);
    }

    #[tokio::test]
    async fn test_process_event_deduplication() {
        let idempotency = ReplicationEventIdempotency::new(100, 3600);
        let event_id = EventId::generate();

        let mut call_count = 0;
        let result1 = idempotency
            .process_event(event_id, || async {
                call_count += 1;
                Ok::<_, OSIRISError>(format!("call_{}", call_count))
            })
            .await;

        let result2 = idempotency
            .process_event(event_id, || async {
                call_count += 1;
                Ok::<_, OSIRISError>(format!("call_{}", call_count))
            })
            .await;

        assert!(result1.is_ok());
        assert!(result2.is_err()); // Already processed
        assert_eq!(call_count, 1); // Only called once
    }

    #[tokio::test]
    async fn test_process_event_propagates_errors() {
        let idempotency = ReplicationEventIdempotency::new(100, 3600);
        let event_id = EventId::generate();

        let result: Result<()> = idempotency
            .process_event(event_id, || async {
                Err(OSIRISError::ConfigurationError("test error".to_string()))
            })
            .await;

        assert!(result.is_err());
        // Event should not be recorded if processing failed
        assert!(!idempotency.contains(event_id).await);
    }

    // ========== Streaming Event Tests ==========

    #[tokio::test]
    async fn test_record_streaming_event_health_status() {
        let idempotency = ReplicationEventIdempotency::new(100, 3600);
        let event_id = EventId::generate();

        let event = StreamingEvent::HealthStatus(HealthStatusEvent {
            region_id: "us-east".to_string(),
            health: RegionHealth::Healthy,
            time_since_last_heartbeat_ms: 100,
            timestamp: 12345,
        });

        idempotency
            .record_streaming_event(event_id, &event, HashMap::new())
            .await
            .unwrap();

        let record = idempotency.get(event_id).await.unwrap();
        assert_eq!(record.event_type, "HealthStatus");
        assert_eq!(record.region_id, Some("us-east".to_string()));
    }

    #[tokio::test]
    async fn test_record_streaming_event_region_update() {
        let idempotency = ReplicationEventIdempotency::new(100, 3600);
        let event_id = EventId::generate();

        let event = StreamingEvent::RegionUpdate(RegionUpdateEvent {
            region_id: "us-west".to_string(),
            previous_health: None,
            new_health: RegionHealth::Degraded,
            replication_lag_ms: 150,
            timestamp: 12345,
        });

        idempotency
            .record_streaming_event(event_id, &event, HashMap::new())
            .await
            .unwrap();

        let record = idempotency.get(event_id).await.unwrap();
        assert_eq!(record.event_type, "RegionUpdate");
        assert_eq!(record.region_id, Some("us-west".to_string()));
    }

    // ========== Statistics Tests ==========

    #[tokio::test]
    async fn test_stats_empty() {
        let idempotency = ReplicationEventIdempotency::new(100, 3600);
        let stats = idempotency.stats().await;

        assert_eq!(stats.total_events, 0);
        assert_eq!(stats.expired_count, 0);
        assert_eq!(stats.capacity, 100);
        assert!(stats.events_by_type.is_empty());
        assert!(stats.events_by_region.is_empty());
    }

    #[tokio::test]
    async fn test_stats_with_events() {
        let idempotency = ReplicationEventIdempotency::new(100, 3600);

        idempotency
            .record(
                EventId::generate(),
                "HealthStatus".to_string(),
                Some("us-east".to_string()),
                HashMap::new(),
            )
            .await
            .unwrap();

        idempotency
            .record(
                EventId::generate(),
                "HealthStatus".to_string(),
                Some("us-west".to_string()),
                HashMap::new(),
            )
            .await
            .unwrap();

        idempotency
            .record(
                EventId::generate(),
                "VectorClockChange".to_string(),
                Some("us-east".to_string()),
                HashMap::new(),
            )
            .await
            .unwrap();

        let stats = idempotency.stats().await;

        assert_eq!(stats.total_events, 3);
        assert_eq!(stats.events_by_type.get("HealthStatus"), Some(&2));
        assert_eq!(stats.events_by_type.get("VectorClockChange"), Some(&1));
        assert_eq!(stats.events_by_region.get("us-east"), Some(&2));
        assert_eq!(stats.events_by_region.get("us-west"), Some(&1));
    }

    // ========== Concurrency Tests ==========

    #[tokio::test]
    async fn test_concurrent_record() {
        let idempotency = ReplicationEventIdempotency::new(100, 3600);

        // Spawn multiple concurrent tasks
        let mut handles = Vec::new();

        for i in 0..10 {
            let idempotency_clone = idempotency.clone();
            let handle = tokio::spawn(async move {
                let event_id = EventId::generate();
                idempotency_clone
                    .record(
                        event_id,
                        format!("event_{}", i),
                        Some(format!("region_{}", i % 3)),
                        HashMap::new(),
                    )
                    .await
            });
            handles.push(handle);
        }

        // Wait for all to complete
        for handle in handles {
            handle.await.unwrap().unwrap();
        }

        assert_eq!(idempotency.len().await, 10);
    }

    #[tokio::test]
    async fn test_concurrent_contains() {
        let idempotency = ReplicationEventIdempotency::new(100, 3600);
        let event_id = EventId::generate();

        idempotency
            .record(
                event_id,
                "test".to_string(),
                None,
                HashMap::new(),
            )
            .await
            .unwrap();

        // Spawn multiple concurrent reads
        let mut handles = Vec::new();

        for _ in 0..10 {
            let idempotency_clone = idempotency.clone();
            let handle = tokio::spawn(async move {
                idempotency_clone.contains(event_id).await
            });
            handles.push(handle);
        }

        // All should return true
        for handle in handles {
            assert!(handle.await.unwrap());
        }
    }

    // ========== Integration Tests ==========

    #[tokio::test]
    async fn test_full_workflow() {
        let idempotency = ReplicationEventIdempotency::new(100, 3600);

        // Simulate processing multiple events
        for i in 0..5 {
            let event_id = EventId::generate();

            // Process event
            let processed = idempotency
                .process_event(event_id, || async {
                    Ok::<_, OSIRISError>(format!("result_{}", i))
                })
                .await;

            assert!(processed.is_ok());

            // Try to process again (should fail)
            let duplicate = idempotency
                .process_event(event_id, || async {
                    Ok::<_, OSIRISError>("should_not_execute")
                })
                .await;

            assert!(duplicate.is_err());
        }

        assert_eq!(idempotency.len().await, 5);
    }

    #[tokio::test]
    async fn test_metadata_preservation() {
        let idempotency = ReplicationEventIdempotency::new(100, 3600);
        let event_id = EventId::generate();

        let mut metadata = HashMap::new();
        metadata.insert("key1".to_string(), "value1".to_string());
        metadata.insert("key2".to_string(), "value2".to_string());

        idempotency
            .record(
                event_id,
                "test_event".to_string(),
                Some("us-east".to_string()),
                metadata.clone(),
            )
            .await
            .unwrap();

        let record = idempotency.get(event_id).await.unwrap();
        assert_eq!(record.metadata, metadata);
    }

    #[tokio::test]
    async fn test_region_id_none() {
        let idempotency = ReplicationEventIdempotency::new(100, 3600);
        let event_id = EventId::generate();

        idempotency
            .record(
                event_id,
                "global_event".to_string(),
                None,
                HashMap::new(),
            )
            .await
            .unwrap();

        let record = idempotency.get(event_id).await.unwrap();
        assert!(record.region_id.is_none());
    }

    #[tokio::test]
    async fn test_capacity_limit_enforcement() {
        let idempotency = ReplicationEventIdempotency::new(5, 3600);

        // Add more events than capacity
        for _ in 0..10 {
            let event_id = EventId::generate();
            idempotency
                .record(event_id, "test".to_string(), None, HashMap::new())
                .await
                .unwrap();
        }

        // Should not exceed capacity
        assert_eq!(idempotency.len().await, 5);
    }

    #[tokio::test]
    async fn test_multiple_regions_tracking() {
        let idempotency = ReplicationEventIdempotency::new(100, 3600);

        let regions = vec!["us-east", "us-west", "eu"];

        for region in &regions {
            for i in 0..3 {
                let event_id = EventId::generate();
                idempotency
                    .record(
                        event_id,
                        format!("event_{}", i),
                        Some(region.to_string()),
                        HashMap::new(),
                    )
                    .await
                    .unwrap();
            }
        }

        let stats = idempotency.stats().await;
        assert_eq!(stats.total_events, 9);
        assert_eq!(stats.events_by_region.get("us-east"), Some(&3));
        assert_eq!(stats.events_by_region.get("us-west"), Some(&3));
        assert_eq!(stats.events_by_region.get("eu"), Some(&3));
    }
}
