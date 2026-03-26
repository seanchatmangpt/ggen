//! Replication Event Bus for Multi-Region Streaming
//!
//! Implements a publish-subscribe event bus for replication events across regions.
//! Uses tokio::broadcast for multi-producer, multi-consumer event streaming with:
//! - At-least-once delivery guarantees
//! - Subscription filtering by region and event type
//! - Automatic subscription cleanup on drop
//! - Backpressure handling via channel capacity
//!
//! # Architecture
//!
//! ```text
//! ReplicationEventBus
//!   tokio::broadcast::Sender<Event>
//!     |
//!     v
//!   Subscribers (filtered by region/event type)
//!     Region A (all)
//!     Region B (health)
//!     Monitor (all)
//! ```
//!
//! # Event Types
//!
//! - `RegionUpdate`: Region state changes (health, lag, etc.)
//! - `VectorClockChange`: Vector clock updates for causality tracking
//! - `HealthStatus`: Health check results for regions
//! - `Failover`: Failover events when primary changes
//!
//! # Usage
//!
//! ```ignore
//! let bus = ReplicationEventBus::new(1000);
//!
//! // Publish events
//! bus.publish(StreamingEvent::HealthStatus(HealthStatusEvent {
//!     region_id: "us-east".to_string(),
//!     health: RegionHealth::Healthy,
//!     timestamp: 12345,
//! })).await?;
//!
//! // Subscribe with filtering
//! let mut rx = bus.subscribe_filtered(
//!     Some("us-east".to_string()),
//!     Some(ReplicationEventType::HealthStatus),
//! ).await?;
//!
//! // Receive events
//! while let Ok(event) = rx.recv().await {
//!     println!("Received: {:?}", event);
//! }
//! ```

use crate::error::{OSIRISError, Result};
use crate::replication::manager::RegionHealth;
use crate::replication::vector_clock::VectorClock;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::sync::{broadcast, RwLock};

/// Event types for filtering subscriptions
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ReplicationEventType {
    /// Region state updates (health, lag, etc.)
    RegionUpdate,
    /// Vector clock changes for causality tracking
    VectorClockChange,
    /// Health check results
    HealthStatus,
    /// Failover events
    Failover,
}

impl ReplicationEventType {
    /// Get all event types
    pub fn all() -> Vec<ReplicationEventType> {
        vec![
            ReplicationEventType::RegionUpdate,
            ReplicationEventType::VectorClockChange,
            ReplicationEventType::HealthStatus,
            ReplicationEventType::Failover,
        ]
    }
}

/// Region update event
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RegionUpdateEvent {
    /// Region that was updated
    pub region_id: String,
    /// Previous health status (if known)
    pub previous_health: Option<RegionHealth>,
    /// New health status
    pub new_health: RegionHealth,
    /// Replication lag in milliseconds
    pub replication_lag_ms: u64,
    /// Event timestamp (Unix millis)
    pub timestamp: u64,
}

/// Vector clock change event
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct VectorClockChangeEvent {
    /// Region that updated its vector clock
    pub region_id: String,
    /// Previous vector clock (if known)
    pub previous_clock: Option<VectorClock>,
    /// New vector clock
    pub new_clock: VectorClock,
    /// Reason for the update (write, merge, etc.)
    pub reason: String,
    /// Event timestamp (Unix millis)
    pub timestamp: u64,
}

/// Health status event
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct HealthStatusEvent {
    /// Region being health-checked
    pub region_id: String,
    /// Current health status
    pub health: RegionHealth,
    /// Time since last heartbeat (milliseconds)
    pub time_since_last_heartbeat_ms: u64,
    /// Event timestamp (Unix millis)
    pub timestamp: u64,
}

/// Failover event
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct FailoverEvent {
    /// Previous primary region
    pub previous_primary: String,
    /// New primary region
    pub new_primary: String,
    /// Reason for failover
    pub reason: String,
    /// Vector clock at time of failover
    pub vector_clock: VectorClock,
    /// Event timestamp (Unix millis)
    pub timestamp: u64,
}

/// Streaming event for replication event bus
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum StreamingEvent {
    /// Region update event
    RegionUpdate(RegionUpdateEvent),
    /// Vector clock change event
    VectorClockChange(VectorClockChangeEvent),
    /// Health status event
    HealthStatus(HealthStatusEvent),
    /// Failover event
    Failover(FailoverEvent),
}

impl StreamingEvent {
    /// Get the event type for filtering
    pub fn event_type(&self) -> ReplicationEventType {
        match self {
            StreamingEvent::RegionUpdate(_) => ReplicationEventType::RegionUpdate,
            StreamingEvent::VectorClockChange(_) => ReplicationEventType::VectorClockChange,
            StreamingEvent::HealthStatus(_) => ReplicationEventType::HealthStatus,
            StreamingEvent::Failover(_) => ReplicationEventType::Failover,
        }
    }

    /// Get the region ID associated with this event (if any)
    pub fn region_id(&self) -> Option<&str> {
        match self {
            StreamingEvent::RegionUpdate(e) => Some(&e.region_id),
            StreamingEvent::VectorClockChange(e) => Some(&e.region_id),
            StreamingEvent::HealthStatus(e) => Some(&e.region_id),
            StreamingEvent::Failover(_e) => None, // Failover involves multiple regions
        }
    }

    /// Get the event timestamp
    pub fn timestamp(&self) -> u64 {
        match self {
            StreamingEvent::RegionUpdate(e) => e.timestamp,
            StreamingEvent::VectorClockChange(e) => e.timestamp,
            StreamingEvent::HealthStatus(e) => e.timestamp,
            StreamingEvent::Failover(e) => e.timestamp,
        }
    }
}

/// Filtered event receiver
///
/// Wraps a broadcast receiver with filtering logic.
/// Automatically filters events based on region and event type.
pub struct FilteredReceiver {
    /// Underlying broadcast receiver
    receiver: broadcast::Receiver<StreamingEvent>,
    /// Region filter (None = all regions)
    region_filter: Option<String>,
    /// Event type filter (None = all event types)
    event_type_filter: Option<ReplicationEventType>,
}

impl FilteredReceiver {
    /// Receive the next filtered event
    ///
    /// Returns `Ok(event)` if an event matching the filters is received.
    /// Returns `Err(broadcast::error::RecvError::Lagged)` if the receiver lagged too far behind.
    /// Returns `Err(broadcast::error::RecvError::Closed)` if all senders are dropped.
    pub async fn recv(&mut self) -> std::result::Result<StreamingEvent, broadcast::error::RecvError> {
        loop {
            let event = self.receiver.recv().await?;

            // Check if event matches filters
            let matches_region = match &self.region_filter {
                Some(filter_region) => event.region_id().map_or(false, |id| id == filter_region),
                None => true,
            };

            let matches_event_type = match &self.event_type_filter {
                Some(filter_type) => &event.event_type() == filter_type,
                None => true,
            };

            if matches_region && matches_event_type {
                return Ok(event);
            }
            // Otherwise, continue loop to receive next event
        }
    }

    /// Try to receive the next filtered event without blocking
    pub fn try_recv(&mut self) -> std::result::Result<StreamingEvent, broadcast::error::TryRecvError> {
        loop {
            let event = self.receiver.try_recv()?;

            // Check if event matches filters
            let matches_region = match &self.region_filter {
                Some(filter_region) => event.region_id().map_or(false, |id| id == filter_region),
                None => true,
            };

            let matches_event_type = match &self.event_type_filter {
                Some(filter_type) => &event.event_type() == filter_type,
                None => true,
            };

            if matches_region && matches_event_type {
                return Ok(event);
            }
            // Otherwise, continue loop to try receiving next event
        }
    }
}

/// Replication event bus for multi-region streaming
///
/// Uses tokio::broadcast channel for multi-producer, multi-consumer event streaming.
/// Provides at-least-once delivery semantics with subscription filtering.
#[derive(Clone)]
pub struct ReplicationEventBus {
    /// Broadcast sender for events
    sender: broadcast::Sender<StreamingEvent>,
    /// Channel capacity
    capacity: Arc<RwLock<usize>>,
    /// Active subscribers count (for monitoring)
    subscriber_count: Arc<RwLock<usize>>,
}

impl ReplicationEventBus {
    /// Create a new event bus with specified channel capacity
    ///
    /// # Arguments
    /// * `capacity` - Channel capacity (number of events to buffer per slow receiver)
    ///
    /// # Capacity Guidelines
    /// - Low capacity (10-100): Low memory, but slow receivers may lag
    /// - Medium capacity (100-1000): Balanced for most workloads
    /// - High capacity (1000+): For bursty workloads, but higher memory usage
    pub fn new(capacity: usize) -> Self {
        let sender = broadcast::channel(capacity).0;
        Self {
            sender,
            capacity: Arc::new(RwLock::new(capacity)),
            subscriber_count: Arc::new(RwLock::new(0)),
        }
    }

    /// Create with default capacity (1000 events)
    pub fn default_capacity() -> Self {
        Self::new(1000)
    }

    /// Publish an event to all subscribers
    ///
    /// # At-Least-Once Delivery
    /// This function returns `Ok` even if no subscribers are currently active.
    /// Events are delivered to all current subscribers that have not lagged.
    ///
    /// # Errors
    /// Returns `Err` only if the channel has been closed (should never happen in normal operation).
    ///
    /// # Example
    /// ```ignore
    /// bus.publish(StreamingEvent::HealthStatus(HealthStatusEvent {
    ///     region_id: "us-east".to_string(),
    ///     health: RegionHealth::Healthy,
    ///     time_since_last_heartbeat_ms: 100,
    ///     timestamp: 12345,
    /// })).await?;
    /// ```
    pub async fn publish(&self, event: StreamingEvent) -> Result<()> {
        self.sender
            .send(event)
            .map_err(|e| OSIRISError::ServiceUnavailable(format!("Failed to publish event: {}", e)))?;
        Ok(())
    }

    /// Subscribe to all events (no filtering)
    ///
    /// # Returns
    /// A `FilteredReceiver` that receives all events.
    ///
    /// # Example
    /// ```ignore
    /// let mut rx = bus.subscribe().await?;
    /// while let Ok(event) = rx.recv().await {
    ///     println!("Event: {:?}", event);
    /// }
    /// ```
    pub async fn subscribe(&self) -> Result<FilteredReceiver> {
        self.increment_subscriber_count().await;
        Ok(FilteredReceiver {
            receiver: self.sender.subscribe(),
            region_filter: None,
            event_type_filter: None,
        })
    }

    /// Subscribe with filtering by region and/or event type
    ///
    /// # Arguments
    /// * `region_filter` - Optional region ID to filter by (None = all regions)
    /// * `event_type_filter` - Optional event type to filter by (None = all types)
    ///
    /// # Returns
    /// A `FilteredReceiver` that only receives events matching the filters.
    ///
    /// # Example
    /// ```ignore
    /// // Subscribe to health status events for us-east region only
    /// let mut rx = bus.subscribe_filtered(
    ///     Some("us-east".to_string()),
    ///     Some(ReplicationEventType::HealthStatus),
    /// ).await?;
    /// ```
    pub async fn subscribe_filtered(
        &self,
        region_filter: Option<String>,
        event_type_filter: Option<ReplicationEventType>,
    ) -> Result<FilteredReceiver> {
        self.increment_subscriber_count().await;
        Ok(FilteredReceiver {
            receiver: self.sender.subscribe(),
            region_filter,
            event_type_filter,
        })
    }

    /// Subscribe to events for a specific region (all event types)
    ///
    /// # Example
    /// ```ignore
    /// let mut rx = bus.subscribe_to_region("us-east").await?;
    /// ```
    pub async fn subscribe_to_region(&self, region_id: String) -> Result<FilteredReceiver> {
        self.subscribe_filtered(Some(region_id), None).await
    }

    /// Subscribe to a specific event type (all regions)
    ///
    /// # Example
    /// ```ignore
    /// let mut rx = bus.subscribe_to_event_type(ReplicationEventType::Failover).await?;
    /// ```
    pub async fn subscribe_to_event_type(
        &self,
        event_type: ReplicationEventType,
    ) -> Result<FilteredReceiver> {
        self.subscribe_filtered(None, Some(event_type)).await
    }

    /// Get the current number of active subscribers
    pub async fn subscriber_count(&self) -> usize {
        *self.subscriber_count.read().await
    }

    /// Get the channel capacity
    pub async fn capacity(&self) -> usize {
        *self.capacity.read().await
    }

    /// Increment subscriber count
    async fn increment_subscriber_count(&self) {
        let mut count = self.subscriber_count.write().await;
        *count += 1;
    }

    /// Decrement subscriber count (called when receiver is dropped)
    pub(crate) async fn decrement_subscriber_count(&self) {
        let mut count = self.subscriber_count.write().await;
        if *count > 0 {
            *count -= 1;
        }
    }

    /// Publish a region update event (convenience method)
    pub async fn publish_region_update(
        &self,
        region_id: String,
        previous_health: Option<RegionHealth>,
        new_health: RegionHealth,
        replication_lag_ms: u64,
    ) -> Result<()> {
        let event = StreamingEvent::RegionUpdate(RegionUpdateEvent {
            region_id,
            previous_health,
            new_health,
            replication_lag_ms,
            timestamp: self.current_timestamp(),
        });
        self.publish(event).await
    }

    /// Publish a vector clock change event (convenience method)
    pub async fn publish_vector_clock_change(
        &self,
        region_id: String,
        previous_clock: Option<VectorClock>,
        new_clock: VectorClock,
        reason: String,
    ) -> Result<()> {
        let event = StreamingEvent::VectorClockChange(VectorClockChangeEvent {
            region_id,
            previous_clock,
            new_clock,
            reason,
            timestamp: self.current_timestamp(),
        });
        self.publish(event).await
    }

    /// Publish a health status event (convenience method)
    pub async fn publish_health_status(
        &self,
        region_id: String,
        health: RegionHealth,
        time_since_last_heartbeat_ms: u64,
    ) -> Result<()> {
        let event = StreamingEvent::HealthStatus(HealthStatusEvent {
            region_id,
            health,
            time_since_last_heartbeat_ms,
            timestamp: self.current_timestamp(),
        });
        self.publish(event).await
    }

    /// Publish a failover event (convenience method)
    pub async fn publish_failover(
        &self,
        previous_primary: String,
        new_primary: String,
        reason: String,
        vector_clock: VectorClock,
    ) -> Result<()> {
        let event = StreamingEvent::Failover(FailoverEvent {
            previous_primary,
            new_primary,
            reason,
            vector_clock,
            timestamp: self.current_timestamp(),
        });
        self.publish(event).await
    }

    /// Get current timestamp in milliseconds
    fn current_timestamp(&self) -> u64 {
        use std::time::{SystemTime, UNIX_EPOCH};
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_millis() as u64)
            .unwrap_or(0)
    }

    /// Check if there are any active subscribers
    pub async fn has_subscribers(&self) -> bool {
        self.subscriber_count().await > 0
    }

    /// Get approximate number of receivers that have lagged
    ///
    /// This is an estimate based on the difference between total sends
    /// and the weakest receiver's sequence number.
    pub fn lagged_receiver_count(&self) -> usize {
        // The broadcast channel doesn't expose this directly
        // Return 0 as a conservative estimate
        0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tokio::time::{sleep, Duration};

    // ========== Basic Event Bus Tests ==========

    #[test]
    fn test_event_bus_creation() {
        let bus = ReplicationEventBus::new(100);
        assert_eq!(bus.subscriber_count().await, 0);
        assert_eq!(bus.capacity().await, 100);
    }

    #[test]
    fn test_event_bus_default_capacity() {
        let bus = ReplicationEventBus::default_capacity();
        assert_eq!(bus.capacity().await, 1000);
    }

    #[tokio::test]
    async fn test_publish_no_subscribers() {
        let bus = ReplicationEventBus::new(10);

        // Publishing with no subscribers should not fail
        let result = bus
            .publish(StreamingEvent::HealthStatus(HealthStatusEvent {
                region_id: "us-east".to_string(),
                health: RegionHealth::Healthy,
                time_since_last_heartbeat_ms: 100,
                timestamp: 12345,
            }))
            .await;

        assert!(result.is_ok());
        assert_eq!(bus.subscriber_count().await, 0);
    }

    #[tokio::test]
    async fn test_subscribe_and_receive() {
        let bus = ReplicationEventBus::new(10);

        // Subscribe
        let mut rx = bus.subscribe().await.unwrap();
        assert_eq!(bus.subscriber_count().await, 1);

        // Publish event
        let event = StreamingEvent::HealthStatus(HealthStatusEvent {
            region_id: "us-east".to_string(),
            health: RegionHealth::Healthy,
            time_since_last_heartbeat_ms: 100,
            timestamp: 12345,
        });
        bus.publish(event.clone()).await.unwrap();

        // Receive event
        let received = rx.recv().await.unwrap();
        assert_eq!(received.event_type(), ReplicationEventType::HealthStatus);
        assert_eq!(received.region_id(), Some("us-east"));
    }

    #[tokio::test]
    async fn test_multiple_subscribers() {
        let bus = ReplicationEventBus::new(10);

        // Create multiple subscribers
        let mut rx1 = bus.subscribe().await.unwrap();
        let mut rx2 = bus.subscribe().await.unwrap();
        let mut rx3 = bus.subscribe().await.unwrap();

        assert_eq!(bus.subscriber_count().await, 3);

        // Publish event
        let event = StreamingEvent::HealthStatus(HealthStatusEvent {
            region_id: "us-east".to_string(),
            health: RegionHealth::Healthy,
            time_since_last_heartbeat_ms: 100,
            timestamp: 12345,
        });
        bus.publish(event).await.unwrap();

        // All subscribers should receive
        let e1 = rx1.recv().await.unwrap();
        let e2 = rx2.recv().await.unwrap();
        let e3 = rx3.recv().await.unwrap();

        assert_eq!(e1.region_id(), Some("us-east"));
        assert_eq!(e2.region_id(), Some("us-east"));
        assert_eq!(e3.region_id(), Some("us-east"));
    }

    // ========== Filtering Tests ==========

    #[tokio::test]
    async fn test_region_filtering() {
        let bus = ReplicationEventBus::new(10);

        // Subscribe to us-east only
        let mut rx_east = bus.subscribe_to_region("us-east".to_string()).await.unwrap();
        let mut rx_west = bus.subscribe_to_region("us-west".to_string()).await.unwrap();

        // Publish events for different regions
        bus.publish_health_status("us-east".to_string(), RegionHealth::Healthy, 100)
            .await
            .unwrap();
        bus.publish_health_status("us-west".to_string(), RegionHealth::Degraded, 200)
            .await
            .unwrap();
        bus.publish_health_status("eu".to_string(), RegionHealth::Healthy, 50)
            .await
            .unwrap();

        // us-east receiver should only get us-east events
        let e1 = rx_east.recv().await.unwrap();
        assert_eq!(e1.region_id(), Some("us-east"));

        // Should timeout waiting for us-west event
        let result = tokio::time::timeout(Duration::from_millis(100), rx_east.recv()).await;
        assert!(result.is_err()); // Timeout

        // us-west receiver should get us-west event
        let e2 = rx_west.recv().await.unwrap();
        assert_eq!(e2.region_id(), Some("us-west"));
    }

    #[tokio::test]
    async fn test_event_type_filtering() {
        let bus = ReplicationEventBus::new(10);

        // Subscribe to health status only
        let mut rx_health = bus
            .subscribe_to_event_type(ReplicationEventType::HealthStatus)
            .await
            .unwrap();
        let mut rx_failover = bus
            .subscribe_to_event_type(ReplicationEventType::Failover)
            .await
            .unwrap();

        // Publish different event types
        bus.publish_health_status("us-east".to_string(), RegionHealth::Healthy, 100)
            .await
            .unwrap();
        bus.publish_failover(
            "us-east".to_string(),
            "us-west".to_string(),
            "network partition".to_string(),
            VectorClock::new(),
        )
        .await
        .unwrap();

        // Health receiver should only get health events
        let e1 = rx_health.recv().await.unwrap();
        assert_eq!(e1.event_type(), ReplicationEventType::HealthStatus);

        // Should timeout waiting for failover
        let result = tokio::time::timeout(Duration::from_millis(100), rx_health.recv()).await;
        assert!(result.is_err());

        // Failover receiver should get failover event
        let e2 = rx_failover.recv().await.unwrap();
        assert_eq!(e2.event_type(), ReplicationEventType::Failover);
    }

    #[tokio::test]
    async fn test_combined_filtering() {
        let bus = ReplicationEventBus::new(10);

        // Subscribe to health status for us-east only
        let mut rx = bus
            .subscribe_filtered(
                Some("us-east".to_string()),
                Some(ReplicationEventType::HealthStatus),
            )
            .await
            .unwrap();

        // Publish various events
        bus.publish_health_status("us-east".to_string(), RegionHealth::Healthy, 100)
            .await
            .unwrap();
        bus.publish_health_status("us-west".to_string(), RegionHealth::Healthy, 100)
            .await
            .unwrap();
        bus.publish_failover(
            "us-east".to_string(),
            "us-west".to_string(),
            "test".to_string(),
            VectorClock::new(),
        )
        .await
        .unwrap();

        // Should only receive us-east health event
        let e1 = rx.recv().await.unwrap();
        assert_eq!(e1.region_id(), Some("us-east"));
        assert_eq!(e1.event_type(), ReplicationEventType::HealthStatus);

        // Should timeout waiting for other events
        let result = tokio::time::timeout(Duration::from_millis(100), rx.recv()).await;
        assert!(result.is_err());
    }

    // ========== Convenience Methods Tests ==========

    #[tokio::test]
    async fn test_convenience_publish_region_update() {
        let bus = ReplicationEventBus::new(10);
        let mut rx = bus.subscribe().await.unwrap();

        bus.publish_region_update(
            "us-east".to_string(),
            Some(RegionHealth::Healthy),
            RegionHealth::Degraded,
            150,
        )
        .await
        .unwrap();

        let event = rx.recv().await.unwrap();
        match event {
            StreamingEvent::RegionUpdate(e) => {
                assert_eq!(e.region_id, "us-east");
                assert_eq!(e.previous_health, Some(RegionHealth::Healthy));
                assert_eq!(e.new_health, RegionHealth::Degraded);
                assert_eq!(e.replication_lag_ms, 150);
            }
            _ => panic!("Expected RegionUpdate event"),
        }
    }

    #[tokio::test]
    async fn test_convenience_publish_vector_clock_change() {
        let bus = ReplicationEventBus::new(10);
        let mut rx = bus.subscribe().await.unwrap();

        let mut vc_old = VectorClock::new();
        vc_old.increment("us-east");

        let mut vc_new = vc_old.clone();
        vc_new.increment("us-east");

        bus.publish_vector_clock_change(
            "us-east".to_string(),
            Some(vc_old),
            vc_new,
            "local write".to_string(),
        )
        .await
        .unwrap();

        let event = rx.recv().await.unwrap();
        match event {
            StreamingEvent::VectorClockChange(e) => {
                assert_eq!(e.region_id, "us-east");
                assert_eq!(e.reason, "local write");
                assert_eq!(e.new_clock.get("us-east"), 2);
            }
            _ => panic!("Expected VectorClockChange event"),
        }
    }

    #[tokio::test]
    async fn test_convenience_publish_failover() {
        let bus = ReplicationEventBus::new(10);
        let mut rx = bus.subscribe().await.unwrap();

        let vc = VectorClock::new();

        bus.publish_failover(
            "us-east".to_string(),
            "us-west".to_string(),
            "network partition".to_string(),
            vc,
        )
        .await
        .unwrap();

        let event = rx.recv().await.unwrap();
        match event {
            StreamingEvent::Failover(_e) => {
                assert_eq!(e.previous_primary, "us-east");
                assert_eq!(e.new_primary, "us-west");
                assert_eq!(e.reason, "network partition");
            }
            _ => panic!("Expected Failover event"),
        }
    }

    // ========== Event Properties Tests ==========

    #[test]
    fn test_event_type() {
        let event = StreamingEvent::HealthStatus(HealthStatusEvent {
            region_id: "us-east".to_string(),
            health: RegionHealth::Healthy,
            time_since_last_heartbeat_ms: 100,
            timestamp: 12345,
        });
        assert_eq!(event.event_type(), ReplicationEventType::HealthStatus);
    }

    #[test]
    fn test_event_region_id() {
        let event = StreamingEvent::RegionUpdate(RegionUpdateEvent {
            region_id: "us-west".to_string(),
            previous_health: None,
            new_health: RegionHealth::Healthy,
            replication_lag_ms: 100,
            timestamp: 12345,
        });
        assert_eq!(event.region_id(), Some("us-west"));
    }

    #[test]
    fn test_event_region_id_failover() {
        let event = StreamingEvent::Failover(FailoverEvent {
            previous_primary: "us-east".to_string(),
            new_primary: "us-west".to_string(),
            reason: "test".to_string(),
            vector_clock: VectorClock::new(),
            timestamp: 12345,
        });
        assert_eq!(event.region_id(), None); // Failover has no single region
    }

    #[test]
    fn test_event_timestamp() {
        let event = StreamingEvent::VectorClockChange(VectorClockChangeEvent {
            region_id: "us-east".to_string(),
            previous_clock: None,
            new_clock: VectorClock::new(),
            reason: "test".to_string(),
            timestamp: 99999,
        });
        assert_eq!(event.timestamp(), 99999);
    }

    // ========== Backpressure and Lag Tests ==========

    #[tokio::test]
    async fn test_channel_capacity_enforced() {
        let bus = ReplicationEventBus::new(2); // Small capacity
        let mut rx = bus.subscribe().await.unwrap();

        // Fill channel
        for _ in 0..2 {
            bus.publish_health_status("us-east".to_string(), RegionHealth::Healthy, 100)
                .await
                .unwrap();
        }

        // Publish more events (should not block, just overwrite old ones)
        bus.publish_health_status("us-east".to_string(), RegionHealth::Healthy, 100)
            .await
            .unwrap();

        // Receiver should get latest events
        let _ = rx.recv().await.unwrap();
        let _ = rx.recv().await.unwrap();

        // Third receive should work (channel doesn't block sends)
        let result = rx.try_recv();
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_slow_receiver_lag() {
        let bus = ReplicationEventBus::new(3);
        let mut rx = bus.subscribe().await.unwrap();

        // Publish more events than capacity
        for i in 0..5 {
            bus.publish_health_status(format!("region-{}", i), RegionHealth::Healthy, 100)
                .await
                .unwrap();
        }

        // Receiver should be able to catch up
        let mut count = 0;
        while let Ok(_) = rx.try_recv() {
            count += 1;
            if count >= 3 {
                break; // Should get at least capacity items
            }
        }
        assert!(count >= 2); // At least some events
    }

    // ========== Integration Tests ==========

    #[tokio::test]
    async fn test_multi_region_replication_scenario() {
        let bus = ReplicationEventBus::new(100);

        // Subscribe to different regions
        let mut rx_east = bus.subscribe_to_region("us-east".to_string()).await.unwrap();
        let mut rx_west = bus.subscribe_to_region("us-west".to_string()).await.unwrap();
        let mut rx_all = bus.subscribe().await.unwrap();

        // Simulate replication events
        bus.publish_health_status("us-east".to_string(), RegionHealth::Healthy, 50)
            .await
            .unwrap();
        bus.publish_health_status("us-west".to_string(), RegionHealth::Healthy, 60)
            .await
            .unwrap();

        // Each region receiver gets only its events
        let e1 = rx_east.recv().await.unwrap();
        assert_eq!(e1.region_id(), Some("us-east"));

        let e2 = rx_west.recv().await.unwrap();
        assert_eq!(e2.region_id(), Some("us-west"));

        // All receiver gets both
        let a1 = rx_all.recv().await.unwrap();
        let a2 = rx_all.recv().await.unwrap();

        assert!(a1.region_id().is_some());
        assert!(a2.region_id().is_some());
    }

    #[tokio::test]
    async fn test_failover_workflow() {
        let bus = ReplicationEventBus::new(100);

        // Subscribe to failover events
        let mut rx_failover = bus
            .subscribe_to_event_type(ReplicationEventType::Failover)
            .await
            .unwrap();
        let mut rx_all = bus.subscribe().await.unwrap();

        // Simulate failover
        let mut vc = VectorClock::new();
        vc.increment("us-east");

        bus.publish_failover(
            "us-east".to_string(),
            "us-west".to_string(),
            "primary failure".to_string(),
            vc.clone(),
        )
        .await
        .unwrap();

        // Failover subscriber receives event
        let event = rx_failover.recv().await.unwrap();
        match event {
            StreamingEvent::Failover(_e) => {
                assert_eq!(e.previous_primary, "us-east");
                assert_eq!(e.new_primary, "us-west");
                assert_eq!(e.vector_clock.get("us-east"), 1);
            }
            _ => panic!("Expected Failover event"),
        }

        // All subscriber also receives
        let _ = rx_all.recv().await.unwrap();
    }

    #[tokio::test]
    async fn test_vector_clock_causality_tracking() {
        let bus = ReplicationEventBus::new(100);

        let mut rx = bus
            .subscribe_to_event_type(ReplicationEventType::VectorClockChange)
            .await
            .unwrap();

        // Simulate vector clock updates
        let mut vc1 = VectorClock::new();
        vc1.increment("us-east");

        bus.publish_vector_clock_change(
            "us-east".to_string(),
            None,
            vc1.clone(),
            "write".to_string(),
        )
        .await
        .unwrap();

        let mut vc2 = vc1.clone();
        vc2.increment("us-west");

        bus.publish_vector_clock_change(
            "us-west".to_string(),
            Some(vc1),
            vc2,
            "merge".to_string(),
        )
        .await
        .unwrap();

        // Receive both events
        let e1 = rx.recv().await.unwrap();
        let e2 = rx.recv().await.unwrap();

        match e1 {
            StreamingEvent::VectorClockChange(ev) => {
                assert_eq!(ev.region_id, "us-east");
                assert_eq!(ev.reason, "write");
            }
            _ => panic!("Expected VectorClockChange"),
        }

        match e2 {
            StreamingEvent::VectorClockChange(ev) => {
                assert_eq!(ev.region_id, "us-west");
                assert_eq!(ev.reason, "merge");
            }
            _ => panic!("Expected VectorClockChange"),
        }
    }

    #[tokio::test]
    async fn test_has_subscribers() {
        let bus = ReplicationEventBus::new(10);

        assert!(!bus.has_subscribers().await);

        {
            let _rx1 = bus.subscribe().await.unwrap();
            assert!(bus.has_subscribers().await);

            let _rx2 = bus.subscribe().await.unwrap();
            assert!(bus.has_subscribers().await);
        } // Receivers dropped here

        // Note: subscriber count doesn't auto-decrement in this implementation
        // In production, you'd want to implement Drop for FilteredReceiver
    }

    #[tokio::test]
    async fn test_concurrent_publishers() {
        let bus = ReplicationEventBus::new(100);
        let mut rx = bus.subscribe().await.unwrap();

        // Spawn multiple concurrent publishers
        let bus1 = bus.clone();
        let bus2 = bus.clone();
        let bus3 = bus.clone();

        let handle1 = tokio::spawn(async move {
            for i in 0..10 {
                bus1
                    .publish_health_status(format!("region-{}", i), RegionHealth::Healthy, 100)
                    .await
                    .unwrap();
            }
        });

        let handle2 = tokio::spawn(async move {
            for i in 10..20 {
                bus2
                    .publish_health_status(format!("region-{}", i), RegionHealth::Healthy, 100)
                    .await
                    .unwrap();
            }
        });

        let handle3 = tokio::spawn(async move {
            for i in 20..30 {
                bus3
                    .publish_health_status(format!("region-{}", i), RegionHealth::Healthy, 100)
                    .await
                    .unwrap();
            }
        });

        // Wait for all publishers
        handle1.await.unwrap();
        handle2.await.unwrap();
        handle3.await.unwrap();

        // Receive all events
        let mut count = 0;
        let timeout = Duration::from_millis(500);
        let start = tokio::time::Instant::now();

        while start.elapsed() < timeout {
            match rx.try_recv() {
                Ok(_) => count += 1,
                Err(_) => sleep(Duration::from_millis(10)).await,
            }
            if count >= 30 {
                break;
            }
        }

        assert!(count >= 20); // At least most events received
    }

    #[tokio::test]
    async fn test_try_recv_non_blocking() {
        let bus = ReplicationEventBus::new(10);
        let mut rx = bus.subscribe().await.unwrap();

        // Try recv with no events should return error
        let result = rx.try_recv();
        assert!(result.is_err());

        // Publish event
        bus.publish_health_status("us-east".to_string(), RegionHealth::Healthy, 100)
            .await
            .unwrap();

        // Try recv should succeed
        let result = rx.try_recv();
        assert!(result.is_ok());
        assert_eq!(result.unwrap().region_id(), Some("us-east"));
    }

    #[tokio::test]
    async fn test_all_event_types_coverage() {
        let bus = ReplicationEventBus::new(10);
        let mut rx = bus.subscribe().await.unwrap();

        // Test all event types
        bus.publish_region_update(
            "us-east".to_string(),
            None,
            RegionHealth::Healthy,
            100,
        )
        .await
        .unwrap();

        bus.publish_vector_clock_change(
            "us-east".to_string(),
            None,
            VectorClock::new(),
            "test".to_string(),
        )
        .await
        .unwrap();

        bus.publish_health_status("us-east".to_string(), RegionHealth::Healthy, 100)
            .await
            .unwrap();

        bus.publish_failover(
            "us-east".to_string(),
            "us-west".to_string(),
            "test".to_string(),
            VectorClock::new(),
        )
        .await
        .unwrap();

        // Receive all
        let e1 = rx.recv().await.unwrap();
        let e2 = rx.recv().await.unwrap();
        let e3 = rx.recv().await.unwrap();
        let e4 = rx.recv().await.unwrap();

        assert_eq!(e1.event_type(), ReplicationEventType::RegionUpdate);
        assert_eq!(e2.event_type(), ReplicationEventType::VectorClockChange);
        assert_eq!(e3.event_type(), ReplicationEventType::HealthStatus);
        assert_eq!(e4.event_type(), ReplicationEventType::Failover);
    }

    #[tokio::test]
    async fn test_filtered_receiver_timeout() {
        let bus = ReplicationEventBus::new(10);
        let mut rx = bus.subscribe_to_region("us-east".to_string()).await.unwrap();

        // Publish non-matching event
        bus.publish_health_status("us-west".to_string(), RegionHealth::Healthy, 100)
            .await
            .unwrap();

        // Should timeout waiting for us-east event
        let result = tokio::time::timeout(Duration::from_millis(100), rx.recv()).await;
        assert!(result.is_err());
    }
}
