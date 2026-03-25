//! Distributed failure handling for network resilience
//!
//! Provides circuit breaker, idempotency, message replay, and partition detection
//! to ensure distributed systems maintain consistency despite network failures.

pub mod circuit_breaker;
pub mod idempotency;
pub mod message_queue;
pub mod partition_detection;
pub mod resilient_client;

pub use circuit_breaker::{
    CircuitBreaker, CircuitBreakerConfig, CircuitBreakerMetrics, CircuitBreakerState,
};
pub use idempotency::{IdempotencyKey, IdempotencyLog, IdempotencyManager};
pub use message_queue::{Message, MessageQueue, QueuedMessage};
pub use partition_detection::{HeartbeatMonitor, HeartbeatStatus, PartitionDetector};
pub use resilient_client::{
    ResilientA2AClient, ResilientClientConfig, ResilientClientError, SystemStatus,
};
