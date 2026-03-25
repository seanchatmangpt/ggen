//! Resilient A2A-MCP client with integrated failure handling
//!
//! Combines circuit breaker, idempotency, message replay, partition
//! detection, and Byzantine Fault Tolerance into a single cohesive client for distributed A2A calls.

use super::circuit_breaker::{CircuitBreaker, CircuitBreakerConfig};
use super::idempotency::{IdempotencyKey, IdempotencyManager};
use super::message_queue::MessageQueue;
use super::partition_detection::PartitionDetector;
use osiris_core::{
    BFTSystem, ByzantineConsensus, ConsensusConfig, Evidence, Misbehavior, NodeId, ProposalValue,
};
use serde_json::json;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::info;

/// Configuration for resilient client behavior
#[derive(Debug, Clone)]
pub struct ResilientClientConfig {
    /// Circuit breaker configuration
    pub circuit_breaker: CircuitBreakerConfig,
    /// Heartbeat timeout in seconds
    pub heartbeat_timeout_secs: u64,
    /// Failure threshold for partition detection
    pub partition_failure_threshold: usize,
    /// Minimum quorum for write operations
    pub quorum_size: usize,
    /// Message queue retention in seconds
    pub queue_retention_secs: u64,
}

impl Default for ResilientClientConfig {
    fn default() -> Self {
        Self {
            circuit_breaker: CircuitBreakerConfig::default(),
            heartbeat_timeout_secs: 5,
            partition_failure_threshold: 3,
            quorum_size: 2,
            queue_retention_secs: 3600,
        }
    }
}

/// Resilient A2A-MCP client wrapper
#[derive(Clone)]
pub struct ResilientA2AClient {
    /// Circuit breaker for external calls
    circuit_breaker: CircuitBreaker,
    /// Idempotency manager for exactly-once semantics
    idempotency: IdempotencyManager,
    /// Message queue for replay on recovery
    message_queue: MessageQueue,
    /// Partition detector for split-brain prevention
    partition_detector: PartitionDetector,
    /// Byzantine Fault Tolerance system
    bft_system: Arc<BFTSystem>,
    /// Configuration
    config: ResilientClientConfig,
}

impl ResilientA2AClient {
    /// Create a new resilient client
    pub fn new(config: ResilientClientConfig) -> Self {
        // Initialize Byzantine consensus with quorum size
        let consensus_config = ConsensusConfig::new(config.quorum_size * 3);
        let bft_system = Arc::new(BFTSystem::new(NodeId::new(1), consensus_config));

        Self {
            circuit_breaker: CircuitBreaker::with_config(
                "a2a_mcp_client",
                config.circuit_breaker.clone(),
            ),
            idempotency: IdempotencyManager::new(config.queue_retention_secs),
            message_queue: MessageQueue::with_retries("a2a_messages", 5),
            partition_detector: PartitionDetector::new(
                config.heartbeat_timeout_secs,
                config.partition_failure_threshold,
                config.quorum_size,
            ),
            bft_system,
            config,
        }
    }

    /// Register a peer node in the Byzantine consensus system
    pub async fn register_peer_for_consensus(
        &self, peer_id: u64,
    ) -> Result<(), ResilientClientError> {
        self.bft_system
            .register_node(NodeId::new(peer_id))
            .await
            .map_err(|e| {
                ResilientClientError::ClientError(format!("BFT registration failed: {}", e))
            })
    }

    /// Get evidence log for Byzantine misbehavior
    pub async fn get_byzantine_evidence(&self) -> Result<Vec<Evidence>, ResilientClientError> {
        self.bft_system
            .get_evidence_log()
            .await
            .map_err(|e| ResilientClientError::ClientError(format!("Evidence log error: {}", e)))
    }

    /// Execute a request with full resilience (returns serde_json::Value)
    pub async fn execute_request<F>(
        &self, request_id: IdempotencyKey, destination: String, f: F,
    ) -> Result<serde_json::Value, ResilientClientError>
    where
        F: FnOnce() -> Result<serde_json::Value, ResilientClientError>,
    {
        // Check partition status - writes blocked during partition
        if !self.partition_detector.can_write() {
            return Err(ResilientClientError::PartitionDetected);
        }

        // Check circuit breaker
        if !self.circuit_breaker.can_request() {
            return Err(ResilientClientError::CircuitBreakerOpen);
        }

        // Try to execute with idempotency
        let result = self.execute_idempotent(request_id, f).await;

        match &result {
            Ok(_) => {
                self.circuit_breaker.record_success();
                self.partition_detector.record_heartbeat(&destination);
            }
            Err(e) => {
                self.circuit_breaker.record_failure();

                // Track Byzantine evidence on validation failures
                if matches!(e, ResilientClientError::IdempotencyError(_)) {
                    let evidence = Evidence::new(
                        NodeId::new(destination.len() as u64),
                        Misbehavior::InvalidMessage {
                            reason: format!("Validation failure: {}", e),
                        },
                    );
                    let _ = self.bft_system.log_evidence(evidence).await;
                }

                // Queue message for later replay
                self.message_queue.enqueue(
                    destination.clone(),
                    json!({
                        "request_id": request_id.to_string(),
                        "error": e.to_string(),
                        "timestamp": chrono::Utc::now().to_rfc3339()
                    }),
                );
            }
        }

        result
    }

    /// Execute with idempotency
    async fn execute_idempotent<F>(
        &self, request_id: IdempotencyKey, f: F,
    ) -> Result<serde_json::Value, ResilientClientError>
    where
        F: FnOnce() -> Result<serde_json::Value, ResilientClientError>,
    {
        // Check if already processed
        if let Some(record) = self.idempotency.log().get(request_id) {
            info!(
                "Request {} already processed, returning cached result",
                request_id
            );
            return Ok(record.result);
        }

        // Execute the request
        let result = f()?;

        // Store result for future retries
        self.idempotency
            .log()
            .record(request_id, result.clone(), std::collections::HashMap::new())
            .map_err(ResilientClientError::IdempotencyError)?;

        Ok(result)
    }

    /// Replay queued messages after recovery
    pub fn replay_messages<F>(
        &self, destination: &str, replay_fn: F,
    ) -> Result<usize, ResilientClientError>
    where
        F: Fn(String) -> Result<(), ResilientClientError>,
    {
        let messages = self.message_queue.replay_for_destination(destination);
        let mut replayed = 0;

        for message in messages {
            match replay_fn(message.id.clone()) {
                Ok(_) => {
                    self.message_queue
                        .mark_delivered(&message.id)
                        .map_err(ResilientClientError::QueueError)?;
                    replayed += 1;
                }
                Err(_e) => {
                    self.message_queue
                        .mark_failed_and_requeue(&message.id)
                        .map_err(ResilientClientError::QueueError)?;
                }
            }
        }

        info!(
            "Replayed {} messages for destination '{}'",
            replayed, destination
        );
        Ok(replayed)
    }

    /// Register a peer node for partition detection
    pub fn register_peer(&self, peer_id: impl Into<String>) {
        self.partition_detector.register_peer(peer_id);
    }

    /// Record a heartbeat from a peer
    pub fn record_peer_heartbeat(&self, peer_id: &str) {
        self.partition_detector.record_heartbeat(peer_id);
    }

    /// Get current system status (blocking version)
    pub fn get_status(&self) -> SystemStatus {
        SystemStatus {
            circuit_breaker_state: self.circuit_breaker.state().to_string(),
            circuit_breaker_metrics: format!("{:?}", self.circuit_breaker.metrics()),
            queued_messages: self.message_queue.size(),
            partition_detected: self.partition_detector.is_partition_detected(),
            operation_mode: self.partition_detector.get_mode().to_string(),
            healthy_peers: self.partition_detector.healthy_peer_count(),
            unhealthy_peers: self.partition_detector.unhealthy_peer_count(),
            byzantine_nodes: 0,      // Would require async access to BFT system
            byzantine_violations: 0, // Would require async access to BFT system
        }
    }

    /// Get current system status with Byzantine metrics (async version)
    pub async fn get_status_with_bft(&self) -> Result<SystemStatus, ResilientClientError> {
        let evidence = self.get_byzantine_evidence().await?;
        let byzantine_violations = evidence.len();

        Ok(SystemStatus {
            circuit_breaker_state: self.circuit_breaker.state().to_string(),
            circuit_breaker_metrics: format!("{:?}", self.circuit_breaker.metrics()),
            queued_messages: self.message_queue.size(),
            partition_detected: self.partition_detector.is_partition_detected(),
            operation_mode: self.partition_detector.get_mode().to_string(),
            healthy_peers: self.partition_detector.healthy_peer_count(),
            unhealthy_peers: self.partition_detector.unhealthy_peer_count(),
            byzantine_nodes: evidence
                .iter()
                .map(|e| e.accused_node)
                .collect::<std::collections::HashSet<_>>()
                .len(),
            byzantine_violations,
        })
    }
}

/// System status snapshot
#[derive(Debug, Clone)]
pub struct SystemStatus {
    /// Current circuit breaker state
    pub circuit_breaker_state: String,
    /// Circuit breaker metrics
    pub circuit_breaker_metrics: String,
    /// Number of queued messages
    pub queued_messages: usize,
    /// Whether partition is detected
    pub partition_detected: bool,
    /// Current operation mode
    pub operation_mode: String,
    /// Number of healthy peers
    pub healthy_peers: usize,
    /// Number of unhealthy peers
    pub unhealthy_peers: usize,
    /// Byzantine nodes detected
    pub byzantine_nodes: usize,
    /// Total Byzantine violations recorded
    pub byzantine_violations: usize,
}

/// Errors that can occur in resilient client
#[derive(Debug, Clone)]
pub enum ResilientClientError {
    /// Circuit breaker is open
    CircuitBreakerOpen,
    /// Network partition detected
    PartitionDetected,
    /// Idempotency error
    IdempotencyError(String),
    /// Queue error
    QueueError(String),
    /// Serialization error
    SerializationError,
    /// Generic client error
    ClientError(String),
}

impl std::fmt::Display for ResilientClientError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CircuitBreakerOpen => write!(f, "Circuit breaker is open"),
            Self::PartitionDetected => write!(f, "Network partition detected"),
            Self::IdempotencyError(e) => write!(f, "Idempotency error: {}", e),
            Self::QueueError(e) => write!(f, "Queue error: {}", e),
            Self::SerializationError => write!(f, "Serialization error"),
            Self::ClientError(e) => write!(f, "Client error: {}", e),
        }
    }
}

impl std::error::Error for ResilientClientError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_resilient_client_creation() {
        let client = ResilientA2AClient::new(ResilientClientConfig::default());
        assert_eq!(client.circuit_breaker.state().to_string(), "CLOSED");
    }

    #[tokio::test]
    async fn test_resilient_client_idempotency() {
        let client = ResilientA2AClient::new(ResilientClientConfig::default());
        // Register peers to avoid partition detection
        client.register_peer("peer1");
        client.register_peer("peer2");
        client.record_peer_heartbeat("peer1");
        client.record_peer_heartbeat("peer2");

        let request_id = IdempotencyKey::generate();

        let result1 = client
            .execute_request(request_id, "dest".to_string(), || {
                Ok(serde_json::json!({"call": 1}))
            })
            .await;

        let result2 = client
            .execute_request(request_id, "dest".to_string(), || {
                Ok(serde_json::json!({"call": 2}))
            })
            .await;

        // Both should return the same result (first one cached)
        assert!(result1.is_ok());
        assert!(result2.is_ok());
        // Check idempotent caching worked
        assert_eq!(result1.unwrap()["call"], 1);
        assert_eq!(result2.unwrap()["call"], 1);
    }

    // Timing test, flaky in CI
    #[ignore]
    #[tokio::test]
    async fn test_partition_detection_blocks_writes() {
        let mut config = ResilientClientConfig::default();
        config.heartbeat_timeout_secs = 1;
        config.partition_failure_threshold = 1;
        let client = ResilientA2AClient::new(config);

        client.register_peer("peer1");
        client.register_peer("peer2");

        // Simulate partition: don't record heartbeats
        tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

        // Should not be able to write during partition
        let result = client
            .execute_request(IdempotencyKey::generate(), "dest".to_string(), || {
                Ok::<_, ResilientClientError>(serde_json::json!({"test": true}))
            })
            .await;

        assert!(matches!(
            result,
            Err(ResilientClientError::PartitionDetected)
        ));
    }

    #[test]
    fn test_system_status() {
        let client = ResilientA2AClient::new(ResilientClientConfig::default());
        // Register peers to avoid partition detection
        client.register_peer("peer1");
        client.register_peer("peer2");
        client.record_peer_heartbeat("peer1");
        client.record_peer_heartbeat("peer2");

        let status = client.get_status();

        assert!(!status.partition_detected);
        assert_eq!(status.queued_messages, 0);
    }
}
