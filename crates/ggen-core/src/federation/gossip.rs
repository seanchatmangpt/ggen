//! Gossip-based Semantic Synchronization Protocol
//!
//! Implements an epidemic-style gossip protocol optimized for IoT devices with:
//! - Push-pull gossip for bidirectional synchronization
//! - Anti-entropy mechanisms for eventual consistency
//! - Adaptive fanout based on network conditions
//! - Semantic compression for bandwidth efficiency

use super::{NodeId, VectorClock, QosMetrics, DeviceCapabilities};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::time::{Duration, SystemTime};

/// Configuration for gossip protocol
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GossipConfig {
    /// Number of peers to gossip with in each round
    pub fanout: usize,
    /// Interval between gossip rounds
    pub gossip_interval: Duration,
    /// Maximum size of gossip message in bytes
    pub max_message_size: usize,
    /// Enable adaptive fanout based on network conditions
    pub adaptive_fanout: bool,
    /// Probability of selecting random peer vs. preferred peer
    pub randomization_factor: f32,
    /// Enable semantic compression
    pub semantic_compression: bool,
}

impl Default for GossipConfig {
    fn default() -> Self {
        Self {
            fanout: 3,
            gossip_interval: Duration::from_secs(10),
            max_message_size: 65536, // 64KB default for IoT
            adaptive_fanout: true,
            randomization_factor: 0.3,
            semantic_compression: true,
        }
    }
}

/// Types of gossip messages
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GossipMessageType {
    /// Push: Send updates to peer
    Push,
    /// Pull: Request updates from peer
    Pull,
    /// PushPull: Bidirectional update exchange
    PushPull,
    /// AntiEntropy: Full state reconciliation
    AntiEntropy,
}

/// Gossip message for semantic synchronization
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GossipMessage {
    pub message_id: String,
    pub sender: NodeId,
    pub receiver: NodeId,
    pub message_type: GossipMessageType,
    pub vector_clock: VectorClock,
    pub timestamp: SystemTime,
    pub payload: GossipPayload,
    pub ttl: u32,
}

impl GossipMessage {
    pub fn new(
        sender: NodeId,
        receiver: NodeId,
        message_type: GossipMessageType,
        payload: GossipPayload,
    ) -> Self {
        Self {
            message_id: format!("{}-{}", sender.0, uuid::Uuid::new_v4()),
            sender,
            receiver,
            message_type,
            vector_clock: VectorClock::new(),
            timestamp: SystemTime::now(),
            payload,
            ttl: 5,
        }
    }

    pub fn is_expired(&self) -> bool {
        self.ttl == 0
    }

    pub fn decrement_ttl(&mut self) {
        if self.ttl > 0 {
            self.ttl -= 1;
        }
    }
}

/// Payload of gossip message containing RDF triples/deltas
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GossipPayload {
    /// RDF triple updates (subject, predicate, object, graph)
    RdfTriples {
        triples: Vec<(String, String, String, Option<String>)>,
        compressed: bool,
    },
    /// Delta-encoded updates (more bandwidth efficient)
    RdfDeltas {
        deltas: Vec<u8>,
        compression_algorithm: String,
    },
    /// Bloom filter for anti-entropy
    BloomFilter {
        filter: Vec<u8>,
        num_hashes: u32,
        size: usize,
    },
    /// Version vector for causal consistency
    VersionVector {
        versions: HashMap<NodeId, u64>,
    },
}

/// Gossip node for edge device
pub struct GossipNode {
    pub node_id: NodeId,
    pub config: GossipConfig,
    pub peers: HashMap<NodeId, PeerInfo>,
    pub vector_clock: VectorClock,
    pub message_cache: MessageCache,
    pub capabilities: DeviceCapabilities,
    pub qos_metrics: QosMetrics,
}

/// Information about a peer node
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PeerInfo {
    pub node_id: NodeId,
    pub last_seen: SystemTime,
    pub vector_clock: VectorClock,
    pub qos_metrics: QosMetrics,
    pub reliability_score: f32,
    pub round_trip_time_ms: u32,
}

impl PeerInfo {
    pub fn is_reachable(&self, timeout: Duration) -> bool {
        SystemTime::now()
            .duration_since(self.last_seen)
            .map(|d| d < timeout)
            .unwrap_or(false)
    }

    pub fn update_reliability(&mut self, success: bool) {
        const ALPHA: f32 = 0.1; // Exponential moving average factor
        let score = if success { 1.0 } else { 0.0 };
        self.reliability_score = ALPHA * score + (1.0 - ALPHA) * self.reliability_score;
    }
}

/// Message cache for deduplication and anti-entropy
pub struct MessageCache {
    seen_messages: HashSet<String>,
    cache_size_limit: usize,
}

impl MessageCache {
    pub fn new(cache_size_limit: usize) -> Self {
        Self {
            seen_messages: HashSet::new(),
            cache_size_limit,
        }
    }

    pub fn has_seen(&self, message_id: &str) -> bool {
        self.seen_messages.contains(message_id)
    }

    pub fn mark_seen(&mut self, message_id: String) {
        if self.seen_messages.len() >= self.cache_size_limit {
            // Simple FIFO eviction (could use LRU for better performance)
            self.seen_messages.clear();
        }
        self.seen_messages.insert(message_id);
    }
}

impl GossipNode {
    pub fn new(
        node_id: NodeId,
        config: GossipConfig,
        capabilities: DeviceCapabilities,
        qos_metrics: QosMetrics,
    ) -> Self {
        Self {
            node_id: node_id.clone(),
            config,
            peers: HashMap::new(),
            vector_clock: VectorClock::new(),
            message_cache: MessageCache::new(1000),
            capabilities,
            qos_metrics,
        }
    }

    /// Add a peer to the gossip network
    pub fn add_peer(&mut self, peer_info: PeerInfo) {
        self.peers.insert(peer_info.node_id.clone(), peer_info);
    }

    /// Remove a peer from the gossip network
    pub fn remove_peer(&mut self, node_id: &NodeId) {
        self.peers.remove(node_id);
    }

    /// Select peers for gossip round using adaptive strategy
    pub fn select_gossip_peers(&self) -> Vec<NodeId> {
        let mut fanout = self.config.fanout;

        // Adaptive fanout based on network conditions
        if self.config.adaptive_fanout && self.qos_metrics.is_constrained() {
            fanout = (fanout / 2).max(1);
        }

        let mut selected_peers = Vec::new();
        let mut reachable_peers: Vec<_> = self
            .peers
            .values()
            .filter(|p| p.is_reachable(Duration::from_secs(60)))
            .collect();

        // Sort by reliability score
        reachable_peers.sort_by(|a, b| {
            b.reliability_score
                .partial_cmp(&a.reliability_score)
                .unwrap_or(std::cmp::Ordering::Equal)
        });

        // Select mix of reliable and random peers
        let reliable_count = ((1.0 - self.config.randomization_factor) * fanout as f32) as usize;
        let random_count = fanout - reliable_count;

        // Select reliable peers
        for peer in reachable_peers.iter().take(reliable_count) {
            selected_peers.push(peer.node_id.clone());
        }

        // Select random peers for exploration
        use rand::seq::SliceRandom;
        let mut rng = rand::thread_rng();
        let remaining: Vec<_> = reachable_peers
            .iter()
            .skip(reliable_count)
            .collect();

        for peer in remaining.choose_multiple(&mut rng, random_count) {
            selected_peers.push(peer.node_id.clone());
        }

        selected_peers
    }

    /// Create a gossip message for a peer
    pub fn create_gossip_message(
        &mut self,
        receiver: &NodeId,
        message_type: GossipMessageType,
        payload: GossipPayload,
    ) -> GossipMessage {
        self.vector_clock.increment(&self.node_id);

        let mut msg = GossipMessage::new(
            self.node_id.clone(),
            receiver.clone(),
            message_type,
            payload,
        );
        msg.vector_clock = self.vector_clock.clone();
        msg
    }

    /// Process incoming gossip message
    pub fn process_message(&mut self, mut message: GossipMessage) -> Result<Option<GossipMessage>, String> {
        // Check if already seen
        if self.message_cache.has_seen(&message.message_id) {
            return Ok(None);
        }

        // Mark as seen
        self.message_cache.mark_seen(message.message_id.clone());

        // Update vector clock
        self.vector_clock.merge(&message.vector_clock);

        // Update peer information
        if let Some(peer) = self.peers.get_mut(&message.sender) {
            peer.last_seen = SystemTime::now();
            peer.vector_clock = message.vector_clock.clone();
            peer.update_reliability(true);
        }

        // Process based on message type
        match message.message_type {
            GossipMessageType::Push => {
                // Received updates, apply them
                self.apply_payload(&message.payload)?;
                Ok(None)
            }
            GossipMessageType::Pull => {
                // Peer requesting updates, send what we have
                let response_payload = self.generate_delta_payload(&message.sender)?;
                let response = self.create_gossip_message(
                    &message.sender,
                    GossipMessageType::Push,
                    response_payload,
                );
                Ok(Some(response))
            }
            GossipMessageType::PushPull => {
                // Bidirectional exchange
                self.apply_payload(&message.payload)?;
                let response_payload = self.generate_delta_payload(&message.sender)?;
                let response = self.create_gossip_message(
                    &message.sender,
                    GossipMessageType::Push,
                    response_payload,
                );
                Ok(Some(response))
            }
            GossipMessageType::AntiEntropy => {
                // Full reconciliation
                self.reconcile_state(&message.sender, &message.payload)?;
                Ok(None)
            }
        }
    }

    /// Apply gossip payload (placeholder - would integrate with RDF store)
    fn apply_payload(&mut self, payload: &GossipPayload) -> Result<(), String> {
        match payload {
            GossipPayload::RdfTriples { triples, .. } => {
                // Would apply triples to local RDF store
                Ok(())
            }
            GossipPayload::RdfDeltas { deltas, .. } => {
                // Would decode and apply deltas
                Ok(())
            }
            _ => Ok(()),
        }
    }

    /// Generate delta payload for a peer (placeholder)
    fn generate_delta_payload(&self, peer_id: &NodeId) -> Result<GossipPayload, String> {
        // Would generate minimal delta based on peer's vector clock
        Ok(GossipPayload::RdfTriples {
            triples: vec![],
            compressed: self.config.semantic_compression,
        })
    }

    /// Reconcile state with peer (anti-entropy)
    fn reconcile_state(&mut self, peer_id: &NodeId, payload: &GossipPayload) -> Result<(), String> {
        // Would perform full state reconciliation
        Ok(())
    }

    /// Perform gossip round
    pub fn gossip_round(&mut self) -> Result<Vec<GossipMessage>, String> {
        let peers = self.select_gossip_peers();
        let mut messages = Vec::new();

        for peer_id in peers {
            let payload = self.generate_delta_payload(&peer_id)?;
            let message = self.create_gossip_message(
                &peer_id,
                GossipMessageType::PushPull,
                payload,
            );
            messages.push(message);
        }

        Ok(messages)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_node(id: &str) -> GossipNode {
        let capabilities = DeviceCapabilities {
            cpu_cores: 4,
            memory_mb: 512,
            storage_mb: 1024,
            can_generate_templates: true,
            max_ontology_size_mb: 100,
        };

        let qos_metrics = QosMetrics {
            bandwidth_kbps: 1000,
            latency_ms: 20,
            packet_loss_rate: 0.01,
            jitter_ms: 5,
            battery_level: Some(0.8),
        };

        GossipNode::new(
            NodeId::new(id),
            GossipConfig::default(),
            capabilities,
            qos_metrics,
        )
    }

    #[test]
    fn test_gossip_node_creation() {
        let node = create_test_node("node1");
        assert_eq!(node.node_id.0, "node1");
        assert_eq!(node.peers.len(), 0);
    }

    #[test]
    fn test_add_remove_peer() {
        let mut node = create_test_node("node1");

        let peer_info = PeerInfo {
            node_id: NodeId::new("node2"),
            last_seen: SystemTime::now(),
            vector_clock: VectorClock::new(),
            qos_metrics: QosMetrics {
                bandwidth_kbps: 1000,
                latency_ms: 20,
                packet_loss_rate: 0.01,
                jitter_ms: 5,
                battery_level: Some(0.8),
            },
            reliability_score: 1.0,
            round_trip_time_ms: 20,
        };

        node.add_peer(peer_info);
        assert_eq!(node.peers.len(), 1);

        node.remove_peer(&NodeId::new("node2"));
        assert_eq!(node.peers.len(), 0);
    }

    #[test]
    fn test_message_cache() {
        let mut cache = MessageCache::new(10);

        assert!(!cache.has_seen("msg1"));
        cache.mark_seen("msg1".to_string());
        assert!(cache.has_seen("msg1"));
    }

    #[test]
    fn test_peer_reliability_update() {
        let mut peer = PeerInfo {
            node_id: NodeId::new("node2"),
            last_seen: SystemTime::now(),
            vector_clock: VectorClock::new(),
            qos_metrics: QosMetrics {
                bandwidth_kbps: 1000,
                latency_ms: 20,
                packet_loss_rate: 0.01,
                jitter_ms: 5,
                battery_level: Some(0.8),
            },
            reliability_score: 0.5,
            round_trip_time_ms: 20,
        };

        peer.update_reliability(true);
        assert!(peer.reliability_score > 0.5);

        peer.update_reliability(false);
        assert!(peer.reliability_score < 1.0);
    }
}
