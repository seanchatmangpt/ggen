//! Edge Computing Ontology Federation Module
//!
//! This module implements a distributed ontology federation protocol for edge computing
//! environments, featuring:
//! - Gossip-based semantic synchronization across IoT devices
//! - Bandwidth-optimal RDF delta encoding
//! - CRDT extensions for distributed knowledge graphs
//! - Fog computing template generation nodes

pub mod gossip;
pub mod rdf_delta;
pub mod crdt;
pub mod fog_node;
pub mod protocol;

pub use gossip::{GossipNode, GossipMessage, GossipConfig};
pub use rdf_delta::{RdfDelta, DeltaEncoder, CompressionStrategy};
pub use crdt::{DistributedKnowledgeGraph, CrdtOperation, MergeStrategy};
pub use fog_node::{FogNode, TemplateGenerationNode, EdgeDevice};
pub use protocol::{FederationProtocol, SyncRequest, SyncResponse};

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::{Duration, SystemTime};

/// Node identifier in the federation network
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct NodeId(pub String);

impl NodeId {
    pub fn new(id: impl Into<String>) -> Self {
        Self(id.into())
    }
}

/// Vector clock for causal ordering of events
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct VectorClock {
    pub clocks: HashMap<NodeId, u64>,
}

impl VectorClock {
    pub fn new() -> Self {
        Self {
            clocks: HashMap::new(),
        }
    }

    pub fn increment(&mut self, node_id: &NodeId) {
        *self.clocks.entry(node_id.clone()).or_insert(0) += 1;
    }

    pub fn merge(&mut self, other: &VectorClock) {
        for (node_id, &count) in &other.clocks {
            let current = self.clocks.entry(node_id.clone()).or_insert(0);
            *current = (*current).max(count);
        }
    }

    pub fn happens_before(&self, other: &VectorClock) -> bool {
        let mut strictly_less = false;
        for (node_id, &other_count) in &other.clocks {
            let self_count = self.clocks.get(node_id).copied().unwrap_or(0);
            if self_count > other_count {
                return false;
            }
            if self_count < other_count {
                strictly_less = true;
            }
        }
        strictly_less
    }

    pub fn concurrent(&self, other: &VectorClock) -> bool {
        !self.happens_before(other) && !other.happens_before(self)
    }
}

impl Default for VectorClock {
    fn default() -> Self {
        Self::new()
    }
}

/// Network topology for edge computing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum NetworkTopology {
    /// Fully connected mesh network
    Mesh,
    /// Tree-based hierarchy (fog nodes at intermediate layers)
    Hierarchical { max_depth: usize },
    /// Ring topology for gossip protocols
    Ring,
    /// Hybrid topology combining multiple strategies
    Hybrid {
        local: Box<NetworkTopology>,
        global: Box<NetworkTopology>,
    },
}

/// Quality of Service metrics for edge networks
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QosMetrics {
    pub bandwidth_kbps: u32,
    pub latency_ms: u32,
    pub packet_loss_rate: f32,
    pub jitter_ms: u32,
    pub battery_level: Option<f32>,
}

impl QosMetrics {
    pub fn is_constrained(&self) -> bool {
        self.bandwidth_kbps < 100 || self.latency_ms > 100 || self.packet_loss_rate > 0.05
    }
}

/// Edge device capabilities
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeviceCapabilities {
    pub cpu_cores: u32,
    pub memory_mb: u64,
    pub storage_mb: u64,
    pub can_generate_templates: bool,
    pub max_ontology_size_mb: u64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vector_clock_increment() {
        let mut clock = VectorClock::new();
        let node = NodeId::new("node1");

        clock.increment(&node);
        assert_eq!(clock.clocks.get(&node), Some(&1));

        clock.increment(&node);
        assert_eq!(clock.clocks.get(&node), Some(&2));
    }

    #[test]
    fn test_vector_clock_happens_before() {
        let mut clock1 = VectorClock::new();
        let mut clock2 = VectorClock::new();
        let node1 = NodeId::new("node1");
        let node2 = NodeId::new("node2");

        clock1.increment(&node1);
        clock2.increment(&node1);
        clock2.increment(&node1);
        clock2.increment(&node2);

        assert!(clock1.happens_before(&clock2));
        assert!(!clock2.happens_before(&clock1));
    }

    #[test]
    fn test_vector_clock_concurrent() {
        let mut clock1 = VectorClock::new();
        let mut clock2 = VectorClock::new();
        let node1 = NodeId::new("node1");
        let node2 = NodeId::new("node2");

        clock1.increment(&node1);
        clock2.increment(&node2);

        assert!(clock1.concurrent(&clock2));
        assert!(clock2.concurrent(&clock1));
    }

    #[test]
    fn test_qos_metrics_constrained() {
        let constrained = QosMetrics {
            bandwidth_kbps: 50,
            latency_ms: 200,
            packet_loss_rate: 0.1,
            jitter_ms: 50,
            battery_level: Some(0.2),
        };

        assert!(constrained.is_constrained());

        let normal = QosMetrics {
            bandwidth_kbps: 1000,
            latency_ms: 20,
            packet_loss_rate: 0.01,
            jitter_ms: 5,
            battery_level: Some(0.8),
        };

        assert!(!normal.is_constrained());
    }
}
