//! Byzantine Fault Tolerance (BFT) consensus module
//!
//! Implements practical PBFT-lite consensus that survives 1/3 nodes being malicious.
//!
//! # Architecture
//!
//! ```text
//! 1. Leader proposes value
//! 2. 2/3 quorum echoes approval
//! 3. If consensus reached: commit
//! 4. If consensus fails: request new leader
//! ```
//!
//! # Safety Guarantees
//!
//! - Even if 1 node is Byzantine, value is safe
//! - Can detect and isolate malicious nodes
//! - Consensus progress even with failures

pub mod consensus;
pub mod evidence;
pub mod leader;
pub mod messages;

pub use consensus::{
    ByzantineConsensus, CommittedValue, ConsensusConfig, PBFTLiteConsensus, ProposalValue,
};
pub use evidence::{Evidence, EvidenceLog, Misbehavior};
pub use leader::{LeaderElectionStrategy, LeaderElector};
pub use messages::{Message, MessageType};

use crate::error::Result;
use std::collections::HashSet;
use std::sync::Arc;
use tokio::sync::RwLock;

/// Node identifier
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, serde::Serialize, serde::Deserialize,
)]
pub struct NodeId(pub u64);

impl NodeId {
    pub fn new(id: u64) -> Self {
        Self(id)
    }
}

/// Proposal round number
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, serde::Serialize, serde::Deserialize,
)]
pub struct Round(pub u64);

impl Round {
    pub fn new(num: u64) -> Self {
        Self(num)
    }

    pub fn next(self) -> Self {
        Self(self.0 + 1)
    }
}

/// Byzantine consensus state machine
pub struct BFTSystem {
    node_id: NodeId,
    config: ConsensusConfig,
    consensus: Arc<RwLock<Option<Box<dyn ByzantineConsensus>>>>,
    nodes: Arc<RwLock<HashSet<NodeId>>>,
    evidence_log: Arc<RwLock<EvidenceLog>>,
}

impl BFTSystem {
    /// Create a new BFT system
    pub fn new(node_id: NodeId, config: ConsensusConfig) -> Self {
        Self {
            node_id,
            config,
            consensus: Arc::new(RwLock::new(None)),
            nodes: Arc::new(RwLock::new(HashSet::new())),
            evidence_log: Arc::new(RwLock::new(EvidenceLog::new())),
        }
    }

    /// Register a node in the network
    pub async fn register_node(&self, node_id: NodeId) -> Result<()> {
        let mut nodes = self.nodes.write().await;
        nodes.insert(node_id);
        Ok(())
    }

    /// Get all registered nodes
    pub async fn get_nodes(&self) -> Result<Vec<NodeId>> {
        let nodes = self.nodes.read().await;
        Ok(nodes.iter().copied().collect())
    }

    /// Get the evidence log
    pub async fn get_evidence_log(&self) -> Result<Vec<Evidence>> {
        let log = self.evidence_log.read().await;
        Ok(log.get_all().to_vec())
    }

    /// Log misbehavior evidence
    pub async fn log_evidence(&self, evidence: Evidence) -> Result<()> {
        let mut log = self.evidence_log.write().await;
        log.add(evidence);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_bft_system_creation() {
        let config = ConsensusConfig::default();
        let system = BFTSystem::new(NodeId::new(1), config);
        assert_eq!(system.node_id, NodeId::new(1));
    }

    #[tokio::test]
    async fn test_register_node() {
        let config = ConsensusConfig::default();
        let system = BFTSystem::new(NodeId::new(1), config);
        let node2 = NodeId::new(2);

        system.register_node(node2).await.unwrap();
        let nodes = system.get_nodes().await.unwrap();
        assert!(nodes.contains(&node2));
    }

    #[tokio::test]
    async fn test_multiple_nodes() {
        let config = ConsensusConfig::default();
        let system = BFTSystem::new(NodeId::new(1), config);

        for i in 2..=5 {
            system.register_node(NodeId::new(i)).await.unwrap();
        }

        let nodes = system.get_nodes().await.unwrap();
        assert_eq!(nodes.len(), 4);
    }
}
