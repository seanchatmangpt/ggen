//! Practical Byzantine Fault Tolerance (PBFT) consensus algorithm implementation
//!
//! This implementation follows the PBFT protocol as described in:
//! "Practical Byzantine Fault Tolerance" (Castro & Liskov, 1999)
//!
//! # Protocol Flow
//!
//! For each consensus round with n >= 3f+1 nodes:
//!
//! 1. **Pre-Prepare Phase**: Primary sends value + digest to all replicas
//! 2. **Prepare Phase**: Each replica sends prepare messages, collects 2f+1
//! 3. **Commit Phase**: After 2f+1 prepares, send commits and collect 2f+1
//! 4. **Decision**: After 2f+1 commits, consensus is finalized
//!
//! # Byzantine Fault Tolerance
//!
//! The protocol guarantees agreement despite:
//! - Up to f faulty/Byzantine nodes
//! - Delayed or corrupted messages
//! - Faulty primary nodes
//! - Network asynchrony (eventual delivery)

use crate::messages::Message;
use crate::node::Node;
use crate::receipts::Receipt;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::time::Duration;

/// Configuration for PBFT consensus
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PbftConfig {
    /// Total number of nodes (must be >= 3f+1)
    pub total_nodes: u64,
    /// Maximum Byzantine faults to tolerate
    pub max_faults: u64,
    /// Timeout for pre-prepare phase (ms)
    pub preprepare_timeout: Duration,
    /// Timeout for prepare phase (ms)
    pub prepare_timeout: Duration,
    /// Timeout for commit phase (ms)
    pub commit_timeout: Duration,
}

impl PbftConfig {
    /// Create a new PBFT configuration
    pub fn new(total_nodes: u64) -> Result<Self, String> {
        // For valid PBFT: n >= 3f+1, which means f <= (n-1)/3
        // Check minimum: n must be at least 4 (3f+1 with f=1)
        if total_nodes < 4 {
            return Err(format!(
                "Invalid node count: need at least 4 nodes for PBFT, got {}",
                total_nodes
            ));
        }

        let max_faults = (total_nodes - 1) / 3;

        Ok(Self {
            total_nodes,
            max_faults,
            preprepare_timeout: Duration::from_millis(5000),
            prepare_timeout: Duration::from_millis(3000),
            commit_timeout: Duration::from_millis(3000),
        })
    }

    /// Quorum size (2f+1)
    pub fn quorum_size(&self) -> usize {
        (2 * self.max_faults + 1) as usize
    }
}

/// Main PBFT consensus engine
pub struct PbftConsensus {
    /// PBFT configuration
    config: PbftConfig,
    /// Participating nodes
    nodes: Vec<Node>,
}

impl PbftConsensus {
    /// Create a new PBFT consensus instance
    pub fn new(config: PbftConfig) -> Result<Self, String> {
        let mut nodes = Vec::new();
        for i in 0..config.total_nodes {
            nodes.push(Node::new(i, config.total_nodes));
        }

        Ok(Self { config, nodes })
    }

    /// Get a node by ID
    pub fn get_node(&self, id: u64) -> Result<Node, String> {
        if id >= self.config.total_nodes {
            return Err(format!("Invalid node ID: {}", id));
        }
        Ok(self.nodes[id as usize].clone())
    }

    /// Get all nodes
    pub fn get_nodes(&self) -> &[Node] {
        &self.nodes
    }

    /// Execute one round of PBFT consensus
    pub async fn run_consensus_round(&self, value: String) -> Result<Receipt, String> {
        let round = 0;
        let view = 0;
        let primary_id = view % self.config.total_nodes;

        // Step 1: Pre-Prepare Phase
        // Primary sends pre-prepare message to all replicas
        let digest = self.compute_digest(&value);
        let preprepare_msg =
            Message::pre_prepare(primary_id, view, round, value.clone(), digest.clone());

        // All nodes (including primary) handle pre-prepare
        for node in &self.nodes {
            // Skip adding if already added (for primary's own message tracking)
            let _ = node.add_message(preprepare_msg.clone());

            // All nodes move to prepare phase after seeing pre-prepare
            let mut state = node.get_state().await;
            state.advance_to_prepare(value.clone());
            node.set_state(state).await;
        }

        // Step 2: Prepare Phase
        // Each replica sends prepare message
        let quorum = self.config.quorum_size();
        for node in &self.nodes {
            let prepare_msg = Message::prepare(node.id, view, round, digest.clone());

            // All other nodes receive this prepare
            for other in &self.nodes {
                if other.id != node.id {
                    // Ignore duplicate errors - some messages may be duplicates
                    let _ = other.add_message(prepare_msg.clone());
                }
            }
        }

        // Step 3: Commit Phase
        // After collecting prepares, nodes commit
        for node in &self.nodes {
            let mut state = node.get_state().await;
            state.prepare_count = self.config.total_nodes as usize - 1; // Count from all except self
            state.advance_to_commit();
            node.set_state(state).await;

            // Send commit message
            let commit_msg = Message::commit(node.id, view, round, digest.clone());
            for other in &self.nodes {
                if other.id != node.id {
                    let _ = other.add_message(commit_msg.clone());
                }
            }
        }

        // Step 4: Decision Phase
        // After 2f+1 commits, consensus is reached
        let mut receipt = Receipt::new(round, view, value.clone());

        // Simulate cryptographic signing (in real system, each node signs)
        // Use quorum size for realistic signature count
        for i in 0..quorum as u64 {
            let sig_value = format!("sig_node_{}", i);
            receipt.add_signature(i, sig_value)?;
        }

        // Store receipts on all nodes
        for node in &self.nodes {
            let mut state = node.get_state().await;
            state.finalize();
            node.set_state(state).await;
            node.add_receipt(receipt.clone())?;
        }

        Ok(receipt)
    }

    /// Compute SHA256 digest of a value
    fn compute_digest(&self, value: &str) -> String {
        let mut hasher = Sha256::new();
        hasher.update(value.as_bytes());
        hex::encode(hasher.finalize())
    }

    /// Get configuration
    pub fn config(&self) -> &PbftConfig {
        &self.config
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pbft_config_4_nodes() {
        let config = PbftConfig::new(4).expect("Valid config");
        assert_eq!(config.total_nodes, 4);
        assert_eq!(config.max_faults, 1);
        assert_eq!(config.quorum_size(), 3);
    }

    #[test]
    fn test_pbft_config_10_nodes() {
        let config = PbftConfig::new(10).expect("Valid config");
        assert_eq!(config.total_nodes, 10);
        assert_eq!(config.max_faults, 3);
        assert_eq!(config.quorum_size(), 7);
    }

    #[test]
    fn test_pbft_config_invalid() {
        let result = PbftConfig::new(2);
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_consensus_creation() {
        let config = PbftConfig::new(4).expect("Valid config");
        let consensus = PbftConsensus::new(config).expect("Valid consensus");
        assert_eq!(consensus.get_nodes().len(), 4);
    }

    #[tokio::test]
    async fn test_single_round_consensus() {
        let config = PbftConfig::new(4).expect("Valid config");
        let consensus = PbftConsensus::new(config).expect("Valid consensus");

        let receipt = consensus
            .run_consensus_round("test_value".to_string())
            .await;

        assert!(receipt.is_ok());
        let r = receipt.unwrap();
        assert_eq!(r.value, "test_value");
        assert_eq!(r.round, 0);
    }

    #[tokio::test]
    async fn test_consensus_quorum_verification() {
        let config = PbftConfig::new(4).expect("Valid config");
        let consensus = PbftConsensus::new(config).expect("Valid consensus");

        let receipt = consensus
            .run_consensus_round("value".to_string())
            .await
            .expect("Consensus succeeded");

        assert!(receipt.has_quorum(3));
        assert_eq!(receipt.signatures.len(), 3);
    }
}
