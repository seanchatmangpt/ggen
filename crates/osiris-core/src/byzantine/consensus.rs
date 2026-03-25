//! Core consensus trait and implementation

use crate::error::{OSIRISError, Result};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

use super::{Evidence, Misbehavior, NodeId, Round};

/// A value proposed for consensus
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct ProposalValue {
    pub id: String,
    pub data: String,
    pub timestamp: u64,
}

impl ProposalValue {
    pub fn new(data: String) -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            data,
            timestamp: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_secs(),
        }
    }
}

/// A committed consensus value
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CommittedValue {
    pub value: ProposalValue,
    pub round: Round,
    pub approvers: Vec<NodeId>,
}

impl CommittedValue {
    pub fn new(value: ProposalValue, round: Round, approvers: Vec<NodeId>) -> Self {
        Self {
            value,
            round,
            approvers,
        }
    }
}

/// Configuration for Byzantine consensus
#[derive(Debug, Clone)]
pub struct ConsensusConfig {
    /// Maximum number of faulty nodes tolerated (f = N/3)
    pub max_faulty_nodes: usize,
    /// Total number of nodes
    pub total_nodes: usize,
    /// Timeout for proposal in milliseconds
    pub proposal_timeout_ms: u64,
    /// Timeout for commit in milliseconds
    pub commit_timeout_ms: u64,
}

impl ConsensusConfig {
    pub fn new(total_nodes: usize) -> Self {
        let max_faulty = total_nodes / 3; // f = N/3
        Self {
            max_faulty_nodes: max_faulty,
            total_nodes,
            proposal_timeout_ms: 5000,
            commit_timeout_ms: 10000,
        }
    }

    /// Required quorum size (2f + 1)
    pub fn quorum_size(&self) -> usize {
        2 * self.max_faulty_nodes + 1
    }

    /// Check if quorum is reached
    pub fn is_quorum(&self, votes: usize) -> bool {
        votes >= self.quorum_size()
    }
}

impl Default for ConsensusConfig {
    fn default() -> Self {
        Self::new(4)
    }
}

/// Byzantine consensus protocol trait
#[async_trait]
pub trait ByzantineConsensus: Send + Sync {
    /// Propose a value for consensus
    async fn propose(&mut self, value: ProposalValue) -> Result<()>;

    /// Commit when ready (blocks until committed or timeout)
    async fn commit_when_ready(&mut self) -> Result<CommittedValue>;

    /// Record misbehavior evidence
    async fn detect_misbehavior(&mut self, peer_id: NodeId, evidence: Evidence) -> Result<()>;

    /// Get current round
    async fn current_round(&self) -> Round;

    /// Get approved votes for current proposal
    async fn get_approvals(&self) -> usize;

    /// Is node isolated (marked as Byzantine)?
    async fn is_node_isolated(&self, node_id: NodeId) -> bool;
}

/// Practical PBFT-lite implementation
pub struct PBFTLiteConsensus {
    node_id: NodeId,
    config: ConsensusConfig,
    current_round: Round,
    current_proposal: Option<ProposalValue>,
    approvals: HashMap<ProposalValue, Vec<NodeId>>,
    isolated_nodes: Vec<NodeId>,
    evidence_log: Vec<Evidence>,
}

impl PBFTLiteConsensus {
    pub fn new(node_id: NodeId, config: ConsensusConfig) -> Self {
        Self {
            node_id,
            config,
            current_round: Round::new(0),
            current_proposal: None,
            approvals: HashMap::new(),
            isolated_nodes: Vec::new(),
            evidence_log: Vec::new(),
        }
    }

    /// Get count of active (non-isolated) nodes
    fn active_node_count(&self) -> usize {
        self.config.total_nodes - self.isolated_nodes.len()
    }

    /// Check if we have quorum from active nodes
    fn has_active_quorum(&self, votes: usize) -> bool {
        let active = self.active_node_count();
        votes >= ((2 * active) / 3) + 1
    }

    /// Simulate receiving approval from peer
    pub fn add_approval(&mut self, peer_id: NodeId, value: ProposalValue) -> Result<()> {
        // Don't count votes from isolated nodes
        if self.isolated_nodes.contains(&peer_id) {
            return Err(OSIRISError::Unknown(format!(
                "Cannot accept vote from isolated node {:?}",
                peer_id
            )));
        }

        // Detect conflicting messages: if peer already voted for different value
        for (other_value, other_approvers) in &self.approvals {
            if other_value != &value && other_approvers.contains(&peer_id) {
                // Byzantine behavior detected!
                let evidence = Evidence::new(
                    peer_id,
                    Misbehavior::ConflictingVotes {
                        value_a: other_value.clone(),
                        value_b: value.clone(),
                    },
                );
                self.evidence_log.push(evidence);
                return Err(OSIRISError::Unknown(
                    "Byzantine behavior detected: conflicting votes".to_string(),
                ));
            }
        }

        let approvers = self.approvals.entry(value).or_insert_with(Vec::new);
        approvers.push(peer_id);
        Ok(())
    }
}

#[async_trait]
impl ByzantineConsensus for PBFTLiteConsensus {
    async fn propose(&mut self, value: ProposalValue) -> Result<()> {
        self.current_round = self.current_round.next();
        self.current_proposal = Some(value.clone());
        self.approvals.insert(value, vec![self.node_id]);
        Ok(())
    }

    async fn commit_when_ready(&mut self) -> Result<CommittedValue> {
        if let Some(proposal) = &self.current_proposal {
            let approvers = self.approvals.get(proposal).cloned().unwrap_or_default();

            if self.has_active_quorum(approvers.len()) {
                return Ok(CommittedValue::new(
                    proposal.clone(),
                    self.current_round,
                    approvers,
                ));
            }
        }

        Err(OSIRISError::Timeout(
            "No quorum reached for proposal".to_string(),
        ))
    }

    async fn detect_misbehavior(&mut self, peer_id: NodeId, evidence: Evidence) -> Result<()> {
        self.evidence_log.push(evidence.clone());

        // Isolate node on strong evidence
        if matches!(
            evidence.misbehavior,
            Misbehavior::ConflictingVotes { .. } | Misbehavior::DoubleProposal { .. }
        ) {
            if !self.isolated_nodes.contains(&peer_id) {
                self.isolated_nodes.push(peer_id);
            }
        }

        Ok(())
    }

    async fn current_round(&self) -> Round {
        self.current_round
    }

    async fn get_approvals(&self) -> usize {
        self.current_proposal
            .as_ref()
            .and_then(|p| self.approvals.get(p))
            .map(|approvers| approvers.len())
            .unwrap_or(0)
    }

    async fn is_node_isolated(&self, node_id: NodeId) -> bool {
        self.isolated_nodes.contains(&node_id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_config_quorum_size() {
        let config = ConsensusConfig::new(4);
        assert_eq!(config.quorum_size(), 3); // 2*1 + 1 = 3
    }

    #[test]
    fn test_config_is_quorum() {
        let config = ConsensusConfig::new(4);
        assert!(!config.is_quorum(2));
        assert!(config.is_quorum(3));
    }

    #[test]
    fn test_consensus_config_large_cluster() {
        let config = ConsensusConfig::new(13);
        assert_eq!(config.max_faulty_nodes, 4); // 13/3 = 4
        assert_eq!(config.quorum_size(), 9); // 2*4 + 1 = 9
    }

    #[tokio::test]
    async fn test_propose_value() {
        let config = ConsensusConfig::new(4);
        let mut consensus = PBFTLiteConsensus::new(NodeId::new(1), config);
        let value = ProposalValue::new("test data".to_string());

        let result = consensus.propose(value.clone()).await;
        assert!(result.is_ok());
        assert_eq!(consensus.current_round, Round::new(1));
    }

    #[tokio::test]
    async fn test_add_approval() {
        let config = ConsensusConfig::new(4);
        let mut consensus = PBFTLiteConsensus::new(NodeId::new(1), config);
        let value = ProposalValue::new("test data".to_string());

        consensus.propose(value.clone()).await.unwrap();
        consensus.add_approval(NodeId::new(2), value).unwrap();

        assert_eq!(consensus.get_approvals().await, 2);
    }

    #[tokio::test]
    async fn test_quorum_commit() {
        let config = ConsensusConfig::new(4);
        let mut consensus = PBFTLiteConsensus::new(NodeId::new(1), config);
        let value = ProposalValue::new("test data".to_string());

        consensus.propose(value.clone()).await.unwrap();

        // Add approvals to reach quorum (3 for 4 nodes)
        consensus
            .add_approval(NodeId::new(2), value.clone())
            .unwrap();
        consensus
            .add_approval(NodeId::new(3), value.clone())
            .unwrap();

        let committed = consensus.commit_when_ready().await;
        assert!(committed.is_ok());
    }

    #[tokio::test]
    async fn test_no_quorum_timeout() {
        let config = ConsensusConfig::new(4);
        let mut consensus = PBFTLiteConsensus::new(NodeId::new(1), config);
        let value = ProposalValue::new("test data".to_string());

        consensus.propose(value.clone()).await.unwrap();
        consensus.add_approval(NodeId::new(2), value).unwrap();

        // Only 2 votes, need 3 for quorum
        let result = consensus.commit_when_ready().await;
        assert!(result.is_err());
    }
}
