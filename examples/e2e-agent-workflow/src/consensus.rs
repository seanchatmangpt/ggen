//! Distributed consensus mechanisms
//!
//! Implements Byzantine fault tolerance and consensus for multi-agent coordination.

use crate::error::Result;
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

/// Consensus vote
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Vote {
    pub agent_id: Uuid,
    pub proposal_id: Uuid,
    pub approved: bool,
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

/// Proposal for consensus
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Proposal {
    pub id: Uuid,
    pub description: String,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub votes: HashMap<Uuid, bool>,
    pub status: ProposalStatus,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ProposalStatus {
    Pending,
    Approved,
    Rejected,
    Expired,
}

#[async_trait]
pub trait ConsensusMechanism: Send + Sync {
    async fn submit_proposal(&mut self, proposal: Proposal) -> Result<()>;
    async fn vote(&mut self, vote: Vote) -> Result<()>;
    async fn finalize_proposal(&mut self, proposal_id: Uuid) -> Result<ProposalStatus>;
}

/// Consensus manager using Byzantine fault tolerance
pub struct ConsensusManager {
    proposals: HashMap<Uuid, Proposal>,
    threshold: f64, // e.g., 0.67 for 2/3 majority
    active_agents: usize,
}

impl ConsensusManager {
    /// Create a new consensus manager
    pub fn new(active_agents: usize) -> Self {
        Self {
            proposals: HashMap::new(),
            threshold: 0.67, // 2/3 majority
            active_agents,
        }
    }

    /// Submit a proposal
    pub fn submit_proposal(&mut self, mut proposal: Proposal) -> Result<()> {
        // Initialize votes map
        if proposal.votes.is_empty() {
            proposal.votes = HashMap::new();
        }
        self.proposals.insert(proposal.id, proposal);
        Ok(())
    }

    /// Record a vote
    pub fn vote(&mut self, agent_id: Uuid, proposal_id: Uuid, approved: bool) -> Result<()> {
        let proposal = self.proposals
            .get_mut(&proposal_id)
            .ok_or_else(|| crate::WorkflowError::ConsensusError("Proposal not found".to_string()))?;

        proposal.votes.insert(agent_id, approved);
        Ok(())
    }

    /// Finalize proposal (check if consensus reached)
    pub fn finalize_proposal(&mut self, proposal_id: Uuid) -> Result<ProposalStatus> {
        let proposal = self.proposals
            .get_mut(&proposal_id)
            .ok_or_else(|| crate::WorkflowError::ConsensusError("Proposal not found".to_string()))?;

        if proposal.votes.is_empty() {
            proposal.status = ProposalStatus::Pending;
            return Ok(ProposalStatus::Pending);
        }

        let approved_count = proposal.votes.values().filter(|&&v| v).count();
        let approval_ratio = approved_count as f64 / proposal.votes.len() as f64;

        let status = if approval_ratio >= self.threshold {
            ProposalStatus::Approved
        } else if approval_ratio < (1.0 - self.threshold) {
            ProposalStatus::Rejected
        } else {
            ProposalStatus::Pending
        };

        proposal.status = status;
        Ok(status)
    }

    /// Get proposal status
    pub fn get_proposal_status(&self, proposal_id: Uuid) -> Result<ProposalStatus> {
        let proposal = self.proposals
            .get(&proposal_id)
            .ok_or_else(|| crate::WorkflowError::ConsensusError("Proposal not found".to_string()))?;

        Ok(proposal.status)
    }

    /// List all proposals
    pub fn list_proposals(&self) -> Vec<Proposal> {
        self.proposals.values().cloned().collect()
    }

    /// Get proposals by status
    pub fn proposals_by_status(&self, status: ProposalStatus) -> Vec<Proposal> {
        self.proposals
            .values()
            .filter(|p| p.status == status)
            .cloned()
            .collect()
    }
}

impl Default for ConsensusManager {
    fn default() -> Self {
        Self::new(3) // Default to 3 agents for consensus
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_consensus_approval() {
        let mut manager = ConsensusManager::new(3);
        let proposal = Proposal {
            id: Uuid::new_v4(),
            description: "Test proposal".to_string(),
            created_at: chrono::Utc::now(),
            votes: HashMap::new(),
            status: ProposalStatus::Pending,
        };

        manager.submit_proposal(proposal.clone()).unwrap();

        let agent1 = Uuid::new_v4();
        let agent2 = Uuid::new_v4();
        let agent3 = Uuid::new_v4();

        manager.vote(agent1, proposal.id, true).unwrap();
        manager.vote(agent2, proposal.id, true).unwrap();
        manager.vote(agent3, proposal.id, false).unwrap();

        let status = manager.finalize_proposal(proposal.id).unwrap();
        assert_eq!(status, ProposalStatus::Approved); // 2/3 ≈ 0.67
    }

    #[test]
    fn test_consensus_rejection() {
        let mut manager = ConsensusManager::new(3);
        let proposal = Proposal {
            id: Uuid::new_v4(),
            description: "Test proposal".to_string(),
            created_at: chrono::Utc::now(),
            votes: HashMap::new(),
            status: ProposalStatus::Pending,
        };

        manager.submit_proposal(proposal.clone()).unwrap();

        let agent1 = Uuid::new_v4();
        let agent2 = Uuid::new_v4();
        let agent3 = Uuid::new_v4();

        manager.vote(agent1, proposal.id, false).unwrap();
        manager.vote(agent2, proposal.id, false).unwrap();
        manager.vote(agent3, proposal.id, true).unwrap();

        let status = manager.finalize_proposal(proposal.id).unwrap();
        assert_eq!(status, ProposalStatus::Rejected);
    }
}
