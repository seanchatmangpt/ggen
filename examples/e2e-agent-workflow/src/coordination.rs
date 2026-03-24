//! Distributed consensus and coordination between agents

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

/// Voting strategy for consensus
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VotingStrategy {
    /// Simple majority (>50%)
    Majority,
    /// Supermajority (2/3)
    Supermajority,
    /// Unanimous agreement (100%)
    Unanimous,
}

impl VotingStrategy {
    /// Calculate if proposal passes given votes
    pub fn passes(&self, votes_yes: usize, votes_no: usize) -> bool {
        let total = votes_yes + votes_no;
        if total == 0 {
            return false;
        }
        match self {
            VotingStrategy::Majority => votes_yes > total / 2,
            VotingStrategy::Supermajority => votes_yes * 3 > total * 2,
            VotingStrategy::Unanimous => votes_no == 0,
        }
    }
}

/// Agent vote on a proposal
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentVote {
    /// Agent ID voting
    pub agent_id: Uuid,
    /// Vote (true = yes, false = no)
    pub vote: bool,
    /// Reason for vote
    pub reason: String,
}

/// Result of consensus voting
#[derive(Debug, Clone)]
pub struct ConsensusResult {
    /// Proposal ID
    pub proposal_id: String,
    /// Votes received
    pub votes: Vec<AgentVote>,
    /// Final decision (true = approved)
    pub approved: bool,
    /// Strategy used
    pub strategy: VotingStrategy,
}

impl ConsensusResult {
    /// Get number of yes votes
    pub fn yes_votes(&self) -> usize {
        self.votes.iter().filter(|v| v.vote).count()
    }

    /// Get number of no votes
    pub fn no_votes(&self) -> usize {
        self.votes.iter().filter(|v| !v.vote).count()
    }

    /// Get approval percentage
    pub fn approval_percentage(&self) -> f64 {
        if self.votes.is_empty() {
            0.0
        } else {
            (self.yes_votes() as f64 / self.votes.len() as f64) * 100.0
        }
    }
}

/// Manages consensus across agents
pub struct ConsensusManager {
    strategy: VotingStrategy,
    min_participants: usize,
}

impl ConsensusManager {
    /// Create a new consensus manager
    pub fn new(strategy: VotingStrategy, min_participants: usize) -> Self {
        Self {
            strategy,
            min_participants,
        }
    }

    /// Default consensus manager with supermajority
    pub fn default_supermajority(total_agents: usize) -> Self {
        Self {
            strategy: VotingStrategy::Supermajority,
            min_participants: (total_agents * 2) / 3 + 1,
        }
    }

    /// Record votes and determine consensus
    pub fn reach_consensus(
        &self,
        proposal_id: String,
        votes: Vec<AgentVote>,
    ) -> ConsensusResult {
        let approved = self.strategy.passes(
            votes.iter().filter(|v| v.vote).count(),
            votes.iter().filter(|v| !v.vote).count(),
        );

        ConsensusResult {
            proposal_id,
            votes,
            approved,
            strategy: self.strategy,
        }
    }

    /// Check if enough participants have voted
    pub fn has_quorum(&self, vote_count: usize) -> bool {
        vote_count >= self.min_participants
    }
}

/// Byzantine fault tolerance: tolerate f failures in 3f+1 agents
#[derive(Debug)]
pub struct ByzantineFaultTolerance {
    total_agents: usize,
    max_failures: usize,
}

impl ByzantineFaultTolerance {
    /// Create BFT with given agent count
    pub fn new(total_agents: usize) -> Self {
        let max_failures = (total_agents - 1) / 3;
        Self {
            total_agents,
            max_failures,
        }
    }

    /// Check if system can tolerate given failures
    pub fn can_tolerate(&self, failures: usize) -> bool {
        failures <= self.max_failures
    }

    /// Get maximum tolerable failures
    pub fn max_tolerable_failures(&self) -> usize {
        self.max_failures
    }

    /// Calculate minimum healthy agents needed
    pub fn min_healthy_agents_needed(&self) -> usize {
        self.total_agents - self.max_failures
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_majority_voting() {
        let strategy = VotingStrategy::Majority;
        assert!(strategy.passes(3, 0));
        assert!(strategy.passes(2, 1));
        assert!(!strategy.passes(1, 2));
    }

    #[test]
    fn test_supermajority_voting() {
        let strategy = VotingStrategy::Supermajority;
        assert!(strategy.passes(3, 0));
        assert!(strategy.passes(2, 1));
        assert!(!strategy.passes(1, 2));
    }

    #[test]
    fn test_unanimous_voting() {
        let strategy = VotingStrategy::Unanimous;
        assert!(strategy.passes(3, 0));
        assert!(!strategy.passes(2, 1));
        assert!(!strategy.passes(1, 2));
    }

    #[test]
    fn test_consensus_result_percentages() {
        let votes = vec![
            AgentVote {
                agent_id: Uuid::new_v4(),
                vote: true,
                reason: "agree".to_string(),
            },
            AgentVote {
                agent_id: Uuid::new_v4(),
                vote: true,
                reason: "agree".to_string(),
            },
            AgentVote {
                agent_id: Uuid::new_v4(),
                vote: false,
                reason: "disagree".to_string(),
            },
        ];

        let result = ConsensusResult {
            proposal_id: "test".to_string(),
            votes,
            approved: true,
            strategy: VotingStrategy::Majority,
        };

        assert_eq!(result.yes_votes(), 2);
        assert_eq!(result.no_votes(), 1);
    }

    #[test]
    fn test_byzantine_tolerance() {
        let bft = ByzantineFaultTolerance::new(7);
        assert_eq!(bft.max_tolerable_failures(), 2);
        assert!(bft.can_tolerate(2));
        assert!(!bft.can_tolerate(3));
    }

    #[test]
    fn test_consensus_manager_quorum() {
        let manager = ConsensusManager::new(VotingStrategy::Majority, 3);
        assert!(manager.has_quorum(3));
        assert!(!manager.has_quorum(2));
    }
}
