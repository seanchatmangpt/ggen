use crate::error::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ConsensusResult {
    Agreed,
    Disagreed,
    Pending,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Vote {
    pub voter_id: Uuid,
    pub decision: bool,
    pub timestamp: u64,
}

#[derive(Debug, Clone)]
pub struct Consensus {
    proposal_id: Uuid,
    votes: HashMap<Uuid, bool>,
    required_votes: usize,
}

impl Consensus {
    pub fn new(required_votes: usize) -> Self {
        Self {
            proposal_id: Uuid::new_v4(),
            votes: HashMap::new(),
            required_votes,
        }
    }

    pub fn add_vote(&mut self, voter_id: Uuid, decision: bool) -> Result<()> {
        self.votes.insert(voter_id, decision);
        Ok(())
    }

    pub fn get_result(&self) -> ConsensusResult {
        if self.votes.len() < self.required_votes {
            return ConsensusResult::Pending;
        }

        let agreed = self.votes.values().filter(|&&v| v).count();
        let disagreed = self.votes.values().filter(|&&v| !v).count();

        if agreed > disagreed {
            ConsensusResult::Agreed
        } else if disagreed > agreed {
            ConsensusResult::Disagreed
        } else {
            ConsensusResult::Pending
        }
    }

    pub fn is_complete(&self) -> bool {
        self.votes.len() >= self.required_votes
    }

    pub fn vote_count(&self) -> (usize, usize) {
        let agreed = self.votes.values().filter(|&&v| v).count();
        let disagreed = self.votes.len() - agreed;
        (agreed, disagreed)
    }

    pub fn get_proposal_id(&self) -> Uuid {
        self.proposal_id
    }

    pub fn get_required_votes(&self) -> usize {
        self.required_votes
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_consensus_creation() {
        let consensus = Consensus::new(3);
        assert_eq!(consensus.required_votes, 3);
        assert!(!consensus.is_complete());
    }

    #[test]
    fn test_consensus_add_vote() {
        let mut consensus = Consensus::new(2);
        let voter1 = Uuid::new_v4();

        assert!(consensus.add_vote(voter1, true).is_ok());
        assert_eq!(consensus.votes.len(), 1);
    }

    #[test]
    fn test_consensus_pending_result() {
        let mut consensus = Consensus::new(3);
        let voter1 = Uuid::new_v4();

        consensus.add_vote(voter1, true).unwrap();
        assert_eq!(consensus.get_result(), ConsensusResult::Pending);
    }

    #[test]
    fn test_consensus_agreed() {
        let mut consensus = Consensus::new(3);
        let voter1 = Uuid::new_v4();
        let voter2 = Uuid::new_v4();
        let voter3 = Uuid::new_v4();

        consensus.add_vote(voter1, true).unwrap();
        consensus.add_vote(voter2, true).unwrap();
        consensus.add_vote(voter3, false).unwrap();

        assert_eq!(consensus.get_result(), ConsensusResult::Agreed);
    }

    #[test]
    fn test_consensus_disagreed() {
        let mut consensus = Consensus::new(3);
        let voter1 = Uuid::new_v4();
        let voter2 = Uuid::new_v4();
        let voter3 = Uuid::new_v4();

        consensus.add_vote(voter1, false).unwrap();
        consensus.add_vote(voter2, false).unwrap();
        consensus.add_vote(voter3, true).unwrap();

        assert_eq!(consensus.get_result(), ConsensusResult::Disagreed);
    }

    #[test]
    fn test_consensus_vote_count() {
        let mut consensus = Consensus::new(3);
        let voter1 = Uuid::new_v4();
        let voter2 = Uuid::new_v4();
        let voter3 = Uuid::new_v4();

        consensus.add_vote(voter1, true).unwrap();
        consensus.add_vote(voter2, true).unwrap();
        consensus.add_vote(voter3, false).unwrap();

        let (agreed, disagreed) = consensus.vote_count();
        assert_eq!(agreed, 2);
        assert_eq!(disagreed, 1);
    }

    #[test]
    fn test_consensus_is_complete() {
        let mut consensus = Consensus::new(2);
        let voter1 = Uuid::new_v4();
        let voter2 = Uuid::new_v4();

        assert!(!consensus.is_complete());

        consensus.add_vote(voter1, true).unwrap();
        assert!(!consensus.is_complete());

        consensus.add_vote(voter2, false).unwrap();
        assert!(consensus.is_complete());
    }

    #[test]
    fn test_consensus_unanimous_agreement() {
        let mut consensus = Consensus::new(4);
        let voters: Vec<Uuid> = (0..4).map(|_| Uuid::new_v4()).collect();

        for voter in voters {
            consensus.add_vote(voter, true).unwrap();
        }

        assert_eq!(consensus.get_result(), ConsensusResult::Agreed);
        let (agreed, disagreed) = consensus.vote_count();
        assert_eq!(agreed, 4);
        assert_eq!(disagreed, 0);
    }
}
