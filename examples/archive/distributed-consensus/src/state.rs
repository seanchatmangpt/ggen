//! Consensus state machine for PBFT protocol
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Phases in the PBFT consensus algorithm
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Phase {
    /// Waiting for pre-prepare message from primary
    PrePrepare,
    /// Preparing: collecting 2f+1 prepares
    Prepare,
    /// Committing: collected prepares, now collecting commits
    Commit,
    /// Consensus reached and committed
    Decision,
}

impl std::fmt::Display for Phase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Phase::PrePrepare => write!(f, "PRE_PREPARE"),
            Phase::Prepare => write!(f, "PREPARE"),
            Phase::Commit => write!(f, "COMMIT"),
            Phase::Decision => write!(f, "DECISION"),
        }
    }
}

/// Current consensus round state for a node
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConsensusState {
    /// Current round number
    pub round: u64,
    /// Current view (primary node ID, incremented on view change)
    pub view: u64,
    /// Current phase in the PBFT state machine
    pub phase: Phase,
    /// Proposed value for this round
    pub value: Option<String>,
    /// Timestamp when this round started
    pub timestamp: DateTime<Utc>,
    /// Number of prepare messages received
    pub prepare_count: usize,
    /// Number of commit messages received
    pub commit_count: usize,
    /// IDs of nodes that sent prepare messages (for deduplication)
    pub prepared_by: HashMap<u64, DateTime<Utc>>,
    /// IDs of nodes that sent commit messages (for deduplication)
    pub committed_by: HashMap<u64, DateTime<Utc>>,
}

impl ConsensusState {
    /// Create a new consensus state for a given round
    pub fn new(round: u64, view: u64) -> Self {
        Self {
            round,
            view,
            phase: Phase::PrePrepare,
            value: None,
            timestamp: Utc::now(),
            prepare_count: 0,
            commit_count: 0,
            prepared_by: HashMap::new(),
            committed_by: HashMap::new(),
        }
    }

    /// Reset to pre-prepare phase for a new value
    pub fn reset_round(&mut self) {
        self.round += 1;
        self.phase = Phase::PrePrepare;
        self.value = None;
        self.timestamp = Utc::now();
        self.prepare_count = 0;
        self.commit_count = 0;
        self.prepared_by.clear();
        self.committed_by.clear();
    }

    /// Advance to prepare phase after receiving valid pre-prepare
    pub fn advance_to_prepare(&mut self, value: String) {
        self.value = Some(value);
        self.phase = Phase::Prepare;
    }

    /// Advance to commit phase after collecting enough prepares
    pub fn advance_to_commit(&mut self) {
        self.phase = Phase::Commit;
    }

    /// Finalize consensus decision
    pub fn finalize(&mut self) {
        self.phase = Phase::Decision;
    }

    /// Check if we have a valid quorum of prepares (2f+1)
    pub fn has_prepare_quorum(&self, quorum_size: usize) -> bool {
        self.prepare_count >= quorum_size
    }

    /// Check if we have a valid quorum of commits (2f+1)
    pub fn has_commit_quorum(&self, quorum_size: usize) -> bool {
        self.commit_count >= quorum_size
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_phase_display() {
        assert_eq!(Phase::PrePrepare.to_string(), "PRE_PREPARE");
        assert_eq!(Phase::Prepare.to_string(), "PREPARE");
        assert_eq!(Phase::Commit.to_string(), "COMMIT");
        assert_eq!(Phase::Decision.to_string(), "DECISION");
    }

    #[test]
    fn test_consensus_state_creation() {
        let state = ConsensusState::new(1, 0);
        assert_eq!(state.round, 1);
        assert_eq!(state.view, 0);
        assert_eq!(state.phase, Phase::PrePrepare);
        assert_eq!(state.prepare_count, 0);
        assert_eq!(state.commit_count, 0);
    }

    #[test]
    fn test_advance_to_prepare() {
        let mut state = ConsensusState::new(1, 0);
        state.advance_to_prepare("test_value".to_string());
        assert_eq!(state.phase, Phase::Prepare);
        assert_eq!(state.value, Some("test_value".to_string()));
    }

    #[test]
    fn test_quorum_checks() {
        let mut state = ConsensusState::new(1, 0);
        assert!(!state.has_prepare_quorum(3));
        state.prepare_count = 3;
        assert!(state.has_prepare_quorum(3));
    }
}
