//! Practical Byzantine Fault Tolerance (PBFT) consensus protocol
//!
//! Implements the PBFT algorithm for Byzantine consensus:
//! 1. Pre-prepare: Primary broadcasts message
//! 2. Prepare: Replicas broadcast prepare messages (need 2f prepares)
//! 3. Commit: Replicas broadcast commit messages (need 2f+1 commits)
//! 4. Reply: Execute and reply to client

use crate::{
    quorum::{QuorumCalculator, QuorumConfig},
    voting::{Vote, VoteCollector, VoteType},
    ConsensusError, ConsensusMessage, ConsensusProtocol, Digest, ReplicaId, Result,
    SequenceNumber, ViewNumber,
};
use ed25519_dalek::{Signer, SigningKey, VerifyingKey};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// PBFT protocol phases
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Phase {
    /// Idle - waiting for request
    Idle,
    /// Pre-prepare received, waiting for prepares
    PrePrepare,
    /// Prepared - received 2f prepares, waiting for commits
    Prepared,
    /// Committed - received 2f+1 commits, can execute
    Committed,
}

/// PBFT configuration
#[derive(Debug, Clone)]
pub struct PbftConfig {
    /// This replica's ID
    pub replica_id: ReplicaId,
    /// This replica's signing key
    pub signing_key: SigningKey,
    /// All replica public keys
    pub public_keys: HashMap<ReplicaId, VerifyingKey>,
    /// Quorum configuration
    pub quorum_config: QuorumConfig,
    /// Timeout for view change (milliseconds)
    pub view_change_timeout_ms: u64,
}

impl PbftConfig {
    /// Create a new PBFT configuration
    pub fn new(
        replica_id: ReplicaId,
        signing_key: SigningKey,
        public_keys: HashMap<ReplicaId, VerifyingKey>,
        quorum_config: QuorumConfig,
    ) -> Self {
        Self {
            replica_id,
            signing_key,
            public_keys,
            quorum_config,
            view_change_timeout_ms: 5000,
        }
    }
}

/// Request state in PBFT
#[derive(Debug, Clone)]
struct RequestState {
    /// Message being processed
    message: Vec<u8>,
    /// Message digest
    _digest: Digest,
    /// Current phase
    phase: Phase,
    /// Vote collector
    vote_collector: VoteCollector,
}

/// PBFT consensus implementation
#[derive(Debug)]
pub struct PbftConsensus {
    /// Configuration
    config: PbftConfig,
    /// Current view number
    current_view: ViewNumber,
    /// Current sequence number
    current_sequence: SequenceNumber,
    /// Active requests by digest
    requests: HashMap<Digest, RequestState>,
    /// Quorum calculator
    quorum: QuorumCalculator,
    /// Committed digests
    committed: HashMap<Digest, Vec<u8>>,
}

impl PbftConsensus {
    /// Create a new PBFT consensus instance
    pub fn new(config: PbftConfig) -> Self {
        let quorum = QuorumCalculator::new(config.quorum_config);

        Self {
            config,
            current_view: 0,
            current_sequence: 0,
            requests: HashMap::new(),
            quorum,
            committed: HashMap::new(),
        }
    }

    /// Check if this replica is the primary for the current view
    pub fn is_primary(&self) -> bool {
        self.primary_for_view(self.current_view) == self.config.replica_id
    }

    /// Get primary replica ID for a view
    fn primary_for_view(&self, view: ViewNumber) -> ReplicaId {
        let mut replica_ids: Vec<_> = self.config.public_keys.keys().copied().collect();
        replica_ids.sort_unstable();
        replica_ids[(view as usize) % replica_ids.len()]
    }

    /// Sign a message
    fn sign_message(&self, message: &[u8]) -> Vec<u8> {
        let hash = blake3::hash(message);
        let signature = self.config.signing_key.sign(hash.as_bytes());
        signature.to_bytes().to_vec()
    }

    /// Process pre-prepare message
    fn process_pre_prepare(
        &mut self,
        view: ViewNumber,
        sequence: SequenceNumber,
        digest: Digest,
        message: Vec<u8>,
    ) -> Result<()> {
        // Verify we're in the right view
        if view != self.current_view {
            return Err(ConsensusError::InvalidView {
                expected: self.current_view,
                got: view,
            });
        }

        // Verify digest matches message
        let computed_digest: Digest = blake3::hash(&message).into();
        if computed_digest != digest {
            return Err(ConsensusError::DigestMismatch);
        }

        // Create vote collector
        let vote_collector = VoteCollector::new(
            view,
            sequence,
            digest,
            self.quorum.clone(),
            self.config.public_keys.clone(),
        );

        // Store request state
        let state = RequestState {
            message,
            _digest: digest,
            phase: Phase::PrePrepare,
            vote_collector,
        };

        self.requests.insert(digest, state);

        // Broadcast prepare message
        self.broadcast_prepare(view, sequence, digest)?;

        Ok(())
    }

    /// Broadcast prepare message
    fn broadcast_prepare(
        &mut self,
        view: ViewNumber,
        sequence: SequenceNumber,
        digest: Digest,
    ) -> Result<()> {
        // Create prepare vote
        let vote = self.create_vote(VoteType::Prepare, view, sequence, digest)?;

        // Add own vote
        if let Some(state) = self.requests.get_mut(&digest) {
            state.vote_collector.add_vote(vote)?;
        }

        Ok(())
    }

    /// Create a vote
    fn create_vote(
        &self,
        vote_type: VoteType,
        view: ViewNumber,
        sequence: SequenceNumber,
        digest: Digest,
    ) -> Result<Vote> {
        let mut content = Vec::new();
        content.extend_from_slice(&[vote_type as u8]);
        content.extend_from_slice(&view.to_le_bytes());
        content.extend_from_slice(&sequence.to_le_bytes());
        content.extend_from_slice(&digest);

        let signature = self.sign_message(&content);

        Ok(Vote::new(
            vote_type,
            view,
            sequence,
            digest,
            self.config.replica_id,
            signature,
        ))
    }

    /// Process prepare message
    fn process_prepare(&mut self, vote: Vote) -> Result<()> {
        let digest = vote.digest;

        // Check if we need to broadcast commit
        let should_broadcast = {
            // Get request state
            let state = self.requests.get_mut(&digest).ok_or_else(|| {
                ConsensusError::ByzantineFault {
                    reason: "Prepare for unknown request".to_string(),
                }
            })?;

            // Add vote
            state.vote_collector.add_vote(vote)?;

            // Check if we have prepare quorum
            if state.vote_collector.has_prepare_quorum() && state.phase == Phase::PrePrepare {
                state.phase = Phase::Prepared;
                true
            } else {
                false
            }
        };

        // Broadcast commit message (after releasing the borrow)
        if should_broadcast {
            let view = self.requests.get(&digest).map(|s| s.vote_collector.view()).unwrap_or(0);
            let sequence = self.requests.get(&digest).map(|s| s.vote_collector.sequence()).unwrap_or(0);
            self.broadcast_commit(view, sequence, digest)?;
        }

        Ok(())
    }

    /// Broadcast commit message
    fn broadcast_commit(
        &mut self,
        view: ViewNumber,
        sequence: SequenceNumber,
        digest: Digest,
    ) -> Result<()> {
        // Create commit vote
        let vote = self.create_vote(VoteType::Commit, view, sequence, digest)?;

        // Add own vote
        if let Some(state) = self.requests.get_mut(&digest) {
            state.vote_collector.add_vote(vote)?;
        }

        Ok(())
    }

    /// Process commit message
    fn process_commit(&mut self, vote: Vote) -> Result<()> {
        let digest = vote.digest;

        // Check if we need to mark as committed
        let message_to_commit = {
            // Get request state
            let state = self.requests.get_mut(&digest).ok_or_else(|| {
                ConsensusError::ByzantineFault {
                    reason: "Commit for unknown request".to_string(),
                }
            })?;

            // Add vote
            state.vote_collector.add_vote(vote)?;

            // Check if we have commit quorum
            if state.vote_collector.has_commit_quorum() && state.phase == Phase::Prepared {
                state.phase = Phase::Committed;
                Some(state.message.clone())
            } else {
                None
            }
        };

        // Mark as committed (after releasing the borrow)
        if let Some(message) = message_to_commit {
            self.committed.insert(digest, message);
        }

        Ok(())
    }

    /// Get the current phase for a request
    pub fn get_phase(&self, digest: &Digest) -> Option<Phase> {
        self.requests.get(digest).map(|s| s.phase)
    }

    /// Get committed message
    pub fn get_committed(&self, digest: &Digest) -> Option<&[u8]> {
        self.committed.get(digest).map(|v| v.as_slice())
    }
}

impl ConsensusProtocol for PbftConsensus {
    fn propose(&mut self, message: &[u8]) -> Result<Digest> {
        if !self.is_primary() {
            return Err(ConsensusError::ByzantineFault {
                reason: "Only primary can propose".to_string(),
            });
        }

        let digest: Digest = blake3::hash(message).into();
        let sequence = self.current_sequence;
        self.current_sequence += 1;

        // Process own pre-prepare
        self.process_pre_prepare(self.current_view, sequence, digest, message.to_vec())?;

        Ok(digest)
    }

    fn process_message(&mut self, message: ConsensusMessage) -> Result<()> {
        match message {
            ConsensusMessage::PrePrepare {
                view,
                sequence,
                digest,
                message,
            } => self.process_pre_prepare(view, sequence, digest, message),

            ConsensusMessage::Prepare {
                view,
                sequence,
                digest,
                replica_id,
                signature,
            } => {
                let vote = Vote::new(VoteType::Prepare, view, sequence, digest, replica_id, signature);
                self.process_prepare(vote)
            }

            ConsensusMessage::Commit {
                view,
                sequence,
                digest,
                replica_id,
                signature,
            } => {
                let vote = Vote::new(VoteType::Commit, view, sequence, digest, replica_id, signature);
                self.process_commit(vote)
            }

            ConsensusMessage::ViewChange { .. } => {
                // View change not implemented in this basic version
                Ok(())
            }

            ConsensusMessage::NewView { .. } => {
                // New view not implemented in this basic version
                Ok(())
            }
        }
    }

    fn is_committed(&self, digest: &Digest) -> bool {
        self.committed.contains_key(digest)
    }

    fn current_view(&self) -> ViewNumber {
        self.current_view
    }

    fn change_view(&mut self) -> Result<ViewNumber> {
        self.current_view += 1;
        Ok(self.current_view)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ed25519_dalek::Signer;

    fn create_test_config(replica_id: ReplicaId, total_replicas: usize) -> (PbftConfig, Vec<SigningKey>) {
        let mut signing_keys = Vec::new();
        let mut public_keys = HashMap::new();

        for i in 0..total_replicas {
            let signing_key = SigningKey::from_bytes(&[i as u8; 32]);
            let verifying_key = signing_key.verifying_key();
            public_keys.insert(i as u64, verifying_key);
            signing_keys.push(signing_key);
        }

        let quorum_config = QuorumConfig::new(total_replicas, (total_replicas - 1) / 3).unwrap();

        let config = PbftConfig::new(
            replica_id,
            signing_keys[replica_id as usize].clone(),
            public_keys,
            quorum_config,
        );

        (config, signing_keys)
    }

    #[test]
    fn test_pbft_creation() {
        // Arrange
        let (config, _) = create_test_config(0, 4);

        // Act
        let pbft = PbftConsensus::new(config);

        // Assert
        assert_eq!(pbft.current_view(), 0);
        assert!(pbft.is_primary());
    }

    #[test]
    fn test_primary_rotation() {
        // Arrange
        let (config, _) = create_test_config(0, 4);
        let mut pbft = PbftConsensus::new(config);

        // Act & Assert
        assert_eq!(pbft.primary_for_view(0), 0);
        assert_eq!(pbft.primary_for_view(1), 1);
        assert_eq!(pbft.primary_for_view(2), 2);
        assert_eq!(pbft.primary_for_view(3), 3);
        assert_eq!(pbft.primary_for_view(4), 0); // Wraps around
    }

    #[test]
    fn test_propose_as_primary() {
        // Arrange
        let (config, _) = create_test_config(0, 4);
        let mut pbft = PbftConsensus::new(config);

        // Act
        let message = b"test message";
        let result = pbft.propose(message);

        // Assert
        assert!(result.is_ok());
        let digest = result.unwrap();
        assert_eq!(pbft.get_phase(&digest), Some(Phase::PrePrepare));
    }

    #[test]
    fn test_propose_as_non_primary() {
        // Arrange
        let (config, _) = create_test_config(1, 4); // Replica 1 is not primary for view 0
        let mut pbft = PbftConsensus::new(config);

        // Act
        let message = b"test message";
        let result = pbft.propose(message);

        // Assert
        assert!(result.is_err());
    }

    #[test]
    fn test_phase_progression() {
        // Arrange
        let (config, signing_keys) = create_test_config(0, 4);
        let mut pbft = PbftConsensus::new(config);

        let message = b"test message";
        let digest = pbft.propose(message).unwrap();

        // Act & Assert - Start in PrePrepare phase
        assert_eq!(pbft.get_phase(&digest), Some(Phase::PrePrepare));

        // Add prepare votes from other replicas (replicas 1 and 2)
        for replica_id in 1..3 {
            // Create vote from other replica
            let mut content = Vec::new();
            content.extend_from_slice(&[VoteType::Prepare as u8]);
            content.extend_from_slice(&0u64.to_le_bytes()); // view
            content.extend_from_slice(&0u64.to_le_bytes()); // sequence
            content.extend_from_slice(&digest);

            let hash = blake3::hash(&content);
            let signature = signing_keys[replica_id as usize].sign(hash.as_bytes());

            let vote = Vote::new(
                VoteType::Prepare,
                0,
                0,
                digest,
                replica_id as u64,
                signature.to_bytes().to_vec(),
            );

            pbft.process_prepare(vote).unwrap();
        }

        // Should progress to Prepared phase after 2f prepares (replica 0 already voted + 2 more = 3 total, but need only 2f = 2)
        assert_eq!(pbft.get_phase(&digest), Some(Phase::Prepared));
    }

    #[test]
    fn test_view_change() {
        // Arrange
        let (config, _) = create_test_config(0, 4);
        let mut pbft = PbftConsensus::new(config);

        // Act
        let new_view = pbft.change_view().unwrap();

        // Assert
        assert_eq!(new_view, 1);
        assert_eq!(pbft.current_view(), 1);
        assert!(!pbft.is_primary()); // Now replica 1 is primary
    }

    #[test]
    fn test_invalid_view_rejected() {
        // Arrange
        let (config, _) = create_test_config(0, 4);
        let mut pbft = PbftConsensus::new(config);

        let message = b"test message";
        let digest: Digest = blake3::hash(message).into();

        // Act - Try to process pre-prepare with wrong view
        let result = pbft.process_pre_prepare(1, 0, digest, message.to_vec());

        // Assert
        assert!(matches!(
            result,
            Err(ConsensusError::InvalidView {
                expected: 0,
                got: 1
            })
        ));
    }

    #[test]
    fn test_digest_mismatch_rejected() {
        // Arrange
        let (config, _) = create_test_config(0, 4);
        let mut pbft = PbftConsensus::new(config);

        let message = b"test message";
        let wrong_digest = [0u8; 32];

        // Act
        let result = pbft.process_pre_prepare(0, 0, wrong_digest, message.to_vec());

        // Assert
        assert!(matches!(result, Err(ConsensusError::DigestMismatch)));
    }
}
