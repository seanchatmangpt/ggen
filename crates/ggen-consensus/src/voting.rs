//! Vote collection and verification for Byzantine consensus

use crate::{
    quorum::QuorumCalculator, signatures::SignatureAggregator, ConsensusError, Digest, ReplicaId,
    Result, SequenceNumber, ViewNumber,
};
use ed25519_dalek::VerifyingKey;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

/// Type of vote in PBFT protocol
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum VoteType {
    /// Prepare vote (after receiving pre-prepare)
    Prepare,
    /// Commit vote (after receiving 2f prepares)
    Commit,
    /// View change vote
    ViewChange,
}

/// A vote from a replica
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Vote {
    /// Type of vote
    pub vote_type: VoteType,
    /// View number
    pub view: ViewNumber,
    /// Sequence number
    pub sequence: SequenceNumber,
    /// Message digest
    pub digest: Digest,
    /// Replica that cast this vote
    pub replica_id: ReplicaId,
    /// Signature over (vote_type, view, sequence, digest)
    pub signature: Vec<u8>,
}

impl Vote {
    /// Create a new vote
    pub fn new(
        vote_type: VoteType,
        view: ViewNumber,
        sequence: SequenceNumber,
        digest: Digest,
        replica_id: ReplicaId,
        signature: Vec<u8>,
    ) -> Self {
        Self {
            vote_type,
            view,
            sequence,
            digest,
            replica_id,
            signature,
        }
    }

    /// Get the vote content for signing/verification
    pub fn vote_content(&self) -> Vec<u8> {
        let mut content = Vec::new();
        content.extend_from_slice(&[self.vote_type as u8]);
        content.extend_from_slice(&self.view.to_le_bytes());
        content.extend_from_slice(&self.sequence.to_le_bytes());
        content.extend_from_slice(&self.digest);
        content
    }
}

/// Vote collector for a specific (view, sequence, digest) tuple
#[derive(Debug, Clone)]
pub struct VoteCollector {
    /// View number
    view: ViewNumber,
    /// Sequence number
    sequence: SequenceNumber,
    /// Message digest
    digest: Digest,
    /// Prepare votes received
    prepare_votes: HashMap<ReplicaId, Vote>,
    /// Commit votes received
    commit_votes: HashMap<ReplicaId, Vote>,
    /// View change votes received
    view_change_votes: HashMap<ReplicaId, Vote>,
    /// Quorum calculator
    quorum: QuorumCalculator,
    /// Signature aggregator
    aggregator: SignatureAggregator,
}

impl VoteCollector {
    /// Create a new vote collector
    pub fn new(
        view: ViewNumber,
        sequence: SequenceNumber,
        digest: Digest,
        quorum: QuorumCalculator,
        public_keys: HashMap<ReplicaId, VerifyingKey>,
    ) -> Self {
        Self {
            view,
            sequence,
            digest,
            prepare_votes: HashMap::new(),
            commit_votes: HashMap::new(),
            view_change_votes: HashMap::new(),
            quorum,
            aggregator: SignatureAggregator::new(public_keys),
        }
    }

    /// Add a vote and verify it
    pub fn add_vote(&mut self, vote: Vote) -> Result<()> {
        // Validate vote matches this collector
        if vote.view != self.view {
            return Err(ConsensusError::InvalidView {
                expected: self.view,
                got: vote.view,
            });
        }

        if vote.sequence != self.sequence {
            return Err(ConsensusError::InvalidSequence {
                expected: self.sequence,
                got: vote.sequence,
            });
        }

        if vote.digest != self.digest {
            return Err(ConsensusError::DigestMismatch);
        }

        // Verify signature
        let vote_content = vote.vote_content();
        self.aggregator
            .verify_signature(vote.replica_id, &vote_content, &vote.signature)?;

        // Check for duplicate vote
        let votes = match vote.vote_type {
            VoteType::Prepare => &mut self.prepare_votes,
            VoteType::Commit => &mut self.commit_votes,
            VoteType::ViewChange => &mut self.view_change_votes,
        };

        if votes.contains_key(&vote.replica_id) {
            return Err(ConsensusError::DuplicateVote {
                replica_id: vote.replica_id,
            });
        }

        // Add vote
        votes.insert(vote.replica_id, vote);

        Ok(())
    }

    /// Get number of prepare votes
    pub fn prepare_count(&self) -> usize {
        self.prepare_votes.len()
    }

    /// Get number of commit votes
    pub fn commit_count(&self) -> usize {
        self.commit_votes.len()
    }

    /// Get number of view change votes
    pub fn view_change_count(&self) -> usize {
        self.view_change_votes.len()
    }

    /// Check if prepare quorum reached
    pub fn has_prepare_quorum(&self) -> bool {
        self.quorum.has_prepare_quorum(self.prepare_count())
    }

    /// Check if commit quorum reached
    pub fn has_commit_quorum(&self) -> bool {
        self.quorum.has_commit_quorum(self.commit_count())
    }

    /// Check if view change quorum reached
    pub fn has_view_change_quorum(&self) -> bool {
        self.quorum.has_view_change_quorum(self.view_change_count())
    }

    /// Get all prepare voters
    pub fn prepare_voters(&self) -> HashSet<ReplicaId> {
        self.prepare_votes.keys().copied().collect()
    }

    /// Get all commit voters
    pub fn commit_voters(&self) -> HashSet<ReplicaId> {
        self.commit_votes.keys().copied().collect()
    }

    /// Get all view change voters
    pub fn view_change_voters(&self) -> HashSet<ReplicaId> {
        self.view_change_votes.keys().copied().collect()
    }

    /// Check if a replica has voted for prepare
    pub fn has_prepare_vote(&self, replica_id: ReplicaId) -> bool {
        self.prepare_votes.contains_key(&replica_id)
    }

    /// Check if a replica has voted for commit
    pub fn has_commit_vote(&self, replica_id: ReplicaId) -> bool {
        self.commit_votes.contains_key(&replica_id)
    }

    /// Get the view number
    pub fn view(&self) -> ViewNumber {
        self.view
    }

    /// Get the sequence number
    pub fn sequence(&self) -> SequenceNumber {
        self.sequence
    }

    /// Get the digest
    pub fn digest(&self) -> Digest {
        self.digest
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::quorum::QuorumConfig;
    use ed25519_dalek::{Signer, SigningKey};

    fn create_test_setup() -> (
        Vec<(ReplicaId, SigningKey, VerifyingKey)>,
        QuorumCalculator,
        HashMap<ReplicaId, VerifyingKey>,
    ) {
        let keys: Vec<_> = (0..4)
            .map(|i| {
                let signing_key = SigningKey::from_bytes(&[i as u8; 32]);
                let verifying_key = signing_key.verifying_key();
                (i as u64, signing_key, verifying_key)
            })
            .collect();

        let config = QuorumConfig::new(4, 1).unwrap();
        let quorum = QuorumCalculator::new(config);
        let public_keys: HashMap<_, _> = keys.iter().map(|(id, _, pk)| (*id, *pk)).collect();

        (keys, quorum, public_keys)
    }

    fn create_vote(
        vote_type: VoteType,
        view: ViewNumber,
        sequence: SequenceNumber,
        digest: Digest,
        replica_id: ReplicaId,
        signing_key: &SigningKey,
    ) -> Vote {
        let mut content = Vec::new();
        content.extend_from_slice(&[vote_type as u8]);
        content.extend_from_slice(&view.to_le_bytes());
        content.extend_from_slice(&sequence.to_le_bytes());
        content.extend_from_slice(&digest);

        let hash = blake3::hash(&content);
        let signature = signing_key.sign(hash.as_bytes());

        Vote::new(
            vote_type,
            view,
            sequence,
            digest,
            replica_id,
            signature.to_bytes().to_vec(),
        )
    }

    #[test]
    fn test_vote_collector_creation() {
        // Arrange
        let (_, quorum, public_keys) = create_test_setup();
        let view = 0;
        let sequence = 1;
        let digest = [1u8; 32];

        // Act
        let collector = VoteCollector::new(view, sequence, digest, quorum, public_keys);

        // Assert
        assert_eq!(collector.view(), view);
        assert_eq!(collector.sequence(), sequence);
        assert_eq!(collector.digest(), digest);
        assert_eq!(collector.prepare_count(), 0);
        assert_eq!(collector.commit_count(), 0);
    }

    #[test]
    fn test_add_prepare_vote() {
        // Arrange
        let (keys, quorum, public_keys) = create_test_setup();
        let view = 0;
        let sequence = 1;
        let digest = [1u8; 32];
        let mut collector = VoteCollector::new(view, sequence, digest, quorum, public_keys);

        let vote = create_vote(VoteType::Prepare, view, sequence, digest, 0, &keys[0].1);

        // Act
        let result = collector.add_vote(vote);

        // Assert
        assert!(result.is_ok());
        assert_eq!(collector.prepare_count(), 1);
        assert!(collector.has_prepare_vote(0));
    }

    #[test]
    fn test_duplicate_vote_rejected() {
        // Arrange
        let (keys, quorum, public_keys) = create_test_setup();
        let view = 0;
        let sequence = 1;
        let digest = [1u8; 32];
        let mut collector = VoteCollector::new(view, sequence, digest, quorum, public_keys);

        let vote1 = create_vote(VoteType::Prepare, view, sequence, digest, 0, &keys[0].1);
        let vote2 = create_vote(VoteType::Prepare, view, sequence, digest, 0, &keys[0].1);

        // Act
        collector.add_vote(vote1).unwrap();
        let result = collector.add_vote(vote2);

        // Assert
        assert!(matches!(
            result,
            Err(ConsensusError::DuplicateVote { replica_id: 0 })
        ));
    }

    #[test]
    fn test_invalid_view_rejected() {
        // Arrange
        let (keys, quorum, public_keys) = create_test_setup();
        let view = 0;
        let sequence = 1;
        let digest = [1u8; 32];
        let mut collector = VoteCollector::new(view, sequence, digest, quorum, public_keys);

        let vote = create_vote(VoteType::Prepare, 1, sequence, digest, 0, &keys[0].1); // Wrong view

        // Act
        let result = collector.add_vote(vote);

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
    fn test_invalid_sequence_rejected() {
        // Arrange
        let (keys, quorum, public_keys) = create_test_setup();
        let view = 0;
        let sequence = 1;
        let digest = [1u8; 32];
        let mut collector = VoteCollector::new(view, sequence, digest, quorum, public_keys);

        let vote = create_vote(VoteType::Prepare, view, 2, digest, 0, &keys[0].1); // Wrong sequence

        // Act
        let result = collector.add_vote(vote);

        // Assert
        assert!(matches!(
            result,
            Err(ConsensusError::InvalidSequence {
                expected: 1,
                got: 2
            })
        ));
    }

    #[test]
    fn test_prepare_quorum() {
        // Arrange
        let (keys, quorum, public_keys) = create_test_setup();
        let view = 0;
        let sequence = 1;
        let digest = [1u8; 32];
        let mut collector = VoteCollector::new(view, sequence, digest, quorum, public_keys);

        // Act - Add 2 prepare votes (need 2f = 2 for f=1)
        for i in 0..2 {
            let vote = create_vote(VoteType::Prepare, view, sequence, digest, i, &keys[i as usize].1);
            collector.add_vote(vote).unwrap();
        }

        // Assert
        assert!(collector.has_prepare_quorum());
        assert_eq!(collector.prepare_count(), 2);
    }

    #[test]
    fn test_commit_quorum() {
        // Arrange
        let (keys, quorum, public_keys) = create_test_setup();
        let view = 0;
        let sequence = 1;
        let digest = [1u8; 32];
        let mut collector = VoteCollector::new(view, sequence, digest, quorum, public_keys);

        // Act - Add 3 commit votes (need 2f+1 = 3 for f=1)
        for i in 0..3 {
            let vote = create_vote(VoteType::Commit, view, sequence, digest, i, &keys[i as usize].1);
            collector.add_vote(vote).unwrap();
        }

        // Assert
        assert!(collector.has_commit_quorum());
        assert_eq!(collector.commit_count(), 3);
    }

    #[test]
    fn test_mixed_vote_types() {
        // Arrange
        let (keys, quorum, public_keys) = create_test_setup();
        let view = 0;
        let sequence = 1;
        let digest = [1u8; 32];
        let mut collector = VoteCollector::new(view, sequence, digest, quorum, public_keys);

        // Act - Add prepare and commit votes from same replica
        let prepare_vote = create_vote(VoteType::Prepare, view, sequence, digest, 0, &keys[0].1);
        let commit_vote = create_vote(VoteType::Commit, view, sequence, digest, 0, &keys[0].1);

        collector.add_vote(prepare_vote).unwrap();
        collector.add_vote(commit_vote).unwrap();

        // Assert
        assert_eq!(collector.prepare_count(), 1);
        assert_eq!(collector.commit_count(), 1);
        assert!(collector.has_prepare_vote(0));
        assert!(collector.has_commit_vote(0));
    }

    #[test]
    fn test_voter_sets() {
        // Arrange
        let (keys, quorum, public_keys) = create_test_setup();
        let view = 0;
        let sequence = 1;
        let digest = [1u8; 32];
        let mut collector = VoteCollector::new(view, sequence, digest, quorum, public_keys);

        // Act
        for i in 0..3 {
            let prepare_vote = create_vote(VoteType::Prepare, view, sequence, digest, i, &keys[i as usize].1);
            collector.add_vote(prepare_vote).unwrap();
        }

        for i in 1..4 {
            let commit_vote = create_vote(VoteType::Commit, view, sequence, digest, i, &keys[i as usize].1);
            collector.add_vote(commit_vote).unwrap();
        }

        // Assert
        let prepare_voters = collector.prepare_voters();
        let commit_voters = collector.commit_voters();

        assert_eq!(prepare_voters.len(), 3);
        assert!(prepare_voters.contains(&0));
        assert!(prepare_voters.contains(&1));
        assert!(prepare_voters.contains(&2));

        assert_eq!(commit_voters.len(), 3);
        assert!(commit_voters.contains(&1));
        assert!(commit_voters.contains(&2));
        assert!(commit_voters.contains(&3));
    }
}
