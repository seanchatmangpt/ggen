//! Byzantine consensus for receipt verification
//!
//! Implements PBFT (Practical Byzantine Fault Tolerance) for distributed receipt verification.
//! Supports 3f+1 replicas tolerating f Byzantine faults.

pub mod error;
pub mod pbft;
pub mod quorum;
pub mod signatures;
pub mod voting;

pub use error::{ConsensusError, Result};
pub use pbft::{PbftConfig, PbftConsensus, Phase};
pub use quorum::{QuorumCalculator, QuorumConfig};
pub use signatures::{MultiSignature, SignatureAggregator};
pub use voting::{Vote, VoteCollector, VoteType};

use ed25519_dalek::VerifyingKey;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Unique identifier for a replica in the consensus network
pub type ReplicaId = u64;

/// Hash digest of consensus messages
pub type Digest = [u8; 32];

/// View number for PBFT protocol
pub type ViewNumber = u64;

/// Sequence number for ordered messages
pub type SequenceNumber = u64;

/// Consensus protocol trait
pub trait ConsensusProtocol {
    /// Start a new consensus round for the given message
    fn propose(&mut self, message: &[u8]) -> Result<Digest>;

    /// Process an incoming message
    fn process_message(&mut self, message: ConsensusMessage) -> Result<()>;

    /// Check if consensus has been reached
    fn is_committed(&self, digest: &Digest) -> bool;

    /// Get the current view number
    fn current_view(&self) -> ViewNumber;

    /// Trigger view change
    fn change_view(&mut self) -> Result<ViewNumber>;
}

/// Replica information
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Replica {
    /// Unique identifier
    pub id: ReplicaId,
    /// Public key for signature verification
    pub public_key: VerifyingKey,
    /// Network address (for future use)
    pub address: String,
}

/// Consensus message types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ConsensusMessage {
    /// Pre-prepare message from primary
    PrePrepare {
        view: ViewNumber,
        sequence: SequenceNumber,
        digest: Digest,
        message: Vec<u8>,
    },
    /// Prepare message from replicas
    Prepare {
        view: ViewNumber,
        sequence: SequenceNumber,
        digest: Digest,
        replica_id: ReplicaId,
        signature: Vec<u8>,
    },
    /// Commit message from replicas
    Commit {
        view: ViewNumber,
        sequence: SequenceNumber,
        digest: Digest,
        replica_id: ReplicaId,
        signature: Vec<u8>,
    },
    /// View change request
    ViewChange {
        new_view: ViewNumber,
        replica_id: ReplicaId,
        signature: Vec<u8>,
    },
    /// New view message from new primary
    NewView {
        view: ViewNumber,
        view_change_messages: Vec<ConsensusMessage>,
        signature: Vec<u8>,
    },
}

impl ConsensusMessage {
    /// Get the digest of this message
    pub fn digest(&self) -> Digest {
        let bytes = serde_json::to_vec(self).unwrap_or_default();
        blake3::hash(&bytes).into()
    }

    /// Get the replica ID if applicable
    pub fn replica_id(&self) -> Option<ReplicaId> {
        match self {
            ConsensusMessage::Prepare { replica_id, .. }
            | ConsensusMessage::Commit { replica_id, .. }
            | ConsensusMessage::ViewChange { replica_id, .. } => Some(*replica_id),
            _ => None,
        }
    }

    /// Get the view number
    pub fn view(&self) -> ViewNumber {
        match self {
            ConsensusMessage::PrePrepare { view, .. }
            | ConsensusMessage::Prepare { view, .. }
            | ConsensusMessage::Commit { view, .. }
            | ConsensusMessage::ViewChange { new_view: view, .. }
            | ConsensusMessage::NewView { view, .. } => *view,
        }
    }
}

/// Network configuration for consensus
#[derive(Debug, Clone)]
pub struct ConsensusConfig {
    /// All replicas in the network
    pub replicas: HashMap<ReplicaId, Replica>,
    /// Maximum Byzantine faults tolerated (f)
    pub max_faults: usize,
    /// Timeout for consensus rounds (milliseconds)
    pub timeout_ms: u64,
}

impl ConsensusConfig {
    /// Create a new consensus configuration
    pub fn new(replicas: Vec<Replica>, max_faults: usize) -> Result<Self> {
        let total_replicas = replicas.len();
        let min_replicas = 3 * max_faults + 1;

        if total_replicas < min_replicas {
            return Err(ConsensusError::InsufficientReplicas {
                got: total_replicas,
                needed: min_replicas,
            });
        }

        let replicas_map = replicas.into_iter().map(|r| (r.id, r)).collect();

        Ok(Self {
            replicas: replicas_map,
            max_faults,
            timeout_ms: 5000,
        })
    }

    /// Get total number of replicas
    pub fn total_replicas(&self) -> usize {
        self.replicas.len()
    }

    /// Calculate primary replica ID for a given view
    pub fn primary_for_view(&self, view: ViewNumber) -> ReplicaId {
        let mut replica_ids: Vec<_> = self.replicas.keys().copied().collect();
        replica_ids.sort_unstable();
        replica_ids[(view as usize) % replica_ids.len()]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ed25519_dalek::SigningKey;

    fn create_test_replica(id: ReplicaId) -> Replica {
        let signing_key = SigningKey::from_bytes(&[id as u8; 32]);
        Replica {
            id,
            public_key: signing_key.verifying_key(),
            address: format!("replica-{}", id),
        }
    }

    #[test]
    fn test_consensus_config_valid() {
        // Arrange
        let replicas = vec![
            create_test_replica(0),
            create_test_replica(1),
            create_test_replica(2),
            create_test_replica(3),
        ];

        // Act
        let config = ConsensusConfig::new(replicas, 1);

        // Assert
        assert!(config.is_ok());
        let config = config.unwrap();
        assert_eq!(config.total_replicas(), 4);
        assert_eq!(config.max_faults, 1);
    }

    #[test]
    fn test_consensus_config_insufficient_replicas() {
        // Arrange
        let replicas = vec![create_test_replica(0), create_test_replica(1)];

        // Act
        let result = ConsensusConfig::new(replicas, 1);

        // Assert
        assert!(matches!(
            result,
            Err(ConsensusError::InsufficientReplicas { got: 2, needed: 4 })
        ));
    }

    #[test]
    fn test_primary_rotation() {
        // Arrange
        let replicas = vec![
            create_test_replica(0),
            create_test_replica(1),
            create_test_replica(2),
            create_test_replica(3),
        ];
        let config = ConsensusConfig::new(replicas, 1).unwrap();

        // Act & Assert
        assert_eq!(config.primary_for_view(0), 0);
        assert_eq!(config.primary_for_view(1), 1);
        assert_eq!(config.primary_for_view(2), 2);
        assert_eq!(config.primary_for_view(3), 3);
        assert_eq!(config.primary_for_view(4), 0); // Wraps around
    }

    #[test]
    fn test_message_digest() {
        // Arrange
        let msg1 = ConsensusMessage::Prepare {
            view: 0,
            sequence: 1,
            digest: [0u8; 32],
            replica_id: 1,
            signature: vec![1, 2, 3],
        };

        let msg2 = ConsensusMessage::Prepare {
            view: 0,
            sequence: 1,
            digest: [0u8; 32],
            replica_id: 1,
            signature: vec![1, 2, 3],
        };

        let msg3 = ConsensusMessage::Prepare {
            view: 0,
            sequence: 2, // Different sequence
            digest: [0u8; 32],
            replica_id: 1,
            signature: vec![1, 2, 3],
        };

        // Act
        let digest1 = msg1.digest();
        let digest2 = msg2.digest();
        let digest3 = msg3.digest();

        // Assert
        assert_eq!(digest1, digest2);
        assert_ne!(digest1, digest3);
    }
}
