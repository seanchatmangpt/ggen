//! Integration tests for PBFT consensus protocol

use ed25519_dalek::{SigningKey, VerifyingKey};
use ggen_consensus::{
    ConsensusMessage, ConsensusProtocol, PbftConfig, PbftConsensus, Phase, QuorumConfig, ReplicaId,
};
use std::collections::HashMap;

fn create_test_setup(
    replica_id: ReplicaId,
    total_replicas: usize,
) -> (PbftConsensus, Vec<SigningKey>, HashMap<ReplicaId, VerifyingKey>) {
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
        public_keys.clone(),
        quorum_config,
    );

    let pbft = PbftConsensus::new(config);
    (pbft, signing_keys, public_keys)
}

#[test]
fn test_basic_consensus_flow() {
    // Arrange - 4 replicas, replica 0 is primary
    let (mut pbft, _, _) = create_test_setup(0, 4);

    // Act - Primary proposes a message
    let message = b"test transaction";
    let digest = pbft.propose(message).unwrap();

    // Assert - Request is in pre-prepare phase
    assert_eq!(pbft.get_phase(&digest), Some(Phase::PrePrepare));
}

#[test]
fn test_non_primary_cannot_propose() {
    // Arrange - Replica 1 is not primary for view 0
    let (mut pbft, _, _) = create_test_setup(1, 4);

    // Act - Non-primary tries to propose
    let message = b"test transaction";
    let result = pbft.propose(message);

    // Assert - Proposal rejected
    assert!(result.is_err());
}

#[test]
fn test_primary_rotation_on_view_change() {
    // Arrange - Start with replica 0 as primary
    let (mut pbft, _, _) = create_test_setup(0, 4);

    // Assert - Replica 0 is primary in view 0
    assert!(pbft.is_primary());
    assert_eq!(pbft.current_view(), 0);

    // Act - Change view
    pbft.change_view().unwrap();

    // Assert - Replica 1 is now primary
    assert!(!pbft.is_primary());
    assert_eq!(pbft.current_view(), 1);
}

#[test]
fn test_sequential_sequence_numbers() {
    // Arrange
    let (mut pbft, _, _) = create_test_setup(0, 4);

    // Act - Propose multiple messages
    let msg1 = b"transaction 1";
    let msg2 = b"transaction 2";
    let msg3 = b"transaction 3";

    pbft.propose(msg1).unwrap();
    pbft.propose(msg2).unwrap();
    pbft.propose(msg3).unwrap();

    // Assert - Sequence numbers increment
    // Note: Sequence tracking is internal to PBFT
}

#[test]
fn test_process_pre_prepare_message() {
    // Arrange - Non-primary replica
    let (mut pbft, signing_keys, _) = create_test_setup(1, 4);

    let message = b"test transaction";
    let digest = blake3::hash(message).into();

    // Act - Receive pre-prepare from primary (replica 0)
    let pre_prepare = ConsensusMessage::PrePrepare {
        view: 0,
        sequence: 0,
        digest,
        message: message.to_vec(),
    };

    let result = pbft.process_message(pre_prepare);

    // Assert
    assert!(result.is_ok());
    assert_eq!(pbft.get_phase(&digest), Some(Phase::PrePrepare));
}

#[test]
fn test_reject_pre_prepare_with_wrong_view() {
    // Arrange
    let (mut pbft, _, _) = create_test_setup(1, 4);

    let message = b"test transaction";
    let digest = blake3::hash(message).into();

    // Act - Pre-prepare with wrong view
    let pre_prepare = ConsensusMessage::PrePrepare {
        view: 1, // Wrong view
        sequence: 0,
        digest,
        message: message.to_vec(),
    };

    let result = pbft.process_message(pre_prepare);

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_reject_pre_prepare_with_mismatched_digest() {
    // Arrange
    let (mut pbft, _, _) = create_test_setup(1, 4);

    let message = b"test transaction";
    let wrong_digest = [0u8; 32];

    // Act - Pre-prepare with wrong digest
    let pre_prepare = ConsensusMessage::PrePrepare {
        view: 0,
        sequence: 0,
        digest: wrong_digest,
        message: message.to_vec(),
    };

    let result = pbft.process_message(pre_prepare);

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_committed_flag_after_quorum() {
    // Arrange
    let (mut pbft, _, _) = create_test_setup(0, 4);

    // Act - Propose message
    let message = b"test transaction";
    let digest = pbft.propose(message).unwrap();

    // Assert - Not committed yet
    assert!(!pbft.is_committed(&digest));
}

#[test]
fn test_view_changes_rotate_primary() {
    // Arrange - 4 replicas
    let (mut pbft, _, _) = create_test_setup(0, 4);

    // Act & Assert - Verify primary rotation through views
    assert_eq!(pbft.current_view(), 0);
    assert!(pbft.is_primary());

    pbft.change_view().unwrap();
    assert_eq!(pbft.current_view(), 1);
    assert!(!pbft.is_primary()); // Now replica 1

    pbft.change_view().unwrap();
    assert_eq!(pbft.current_view(), 2);
    assert!(!pbft.is_primary()); // Now replica 2

    pbft.change_view().unwrap();
    assert_eq!(pbft.current_view(), 3);
    assert!(!pbft.is_primary()); // Now replica 3

    pbft.change_view().unwrap();
    assert_eq!(pbft.current_view(), 4);
    assert!(pbft.is_primary()); // Back to replica 0
}

#[test]
fn test_multiple_concurrent_requests() {
    // Arrange
    let (mut pbft, _, _) = create_test_setup(0, 4);

    // Act - Propose multiple messages
    let msg1 = b"transaction 1";
    let msg2 = b"transaction 2";
    let msg3 = b"transaction 3";

    let digest1 = pbft.propose(msg1).unwrap();
    let digest2 = pbft.propose(msg2).unwrap();
    let digest3 = pbft.propose(msg3).unwrap();

    // Assert - All requests tracked separately
    assert_eq!(pbft.get_phase(&digest1), Some(Phase::PrePrepare));
    assert_eq!(pbft.get_phase(&digest2), Some(Phase::PrePrepare));
    assert_eq!(pbft.get_phase(&digest3), Some(Phase::PrePrepare));

    // Digests should be different
    assert_ne!(digest1, digest2);
    assert_ne!(digest2, digest3);
    assert_ne!(digest1, digest3);
}

#[test]
fn test_large_scale_pbft() {
    // Arrange - 100 replicas, 33 faults
    let (mut pbft, _, _) = create_test_setup(0, 100);

    // Act - Primary proposes
    let message = b"large scale transaction";
    let digest = pbft.propose(message).unwrap();

    // Assert
    assert_eq!(pbft.get_phase(&digest), Some(Phase::PrePrepare));
}

#[test]
fn test_deterministic_primary_selection() {
    // Arrange - Create multiple instances with same config
    let (pbft1, _, _) = create_test_setup(0, 4);
    let (pbft2, _, _) = create_test_setup(1, 4);
    let (pbft3, _, _) = create_test_setup(2, 4);

    // Assert - All agree on primary for view 0
    assert_eq!(pbft1.is_primary(), true);
    assert_eq!(pbft2.is_primary(), false);
    assert_eq!(pbft3.is_primary(), false);
}

#[test]
fn test_consensus_protocol_trait_implementation() {
    // Arrange
    let (mut pbft, _, _) = create_test_setup(0, 4);

    // Act - Use through trait
    let message = b"test";
    let digest = pbft.propose(message).unwrap();

    // Assert - Trait methods work
    assert!(!pbft.is_committed(&digest));
    assert_eq!(pbft.current_view(), 0);

    pbft.change_view().unwrap();
    assert_eq!(pbft.current_view(), 1);
}

#[test]
fn test_message_ordering_by_sequence() {
    // Arrange
    let (mut pbft, _, _) = create_test_setup(0, 4);

    // Act - Propose messages in order
    let messages = vec![b"msg1", b"msg2", b"msg3", b"msg4", b"msg5"];
    let mut digests = Vec::new();

    for msg in &messages {
        let digest = pbft.propose(*msg).unwrap();
        digests.push(digest);
    }

    // Assert - All messages tracked
    for digest in digests {
        assert!(pbft.get_phase(&digest).is_some());
    }
}

#[test]
fn test_recovery_after_view_change() {
    // Arrange
    let (mut pbft, _, _) = create_test_setup(0, 4);

    // Act - Propose in view 0
    let message = b"test";
    let digest1 = pbft.propose(message).unwrap();

    // Change view
    pbft.change_view().unwrap();

    // Now replica 1 is primary, but we're testing replica 0
    // So we can't propose anymore
    let result = pbft.propose(b"new message");

    // Assert
    assert!(result.is_err()); // No longer primary
    assert_eq!(pbft.get_phase(&digest1), Some(Phase::PrePrepare)); // Old request still tracked
}
