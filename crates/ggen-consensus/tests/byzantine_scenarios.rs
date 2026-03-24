//! Byzantine fault injection and tolerance tests

use ed25519_dalek::{Signer, SigningKey, VerifyingKey};
use ggen_consensus::{
    ConsensusMessage, ConsensusProtocol, PbftConfig, PbftConsensus, QuorumCalculator,
    QuorumConfig, ReplicaId, Vote, VoteCollector, VoteType,
};
use std::collections::HashMap;

fn create_pbft_setup(
    replica_id: ReplicaId,
    total: usize,
) -> (PbftConsensus, Vec<SigningKey>, HashMap<ReplicaId, VerifyingKey>) {
    let mut signing_keys = Vec::new();
    let mut public_keys = HashMap::new();

    for i in 0..total {
        let signing_key = SigningKey::from_bytes(&[i as u8; 32]);
        let verifying_key = signing_key.verifying_key();
        public_keys.insert(i as u64, verifying_key);
        signing_keys.push(signing_key);
    }

    let quorum_config = QuorumConfig::new(total, (total - 1) / 3).unwrap();
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
fn test_single_byzantine_replica_signature_forgery() {
    // Arrange - 4 replicas, tolerate 1 Byzantine fault
    let signing_keys: Vec<_> = (0..4)
        .map(|i| SigningKey::from_bytes(&[i as u8; 32]))
        .collect();
    let public_keys: HashMap<_, _> = signing_keys
        .iter()
        .enumerate()
        .map(|(i, sk)| (i as u64, sk.verifying_key()))
        .collect();

    let config = QuorumConfig::new(4, 1).unwrap();
    let quorum = QuorumCalculator::new(config);

    let view = 0;
    let sequence = 1;
    let digest = [1u8; 32];
    let mut collector = VoteCollector::new(view, sequence, digest, quorum, public_keys);

    // Act - Byzantine replica tries to forge signature
    let mut content = Vec::new();
    content.extend_from_slice(&[VoteType::Prepare as u8]);
    content.extend_from_slice(&view.to_le_bytes());
    content.extend_from_slice(&sequence.to_le_bytes());
    content.extend_from_slice(&digest);

    // Replica 0 signs vote claiming to be replica 1
    let hash = blake3::hash(&content);
    let forged_signature = signing_keys[0].sign(hash.as_bytes());

    let vote = Vote::new(
        VoteType::Prepare,
        view,
        sequence,
        digest,
        1, // Claims to be replica 1
        forged_signature.to_bytes().to_vec(),
    );

    let result = collector.add_vote(vote);

    // Assert - Signature verification fails
    assert!(result.is_err());
}

#[test]
fn test_byzantine_replica_double_voting() {
    // Arrange
    let signing_keys: Vec<_> = (0..4)
        .map(|i| SigningKey::from_bytes(&[i as u8; 32]))
        .collect();
    let public_keys: HashMap<_, _> = signing_keys
        .iter()
        .enumerate()
        .map(|(i, sk)| (i as u64, sk.verifying_key()))
        .collect();

    let config = QuorumConfig::new(4, 1).unwrap();
    let quorum = QuorumCalculator::new(config);

    let view = 0;
    let sequence = 1;
    let digest = [1u8; 32];
    let mut collector = VoteCollector::new(view, sequence, digest, quorum, public_keys);

    // Act - Byzantine replica tries to vote twice
    for i in 0..2 {
        let mut content = Vec::new();
        content.extend_from_slice(&[VoteType::Prepare as u8]);
        content.extend_from_slice(&view.to_le_bytes());
        content.extend_from_slice(&sequence.to_le_bytes());
        content.extend_from_slice(&digest);

        let hash = blake3::hash(&content);
        let signature = signing_keys[0].sign(hash.as_bytes());

        let vote = Vote::new(
            VoteType::Prepare,
            view,
            sequence,
            digest,
            0,
            signature.to_bytes().to_vec(),
        );

        let result = collector.add_vote(vote);
        if i == 0 {
            // First vote accepted
            assert!(result.is_ok());
        } else {
            // Second vote rejected
            assert!(result.is_err());
        }
    }

    // Assert - Only one vote counted
    assert_eq!(collector.prepare_count(), 1);
}

#[test]
fn test_byzantine_replica_conflicting_digests() {
    // Arrange - Byzantine replica tries to vote for different digests
    let signing_keys: Vec<_> = (0..4)
        .map(|i| SigningKey::from_bytes(&[i as u8; 32]))
        .collect();
    let public_keys: HashMap<_, _> = signing_keys
        .iter()
        .enumerate()
        .map(|(i, sk)| (i as u64, sk.verifying_key()))
        .collect();

    let config = QuorumConfig::new(4, 1).unwrap();
    let quorum = QuorumCalculator::new(config);

    let view = 0;
    let sequence = 1;
    let digest1 = [1u8; 32];
    let digest2 = [2u8; 32];

    let mut collector1 = VoteCollector::new(view, sequence, digest1, quorum.clone(), public_keys.clone());
    let mut collector2 = VoteCollector::new(view, sequence, digest2, quorum, public_keys);

    // Act - Byzantine replica votes for both digests
    for (collector, digest) in [(&mut collector1, digest1), (&mut collector2, digest2)] {
        let mut content = Vec::new();
        content.extend_from_slice(&[VoteType::Prepare as u8]);
        content.extend_from_slice(&view.to_le_bytes());
        content.extend_from_slice(&sequence.to_le_bytes());
        content.extend_from_slice(&digest);

        let hash = blake3::hash(&content);
        let signature = signing_keys[0].sign(hash.as_bytes());

        let vote = Vote::new(VoteType::Prepare, view, sequence, digest, 0, signature.to_bytes().to_vec());
        collector.add_vote(vote).unwrap();
    }

    // Assert - Both collectors see the vote, but honest replicas would detect equivocation
    assert_eq!(collector1.prepare_count(), 1);
    assert_eq!(collector2.prepare_count(), 1);
}

#[test]
fn test_tolerate_f_byzantine_with_honest_quorum() {
    // Arrange - 7 replicas, tolerate 2 Byzantine faults
    let signing_keys: Vec<_> = (0..7)
        .map(|i| SigningKey::from_bytes(&[i as u8; 32]))
        .collect();
    let public_keys: HashMap<_, _> = signing_keys
        .iter()
        .enumerate()
        .map(|(i, sk)| (i as u64, sk.verifying_key()))
        .collect();

    let config = QuorumConfig::new(7, 2).unwrap();
    let quorum = QuorumCalculator::new(config);

    let view = 0;
    let sequence = 1;
    let digest = [1u8; 32];
    let mut collector = VoteCollector::new(view, sequence, digest, quorum.clone(), public_keys);

    // Act - 5 honest replicas vote (2f+1 = 5)
    for i in 0..5 {
        let mut content = Vec::new();
        content.extend_from_slice(&[VoteType::Prepare as u8]);
        content.extend_from_slice(&view.to_le_bytes());
        content.extend_from_slice(&sequence.to_le_bytes());
        content.extend_from_slice(&digest);

        let hash = blake3::hash(&content);
        let signature = signing_keys[i].sign(hash.as_bytes());

        let vote = Vote::new(VoteType::Prepare, view, sequence, digest, i as u64, signature.to_bytes().to_vec());
        collector.add_vote(vote).unwrap();
    }

    // Assert - Quorum reached even with 2 Byzantine replicas not voting
    assert!(collector.has_prepare_quorum());
    assert_eq!(quorum.quorum_size(), 5); // 2*2+1
}

#[test]
fn test_byzantine_primary_wrong_digest() {
    // Arrange - Byzantine primary sends wrong digest
    let (mut pbft, _, _) = create_pbft_setup(1, 4); // Non-primary replica

    let message = b"correct message";
    let wrong_digest = [0u8; 32]; // Byzantine primary sends wrong digest

    // Act - Receive pre-prepare with wrong digest
    let pre_prepare = ConsensusMessage::PrePrepare {
        view: 0,
        sequence: 0,
        digest: wrong_digest,
        message: message.to_vec(),
    };

    let result = pbft.process_message(pre_prepare);

    // Assert - Detected and rejected
    assert!(result.is_err());
}

#[test]
fn test_view_change_with_byzantine_primary() {
    // Arrange - Primary is Byzantine (doesn't respond)
    let (mut pbft, _, _) = create_pbft_setup(1, 4); // Replica 1

    // Act - Detect timeout and initiate view change
    let old_view = pbft.current_view();
    pbft.change_view().unwrap();

    // Assert - New primary selected
    assert_eq!(pbft.current_view(), old_view + 1);
}

#[test]
fn test_quorum_prevents_byzantine_minority() {
    // Arrange - 10 replicas, 3 Byzantine (f=3)
    let config = QuorumConfig::new(10, 3).unwrap();
    let quorum = QuorumCalculator::new(config);

    // Act - Only 6 votes (less than 2f+1=7)
    let has_quorum = quorum.has_quorum(6);

    // Assert - No quorum with 3 Byzantine not voting
    assert!(!has_quorum);

    // Act - 7 votes (exactly 2f+1)
    let has_quorum = quorum.has_quorum(7);

    // Assert - Quorum reached, guarantees 4 honest votes (more than f=3)
    assert!(has_quorum);
}

#[test]
fn test_byzantine_replicas_cannot_form_quorum_alone() {
    // Arrange - 7 replicas, 2 Byzantine (f=2)
    let config = QuorumConfig::new(7, 2).unwrap();
    let quorum = QuorumCalculator::new(config);

    // Act - Only f=2 Byzantine replicas vote
    let byzantine_votes = 2;
    let has_quorum = quorum.has_quorum(byzantine_votes);

    // Assert - Cannot reach quorum (need 2f+1=5)
    assert!(!has_quorum);
}

#[test]
fn test_network_partition_no_quorum() {
    // Arrange - 4 replicas, network partition isolates 1 replica
    let config = QuorumConfig::new(4, 1).unwrap();
    let quorum = QuorumCalculator::new(config);

    // Act - Isolated replica only has 1 vote (itself)
    let isolated_votes = 1;

    // Assert - Cannot reach quorum (need 2f+1=3)
    assert!(!quorum.has_quorum(isolated_votes));
}

#[test]
fn test_majority_partition_reaches_quorum() {
    // Arrange - 7 replicas, network partition splits into 5 and 2
    let config = QuorumConfig::new(7, 2).unwrap();
    let quorum = QuorumCalculator::new(config);

    // Act - Majority partition has 5 replicas
    let majority_votes = 5;

    // Assert - Majority can reach quorum (need 2f+1=5)
    assert!(quorum.has_quorum(majority_votes));
}

#[test]
fn test_byzantine_replica_sends_invalid_view() {
    // Arrange
    let (mut pbft, _, _) = create_pbft_setup(1, 4);

    let message = b"test message";
    let digest = blake3::hash(message).into();

    // Act - Byzantine replica sends message for future view
    let invalid_pre_prepare = ConsensusMessage::PrePrepare {
        view: 99, // Invalid future view
        sequence: 0,
        digest,
        message: message.to_vec(),
    };

    let result = pbft.process_message(invalid_pre_prepare);

    // Assert - Rejected
    assert!(result.is_err());
}

#[test]
fn test_safety_with_f_byzantine_failures() {
    // Arrange - 4 replicas, 1 Byzantine (f=1)
    let signing_keys: Vec<_> = (0..4)
        .map(|i| SigningKey::from_bytes(&[i as u8; 32]))
        .collect();
    let public_keys: HashMap<_, _> = signing_keys
        .iter()
        .enumerate()
        .map(|(i, sk)| (i as u64, sk.verifying_key()))
        .collect();

    let config = QuorumConfig::new(4, 1).unwrap();
    let quorum = QuorumCalculator::new(config);

    let view = 0;
    let sequence = 1;
    let digest = [1u8; 32];
    let mut collector = VoteCollector::new(view, sequence, digest, quorum, public_keys);

    // Act - 3 honest replicas vote (2f+1 = 3), 1 Byzantine doesn't
    for i in 0..3 {
        let mut content = Vec::new();
        content.extend_from_slice(&[VoteType::Commit as u8]);
        content.extend_from_slice(&view.to_le_bytes());
        content.extend_from_slice(&sequence.to_le_bytes());
        content.extend_from_slice(&digest);

        let hash = blake3::hash(&content);
        let signature = signing_keys[i].sign(hash.as_bytes());

        let vote = Vote::new(VoteType::Commit, view, sequence, digest, i as u64, signature.to_bytes().to_vec());
        collector.add_vote(vote).unwrap();
    }

    // Assert - Safety guaranteed: at least f+1=2 honest replicas in quorum
    assert!(collector.has_commit_quorum());
    assert_eq!(collector.commit_count(), 3);
}

#[test]
fn test_liveness_under_asynchrony() {
    // Arrange - Simulate slow network (messages arrive eventually)
    let (mut pbft, _, _) = create_pbft_setup(0, 4);

    // Act - Primary proposes
    let message = b"test";
    let digest = pbft.propose(message).unwrap();

    // Assert - Request tracked even if messages delayed
    assert!(pbft.get_phase(&digest).is_some());
}

#[test]
fn test_maximum_byzantine_tolerance() {
    // Test boundary: exactly 3f+1 replicas
    let scenarios = vec![
        (4, 1),   // Minimum: 4 replicas, 1 Byzantine
        (7, 2),   // 7 replicas, 2 Byzantine
        (10, 3),  // 10 replicas, 3 Byzantine
        (100, 33), // Large: 100 replicas, 33 Byzantine
    ];

    for (replicas, faults) in scenarios {
        // Arrange
        let config = QuorumConfig::new(replicas, faults).unwrap();
        let quorum = QuorumCalculator::new(config);

        // Assert - Can tolerate exactly f faults
        let quorum_size = quorum.quorum_size();
        let honest_min = replicas - faults;

        // With quorum votes, at least f+1 must be honest
        assert!(honest_min >= quorum_size);
        assert_eq!(quorum_size, 2 * faults + 1);
    }
}
