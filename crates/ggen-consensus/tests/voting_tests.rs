//! Integration tests for vote collection and verification

use ed25519_dalek::{Signer, SigningKey, VerifyingKey};
use ggen_consensus::{QuorumCalculator, QuorumConfig, ReplicaId, Vote, VoteCollector, VoteType};
use std::collections::HashMap;

fn create_test_keys(count: usize) -> Vec<(ReplicaId, SigningKey, VerifyingKey)> {
    (0..count)
        .map(|i| {
            let signing_key = SigningKey::from_bytes(&[i as u8; 32]);
            let verifying_key = signing_key.verifying_key();
            (i as u64, signing_key, verifying_key)
        })
        .collect()
}

fn create_vote(
    vote_type: VoteType,
    view: u64,
    sequence: u64,
    digest: [u8; 32],
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
fn test_vote_collection_basic() {
    // Arrange
    let keys = create_test_keys(4);
    let config = QuorumConfig::new(4, 1).unwrap();
    let quorum = QuorumCalculator::new(config);
    let public_keys: HashMap<_, _> = keys.iter().map(|(id, _, pk)| (*id, *pk)).collect();

    let view = 0;
    let sequence = 1;
    let digest = [1u8; 32];
    let mut collector = VoteCollector::new(view, sequence, digest, quorum, public_keys);

    // Act - Add prepare votes
    for i in 0..2 {
        let vote = create_vote(VoteType::Prepare, view, sequence, digest, i, &keys[i as usize].1);
        collector.add_vote(vote).unwrap();
    }

    // Assert
    assert_eq!(collector.prepare_count(), 2);
    assert!(collector.has_prepare_quorum());
}

#[test]
fn test_separate_prepare_and_commit_votes() {
    // Arrange
    let keys = create_test_keys(4);
    let config = QuorumConfig::new(4, 1).unwrap();
    let quorum = QuorumCalculator::new(config);
    let public_keys: HashMap<_, _> = keys.iter().map(|(id, _, pk)| (*id, *pk)).collect();

    let view = 0;
    let sequence = 1;
    let digest = [1u8; 32];
    let mut collector = VoteCollector::new(view, sequence, digest, quorum, public_keys);

    // Act - Same replica can vote in different phases
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
fn test_duplicate_vote_detection() {
    // Arrange
    let keys = create_test_keys(4);
    let config = QuorumConfig::new(4, 1).unwrap();
    let quorum = QuorumCalculator::new(config);
    let public_keys: HashMap<_, _> = keys.iter().map(|(id, _, pk)| (*id, *pk)).collect();

    let view = 0;
    let sequence = 1;
    let digest = [1u8; 32];
    let mut collector = VoteCollector::new(view, sequence, digest, quorum, public_keys);

    // Act - Try to add same vote twice
    let vote1 = create_vote(VoteType::Prepare, view, sequence, digest, 0, &keys[0].1);
    let vote2 = create_vote(VoteType::Prepare, view, sequence, digest, 0, &keys[0].1);

    collector.add_vote(vote1).unwrap();
    let result = collector.add_vote(vote2);

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_invalid_signature_rejected() {
    // Arrange
    let keys = create_test_keys(4);
    let config = QuorumConfig::new(4, 1).unwrap();
    let quorum = QuorumCalculator::new(config);
    let public_keys: HashMap<_, _> = keys.iter().map(|(id, _, pk)| (*id, *pk)).collect();

    let view = 0;
    let sequence = 1;
    let digest = [1u8; 32];
    let mut collector = VoteCollector::new(view, sequence, digest, quorum, public_keys);

    // Act - Create vote signed by wrong key
    let vote = create_vote(VoteType::Prepare, view, sequence, digest, 0, &keys[1].1); // Wrong key
    let result = collector.add_vote(vote);

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_view_mismatch_rejected() {
    // Arrange
    let keys = create_test_keys(4);
    let config = QuorumConfig::new(4, 1).unwrap();
    let quorum = QuorumCalculator::new(config);
    let public_keys: HashMap<_, _> = keys.iter().map(|(id, _, pk)| (*id, *pk)).collect();

    let view = 0;
    let sequence = 1;
    let digest = [1u8; 32];
    let mut collector = VoteCollector::new(view, sequence, digest, quorum, public_keys);

    // Act - Vote with different view
    let vote = create_vote(VoteType::Prepare, 1, sequence, digest, 0, &keys[0].1); // Wrong view
    let result = collector.add_vote(vote);

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_sequence_mismatch_rejected() {
    // Arrange
    let keys = create_test_keys(4);
    let config = QuorumConfig::new(4, 1).unwrap();
    let quorum = QuorumCalculator::new(config);
    let public_keys: HashMap<_, _> = keys.iter().map(|(id, _, pk)| (*id, *pk)).collect();

    let view = 0;
    let sequence = 1;
    let digest = [1u8; 32];
    let mut collector = VoteCollector::new(view, sequence, digest, quorum, public_keys);

    // Act - Vote with different sequence
    let vote = create_vote(VoteType::Prepare, view, 2, digest, 0, &keys[0].1); // Wrong sequence
    let result = collector.add_vote(vote);

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_digest_mismatch_rejected() {
    // Arrange
    let keys = create_test_keys(4);
    let config = QuorumConfig::new(4, 1).unwrap();
    let quorum = QuorumCalculator::new(config);
    let public_keys: HashMap<_, _> = keys.iter().map(|(id, _, pk)| (*id, *pk)).collect();

    let view = 0;
    let sequence = 1;
    let digest = [1u8; 32];
    let mut collector = VoteCollector::new(view, sequence, digest, quorum, public_keys);

    // Act - Vote with different digest
    let wrong_digest = [2u8; 32];
    let vote = create_vote(VoteType::Prepare, view, sequence, wrong_digest, 0, &keys[0].1);
    let result = collector.add_vote(vote);

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_commit_quorum_requirement() {
    // Arrange
    let keys = create_test_keys(4);
    let config = QuorumConfig::new(4, 1).unwrap();
    let quorum = QuorumCalculator::new(config);
    let public_keys: HashMap<_, _> = keys.iter().map(|(id, _, pk)| (*id, *pk)).collect();

    let view = 0;
    let sequence = 1;
    let digest = [1u8; 32];
    let mut collector = VoteCollector::new(view, sequence, digest, quorum, public_keys);

    // Act - Add commit votes one by one
    assert!(!collector.has_commit_quorum());

    collector
        .add_vote(create_vote(VoteType::Commit, view, sequence, digest, 0, &keys[0].1))
        .unwrap();
    assert!(!collector.has_commit_quorum()); // 1 vote, need 3

    collector
        .add_vote(create_vote(VoteType::Commit, view, sequence, digest, 1, &keys[1].1))
        .unwrap();
    assert!(!collector.has_commit_quorum()); // 2 votes, need 3

    collector
        .add_vote(create_vote(VoteType::Commit, view, sequence, digest, 2, &keys[2].1))
        .unwrap();
    assert!(collector.has_commit_quorum()); // 3 votes, quorum reached
}

#[test]
fn test_voter_tracking() {
    // Arrange
    let keys = create_test_keys(4);
    let config = QuorumConfig::new(4, 1).unwrap();
    let quorum = QuorumCalculator::new(config);
    let public_keys: HashMap<_, _> = keys.iter().map(|(id, _, pk)| (*id, *pk)).collect();

    let view = 0;
    let sequence = 1;
    let digest = [1u8; 32];
    let mut collector = VoteCollector::new(view, sequence, digest, quorum, public_keys);

    // Act - Add votes from different replicas
    collector
        .add_vote(create_vote(VoteType::Prepare, view, sequence, digest, 0, &keys[0].1))
        .unwrap();
    collector
        .add_vote(create_vote(VoteType::Prepare, view, sequence, digest, 2, &keys[2].1))
        .unwrap();
    collector
        .add_vote(create_vote(VoteType::Commit, view, sequence, digest, 1, &keys[1].1))
        .unwrap();

    // Assert
    let prepare_voters = collector.prepare_voters();
    let commit_voters = collector.commit_voters();

    assert_eq!(prepare_voters.len(), 2);
    assert!(prepare_voters.contains(&0));
    assert!(prepare_voters.contains(&2));

    assert_eq!(commit_voters.len(), 1);
    assert!(commit_voters.contains(&1));
}

#[test]
fn test_large_scale_voting() {
    // Arrange - Large scale: 100 replicas, 33 faults
    let keys = create_test_keys(100);
    let config = QuorumConfig::new(100, 33).unwrap();
    let quorum = QuorumCalculator::new(config);
    let public_keys: HashMap<_, _> = keys.iter().map(|(id, _, pk)| (*id, *pk)).collect();

    let view = 0;
    let sequence = 1;
    let digest = [1u8; 32];
    let mut collector = VoteCollector::new(view, sequence, digest, quorum, public_keys);

    // Act - Add 67 prepare votes (2f+1)
    for i in 0..67 {
        let vote = create_vote(VoteType::Prepare, view, sequence, digest, i, &keys[i as usize].1);
        collector.add_vote(vote).unwrap();
    }

    // Assert
    assert_eq!(collector.prepare_count(), 67);
    assert!(collector.has_prepare_quorum());
}

#[test]
fn test_view_change_voting() {
    // Arrange
    let keys = create_test_keys(7);
    let config = QuorumConfig::new(7, 2).unwrap();
    let quorum = QuorumCalculator::new(config);
    let public_keys: HashMap<_, _> = keys.iter().map(|(id, _, pk)| (*id, *pk)).collect();

    let view = 0;
    let sequence = 1;
    let digest = [1u8; 32];
    let mut collector = VoteCollector::new(view, sequence, digest, quorum, public_keys);

    // Act - Add view change votes (need 2f+1 = 5)
    for i in 0..5 {
        let vote = create_vote(VoteType::ViewChange, view, sequence, digest, i, &keys[i as usize].1);
        collector.add_vote(vote).unwrap();
    }

    // Assert
    assert_eq!(collector.view_change_count(), 5);
    assert!(collector.has_view_change_quorum());
}
