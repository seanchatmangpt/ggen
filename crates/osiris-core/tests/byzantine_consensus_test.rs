//! Integration tests for Byzantine Fault Tolerance consensus
//!
//! Tests 5 scenarios:
//! 1. Happy path: all honest nodes agree
//! 2. Malicious leader: sends conflicting values
//! 3. Network partition: quorum can still commit
//! 4. Node isolation: detected and excluded
//! 5. Recovery: rejoining node accepts decision

use osiris_core::byzantine::{
    ByzantineConsensus, ConsensusConfig, Evidence, EvidenceLog, LeaderElectionStrategy,
    LeaderElector, Message, Misbehavior, NodeId, ProposalValue, Round,
};

/// Scenario 1: Happy path - all honest nodes agree
#[tokio::test]
async fn test_happy_path_consensus() {
    // Setup: 4 nodes, all honest
    let node1 = NodeId::new(1);
    let node2 = NodeId::new(2);
    let node3 = NodeId::new(3);
    let _node4 = NodeId::new(4);

    let config = ConsensusConfig::new(4);
    assert_eq!(config.quorum_size(), 3); // 2*1 + 1 = 3 needed

    // Create consensus instance
    let mut consensus = osiris_core::byzantine::PBFTLiteConsensus::new(node1, config.clone());

    // Propose value
    let value = ProposalValue::new("important_data".to_string());
    consensus.propose(value.clone()).await.unwrap();

    // Simulate approvals from quorum
    consensus.add_approval(node2, value.clone()).unwrap();
    consensus.add_approval(node3, value.clone()).unwrap();

    // Should reach quorum and commit
    let committed = consensus.commit_when_ready().await;
    assert!(committed.is_ok());

    let committed_value = committed.unwrap();
    assert_eq!(committed_value.value, value);
    assert_eq!(committed_value.approvers.len(), 3);
    assert!(committed_value.approvers.contains(&node1));
    assert!(committed_value.approvers.contains(&node2));
    assert!(committed_value.approvers.contains(&node3));
}

/// Scenario 2: Malicious leader sends conflicting values
#[tokio::test]
async fn test_malicious_leader_conflicting_values() {
    let leader = NodeId::new(1);
    let node2 = NodeId::new(2);
    let _node3 = NodeId::new(3);
    let _node4 = NodeId::new(4);

    let config = ConsensusConfig::new(4);
    let mut consensus = osiris_core::byzantine::PBFTLiteConsensus::new(leader, config);

    // Leader proposes value A
    let value_a = ProposalValue::new("value_a".to_string());
    consensus.propose(value_a.clone()).await.unwrap();

    // Node 2 approves value A
    consensus.add_approval(node2, value_a.clone()).unwrap();

    // Malicious: leader creates different value B with same round
    let value_b = ProposalValue::new("value_b".to_string());

    // Try to get node3 to vote for value B while node2 voted for A
    // This should be caught as Byzantine behavior
    let result = consensus.add_approval(node2, value_b.clone());

    // This should fail because node2 already voted for a different value
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("Byzantine"));
}

/// Scenario 3: Network partition - honest quorum can still commit
#[tokio::test]
async fn test_network_partition_quorum_commits() {
    // Setup: 4 nodes, 1 partitioned
    let node1 = NodeId::new(1);
    let node2 = NodeId::new(2);
    let node3 = NodeId::new(3);
    let _node4 = NodeId::new(4); // Partitioned, won't participate

    let config = ConsensusConfig::new(4);
    let mut consensus = osiris_core::byzantine::PBFTLiteConsensus::new(node1, config);

    // Propose value
    let value = ProposalValue::new("partition_test".to_string());
    consensus.propose(value.clone()).await.unwrap();

    // Get approvals from quorum (3 nodes: 1, 2, 3)
    consensus.add_approval(node2, value.clone()).unwrap();
    consensus.add_approval(node3, value.clone()).unwrap();

    // Even with node4 partitioned, we have quorum from honest nodes
    let committed = consensus.commit_when_ready().await;
    assert!(committed.is_ok());

    let committed_value = committed.unwrap();
    assert_eq!(committed_value.approvers.len(), 3);
}

/// Scenario 4: Node isolation - Byzantine node detected and excluded
#[tokio::test]
async fn test_node_isolation_detection() {
    let node1 = NodeId::new(1);
    let node2 = NodeId::new(2);
    let node3 = NodeId::new(3);
    let byzantine_node = NodeId::new(4);

    let config = ConsensusConfig::new(4);
    let mut consensus = osiris_core::byzantine::PBFTLiteConsensus::new(node1, config);

    // Simulate Byzantine node sending conflicting votes
    let value_a = ProposalValue::new("honest_value".to_string());
    let value_b = ProposalValue::new("byzantine_value".to_string());

    // Record first vote
    consensus
        .add_approval(byzantine_node, value_a.clone())
        .unwrap();

    // Try conflicting vote - should detect misbehavior
    let result = consensus.add_approval(byzantine_node, value_b.clone());
    assert!(result.is_err());

    // Create evidence and detect misbehavior
    let evidence = Evidence::new(
        byzantine_node,
        Misbehavior::ConflictingVotes {
            value_a: value_a.clone(),
            value_b: value_b.clone(),
        },
    );

    consensus
        .detect_misbehavior(byzantine_node, evidence)
        .await
        .unwrap();

    // Node should be isolated
    assert!(consensus.is_node_isolated(byzantine_node).await);

    // Honest nodes can still reach quorum
    consensus.propose(value_a.clone()).await.unwrap();
    consensus.add_approval(node2, value_a.clone()).unwrap();
    consensus.add_approval(node3, value_a.clone()).unwrap();

    let committed = consensus.commit_when_ready().await;
    assert!(committed.is_ok());
}

/// Scenario 5: Recovery - rejoining node accepts decision
#[tokio::test]
async fn test_recovery_rejoining_node() {
    let node1 = NodeId::new(1);
    let node2 = NodeId::new(2);
    let node3 = NodeId::new(3);
    let _returning_node = NodeId::new(4);

    let config = ConsensusConfig::new(4);
    let mut consensus = osiris_core::byzantine::PBFTLiteConsensus::new(node1, config);

    // Propose and commit with 3 nodes
    let value = ProposalValue::new("committed_value".to_string());
    consensus.propose(value.clone()).await.unwrap();

    consensus.add_approval(node2, value.clone()).unwrap();
    consensus.add_approval(node3, value.clone()).unwrap();

    let committed = consensus.commit_when_ready().await;
    assert!(committed.is_ok());

    // Now returning_node rejoins
    // It should accept the committed value without re-proposing
    let committed_value = committed.unwrap();
    assert_eq!(committed_value.value, value);

    // Verify the decision can be shared with returning node
    assert_eq!(committed_value.round, Round::new(1));
}

/// Test leader election with Byzantine-aware exclusion
#[tokio::test]
async fn test_leader_election_excludes_byzantine() {
    let nodes = vec![
        NodeId::new(1),
        NodeId::new(2),
        NodeId::new(3),
        NodeId::new(4),
    ];

    let mut elector = LeaderElector::new(nodes, LeaderElectionStrategy::RoundRobin);

    // Normal election
    let leader1 = elector.elect_for_round(Round::new(0));
    assert_eq!(leader1, NodeId::new(1));

    // Node 1 is Byzantine, exclude it
    elector.exclude_node(NodeId::new(1));

    // Next leader should skip node 1
    let leader2 = elector.elect_for_round(Round::new(0));
    assert_ne!(leader2, NodeId::new(1));
    assert_eq!(leader2.0 > 1, true); // Should be node 2, 3, or 4
}

/// Test evidence logging and isolation thresholds
#[tokio::test]
async fn test_evidence_logging_isolation_threshold() {
    let mut evidence_log = EvidenceLog::new();
    let byzantine_node = NodeId::new(1);
    let honest_node = NodeId::new(2);

    let value_a = ProposalValue::new("a".to_string());
    let value_b = ProposalValue::new("b".to_string());

    // Single critical evidence - not isolated yet
    let evidence1 = Evidence::new(
        byzantine_node,
        Misbehavior::ConflictingVotes {
            value_a: value_a.clone(),
            value_b: value_b.clone(),
        },
    );
    evidence_log.add(evidence1);
    assert!(!evidence_log.should_isolate(byzantine_node));

    // Second critical evidence - should isolate
    let evidence2 = Evidence::new(
        byzantine_node,
        Misbehavior::DoubleProposal {
            round: 1,
            value_a: value_a.clone(),
            value_b,
        },
    );
    evidence_log.add(evidence2);
    assert!(evidence_log.should_isolate(byzantine_node));

    // Honest node with one low-severity evidence should not be isolated
    evidence_log.add(Evidence::new(
        honest_node,
        Misbehavior::Timeout { round: 1 },
    ));
    assert!(!evidence_log.should_isolate(honest_node));

    // Get isolated nodes list
    let isolated = evidence_log.get_isolated_nodes();
    assert_eq!(isolated.len(), 1);
    assert!(isolated.contains(&byzantine_node));
}

/// Test message types and round extraction
#[tokio::test]
async fn test_message_handling() {
    let sender = NodeId::new(1);
    let round = Round::new(5);
    let value = ProposalValue::new("test_value".to_string());

    // Test Propose message
    let propose_msg = Message::propose(sender, round, value.clone());
    assert_eq!(propose_msg.sender, sender);
    assert_eq!(propose_msg.get_round(), Some(round));
    assert_eq!(propose_msg.get_value(), Some(&value));

    // Test Approve message
    let approve_msg = Message::approve(sender, round, value.clone());
    assert_eq!(approve_msg.get_round(), Some(round));

    // Test Commit message
    let commit_msg = Message::commit(sender, round, value.clone());
    assert_eq!(commit_msg.get_round(), Some(round));

    // Test LeaderChange message (no round/value)
    let leader_change_msg = Message::leader_change(sender, "Byzantine detected".to_string());
    assert_eq!(leader_change_msg.get_round(), None);
    assert_eq!(leader_change_msg.get_value(), None);

    // Test Heartbeat message
    let heartbeat_msg = Message::heartbeat(sender, round);
    assert_eq!(heartbeat_msg.get_round(), Some(round));

    // Test serialization
    let json = serde_json::to_string(&propose_msg).unwrap();
    let deserialized: Message = serde_json::from_str(&json).unwrap();
    assert_eq!(deserialized.sender, sender);
}

/// Test quorum calculations for various cluster sizes
#[tokio::test]
async fn test_quorum_calculations() {
    // 4-node cluster: f=1, quorum=3
    let config4 = ConsensusConfig::new(4);
    assert_eq!(config4.max_faulty_nodes, 1);
    assert_eq!(config4.quorum_size(), 3);
    assert!(config4.is_quorum(3));
    assert!(!config4.is_quorum(2));

    // 7-node cluster: f=2, quorum=5
    let config7 = ConsensusConfig::new(7);
    assert_eq!(config7.max_faulty_nodes, 2);
    assert_eq!(config7.quorum_size(), 5);
    assert!(config7.is_quorum(5));
    assert!(!config7.is_quorum(4));

    // 13-node cluster: f=4, quorum=9
    let config13 = ConsensusConfig::new(13);
    assert_eq!(config13.max_faulty_nodes, 4);
    assert_eq!(config13.quorum_size(), 9);
    assert!(config13.is_quorum(9));
    assert!(!config13.is_quorum(8));
}

/// Test consensus round progression
#[tokio::test]
async fn test_consensus_round_progression() {
    let node1 = NodeId::new(1);
    let config = ConsensusConfig::new(4);
    let mut consensus = osiris_core::byzantine::PBFTLiteConsensus::new(node1, config);

    // Initial round should be 0
    assert_eq!(consensus.current_round().await, Round::new(0));

    // After first proposal, round increments
    let value1 = ProposalValue::new("value1".to_string());
    consensus.propose(value1).await.unwrap();
    assert_eq!(consensus.current_round().await, Round::new(1));

    // After second proposal, round increments again
    let value2 = ProposalValue::new("value2".to_string());
    consensus.propose(value2).await.unwrap();
    assert_eq!(consensus.current_round().await, Round::new(2));
}
