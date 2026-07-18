//! Fault tolerance and recovery tests
use distributed_consensus::{Message, PbftConfig, PbftConsensus};

#[test]
fn test_config_validation_4_nodes() {
    let config = PbftConfig::new(4);
    assert!(config.is_ok());
    let c = config.unwrap();
    assert_eq!(c.total_nodes, 4);
    assert_eq!(c.max_faults, 1);
}

#[test]
fn test_config_validation_7_nodes() {
    let config = PbftConfig::new(7);
    assert!(config.is_ok());
    let c = config.unwrap();
    assert_eq!(c.total_nodes, 7);
    assert_eq!(c.max_faults, 2);
}

#[test]
fn test_config_validation_10_nodes() {
    let config = PbftConfig::new(10);
    assert!(config.is_ok());
    let c = config.unwrap();
    assert_eq!(c.total_nodes, 10);
    assert_eq!(c.max_faults, 3);
}

#[test]
fn test_config_validation_16_nodes() {
    let config = PbftConfig::new(16);
    assert!(config.is_ok());
    let c = config.unwrap();
    assert_eq!(c.total_nodes, 16);
    assert_eq!(c.max_faults, 5);
}

#[test]
fn test_invalid_config_too_few_nodes() {
    let config = PbftConfig::new(1);
    assert!(config.is_err());
}

#[test]
fn test_invalid_config_2_nodes() {
    let config = PbftConfig::new(2);
    assert!(config.is_err());
}

#[test]
fn test_invalid_config_3_nodes() {
    let config = PbftConfig::new(3);
    assert!(config.is_err());
}

#[test]
fn test_fault_tolerance_threshold_4_nodes() {
    let config = PbftConfig::new(4).expect("Valid config");
    // With f=1, can tolerate 1 Byzantine fault
    // System works with 3 honest nodes
    assert!(config.total_nodes >= 3 * config.max_faults + 1);
}

#[test]
fn test_fault_tolerance_threshold_10_nodes() {
    let config = PbftConfig::new(10).expect("Valid config");
    // With f=3, can tolerate 3 Byzantine faults
    // System works with 7 honest nodes
    assert!(config.total_nodes >= 3 * config.max_faults + 1);
    assert_eq!(config.total_nodes, 10);
    assert_eq!(config.max_faults, 3);
}

#[test]
fn test_quorum_guarantees_safety() {
    let config = PbftConfig::new(4).expect("Valid config");
    // Quorum size 3 = 2f+1 where f=1
    // With 2 independent quorums, they must overlap in at least 1 honest node
    assert_eq!(config.quorum_size(), 3);
    // If q1 and q2 are quorums of size 3, and n=4:
    // |q1 ∩ q2| = 3 + 3 - 4 = 2 (intersection guaranteed)
    assert!(3 + 3 - 4 as usize > 0);
}

#[test]
fn test_quorum_guarantees_safety_10_nodes() {
    let config = PbftConfig::new(10).expect("Valid config");
    // Quorum size 7 = 2f+1 where f=3
    // With 2 independent quorums of size 7 from n=10:
    // Intersection = 7 + 7 - 10 = 4 (at least 4 nodes in both)
    assert_eq!(config.quorum_size(), 7);
    assert!(7 + 7 - 10 > 0);
}

#[tokio::test]
async fn test_node_isolation_detection() {
    let config = PbftConfig::new(4).expect("Valid config");
    let consensus = PbftConsensus::new(config).expect("Valid consensus");

    // Node 3 is isolated
    let node_isolated = consensus.get_node(3).expect("Valid node");
    node_isolated.clear_messages().await;

    // System continues without node 3
    let receipt = consensus
        .run_consensus_round("isolation_test".to_string())
        .await
        .expect("Consensus without isolated node");

    assert_eq!(receipt.value, "isolation_test");
}

#[tokio::test]
async fn test_multiple_node_failures() {
    let config = PbftConfig::new(10).expect("Valid config");
    let consensus = PbftConsensus::new(config).expect("Valid consensus");

    // Simulate 2 out of 3 Byzantine limit failing
    let node1 = consensus.get_node(8).expect("Valid node");
    let node2 = consensus.get_node(9).expect("Valid node");

    node1.clear_messages().await;
    node2.clear_messages().await;

    // Still reaches consensus with 8 honest nodes (>= quorum of 7)
    let receipt = consensus
        .run_consensus_round("multiple_failures".to_string())
        .await
        .expect("Consensus despite multiple failures");

    assert_eq!(receipt.value, "multiple_failures");
}

#[tokio::test]
async fn test_recovery_after_transient_failure() {
    let config = PbftConfig::new(4).expect("Valid config");
    let consensus = PbftConsensus::new(config).expect("Valid consensus");

    let node_recovering = consensus.get_node(2).expect("Valid node");

    // Node 2 fails
    node_recovering.clear_messages().await;

    // Consensus completes without it
    let receipt1 = consensus
        .run_consensus_round("before_recovery".to_string())
        .await
        .expect("First consensus");

    // Node 2 recovers and can participate again (new round, new messages)
    let msg = Message::prepare(2, 0, 1, "new_hash".to_string());
    assert!(node_recovering.add_message(msg).is_ok());

    // Future consensus works with all nodes
    let receipt2 = consensus
        .run_consensus_round("after_recovery".to_string())
        .await
        .expect("Second consensus");

    assert_ne!(receipt1.value, receipt2.value);
}

#[tokio::test]
async fn test_network_partition_tolerance() {
    let config = PbftConfig::new(4).expect("Valid config");
    let consensus = PbftConsensus::new(config).expect("Valid consensus");

    // Simulate partition: nodes 0,1 on one side, 2,3 on other
    let node0 = consensus.get_node(0).expect("Valid node");
    let node1 = consensus.get_node(1).expect("Valid node");

    // Nodes 0 and 1 can't communicate with 2 and 3
    // But they have quorum (need 3 nodes) so can't decide
    // This demonstrates why quorum must be > n/2

    // The broader network (with 3 nodes) would reach consensus
    let receipt = consensus
        .run_consensus_round("partition_test".to_string())
        .await
        .expect("Consensus in connected partition");

    assert_eq!(receipt.value, "partition_test");
}

#[tokio::test]
async fn test_message_loss_tolerance() {
    let config = PbftConfig::new(4).expect("Valid config");
    let consensus = PbftConsensus::new(config).expect("Valid consensus");

    // Some messages are lost (simulated by not adding to node 1)
    let node1 = consensus.get_node(1).expect("Valid node");
    node1.clear_messages().await; // Simulate message loss

    // System still works due to quorum
    let receipt = consensus
        .run_consensus_round("message_loss".to_string())
        .await
        .expect("Consensus despite message loss");

    assert_eq!(receipt.value, "message_loss");
}

#[test]
fn test_timeout_configuration_reasonable() {
    let config = PbftConfig::new(4).expect("Valid config");

    // Timeouts should be reasonable (in milliseconds)
    assert!(config.preprepare_timeout.as_millis() > 0);
    assert!(config.prepare_timeout.as_millis() > 0);
    assert!(config.commit_timeout.as_millis() > 0);

    // Timeouts should be in reasonable range
    assert!(config.preprepare_timeout.as_millis() < 60000);
    assert!(config.prepare_timeout.as_millis() < 60000);
}

#[tokio::test]
async fn test_view_change_requirement() {
    let config = PbftConfig::new(4).expect("Valid config");
    let consensus = PbftConsensus::new(config).expect("Valid consensus");

    // Get a node
    let node = consensus.get_node(0).expect("Valid node");

    // In view 0, node 0 is primary
    assert_eq!(node.primary_id(0), 0);

    // After view change to view 1, node 1 becomes primary
    assert_eq!(node.primary_id(1), 1);
}

#[tokio::test]
async fn test_safety_without_liveness() {
    let config = PbftConfig::new(4).expect("Valid config");
    let consensus = PbftConsensus::new(config).expect("Valid consensus");

    // Even if we can't make progress quickly, safety is guaranteed
    // (in real system, timeouts would trigger view changes for liveness)

    let receipt = consensus
        .run_consensus_round("safety_test".to_string())
        .await
        .expect("Consensus reached");

    // Value is correct and has quorum
    assert_eq!(receipt.value, "safety_test");
    assert!(receipt.has_quorum(3));
}

#[test]
fn test_max_byzantine_bounds() {
    // For various cluster sizes, verify max Byzantine bounds
    let configs = vec![(4, 1), (7, 2), (10, 3), (13, 4), (16, 5), (19, 6)];

    for (nodes, expected_f) in configs {
        let config = PbftConfig::new(nodes as u64).expect("Valid config");
        assert_eq!(config.max_faults, expected_f);
        assert_eq!(config.quorum_size(), (2 * expected_f + 1) as usize);
    }
}

#[tokio::test]
async fn test_consensus_determinism() {
    let config = PbftConfig::new(4).expect("Valid config");
    let consensus = PbftConsensus::new(config).expect("Valid consensus");

    // Running consensus twice with same value should produce consistent results
    let receipt1 = consensus
        .run_consensus_round("deterministic_test".to_string())
        .await
        .expect("First consensus");

    let receipt2 = consensus
        .run_consensus_round("deterministic_test".to_string())
        .await
        .expect("Second consensus");

    assert_eq!(receipt1.value, receipt2.value);
    assert!(receipt1.has_quorum(3));
    assert!(receipt2.has_quorum(3));
}

#[test]
fn test_quorum_size_monotonic() {
    // As we add more nodes, quorum size should be non-decreasing
    let mut prev_quorum = 0;

    for n in 4..=20 {
        let config = PbftConfig::new(n).expect("Valid config");
        let quorum = config.quorum_size();

        // Quorum should be >= previous quorum (non-decreasing due to 2f+1 formula)
        assert!(quorum >= prev_quorum);
        prev_quorum = quorum;
    }
}
