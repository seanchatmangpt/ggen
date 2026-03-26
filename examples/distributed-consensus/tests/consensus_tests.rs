//! Core consensus tests - PBFT algorithm verification
use distributed_consensus::{PbftConfig, PbftConsensus, Phase};

#[tokio::test]
async fn test_pbft_single_round_4_nodes() {
    let config = PbftConfig::new(4).expect("Valid config");
    let consensus = PbftConsensus::new(config).expect("Valid consensus");

    let receipt = consensus
        .run_consensus_round("proposal_1".to_string())
        .await
        .expect("Consensus succeeded");

    assert_eq!(receipt.value, "proposal_1");
    assert_eq!(receipt.round, 0);
    assert_eq!(receipt.view, 0);
    assert!(receipt.has_quorum(3));
}

#[tokio::test]
async fn test_pbft_multiple_rounds() {
    let config = PbftConfig::new(4).expect("Valid config");
    let consensus = PbftConsensus::new(config).expect("Valid consensus");

    for i in 0..5 {
        let value = format!("value_{}", i);
        let receipt = consensus
            .run_consensus_round(value.clone())
            .await
            .expect("Consensus succeeded");

        assert_eq!(receipt.value, value);
        assert_eq!(receipt.round, 0); // Round is always 0 in this simulation
    }
}

#[tokio::test]
async fn test_quorum_calculation_4_nodes() {
    let config = PbftConfig::new(4).expect("Valid config");
    assert_eq!(config.max_faults, 1);
    assert_eq!(config.quorum_size(), 3);
}

#[tokio::test]
async fn test_quorum_calculation_10_nodes() {
    let config = PbftConfig::new(10).expect("Valid config");
    assert_eq!(config.max_faults, 3);
    assert_eq!(config.quorum_size(), 7);
}

#[tokio::test]
async fn test_quorum_calculation_16_nodes() {
    let config = PbftConfig::new(16).expect("Valid config");
    assert_eq!(config.max_faults, 5);
    assert_eq!(config.quorum_size(), 11);
}

#[tokio::test]
async fn test_node_access() {
    let config = PbftConfig::new(4).expect("Valid config");
    let consensus = PbftConsensus::new(config).expect("Valid consensus");

    // Access each node
    for i in 0..4 {
        let node = consensus.get_node(i).expect("Valid node");
        assert_eq!(node.id, i);
        assert_eq!(node.total_nodes, 4);
    }
}

#[tokio::test]
async fn test_invalid_node_access() {
    let config = PbftConfig::new(4).expect("Valid config");
    let consensus = PbftConsensus::new(config).expect("Valid consensus");

    let result = consensus.get_node(10);
    assert!(result.is_err());
}

#[tokio::test]
async fn test_primary_selection() {
    let config = PbftConfig::new(4).expect("Valid config");
    let consensus = PbftConsensus::new(config).expect("Valid consensus");

    let node = consensus.get_node(0).expect("Valid node");

    // View 0 -> Node 0 is primary
    assert_eq!(node.primary_id(0), 0);
    // View 1 -> Node 1 is primary
    assert_eq!(node.primary_id(1), 1);
    // View 2 -> Node 2 is primary
    assert_eq!(node.primary_id(2), 2);
    // View 3 -> Node 3 is primary
    assert_eq!(node.primary_id(3), 3);
    // View 4 -> Node 0 is primary again (wraps)
    assert_eq!(node.primary_id(4), 0);
}

#[tokio::test]
async fn test_message_flow() {
    let config = PbftConfig::new(4).expect("Valid config");
    let consensus = PbftConsensus::new(config).expect("Valid consensus");

    let node0 = consensus.get_node(0).expect("Valid node");
    let node1 = consensus.get_node(1).expect("Valid node");

    // Node 1 sends a message to Node 0
    let msg = distributed_consensus::Message::prepare(1, 0, 1, "hash".to_string());
    node0.add_message(msg).expect("Message added");

    assert_eq!(node0.count_message_type("PREPARE"), 1);
    assert_eq!(node1.count_message_type("PREPARE"), 0); // Node 1 doesn't have it
}

#[tokio::test]
async fn test_receipt_storage() {
    let config = PbftConfig::new(4).expect("Valid config");
    let consensus = PbftConsensus::new(config).expect("Valid consensus");

    let receipt = consensus
        .run_consensus_round("value".to_string())
        .await
        .expect("Consensus succeeded");

    let node = consensus.get_node(0).expect("Valid node");
    let stored = node.get_receipt(receipt.round).expect("Receipt exists");

    assert_eq!(stored.value, receipt.value);
    assert_eq!(stored.round, receipt.round);
}

#[tokio::test]
async fn test_node_state_transitions() {
    let config = PbftConfig::new(4).expect("Valid config");
    let consensus = PbftConsensus::new(config).expect("Valid consensus");

    let node = consensus.get_node(1).expect("Valid node");
    let mut state = node.get_state().await;

    assert_eq!(state.phase, Phase::PrePrepare);

    state.advance_to_prepare("value".to_string());
    assert_eq!(state.phase, Phase::Prepare);

    state.advance_to_commit();
    assert_eq!(state.phase, Phase::Commit);

    state.finalize();
    assert_eq!(state.phase, Phase::Decision);
}

#[tokio::test]
async fn test_all_nodes_reach_consensus() {
    let config = PbftConfig::new(4).expect("Valid config");
    let consensus = PbftConsensus::new(config).expect("Valid consensus");

    let test_value = "consensus_value".to_string();
    let receipt = consensus
        .run_consensus_round(test_value.clone())
        .await
        .expect("Consensus succeeded");

    // Verify all nodes have the receipt
    for i in 0..4 {
        let node = consensus.get_node(i).expect("Valid node");
        let stored = node.get_receipt(0);
        assert!(stored.is_some());
        assert_eq!(stored.unwrap().value, test_value);
    }
}

#[tokio::test]
async fn test_receipt_contains_value() {
    let config = PbftConfig::new(4).expect("Valid config");
    let consensus = PbftConsensus::new(config).expect("Valid consensus");

    let value = "test_consensus_value".to_string();
    let receipt = consensus
        .run_consensus_round(value.clone())
        .await
        .expect("Consensus succeeded");

    assert_eq!(receipt.value, value);
    assert!(!receipt.content_hash.is_empty());
}

#[tokio::test]
async fn test_sequential_consensus_rounds() {
    let config = PbftConfig::new(4).expect("Valid config");
    let consensus = PbftConsensus::new(config).expect("Valid consensus");

    for round_num in 0..3 {
        let value = format!("round_{}", round_num);
        let receipt = consensus
            .run_consensus_round(value.clone())
            .await
            .expect("Consensus succeeded");

        assert_eq!(receipt.value, value);

        // Verify all nodes have receipts
        for node_id in 0..4 {
            let node = consensus.get_node(node_id).expect("Valid node");
            let receipts = node.get_all_receipts();
            assert!(!receipts.is_empty());
        }
    }
}

#[test]
fn test_config_minimum_nodes() {
    // Minimum valid config: 4 nodes (f=1)
    let config = PbftConfig::new(4);
    assert!(config.is_ok());
}

#[test]
fn test_config_too_few_nodes() {
    // 3 nodes is invalid (need 3f+1, minimum 4 for f=1)
    let config = PbftConfig::new(3);
    assert!(config.is_err());
}

#[test]
fn test_config_large_cluster() {
    let config = PbftConfig::new(100).expect("Valid config");
    assert_eq!(config.max_faults, 33);
    assert_eq!(config.quorum_size(), 67);
}

#[tokio::test]
async fn test_node_quorum_size_calculation() {
    let config = PbftConfig::new(4).expect("Valid config");
    let consensus = PbftConsensus::new(config).expect("Valid consensus");

    for i in 0..4 {
        let node = consensus.get_node(i).expect("Valid node");
        assert_eq!(node.quorum_size(), 3);
    }
}

#[tokio::test]
async fn test_message_deduplication() {
    let config = PbftConfig::new(4).expect("Valid config");
    let consensus = PbftConsensus::new(config).expect("Valid consensus");

    let node = consensus.get_node(0).expect("Valid node");
    let msg = distributed_consensus::Message::prepare(1, 0, 1, "hash".to_string());

    // First add succeeds
    assert!(node.add_message(msg.clone()).is_ok());

    // Duplicate add fails
    assert!(node.add_message(msg).is_err());
}
