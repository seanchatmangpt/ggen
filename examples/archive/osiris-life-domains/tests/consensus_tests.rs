//! Domain balancing and consensus tests

use osiris_life_domains::*;
use std::collections::HashMap;

#[test]
fn test_balance_creation() {
    let balance = DomainBalance::new();
    assert_eq!(balance.consensus_threshold, 0.67);
    assert_eq!(balance.imbalance_threshold, 0.65);
}

#[test]
fn test_detect_single_imbalance() {
    let balance = DomainBalance::new();
    let mut scores = HashMap::new();
    scores.insert("health".to_string(), 0.5);
    scores.insert("career".to_string(), 0.8);
    
    let imbalanced = balance.detect_imbalances(&scores).unwrap();
    assert_eq!(imbalanced.len(), 1);
    assert!(imbalanced.contains(&"health".to_string()));
}

#[test]
fn test_detect_multiple_imbalances() {
    let balance = DomainBalance::new();
    let mut scores = HashMap::new();
    scores.insert("health".to_string(), 0.5);
    scores.insert("career".to_string(), 0.4);
    scores.insert("learning".to_string(), 0.9);
    
    let imbalanced = balance.detect_imbalances(&scores).unwrap();
    assert_eq!(imbalanced.len(), 2);
}

#[test]
fn test_detect_no_imbalances() {
    let balance = DomainBalance::new();
    let mut scores = HashMap::new();
    scores.insert("health".to_string(), 0.8);
    scores.insert("career".to_string(), 0.75);
    scores.insert("learning".to_string(), 0.9);
    
    let imbalanced = balance.detect_imbalances(&scores).unwrap();
    assert!(imbalanced.is_empty());
}

#[test]
fn test_consensus_voting_high_agreement() {
    let voting = ConsensusVoting::new();
    let mut votes = HashMap::new();
    votes.insert("agent1".to_string(), 0.9);
    votes.insert("agent2".to_string(), 0.8);
    
    let consensus = voting.vote(&votes).unwrap();
    assert!(consensus);
}

#[test]
fn test_consensus_voting_low_agreement() {
    let voting = ConsensusVoting::new();
    let mut votes = HashMap::new();
    votes.insert("agent1".to_string(), 0.4);
    votes.insert("agent2".to_string(), 0.5);
    
    let consensus = voting.vote(&votes).unwrap();
    assert!(!consensus);
}

#[test]
fn test_consensus_calculate_consensus_score() {
    let voting = ConsensusVoting::new();
    let scores = vec![0.8, 0.6, 0.9];
    let consensus = voting.calculate_consensus(&scores);
    let expected = (0.8 + 0.6 + 0.9) / 3.0;
    assert!((consensus - expected).abs() < 0.01);
}

#[tokio::test]
async fn test_consensus_voting_on_balance() {
    let mut balance = DomainBalance::new();
    let mut scores = HashMap::new();
    scores.insert("health".to_string(), 0.7);
    scores.insert("career".to_string(), 0.6);
    scores.insert("learning".to_string(), 0.8);
    
    let result = balance.run_consensus_voting(&scores);
    assert!(result.is_ok());
}

#[test]
fn test_propose_rebalancing() {
    let balance = DomainBalance::new();
    let imbalanced = vec!["health".to_string(), "career".to_string()];
    
    let strategy = balance.propose_rebalancing(&imbalanced).unwrap();
    assert_eq!(strategy.len(), 2);
    assert!(strategy.contains_key("health"));
    assert!(strategy.contains_key("career"));
}

#[test]
fn test_get_allocation() {
    let mut balance = DomainBalance::new();
    let mut scores = HashMap::new();
    scores.insert("health".to_string(), 0.7);
    
    balance.run_consensus_voting(&scores).unwrap();
    let allocation = balance.get_allocation("health");
    assert!(allocation > 0.0);
}
