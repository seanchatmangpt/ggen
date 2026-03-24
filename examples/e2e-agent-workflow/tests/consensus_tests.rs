//! Consensus and coordination tests

use e2e_agent_workflow::*;
use uuid::Uuid;

#[test]
fn test_majority_consensus() {
    let manager = ConsensusManager::new(VotingStrategy::Majority, 2);
    
    let votes = vec![
        AgentVote {
            agent_id: Uuid::new_v4(),
            vote: true,
            reason: "yes".to_string(),
        },
        AgentVote {
            agent_id: Uuid::new_v4(),
            vote: true,
            reason: "yes".to_string(),
        },
        AgentVote {
            agent_id: Uuid::new_v4(),
            vote: false,
            reason: "no".to_string(),
        },
    ];

    let result = manager.reach_consensus("majority_test".to_string(), votes);
    assert!(result.approved);
    assert!(result.approval_percentage() > 50.0);
}

#[test]
fn test_supermajority_consensus() {
    let manager = ConsensusManager::new(VotingStrategy::Supermajority, 3);
    
    let votes = vec![
        AgentVote { agent_id: Uuid::new_v4(), vote: true, reason: "yes".to_string() },
        AgentVote { agent_id: Uuid::new_v4(), vote: true, reason: "yes".to_string() },
        AgentVote { agent_id: Uuid::new_v4(), vote: true, reason: "yes".to_string() },
        AgentVote { agent_id: Uuid::new_v4(), vote: false, reason: "no".to_string() },
    ];

    let result = manager.reach_consensus("supermajority".to_string(), votes);
    assert!(result.approved);
}

#[test]
fn test_unanimous_consensus() {
    let manager = ConsensusManager::new(VotingStrategy::Unanimous, 3);
    
    let votes = vec![
        AgentVote { agent_id: Uuid::new_v4(), vote: true, reason: "yes".to_string() },
        AgentVote { agent_id: Uuid::new_v4(), vote: true, reason: "yes".to_string() },
        AgentVote { agent_id: Uuid::new_v4(), vote: true, reason: "yes".to_string() },
    ];

    let result = manager.reach_consensus("unanimous".to_string(), votes);
    assert!(result.approved);
}

#[test]
fn test_consensus_blocking() {
    let manager = ConsensusManager::new(VotingStrategy::Unanimous, 3);
    
    let votes = vec![
        AgentVote { agent_id: Uuid::new_v4(), vote: true, reason: "yes".to_string() },
        AgentVote { agent_id: Uuid::new_v4(), vote: true, reason: "yes".to_string() },
        AgentVote { agent_id: Uuid::new_v4(), vote: false, reason: "no".to_string() },
    ];

    let result = manager.reach_consensus("blocked".to_string(), votes);
    assert!(!result.approved);
}

#[test]
fn test_quorum_requirement() {
    let manager = ConsensusManager::new(VotingStrategy::Majority, 3);
    
    // Only 2 votes, but quorum is 3
    assert!(!manager.has_quorum(2));
    assert!(manager.has_quorum(3));
    assert!(manager.has_quorum(4));
}

#[test]
fn test_consensus_with_domain_prioritization() {
    let agent1 = Uuid::new_v4();
    let agent2 = Uuid::new_v4();
    let agent3 = Uuid::new_v4();

    let manager = ConsensusManager::default_supermajority(3);

    // Agents vote on domain priorities
    let votes = vec![
        AgentVote {
            agent_id: agent1,
            vote: true,
            reason: "health is priority".to_string(),
        },
        AgentVote {
            agent_id: agent2,
            vote: true,
            reason: "health is priority".to_string(),
        },
        AgentVote {
            agent_id: agent3,
            vote: true,
            reason: "health is priority".to_string(),
        },
    ];

    let result = manager.reach_consensus("health_priority".to_string(), votes);
    assert!(result.approved);
    assert_eq!(result.approval_percentage(), 100.0);
}

#[test]
fn test_consensus_for_goal_completion() {
    let manager = ConsensusManager::new(VotingStrategy::Majority, 2);

    let votes = vec![
        AgentVote {
            agent_id: Uuid::new_v4(),
            vote: true,
            reason: "goal completed successfully".to_string(),
        },
        AgentVote {
            agent_id: Uuid::new_v4(),
            vote: true,
            reason: "goal met all criteria".to_string(),
        },
    ];

    let result = manager.reach_consensus("goal_completion".to_string(), votes);
    assert!(result.approved);
    assert_eq!(result.yes_votes(), 2);
}

#[test]
fn test_byzantine_agreement_safety() {
    let bft = ByzantineFaultTolerance::new(7);
    
    // With 7 agents, can tolerate 2 Byzantine failures
    assert_eq!(bft.max_tolerable_failures(), 2);
    
    // Need at least 5 honest agents
    assert_eq!(bft.min_healthy_agents_needed(), 5);
    
    // Verify safety property: 3f+1 = 7
    assert_eq!(bft.total_agents, (3 * bft.max_failures) + 1);
}

#[test]
fn test_consensus_refinement_for_workflow_order() {
    let manager = ConsensusManager::new(VotingStrategy::Majority, 3);
    
    let agents: Vec<_> = (0..5).map(|_| Uuid::new_v4()).collect();

    // Agents vote on workflow step order
    let votes: Vec<_> = agents
        .iter()
        .enumerate()
        .map(|(i, &agent_id)| AgentVote {
            agent_id,
            vote: i < 3, // 3 yes, 2 no
            reason: "workflow order decision".to_string(),
        })
        .collect();

    let result = manager.reach_consensus("workflow_order".to_string(), votes);
    assert!(result.approved);
}

#[test]
fn test_multi_round_consensus() {
    let manager = ConsensusManager::default_supermajority(5);

    // Round 1: Vote on domain priorities
    let round1_votes = vec![
        AgentVote { agent_id: Uuid::new_v4(), vote: true, reason: "agree round1".to_string() },
        AgentVote { agent_id: Uuid::new_v4(), vote: true, reason: "agree round1".to_string() },
        AgentVote { agent_id: Uuid::new_v4(), vote: false, reason: "disagree round1".to_string() },
    ];

    let result1 = manager.reach_consensus("round1".to_string(), round1_votes);
    assert!(result1.approved);

    // Round 2: Vote on execution plan
    let round2_votes = vec![
        AgentVote { agent_id: Uuid::new_v4(), vote: true, reason: "agree round2".to_string() },
        AgentVote { agent_id: Uuid::new_v4(), vote: true, reason: "agree round2".to_string() },
        AgentVote { agent_id: Uuid::new_v4(), vote: true, reason: "agree round2".to_string() },
    ];

    let result2 = manager.reach_consensus("round2".to_string(), round2_votes);
    assert!(result2.approved);
}
