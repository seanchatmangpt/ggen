//! Autonomous reasoning tests

use osiris_life_domains::*;

#[test]
fn test_reasoning_high_health_score() {
    let status = AgentStatus {
        domain_id: "health".to_string(),
        health_score: 0.8,
        goals: vec![],
        current_actions: vec![],
        recent_outcomes: vec![],
    };
    
    let reasoning = AutonomousReasoning::analyze(&status).unwrap();
    assert_eq!(reasoning.priority_level, "medium");
    assert_eq!(reasoning.domain_id, "health");
}

#[test]
fn test_reasoning_low_health_score() {
    let status = AgentStatus {
        domain_id: "health".to_string(),
        health_score: 0.5,
        goals: vec![],
        current_actions: vec![],
        recent_outcomes: vec![],
    };
    
    let reasoning = AutonomousReasoning::analyze(&status).unwrap();
    assert_eq!(reasoning.priority_level, "high");
    assert!(!reasoning.action_plan.is_empty());
}

#[test]
fn test_reasoning_career_domain() {
    let status = AgentStatus {
        domain_id: "career".to_string(),
        health_score: 0.6,
        goals: vec![],
        current_actions: vec![],
        recent_outcomes: vec![],
    };
    
    let reasoning = AutonomousReasoning::analyze(&status).unwrap();
    assert_eq!(reasoning.domain_id, "career");
    assert_eq!(reasoning.priority_level, "high");
}

#[test]
fn test_reasoning_finance_domain() {
    let status = AgentStatus {
        domain_id: "finance".to_string(),
        health_score: 0.65,
        goals: vec![],
        current_actions: vec![],
        recent_outcomes: vec![],
    };
    
    let reasoning = AutonomousReasoning::analyze(&status).unwrap();
    assert_eq!(reasoning.domain_id, "finance");
}

#[test]
fn test_reasoning_learning_domain() {
    let status = AgentStatus {
        domain_id: "learning".to_string(),
        health_score: 0.4,
        goals: vec![],
        current_actions: vec![],
        recent_outcomes: vec![],
    };
    
    let reasoning = AutonomousReasoning::analyze(&status).unwrap();
    assert_eq!(reasoning.priority_level, "high");
}

#[test]
fn test_reasoning_spirituality_domain() {
    let status = AgentStatus {
        domain_id: "spirituality".to_string(),
        health_score: 0.75,
        goals: vec![],
        current_actions: vec![],
        recent_outcomes: vec![],
    };
    
    let reasoning = AutonomousReasoning::analyze(&status).unwrap();
    assert_eq!(reasoning.priority_level, "medium");
}

#[test]
fn test_reasoning_relationship_domain() {
    let status = AgentStatus {
        domain_id: "relationships".to_string(),
        health_score: 0.6,
        goals: vec![],
        current_actions: vec![],
        recent_outcomes: vec![],
    };
    
    let reasoning = AutonomousReasoning::analyze(&status).unwrap();
    assert_eq!(reasoning.domain_id, "relationships");
}

#[test]
fn test_reasoning_includes_action_plan() {
    let status = AgentStatus {
        domain_id: "health".to_string(),
        health_score: 0.4,
        goals: vec![],
        current_actions: vec![],
        recent_outcomes: vec![],
    };
    
    let reasoning = AutonomousReasoning::analyze(&status).unwrap();
    assert!(!reasoning.action_plan.is_empty());
    assert!(!reasoning.recommended_goals.is_empty());
}
