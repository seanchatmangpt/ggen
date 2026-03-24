//! Fault tolerance and recovery tests

use e2e_agent_workflow::*;
use uuid::Uuid;

#[test]
fn test_agent_heartbeat_recovery() {
    let pool = AgentPool::new(3);
    let agents = pool.all();
    let agent_id = agents[0].id;

    let mut agent = pool.get(agent_id).unwrap();
    agent.health.heartbeat();
    assert_eq!(agent.health.status, HealthStatus::Healthy);

    pool.update_health(agent_id, agent.health);
    let updated = pool.get(agent_id).unwrap();
    assert_eq!(updated.health.status, HealthStatus::Healthy);
}

#[test]
fn test_agent_degradation_path() {
    let pool = AgentPool::new(1);
    let agent = pool.all()[0].clone();
    let agent_id = agent.id;

    let mut health = agent.health.clone();
    assert_eq!(health.status, HealthStatus::Healthy);

    health.record_failure();
    assert_eq!(health.status, HealthStatus::Degraded);
    
    health.record_failure();
    assert_eq!(health.status, HealthStatus::Degraded);
    
    health.record_failure();
    assert_eq!(health.status, HealthStatus::Failed);

    pool.update_health(agent_id, health);
    let failed_agent = pool.get(agent_id).unwrap();
    assert_eq!(failed_agent.health.status, HealthStatus::Failed);
}

#[test]
fn test_failed_agent_isolation() {
    let pool = AgentPool::new(6);
    let agents = pool.all();

    // Simulate failures for first 2 agents
    for i in 0..2 {
        let mut health = agents[i].health.clone();
        for _ in 0..3 {
            health.record_failure();
        }
        pool.update_health(agents[i].id, health);
    }

    let healthy = pool.healthy_agents();
    assert_eq!(healthy.len(), 4);
    
    let failed = pool.failed_agents();
    assert_eq!(failed.len(), 2);
}

#[test]
fn test_workflow_step_failure_handling() {
    let mut workflow = Workflow::new("failure_handling".to_string());
    let agent_id = Uuid::new_v4();

    let step = WorkflowStep::new(
        "failing_step".to_string(),
        agent_id,
        "search_web".to_string(),
        "{}".to_string(),
    );
    workflow.add_step(step);

    let step_id = workflow.all_steps()[0].id;
    if let Some(step) = workflow.get_step_mut(step_id) {
        step.mark_failed("Connection timeout".to_string());
    }

    assert!(workflow.has_failures());
}

#[test]
fn test_workflow_recovery_through_retry() {
    let mut workflow = Workflow::new("retry_recovery".to_string());
    let agent_id = Uuid::new_v4();

    let mut step = WorkflowStep::new(
        "retry_step".to_string(),
        agent_id,
        "search_web".to_string(),
        "{}".to_string(),
    );
    
    // First attempt fails
    step.mark_failed("Attempt 1 failed".to_string());
    let step_id = step.id;
    workflow.add_step(step);

    // Retry: reset and mark as pending
    if let Some(s) = workflow.get_step_mut(step_id) {
        s.status = WorkflowStatus::Pending;
        s.error = None;
    }

    if let Some(s) = workflow.get_step(step_id) {
        assert_eq!(s.status, WorkflowStatus::Pending);
    }
}

#[test]
fn test_consensus_resilience_with_failures() {
    let agents = vec![
        Uuid::new_v4(),
        Uuid::new_v4(),
        Uuid::new_v4(),
        Uuid::new_v4(),
        Uuid::new_v4(),
    ];

    let manager = ConsensusManager::default_supermajority(agents.len());

    // 3 agents vote yes, 2 agents fail (no vote)
    let votes = vec![
        AgentVote {
            agent_id: agents[0],
            vote: true,
            reason: "approve".to_string(),
        },
        AgentVote {
            agent_id: agents[1],
            vote: true,
            reason: "approve".to_string(),
        },
        AgentVote {
            agent_id: agents[2],
            vote: true,
            reason: "approve".to_string(),
        },
    ];

    let result = manager.reach_consensus("decision_1".to_string(), votes);
    assert!(result.approved);
}

#[test]
fn test_Byzantine_attack_resilience() {
    let bft = ByzantineFaultTolerance::new(7);
    
    // Byzantine attacks can be from up to 2 agents
    let max_malicious = bft.max_tolerable_failures();
    assert_eq!(max_malicious, 2);

    // System remains safe with 2 failures
    assert!(bft.can_tolerate(2));
    
    // System is unsafe with 3 failures
    assert!(!bft.can_tolerate(3));
}

#[test]
fn test_self_healing_agent_pool() {
    let pool = AgentPool::new(4);
    
    // All start healthy
    assert_eq!(pool.healthy_agents().len(), 4);
    assert_eq!(pool.failed_agents().len(), 0);

    // One agent fails
    let agents = pool.all();
    let failed_id = agents[0].id;
    
    let mut health = agents[0].health.clone();
    for _ in 0..3 {
        health.record_failure();
    }
    pool.update_health(failed_id, health);

    // System detects failure
    assert_eq!(pool.failed_agents().len(), 1);
    assert_eq!(pool.healthy_agents().len(), 3);

    // Attempt recovery
    pool.recover_agent(failed_id);
    let recovered = pool.get(failed_id).unwrap();
    assert_eq!(recovered.health.status, HealthStatus::Recovering);
}

#[test]
fn test_cascading_failure_prevention() {
    let pool = AgentPool::new(6);
    
    // Mark agents as failed one by one, but stay above BFT threshold
    let agents = pool.all();
    for i in 0..2 {
        let mut health = agents[i].health.clone();
        for _ in 0..3 {
            health.record_failure();
        }
        pool.update_health(agents[i].id, health);
    }

    let summary = pool.health_summary();
    
    // Still have enough healthy agents
    assert!(summary.healthy_count >= 4);
    
    // BFT: 6 agents can tolerate 2 failures
    let bft = ByzantineFaultTolerance::new(6);
    assert!(bft.can_tolerate(summary.failed_count));
}

#[test]
fn test_partial_failure_system_continuity() {
    let pool = AgentPool::new(5);
    let agents = pool.all();

    // Fail 2 agents
    for i in 0..2 {
        let mut health = agents[i].health.clone();
        for _ in 0..3 {
            health.record_failure();
        }
        pool.update_health(agents[i].id, health);
    }

    // Create workflow with remaining agents
    let mut workflow = Workflow::new("partial_failure".to_string());
    let remaining_agents = pool.healthy_agents();

    for agent in &remaining_agents {
        let step = WorkflowStep::new(
            format!("step_{}", agent.name),
            agent.id,
            "search_web".to_string(),
            "{}".to_string(),
        );
        workflow.add_step(step);
    }

    assert_eq!(workflow.all_steps().len(), remaining_agents.len());
}
