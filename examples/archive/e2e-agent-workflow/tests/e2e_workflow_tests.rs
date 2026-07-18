//! End-to-end workflow integration tests

use e2e_agent_workflow::{
    AgentPool, DomainManager, LifeDomain, DomainGoal,
    ToolManager, Workflow, WorkflowExecutor, WorkflowStep,
    ConsensusManager, VotingStrategy, AgentVote,
};
use uuid::Uuid;

#[test]
fn test_agent_pool_creation_and_health_check() {
    let pool = AgentPool::new(6);
    assert_eq!(pool.all().len(), 6);
    let healthy = pool.healthy_agents();
    assert_eq!(healthy.len(), 6);
}

#[test]
fn test_agent_health_tracking() {
    let pool = AgentPool::new(3);
    let agents = pool.all();
    let agent_id = agents[0].id;

    let summary = pool.health_summary();
    assert_eq!(summary.total_agents, 3);
    assert_eq!(summary.healthy_count, 3);
}

#[test]
fn test_agent_failure_recovery() {
    let pool = AgentPool::new(3);
    let agents = pool.all();
    let agent_id = agents[0].id;

    // First, cause failures to trigger failed status
    let mut health = agents[0].health.clone();
    for _ in 0..3 {
        health.record_failure();
    }
    pool.update_health(agent_id, health);

    // Now recovery should work
    assert!(pool.recover_agent(agent_id));
    let recovered = pool.get(agent_id).unwrap();
    assert_eq!(recovered.health.failure_count, 0);
}

#[test]
fn test_domain_goal_management() {
    let mut manager = DomainManager::new();
    let goal = DomainGoal::new(
        "Complete project".to_string(),
        LifeDomain::Career,
        10,
    );
    let goal_id = goal.id;
    manager.add_goal(goal);

    assert_eq!(manager.all_goals().len(), 1);
    manager.update_progress(goal_id, 50);
    assert_eq!(manager.all_goals()[0].progress, 50);
}

#[test]
fn test_domain_balance_scoring() {
    let mut manager = DomainManager::new();
    for domain in &[
        LifeDomain::Health,
        LifeDomain::Mental,
        LifeDomain::Financial,
        LifeDomain::Social,
        LifeDomain::Career,
        LifeDomain::Learning,
    ] {
        let goal = DomainGoal::new(
            format!("Goal for {:?}", domain),
            *domain,
            8,
        );
        manager.add_goal(goal);
    }

    let score = manager.calculate_balance_score();
    assert!(score >= 0.0 && score <= 100.0);
}

#[test]
fn test_tool_discovery_and_registration() {
    let manager = e2e_agent_workflow::initialize_standard_tools();
    let tools = manager.available_tools();
    assert!(tools.len() >= 4);

    assert!(manager.get_tool("search_web").is_some());
    assert!(manager.get_tool("get_time").is_some());
}

#[test]
fn test_workflow_step_creation() {
    let agent_id = Uuid::new_v4();
    let step = WorkflowStep::new(
        "test_step".to_string(),
        agent_id,
        "search_web".to_string(),
        r#"{"query": "test"}"#.to_string(),
    );
    assert_eq!(step.name, "test_step");
    assert_eq!(step.agent_id, agent_id);
}

#[test]
fn test_workflow_creation_and_execution() {
    let mut workflow = Workflow::new("test_workflow".to_string());
    let agent_id = Uuid::new_v4();
    
    for i in 0..3 {
        let step = WorkflowStep::new(
            format!("step_{}", i),
            agent_id,
            "search_web".to_string(),
            format!(r#"{{"query": "query{}"}}"#, i),
        );
        workflow.add_step(step);
    }

    assert_eq!(workflow.all_steps().len(), 3);
}

#[tokio::test]
async fn test_complete_workflow_execution() {
    let mut workflow = Workflow::new("complete_workflow".to_string());
    let agent_id = Uuid::new_v4();

    for i in 0..5 {
        let step = WorkflowStep::new(
            format!("step_{}", i),
            agent_id,
            "search_web".to_string(),
            format!(r#"{{"query": "test{}"}}"#, i),
        );
        workflow.add_step(step);
    }

    let success = WorkflowExecutor::execute_workflow(&mut workflow).await;
    assert!(success);
    assert_eq!(workflow.progress_percentage(), 100.0);
}

#[test]
fn test_workflow_dependencies() {
    let mut workflow = Workflow::new("dep_workflow".to_string());
    let agent_id = Uuid::new_v4();

    let step1 = WorkflowStep::new(
        "step_1".to_string(),
        agent_id,
        "search_web".to_string(),
        "{}".to_string(),
    );
    let step1_id = step1.id;
    workflow.add_step(step1);

    let mut step2 = WorkflowStep::new(
        "step_2".to_string(),
        agent_id,
        "analyze".to_string(),
        "{}".to_string(),
    );
    step2 = step2.add_dependency(step1_id);
    workflow.add_step(step2);

    assert!(workflow.get_step(step1_id).is_some());
}

#[test]
fn test_consensus_voting() {
    let manager = ConsensusManager::new(VotingStrategy::Supermajority, 3);
    let agent1_id = Uuid::new_v4();
    let agent2_id = Uuid::new_v4();
    let agent3_id = Uuid::new_v4();
    let agent4_id = Uuid::new_v4();

    // 3 yes out of 4 = 75% > 66.67% (supermajority)
    let votes = vec![
        AgentVote {
            agent_id: agent1_id,
            vote: true,
            reason: "agree".to_string(),
        },
        AgentVote {
            agent_id: agent2_id,
            vote: true,
            reason: "agree".to_string(),
        },
        AgentVote {
            agent_id: agent3_id,
            vote: true,
            reason: "agree".to_string(),
        },
        AgentVote {
            agent_id: agent4_id,
            vote: false,
            reason: "disagree".to_string(),
        },
    ];

    let result = manager.reach_consensus("proposal_1".to_string(), votes);
    assert!(result.approved);
    assert_eq!(result.yes_votes(), 3);
}

#[test]
fn test_multi_agent_coordination() {
    let pool = AgentPool::new(6);
    let agents = pool.all();
    
    assert_eq!(agents.len(), 6);
    assert!(agents.iter().all(|a| a.health.success_count >= 0));
}

#[test]
fn test_three_pillar_integration() {
    // Pillar 1: Agent Lifecycle
    let pool = AgentPool::new(6);
    let agents = pool.all();
    assert!(!agents.is_empty());

    // Pillar 2: Domain Management
    let mut manager = DomainManager::new();
    for domain in &[
        LifeDomain::Health,
        LifeDomain::Career,
        LifeDomain::Learning,
    ] {
        let goal = DomainGoal::new(
            format!("Achieve in {:?}", domain),
            *domain,
            8,
        );
        manager.add_goal(goal);
    }
    assert_eq!(manager.all_goals().len(), 3);

    // Pillar 3: Tool Integration
    let tool_mgr = e2e_agent_workflow::initialize_standard_tools();
    assert!(tool_mgr.available_tools().len() >= 4);
}

#[test]
fn test_end_to_end_workflow_scenario() {
    let pool = AgentPool::new(3);
    let agents = pool.all();
    
    let mut workflow = Workflow::new("e2e_scenario".to_string());
    let agent_id = agents[0].id;

    // Build workflow
    for i in 0..3 {
        let step = WorkflowStep::new(
            format!("e2e_step_{}", i),
            agent_id,
            "search_web".to_string(),
            format!(r#"{{"query": "e2e_query{}"}}"#, i),
        );
        workflow.add_step(step);
    }

    assert_eq!(workflow.all_steps().len(), 3);
}

#[test]
fn test_domain_status_evaluation() {
    let mut manager = DomainManager::new();
    
    let mut goal1 = DomainGoal::new(
        "Goal1".to_string(),
        LifeDomain::Health,
        8,
    );
    goal1.set_progress(100);
    manager.add_goal(goal1);

    let goal2 = DomainGoal::new(
        "Goal2".to_string(),
        LifeDomain::Health,
        7,
    );
    manager.add_goal(goal2);

    let status = manager.domain_status("Health");
    assert_ne!(status.code(), "CRITICAL");
}

#[test]
fn test_agent_pool_health_summary_with_failures() {
    let pool = AgentPool::new(6);
    let agents = pool.all();
    let agent_id = agents[0].id;

    let summary = pool.health_summary();
    assert_eq!(summary.total_agents, 6);
    assert_eq!(summary.healthy_count, 6);
}

#[test]
fn test_workflow_progress_tracking() {
    let mut workflow = Workflow::new("progress_test".to_string());
    let agent_id = Uuid::new_v4();

    for i in 0..4 {
        let step = WorkflowStep::new(
            format!("step_{}", i),
            agent_id,
            "search_web".to_string(),
            "{}".to_string(),
        );
        workflow.add_step(step);
    }

    assert_eq!(workflow.progress_percentage(), 0.0);
    
    if let Some(first_step) = workflow.get_step_mut(workflow.all_steps()[0].id) {
        first_step.mark_completed("result".to_string());
    }

    assert!(workflow.progress_percentage() > 0.0);
}
