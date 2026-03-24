//! Integration tests for complete workflows

use e2e_agent_workflow::*;
use uuid::Uuid;

#[test]
fn test_agent_to_workflow_integration() {
    let pool = AgentPool::new(6);
    let agents = pool.all();
    
    let mut workflow = Workflow::new("agent_workflow".to_string());
    for (i, agent) in agents.iter().enumerate() {
        let step = WorkflowStep::new(
            format!("agent_{}_step", i),
            agent.id,
            "search_web".to_string(),
            r#"{"query": "test"}"#.to_string(),
        );
        workflow.add_step(step);
    }

    assert_eq!(workflow.all_steps().len(), 6);
}

#[test]
fn test_domain_tool_integration() {
    let mut domain_mgr = DomainManager::new();
    let tool_mgr = initialize_standard_tools();

    let goal = DomainGoal::new(
        "Use tools for health".to_string(),
        LifeDomain::Health,
        9,
    );
    domain_mgr.add_goal(goal);

    let tools = tool_mgr.available_tools();
    assert!(!tools.is_empty());
}

#[test]
fn test_agent_consensus_for_domain_priorities() {
    let pool = AgentPool::new(5);
    let agents = pool.all();
    
    let consensus_mgr = ConsensusManager::default_supermajority(agents.len());
    
    let votes: Vec<_> = agents
        .iter()
        .map(|agent| AgentVote {
            agent_id: agent.id,
            vote: true,
            reason: "agree on priority".to_string(),
        })
        .collect();

    let result = consensus_mgr.reach_consensus("domain_priority".to_string(), votes);
    assert!(result.approved);
}

#[test]
fn test_tool_execution_tracking() {
    use std::collections::HashMap;
    
    let mut tool_mgr = initialize_standard_tools();
    let agent_id = Uuid::new_v4();

    let mut args = HashMap::new();
    args.insert("query".to_string(), "test query".to_string());
    
    let call = ToolCall {
        id: Uuid::new_v4(),
        tool: "search_web".to_string(),
        arguments: args,
        caller_id: agent_id,
    };

    let result = tool_mgr.execute_tool(&call);
    assert!(result.success);
    assert_eq!(result.tool_name, "search_web");
}

#[tokio::test]
async fn test_complete_system_workflow() {
    // Initialize all system components
    let pool = AgentPool::new(3);
    let mut domain_mgr = DomainManager::new();
    let tool_mgr = initialize_standard_tools();
    let mut workflow = Workflow::new("complete_system".to_string());

    // Setup domains
    let goal = DomainGoal::new(
        "Complete workflow".to_string(),
        LifeDomain::Career,
        10,
    );
    domain_mgr.add_goal(goal);

    // Setup workflow steps with agent pool
    let agents = pool.all();
    for (i, agent) in agents.iter().enumerate() {
        let step = WorkflowStep::new(
            format!("step_{}", i),
            agent.id,
            "search_web".to_string(),
            r#"{"query": "workflow"}"#.to_string(),
        );
        workflow.add_step(step);
    }

    // Execute workflow
    let success = WorkflowExecutor::execute_workflow(&mut workflow).await;
    assert!(success);

    // Verify all systems worked
    assert_eq!(pool.all().len(), 3);
    assert!(!domain_mgr.all_goals().is_empty());
    assert!(!tool_mgr.available_tools().is_empty());
}

#[test]
fn test_failure_scenario_with_recovery() {
    let pool = AgentPool::new(6);
    let agents = pool.all();
    let failed_agent_id = agents[0].id;

    // Agent fails
    let mut failed_health = agents[0].health.clone();
    failed_health.record_failure();
    failed_health.record_failure();
    failed_health.record_failure();
    pool.update_health(failed_agent_id, failed_health);

    // System detects failure
    let failed = pool.failed_agents();
    assert_eq!(failed.len(), 1);

    // System recovers agent
    assert!(pool.recover_agent(failed_agent_id));
    
    // Agent returns to healthy
    let recovered = pool.get(failed_agent_id).unwrap();
    assert_eq!(recovered.health.status, HealthStatus::Recovering);
}

#[test]
fn test_byzantine_fault_tolerance() {
    let bft = ByzantineFaultTolerance::new(7);
    assert_eq!(bft.max_tolerable_failures(), 2);
    assert!(bft.can_tolerate(2));
    assert!(!bft.can_tolerate(3));
    
    let pool = AgentPool::new(7);
    let health_summary = pool.health_summary();
    assert!(health_summary.total_agents >= bft.min_healthy_agents_needed());
}

#[test]
fn test_multi_step_workflow_with_dependencies() {
    let mut workflow = Workflow::new("multi_step".to_string());
    let agent_id = Uuid::new_v4();

    let step1 = WorkflowStep::new(
        "fetch".to_string(),
        agent_id,
        "search_web".to_string(),
        "{}".to_string(),
    );
    let step1_id = step1.id;
    workflow.add_step(step1);

    let step2 = WorkflowStep::new(
        "parse".to_string(),
        agent_id,
        "analyze".to_string(),
        "{}".to_string(),
    ).add_dependency(step1_id);
    let step2_id = step2.id;
    workflow.add_step(step2);

    let step3 = WorkflowStep::new(
        "store".to_string(),
        agent_id,
        "store_data".to_string(),
        "{}".to_string(),
    ).add_dependency(step2_id);
    workflow.add_step(step3);

    assert_eq!(workflow.all_steps().len(), 3);
}
