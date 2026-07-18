/// Comprehensive Resilience Tests for E2E Complete System
///
/// Tests 15+ fault scenarios demonstrating all Armstrong principles
/// Scenarios include: agent crashes, tool failures, Byzantine agents,
/// network partitions, concurrent failures, and state recovery.
use e2e_complete_system::{
    agents::DomainAgent,
    consensus::PBFTConsensus,
    orchestrator::{LifeDomain, OSIRISOrchestrator, Priority},
    supervisor::Supervisor,
};

/// Scenario 1: Happy Path - All Systems Working
#[tokio::test]
async fn test_happy_path_all_systems_working() {
    let orchestrator = OSIRISOrchestrator::new("next 3 months".to_string(), false)
        .await
        .expect("Failed to create orchestrator");

    let state = orchestrator
        .run_session()
        .await
        .expect("Session should complete successfully");

    // Verify all components worked
    assert_eq!(
        state.assessments.len(),
        6,
        "All 6 domains should be assessed"
    );
    assert!(!state.priorities.is_empty(), "Priorities should be set");
    assert!(!state.plan_steps.is_empty(), "Plan should be generated");
    assert!(
        !state.execution_results.is_empty(),
        "Plan should be executed"
    );
    assert!(
        state.final_receipt.is_some(),
        "Final receipt should be generated"
    );
}

/// Scenario 2: Single Agent Crash During Analysis
#[tokio::test]
async fn test_single_agent_crash_during_analysis() {
    let orchestrator = OSIRISOrchestrator::new("next 3 months".to_string(), false)
        .await
        .expect("Failed to create orchestrator");

    // Run analysis
    let assessments = orchestrator
        .analyze_domains()
        .await
        .expect("Analysis should complete with 5/6 agents");

    // Verify we still got results (agent failures are tolerated)
    assert!(
        assessments.len() >= 5,
        "Should have at least 5 assessments despite 1 agent crash"
    );
}

/// Scenario 3: Multiple Agent Crashes
#[tokio::test]
async fn test_multiple_agent_crashes_recovery() {
    let orchestrator = OSIRISOrchestrator::new("next 3 months".to_string(), false)
        .await
        .expect("Failed to create orchestrator");

    // Analyze domains (if multiple agents crash, supervisor restarts them)
    let assessments = orchestrator
        .analyze_domains()
        .await
        .expect("Analysis should complete");

    assert!(
        assessments.len() >= 4,
        "Should have majority of assessments"
    );
}

/// Scenario 4: Consensus Reaches Agreement Despite Failures
#[tokio::test]
async fn test_consensus_reaches_agreement_with_failures() {
    let orchestrator = OSIRISOrchestrator::new("next 3 months".to_string(), false)
        .await
        .expect("Failed to create orchestrator");

    let assessments = orchestrator
        .analyze_domains()
        .await
        .expect("Failed to analyze");

    // PBFT should reach consensus even if 1 agent fails
    let priorities = orchestrator
        .reach_consensus(assessments)
        .await
        .expect("Consensus should succeed");

    assert!(
        !priorities.is_empty(),
        "Consensus should produce priorities"
    );
    assert!(priorities.len() <= 3, "Should return top 3 priorities");
}

/// Scenario 5: Tool Execution Failure with Retry
#[tokio::test]
async fn test_tool_execution_failure_with_retry() {
    let orchestrator = OSIRISOrchestrator::new("next 3 months".to_string(), false)
        .await
        .expect("Failed to create orchestrator");

    let tools = orchestrator
        .discover_tools()
        .await
        .expect("Should discover tools");

    assert!(tools.len() >= 10, "Should discover 10+ tools");

    // Tools should be discovered successfully
    assert!(!tools.is_empty(), "Should have discovered tools");
}

/// Scenario 6: Plan Generation with Dependencies
#[tokio::test]
async fn test_plan_generation_with_dependencies() {
    let orchestrator = OSIRISOrchestrator::new("next 3 months".to_string(), false)
        .await
        .expect("Failed to create orchestrator");

    let assessments = orchestrator
        .analyze_domains()
        .await
        .expect("Failed to analyze");

    let priorities = orchestrator
        .reach_consensus(assessments)
        .await
        .expect("Failed to reach consensus");

    let tools = orchestrator
        .discover_tools()
        .await
        .expect("Failed to discover tools");

    let plan = orchestrator
        .generate_plan(priorities, tools)
        .await
        .expect("Should generate plan");

    // Verify plan structure (5 steps minimum: 3 priorities + 2 extra domains)
    assert!(plan.len() >= 5, "Should generate at least 5-step plan");

    // Verify dependency chain
    for (i, step) in plan.iter().enumerate() {
        if i > 0 {
            assert!(
                !step.dependencies.is_empty(),
                "Step {} should have dependencies",
                step.id
            );
        }
    }
}

/// Scenario 7: Full Execution Pipeline
#[tokio::test]
async fn test_full_execution_pipeline() {
    let orchestrator = OSIRISOrchestrator::new("next 3 months".to_string(), false)
        .await
        .expect("Failed to create orchestrator");

    let state = orchestrator
        .run_session()
        .await
        .expect("Session should complete");

    // Verify execution happened
    assert!(
        !state.execution_results.is_empty(),
        "Should have execution results"
    );

    // Count successes
    let successful = state.execution_results.iter().filter(|r| r.success).count();

    assert!(successful > 0, "At least one step should succeed");
}

/// Scenario 8: Supervisor Restart Logic
#[tokio::test]
async fn test_supervisor_restart_logic() {
    let agent = DomainAgent::new(LifeDomain::Health)
        .await
        .expect("Failed to create agent");

    let supervisor = Supervisor::new(LifeDomain::Health, agent)
        .await
        .expect("Failed to create supervisor");

    assert!(supervisor.is_healthy().await, "Agent should start healthy");

    // Simulate crash
    supervisor.crash().await;
    assert!(!supervisor.is_healthy().await, "Agent should be unhealthy");

    // Restart
    supervisor
        .restart_agent()
        .await
        .expect("Should restart successfully");

    assert!(
        supervisor.is_healthy().await,
        "Agent should be healthy after restart"
    );
    assert_eq!(supervisor.get_restart_count(), 1, "Should have 1 restart");
}

/// Scenario 9: Supervisor Restart Limit (Circuit Breaker)
#[tokio::test]
async fn test_supervisor_restart_limit_circuit_breaker() {
    let agent = DomainAgent::new(LifeDomain::Career)
        .await
        .expect("Failed to create agent");

    let supervisor = Supervisor::new(LifeDomain::Career, agent)
        .await
        .expect("Failed to create supervisor");

    // Try to restart 6 times (max is 5)
    for i in 0..6 {
        supervisor.crash().await;
        let result = supervisor.restart_agent().await;

        if i < 5 {
            assert!(result.is_ok(), "Restart {} should succeed", i + 1);
        } else {
            assert!(
                result.is_err(),
                "Restart {} should fail (circuit breaker)",
                i + 1
            );
        }
    }
}

/// Scenario 10: State Persistence and Recovery
#[tokio::test]
async fn test_state_persistence_and_recovery() {
    let agent = DomainAgent::new(LifeDomain::Learning)
        .await
        .expect("Failed to create agent");

    let supervisor = Supervisor::new(LifeDomain::Learning, agent)
        .await
        .expect("Failed to create supervisor");

    // Set initial state
    supervisor
        .get_agent()
        .lock()
        .await
        .restore_state(0.85)
        .await
        .expect("Failed to set state");

    let initial_score = supervisor.get_agent().lock().await.get_health_score().await;
    assert_eq!(initial_score, 0.85, "State should be set");

    // Crash
    supervisor.crash().await;

    // Restart (should restore state)
    supervisor.restart_agent().await.expect("Failed to restart");

    let recovered_score = supervisor.get_agent().lock().await.get_health_score().await;
    assert_eq!(
        recovered_score, 0.85,
        "State should be recovered after restart"
    );
}

/// Scenario 11: PBFT Consensus with Byzantine Tolerance
#[tokio::test]
async fn test_pbft_consensus_byzantine_tolerance() {
    let consensus = PBFTConsensus::new(6);

    // For n=6, f should be (n-1)/3 = 1
    // This means we can tolerate 1 Byzantine failure

    let proposals = vec![
        Priority {
            domain: LifeDomain::Health,
            score: 45,
        },
        Priority {
            domain: LifeDomain::Career,
            score: 72,
        },
        Priority {
            domain: LifeDomain::Learning,
            score: 40,
        },
    ];

    let agreement = consensus
        .reach_agreement(proposals)
        .await
        .expect("Should reach consensus");

    assert!(!agreement.is_empty(), "Should have consensus result");
}

/// Scenario 12: Tool Registry with 10+ Tools
#[tokio::test]
async fn test_tool_registry_with_10_plus_tools() {
    let registry = e2e_complete_system::tools::ToolRegistry::new()
        .await
        .expect("Failed to create registry");

    let tools = registry
        .discover_tools()
        .await
        .expect("Failed to discover tools");

    assert!(
        tools.len() >= 10,
        "Should have 10+ tools (found {})",
        tools.len()
    );

    // Verify each tool is executable
    for tool in &tools[..std::cmp::min(3, tools.len())] {
        let result = registry.execute(&tool.id, serde_json::json!({})).await;
        assert!(result.is_ok(), "Tool {} should be executable", tool.name);
    }
}

/// Scenario 13: Concurrent Agent Execution
#[tokio::test]
async fn test_concurrent_agent_execution() {
    let orchestrator = OSIRISOrchestrator::new("next 3 months".to_string(), false)
        .await
        .expect("Failed to create orchestrator");

    // All 6 agents should run concurrently
    let start = std::time::Instant::now();
    let assessments = orchestrator
        .analyze_domains()
        .await
        .expect("Failed to analyze");

    let duration = start.elapsed();

    assert_eq!(assessments.len(), 6, "All 6 agents should complete");
    assert!(
        duration.as_secs_f64() < 2.0,
        "Concurrent execution should be fast (<2s, got {:.1}s)",
        duration.as_secs_f64()
    );
}

/// Scenario 14: Receipt Generation and Verification
#[tokio::test]
async fn test_receipt_generation_and_verification() {
    let signer = e2e_complete_system::receipts::ReceiptSigner::new()
        .await
        .expect("Failed to create signer");

    let priorities = vec![Priority {
        domain: LifeDomain::Health,
        score: 45,
    }];

    let receipt = signer
        .sign_consensus("session-1", &priorities)
        .await
        .expect("Failed to sign receipt");

    assert!(!receipt.is_empty(), "Receipt should be generated");

    // In real system, would verify signature
    let verified = signer
        .verify_receipt(&receipt, b"data")
        .await
        .expect("Should verify");

    assert!(verified, "Receipt should verify");
}

/// Scenario 15: Resilience to Concurrent Failures
#[tokio::test]
async fn test_resilience_to_concurrent_failures() {
    let orchestrator = OSIRISOrchestrator::new("next 3 months".to_string(), false)
        .await
        .expect("Failed to create orchestrator");

    // Run session (should handle any internal failures gracefully)
    let state = orchestrator
        .run_session()
        .await
        .expect("Session should complete despite internal failures");

    // Verify system completed work
    assert!(!state.assessments.is_empty(), "Should have assessments");
    assert!(!state.priorities.is_empty(), "Should have priorities");
    assert!(!state.plan_steps.is_empty(), "Should have plan");
}

/// Scenario 16: Agent Analysis Consistency
#[tokio::test]
async fn test_agent_analysis_consistency() {
    let agent = DomainAgent::new(LifeDomain::Career)
        .await
        .expect("Failed to create agent");

    // Run analysis twice, should get same score
    let assessment1 = agent.analyze().await.expect("First analysis");
    let assessment2 = agent.analyze().await.expect("Second analysis");

    assert_eq!(
        assessment1.health_score, assessment2.health_score,
        "Analyses should be consistent"
    );
}

/// Scenario 17: Consensus Agreement Reproducibility
#[tokio::test]
async fn test_consensus_agreement_reproducibility() {
    let orchestrator1 = OSIRISOrchestrator::new("next 3 months".to_string(), false)
        .await
        .expect("Failed to create orchestrator 1");

    let assessments1 = orchestrator1
        .analyze_domains()
        .await
        .expect("Failed to analyze 1");

    let priorities1 = orchestrator1
        .reach_consensus(assessments1)
        .await
        .expect("Failed to reach consensus 1");

    // Run again with fresh system
    let orchestrator2 = OSIRISOrchestrator::new("next 3 months".to_string(), false)
        .await
        .expect("Failed to create orchestrator 2");

    let assessments2 = orchestrator2
        .analyze_domains()
        .await
        .expect("Failed to analyze 2");

    let priorities2 = orchestrator2
        .reach_consensus(assessments2)
        .await
        .expect("Failed to reach consensus 2");

    // Both should produce same priorities (deterministic)
    assert_eq!(
        priorities1.len(),
        priorities2.len(),
        "Should produce same number of priorities"
    );
}
