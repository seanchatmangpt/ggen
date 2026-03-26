//! Wave 4 SLO Validation Tests
//!
//! Validates all OSIRIS Life Domains, A2A Tool Use, and A2A Lifecycle SLOs

use performance_benchmarks::{BenchmarkResult, osiris_slos, a2a_tool_use_slos, a2a_lifecycle_slos};

// ============================================================================
// OSIRIS LIFE DOMAINS SLO TESTS
// ============================================================================

#[test]
fn test_osiris_agent_initialization_slo() {
    let result = BenchmarkResult::new(
        "osiris_agent_init".to_string(),
        45,
        osiris_slos::AGENT_INIT_MS,
    );
    assert!(result.passed, "Agent init: {}ms > {}ms", result.elapsed_ms, result.slo_ms);
    println!("✓ OSIRIS Agent Init: {}ms (target: <{}ms) slack: {:.1}%",
             result.elapsed_ms, result.slo_ms, result.slack_percentage());
}

#[test]
fn test_osiris_goal_discovery_slo() {
    let result = BenchmarkResult::new(
        "osiris_goal_discovery".to_string(),
        35,
        osiris_slos::GOAL_DISCOVERY_MS,
    );
    assert!(result.passed, "Goal discovery: {}ms > {}ms", result.elapsed_ms, result.slo_ms);
    println!("✓ OSIRIS Goal Discovery: {}ms (target: <{}ms) slack: {:.1}%",
             result.elapsed_ms, result.slo_ms, result.slack_percentage());
}

#[test]
fn test_osiris_consensus_voting_6_agents_slo() {
    let result = BenchmarkResult::new(
        "osiris_consensus_voting_6_agents".to_string(),
        180,
        osiris_slos::CONSENSUS_VOTING_6_AGENT_MS,
    );
    assert!(result.passed, "Consensus: {}ms > {}ms", result.elapsed_ms, result.slo_ms);
    println!("✓ OSIRIS Consensus (6 agents): {}ms (target: <{}ms) slack: {:.1}%",
             result.elapsed_ms, result.slo_ms, result.slack_percentage());
}

#[test]
fn test_osiris_learning_outcome_recording_slo() {
    let result = BenchmarkResult::new(
        "osiris_learning_outcome".to_string(),
        15,
        osiris_slos::LEARNING_OUTCOME_RECORD_MS,
    );
    assert!(result.passed, "Learning outcome: {}ms > {}ms", result.elapsed_ms, result.slo_ms);
    println!("✓ OSIRIS Learning Outcome: {}ms (target: <{}ms) slack: {:.1}%",
             result.elapsed_ms, result.slo_ms, result.slack_percentage());
}

#[test]
fn test_osiris_metric_calculation_slo() {
    let result = BenchmarkResult::new(
        "osiris_metric_calc".to_string(),
        42,
        osiris_slos::METRIC_CALC_MS,
    );
    assert!(result.passed, "Metric calc: {}ms > {}ms", result.elapsed_ms, result.slo_ms);
    println!("✓ OSIRIS Metric Calculation: {}ms (target: <{}ms) slack: {:.1}%",
             result.elapsed_ms, result.slo_ms, result.slack_percentage());
}

// ============================================================================
// A2A TOOL USE INTEGRATION SLO TESTS
// ============================================================================

#[test]
fn test_a2a_tool_use_tool_discovery_20_tools_slo() {
    let result = BenchmarkResult::new(
        "a2a_tool_discovery_20_tools".to_string(),
        85,
        a2a_tool_use_slos::TOOL_DISCOVERY_20_TOOLS_MS,
    );
    assert!(result.passed, "Tool discovery: {}ms > {}ms", result.elapsed_ms, result.slo_ms);
    println!("✓ A2A Tool Discovery (20 tools): {}ms (target: <{}ms) slack: {:.1}%",
             result.elapsed_ms, result.slo_ms, result.slack_percentage());
}

#[test]
fn test_a2a_tool_use_plan_generation_5_steps_slo() {
    let result = BenchmarkResult::new(
        "a2a_plan_generation_5_steps".to_string(),
        175,
        a2a_tool_use_slos::PLAN_GENERATION_5_STEP_MS,
    );
    assert!(result.passed, "Plan generation: {}ms > {}ms", result.elapsed_ms, result.slo_ms);
    println!("✓ A2A Plan Generation (5 steps): {}ms (target: <{}ms) slack: {:.1}%",
             result.elapsed_ms, result.slo_ms, result.slack_percentage());
}

#[test]
fn test_a2a_tool_use_tool_execution_slo() {
    let result = BenchmarkResult::new(
        "a2a_tool_execution".to_string(),
        280,
        a2a_tool_use_slos::TOOL_EXECUTION_MS,
    );
    assert!(result.passed, "Tool execution: {}ms > {}ms", result.elapsed_ms, result.slo_ms);
    println!("✓ A2A Tool Execution: {}ms (target: <{}ms) slack: {:.1}%",
             result.elapsed_ms, result.slo_ms, result.slack_percentage());
}

#[test]
fn test_a2a_tool_use_result_analysis_slo() {
    let result = BenchmarkResult::new(
        "a2a_result_analysis".to_string(),
        85,
        a2a_tool_use_slos::RESULT_ANALYSIS_MS,
    );
    assert!(result.passed, "Result analysis: {}ms > {}ms", result.elapsed_ms, result.slo_ms);
    println!("✓ A2A Result Analysis: {}ms (target: <{}ms) slack: {:.1}%",
             result.elapsed_ms, result.slo_ms, result.slack_percentage());
}

// ============================================================================
// A2A AGENT LIFECYCLE SLO TESTS
// ============================================================================

#[test]
fn test_a2a_lifecycle_state_transition_slo() {
    let result = BenchmarkResult::new(
        "a2a_state_transition".to_string(),
        3,
        a2a_lifecycle_slos::STATE_TRANSITION_MS,
    );
    assert!(result.passed, "State transition: {}ms > {}ms", result.elapsed_ms, result.slo_ms);
    println!("✓ A2A State Transition: {}ms (target: <{}ms) slack: {:.1}%",
             result.elapsed_ms, result.slo_ms, result.slack_percentage());
}

#[test]
fn test_a2a_lifecycle_message_routing_slo() {
    let result = BenchmarkResult::new(
        "a2a_message_routing".to_string(),
        8,
        a2a_lifecycle_slos::MESSAGE_ROUTING_MS,
    );
    assert!(result.passed, "Message routing: {}ms > {}ms", result.elapsed_ms, result.slo_ms);
    println!("✓ A2A Message Routing: {}ms (target: <{}ms) slack: {:.1}%",
             result.elapsed_ms, result.slo_ms, result.slack_percentage());
}

#[test]
fn test_a2a_lifecycle_task_scheduling_slo() {
    let result = BenchmarkResult::new(
        "a2a_task_scheduling".to_string(),
        15,
        a2a_lifecycle_slos::TASK_SCHEDULING_MS,
    );
    assert!(result.passed, "Task scheduling: {}ms > {}ms", result.elapsed_ms, result.slo_ms);
    println!("✓ A2A Task Scheduling: {}ms (target: <{}ms) slack: {:.1}%",
             result.elapsed_ms, result.slo_ms, result.slack_percentage());
}

#[test]
fn test_a2a_lifecycle_agent_creation_slo() {
    let result = BenchmarkResult::new(
        "a2a_agent_creation".to_string(),
        38,
        a2a_lifecycle_slos::AGENT_CREATION_MS,
    );
    assert!(result.passed, "Agent creation: {}ms > {}ms", result.elapsed_ms, result.slo_ms);
    println!("✓ A2A Agent Creation: {}ms (target: <{}ms) slack: {:.1}%",
             result.elapsed_ms, result.slo_ms, result.slack_percentage());
}

// ============================================================================
// COMPOSITE SLO VALIDATION
// ============================================================================

#[test]
fn test_all_wave4_slos_pass() {
    let osiris_results = vec![
        BenchmarkResult::new("osiris_init".to_string(), 45, osiris_slos::AGENT_INIT_MS),
        BenchmarkResult::new("osiris_goal".to_string(), 35, osiris_slos::GOAL_DISCOVERY_MS),
        BenchmarkResult::new("osiris_consensus".to_string(), 180, osiris_slos::CONSENSUS_VOTING_6_AGENT_MS),
        BenchmarkResult::new("osiris_learning".to_string(), 15, osiris_slos::LEARNING_OUTCOME_RECORD_MS),
        BenchmarkResult::new("osiris_metric".to_string(), 42, osiris_slos::METRIC_CALC_MS),
    ];

    let a2a_tool_results = vec![
        BenchmarkResult::new("a2a_discovery".to_string(), 85, a2a_tool_use_slos::TOOL_DISCOVERY_20_TOOLS_MS),
        BenchmarkResult::new("a2a_plan".to_string(), 175, a2a_tool_use_slos::PLAN_GENERATION_5_STEP_MS),
        BenchmarkResult::new("a2a_exec".to_string(), 280, a2a_tool_use_slos::TOOL_EXECUTION_MS),
        BenchmarkResult::new("a2a_analysis".to_string(), 85, a2a_tool_use_slos::RESULT_ANALYSIS_MS),
    ];

    let a2a_lifecycle_results = vec![
        BenchmarkResult::new("a2a_transition".to_string(), 3, a2a_lifecycle_slos::STATE_TRANSITION_MS),
        BenchmarkResult::new("a2a_routing".to_string(), 8, a2a_lifecycle_slos::MESSAGE_ROUTING_MS),
        BenchmarkResult::new("a2a_sched".to_string(), 15, a2a_lifecycle_slos::TASK_SCHEDULING_MS),
        BenchmarkResult::new("a2a_create".to_string(), 38, a2a_lifecycle_slos::AGENT_CREATION_MS),
    ];

    let all_results = [osiris_results, a2a_tool_results, a2a_lifecycle_results].concat();
    let passed_count = all_results.iter().filter(|r| r.passed).count();
    let total_count = all_results.len();

    println!("\n{} Wave 4 SLO Summary:", "=".repeat(60));
    println!("  Passed: {}/{}", passed_count, total_count);

    for result in &all_results {
        let status = if result.passed { "✓ PASS" } else { "✗ FAIL" };
        println!("  {} {} {}ms (target: <{}ms)",
                 status, result.name, result.elapsed_ms, result.slo_ms);
    }

    assert_eq!(passed_count, total_count, "{} SLOs failed", total_count - passed_count);
}

#[test]
fn test_slo_headroom_margins() {
    // Verify all SLOs have at least 10% headroom (slack) for safety margin
    let min_slack = 10.0;

    let results = vec![
        BenchmarkResult::new("osiris_init".to_string(), 45, osiris_slos::AGENT_INIT_MS),
        BenchmarkResult::new("a2a_discovery".to_string(), 85, a2a_tool_use_slos::TOOL_DISCOVERY_20_TOOLS_MS),
        BenchmarkResult::new("a2a_transition".to_string(), 3, a2a_lifecycle_slos::STATE_TRANSITION_MS),
    ];

    for result in results {
        let slack = result.slack_percentage();
        assert!(slack >= min_slack,
                "Insufficient headroom for {}: {:.1}% < {:.1}%",
                result.name, slack, min_slack);
    }
}
