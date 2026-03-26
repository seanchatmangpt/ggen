//! SLO validation tests

use performance_benchmarks::{BenchmarkResult, slos};

#[test]
fn test_agent_creation_slo() {
    let result = BenchmarkResult::new(
        "agent_creation".to_string(),
        75,
        slos::AGENT_CREATION_MS,
    );
    assert!(result.passed);
    assert!(result.slack_percentage() > 0.0);
}

#[test]
fn test_agent_startup_slo() {
    let result = BenchmarkResult::new(
        "agent_startup".to_string(),
        400,
        slos::AGENT_STARTUP_MS,
    );
    assert!(result.passed);
}

#[test]
fn test_tool_discovery_slo() {
    let result = BenchmarkResult::new(
        "tool_discovery".to_string(),
        150,
        slos::TOOL_DISCOVERY_MS,
    );
    assert!(result.passed);
}

#[test]
fn test_plan_generation_slo() {
    let result = BenchmarkResult::new(
        "plan_generation_10_steps".to_string(),
        850,
        slos::PLAN_GENERATION_10_STEPS_MS,
    );
    assert!(result.passed);
}

#[test]
fn test_tool_execution_slo() {
    let result = BenchmarkResult::new(
        "tool_execution".to_string(),
        85,
        slos::TOOL_EXECUTION_MS,
    );
    assert!(result.passed);
}

#[test]
fn test_consensus_slo() {
    let result = BenchmarkResult::new(
        "consensus_3_agent".to_string(),
        1800,
        slos::CONSENSUS_3_AGENT_MS,
    );
    assert!(result.passed);
}

#[test]
fn test_domain_balance_slo() {
    let result = BenchmarkResult::new(
        "domain_balance".to_string(),
        450,
        slos::DOMAIN_BALANCE_MS,
    );
    assert!(result.passed);
}

#[test]
fn test_all_slos_configuration() {
    assert!(slos::AGENT_CREATION_MS > 0);
    assert!(slos::AGENT_STARTUP_MS > slos::AGENT_CREATION_MS);
    assert!(slos::TOOL_DISCOVERY_MS > 0);
    assert!(slos::PLAN_GENERATION_10_STEPS_MS > 0);
    assert!(slos::TOOL_EXECUTION_MS > 0);
    assert!(slos::CONSENSUS_3_AGENT_MS > 0);
    assert!(slos::DOMAIN_BALANCE_MS > 0);
}
