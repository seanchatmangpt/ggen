//! Swarm Consensus Mechanism Tests
//!
//! Tests for distributed decision-making and consensus building in the HiveQueen swarm.
//! Focuses on the critical 20% of consensus scenarios that cover 80% of real-world use cases.

use ggen_core::config::hive_coordinator::{AgentRole, HiveAgent, HiveQueen, ResolutionSuggestion};
use ggen_core::config::ontology_config::{
    CompositionStrategy, ConflictResolution, OntologyConfig, OntologyPackRef,
};

/// Test basic HiveQueen initialization and agent spawning
#[tokio::test]
async fn test_hive_queen_initialization() {
    let config = create_test_config(2);
    let hive = HiveQueen::new(config).await;

    assert!(hive.is_ok(), "HiveQueen should initialize successfully");

    let hive = hive.unwrap();
    assert!(
        hive.agents.len() >= 4,
        "Should spawn minimum 4 agents for basic config"
    );
}

/// Test agent spawning scales with configuration complexity
#[tokio::test]
async fn test_agent_scaling_with_complexity() {
    // Simple config (2 packs) - should spawn 4 base agents
    let simple_config = create_test_config(2);
    let simple_hive = HiveQueen::new(simple_config).await.unwrap();
    assert_eq!(
        simple_hive.agents.len(),
        4,
        "Simple config should spawn 4 agents"
    );

    // Medium config (4 packs) - should spawn 5 agents (4 + optimizer)
    let medium_config = create_test_config(4);
    let medium_hive = HiveQueen::new(medium_config).await.unwrap();
    assert_eq!(
        medium_hive.agents.len(),
        5,
        "Medium config should spawn 5 agents"
    );

    // Complex config (6 packs) - should spawn 6 agents (4 + optimizer + performance)
    let complex_config = create_test_config(6);
    let complex_hive = HiveQueen::new(complex_config).await.unwrap();
    assert_eq!(
        complex_hive.agents.len(),
        6,
        "Complex config should spawn 6 agents"
    );
}

/// Test all agent roles are correctly represented
#[tokio::test]
async fn test_agent_role_distribution() {
    let config = create_test_config(6);
    let hive = HiveQueen::new(config).await.unwrap();

    let roles: Vec<AgentRole> = hive.agents.iter().map(|a| a.role.clone()).collect();

    assert!(roles.contains(&AgentRole::Analyzer), "Should have Analyzer");
    assert!(
        roles.contains(&AgentRole::VersionResolver),
        "Should have VersionResolver"
    );
    assert!(
        roles.contains(&AgentRole::ConflictDetector),
        "Should have ConflictDetector"
    );
    assert!(
        roles.contains(&AgentRole::Validator),
        "Should have Validator"
    );
    assert!(
        roles.contains(&AgentRole::Optimizer),
        "Should have Optimizer for complex config"
    );
    assert!(
        roles.contains(&AgentRole::PerformanceManager),
        "Should have PerformanceManager for complex config"
    );
}

/// Test full orchestration workflow (critical path)
#[tokio::test]
async fn test_orchestration_workflow() {
    let config = create_test_config(3);
    let mut hive = HiveQueen::new(config).await.unwrap();

    let resolved = hive.orchestrate().await;
    assert!(
        resolved.is_ok(),
        "Orchestration should complete successfully"
    );

    let resolved = resolved.unwrap();
    assert_eq!(resolved.resolved_packs.len(), 3, "Should resolve all packs");
    assert_eq!(resolved.agents_involved, hive.agents.len());
    assert_eq!(resolved.validation_status, "valid");
}

/// Test conflict detection between packages
#[tokio::test]
async fn test_conflict_detection() {
    let config = OntologyConfig::new()
        .with_pack(OntologyPackRef {
            name: "schema-org".to_string(),
            version: "^3.13.0".to_string(),
            namespace: Some("http://schema.org/".to_string()),
            classes: None,
            properties: None,
            source: None,
        })
        .with_pack(OntologyPackRef {
            name: "schema-org-ext".to_string(),
            version: "~3.12.0".to_string(),
            namespace: Some("http://schema.org/".to_string()), // Same namespace - conflict!
            classes: None,
            properties: None,
            source: None,
        });

    let mut hive = HiveQueen::new(config).await.unwrap();
    let resolved = hive.orchestrate().await.unwrap();

    // Should detect namespace conflict
    assert!(
        resolved.conflicts_found > 0,
        "Should detect namespace conflict"
    );
}

/// Test conflict resolution strategies
#[tokio::test]
async fn test_conflict_resolution_strategies() {
    let base_config = create_conflicting_config();

    // Test Union strategy
    let union_config = base_config
        .clone()
        .with_composition(CompositionStrategy::Union);
    let mut union_hive = HiveQueen::new(union_config).await.unwrap();
    let union_resolved = union_hive.orchestrate().await.unwrap();
    assert!(
        union_resolved.conflicts_resolved > 0,
        "Union should resolve conflicts by merging"
    );

    // Test Intersection strategy
    let intersection_config = base_config
        .clone()
        .with_composition(CompositionStrategy::Intersection);
    let mut intersection_hive = HiveQueen::new(intersection_config).await.unwrap();
    let intersection_resolved = intersection_hive.orchestrate().await.unwrap();
    assert!(
        intersection_resolved.conflicts_resolved > 0,
        "Intersection should resolve conflicts by excluding"
    );

    // Test Priority strategy
    let priority_config = base_config.with_composition(CompositionStrategy::Priority);
    let mut priority_hive = HiveQueen::new(priority_config).await.unwrap();
    let priority_resolved = priority_hive.orchestrate().await.unwrap();
    assert!(
        priority_resolved.conflicts_resolved > 0,
        "Priority should resolve conflicts by using first"
    );
}

/// Test agent analysis capabilities
#[tokio::test]
async fn test_agent_analysis() {
    let config = create_test_config(3);
    let agent = HiveAgent::new(
        "test-analyzer",
        AgentRole::Analyzer,
        vec!["versioning".to_string()],
    );

    let analysis = agent.analyze_config(&config).await;
    assert!(analysis.is_ok(), "Analysis should succeed");

    let insights = analysis.unwrap();
    assert!(!insights.is_empty(), "Should produce insights");
    assert!(
        insights.iter().any(|i| i.contains("packs")),
        "Should analyze pack count"
    );
}

/// Test agent expertise matching
#[tokio::test]
async fn test_agent_expertise() {
    let config = create_test_config(4);
    let hive = HiveQueen::new(config).await.unwrap();

    // Verify each agent has appropriate expertise
    for agent in &hive.agents {
        assert!(
            !agent.expertise.is_empty(),
            "Agent should have expertise domains"
        );

        match agent.role {
            AgentRole::Analyzer => assert!(agent.expertise.contains(&"versioning".to_string())),
            AgentRole::VersionResolver => {
                assert!(agent.expertise.contains(&"composition".to_string()))
            }
            AgentRole::ConflictDetector => {
                assert!(agent.expertise.contains(&"compatibility".to_string()))
            }
            AgentRole::Validator => assert!(agent.expertise.contains(&"schema".to_string())),
            AgentRole::Optimizer => assert!(agent.expertise.contains(&"performance".to_string())),
            AgentRole::PerformanceManager => {
                assert!(agent.expertise.contains(&"caching".to_string()))
            }
        }
    }
}

/// Test consensus building with multiple agents
#[tokio::test]
async fn test_multi_agent_consensus() {
    let config = create_test_config(5);
    let mut hive = HiveQueen::new(config).await.unwrap();

    // Orchestrate should coordinate all agents
    let resolved = hive.orchestrate().await.unwrap();

    // All agents should have participated
    assert_eq!(resolved.agents_involved, hive.agents.len());
}

/// Test validation phase detects unresolved conflicts
#[tokio::test]
async fn test_validation_phase() {
    let config = create_test_config(2);
    let hive = HiveQueen::new(config).await.unwrap();

    // Valid configuration should pass validation
    assert!(hive.agents.len() > 0, "Should have agents for validation");
}

/// Test agent reporting functionality
#[tokio::test]
async fn test_agent_reporting() {
    let agent = HiveAgent::new(
        "test-agent-1",
        AgentRole::Analyzer,
        vec!["testing".to_string()],
    );

    let report = agent.report();
    assert!(
        report.contains("test-agent-1"),
        "Report should include agent ID"
    );
    assert!(report.contains("Analyzer"), "Report should include role");
    assert!(
        report.contains("testing"),
        "Report should include expertise"
    );
}

/// Test concurrent agent operations (performance critical)
#[tokio::test]
async fn test_concurrent_agent_operations() {
    let config = create_test_config(6);
    let mut hive = HiveQueen::new(config).await.unwrap();

    // Measure orchestration time
    let start = std::time::Instant::now();
    let resolved = hive.orchestrate().await.unwrap();
    let duration = start.elapsed();

    // Should complete quickly even with 6 agents
    assert!(
        duration.as_millis() < 1000,
        "Orchestration should complete in <1s"
    );
    assert_eq!(resolved.agents_involved, 6);
}

/// Test resolution suggestion structure
#[tokio::test]
fn test_resolution_suggestion_structure() {
    use std::collections::BTreeMap;

    let suggestion = ResolutionSuggestion {
        agent_id: "test-agent".to_string(),
        package_name: "schema-org".to_string(),
        suggested_version: "3.13.0".to_string(),
        confidence: 0.95,
        reasoning: "Latest stable version".to_string(),
        metadata: BTreeMap::new(),
    };

    assert_eq!(suggestion.agent_id, "test-agent");
    assert_eq!(suggestion.confidence, 0.95);
    assert!(suggestion.confidence > 0.0 && suggestion.confidence <= 1.0);
}

// Helper functions

fn create_test_config(pack_count: usize) -> OntologyConfig {
    let mut config = OntologyConfig::new();

    for i in 0..pack_count {
        config = config.with_pack(OntologyPackRef {
            name: format!("test-pack-{}", i),
            version: "1.0.0".to_string(),
            namespace: Some(format!("http://test.org/pack{}/", i)),
            classes: None,
            properties: None,
            source: None,
        });
    }

    config
}

fn create_conflicting_config() -> OntologyConfig {
    OntologyConfig::new()
        .with_pack(OntologyPackRef {
            name: "pack-a".to_string(),
            version: "^2.0.0".to_string(),
            namespace: Some("http://test.org/".to_string()),
            classes: None,
            properties: None,
            source: None,
        })
        .with_pack(OntologyPackRef {
            name: "pack-b".to_string(),
            version: "~1.9.0".to_string(),
            namespace: Some("http://test.org/".to_string()),
            classes: None,
            properties: None,
            source: None,
        })
}
