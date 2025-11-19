//! Swarm Integration Tests
//!
//! End-to-end integration tests for swarm coordination, combining consensus,
//! security, and orchestration in realistic workflows.

use ggen_core::config::hive_coordinator::{AgentRole, HiveQueen};
use ggen_core::config::ontology_config::{CompositionStrategy, OntologyConfig, OntologyPackRef};
use ggen_core::security::command::SafeCommand;

/// Test full swarm workflow: spawn -> orchestrate -> validate
#[tokio::test]
async fn test_full_swarm_workflow() {
    // Create realistic configuration
    let config = OntologyConfig::new()
        .with_pack(OntologyPackRef {
            name: "schema-org".to_string(),
            version: "3.13.0".to_string(),
            namespace: Some("http://schema.org/".to_string()),
            classes: None,
            properties: None,
            source: None,
        })
        .with_pack(OntologyPackRef {
            name: "dublin-core".to_string(),
            version: "2.0.0".to_string(),
            namespace: Some("http://purl.org/dc/elements/1.1/".to_string()),
            classes: None,
            properties: None,
            source: None,
        })
        .with_composition(CompositionStrategy::Union);

    // Initialize swarm
    let mut hive = HiveQueen::new(config)
        .await
        .expect("Swarm initialization failed");

    // Verify agent spawning
    assert!(hive.agents.len() >= 4, "Should spawn minimum agents");

    // Execute orchestration
    let resolved = hive.orchestrate().await.expect("Orchestration failed");

    // Validate results
    assert_eq!(resolved.resolved_packs.len(), 2);
    assert_eq!(resolved.validation_status, "valid");
    assert!(resolved.agents_involved >= 4);
}

/// Test swarm handles complex multi-pack scenarios
#[tokio::test]
async fn test_complex_multi_pack_orchestration() {
    let mut config = OntologyConfig::new().with_composition(CompositionStrategy::Priority);

    // Add 8 different packs
    for i in 0..8 {
        config = config.with_pack(OntologyPackRef {
            name: format!("ontology-pack-{}", i),
            version: format!("{}.0.0", i + 1),
            namespace: Some(format!("http://example.org/pack{}/", i)),
            classes: None,
            properties: None,
            source: None,
        });
    }

    let mut hive = HiveQueen::new(config)
        .await
        .expect("Complex config initialization failed");

    // Should spawn 6 agents (4 base + optimizer + performance)
    assert_eq!(hive.agents.len(), 6);

    let resolved = hive
        .orchestrate()
        .await
        .expect("Complex orchestration failed");

    assert_eq!(resolved.resolved_packs.len(), 8);
    assert_eq!(resolved.agents_involved, 6);
}

/// Test swarm handles conflicting packages correctly
#[tokio::test]
async fn test_conflict_resolution_workflow() {
    let config = OntologyConfig::new()
        .with_pack(OntologyPackRef {
            name: "conflicting-a".to_string(),
            version: "^2.0.0".to_string(),
            namespace: Some("http://conflict.org/".to_string()),
            classes: None,
            properties: None,
            source: None,
        })
        .with_pack(OntologyPackRef {
            name: "conflicting-b".to_string(),
            version: "~1.9.0".to_string(),
            namespace: Some("http://conflict.org/".to_string()),
            classes: None,
            properties: None,
            source: None,
        })
        .with_composition(CompositionStrategy::Union);

    let mut hive = HiveQueen::new(config).await.unwrap();
    let resolved = hive.orchestrate().await.unwrap();

    // Should detect conflicts
    assert!(resolved.conflicts_found > 0);

    // Should attempt resolution
    assert!(resolved.conflicts_resolved > 0);
}

/// Test swarm coordination with secure command execution
#[tokio::test]
async fn test_swarm_with_secure_commands() {
    let config = create_test_config(3);
    let mut hive = HiveQueen::new(config).await.unwrap();

    // Orchestrate
    let resolved = hive.orchestrate().await.unwrap();
    assert_eq!(resolved.validation_status, "valid");

    // Verify safe command execution works alongside swarm
    let git_cmd = SafeCommand::new("git").unwrap().arg("--version");
    assert!(git_cmd.is_ok());

    // Verify dangerous commands still blocked
    let dangerous_cmd = SafeCommand::new("rm");
    assert!(dangerous_cmd.is_err());
}

/// Test agent collaboration during orchestration
#[tokio::test]
async fn test_agent_collaboration() {
    let config = create_test_config(5);
    let mut hive = HiveQueen::new(config).await.unwrap();

    // Track initial agent count
    let agent_count = hive.agents.len();
    assert_eq!(agent_count, 5);

    // Orchestrate - all agents should participate
    let resolved = hive.orchestrate().await.unwrap();
    assert_eq!(resolved.agents_involved, agent_count);

    // Verify different roles collaborated
    let roles: Vec<AgentRole> = hive.agents.iter().map(|a| a.role.clone()).collect();
    assert!(roles.contains(&AgentRole::Analyzer));
    assert!(roles.contains(&AgentRole::ConflictDetector));
    assert!(roles.contains(&AgentRole::Validator));
}

/// Test swarm memory and state management
#[tokio::test]
async fn test_swarm_state_management() {
    let config = create_test_config(4);
    let mut hive = HiveQueen::new(config).await.unwrap();

    // First orchestration
    let resolved1 = hive.orchestrate().await.unwrap();
    assert_eq!(resolved1.resolved_packs.len(), 4);

    // Second orchestration should maintain state consistency
    let resolved2 = hive.orchestrate().await.unwrap();
    assert_eq!(
        resolved2.resolved_packs.len(),
        resolved1.resolved_packs.len()
    );
}

/// Test swarm performance under load
#[tokio::test]
async fn test_swarm_performance() {
    let config = create_test_config(10);
    let mut hive = HiveQueen::new(config).await.unwrap();

    let start = std::time::Instant::now();
    let resolved = hive.orchestrate().await.unwrap();
    let duration = start.elapsed();

    // Should complete within 2 seconds even with 10 packs
    assert!(
        duration.as_secs() < 2,
        "Orchestration too slow: {:?}",
        duration
    );
    assert_eq!(resolved.resolved_packs.len(), 10);
}

/// Test swarm handles empty configuration
#[tokio::test]
async fn test_empty_configuration() {
    let config = OntologyConfig::new();
    let mut hive = HiveQueen::new(config).await.unwrap();

    let resolved = hive.orchestrate().await.unwrap();
    assert_eq!(resolved.resolved_packs.len(), 0);
    assert_eq!(resolved.conflicts_found, 0);
}

/// Test swarm handles single pack configuration
#[tokio::test]
async fn test_single_pack_configuration() {
    let config = create_test_config(1);
    let mut hive = HiveQueen::new(config).await.unwrap();

    // Should still spawn base agents
    assert_eq!(hive.agents.len(), 4);

    let resolved = hive.orchestrate().await.unwrap();
    assert_eq!(resolved.resolved_packs.len(), 1);
    assert_eq!(resolved.conflicts_found, 0);
}

/// Test different composition strategies
#[tokio::test]
async fn test_composition_strategies() {
    let base_packs = vec![
        OntologyPackRef {
            name: "pack-1".to_string(),
            version: "1.0.0".to_string(),
            namespace: Some("http://test1.org/".to_string()),
            classes: None,
            properties: None,
            source: None,
        },
        OntologyPackRef {
            name: "pack-2".to_string(),
            version: "2.0.0".to_string(),
            namespace: Some("http://test2.org/".to_string()),
            classes: None,
            properties: None,
            source: None,
        },
    ];

    // Test Union
    let union_config = OntologyConfig::new()
        .with_packs(base_packs.clone())
        .with_composition(CompositionStrategy::Union);
    let mut union_hive = HiveQueen::new(union_config).await.unwrap();
    let union_resolved = union_hive.orchestrate().await.unwrap();
    assert_eq!(
        union_resolved.composition_strategy,
        CompositionStrategy::Union
    );

    // Test Intersection
    let intersection_config = OntologyConfig::new()
        .with_packs(base_packs.clone())
        .with_composition(CompositionStrategy::Intersection);
    let mut intersection_hive = HiveQueen::new(intersection_config).await.unwrap();
    let intersection_resolved = intersection_hive.orchestrate().await.unwrap();
    assert_eq!(
        intersection_resolved.composition_strategy,
        CompositionStrategy::Intersection
    );

    // Test Priority
    let priority_config = OntologyConfig::new()
        .with_packs(base_packs)
        .with_composition(CompositionStrategy::Priority);
    let mut priority_hive = HiveQueen::new(priority_config).await.unwrap();
    let priority_resolved = priority_hive.orchestrate().await.unwrap();
    assert_eq!(
        priority_resolved.composition_strategy,
        CompositionStrategy::Priority
    );
}

/// Test concurrent swarm operations
#[tokio::test]
async fn test_concurrent_swarms() {
    let config1 = create_test_config(3);
    let config2 = create_test_config(4);
    let config3 = create_test_config(5);

    // Spawn multiple swarms concurrently
    let (hive1, hive2, hive3) = tokio::join!(
        HiveQueen::new(config1),
        HiveQueen::new(config2),
        HiveQueen::new(config3)
    );

    assert!(hive1.is_ok());
    assert!(hive2.is_ok());
    assert!(hive3.is_ok());

    // Orchestrate all concurrently
    let (resolved1, resolved2, resolved3) = tokio::join!(
        hive1.unwrap().orchestrate(),
        hive2.unwrap().orchestrate(),
        hive3.unwrap().orchestrate()
    );

    assert!(resolved1.is_ok());
    assert!(resolved2.is_ok());
    assert!(resolved3.is_ok());
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
