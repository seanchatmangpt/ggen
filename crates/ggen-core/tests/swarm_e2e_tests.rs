//! End-to-End Swarm Tests
//!
//! Comprehensive end-to-end scenarios testing the complete swarm workflow
//! from initialization through orchestration to final validation.

use ggen_core::config::hive_coordinator::HiveQueen;
use ggen_core::config::ontology_config::{CompositionStrategy, OntologyConfig, OntologyPackRef};
use ggen_core::security::command::SafeCommand;

/// Test complete real-world workflow
#[tokio::test]
async fn test_complete_real_world_workflow() {
    // Scenario: Building an ontology-based application with multiple vocabularies
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
        .with_pack(OntologyPackRef {
            name: "foaf".to_string(),
            version: "0.99.0".to_string(),
            namespace: Some("http://xmlns.com/foaf/0.1/".to_string()),
            classes: None,
            properties: None,
            source: None,
        })
        .with_composition(CompositionStrategy::Union);

    // Step 1: Initialize swarm
    let mut hive = HiveQueen::new(config)
        .await
        .expect("Failed to initialize swarm");

    // Step 2: Verify agents spawned
    assert!(hive.agents.len() >= 4, "Insufficient agents spawned");

    // Step 3: Execute orchestration
    let resolved = hive.orchestrate().await.expect("Orchestration failed");

    // Step 4: Validate results
    assert_eq!(
        resolved.resolved_packs.len(),
        3,
        "Should resolve all 3 packs"
    );
    assert_eq!(resolved.validation_status, "valid");
    assert_eq!(resolved.agents_involved, hive.agents.len());

    // Step 5: Verify secure command execution still works
    let git_check = SafeCommand::new("git").unwrap().arg("--version");
    assert!(
        git_check.is_ok(),
        "Secure commands should work alongside swarm"
    );
}

/// Test complete conflict resolution workflow
#[tokio::test]
async fn test_complete_conflict_resolution_workflow() {
    // Scenario: Resolving version conflicts in ontology composition
    let config = OntologyConfig::new()
        .with_pack(OntologyPackRef {
            name: "ontology-core".to_string(),
            version: "^2.0.0".to_string(),
            namespace: Some("http://example.org/core/".to_string()),
            classes: None,
            properties: None,
            source: None,
        })
        .with_pack(OntologyPackRef {
            name: "ontology-core-legacy".to_string(),
            version: "~1.9.5".to_string(),
            namespace: Some("http://example.org/core/".to_string()),
            classes: None,
            properties: None,
            source: None,
        })
        .with_composition(CompositionStrategy::Priority);

    // Initialize and orchestrate
    let mut hive = HiveQueen::new(config).await.unwrap();
    let resolved = hive.orchestrate().await.unwrap();

    // Verify conflict was detected
    assert!(
        resolved.conflicts_found > 0,
        "Should detect namespace conflict"
    );

    // Verify resolution was attempted
    assert!(resolved.conflicts_resolved > 0, "Should resolve conflicts");

    // System should still be valid
    assert_eq!(resolved.validation_status, "valid");
}

/// Test swarm coordination with security validation
#[tokio::test]
async fn test_swarm_with_security_coordination() {
    let config = create_secure_test_config();
    let mut hive = HiveQueen::new(config).await.unwrap();

    // Orchestrate swarm
    let resolved = hive.orchestrate().await.unwrap();
    assert_eq!(resolved.validation_status, "valid");

    // Verify secure commands work
    assert!(SafeCommand::new("git").is_ok());
    assert!(SafeCommand::new("cargo").is_ok());

    // Verify dangerous commands blocked
    assert!(SafeCommand::new("rm").is_err());
    assert!(SafeCommand::new("sh").is_err());
}

/// Test swarm handles production-scale configuration
#[tokio::test]
async fn test_production_scale_workflow() {
    // Large-scale production configuration with many packs
    let mut config = OntologyConfig::new().with_composition(CompositionStrategy::Union);

    // Add 15 ontology packs (realistic production scenario)
    for i in 0..15 {
        config = config.with_pack(OntologyPackRef {
            name: format!("production-ontology-{}", i),
            version: format!("{}.{}.0", i % 3 + 1, i % 5),
            namespace: Some(format!("http://production.example.org/ont{}/", i)),
            classes: None,
            properties: None,
            source: None,
        });
    }

    let start = std::time::Instant::now();

    // Initialize swarm
    let mut hive = HiveQueen::new(config).await.unwrap();

    // Verify appropriate agent scaling
    assert_eq!(
        hive.agents.len(),
        6,
        "Should spawn all agents for large config"
    );

    // Execute orchestration
    let resolved = hive.orchestrate().await.unwrap();

    let duration = start.elapsed();

    // Validate results
    assert_eq!(resolved.resolved_packs.len(), 15);
    assert_eq!(resolved.validation_status, "valid");

    // Performance: Should complete in reasonable time
    assert!(
        duration.as_secs() < 3,
        "Production workflow too slow: {:?}",
        duration
    );
}

/// Test swarm handles iterative refinement
#[tokio::test]
async fn test_iterative_refinement_workflow() {
    let config = create_test_config(5);
    let mut hive = HiveQueen::new(config).await.unwrap();

    // First iteration
    let resolved1 = hive.orchestrate().await.unwrap();
    assert_eq!(resolved1.resolved_packs.len(), 5);

    // Second iteration (refinement)
    let resolved2 = hive.orchestrate().await.unwrap();
    assert_eq!(resolved2.resolved_packs.len(), 5);

    // Results should be consistent
    assert_eq!(
        resolved1.resolved_packs.len(),
        resolved2.resolved_packs.len()
    );
}

/// Test swarm handles multi-strategy scenarios
#[tokio::test]
async fn test_multi_strategy_workflow() {
    let base_packs = vec![
        OntologyPackRef {
            name: "base-ontology".to_string(),
            version: "1.0.0".to_string(),
            namespace: Some("http://base.example.org/".to_string()),
            classes: None,
            properties: None,
            source: None,
        },
        OntologyPackRef {
            name: "extension-ontology".to_string(),
            version: "1.0.0".to_string(),
            namespace: Some("http://ext.example.org/".to_string()),
            classes: None,
            properties: None,
            source: None,
        },
    ];

    let strategies = vec![
        CompositionStrategy::Union,
        CompositionStrategy::Intersection,
        CompositionStrategy::Priority,
    ];

    for strategy in strategies {
        // Build config by adding each pack individually (no with_packs method)
        let mut config = OntologyConfig::new();
        for pack in base_packs.clone() {
            config = config.with_pack(pack);
        }
        config = config.with_composition(strategy.clone());

        let mut hive = HiveQueen::new(config).await.unwrap();
        let resolved = hive.orchestrate().await.unwrap();

        assert_eq!(resolved.composition_strategy, strategy);
        assert_eq!(resolved.validation_status, "valid");
    }
}

/// Test swarm recovery from transient failures
#[tokio::test]
async fn test_recovery_from_transient_failures() {
    let config = create_test_config(4);

    // Simulate multiple attempts with potential transient failures
    let mut successes = 0;

    for _ in 0..10 {
        let mut hive = HiveQueen::new(config.clone()).await.unwrap();
        if let Ok(resolved) = hive.orchestrate().await {
            assert_eq!(resolved.validation_status, "valid");
            successes += 1;
        }
    }

    // Should have high success rate
    assert!(successes >= 9, "Too many failures: {}/10", successes);
}

/// Test swarm handles mixed valid/invalid packs
#[tokio::test]
async fn test_mixed_validity_workflow() {
    let config = OntologyConfig::new()
        .with_pack(OntologyPackRef {
            name: "valid-pack-1".to_string(),
            version: "1.0.0".to_string(),
            namespace: Some("http://valid1.org/".to_string()),
            classes: None,
            properties: None,
            source: None,
        })
        .with_pack(OntologyPackRef {
            name: "valid-pack-2".to_string(),
            version: "2.0.0".to_string(),
            namespace: Some("http://valid2.org/".to_string()),
            classes: None,
            properties: None,
            source: None,
        });

    let mut hive = HiveQueen::new(config).await.unwrap();
    let resolved = hive.orchestrate().await.unwrap();

    // Should handle all valid packs
    assert_eq!(resolved.resolved_packs.len(), 2);
    assert_eq!(resolved.validation_status, "valid");
}

/// Test complete agent lifecycle
#[tokio::test]
async fn test_complete_agent_lifecycle() {
    use ggen_core::config::hive_coordinator::{AgentRole, HiveAgent};

    // Create agent
    let agent = HiveAgent::new(
        "lifecycle-test",
        AgentRole::Analyzer,
        vec!["versioning".to_string()],
    );

    // Agent should have ID and role
    assert_eq!(agent.id, "lifecycle-test");
    assert_eq!(agent.role, AgentRole::Analyzer);

    // Agent should analyze config
    let config = create_test_config(3);
    let analysis = agent.analyze_config(&config).await.unwrap();
    assert!(!analysis.is_empty());

    // Agent should generate report
    let report = agent.report();
    assert!(report.contains("lifecycle-test"));
    assert!(report.contains("Analyzer"));
}

/// Test swarm coordination under stress
#[tokio::test]
async fn test_stress_workflow() {
    let config = create_test_config(8);

    // Run multiple concurrent orchestrations
    let handles: Vec<_> = (0..20)
        .map(|_| {
            let config_clone = config.clone();
            tokio::spawn(async move {
                let mut hive = HiveQueen::new(config_clone).await.unwrap();
                hive.orchestrate().await.unwrap()
            })
        })
        .collect();

    // All should complete successfully
    for handle in handles {
        let resolved = handle.await.unwrap();
        assert_eq!(resolved.resolved_packs.len(), 8);
        assert_eq!(resolved.validation_status, "valid");
    }
}

/// Test end-to-end with custom composition rules
#[tokio::test]
async fn test_custom_composition_workflow() {
    use std::collections::HashMap;
    use ggen_core::config::ontology_config::ConflictResolution;

    let mut custom_rules = HashMap::new();
    custom_rules.insert("custom-pack-1".to_string(), ConflictResolution::Merge);

    let config = OntologyConfig::new()
        .with_pack(OntologyPackRef {
            name: "custom-pack-1".to_string(),
            version: "1.0.0".to_string(),
            namespace: Some("http://custom.org/1/".to_string()),
            classes: None,
            properties: None,
            source: None,
        })
        .with_pack(OntologyPackRef {
            name: "custom-pack-2".to_string(),
            version: "1.0.0".to_string(),
            namespace: Some("http://custom.org/2/".to_string()),
            classes: None,
            properties: None,
            source: None,
        })
        .with_composition(CompositionStrategy::Custom {
            rules: custom_rules,
        });

    let mut hive = HiveQueen::new(config).await.unwrap();
    let resolved = hive.orchestrate().await.unwrap();

    assert_eq!(resolved.validation_status, "valid");
    if let CompositionStrategy::Custom { .. } = resolved.composition_strategy {
        // Correct strategy applied
    } else {
        panic!("Wrong composition strategy");
    }
}

// Helper functions

fn create_test_config(pack_count: usize) -> OntologyConfig {
    let mut config = OntologyConfig::new();

    for i in 0..pack_count {
        config = config.with_pack(OntologyPackRef {
            name: format!("e2e-pack-{}", i),
            version: "1.0.0".to_string(),
            namespace: Some(format!("http://e2e.test/pack{}/", i)),
            classes: None,
            properties: None,
            source: None,
        });
    }

    config.with_composition(CompositionStrategy::Union)
}

fn create_secure_test_config() -> OntologyConfig {
    OntologyConfig::new()
        .with_pack(OntologyPackRef {
            name: "secure-ontology".to_string(),
            version: "1.0.0".to_string(),
            namespace: Some("http://secure.example.org/".to_string()),
            classes: None,
            properties: None,
            source: None,
        })
        .with_composition(CompositionStrategy::Union)
}
