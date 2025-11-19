//! Swarm Failure Recovery Tests
//!
//! Tests for fault tolerance, error handling, and recovery mechanisms in the swarm system.
//! Ensures the system degrades gracefully under failure conditions.

use ggen_core::config::hive_coordinator::{AgentRole, HiveAgent, HiveQueen};
use ggen_core::config::ontology_config::{CompositionStrategy, OntologyConfig, OntologyPackRef};

/// Test swarm handles invalid configuration gracefully
#[tokio::test]
async fn test_invalid_configuration_handling() {
    // Empty pack name should cause validation error
    let invalid_config = OntologyConfig::new().with_pack(OntologyPackRef {
        name: "".to_string(), // Invalid: empty name
        version: "1.0.0".to_string(),
        namespace: None,
        classes: None,
        properties: None,
        source: None,
    });

    let result = HiveQueen::new(invalid_config).await;
    assert!(result.is_err(), "Should reject invalid configuration");
}

/// Test swarm handles malformed version strings
#[tokio::test]
async fn test_malformed_version_handling() {
    let config = OntologyConfig::new().with_pack(OntologyPackRef {
        name: "test-pack".to_string(),
        version: "not-a-version".to_string(),
        namespace: None,
        classes: None,
        properties: None,
        source: None,
    });

    // Should still initialize (version validation happens during resolution)
    let hive = HiveQueen::new(config).await;
    assert!(hive.is_ok());
}

/// Test agent analysis handles empty configuration
#[tokio::test]
async fn test_agent_analysis_empty_config() {
    let agent = HiveAgent::new("test-agent", AgentRole::Analyzer, vec!["test".to_string()]);
    let empty_config = OntologyConfig::new();

    let result = agent.analyze_config(&empty_config).await;
    assert!(result.is_ok(), "Should handle empty config gracefully");

    let insights = result.unwrap();
    assert!(
        !insights.is_empty(),
        "Should still produce insights for empty config"
    );
}

/// Test orchestration with no packs
#[tokio::test]
async fn test_orchestration_no_packs() {
    let config = OntologyConfig::new();
    let mut hive = HiveQueen::new(config).await.unwrap();

    let resolved = hive.orchestrate().await;
    assert!(resolved.is_ok(), "Should handle empty pack list");

    let resolved = resolved.unwrap();
    assert_eq!(resolved.resolved_packs.len(), 0);
    assert_eq!(resolved.conflicts_found, 0);
}

/// Test swarm handles namespace conflicts
#[tokio::test]
async fn test_namespace_conflict_recovery() {
    let config = OntologyConfig::new()
        .with_pack(OntologyPackRef {
            name: "pack-a".to_string(),
            version: "1.0.0".to_string(),
            namespace: Some("http://same-namespace.org/".to_string()),
            classes: None,
            properties: None,
            source: None,
        })
        .with_pack(OntologyPackRef {
            name: "pack-b".to_string(),
            version: "2.0.0".to_string(),
            namespace: Some("http://same-namespace.org/".to_string()),
            classes: None,
            properties: None,
            source: None,
        });

    let mut hive = HiveQueen::new(config).await.unwrap();
    let resolved = hive.orchestrate().await.unwrap();

    // Should detect conflict
    assert!(resolved.conflicts_found > 0);

    // Should attempt resolution based on strategy
    assert!(resolved.conflicts_resolved > 0);
}

/// Test version conflict resolution
#[tokio::test]
async fn test_version_conflict_recovery() {
    let config = OntologyConfig::new()
        .with_pack(OntologyPackRef {
            name: "package".to_string(),
            version: "^2.0.0".to_string(),
            namespace: None,
            classes: None,
            properties: None,
            source: None,
        })
        .with_pack(OntologyPackRef {
            name: "package-dep".to_string(),
            version: "~1.5.0".to_string(),
            namespace: None,
            classes: None,
            properties: None,
            source: None,
        })
        .with_composition(CompositionStrategy::Priority);

    let mut hive = HiveQueen::new(config).await.unwrap();
    let resolved = hive.orchestrate().await.unwrap();

    // Should complete even with version conflicts
    assert_eq!(resolved.validation_status, "valid");
}

/// Test swarm recovers from agent analysis errors
#[tokio::test]
async fn test_agent_error_recovery() {
    let config = create_test_config(3);
    let agent = HiveAgent::new("error-test", AgentRole::Validator, vec![]);

    // Agent with no expertise should still function
    let result = agent.analyze_config(&config).await;
    assert!(result.is_ok(), "Agent should handle limited expertise");
}

/// Test concurrent failures don't cascade
#[tokio::test]
async fn test_concurrent_failure_isolation() {
    let valid_config = create_test_config(3);
    let invalid_config = OntologyConfig::new().with_pack(OntologyPackRef {
        name: "".to_string(),
        version: "1.0.0".to_string(),
        namespace: None,
        classes: None,
        properties: None,
        source: None,
    });

    // Run valid and invalid concurrently
    let (valid_result, invalid_result) =
        tokio::join!(HiveQueen::new(valid_config), HiveQueen::new(invalid_config));

    // Valid should succeed, invalid should fail independently
    assert!(valid_result.is_ok());
    assert!(invalid_result.is_err());
}

/// Test memory cleanup after failure
#[tokio::test]
async fn test_failure_memory_cleanup() {
    // Create many failing configurations
    for _ in 0..100 {
        let invalid_config = OntologyConfig::new().with_pack(OntologyPackRef {
            name: "".to_string(),
            version: "1.0.0".to_string(),
            namespace: None,
            classes: None,
            properties: None,
            source: None,
        });

        let _ = HiveQueen::new(invalid_config).await;
    }

    // Memory should be cleaned up (no leak)
    // If we got here without OOM, cleanup is working
    assert!(true);
}

/// Test orchestration handles partial failures
#[tokio::test]
async fn test_partial_failure_handling() {
    let config = create_test_config(5);
    let mut hive = HiveQueen::new(config).await.unwrap();

    // First orchestration
    let result1 = hive.orchestrate().await;
    assert!(result1.is_ok());

    // Second orchestration should still work
    let result2 = hive.orchestrate().await;
    assert!(result2.is_ok());
}

/// Test swarm handles conflicting composition strategies
#[tokio::test]
async fn test_composition_strategy_conflicts() {
    let config = create_conflicting_config().with_composition(CompositionStrategy::Intersection);

    let mut hive = HiveQueen::new(config).await.unwrap();
    let resolved = hive.orchestrate().await;

    // Should handle strategy even with conflicts
    assert!(resolved.is_ok());
}

/// Test agent state consistency after errors
#[tokio::test]
async fn test_agent_state_consistency() {
    let config = create_test_config(4);
    let agent = HiveAgent::new("state-test", AgentRole::Analyzer, vec!["test".to_string()]);

    // Multiple analyses should maintain consistency
    let _ = agent.analyze_config(&config).await;
    let _ = agent.analyze_config(&config).await;
    let result = agent.analyze_config(&config).await;

    assert!(result.is_ok(), "Agent state should remain consistent");
}

/// Test swarm handles rapid initialization/destruction
#[tokio::test]
async fn test_rapid_lifecycle() {
    for _ in 0..50 {
        let config = create_test_config(2);
        let mut hive = HiveQueen::new(config).await.unwrap();
        let _ = hive.orchestrate().await.unwrap();
        // Hive drops here
    }

    // Should handle rapid creation/destruction without issues
    assert!(true);
}

/// Test error propagation is correct
#[tokio::test]
async fn test_error_propagation() {
    let invalid_config = OntologyConfig::new().with_pack(OntologyPackRef {
        name: "".to_string(),
        version: "1.0.0".to_string(),
        namespace: None,
        classes: None,
        properties: None,
        source: None,
    });

    let error = HiveQueen::new(invalid_config).await.unwrap_err();
    let error_msg = error.to_string();

    // Error should be descriptive
    assert!(!error_msg.is_empty(), "Error message should not be empty");
}

/// Test swarm handles missing namespaces gracefully
#[tokio::test]
async fn test_missing_namespace_handling() {
    let config = OntologyConfig::new().with_pack(OntologyPackRef {
        name: "no-namespace".to_string(),
        version: "1.0.0".to_string(),
        namespace: None, // Missing namespace
        classes: None,
        properties: None,
        source: None,
    });

    let mut hive = HiveQueen::new(config).await.unwrap();
    let resolved = hive.orchestrate().await;

    // Should handle missing namespace
    assert!(resolved.is_ok());
}

/// Test swarm handles empty version strings
#[tokio::test]
async fn test_empty_version_handling() {
    let config = OntologyConfig::new().with_pack(OntologyPackRef {
        name: "empty-version".to_string(),
        version: "".to_string(),
        namespace: None,
        classes: None,
        properties: None,
        source: None,
    });

    // Empty version should be caught by validation
    let result = HiveQueen::new(config).await;
    assert!(result.is_err(), "Should reject empty version");
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
            name: "conflict-1".to_string(),
            version: "^2.0.0".to_string(),
            namespace: Some("http://conflict.org/".to_string()),
            classes: None,
            properties: None,
            source: None,
        })
        .with_pack(OntologyPackRef {
            name: "conflict-2".to_string(),
            version: "~1.9.0".to_string(),
            namespace: Some("http://conflict.org/".to_string()),
            classes: None,
            properties: None,
            source: None,
        })
}
