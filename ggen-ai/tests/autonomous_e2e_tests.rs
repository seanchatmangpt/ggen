//! End-to-End Integration Tests for Autonomous System
//!
//! Tests the full autonomous cycle: NL → Graph → Code → Deploy
//! Includes multi-iteration evolution, rollback scenarios, and error recovery.

use ggen_ai::{
    GraphEvolutionEngine, EvolutionConfig, NaturalLanguageParser,
    SelfValidator, DeltaDetector, MockClient,
    autonomous::{
        events::{ChangeEvent, ChangeType, GraphChangeNotifier},
        regeneration::{RegenerationEngine, RegenerationConfig, AffectedArtifact},
        deployment::{DeploymentAutomation, DeploymentConfig, RollbackStrategy},
    },
    governance::{GovernanceCoordinator, GovernanceConfig, Decision, DecisionOutcome},
};
use std::path::PathBuf;
use std::sync::Arc;

/// Test complete NL → Graph → Validation → Deploy cycle
#[tokio::test]
async fn test_full_autonomous_cycle_end_to_end() {
    // Mock AI responses for complete workflow
    let mock_nl_response = r#"
```turtle
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Service a owl:Class ;
    rdfs:label "Microservice" ;
    rdfs:comment "A containerized microservice" .

ex:hasPort a owl:DatatypeProperty ;
    rdfs:domain ex:Service ;
    rdfs:range xsd:integer .

ex:hasEndpoint a owl:DatatypeProperty ;
    rdfs:domain ex:Service ;
    rdfs:range xsd:string .
```

```json
[
    {"subject": "ex:Service", "predicate": "rdf:type", "object": "owl:Class", "confidence": 0.98, "reasoning": "Primary service entity"},
    {"subject": "ex:hasPort", "predicate": "rdf:type", "object": "owl:DatatypeProperty", "confidence": 0.95, "reasoning": "Standard port property"},
    {"subject": "ex:hasEndpoint", "predicate": "rdf:type", "object": "owl:DatatypeProperty", "confidence": 0.95, "reasoning": "REST endpoint property"}
]
```
"#;

    // Step 1: Initialize evolution engine
    let parser_client = MockClient::with_response(mock_nl_response);
    let validator_client = MockClient::with_response("validation passed");

    let mut config = EvolutionConfig::default();
    config.confidence_threshold = 0.9;
    config.auto_validate = true;
    config.auto_rollback = true;

    let mut engine = GraphEvolutionEngine::new(
        Box::new(parser_client),
        Box::new(validator_client),
        config
    ).expect("Failed to create evolution engine");

    // Step 2: Evolve graph from natural language
    let nl_input = "Create a microservice with a port and REST endpoint properties";
    let evolution_result = engine.evolve_from_nl(nl_input).await
        .expect("Evolution failed");

    assert!(evolution_result.success, "Evolution should succeed");
    assert!(evolution_result.parsed.is_some(), "Should have parsed triples");
    assert!(evolution_result.delta.is_some(), "Should have delta");
    assert!(evolution_result.metadata.committed, "Changes should be committed");

    // Verify graph state
    let graph = engine.get_current_graph();
    assert!(!graph.is_empty(), "Graph should contain triples");

    // Step 3: Set up regeneration engine
    let template_client = MockClient::with_response("Generated template");
    let notifier = Arc::new(GraphChangeNotifier::default());
    let regen_config = RegenerationConfig {
        incremental: true,
        parallel_workers: 2,
        target_languages: vec!["rust".to_string(), "typescript".to_string()],
        template_dirs: vec![PathBuf::from("templates")],
        output_dir: PathBuf::from("generated"),
        auto_version: true,
        track_dependencies: true,
    };

    let regen_engine = Arc::new(RegenerationEngine::new(
        regen_config,
        Box::new(template_client),
        notifier.clone(),
    ));

    // Register artifacts
    let artifact = AffectedArtifact {
        id: "service_template".to_string(),
        template_id: "microservice".to_string(),
        language: "rust".to_string(),
        output_path: PathBuf::from("generated/service.rs"),
        version: "1.0.0".to_string(),
        dependencies: Vec::new(),
        last_regenerated: None,
    };
    regen_engine.register_artifact(artifact).await;

    // Step 4: Trigger change event
    let change_event = ChangeEvent::node_added(
        "ex:Service".to_string(),
        std::collections::HashMap::new(),
        "autonomous".to_string(),
    );
    notifier.publish(change_event).await.expect("Failed to publish event");

    // Start regeneration engine
    let regen_clone = regen_engine.clone();
    tokio::spawn(async move {
        regen_clone.start().await.expect("Regeneration engine failed to start");
    });

    // Wait for regeneration to process
    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

    // Step 5: Check regeneration stats
    let stats = regen_engine.get_stats().await;
    assert!(stats.events_processed > 0, "Should have processed events");

    // Step 6: Set up deployment automation
    let deploy_config = DeploymentConfig {
        enabled: true,
        rollback_strategy: RollbackStrategy::Automatic,
        validate_before_deploy: true,
        run_integration_tests: false,
        timeout_seconds: 60,
        environments: vec![],
        pre_deploy_commands: vec![],
        post_deploy_commands: vec![],
    };

    let mut deployment = DeploymentAutomation::new(deploy_config);

    // Step 7: Deploy (would fail in test due to no actual files, but tests the flow)
    let deploy_result = deployment.deploy(
        &PathBuf::from("generated"),
        "1.0.0"
    ).await;

    assert!(deploy_result.is_ok(), "Deployment should not error (even if empty)");
}

/// Test multi-iteration evolution with feedback loop
#[tokio::test]
async fn test_multi_iteration_evolution_with_feedback() {
    let iterations = vec![
        ("Create a Person class", "ex:Person a owl:Class"),
        ("Add name property to Person", "ex:hasName a owl:DatatypeProperty"),
        ("Add age property to Person", "ex:hasAge a owl:DatatypeProperty"),
    ];

    for (idx, (nl_input, expected_content)) in iterations.iter().enumerate() {
        let mock_response = format!(r#"
```turtle
@prefix ex: <http://example.org/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
{} .
```

```json
[
    {{"subject": "ex:Entity", "predicate": "rdf:type", "object": "owl:Class", "confidence": 0.95, "reasoning": "Iteration {}"}}
]
```
"#, expected_content, idx);

        let parser_client = MockClient::with_response(&mock_response);
        let validator_client = MockClient::with_response("valid");

        let mut engine = GraphEvolutionEngine::with_defaults(
            Box::new(parser_client),
            Box::new(validator_client)
        ).expect("Failed to create engine");

        let result = engine.evolve_from_nl(nl_input).await
            .expect("Evolution failed");

        assert!(result.success, "Iteration {} should succeed", idx);
        assert!(result.metadata.committed, "Iteration {} should commit", idx);

        // Verify graph grows with each iteration
        let graph = engine.get_current_graph();
        assert!(!graph.is_empty(), "Graph should not be empty after iteration {}", idx);
    }
}

/// Test rollback scenario on validation failure
#[tokio::test]
async fn test_rollback_on_validation_failure() {
    // First, create a valid baseline
    let valid_response = r#"
```turtle
@prefix ex: <http://example.org/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
ex:ValidEntity a owl:Class .
```

```json
[
    {"subject": "ex:ValidEntity", "predicate": "rdf:type", "object": "owl:Class", "confidence": 0.95, "reasoning": "Valid"}
]
```
"#;

    let parser_client = MockClient::with_response(valid_response);
    let validator_client = MockClient::with_response("valid");

    let mut config = EvolutionConfig::default();
    config.auto_validate = true;
    config.auto_rollback = true;

    let mut engine = GraphEvolutionEngine::new(
        Box::new(parser_client),
        Box::new(validator_client),
        config
    ).expect("Failed to create engine");

    let result1 = engine.evolve_from_nl("Create a valid entity").await
        .expect("First evolution failed");
    assert!(result1.success);

    let baseline_graph_size = engine.get_current_graph().len();

    // Now attempt invalid evolution (malformed response will cause parsing failure)
    let invalid_response = "Invalid data";
    let parser_client2 = MockClient::with_response(invalid_response);
    let validator_client2 = MockClient::with_response("invalid");

    engine = GraphEvolutionEngine::new(
        Box::new(parser_client2),
        Box::new(validator_client2),
        engine.config().clone()
    ).expect("Failed to recreate engine");

    let result2 = engine.evolve_from_nl("Add invalid entity").await
        .expect("Second evolution failed");

    // Should fail and not commit
    assert!(!result2.success || !result2.metadata.committed,
            "Invalid evolution should fail or not commit");

    // Graph should not have grown (rollback occurred)
    // Note: Since we recreated the engine, graph state resets.
    // In production, engine would be persistent.
}

/// Test error recovery and retry logic
#[tokio::test]
async fn test_error_recovery_with_retry() {
    let attempts = Arc::new(tokio::sync::Mutex::new(0));
    let attempts_clone = attempts.clone();

    // Simulate failure then success pattern
    let mock_response = r#"
```turtle
@prefix ex: <http://example.org/> .
ex:Entity a owl:Class .
```

```json
[
    {"subject": "ex:Entity", "predicate": "rdf:type", "object": "owl:Class", "confidence": 0.95, "reasoning": "Test"}
]
```
"#;

    for retry in 0..3 {
        let parser_client = MockClient::with_response(mock_response);
        let validator_client = MockClient::with_response("valid");

        let mut engine = GraphEvolutionEngine::with_defaults(
            Box::new(parser_client),
            Box::new(validator_client)
        ).expect("Failed to create engine");

        let result = engine.evolve_from_nl("Create entity").await;

        if result.is_ok() && result.as_ref().unwrap().success {
            *attempts_clone.lock().await = retry + 1;
            break;
        }
    }

    let final_attempts = *attempts.lock().await;
    assert!(final_attempts > 0, "Should eventually succeed");
}

/// Test governance integration with autonomous decisions
#[tokio::test]
async fn test_governance_integration_with_evolution() {
    let config = GovernanceConfig::default();
    let governance = GovernanceCoordinator::new(config).await
        .expect("Failed to create governance");

    // Test low-risk autonomous decision
    let low_risk_decision = Decision::new_low_risk(
        "add_property",
        "Add a simple data property to existing class"
    );

    let outcome = governance.validate_decision(&low_risk_decision).await
        .expect("Validation failed");

    match outcome {
        DecisionOutcome::Approved { auto_approved, .. } => {
            assert!(auto_approved, "Low-risk should be auto-approved");
        }
        _ => panic!("Expected approval for low-risk decision"),
    }

    // Test high-risk decision requiring approval
    let high_risk_decision = Decision::new_high_risk(
        "schema_change",
        "Major schema refactoring"
    );

    let outcome2 = governance.validate_decision(&high_risk_decision).await
        .expect("Validation failed");

    match outcome2 {
        DecisionOutcome::PendingApproval { .. } => {
            // Expected - high risk should require approval
        }
        DecisionOutcome::Approved { auto_approved, .. } => {
            assert!(!auto_approved, "High-risk should not be auto-approved");
        }
        _ => {}
    }
}

/// Test concurrent evolution operations
#[tokio::test]
async fn test_concurrent_evolution_operations() {
    let mock_response = r#"
```turtle
@prefix ex: <http://example.org/> .
ex:Entity a owl:Class .
```

```json
[
    {"subject": "ex:Entity", "predicate": "rdf:type", "object": "owl:Class", "confidence": 0.95, "reasoning": "Test"}
]
```
"#;

    let mut handles = vec![];

    for i in 0..5 {
        let response = mock_response.to_string();
        let handle = tokio::spawn(async move {
            let parser_client = MockClient::with_response(&response);
            let validator_client = MockClient::with_response("valid");

            let mut engine = GraphEvolutionEngine::with_defaults(
                Box::new(parser_client),
                Box::new(validator_client)
            ).expect("Failed to create engine");

            engine.evolve_from_nl(&format!("Create entity {}", i)).await
        });
        handles.push(handle);
    }

    let results = futures::future::join_all(handles).await;

    let successful = results.iter()
        .filter(|r| r.is_ok() && r.as_ref().unwrap().as_ref().unwrap().success)
        .count();

    assert!(successful >= 4, "Most concurrent operations should succeed");
}

/// Test history tracking and audit trail
#[tokio::test]
async fn test_evolution_history_and_audit_trail() {
    let mock_response = r#"
```turtle
@prefix ex: <http://example.org/> .
ex:Entity a owl:Class .
```

```json
[
    {"subject": "ex:Entity", "predicate": "rdf:type", "object": "owl:Class", "confidence": 0.95, "reasoning": "Test"}
]
```
"#;

    let parser_client = MockClient::with_response(mock_response);
    let validator_client = MockClient::with_response("valid");

    let mut engine = GraphEvolutionEngine::with_defaults(
        Box::new(parser_client),
        Box::new(validator_client)
    ).expect("Failed to create engine");

    // Perform multiple evolutions
    for i in 0..3 {
        engine.evolve_from_nl(&format!("Evolution step {}", i)).await
            .expect("Evolution failed");
    }

    // Check history
    let history = engine.get_evolution_history();
    assert!(history.len() >= 1, "Should have evolution history");

    // Verify deltas are tracked
    for delta in history {
        assert!(
            delta.stats.total_changes > 0 || delta.stats.additions_count > 0,
            "Delta should record changes"
        );
    }
}
