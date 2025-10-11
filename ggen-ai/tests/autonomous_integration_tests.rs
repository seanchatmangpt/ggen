//! Integration tests for autonomous graph evolution system

use ggen_ai::providers::MockClient;
use ggen_ai::{
    DeltaDetector, EvolutionConfig, GraphEvolutionEngine, LlmConfig, NaturalLanguageParser,
    SelfValidator,
};

#[tokio::test]
async fn test_end_to_end_evolution() {
    // Mock AI responses for complete workflow
    let mock_nl_response = r#"
```turtle
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Person a owl:Class ;
    rdfs:label "Person" ;
    rdfs:comment "A human being" .

ex:hasName a owl:DatatypeProperty ;
    rdfs:domain ex:Person ;
    rdfs:range xsd:string .
```

```json
[
    {"subject": "ex:Person", "predicate": "rdf:type", "object": "owl:Class", "confidence": 0.95, "reasoning": "Explicitly stated as a class"},
    {"subject": "ex:hasName", "predicate": "rdf:type", "object": "owl:DatatypeProperty", "confidence": 0.90, "reasoning": "Inferred from domain and range"}
]
```
"#;

    let parser_client = MockClient::with_response(mock_nl_response);
    let validator_client = MockClient::with_response("validation");
    let mut engine =
        GraphEvolutionEngine::with_defaults(Box::new(parser_client), Box::new(validator_client))
            .expect("Failed to create engine");

    // Evolve graph from natural language
    let result = engine
        .evolve_from_nl("A Person has a name property")
        .await
        .expect("Evolution failed");

    assert!(result.success, "Evolution should succeed");
    assert!(result.parsed.is_some(), "Should have parsed triples");
    assert!(result.delta.is_some(), "Should have delta");
    assert!(result.metadata.committed, "Changes should be committed");

    // Verify graph state
    let graph = engine.get_current_graph();
    assert!(!graph.is_empty(), "Graph should not be empty");

    // Export Turtle
    let turtle = engine.export_turtle();
    assert!(turtle.contains("@prefix"), "Should have prefixes");
    assert!(turtle.contains("ex:Person"), "Should contain Person class");
}

#[tokio::test]
async fn test_nl_parser_integration() {
    let mock_response = r#"
```turtle
ex:Book a owl:Class .
ex:title a owl:DatatypeProperty .
```

```json
[
    {"subject": "ex:Book", "predicate": "rdf:type", "object": "owl:Class", "confidence": 0.92, "reasoning": "Primary entity"}
]
```
"#;

    let client = MockClient::with_response(mock_response);
    let parser = NaturalLanguageParser::new(Box::new(client));

    let parsed = parser
        .parse_to_rdf("A book has a title")
        .await
        .expect("Parsing failed");

    assert!(!parsed.triples.is_empty());
    assert!(!parsed.relations.is_empty());
    assert!(parsed.relations[0].confidence > 0.0);
}

#[tokio::test]
async fn test_validator_integration() {
    let client = MockClient::with_response("Validation queries generated");
    let validator = SelfValidator::new(Box::new(client)).expect("Failed to create validator");

    let triples = vec![
        "@prefix ex: <http://example.org/> .".to_string(),
        "@prefix owl: <http://www.w3.org/2002/07/owl#> .".to_string(),
        "ex:Person a owl:Class .".to_string(),
    ];

    let result = validator
        .validate(&triples)
        .await
        .expect("Validation failed");

    assert!(!result.queries_executed.is_empty());
}

#[test]
fn test_delta_detector_integration() {
    let mut detector = DeltaDetector::new().expect("Failed to create detector");

    let baseline = vec![
        "ex:Person rdf:type owl:Class .".to_string(),
        "ex:Person rdfs:label \"Person\" .".to_string(),
    ];

    detector
        .set_baseline(&baseline)
        .expect("Failed to set baseline");

    let new_state = vec![
        "ex:Person rdf:type owl:Class .".to_string(),
        "ex:Person rdfs:label \"Person\" .".to_string(),
        "ex:Person rdfs:comment \"A human\" .".to_string(), // New triple
    ];

    let delta = detector
        .compute_delta(&new_state)
        .expect("Failed to compute delta");

    assert_eq!(delta.stats.additions_count, 1);
    assert_eq!(delta.stats.deletions_count, 0);
    assert!(delta.stats.total_changes > 0);
}

#[tokio::test]
async fn test_confidence_threshold_filtering() {
    let mock_response = r#"
```turtle
ex:LowConfidence a owl:Class .
ex:HighConfidence a owl:Class .
```

```json
[
    {"subject": "ex:LowConfidence", "predicate": "rdf:type", "object": "owl:Class", "confidence": 0.5, "reasoning": "Uncertain"},
    {"subject": "ex:HighConfidence", "predicate": "rdf:type", "object": "owl:Class", "confidence": 0.95, "reasoning": "Certain"}
]
```
"#;

    let parser_client = MockClient::with_response(mock_response);
    let validator_client = MockClient::with_response("validation");

    let mut config = EvolutionConfig::default();
    config.confidence_threshold = 0.8; // High threshold

    let mut engine =
        GraphEvolutionEngine::new(Box::new(parser_client), Box::new(validator_client), config)
            .expect("Failed to create engine");

    let result = engine
        .evolve_from_nl("Test entities")
        .await
        .expect("Evolution failed");

    // Low confidence triples should be filtered out
    assert!(result.success);
}

#[tokio::test]
async fn test_validation_rollback() {
    let mock_response = "Invalid triples";
    let parser_client = MockClient::with_response(mock_response);
    let validator_client = MockClient::with_response("validation");

    let mut config = EvolutionConfig::default();
    config.auto_validate = true;
    config.auto_rollback = true;

    let mut engine =
        GraphEvolutionEngine::new(Box::new(parser_client), Box::new(validator_client), config)
            .expect("Failed to create engine");

    let result = engine
        .evolve_from_nl("Invalid input")
        .await
        .expect("Evolution failed");

    // Should fail due to validation/parsing issues
    assert!(!result.metadata.committed || !result.success);
}

#[tokio::test]
async fn test_batch_parsing() {
    let mock_response = r#"
```turtle
ex:Entity a owl:Class .
```

```json
[
    {"subject": "ex:Entity", "predicate": "rdf:type", "object": "owl:Class", "confidence": 0.9, "reasoning": "Test"}
]
```
"#;

    let client = MockClient::with_response(mock_response);
    let parser = NaturalLanguageParser::new(Box::new(client));

    let texts = vec!["First entity", "Second entity", "Third entity"];

    let results = parser
        .batch_parse(texts)
        .await
        .expect("Batch parsing failed");

    // Should parse multiple texts
    assert!(!results.is_empty());
}

#[test]
fn test_evolution_history_tracking() {
    let mut detector = DeltaDetector::new().expect("Failed to create detector");

    let baseline = vec!["ex:A rdf:type owl:Class .".to_string()];
    detector.set_baseline(&baseline).unwrap();

    // Make several changes
    let state1 = vec![
        "ex:A rdf:type owl:Class .".to_string(),
        "ex:A rdfs:label \"A\" .".to_string(),
    ];
    detector.compute_delta(&state1).unwrap();

    let state2 = vec![
        "ex:A rdf:type owl:Class .".to_string(),
        "ex:A rdfs:label \"A\" .".to_string(),
        "ex:A rdfs:comment \"Comment\" .".to_string(),
    ];
    detector.compute_delta(&state2).unwrap();

    let history = detector.get_history();
    assert_eq!(history.len(), 2, "Should track multiple deltas");
}

#[test]
fn test_regeneration_threshold() {
    let mock_response = "test";
    let parser_client = MockClient::with_response(mock_response);
    let validator_client = MockClient::with_response("validation");

    let mut config = EvolutionConfig::default();
    config.regeneration_threshold = 3;

    let engine =
        GraphEvolutionEngine::new(Box::new(parser_client), Box::new(validator_client), config)
            .expect("Failed to create engine");

    // Initially should not need regeneration
    assert!(!engine.needs_regeneration());
}
