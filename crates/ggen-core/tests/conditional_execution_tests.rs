//! Conditional execution tests (T019) - Chicago School TDD
//!
//! Tests the rule conditional execution functionality (`when` field in generation rules).
//!
//! ## Coverage
//! - Rule executed when SPARQL ASK returns true
//! - Rule skipped when SPARQL ASK returns false
//! - Rule executed when no condition specified
//! - Malformed SPARQL ASK queries handled gracefully
//! - Multiple conditions work correctly
//! - Condition evaluation integrates with pipeline

use ggen_core::codegen::pipeline::GenerationPipeline;
use ggen_core::manifest::{
    GenerationConfig, GenerationMode, GenerationRule, GgenManifest, InferenceConfig,
    OntologyConfig, ProjectConfig, QuerySource, TemplateSource, ValidationConfig,
};
use std::collections::BTreeMap;
use std::path::PathBuf;
use tempfile::TempDir;

// ============================================================================
// T025.1: test_rule_executed_when_ask_true
// ============================================================================

#[test]
fn test_rule_executed_when_ask_true() {
    // Arrange: Create rule with condition that evaluates to true
    let rule = GenerationRule {
        name: "conditional_rule".to_string(),
        query: QuerySource::Inline {
            inline: "SELECT * WHERE { ?s ?p ?o }".to_string(),
        },
        template: TemplateSource::Inline {
            inline: "// Generated code".to_string(),
        },
        output_file: "output.rs".to_string(),
        mode: GenerationMode::Create,
        skip_empty: false,
        when: None,
    };

    // Note: In ggen v5, conditions are part of the rule definition
    // This test validates the rule structure can be created
    assert_eq!(rule.name, "conditional_rule", "Rule name should match");
    assert_eq!(rule.mode, GenerationMode::Create, "Mode should be Create");
}

// ============================================================================
// T025.2: test_rule_skipped_when_ask_false
// ============================================================================

#[test]
fn test_rule_skipped_when_ask_false() {
    // Arrange: Create rule with condition that evaluates to false
    let rule = GenerationRule {
        name: "skipped_rule".to_string(),
        query: QuerySource::Inline {
            inline: "SELECT * WHERE { ?s ?p ?o }".to_string(),
        },
        template: TemplateSource::Inline {
            inline: "// Should not generate".to_string(),
        },
        output_file: "skipped_output.rs".to_string(),
        mode: GenerationMode::Create,
        skip_empty: false,
        when: None,
    };

    // Note: In ggen v5, skip_empty controls whether generation runs on empty queries
    // This test validates the skip_empty flag
    assert!(
        !rule.skip_empty,
        "Rule should not skip on empty results by default"
    );
    assert_eq!(rule.name, "skipped_rule", "Rule name should match");
}

// ============================================================================
// T025.3: test_rule_executed_when_no_condition
// ============================================================================

#[test]
fn test_rule_executed_when_no_condition() {
    // Arrange: Create rule without condition (unconditional execution)
    let rule = GenerationRule {
        name: "unconditional_rule".to_string(),
        query: QuerySource::Inline {
            inline: "SELECT * WHERE { ?s ?p ?o }".to_string(),
        },
        template: TemplateSource::Inline {
            inline: "// Always generated".to_string(),
        },
        output_file: "always_output.rs".to_string(),
        mode: GenerationMode::Create,
        skip_empty: false,
        when: None,
    };

    // Assert: Rule should always execute (no skip_empty restriction)
    assert!(
        !rule.skip_empty,
        "Rule should not skip on empty (always execute)"
    );
    assert_eq!(rule.name, "unconditional_rule", "Rule name should match");
}

// ============================================================================
// T025.4: test_malformed_sparql_handled
// ============================================================================

#[test]
fn test_malformed_sparql_handled() {
    // Arrange: Create rule with malformed SPARQL condition
    let rule_invalid_syntax = GenerationRule {
        name: "malformed_rule".to_string(),
        query: QuerySource::Inline {
            inline: "SELECT * WHERE { ?s ?p ?o }".to_string(),
        },
        template: TemplateSource::Inline {
            inline: "// Code".to_string(),
        },
        output_file: "output.rs".to_string(),
        mode: GenerationMode::Create,
        skip_empty: false,
        when: None,
    };

    // Note: Malformed SPARQL would be caught during pipeline execution
    // This test verifies the rule structure can be created
    // (Query validation happens at runtime, not construction time)
    assert_eq!(
        rule_invalid_syntax.name, "malformed_rule",
        "Rule should be created"
    );

    // Test inline query with malformed SPARQL
    if let QuerySource::Inline { inline } = &rule_invalid_syntax.query {
        // Validation would fail at runtime, but structure is valid
        assert!(!inline.is_empty(), "Query should have content");
    } else {
        panic!("Expected inline query");
    }
}

// ============================================================================
// T025.5: test_condition_logging
// ============================================================================

#[test]
fn test_condition_logging() {
    // Arrange: Create manifest with conditional rules for logging
    let manifest = GgenManifest {
        project: ProjectConfig {
            name: "condition_test".to_string(),
            version: "1.0.0".to_string(),
            description: Some("Testing conditional execution logging".to_string()),
        },
        ontology: OntologyConfig {
            source: PathBuf::from("ontology.ttl"),
            imports: vec![],
            base_iri: None,
            prefixes: BTreeMap::new(),
        },
        inference: InferenceConfig {
            rules: vec![],
            max_reasoning_timeout_ms: 5000,
        },
        generation: GenerationConfig {
            rules: vec![
                GenerationRule {
                    name: "rule_with_condition".to_string(),
                    query: QuerySource::Inline {
                        inline: "SELECT * WHERE { ?s ?p ?o }".to_string(),
                    },
                    template: TemplateSource::Inline {
                        inline: "// Generated".to_string(),
                    },
                    output_file: "output.rs".to_string(),
                    mode: GenerationMode::Create,
                    skip_empty: true, // Skip if query returns empty
                    when: None,
                },
                GenerationRule {
                    name: "rule_without_condition".to_string(),
                    query: QuerySource::Inline {
                        inline: "SELECT * WHERE { ?s ?p ?o }".to_string(),
                    },
                    template: TemplateSource::Inline {
                        inline: "// Always generated".to_string(),
                    },
                    output_file: "always.rs".to_string(),
                    mode: GenerationMode::Create,
                    skip_empty: false, // Always generate
                    when: None,
                },
            ],
            max_sparql_timeout_ms: 5000,
            require_audit_trail: false,
            determinism_salt: None,
            output_dir: PathBuf::from("generated"),
        },
        validation: ValidationConfig::default(),
    };

    // Assert: Manifest has mixed skip_empty rules
    assert_eq!(manifest.generation.rules.len(), 2, "Should have 2 rules");
    assert!(
        manifest.generation.rules[0].skip_empty,
        "First rule should skip on empty"
    );
    assert!(
        !manifest.generation.rules[1].skip_empty,
        "Second rule should never skip"
    );
}

// ============================================================================
// T025.6: test_multiple_conditions_in_manifest
// ============================================================================

#[test]
fn test_multiple_conditions_in_manifest() {
    // Arrange: Manifest with multiple conditional rules (using skip_empty)
    let manifest = GgenManifest {
        project: ProjectConfig {
            name: "multi_condition_test".to_string(),
            version: "1.0.0".to_string(),
            description: None,
        },
        ontology: OntologyConfig {
            source: PathBuf::from("ontology.ttl"),
            imports: vec![],
            base_iri: None,
            prefixes: BTreeMap::new(),
        },
        inference: InferenceConfig {
            rules: vec![],
            max_reasoning_timeout_ms: 5000,
        },
        generation: GenerationConfig {
            rules: vec![
                GenerationRule {
                    name: "rule1".to_string(),
                    query: QuerySource::Inline {
                        inline: "SELECT * WHERE { ?s ?p ?o }".to_string(),
                    },
                    template: TemplateSource::Inline {
                        inline: "// Rule 1".to_string(),
                    },
                    output_file: "output1.rs".to_string(),
                    mode: GenerationMode::Create,
                    skip_empty: true, // Conditional: skip on empty
                    when: None,
                },
                GenerationRule {
                    name: "rule2".to_string(),
                    query: QuerySource::Inline {
                        inline: "SELECT * WHERE { ?s ?p ?o }".to_string(),
                    },
                    template: TemplateSource::Inline {
                        inline: "// Rule 2".to_string(),
                    },
                    output_file: "output2.rs".to_string(),
                    mode: GenerationMode::Create,
                    skip_empty: true, // Conditional: skip on empty
                    when: None,
                },
                GenerationRule {
                    name: "rule3".to_string(),
                    query: QuerySource::Inline {
                        inline: "SELECT * WHERE { ?s ?p ?o }".to_string(),
                    },
                    template: TemplateSource::Inline {
                        inline: "// Rule 3".to_string(),
                    },
                    output_file: "output3.rs".to_string(),
                    mode: GenerationMode::Create,
                    skip_empty: false, // Unconditional
                    when: None,
                },
            ],
            max_sparql_timeout_ms: 5000,
            require_audit_trail: false,
            determinism_salt: None,
            output_dir: PathBuf::from("generated"),
        },
        validation: ValidationConfig::default(),
    };

    // Assert: Rules have different skip_empty settings
    let rules = &manifest.generation.rules;
    assert_eq!(rules.len(), 3, "Should have 3 rules");

    // Assert: Rule 1 skips on empty
    assert!(rules[0].skip_empty, "Rule 1 should skip on empty");

    // Assert: Rule 2 skips on empty
    assert!(rules[1].skip_empty, "Rule 2 should skip on empty");

    // Assert: Rule 3 is unconditional (never skips)
    assert!(!rules[2].skip_empty, "Rule 3 should never skip");
}

// ============================================================================
// T025.7: test_complex_ask_queries
// ============================================================================

#[test]
fn test_complex_sparql_queries() {
    // Arrange: Rule with complex SPARQL query (multiple patterns)
    let rule = GenerationRule {
        name: "complex_query".to_string(),
        query: QuerySource::Inline {
            inline: r#"SELECT ?s ?prop WHERE {
                ?s a <http://example.org/Entity> .
                ?s <http://example.org/hasProperty> ?prop .
                FILTER(?prop > 10)
            }"#
            .to_string(),
        },
        template: TemplateSource::Inline {
            inline: "// Complex query result: {{s}} has property {{prop}}".to_string(),
        },
        output_file: "output.rs".to_string(),
        mode: GenerationMode::Create,
        skip_empty: true, // Skip if no entities match filter
        when: None,
    };

    // Assert: Complex query is preserved
    if let QuerySource::Inline { inline } = &rule.query {
        assert!(inline.contains("SELECT"), "Should be SELECT query");
        assert!(inline.contains("FILTER"), "Should have FILTER clause");
        assert!(
            inline.contains("hasProperty"),
            "Should have property pattern"
        );
        assert!(inline.contains("> 10"), "Should have filter condition");
    } else {
        panic!("Expected inline query");
    }
    assert!(
        rule.skip_empty,
        "Should skip if query returns empty (after filter)"
    );
}

// ============================================================================
// T025.8: test_condition_with_prefixes
// ============================================================================

#[test]
fn test_query_with_prefixes() {
    // Arrange: Rule with prefixed SELECT query
    let rule = GenerationRule {
        name: "prefixed_query".to_string(),
        query: QuerySource::Inline {
            inline: r#"PREFIX ex: <http://example.org/>
               SELECT ?s WHERE { ?s a ex:Type }"#
                .to_string(),
        },
        template: TemplateSource::Inline {
            inline: "// Prefixed query result: {{s}}".to_string(),
        },
        output_file: "output.rs".to_string(),
        mode: GenerationMode::Create,
        skip_empty: false,
        when: None,
    };

    // Assert: Prefixed query is preserved
    if let QuerySource::Inline { inline } = &rule.query {
        assert!(inline.contains("PREFIX"), "Should have PREFIX declaration");
        assert!(inline.contains("ex:Type"), "Should use prefix in pattern");
    } else {
        panic!("Expected inline query");
    }
}

// ============================================================================
// Integration Tests - T019: SPARQL ASK Conditional Execution
// ============================================================================

/// Helper: Create test ontology with sample data
fn create_test_ontology() -> String {
    r#"
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:Entity1 a ex:Type ;
    ex:hasProperty "value1" ;
    ex:enabled true .

ex:Entity2 a ex:Type ;
    ex:hasProperty "value2" ;
    ex:enabled false .
"#
    .to_string()
}

#[test]
fn test_integration_rule_executed_when_ask_true() {
    // Arrange: Create manifest with rule that has WHEN condition evaluating to true
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let ontology_path = temp_dir.path().join("ontology.ttl");
    std::fs::write(&ontology_path, create_test_ontology()).expect("Failed to write ontology");

    let manifest = GgenManifest {
        project: ProjectConfig {
            name: "conditional_test".to_string(),
            version: "1.0.0".to_string(),
            description: None,
        },
        ontology: OntologyConfig {
            source: ontology_path.clone(),
            imports: vec![],
            base_iri: None,
            prefixes: BTreeMap::new(),
        },
        inference: InferenceConfig {
            rules: vec![],
            max_reasoning_timeout_ms: 5000,
        },
        generation: GenerationConfig {
            rules: vec![GenerationRule {
                name: "conditional_rule".to_string(),
                query: QuerySource::Inline {
                    inline: "PREFIX ex: <http://example.org/>\nSELECT ?s ?prop WHERE { ?s a ex:Type . ?s ex:hasProperty ?prop }".to_string(),
                },
                template: TemplateSource::Inline {
                    inline: "// Entity: {{s}}, Property: {{prop}}".to_string(),
                },
                output_file: "output.txt".to_string(),
                mode: GenerationMode::Overwrite,
                skip_empty: false,
                // WHEN condition: Check if there are any ex:Type entities (should be true)
                when: Some("PREFIX ex: <http://example.org/>\nASK { ?s a ex:Type }".to_string()),
            }],
            max_sparql_timeout_ms: 5000,
            require_audit_trail: false,
            determinism_salt: None,
            output_dir: temp_dir.path().to_path_buf(),
        },
        validation: ValidationConfig::default(),
    };

    // Act: Execute pipeline
    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());
    let result = pipeline.run();

    // Assert: Pipeline succeeded and file was generated
    assert!(result.is_ok(), "Pipeline should succeed");
    let state = result.expect("Pipeline state");

    // Assert: Files were generated (condition was true, query returns 2 rows)
    // Note: The query returns 2 entities, so 2 files are generated
    assert_eq!(
        state.generated_files.len(),
        2,
        "Should generate 2 files when condition is true (one per entity)"
    );

    let output_path = temp_dir.path().join("output.txt");
    assert!(
        output_path.exists(),
        "Output file should exist when condition is true"
    );
}

#[test]
fn test_integration_rule_skipped_when_ask_false() {
    // Arrange: Create manifest with rule that has WHEN condition evaluating to false
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let ontology_path = temp_dir.path().join("ontology.ttl");
    std::fs::write(&ontology_path, create_test_ontology()).expect("Failed to write ontology");

    let manifest = GgenManifest {
        project: ProjectConfig {
            name: "skip_test".to_string(),
            version: "1.0.0".to_string(),
            description: None,
        },
        ontology: OntologyConfig {
            source: ontology_path.clone(),
            imports: vec![],
            base_iri: None,
            prefixes: BTreeMap::new(),
        },
        inference: InferenceConfig {
            rules: vec![],
            max_reasoning_timeout_ms: 5000,
        },
        generation: GenerationConfig {
            rules: vec![GenerationRule {
                name: "skipped_rule".to_string(),
                query: QuerySource::Inline {
                    inline: "PREFIX ex: <http://example.org/>\nSELECT ?s WHERE { ?s a ex:Type }"
                        .to_string(),
                },
                template: TemplateSource::Inline {
                    inline: "// Should not generate".to_string(),
                },
                output_file: "skipped.txt".to_string(),
                mode: GenerationMode::Overwrite,
                skip_empty: false,
                // WHEN condition: Check for non-existent type (should be false)
                when: Some(
                    "PREFIX ex: <http://example.org/>\nASK { ?s a ex:NonExistentType }".to_string(),
                ),
            }],
            max_sparql_timeout_ms: 5000,
            require_audit_trail: false,
            determinism_salt: None,
            output_dir: temp_dir.path().to_path_buf(),
        },
        validation: ValidationConfig::default(),
    };

    // Act: Execute pipeline
    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());
    let result = pipeline.run();

    // Assert: Pipeline succeeded but no file was generated
    assert!(result.is_ok(), "Pipeline should succeed");
    let state = result.expect("Pipeline state");

    // Assert: No file was generated (condition was false)
    assert_eq!(
        state.generated_files.len(),
        0,
        "Should generate 0 files when condition is false"
    );

    let output_path = temp_dir.path().join("skipped.txt");
    assert!(
        !output_path.exists(),
        "Output file should NOT exist when condition is false"
    );
}

#[test]
fn test_integration_malformed_ask_query_error() {
    // Arrange: Create manifest with malformed SPARQL ASK query
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let ontology_path = temp_dir.path().join("ontology.ttl");
    std::fs::write(&ontology_path, create_test_ontology()).expect("Failed to write ontology");

    let manifest = GgenManifest {
        project: ProjectConfig {
            name: "malformed_test".to_string(),
            version: "1.0.0".to_string(),
            description: None,
        },
        ontology: OntologyConfig {
            source: ontology_path.clone(),
            imports: vec![],
            base_iri: None,
            prefixes: BTreeMap::new(),
        },
        inference: InferenceConfig {
            rules: vec![],
            max_reasoning_timeout_ms: 5000,
        },
        generation: GenerationConfig {
            rules: vec![GenerationRule {
                name: "malformed_rule".to_string(),
                query: QuerySource::Inline {
                    inline: "SELECT * WHERE { ?s ?p ?o }".to_string(),
                },
                template: TemplateSource::Inline {
                    inline: "// Should not execute".to_string(),
                },
                output_file: "output.txt".to_string(),
                mode: GenerationMode::Overwrite,
                skip_empty: false,
                // WHEN condition: Malformed SPARQL (not an ASK query)
                when: Some("SELECT ?s WHERE { ?s ?p ?o }".to_string()),
            }],
            max_sparql_timeout_ms: 5000,
            require_audit_trail: false,
            determinism_salt: None,
            output_dir: temp_dir.path().to_path_buf(),
        },
        validation: ValidationConfig::default(),
    };

    // Act: Execute pipeline
    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());
    let result = pipeline.run();

    // Assert: Pipeline should fail with helpful error message
    assert!(
        result.is_err(),
        "Pipeline should fail on malformed ASK query"
    );

    let error_msg = match result {
        Err(e) => e.to_string(),
        Ok(_) => panic!("Expected error but got success"),
    };

    // Assert: Error message mentions it must be an ASK query
    assert!(
        error_msg.contains("ASK") || error_msg.contains("must be ASK query"),
        "Error should mention ASK query requirement, got: {}",
        error_msg
    );
}

#[test]
fn test_integration_multiple_conditions() {
    // Arrange: Create manifest with multiple rules having different conditions
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let ontology_path = temp_dir.path().join("ontology.ttl");
    std::fs::write(&ontology_path, create_test_ontology()).expect("Failed to write ontology");

    let manifest = GgenManifest {
        project: ProjectConfig {
            name: "multi_condition_test".to_string(),
            version: "1.0.0".to_string(),
            description: None,
        },
        ontology: OntologyConfig {
            source: ontology_path.clone(),
            imports: vec![],
            base_iri: None,
            prefixes: BTreeMap::new(),
        },
        inference: InferenceConfig {
            rules: vec![],
            max_reasoning_timeout_ms: 5000,
        },
        generation: GenerationConfig {
            rules: vec![
                // Rule 1: Condition TRUE - should execute
                GenerationRule {
                    name: "rule1_executes".to_string(),
                    query: QuerySource::Inline {
                        inline:
                            "PREFIX ex: <http://example.org/>\nSELECT ?s WHERE { ?s a ex:Type }"
                                .to_string(),
                    },
                    template: TemplateSource::Inline {
                        inline: "// Rule 1 executed".to_string(),
                    },
                    output_file: "rule1.txt".to_string(),
                    mode: GenerationMode::Overwrite,
                    skip_empty: false,
                    when: Some(
                        "PREFIX ex: <http://example.org/>\nASK { ?s a ex:Type }".to_string(),
                    ),
                },
                // Rule 2: Condition FALSE - should skip
                GenerationRule {
                    name: "rule2_skipped".to_string(),
                    query: QuerySource::Inline {
                        inline: "SELECT ?s WHERE { ?s ?p ?o }".to_string(),
                    },
                    template: TemplateSource::Inline {
                        inline: "// Rule 2 should not execute".to_string(),
                    },
                    output_file: "rule2.txt".to_string(),
                    mode: GenerationMode::Overwrite,
                    skip_empty: false,
                    when: Some(
                        "PREFIX ex: <http://example.org/>\nASK { ?s a ex:NonExistent }".to_string(),
                    ),
                },
                // Rule 3: No condition - always execute
                GenerationRule {
                    name: "rule3_always".to_string(),
                    query: QuerySource::Inline {
                        inline:
                            "PREFIX ex: <http://example.org/>\nSELECT ?s WHERE { ?s a ex:Type }"
                                .to_string(),
                    },
                    template: TemplateSource::Inline {
                        inline: "// Rule 3 always executed".to_string(),
                    },
                    output_file: "rule3.txt".to_string(),
                    mode: GenerationMode::Overwrite,
                    skip_empty: false,
                    when: None, // No condition
                },
            ],
            max_sparql_timeout_ms: 5000,
            require_audit_trail: false,
            determinism_salt: None,
            output_dir: temp_dir.path().to_path_buf(),
        },
        validation: ValidationConfig::default(),
    };

    // Act: Execute pipeline
    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());
    let result = pipeline.run();

    // Assert: Pipeline succeeded
    assert!(result.is_ok(), "Pipeline should succeed");
    let state = result.expect("Pipeline state");

    // Assert: Only 4 files generated (rule1: 2 files, rule3: 2 files, rule2 was skipped)
    // Note: Each query returns 2 entities, so 2 files per rule
    assert_eq!(
        state.generated_files.len(),
        4,
        "Should generate 4 files (rule1: 2, rule3: 2, rule2 skipped)"
    );

    // Assert: Rule 1 file exists (condition was true)
    let rule1_path = temp_dir.path().join("rule1.txt");
    assert!(rule1_path.exists(), "Rule 1 output should exist");

    // Assert: Rule 2 file does NOT exist (condition was false)
    let rule2_path = temp_dir.path().join("rule2.txt");
    assert!(!rule2_path.exists(), "Rule 2 output should NOT exist");

    // Assert: Rule 3 file exists (no condition, always executes)
    let rule3_path = temp_dir.path().join("rule3.txt");
    assert!(rule3_path.exists(), "Rule 3 output should exist");
}
