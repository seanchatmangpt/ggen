//! Edge case tests for GenerationPipeline
//!
//! Tests error paths, boundary conditions, and edge cases:
//! - Missing files and invalid input
//! - Empty graphs and single-node graphs
//! - Concurrent operations
//! - Boundary conditions (max/min sizes)
//! - Malformed SPARQL queries
//! - Template rendering errors
//! - File system errors

use ggen_core::codegen::pipeline::{GenerationPipeline, LlmService};
use ggen_core::manifest::{
    GenerationMode, GenerationRule, GgenManifest, InferenceRule, OntologyConfig, QuerySource,
    TemplateSource,
};
use std::collections::BTreeMap;
use std::fs;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use tempfile::TempDir;

// ---------------------------------------------------------------------------
// Helper Functions
// ---------------------------------------------------------------------------

/// Create a minimal valid manifest
fn create_minimal_manifest() -> GgenManifest {
    use std::collections::BTreeMap;

    GgenManifest {
        project: ggen_core::manifest::ProjectConfig {
            name: "test-project".to_string(),
            version: "1.0.0".to_string(),
            description: None,
        },
        ontology: OntologyConfig {
            source: PathBuf::from("ontology.ttl"),
            imports: Vec::new(),
            base_iri: None,
            prefixes: BTreeMap::new(),
        },
        inference: ggen_core::manifest::InferenceConfig {
            rules: Vec::new(),
            max_reasoning_timeout_ms: 60000,
        },
        generation: ggen_core::manifest::GenerationConfig {
            output_dir: PathBuf::from("output"),
            enable_llm: false,
            rules: Vec::new(),
            max_sparql_timeout_ms: 30000,
            require_audit_trail: false,
            determinism_salt: None,
            llm_provider: None,
            llm_model: None,
        },
        validation: ggen_core::manifest::ValidationConfig::default(),
    }
}

/// Create a minimal test ontology
fn create_minimal_ontology() -> String {
    r#"
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <http://example.org/test#> .

ex:TestClass a rdfs:Class ;
    rdfs:label "Test Class" .
"#
    .to_string()
}

/// Create a test generation rule
fn create_test_rule(name: &str, output_file: &str) -> GenerationRule {
    GenerationRule {
        name: name.to_string(),
        query: QuerySource::Inline {
            inline: "SELECT ?class WHERE { ?class a rdfs:Class }".to_string(),
        },
        template: TemplateSource::Inline {
            inline: "Class: {{ class }}\n".to_string(),
        },
        output_file: output_file.to_string(),
        mode: GenerationMode::Overwrite,
        when: None,
        skip_empty: false,
    }
}

// ---------------------------------------------------------------------------
// Test 1: Missing Ontology File
// ---------------------------------------------------------------------------

#[test]
fn test_load_ontology_missing_file() {
    let temp_dir = TempDir::new().expect("TempDir should create");
    let manifest = create_minimal_manifest();

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());

    // Try to load ontology without creating the file
    let result = pipeline.load_ontology();

    assert!(result.is_err(), "Should fail when ontology file is missing");
    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("Failed to read ontology") || error_msg.contains("No such file"),
        "Error should mention file not found: {}",
        error_msg
    );
}

// ---------------------------------------------------------------------------
// Test 2: Invalid Turtle Syntax in Ontology
// ---------------------------------------------------------------------------

#[test]
fn test_load_ontology_invalid_turtle() {
    let temp_dir = TempDir::new().expect("TempDir should create");
    let manifest = create_minimal_manifest();

    let ontology_path = temp_dir.path().join("ontology.ttl");
    fs::write(&ontology_path, "invalid turtle syntax {{{").expect("Should write file");

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());

    let result = pipeline.load_ontology();

    assert!(result.is_err(), "Should fail with invalid Turtle syntax");
    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("parse") || error_msg.contains("syntax") || error_msg.contains("error"),
        "Error should mention parse/syntax error: {}",
        error_msg
    );
}

// ---------------------------------------------------------------------------
// Test 3: Missing Import File
// ---------------------------------------------------------------------------

#[test]
fn test_load_ontology_missing_import() {
    let temp_dir = TempDir::new().expect("TempDir should create");
    let mut manifest = create_minimal_manifest();

    // Add a non-existent import
    manifest.ontology.imports = vec![PathBuf::from("nonexistent.ttl")];

    // Create valid main ontology
    let ontology_path = temp_dir.path().join("ontology.ttl");
    fs::write(&ontology_path, create_minimal_ontology()).expect("Should write file");

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());

    let result = pipeline.load_ontology();

    assert!(result.is_err(), "Should fail when import file is missing");
    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("Failed to read ontology import"),
        "Error should mention import file failure: {}",
        error_msg
    );
}

// ---------------------------------------------------------------------------
// Test 4: Empty SPARQL Query Results (skip_empty = false)
// ---------------------------------------------------------------------------

#[test]
fn test_generation_rule_empty_results_no_skip() {
    let temp_dir = TempDir::new().expect("TempDir should create");
    let mut manifest = create_minimal_manifest();

    // Create ontology with no matching data
    let empty_ontology = r#"
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <http://example.org/test#> .

# No classes defined
"#
    .to_string();
    fs::write(temp_dir.path().join("ontology.ttl"), empty_ontology).expect("Should write file");

    // Add rule with skip_empty = false
    manifest.generation.rules = vec![create_test_rule("empty_test", "output.txt")];

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());

    // Load ontology first
    pipeline.load_ontology().expect("Should load ontology");

    // Execute generation rules
    let result = pipeline.execute_generation_rules();

    // Should succeed but generate no files
    assert!(
        result.is_ok(),
        "Generation should succeed even with empty results"
    );
    let generated = result.unwrap();
    assert_eq!(
        generated.len(),
        0,
        "Should generate zero files when query returns no results"
    );
}

// ---------------------------------------------------------------------------
// Test 5: Empty SPARQL Query Results (skip_empty = true)
// ---------------------------------------------------------------------------

#[test]
fn test_generation_rule_empty_results_with_skip() {
    let temp_dir = TempDir::new().expect("TempDir should create");
    let mut manifest = create_minimal_manifest();

    // Create empty ontology
    let empty_ontology = r#"
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
"#
    .to_string();
    fs::write(temp_dir.path().join("ontology.ttl"), empty_ontology).expect("Should write file");

    // Add rule with skip_empty = true
    let mut rule = create_test_rule("skip_test", "output.txt");
    rule.skip_empty = true;
    manifest.generation.rules = vec![rule];

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());

    pipeline.load_ontology().expect("Should load ontology");

    let result = pipeline.execute_generation_rules();

    assert!(result.is_ok(), "Generation should succeed");
    let generated = result.unwrap();
    assert_eq!(
        generated.len(),
        0,
        "Should skip generation when skip_empty is true"
    );
}

// ---------------------------------------------------------------------------
// Test 6: Malformed SPARQL Query
// ---------------------------------------------------------------------------

#[test]
fn test_generation_rule_malformed_sparql() {
    let temp_dir = TempDir::new().expect("TempDir should create");
    let mut manifest = create_minimal_manifest();

    fs::write(
        temp_dir.path().join("ontology.ttl"),
        create_minimal_ontology(),
    )
    .expect("Should write file");

    // Add rule with invalid SPARQL
    let invalid_rule = GenerationRule {
        name: "malformed_sparql".to_string(),
        query: QuerySource::Inline {
            inline: "SELCT ?class WHER { ?class a rdfs:Class }".to_string(), // Typo: SELCT
        },
        template: TemplateSource::Inline {
            inline: "{{ class }}\n".to_string(),
        },
        output_file: "output.txt".to_string(),
        mode: GenerationMode::Overwrite,
        when: None,
        skip_empty: false,
    };
    manifest.generation.rules = vec![invalid_rule];

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());

    pipeline.load_ontology().expect("Should load ontology");

    let result = pipeline.execute_generation_rules();

    assert!(result.is_err(), "Should fail with malformed SPARQL");
    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("query failed") || error_msg.contains("parse"),
        "Error should mention query failure: {}",
        error_msg
    );
}

// ---------------------------------------------------------------------------
// Test 7: Missing Template File
// ---------------------------------------------------------------------------

#[test]
fn test_generation_rule_missing_template_file() {
    let temp_dir = TempDir::new().expect("TempDir should create");
    let mut manifest = create_minimal_manifest();

    fs::write(
        temp_dir.path().join("ontology.ttl"),
        create_minimal_ontology(),
    )
    .expect("Should write file");

    // Add rule with non-existent template file
    let file_rule = GenerationRule {
        name: "missing_template".to_string(),
        query: QuerySource::Inline {
            inline: "SELECT ?class WHERE { ?class a rdfs:Class }".to_string(),
        },
        template: TemplateSource::File {
            file: PathBuf::from("nonexistent_template.txt"),
        },
        output_file: "output.txt".to_string(),
        mode: GenerationMode::Overwrite,
        when: None,
        skip_empty: false,
    };
    manifest.generation.rules = vec![file_rule];

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());

    pipeline.load_ontology().expect("Should load ontology");

    let result = pipeline.execute_generation_rules();

    assert!(result.is_err(), "Should fail when template file is missing");
    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("Failed to read template"),
        "Error should mention template file failure: {}",
        error_msg
    );
}

// ---------------------------------------------------------------------------
// Test 8: Invalid Template Syntax
// ---------------------------------------------------------------------------

#[test]
fn test_generation_rule_invalid_template_syntax() {
    let temp_dir = TempDir::new().expect("TempDir should create");
    let mut manifest = create_minimal_manifest();

    fs::write(
        temp_dir.path().join("ontology.ttl"),
        create_minimal_ontology(),
    )
    .expect("Should write file");

    // Add rule with invalid Tera syntax
    let invalid_template_rule = GenerationRule {
        name: "invalid_template".to_string(),
        query: QuerySource::Inline {
            inline: "SELECT ?class WHERE { ?class a rdfs:Class }".to_string(),
        },
        template: TemplateSource::Inline {
            inline: "{{ unclosed_bracket\n".to_string(),
        },
        output_file: "output.txt".to_string(),
        mode: GenerationMode::Overwrite,
        when: None,
        skip_empty: false,
    };
    manifest.generation.rules = vec![invalid_template_rule];

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());

    pipeline.load_ontology().expect("Should load ontology");

    let result = pipeline.execute_generation_rules();

    assert!(result.is_err(), "Should fail with invalid template syntax");
    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("Template parse error") || error_msg.contains("syntax"),
        "Error should mention template error: {}",
        error_msg
    );
}

// ---------------------------------------------------------------------------
// Test 9: Template Variable Not Found in SPARQL Results
// ---------------------------------------------------------------------------

#[test]
fn test_generation_rule_template_variable_not_found() {
    let temp_dir = TempDir::new().expect("TempDir should create");
    let mut manifest = create_minimal_manifest();

    fs::write(
        temp_dir.path().join("ontology.ttl"),
        create_minimal_ontology(),
    )
    .expect("Should write file");

    // Template references ?missing_var but SPARQL only returns ?class
    let mismatch_rule = GenerationRule {
        name: "mismatch_vars".to_string(),
        query: QuerySource::Inline {
            inline: "SELECT ?class WHERE { ?class a rdfs:Class }".to_string(),
        },
        template: TemplateSource::Inline {
            inline: "Class: {{ class }}\nMissing: {{ missing_var }}\n".to_string(),
        },
        output_file: "output.txt".to_string(),
        mode: GenerationMode::Overwrite,
        when: None,
        skip_empty: false,
    };
    manifest.generation.rules = vec![mismatch_rule];

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());

    pipeline.load_ontology().expect("Should load ontology");

    let result = pipeline.execute_generation_rules();

    assert!(
        result.is_err(),
        "Should fail when template variable is missing"
    );
    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("Failed to render template"),
        "Error should mention rendering failure: {}",
        error_msg
    );
}

// ---------------------------------------------------------------------------
// Test 10: Create Mode - File Already Exists
// ---------------------------------------------------------------------------

#[test]
fn test_generation_mode_create_file_exists() {
    let temp_dir = TempDir::new().expect("TempDir should create");
    let mut manifest = create_minimal_manifest();

    fs::write(
        temp_dir.path().join("ontology.ttl"),
        create_minimal_ontology(),
    )
    .expect("Should write file");

    // Pre-create the output file
    let output_dir = temp_dir.path().join("output");
    fs::create_dir_all(&output_dir).expect("Should create output dir");
    let existing_file = output_dir.join("existing.txt");
    fs::write(&existing_file, "Existing content\n").expect("Should write file");

    // Add rule with Create mode
    let create_rule = GenerationRule {
        name: "create_mode".to_string(),
        query: QuerySource::Inline {
            inline: "SELECT ?class WHERE { ?class a rdfs:Class }".to_string(),
        },
        template: TemplateSource::Inline {
            inline: "New content: {{ class }}\n".to_string(),
        },
        output_file: "output/existing.txt".to_string(),
        mode: GenerationMode::Create,
        when: None,
        skip_empty: false,
    };
    manifest.generation.rules = vec![create_rule];

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());

    pipeline.load_ontology().expect("Should load ontology");

    let result = pipeline.execute_generation_rules();

    assert!(result.is_ok(), "Generation should succeed");
    let generated = result.unwrap();
    assert_eq!(
        generated.len(),
        0,
        "Should skip file in Create mode when it exists"
    );

    // Verify original content is unchanged
    let original_content = fs::read_to_string(&existing_file).expect("Should read file");
    assert_eq!(
        original_content, "Existing content\n",
        "File should not be modified"
    );
}

// ---------------------------------------------------------------------------
// Test 11: Non-SELECT Query in Generation Rule
// ---------------------------------------------------------------------------

#[test]
fn test_generation_rule_non_select_query() {
    let temp_dir = TempDir::new().expect("TempDir should create");
    let mut manifest = create_minimal_manifest();

    fs::write(
        temp_dir.path().join("ontology.ttl"),
        create_minimal_ontology(),
    )
    .expect("Should write file");

    // Use CONSTRUCT instead of SELECT
    let construct_rule = GenerationRule {
        name: "construct_query".to_string(),
        query: QuerySource::Inline {
            inline: "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }".to_string(),
        },
        template: TemplateSource::Inline {
            inline: "{{ s }} {{ p }} {{ o }}\n".to_string(),
        },
        output_file: "output.txt".to_string(),
        mode: GenerationMode::Overwrite,
        when: None,
        skip_empty: false,
    };
    manifest.generation.rules = vec![construct_rule];

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());

    pipeline.load_ontology().expect("Should load ontology");

    let result = pipeline.execute_generation_rules();

    assert!(result.is_err(), "Should fail with CONSTRUCT query");
    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("SELECT queries") || error_msg.contains("E0003"),
        "Error should mention SELECT requirement: {}",
        error_msg
    );
}

// ---------------------------------------------------------------------------
// Test 12: WHEN Condition (ASK Query) - True
// ---------------------------------------------------------------------------

#[test]
fn test_generation_rule_when_condition_true() {
    let temp_dir = TempDir::new().expect("TempDir should create");
    let mut manifest = create_minimal_manifest();

    fs::write(
        temp_dir.path().join("ontology.ttl"),
        create_minimal_ontology(),
    )
    .expect("Should write file");

    // Add rule with WHEN condition that evaluates to true
    let mut rule = create_test_rule("when_true", "output.txt");
    rule.when = Some("ASK { ?class a rdfs:Class }".to_string());
    manifest.generation.rules = vec![rule];

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());

    pipeline.load_ontology().expect("Should load ontology");

    let result = pipeline.execute_generation_rules();

    assert!(
        result.is_ok(),
        "Generation should succeed when WHEN condition is true"
    );
    let generated = result.unwrap();
    assert!(
        generated.len() > 0,
        "Should generate files when condition is true"
    );
}

// ---------------------------------------------------------------------------
// Test 13: WHEN Condition (ASK Query) - False
// ---------------------------------------------------------------------------

#[test]
fn test_generation_rule_when_condition_false() {
    let temp_dir = TempDir::new().expect("TempDir should create");
    let mut manifest = create_minimal_manifest();

    fs::write(
        temp_dir.path().join("ontology.ttl"),
        create_minimal_ontology(),
    )
    .expect("Should write file");

    // Add rule with WHEN condition that evaluates to false
    let mut rule = create_test_rule("when_false", "output.txt");
    rule.when = Some("ASK { ?class a <http://example.org/NonExistentClass> }".to_string());
    manifest.generation.rules = vec![rule];

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());

    pipeline.load_ontology().expect("Should load ontology");

    let result = pipeline.execute_generation_rules();

    assert!(
        result.is_ok(),
        "Generation should succeed when WHEN condition is false"
    );
    let generated = result.unwrap();
    assert_eq!(
        generated.len(),
        0,
        "Should skip generation when condition is false"
    );
}

// ---------------------------------------------------------------------------
// Test 14: WHEN Condition with Invalid ASK Query
// ---------------------------------------------------------------------------

#[test]
fn test_generation_rule_when_condition_invalid_ask() {
    let temp_dir = TempDir::new().expect("TempDir should create");
    let mut manifest = create_minimal_manifest();

    fs::write(
        temp_dir.path().join("ontology.ttl"),
        create_minimal_ontology(),
    )
    .expect("Should write file");

    // Use SELECT instead of ASK for WHEN condition
    let mut rule = create_test_rule("invalid_when", "output.txt");
    rule.when = Some("SELECT ?class WHERE { ?class a rdfs:Class }".to_string());
    manifest.generation.rules = vec![rule];

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());

    pipeline.load_ontology().expect("Should load ontology");

    let result = pipeline.execute_generation_rules();

    assert!(result.is_err(), "Should fail with non-ASK WHEN query");
    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("ASK query") || error_msg.contains("must be ASK"),
        "Error should mention ASK requirement: {}",
        error_msg
    );
}

// ---------------------------------------------------------------------------
// Test 15: Inference Rule with WHEN Condition - Skipped
// ---------------------------------------------------------------------------

#[test]
fn test_inference_rule_when_condition_skipped() {
    let temp_dir = TempDir::new().expect("TempDir should create");
    let mut manifest = create_minimal_manifest();

    fs::write(
        temp_dir.path().join("ontology.ttl"),
        create_minimal_ontology(),
    )
    .expect("Should write file");

    // Add inference rule with WHEN condition that evaluates to false
    let rule = InferenceRule {
        name: "skipped_inference".to_string(),
        description: None,
        order: 1,
        when: Some("ASK { ?class a <http://example.org/NonExistentClass> }".to_string()),
        construct: "CONSTRUCT { ?s a <http://example.org/Inferred> } WHERE { ?s a rdfs:Class }"
            .to_string(),
    };
    manifest.inference.rules = vec![rule];

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());

    pipeline.load_ontology().expect("Should load ontology");

    let result = pipeline.execute_inference_rules();

    assert!(result.is_ok(), "Inference execution should succeed");
    let executed = result.unwrap();
    assert_eq!(executed.len(), 1, "Should have one executed rule");
    assert_eq!(
        executed[0].triples_added, 0,
        "Skipped rule should add no triples"
    );
    assert_eq!(
        executed[0].query_hash, "skipped",
        "Skipped rule should have 'skipped' hash"
    );
}

// ---------------------------------------------------------------------------
// Test 16: Output Directory Creation
// ---------------------------------------------------------------------------

#[test]
fn test_output_directory_created_automatically() {
    let temp_dir = TempDir::new().expect("TempDir should create");
    let mut manifest = create_minimal_manifest();

    fs::write(
        temp_dir.path().join("ontology.ttl"),
        create_minimal_ontology(),
    )
    .expect("Should write file");

    // Use nested output directory that doesn't exist
    let nested_path = "output/nested/deep/output.txt";
    let rule = create_test_rule("nested_output", nested_path);
    manifest.generation.rules = vec![rule];

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());

    pipeline.load_ontology().expect("Should load ontology");

    let result = pipeline.execute_generation_rules();

    assert!(result.is_ok(), "Generation should succeed");
    let generated = result.unwrap();
    assert_eq!(generated.len(), 1, "Should generate one file");

    // Verify nested directory was created
    let nested_file = temp_dir.path().join(nested_path);
    assert!(nested_file.exists(), "Nested directory should be created");
    assert!(nested_file.is_file(), "Output should be a file");
}

// ---------------------------------------------------------------------------
// Test 17: Execute Inference Rules Without Loading Ontology
// ---------------------------------------------------------------------------

#[test]
fn test_execute_inference_rules_without_ontology() {
    let temp_dir = TempDir::new().expect("TempDir should create");
    let manifest = create_minimal_manifest();

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());

    // Try to execute inference rules without loading ontology
    let result = pipeline.execute_inference_rules();

    assert!(result.is_err(), "Should fail when ontology not loaded");
    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("not loaded") || error_msg.contains("load_ontology"),
        "Error should mention ontology not loaded: {}",
        error_msg
    );
}

// ---------------------------------------------------------------------------
// Test 18: Execute Generation Rules Without Loading Ontology
// ---------------------------------------------------------------------------

#[test]
fn test_execute_generation_rules_without_ontology() {
    let temp_dir = TempDir::new().expect("TempDir should create");
    let mut manifest = create_minimal_manifest();

    manifest.generation.rules = vec![create_test_rule("test", "output.txt")];

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());

    // Try to execute generation rules without loading ontology
    let result = pipeline.execute_generation_rules();

    assert!(result.is_err(), "Should fail when ontology not loaded");
    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("not loaded") || error_msg.contains("load_ontology"),
        "Error should mention ontology not loaded: {}",
        error_msg
    );
}

// ---------------------------------------------------------------------------
// Test 19: Execute Single Generation Rule
// ---------------------------------------------------------------------------

#[test]
fn test_execute_single_generation_rule() {
    let temp_dir = TempDir::new().expect("TempDir should create");
    let mut manifest = create_minimal_manifest();

    fs::write(
        temp_dir.path().join("ontology.ttl"),
        create_minimal_ontology(),
    )
    .expect("Should write file");

    // Add multiple rules
    let rule1 = create_test_rule("rule1", "output1.txt");
    let rule2 = create_test_rule("rule2", "output2.txt");
    manifest.generation.rules = vec![rule1.clone(), rule2];

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());

    pipeline.load_ontology().expect("Should load ontology");

    // Execute only the first rule
    let result = pipeline.execute_generation_rule(&rule1);

    assert!(result.is_ok(), "Single rule execution should succeed");
    let generated = result.unwrap();
    assert_eq!(generated.len(), 1, "Should generate one file");

    // Verify only first file exists
    assert!(temp_dir.path().join("output/output1.txt").exists());
    assert!(!temp_dir.path().join("output/output2.txt").exists());
}

// ---------------------------------------------------------------------------
// Test 20: Expand Output Path with Empty Context
// ---------------------------------------------------------------------------

#[test]
fn test_expand_output_path_empty_context() {
    let context = BTreeMap::new();
    let result = GenerationPipeline::expand_output_path("src/fixed.rs", &context);

    assert_eq!(result, PathBuf::from("src/fixed.rs"));
}

// ---------------------------------------------------------------------------
// Test 21: Expand Output Path with Special Characters in Values
// ---------------------------------------------------------------------------

#[test]
fn test_expand_output_path_special_characters() {
    let mut context = BTreeMap::new();
    context.insert("name".to_string(), "test-name".to_string());
    context.insert("module".to_string(), "my_module".to_string());

    let result = GenerationPipeline::expand_output_path("src/{{module}}/{{name}}.rs", &context);

    assert_eq!(result, PathBuf::from("src/my_module/test-name.rs"));
}

// ---------------------------------------------------------------------------
// Test 22: Expand Output Path with SPARQL Variable Prefix
// ---------------------------------------------------------------------------

#[test]
fn test_expand_output_path_sparql_prefix() {
    let mut context = BTreeMap::new();
    context.insert("?name".to_string(), "test".to_string());

    let result = GenerationPipeline::expand_output_path("src/{{name}}.rs", &context);

    assert_eq!(result, PathBuf::from("src/test.rs"));
}

// ---------------------------------------------------------------------------
// Test 23: Expand Output Path with IRI Value
// ---------------------------------------------------------------------------

#[test]
fn test_expand_output_path_iri_value() {
    let mut context = BTreeMap::new();
    context.insert("iri".to_string(), "<http://example.org/test>".to_string());

    let result = GenerationPipeline::expand_output_path("src/{{iri}}.rs", &context);

    // IRI angle brackets should be stripped
    assert_eq!(result, PathBuf::from("src/http://example.org/test.rs"));
}

// ---------------------------------------------------------------------------
// Test 24: Expand Output Path with Literal Value
// ---------------------------------------------------------------------------

#[test]
fn test_expand_output_path_literal_value() {
    let mut context = BTreeMap::new();
    context.insert("literal".to_string(), "\"test value\"".to_string());

    let result = GenerationPipeline::expand_output_path("src/{{literal}}.rs", &context);

    // Literal quotes should be stripped
    assert_eq!(result, PathBuf::from("src/test value.rs"));
}

// ---------------------------------------------------------------------------
// Test 25: Run Complete Pipeline Successfully
// ---------------------------------------------------------------------------

#[test]
fn test_run_complete_pipeline() {
    let temp_dir = TempDir::new().expect("TempDir should create");
    let mut manifest = create_minimal_manifest();

    fs::write(
        temp_dir.path().join("ontology.ttl"),
        create_minimal_ontology(),
    )
    .expect("Should write file");

    // Add generation rule
    manifest.generation.rules = vec![create_test_rule("test", "output.txt")];

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());

    let result = pipeline.run();

    assert!(result.is_ok(), "Pipeline should run successfully");
    let state = result.unwrap();

    assert!(!state.manifest.ontology.source.as_os_str().is_empty());
    assert!(state.ontology_graph.len() > 0, "Graph should have triples");
    assert_eq!(state.generated_files.len(), 1, "Should generate one file");

    // Verify output file exists
    assert!(temp_dir.path().join("output/output.txt").exists());
}

// ---------------------------------------------------------------------------
// Test 26: LLM Service Injection - Real Service
// ---------------------------------------------------------------------------

#[test]
fn test_llm_service_injection() {
    struct MockLlmService {
        call_count: Arc<Mutex<usize>>,
    }

    impl LlmService for MockLlmService {
        fn generate_skill_impl(
            &self, skill_name: &str, _system_prompt: &str, _implementation_hint: &str,
            _language: &str,
        ) -> std::result::Result<String, Box<dyn std::error::Error + Send + Sync>> {
            *self.call_count.lock().unwrap() += 1;
            Ok(format!("// Generated impl for {}", skill_name))
        }

        fn clone_box(&self) -> Box<dyn LlmService> {
            Box::new(MockLlmService {
                call_count: Arc::clone(&self.call_count),
            })
        }
    }

    let temp_dir = TempDir::new().expect("TempDir should create");
    let mut manifest = create_minimal_manifest();

    fs::write(
        temp_dir.path().join("ontology.ttl"),
        create_minimal_ontology(),
    )
    .expect("Should write file");

    // Enable LLM
    manifest.generation.enable_llm = true;

    // Add rule with skill fields
    let skill_rule = GenerationRule {
        name: "skill_generation".to_string(),
        query: QuerySource::Inline {
            inline: r#"
                SELECT ?skill_name ?system_prompt ?implementation_hint ?language WHERE {
                    BIND("test_skill" AS ?skill_name)
                    BIND("Test description" AS ?system_prompt)
                    BIND("Use async" AS ?implementation_hint)
                    BIND("rust" AS ?language)
                }
            "#
            .to_string(),
        },
        template: TemplateSource::Inline {
            inline: "{{generated_impl}}\n".to_string(),
        },
        output_file: "output/skill.rs".to_string(),
        mode: GenerationMode::Overwrite,
        when: None,
        skip_empty: false,
    };
    manifest.generation.rules = vec![skill_rule];

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());

    // Inject LLM service
    let call_count = Arc::new(Mutex::new(0));
    let llm_service = Box::new(MockLlmService {
        call_count: Arc::clone(&call_count),
    });
    pipeline.set_llm_service(Some(llm_service));

    pipeline.load_ontology().expect("Should load ontology");

    let result = pipeline.execute_generation_rules();

    assert!(result.is_ok(), "Generation should succeed");
    let generated = result.unwrap();
    assert_eq!(generated.len(), 1, "Should generate one file");

    // Verify LLM service was called
    assert_eq!(
        *call_count.lock().unwrap(),
        1,
        "LLM service should be called once"
    );

    // Verify generated content
    let output_path = temp_dir.path().join("output/skill.rs");
    let content = fs::read_to_string(&output_path).expect("Should read file");
    assert!(content.contains("Generated impl for test_skill"));
}

// ---------------------------------------------------------------------------
// Test 27: LLM Service Injection - Disabled in Manifest
// ---------------------------------------------------------------------------

#[test]
fn test_llm_service_disabled_in_manifest() {
    struct CountingLlmService {
        call_count: Arc<Mutex<usize>>,
    }

    impl LlmService for CountingLlmService {
        fn generate_skill_impl(
            &self, skill_name: &str, _system_prompt: &str, _implementation_hint: &str,
            _language: &str,
        ) -> std::result::Result<String, Box<dyn std::error::Error + Send + Sync>> {
            *self.call_count.lock().unwrap() += 1;
            Ok(format!("// Generated impl for {}", skill_name))
        }

        fn clone_box(&self) -> Box<dyn LlmService> {
            Box::new(CountingLlmService {
                call_count: Arc::clone(&self.call_count),
            })
        }
    }

    let temp_dir = TempDir::new().expect("TempDir should create");
    let mut manifest = create_minimal_manifest();

    fs::write(
        temp_dir.path().join("ontology.ttl"),
        create_minimal_ontology(),
    )
    .expect("Should write file");

    // Disable LLM (default)
    manifest.generation.enable_llm = false;

    // Add rule with skill fields
    let skill_rule = GenerationRule {
        name: "skill_generation".to_string(),
        query: QuerySource::Inline {
            inline: r#"
                SELECT ?skill_name ?system_prompt WHERE {
                    BIND("test_skill" AS ?skill_name)
                    BIND("Test description" AS ?system_prompt)
                }
            "#
            .to_string(),
        },
        template: TemplateSource::Inline {
            inline: "{{generated_impl}}\n".to_string(),
        },
        output_file: "output/skill.rs".to_string(),
        mode: GenerationMode::Overwrite,
        when: None,
        skip_empty: false,
    };
    manifest.generation.rules = vec![skill_rule];

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());

    // Inject LLM service (should not be used when disabled)
    let call_count = Arc::new(Mutex::new(0));
    let llm_service = Box::new(CountingLlmService {
        call_count: Arc::clone(&call_count),
    });
    pipeline.set_llm_service(Some(llm_service));

    pipeline.load_ontology().expect("Should load ontology");

    let result = pipeline.execute_generation_rules();

    assert!(result.is_ok(), "Generation should succeed");

    // Verify LLM service was NOT called
    assert_eq!(
        *call_count.lock().unwrap(),
        0,
        "LLM service should not be called when disabled"
    );

    // Verify TODO stub was generated instead
    let output_path = temp_dir.path().join("output/skill.rs");
    let content = fs::read_to_string(&output_path).expect("Should read file");
    assert!(content.contains("TODO") || content.contains("Implement"));
}

// ---------------------------------------------------------------------------
// Test 28: Output Directory Override
// ---------------------------------------------------------------------------

#[test]
fn test_output_directory_override() {
    let temp_dir = TempDir::new().expect("TempDir should create");
    let mut manifest = create_minimal_manifest();

    fs::write(
        temp_dir.path().join("ontology.ttl"),
        create_minimal_ontology(),
    )
    .expect("Should write file");

    manifest.generation.output_dir = PathBuf::from("default_output");
    manifest.generation.rules = vec![create_test_rule("test", "output.txt")];

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());

    // Override output directory
    let custom_output = temp_dir.path().join("custom_output");
    pipeline.set_output_dir(custom_output.clone());

    pipeline.load_ontology().expect("Should load ontology");

    let result = pipeline.execute_generation_rules();

    assert!(result.is_ok(), "Generation should succeed");

    // Verify file is in custom output directory
    assert!(custom_output.join("output.txt").exists());
    assert!(!temp_dir.path().join("default_output/output.txt").exists());
}

// ---------------------------------------------------------------------------
// Test 29: Force Overwrite Flag
// ---------------------------------------------------------------------------

#[test]
fn test_force_overwrite_flag() {
    let temp_dir = TempDir::new().expect("TempDir should create");
    let mut manifest = create_minimal_manifest();

    fs::write(
        temp_dir.path().join("ontology.ttl"),
        create_minimal_ontology(),
    )
    .expect("Should write file");

    manifest.generation.rules = vec![create_test_rule("test", "protected.txt")];

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());

    // Set force overwrite flag
    pipeline.set_force_overwrite(true);

    pipeline.load_ontology().expect("Should load ontology");

    let result = pipeline.execute_generation_rules();

    assert!(
        result.is_ok(),
        "Generation should succeed with force overwrite"
    );
}

// ---------------------------------------------------------------------------
// Test 30: Generate Skill Impl Without LLM Service
// ---------------------------------------------------------------------------

#[test]
fn test_generate_skill_impl_without_llm_service() {
    let temp_dir = TempDir::new().expect("TempDir should create");
    let mut manifest = create_minimal_manifest();

    fs::write(
        temp_dir.path().join("ontology.ttl"),
        create_minimal_ontology(),
    )
    .expect("Should write file");

    // Enable LLM but don't inject service
    manifest.generation.enable_llm = true;

    let skill_rule = GenerationRule {
        name: "skill_generation".to_string(),
        query: QuerySource::Inline {
            inline: r#"
                SELECT ?skill_name ?system_prompt WHERE {
                    BIND("test_skill" AS ?skill_name)
                    BIND("Test description" AS ?system_prompt)
                }
            "#
            .to_string(),
        },
        template: TemplateSource::Inline {
            inline: "{{generated_impl}}\n".to_string(),
        },
        output_file: "output/skill.rs".to_string(),
        mode: GenerationMode::Overwrite,
        when: None,
        skip_empty: false,
    };
    manifest.generation.rules = vec![skill_rule];

    let mut pipeline = GenerationPipeline::new(manifest, temp_dir.path().to_path_buf());

    // Don't inject LLM service - should use default TODO stubs

    pipeline.load_ontology().expect("Should load ontology");

    let result = pipeline.execute_generation_rules();

    assert!(
        result.is_ok(),
        "Generation should succeed with default LLM service"
    );

    // Verify TODO stub was generated
    let output_path = temp_dir.path().join("output/skill.rs");
    let content = fs::read_to_string(&output_path).expect("Should read file");
    assert!(content.contains("TODO") || content.contains("Implement"));
}
