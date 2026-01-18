//! Comprehensive integration tests for ggen sync pipeline
//!
//! These tests cover all scenarios of the sync command:
//! - Basic execution with default options
//! - Custom manifest paths
//! - Output directory overrides
//! - Dry-run mode
//! - Validate-only mode
//! - Rule filtering
//! - Error scenarios
//! - Edge cases

use ggen_core::codegen::{OutputFormat, SyncExecutor, SyncOptions};
use tempfile::TempDir;

/// Scenario 1: Basic sync with minimal manifest
#[test]
fn test_sync_basic_execution() {
    // Create temporary directory structure
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let temp_path = temp_dir.path();

    // Create minimal manifest
    let manifest_content = r#"
[project]
name = "test-project"
version = "0.1.0"

[ontology]
source = "ontology.ttl"

[generation]
output_dir = "output"
rules = []
"#;
    let manifest_path = temp_path.join("ggen.toml");
    std::fs::write(&manifest_path, manifest_content).expect("Failed to write manifest");

    // Create minimal ontology
    let ontology_content = r#"
@prefix test: <http://example.org/test/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

test:MyEntity a test:Entity .
"#;
    let ontology_path = temp_path.join("ontology.ttl");
    std::fs::write(&ontology_path, ontology_content).expect("Failed to write ontology");

    // Create sync options
    let options = SyncOptions {
        manifest_path: manifest_path.clone(),
        output_dir: Some(temp_path.join("output")),
        use_cache: false, // Disable cache for tests
        ..SyncOptions::default()
    };

    // Execute sync - should succeed even with no generation rules
    let executor = SyncExecutor::new(options);
    let result = executor.execute();

    // Verify result
    assert!(
        result.is_ok(),
        "Basic sync should succeed. Error: {:?}",
        result.err()
    );
    let sync_result = result.unwrap();
    assert_eq!(sync_result.status, "success");
}

/// Scenario 2: Sync with custom manifest path
#[test]
fn test_sync_custom_manifest_path() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let temp_path = temp_dir.path();
    let custom_dir = temp_path.join("custom");
    std::fs::create_dir_all(&custom_dir).expect("Failed to create custom dir");

    let manifest_content = r#"
[project]
name = "custom-project"
version = "0.1.0"

[ontology]
source = "ontology.ttl"

[generation]
output_dir = "output"
rules = []
"#;
    let manifest_path = custom_dir.join("custom-ggen.toml");
    std::fs::write(&manifest_path, manifest_content).expect("Failed to write manifest");

    let ontology_path = custom_dir.join("ontology.ttl");
    std::fs::write(
        &ontology_path,
        r#"
@prefix test: <http://example.org/> .
test:Entity a test:Thing .
"#,
    )
    .expect("Failed to write ontology");

    let options = SyncOptions {
        manifest_path: manifest_path.clone(),
        output_dir: Some(custom_dir.join("output")),
        ..SyncOptions::default()
    };

    let executor = SyncExecutor::new(options);
    let result = executor.execute();
    assert!(result.is_ok(), "Custom manifest path should work");
}

/// Scenario 3: Sync with output directory override
#[test]
fn test_sync_output_dir_override() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let temp_path = temp_dir.path();

    let manifest_content = r#"
[project]
name = "override-project"
version = "0.1.0"

[ontology]
source = "ontology.ttl"

[generation]
output_dir = "default-output"
rules = []
"#;
    let manifest_path = temp_path.join("ggen.toml");
    std::fs::write(&manifest_path, manifest_content).expect("Failed to write manifest");

    let ontology_path = temp_path.join("ontology.ttl");
    std::fs::write(
        &ontology_path,
        r#"
@prefix test: <http://example.org/> .
test:Entity a test:Thing .
"#,
    )
    .expect("Failed to write ontology");

    let override_dir = temp_path.join("override-output");
    let options = SyncOptions {
        manifest_path,
        output_dir: Some(override_dir.clone()),
        ..SyncOptions::default()
    };

    let executor = SyncExecutor::new(options);
    let result = executor.execute();
    assert!(result.is_ok(), "Output dir override should work");
}

/// Scenario 4: Dry-run mode (no files written)
#[test]
fn test_sync_dry_run_mode() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let temp_path = temp_dir.path();

    let manifest_content = r#"
[project]
name = "dryrun-project"
version = "0.1.0"

[ontology]
source = "ontology.ttl"

[generation]
output_dir = "output"

[[generation.rules]]
name = "test-rule"
query = { inline = "SELECT ?x WHERE { ?x a ?type }" }
template = { inline = "test content" }
output_file = "test.txt"
mode = "Overwrite"
"#;
    let manifest_path = temp_path.join("ggen.toml");
    std::fs::write(&manifest_path, manifest_content).expect("Failed to write manifest");

    let ontology_path = temp_path.join("ontology.ttl");
    std::fs::write(
        &ontology_path,
        r#"
@prefix test: <http://example.org/> .
test:Entity a test:Thing .
"#,
    )
    .expect("Failed to write ontology");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(temp_path.join("output")),
        dry_run: true,
        verbose: true,
        ..SyncOptions::default()
    };

    let executor = SyncExecutor::new(options);
    let result = executor.execute();
    assert!(result.is_ok(), "Dry-run should succeed");

    let sync_result = result.unwrap();
    // In dry-run, no files should actually be created
    assert_eq!(sync_result.status, "success");
}

/// Scenario 5: Validate-only mode (no generation)
#[test]
fn test_sync_validate_only_mode() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let temp_path = temp_dir.path();

    let manifest_content = r#"
[project]
name = "validate-project"
version = "0.1.0"

[ontology]
source = "ontology.ttl"

[generation]
output_dir = "output"
rules = []
"#;
    let manifest_path = temp_path.join("ggen.toml");
    std::fs::write(&manifest_path, manifest_content).expect("Failed to write manifest");

    let ontology_path = temp_path.join("ontology.ttl");
    std::fs::write(
        &ontology_path,
        r#"
@prefix test: <http://example.org/> .
test:Entity a test:Thing .
"#,
    )
    .expect("Failed to write ontology");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(temp_path.join("output")),
        validate_only: true,
        ..SyncOptions::default()
    };

    let executor = SyncExecutor::new(options);
    let result = executor.execute();
    assert!(result.is_ok(), "Validate-only should succeed");

    let sync_result = result.unwrap();
    assert_eq!(sync_result.status, "success");
    assert_eq!(
        sync_result.files_synced, 0,
        "Validate-only should not create files"
    );
}

/// Scenario 6: Error case - missing manifest
#[test]
fn test_sync_missing_manifest() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let nonexistent_path = temp_dir.path().join("nonexistent.toml");

    let options = SyncOptions {
        manifest_path: nonexistent_path,
        ..SyncOptions::default()
    };

    let executor = SyncExecutor::new(options);
    let result = executor.execute();
    assert!(result.is_err(), "Missing manifest should result in error");
    let err_msg = result.unwrap_err().to_string();
    assert!(
        err_msg.contains("Manifest not found") || err_msg.contains("E0001"),
        "Error should mention manifest not found. Got: {}",
        err_msg
    );
}

/// Scenario 7: Error case - missing ontology
#[test]
fn test_sync_missing_ontology() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let temp_path = temp_dir.path();

    let manifest_content = r#"
[project]
name = "test"

[ontology]
source = "missing.ttl"

[generation]
output_dir = "output"
"#;
    let manifest_path = temp_path.join("ggen.toml");
    std::fs::write(&manifest_path, manifest_content).expect("Failed to write manifest");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(temp_path.join("output")),
        ..SyncOptions::default()
    };

    let executor = SyncExecutor::new(options);
    let result = executor.execute();
    assert!(result.is_err(), "Missing ontology should result in error");
}

/// Scenario 8: Output format JSON
#[test]
fn test_sync_output_format_json() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let temp_path = temp_dir.path();

    let manifest_content = r#"
[project]
name = "json-format"
version = "0.1.0"

[ontology]
source = "ontology.ttl"

[generation]
output_dir = "output"
rules = []
"#;
    let manifest_path = temp_path.join("ggen.toml");
    std::fs::write(&manifest_path, manifest_content).expect("Failed to write manifest");

    let ontology_path = temp_path.join("ontology.ttl");
    std::fs::write(
        &ontology_path,
        r#"
@prefix test: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

test:Entity a test:Thing .
"#,
    )
    .expect("Failed to write ontology");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(temp_path.join("output")),
        output_format: OutputFormat::Json,
        ..SyncOptions::default()
    };

    let executor = SyncExecutor::new(options);
    let result = executor.execute();
    assert!(
        result.is_ok(),
        "JSON format should work. Error: {:?}",
        result.err()
    );
}

/// Scenario 9: Error case - watch mode not implemented
#[test]
fn test_sync_watch_mode_not_implemented() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let temp_path = temp_dir.path();

    let manifest_content = r#"
[project]
name = "watch-test"

[ontology]
source = "ontology.ttl"

[generation]
output_dir = "output"
"#;
    let manifest_path = temp_path.join("ggen.toml");
    std::fs::write(&manifest_path, manifest_content).expect("Failed to write manifest");

    let ontology_path = temp_path.join("ontology.ttl");
    std::fs::write(
        &ontology_path,
        r#"
@prefix test: <http://example.org/> .
test:Entity a test:Thing .
"#,
    )
    .expect("Failed to write ontology");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(temp_path.join("output")),
        watch: true,
        ..SyncOptions::default()
    };

    let executor = SyncExecutor::new(options);
    let result = executor.execute();
    assert!(result.is_err(), "Watch mode should error");
    assert!(
        result
            .unwrap_err()
            .to_string()
            .contains("not yet implemented"),
        "Should indicate watch mode not implemented"
    );
}

/// Scenario 10: Verbose output flag
#[test]
fn test_sync_verbose_output() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let temp_path = temp_dir.path();

    let manifest_content = r#"
[project]
name = "verbose-test"
version = "0.1.0"

[ontology]
source = "ontology.ttl"

[generation]
output_dir = "output"
rules = []
"#;
    let manifest_path = temp_path.join("ggen.toml");
    std::fs::write(&manifest_path, manifest_content).expect("Failed to write manifest");

    let ontology_path = temp_path.join("ontology.ttl");
    std::fs::write(
        &ontology_path,
        r#"
@prefix test: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

test:Entity a test:Thing .
"#,
    )
    .expect("Failed to write ontology");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(temp_path.join("output")),
        verbose: true,
        ..SyncOptions::default()
    };

    let executor = SyncExecutor::new(options);
    let result = executor.execute();
    assert!(
        result.is_ok(),
        "Verbose mode should work. Error: {:?}",
        result.err()
    );
}

/// Scenario 11: Test SyncOptions builder methods
#[test]
fn test_sync_options_builder() {
    let options = SyncOptions::new()
        .with_dry_run(true)
        .with_verbose(true)
        .with_audit(true)
        .with_validate_only(false)
        .with_output_format(OutputFormat::Json)
        .with_timeout_ms(60000)
        .with_rules(vec!["rule1".to_string(), "rule2".to_string()])
        .with_force(true)
        .with_output_dir("/tmp/output");

    assert!(options.dry_run);
    assert!(options.verbose);
    assert!(options.audit);
    assert!(!options.validate_only);
    assert_eq!(options.output_format, OutputFormat::Json);
    assert_eq!(options.timeout_ms, 60000);
    assert_eq!(
        options.selected_rules,
        Some(vec!["rule1".to_string(), "rule2".to_string()])
    );
    assert!(options.force);
}

/// Scenario 12: Test OutputFormat parsing
#[test]
fn test_output_format_parsing() {
    assert_eq!("text".parse::<OutputFormat>().unwrap(), OutputFormat::Text);
    assert_eq!("json".parse::<OutputFormat>().unwrap(), OutputFormat::Json);
    assert_eq!("TEXT".parse::<OutputFormat>().unwrap(), OutputFormat::Text);
    assert_eq!("JSON".parse::<OutputFormat>().unwrap(), OutputFormat::Json);
    assert!("invalid".parse::<OutputFormat>().is_err());
}

/// Scenario 13: Error case - non-existent rule name with --rule
#[test]
fn test_sync_nonexistent_rule_name() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let temp_path = temp_dir.path();

    let manifest_content = r#"
[project]
name = "rule-test"
version = "0.1.0"

[ontology]
source = "ontology.ttl"

[generation]
output_dir = "output"

[[generation.rules]]
name = "actual-rule"
query = { inline = "SELECT ?x WHERE { ?x a ?type }" }
template = { inline = "test" }
output_file = "test.txt"
mode = "Overwrite"
"#;
    let manifest_path = temp_path.join("ggen.toml");
    std::fs::write(&manifest_path, manifest_content).expect("Failed to write manifest");

    let ontology_path = temp_path.join("ontology.ttl");
    std::fs::write(
        &ontology_path,
        r#"
@prefix test: <http://example.org/> .
test:Entity a test:Thing .
"#,
    )
    .expect("Failed to write ontology");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(temp_path.join("output")),
        selected_rules: Some(vec!["nonexistent-rule".to_string()]),
        ..SyncOptions::default()
    };

    let executor = SyncExecutor::new(options);
    let result = executor.execute();
    assert!(result.is_err(), "Non-existent rule should error");
    let err_msg = result.unwrap_err().to_string();
    assert!(
        err_msg.contains("not found"),
        "Error should mention rule not found. Got: {}",
        err_msg
    );
    assert!(
        err_msg.contains("actual-rule"),
        "Error should list available rules"
    );
}

/// Scenario 14: Multiple rules selection
#[test]
fn test_sync_multiple_rule_selection() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let temp_path = temp_dir.path();

    let manifest_content = r#"
[project]
name = "multi-rule"
version = "0.1.0"

[ontology]
source = "ontology.ttl"

[generation]
output_dir = "output"

[[generation.rules]]
name = "rule1"
query = { inline = "SELECT ?x WHERE { ?x a ?type }" }
template = { inline = "test1" }
output_file = "test1.txt"
mode = "Overwrite"

[[generation.rules]]
name = "rule2"
query = { inline = "SELECT ?y WHERE { ?y a ?type }" }
template = { inline = "test2" }
output_file = "test2.txt"
mode = "Overwrite"

[[generation.rules]]
name = "rule3"
query = { inline = "SELECT ?z WHERE { ?z a ?type }" }
template = { inline = "test3" }
output_file = "test3.txt"
mode = "Overwrite"
"#;
    let manifest_path = temp_path.join("ggen.toml");
    std::fs::write(&manifest_path, manifest_content).expect("Failed to write manifest");

    let ontology_path = temp_path.join("ontology.ttl");
    std::fs::write(
        &ontology_path,
        r#"
@prefix test: <http://example.org/> .
test:Entity a test:Thing .
"#,
    )
    .expect("Failed to write ontology");

    // Test selecting rule2
    let options = SyncOptions {
        manifest_path: manifest_path.clone(),
        output_dir: Some(temp_path.join("output")),
        selected_rules: Some(vec!["rule2".to_string()]),
        dry_run: true,
        ..SyncOptions::default()
    };

    let executor = SyncExecutor::new(options);
    let result = executor.execute();
    assert!(result.is_ok(), "Rule filtering should work");
}

/// Scenario 15: Manifest without inference rules
#[test]
fn test_sync_no_inference_rules() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let temp_path = temp_dir.path();

    let manifest_content = r#"
[project]
name = "no-inference"
version = "0.1.0"

[ontology]
source = "ontology.ttl"

[inference]
rules = []

[generation]
output_dir = "output"
rules = []
"#;
    let manifest_path = temp_path.join("ggen.toml");
    std::fs::write(&manifest_path, manifest_content).expect("Failed to write manifest");

    let ontology_path = temp_path.join("ontology.ttl");
    std::fs::write(
        &ontology_path,
        r#"
@prefix test: <http://example.org/> .
test:Entity a test:Thing .
"#,
    )
    .expect("Failed to write ontology");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(temp_path.join("output")),
        ..SyncOptions::default()
    };

    let executor = SyncExecutor::new(options);
    let result = executor.execute();
    assert!(
        result.is_ok(),
        "Manifest without inference rules should work"
    );
}

/// Scenario 16: End-to-end sync with real-world example
#[test]
fn test_sync_end_to_end_real_world() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let temp_path = temp_dir.path();

    // Create realistic project structure
    let manifest_content = r#"
[project]
name = "real-world-project"
version = "1.0.0"
description = "Real-world code generation example"

[ontology]
source = "ontology.ttl"

[inference]
rules = []

[generation]
output_dir = "generated"
require_audit_trail = true

[[generation.rules]]
name = "models"
query = { inline = """
PREFIX ex: <http://example.org/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT ?entity ?name WHERE {
  ?entity a ex:Entity ;
           ex:name ?name .
}
ORDER BY ?entity
""" }
template = { inline = "// Generated model\npub struct Model {\n  entity: String,\n  name: String,\n}" }
output_file = "models.rs"
mode = "Overwrite"
"#;

    let manifest_path = temp_path.join("ggen.toml");
    std::fs::write(&manifest_path, manifest_content).expect("Failed to write manifest");

    // Create comprehensive ontology
    let ontology_content = r#"
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:User a ex:Entity ;
  rdfs:label "User" ;
  ex:name "User" .

ex:Product a ex:Entity ;
  rdfs:label "Product" ;
  ex:name "Product" .

ex:Order a ex:Entity ;
  rdfs:label "Order" ;
  ex:name "Order" .
"#;

    let ontology_path = temp_path.join("ontology.ttl");
    std::fs::write(&ontology_path, ontology_content).expect("Failed to write ontology");

    let output_dir = temp_path.join("generated");

    // Test basic sync
    let options = SyncOptions {
        manifest_path: manifest_path.clone(),
        output_dir: Some(output_dir.clone()),
        verbose: true,
        ..SyncOptions::default()
    };

    let executor = SyncExecutor::new(options);
    let result = executor.execute();

    assert!(
        result.is_ok(),
        "Real-world sync should succeed. Error: {:?}",
        result.err()
    );

    let sync_result = result.unwrap();
    assert_eq!(sync_result.status, "success");
    assert!(sync_result.duration_ms > 0);

    // Test dry-run to verify what would be generated
    let dry_run_options = SyncOptions {
        manifest_path: manifest_path.clone(),
        output_dir: Some(output_dir.clone()),
        dry_run: true,
        verbose: true,
        ..SyncOptions::default()
    };

    let executor = SyncExecutor::new(dry_run_options);
    let result = executor.execute();
    assert!(result.is_ok(), "Dry-run should succeed");

    // Test validate-only mode
    let validate_options = SyncOptions {
        manifest_path,
        output_dir: Some(output_dir),
        validate_only: true,
        verbose: true,
        ..SyncOptions::default()
    };

    let executor = SyncExecutor::new(validate_options);
    let result = executor.execute();
    assert!(result.is_ok(), "Validate-only should succeed");
}
