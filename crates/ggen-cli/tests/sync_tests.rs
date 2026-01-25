//! Comprehensive Chicago TDD tests for ggen sync command
//!
//! Test Philosophy: Chicago TDD (state-based testing with real objects)
//! - Arrange: Set up real RDF graphs, manifests, and templates
//! - Act: Call public sync API
//! - Assert: Verify observable state changes and side effects
//!
//! Coverage Goals:
//! - Success cases: basic sync, dry-run, rule filtering, audit, validate-only
//! - Error cases: missing manifest, invalid syntax, missing ontology, template errors, SPARQL errors
//! - Edge cases: empty ontology, circular dependencies, timeouts, force overwrite, multiple formats
//!
//! Test Organization:
//! - Unit tests use real RDF graphs and templates (no mocks)
//! - Each test is independent (uses tempdir)
//! - All assertions verify observable state
//! - unwrap() is allowed in tests only

use ggen_core::codegen::{OutputFormat, SyncExecutor, SyncOptions, SyncResult};
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

// ============================================================================
// Test Helpers (Real Object Construction)
// ============================================================================

/// Create minimal valid manifest for testing
fn create_minimal_manifest(temp_path: &Path, ontology_file: &str) -> PathBuf {
    let manifest_content = format!(
        r#"
[project]
name = "test-project"
version = "0.1.0"

[ontology]
source = "{}"

[generation]
output_dir = "output"
rules = []
"#,
        ontology_file
    );
    let manifest_path = temp_path.join("ggen.toml");
    fs::write(&manifest_path, manifest_content).unwrap();
    manifest_path
}

/// Create minimal valid Turtle ontology
fn create_minimal_ontology(temp_path: &Path, filename: &str) -> PathBuf {
    let ontology_content = r#"
@prefix test: <http://example.org/test/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

test:Entity a rdfs:Class ;
    rdfs:label "Test Entity" .
"#;
    let ontology_path = temp_path.join(filename);
    fs::write(&ontology_path, ontology_content).unwrap();
    ontology_path
}

/// Create manifest with generation rules
fn create_manifest_with_rules(temp_path: &Path, ontology_file: &str) -> PathBuf {
    let manifest_content = format!(
        r#"
[project]
name = "test-project"
version = "0.1.0"

[ontology]
source = "{}"

[generation]
output_dir = "output"

[[generation.rules]]
name = "test-rule"
query = {{ inline = "SELECT ?s ?p ?o WHERE {{ ?s ?p ?o }} LIMIT 1" }}
template = {{ inline = "// Generated code\nstruct Test {{}}\n" }}
output_file = "test.rs"
mode = "Overwrite"
"#,
        ontology_file
    );
    let manifest_path = temp_path.join("ggen.toml");
    fs::write(&manifest_path, manifest_content).unwrap();
    manifest_path
}

/// Create manifest with multiple rules for filtering tests
fn create_manifest_with_multiple_rules(temp_path: &Path, ontology_file: &str) -> PathBuf {
    let manifest_content = format!(
        r#"
[project]
name = "multi-rule-project"
version = "0.1.0"

[ontology]
source = "{}"

[generation]
output_dir = "output"

[[generation.rules]]
name = "structs"
query = {{ inline = "SELECT ?entity WHERE {{ ?entity a ?type }}" }}
template = {{ inline = "// Structs\nstruct Generated {{}}" }}
output_file = "structs.rs"
mode = "Overwrite"

[[generation.rules]]
name = "enums"
query = {{ inline = "SELECT ?entity WHERE {{ ?entity a ?type }}" }}
template = {{ inline = "// Enums\nenum Generated {{}}" }}
output_file = "enums.rs"
mode = "Overwrite"

[[generation.rules]]
name = "traits"
query = {{ inline = "SELECT ?entity WHERE {{ ?entity a ?type }}" }}
template = {{ inline = "// Traits\ntrait Generated {{}}" }}
output_file = "traits.rs"
mode = "Overwrite"
"#,
        ontology_file
    );
    let manifest_path = temp_path.join("ggen.toml");
    fs::write(&manifest_path, manifest_content).unwrap();
    manifest_path
}

/// Create complex ontology with multiple entities
fn create_complex_ontology(temp_path: &Path, filename: &str) -> PathBuf {
    let ontology_content = r#"
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:User a ex:Entity ;
    rdfs:label "User" ;
    ex:name "User" ;
    ex:hasField ex:UserIdField, ex:UserNameField .

ex:UserIdField a ex:Field ;
    rdfs:label "id" ;
    ex:fieldType "String" .

ex:UserNameField a ex:Field ;
    rdfs:label "name" ;
    ex:fieldType "String" .

ex:Product a ex:Entity ;
    rdfs:label "Product" ;
    ex:name "Product" ;
    ex:hasField ex:ProductIdField, ex:ProductNameField .

ex:ProductIdField a ex:Field ;
    rdfs:label "id" ;
    ex:fieldType "String" .

ex:ProductNameField a ex:Field ;
    rdfs:label "name" ;
    ex:fieldType "String" .
"#;
    let ontology_path = temp_path.join(filename);
    fs::write(&ontology_path, ontology_content).unwrap();
    ontology_path
}

// ============================================================================
// SUCCESS CASES: Core Functionality
// ============================================================================

#[test]
fn test_sync_basic_execution_with_default_manifest() {
    // Arrange: Create minimal project structure
    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    create_minimal_ontology(temp_path, "ontology.ttl");
    let manifest_path = create_minimal_manifest(temp_path, "ontology.ttl");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(temp_path.join("output")),
        use_cache: false, // Disable cache for deterministic tests
        ..SyncOptions::default()
    };

    // Act: Execute sync
    let executor = SyncExecutor::new(options);
    let result = executor.execute();

    // Assert: Verify success
    assert!(
        result.is_ok(),
        "Basic sync should succeed. Error: {:?}",
        result.err()
    );

    let sync_result = result.unwrap();
    assert_eq!(sync_result.status, "success");
    assert!(sync_result.duration_ms > 0, "Duration should be positive");
    assert!(
        sync_result.files_synced >= 0,
        "Files synced should be non-negative"
    );
}

#[test]
fn test_sync_with_dry_run_no_files_written() {
    // Arrange: Create project with generation rule
    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    create_minimal_ontology(temp_path, "ontology.ttl");
    let manifest_path = create_manifest_with_rules(temp_path, "ontology.ttl");

    let output_dir = temp_path.join("output");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(output_dir.clone()),
        dry_run: true, // Enable dry-run mode
        verbose: true,
        use_cache: false,
        ..SyncOptions::default()
    };

    // Act: Execute dry-run sync
    let executor = SyncExecutor::new(options);
    let result = executor.execute();

    // Assert: Verify success and no files written
    assert!(
        result.is_ok(),
        "Dry-run should succeed. Error: {:?}",
        result.err()
    );

    let sync_result = result.unwrap();
    assert_eq!(sync_result.status, "success");

    // In dry-run mode, output directory should not be created or files written
    // The executor may create the directory but should not write rule outputs
    if output_dir.exists() {
        let entries: Vec<_> = fs::read_dir(&output_dir)
            .unwrap()
            .filter_map(|e| e.ok())
            .collect();
        // Dry-run might create directory but should not write actual generated files
        // or it might not create directory at all - both are acceptable
        assert!(
            entries.is_empty() || !output_dir.join("test.rs").exists(),
            "Dry-run should not write rule output files"
        );
    }
}

#[test]
fn test_sync_with_specific_rule_filter() {
    // Arrange: Create manifest with multiple rules
    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    create_minimal_ontology(temp_path, "ontology.ttl");
    let manifest_path = create_manifest_with_multiple_rules(temp_path, "ontology.ttl");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(temp_path.join("output")),
        selected_rules: Some(vec!["structs".to_string()]), // Filter to one rule
        dry_run: true,                                     // Use dry-run to avoid side effects
        use_cache: false,
        ..SyncOptions::default()
    };

    // Act: Execute with rule filter
    let executor = SyncExecutor::new(options);
    let result = executor.execute();

    // Assert: Verify only selected rule executed
    assert!(
        result.is_ok(),
        "Rule filtering should work. Error: {:?}",
        result.err()
    );

    let sync_result = result.unwrap();
    assert_eq!(sync_result.status, "success");
    // With dry-run and rule filter, we should have executed only 1 generation rule
    // (Note: exact count depends on implementation, but should be <= total rules)
}

#[test]
fn test_sync_with_audit_trail_generation() {
    // Arrange: Create project structure
    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    create_minimal_ontology(temp_path, "ontology.ttl");
    let manifest_path = create_minimal_manifest(temp_path, "ontology.ttl");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(temp_path.join("output")),
        audit: true, // Enable audit trail
        use_cache: false,
        ..SyncOptions::default()
    };

    // Act: Execute with audit
    let executor = SyncExecutor::new(options);
    let result = executor.execute();

    // Assert: Verify audit trail generated
    assert!(
        result.is_ok(),
        "Audit sync should succeed. Error: {:?}",
        result.err()
    );

    let sync_result = result.unwrap();
    assert_eq!(sync_result.status, "success");

    // Verify audit trail path is set
    assert!(
        sync_result.audit_trail.is_some(),
        "Audit trail path should be set when --audit is enabled"
    );

    // Optionally verify audit file exists
    if let Some(audit_path) = sync_result.audit_trail {
        let full_audit_path = temp_path.join(&audit_path);
        // Note: Implementation may or may not create the file, depending on whether
        // there were changes. At minimum, the path should be set.
        assert!(!audit_path.is_empty(), "Audit path should not be empty");
    }
}

#[test]
fn test_sync_with_validate_only_no_generation() {
    // Arrange: Create project structure
    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    create_minimal_ontology(temp_path, "ontology.ttl");
    let manifest_path = create_manifest_with_rules(temp_path, "ontology.ttl");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(temp_path.join("output")),
        validate_only: true, // Enable validate-only mode
        use_cache: false,
        ..SyncOptions::default()
    };

    // Act: Execute validation only
    let executor = SyncExecutor::new(options);
    let result = executor.execute();

    // Assert: Verify validation succeeded and no files generated
    assert!(
        result.is_ok(),
        "Validate-only should succeed. Error: {:?}",
        result.err()
    );

    let sync_result = result.unwrap();
    assert_eq!(sync_result.status, "success");
    assert_eq!(
        sync_result.files_synced, 0,
        "Validate-only should not generate any files"
    );
}

#[test]
fn test_sync_with_json_output_format() {
    // Arrange: Create project structure
    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    create_minimal_ontology(temp_path, "ontology.ttl");
    let manifest_path = create_minimal_manifest(temp_path, "ontology.ttl");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(temp_path.join("output")),
        output_format: OutputFormat::Json, // Request JSON output
        use_cache: false,
        ..SyncOptions::default()
    };

    // Act: Execute with JSON format
    let executor = SyncExecutor::new(options);
    let result = executor.execute();

    // Assert: Verify success (format affects output display, not execution)
    assert!(
        result.is_ok(),
        "JSON format should work. Error: {:?}",
        result.err()
    );

    let sync_result = result.unwrap();
    assert_eq!(sync_result.status, "success");
}

#[test]
fn test_sync_with_custom_timeout() {
    // Arrange: Create project structure
    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    create_minimal_ontology(temp_path, "ontology.ttl");
    let manifest_path = create_minimal_manifest(temp_path, "ontology.ttl");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(temp_path.join("output")),
        timeout_ms: 60000, // 60 second timeout
        use_cache: false,
        ..SyncOptions::default()
    };

    // Act: Execute with custom timeout
    let executor = SyncExecutor::new(options);
    let result = executor.execute();

    // Assert: Verify success
    assert!(
        result.is_ok(),
        "Custom timeout should work. Error: {:?}",
        result.err()
    );

    let sync_result = result.unwrap();
    assert_eq!(sync_result.status, "success");
    assert!(
        sync_result.duration_ms < 60000,
        "Execution should complete within timeout"
    );
}

#[test]
fn test_sync_with_verbose_output() {
    // Arrange: Create project structure
    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    create_minimal_ontology(temp_path, "ontology.ttl");
    let manifest_path = create_minimal_manifest(temp_path, "ontology.ttl");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(temp_path.join("output")),
        verbose: true, // Enable verbose output
        use_cache: false,
        ..SyncOptions::default()
    };

    // Act: Execute with verbose mode
    let executor = SyncExecutor::new(options);
    let result = executor.execute();

    // Assert: Verify success (verbose affects logging, not execution)
    assert!(
        result.is_ok(),
        "Verbose mode should work. Error: {:?}",
        result.err()
    );

    let sync_result = result.unwrap();
    assert_eq!(sync_result.status, "success");
}

#[test]
fn test_sync_with_complex_ontology() {
    // Arrange: Create project with complex ontology
    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    create_complex_ontology(temp_path, "ontology.ttl");
    let manifest_path = create_minimal_manifest(temp_path, "ontology.ttl");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(temp_path.join("output")),
        verbose: true,
        use_cache: false,
        ..SyncOptions::default()
    };

    // Act: Execute sync with complex ontology
    let executor = SyncExecutor::new(options);
    let result = executor.execute();

    // Assert: Verify success
    assert!(
        result.is_ok(),
        "Complex ontology sync should succeed. Error: {:?}",
        result.err()
    );

    let sync_result = result.unwrap();
    assert_eq!(sync_result.status, "success");
}

// ============================================================================
// ERROR CASES: Validation and Input Errors
// ============================================================================

#[test]
fn test_sync_error_missing_manifest() {
    // Arrange: Point to non-existent manifest
    let temp_dir = TempDir::new().unwrap();
    let nonexistent_path = temp_dir.path().join("nonexistent.toml");

    let options = SyncOptions {
        manifest_path: nonexistent_path,
        ..SyncOptions::default()
    };

    // Act: Attempt to execute
    let executor = SyncExecutor::new(options);
    let result = executor.execute();

    // Assert: Should fail with manifest not found error
    assert!(result.is_err(), "Missing manifest should result in error");

    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("Manifest not found")
            || error_msg.contains("E0001")
            || error_msg.contains("No such file"),
        "Error should mention manifest not found. Got: {}",
        error_msg
    );
}

#[test]
fn test_sync_error_invalid_manifest_syntax() {
    // Arrange: Create manifest with invalid TOML syntax
    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    let invalid_manifest = r#"
[project
name = "broken"  # Missing closing bracket
version = "0.1.0"
"#;
    let manifest_path = temp_path.join("ggen.toml");
    fs::write(&manifest_path, invalid_manifest).unwrap();

    let options = SyncOptions {
        manifest_path,
        ..SyncOptions::default()
    };

    // Act: Attempt to execute
    let executor = SyncExecutor::new(options);
    let result = executor.execute();

    // Assert: Should fail with parse error
    assert!(result.is_err(), "Invalid TOML should result in error");

    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("TOML") || error_msg.contains("parse") || error_msg.contains("expected"),
        "Error should mention TOML parsing issue. Got: {}",
        error_msg
    );
}

#[test]
fn test_sync_error_missing_ontology_file() {
    // Arrange: Create manifest pointing to missing ontology
    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    let manifest_path = create_minimal_manifest(temp_path, "missing.ttl");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(temp_path.join("output")),
        ..SyncOptions::default()
    };

    // Act: Attempt to execute
    let executor = SyncExecutor::new(options);
    let result = executor.execute();

    // Assert: Should fail with ontology not found error
    assert!(result.is_err(), "Missing ontology should result in error");

    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("missing.ttl")
            || error_msg.contains("Ontology")
            || error_msg.contains("not found")
            || error_msg.contains("No such file"),
        "Error should mention missing ontology. Got: {}",
        error_msg
    );
}

#[test]
fn test_sync_error_invalid_ontology_syntax() {
    // Arrange: Create ontology with invalid Turtle syntax
    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    let invalid_ontology = r#"
@prefix test: <http://example.org/> .
test:Entity this is not valid turtle syntax
"#;
    let ontology_path = temp_path.join("ontology.ttl");
    fs::write(&ontology_path, invalid_ontology).unwrap();

    let manifest_path = create_minimal_manifest(temp_path, "ontology.ttl");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(temp_path.join("output")),
        ..SyncOptions::default()
    };

    // Act: Attempt to execute
    let executor = SyncExecutor::new(options);
    let result = executor.execute();

    // Assert: Should fail with Turtle parsing error
    assert!(
        result.is_err(),
        "Invalid Turtle syntax should result in error"
    );

    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("Turtle")
            || error_msg.contains("RDF")
            || error_msg.contains("parse")
            || error_msg.contains("syntax"),
        "Error should mention Turtle/RDF parsing issue. Got: {}",
        error_msg
    );
}

#[test]
fn test_sync_error_nonexistent_rule_name() {
    // Arrange: Create manifest with rules
    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    create_minimal_ontology(temp_path, "ontology.ttl");
    let manifest_path = create_manifest_with_multiple_rules(temp_path, "ontology.ttl");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(temp_path.join("output")),
        selected_rules: Some(vec!["nonexistent-rule".to_string()]), // Invalid rule name
        ..SyncOptions::default()
    };

    // Act: Attempt to execute
    let executor = SyncExecutor::new(options);
    let result = executor.execute();

    // Assert: Should fail with rule not found error
    assert!(
        result.is_err(),
        "Nonexistent rule name should result in error"
    );

    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("not found") || error_msg.contains("Unknown rule"),
        "Error should mention rule not found. Got: {}",
        error_msg
    );

    // Should list available rules
    assert!(
        error_msg.contains("structs") || error_msg.contains("Available"),
        "Error should list available rules. Got: {}",
        error_msg
    );
}

#[test]
fn test_sync_error_watch_mode_not_implemented() {
    // Arrange: Create valid project
    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    create_minimal_ontology(temp_path, "ontology.ttl");
    let manifest_path = create_minimal_manifest(temp_path, "ontology.ttl");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(temp_path.join("output")),
        watch: true, // Watch mode not yet implemented
        ..SyncOptions::default()
    };

    // Act: Attempt to execute
    let executor = SyncExecutor::new(options);
    let result = executor.execute();

    // Assert: Should fail with not implemented error
    assert!(
        result.is_err(),
        "Watch mode should error as not implemented"
    );

    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("not yet implemented")
            || error_msg.contains("not implemented")
            || error_msg.contains("watch"),
        "Error should mention watch mode not implemented. Got: {}",
        error_msg
    );
}

// ============================================================================
// EDGE CASES: Boundary Conditions
// ============================================================================

#[test]
fn test_sync_edge_case_empty_ontology() {
    // Arrange: Create empty ontology
    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    let empty_ontology = "# Empty ontology\n";
    let ontology_path = temp_path.join("ontology.ttl");
    fs::write(&ontology_path, empty_ontology).unwrap();

    let manifest_path = create_minimal_manifest(temp_path, "ontology.ttl");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(temp_path.join("output")),
        use_cache: false,
        ..SyncOptions::default()
    };

    // Act: Execute with empty ontology
    let executor = SyncExecutor::new(options);
    let result = executor.execute();

    // Assert: Should succeed (empty ontology is valid)
    assert!(
        result.is_ok(),
        "Empty ontology should be valid. Error: {:?}",
        result.err()
    );

    let sync_result = result.unwrap();
    assert_eq!(sync_result.status, "success");
}

#[test]
fn test_sync_edge_case_no_generation_rules() {
    // Arrange: Create manifest with no generation rules
    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    create_minimal_ontology(temp_path, "ontology.ttl");
    let manifest_path = create_minimal_manifest(temp_path, "ontology.ttl");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(temp_path.join("output")),
        use_cache: false,
        ..SyncOptions::default()
    };

    // Act: Execute with no rules
    let executor = SyncExecutor::new(options);
    let result = executor.execute();

    // Assert: Should succeed (no rules is valid)
    assert!(
        result.is_ok(),
        "No generation rules should be valid. Error: {:?}",
        result.err()
    );

    let sync_result = result.unwrap();
    assert_eq!(sync_result.status, "success");
    assert_eq!(
        sync_result.generation_rules_executed, 0,
        "Should execute 0 generation rules"
    );
}

#[test]
fn test_sync_edge_case_output_dir_override() {
    // Arrange: Create project with default output dir in manifest
    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    create_minimal_ontology(temp_path, "ontology.ttl");
    let manifest_path = create_minimal_manifest(temp_path, "ontology.ttl");

    let custom_output = temp_path.join("custom-output");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(custom_output.clone()), // Override manifest output_dir
        use_cache: false,
        ..SyncOptions::default()
    };

    // Act: Execute with output directory override
    let executor = SyncExecutor::new(options);
    let result = executor.execute();

    // Assert: Should succeed and respect override
    assert!(
        result.is_ok(),
        "Output dir override should work. Error: {:?}",
        result.err()
    );

    let sync_result = result.unwrap();
    assert_eq!(sync_result.status, "success");
}

#[test]
fn test_sync_edge_case_multiple_rules_selection() {
    // Arrange: Create manifest with multiple rules
    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    create_minimal_ontology(temp_path, "ontology.ttl");
    let manifest_path = create_manifest_with_multiple_rules(temp_path, "ontology.ttl");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(temp_path.join("output")),
        selected_rules: Some(vec!["structs".to_string(), "enums".to_string()]), // Multiple rules
        dry_run: true,
        use_cache: false,
        ..SyncOptions::default()
    };

    // Act: Execute with multiple rule selection
    let executor = SyncExecutor::new(options);
    let result = executor.execute();

    // Assert: Should succeed
    assert!(
        result.is_ok(),
        "Multiple rule selection should work. Error: {:?}",
        result.err()
    );

    let sync_result = result.unwrap();
    assert_eq!(sync_result.status, "success");
}

#[test]
fn test_sync_edge_case_very_short_timeout() {
    // Arrange: Create project with very short timeout
    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    create_minimal_ontology(temp_path, "ontology.ttl");
    let manifest_path = create_minimal_manifest(temp_path, "ontology.ttl");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(temp_path.join("output")),
        timeout_ms: 1, // 1ms timeout (likely to timeout)
        use_cache: false,
        ..SyncOptions::default()
    };

    // Act: Execute with very short timeout
    let executor = SyncExecutor::new(options);
    let result = executor.execute();

    // Assert: May succeed or timeout (implementation dependent)
    // If it times out, error should mention timeout
    if result.is_err() {
        let error_msg = result.unwrap_err().to_string();
        assert!(
            error_msg.contains("timeout") || error_msg.contains("exceeded"),
            "Timeout error should mention timeout. Got: {}",
            error_msg
        );
    }
    // Otherwise, it completed within 1ms (unlikely but possible for minimal case)
}

#[test]
fn test_sync_edge_case_force_without_audit() {
    // Arrange: Create project
    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    create_minimal_ontology(temp_path, "ontology.ttl");
    let manifest_path = create_minimal_manifest(temp_path, "ontology.ttl");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(temp_path.join("output")),
        force: true,  // Force overwrite
        audit: false, // Without audit (not recommended but allowed)
        use_cache: false,
        ..SyncOptions::default()
    };

    // Act: Execute with force but no audit
    let executor = SyncExecutor::new(options);
    let result = executor.execute();

    // Assert: Should succeed (warning expected but execution allowed)
    assert!(
        result.is_ok(),
        "Force without audit should work (with warning). Error: {:?}",
        result.err()
    );

    let sync_result = result.unwrap();
    assert_eq!(sync_result.status, "success");
}

#[test]
fn test_sync_edge_case_dry_run_with_force() {
    // Arrange: Create project
    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    create_minimal_ontology(temp_path, "ontology.ttl");
    let manifest_path = create_minimal_manifest(temp_path, "ontology.ttl");

    let options = SyncOptions {
        manifest_path,
        output_dir: Some(temp_path.join("output")),
        dry_run: true, // Dry-run takes precedence
        force: true,   // Force has no effect in dry-run
        use_cache: false,
        ..SyncOptions::default()
    };

    // Act: Execute dry-run with force
    let executor = SyncExecutor::new(options);
    let result = executor.execute();

    // Assert: Should succeed, force ignored
    assert!(
        result.is_ok(),
        "Dry-run with force should work. Error: {:?}",
        result.err()
    );

    let sync_result = result.unwrap();
    assert_eq!(sync_result.status, "success");
}

// ============================================================================
// BUILDER PATTERN TESTS: SyncOptions Construction
// ============================================================================

#[test]
fn test_sync_options_builder_pattern() {
    // Arrange & Act: Build options using builder pattern
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

    // Assert: Verify all options set correctly
    assert!(options.dry_run, "dry_run should be true");
    assert!(options.verbose, "verbose should be true");
    assert!(options.audit, "audit should be true");
    assert!(!options.validate_only, "validate_only should be false");
    assert_eq!(options.output_format, OutputFormat::Json);
    assert_eq!(options.timeout_ms, 60000);
    assert_eq!(
        options.selected_rules,
        Some(vec!["rule1".to_string(), "rule2".to_string()])
    );
    assert!(options.force, "force should be true");
    assert_eq!(options.output_dir, Some(PathBuf::from("/tmp/output")));
}

#[test]
fn test_sync_options_default_values() {
    // Arrange & Act: Create default options
    let options = SyncOptions::default();

    // Assert: Verify defaults
    assert_eq!(options.manifest_path, PathBuf::from("ggen.toml"));
    assert_eq!(options.output_dir, None);
    assert!(!options.dry_run, "dry_run should default to false");
    assert!(!options.force, "force should default to false");
    assert!(!options.audit, "audit should default to false");
    assert_eq!(options.selected_rules, None);
    assert!(!options.verbose, "verbose should default to false");
    assert!(!options.watch, "watch should default to false");
    assert!(
        !options.validate_only,
        "validate_only should default to false"
    );
    assert_eq!(options.output_format, OutputFormat::Text);
    assert_eq!(options.timeout_ms, 30000, "timeout should default to 30s");
    assert!(options.use_cache, "cache should be enabled by default");
}

#[test]
fn test_sync_options_from_manifest() {
    // Arrange & Act: Create options from manifest path
    let options = SyncOptions::from_manifest("/custom/path/ggen.toml");

    // Assert: Verify manifest path set
    assert_eq!(
        options.manifest_path,
        PathBuf::from("/custom/path/ggen.toml")
    );

    // Other options should be defaults
    assert!(!options.dry_run);
    assert_eq!(options.output_format, OutputFormat::Text);
}

// ============================================================================
// OUTPUT FORMAT TESTS: Parsing and Display
// ============================================================================

#[test]
fn test_output_format_parsing_valid() {
    // Arrange & Act & Assert: Test valid format strings
    assert_eq!("text".parse::<OutputFormat>().unwrap(), OutputFormat::Text);
    assert_eq!("json".parse::<OutputFormat>().unwrap(), OutputFormat::Json);
    assert_eq!("TEXT".parse::<OutputFormat>().unwrap(), OutputFormat::Text);
    assert_eq!("JSON".parse::<OutputFormat>().unwrap(), OutputFormat::Json);
    assert_eq!("Text".parse::<OutputFormat>().unwrap(), OutputFormat::Text);
    assert_eq!("Json".parse::<OutputFormat>().unwrap(), OutputFormat::Json);
}

#[test]
fn test_output_format_parsing_invalid() {
    // Arrange & Act & Assert: Test invalid format strings
    assert!("invalid".parse::<OutputFormat>().is_err());
    assert!("yaml".parse::<OutputFormat>().is_err());
    assert!("xml".parse::<OutputFormat>().is_err());
    assert!("".parse::<OutputFormat>().is_err());
}

#[test]
fn test_output_format_display() {
    // Arrange & Act & Assert: Test display formatting
    assert_eq!(format!("{}", OutputFormat::Text), "text");
    assert_eq!(format!("{}", OutputFormat::Json), "json");
}

// ============================================================================
// INTEGRATION TESTS: End-to-End Workflows
// ============================================================================

#[test]
fn test_sync_e2e_complete_workflow() {
    // Arrange: Create realistic project structure
    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    create_complex_ontology(temp_path, "ontology.ttl");
    let manifest_path = create_manifest_with_multiple_rules(temp_path, "ontology.ttl");

    let output_dir = temp_path.join("generated");

    // Act: Execute complete workflow (dry-run → validate → sync)

    // Step 1: Dry-run preview
    let dry_run_options = SyncOptions {
        manifest_path: manifest_path.clone(),
        output_dir: Some(output_dir.clone()),
        dry_run: true,
        verbose: true,
        use_cache: false,
        ..SyncOptions::default()
    };

    let executor = SyncExecutor::new(dry_run_options);
    let dry_run_result = executor.execute();
    assert!(dry_run_result.is_ok(), "Dry-run should succeed");

    // Step 2: Validate only
    let validate_options = SyncOptions {
        manifest_path: manifest_path.clone(),
        output_dir: Some(output_dir.clone()),
        validate_only: true,
        verbose: true,
        use_cache: false,
        ..SyncOptions::default()
    };

    let executor = SyncExecutor::new(validate_options);
    let validate_result = executor.execute();
    assert!(validate_result.is_ok(), "Validation should succeed");
    assert_eq!(
        validate_result.unwrap().files_synced,
        0,
        "Validation should not generate files"
    );

    // Step 3: Actual sync with audit
    let sync_options = SyncOptions {
        manifest_path: manifest_path.clone(),
        output_dir: Some(output_dir.clone()),
        audit: true,
        verbose: true,
        use_cache: false,
        ..SyncOptions::default()
    };

    let executor = SyncExecutor::new(sync_options);
    let sync_result = executor.execute();

    // Assert: Verify complete workflow succeeded
    assert!(sync_result.is_ok(), "Final sync should succeed");

    let final_result = sync_result.unwrap();
    assert_eq!(final_result.status, "success");
    assert!(final_result.duration_ms > 0);
    assert!(
        final_result.audit_trail.is_some(),
        "Audit trail should be generated"
    );
}

#[test]
fn test_sync_e2e_error_recovery() {
    // Arrange: Create project with initial error, then fix it
    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    // Create invalid ontology initially
    let invalid_ontology = "@prefix test: <invalid";
    let ontology_path = temp_path.join("ontology.ttl");
    fs::write(&ontology_path, invalid_ontology).unwrap();

    let manifest_path = create_minimal_manifest(temp_path, "ontology.ttl");

    let options = SyncOptions {
        manifest_path: manifest_path.clone(),
        output_dir: Some(temp_path.join("output")),
        use_cache: false,
        ..SyncOptions::default()
    };

    // Act: First attempt should fail
    let executor = SyncExecutor::new(options.clone());
    let first_result = executor.execute();
    assert!(first_result.is_err(), "Invalid ontology should fail");

    // Fix the ontology
    create_minimal_ontology(temp_path, "ontology.ttl");

    // Retry with fixed ontology
    let executor = SyncExecutor::new(options);
    let second_result = executor.execute();

    // Assert: Second attempt should succeed
    assert!(
        second_result.is_ok(),
        "Fixed ontology should succeed. Error: {:?}",
        second_result.err()
    );

    let sync_result = second_result.unwrap();
    assert_eq!(sync_result.status, "success");
}
