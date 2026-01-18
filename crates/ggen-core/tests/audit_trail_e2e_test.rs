//! End-to-end audit trail integration test
//!
//! Verifies that audit trail is correctly integrated into the sync executor
//! and persists audit.json with complete metadata for all sync operations.

use ggen_core::codegen::executor::SyncExecutor;
use ggen_core::codegen::{OutputFormat, SyncOptions};
use serde_json::Value;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

/// Test that audit trail is created during basic sync operation
#[test]
fn test_basic_sync_creates_audit_trail() {
    // Arrange: Create minimal ggen.toml manifest with audit enabled
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let manifest_path = temp_dir.path().join("ggen.toml");
    let ontology_path = temp_dir.path().join("ontology.ttl");
    let output_dir = temp_dir.path().join("output");

    fs::create_dir_all(&output_dir).expect("Failed to create output dir");

    // Minimal valid ontology
    fs::write(
        &ontology_path,
        r#"
@prefix ex: <http://example.org/> .
ex:User a ex:Entity ;
    ex:name "User" .
"#,
    )
    .expect("Failed to write ontology");

    // Minimal manifest with audit enabled
    fs::write(
        &manifest_path,
        format!(
            r#"
[project]
name = "test-project"
version = "1.0.0"

[ontology]
source = "ontology.ttl"

[inference]
rules = []

[generation]
output_dir = "output"
require_audit_trail = true

[[generation.rules]]
name = "test_rule"
output_file = "test.txt"

[generation.rules.query]
inline = "PREFIX ex: <http://example.org/> SELECT ?name WHERE {{ ?s ex:name ?name }}"

[generation.rules.template]
inline = "Name: {{{{ name }}}}"
"#
        ),
    )
    .expect("Failed to write manifest");

    // Act: Execute sync with audit flag
    let options = SyncOptions {
        manifest_path: manifest_path.clone(),
        output_dir: None,
        dry_run: false,
        validate_only: false,
        verbose: false,
        selected_rules: None,
        force: false,
        audit: true, // Enable audit trail
        output_format: OutputFormat::Text,
        watch: false,
        timeout_ms: 30000,
        use_cache: true,
        cache_dir: None,
        max_parallelism: None,
    };

    let executor = SyncExecutor::new(options);
    let result = executor.execute().expect("Sync should succeed");

    // Assert: Verify audit trail was created
    assert_eq!(result.status, "success");
    assert!(
        result.audit_trail.is_some(),
        "Audit trail path should be present"
    );

    let audit_path = PathBuf::from(result.audit_trail.unwrap());
    assert!(
        audit_path.exists(),
        "audit.json should exist at {:?}",
        audit_path
    );

    // Verify audit.json contains valid JSON
    let content = fs::read_to_string(&audit_path).expect("Failed to read audit.json");
    let json: Value = serde_json::from_str(&content).expect("audit.json should be valid JSON");

    // Verify metadata fields
    assert!(json["metadata"]["ggen_version"].is_string());
    assert_eq!(
        json["metadata"]["manifest_path"].as_str(),
        Some(manifest_path.display().to_string().as_str())
    );
    assert!(json["metadata"]["duration_ms"].is_number());

    // Verify rule execution tracking
    assert!(
        json["rules_executed"].as_u64().unwrap() > 0,
        "Should have executed at least one rule"
    );
}

/// Test that audit trail tracks 10 different sync types (generation rules)
#[test]
fn test_audit_tracks_ten_sync_types() {
    // Arrange: Create manifest with 10 generation rules
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let manifest_path = temp_dir.path().join("ggen.toml");
    let ontology_path = temp_dir.path().join("ontology.ttl");
    let output_dir = temp_dir.path().join("output");

    fs::create_dir_all(&output_dir).expect("Failed to create output dir");

    // Ontology with 10 entities
    fs::write(
        &ontology_path,
        r#"
@prefix ex: <http://example.org/> .
ex:Entity1 a ex:Entity ; ex:name "Entity1" .
ex:Entity2 a ex:Entity ; ex:name "Entity2" .
ex:Entity3 a ex:Entity ; ex:name "Entity3" .
ex:Entity4 a ex:Entity ; ex:name "Entity4" .
ex:Entity5 a ex:Entity ; ex:name "Entity5" .
ex:Entity6 a ex:Entity ; ex:name "Entity6" .
ex:Entity7 a ex:Entity ; ex:name "Entity7" .
ex:Entity8 a ex:Entity ; ex:name "Entity8" .
ex:Entity9 a ex:Entity ; ex:name "Entity9" .
ex:Entity10 a ex:Entity ; ex:name "Entity10" .
"#,
    )
    .expect("Failed to write ontology");

    // Manifest with 10 generation rules
    let mut manifest_content = format!(
        r#"
[project]
name = "test-project"
version = "1.0.0"

[ontology]
source = "ontology.ttl"

[inference]
rules = []

[generation]
output_dir = "output"
require_audit_trail = true

"#
    );

    for i in 1..=10 {
        manifest_content.push_str(&format!(
            r#"
[[generation.rules]]
name = "rule_{}"
output_file = "entity_{}.txt"

[generation.rules.query]
inline = "PREFIX ex: <http://example.org/> SELECT ?name WHERE {{ ex:Entity{} ex:name ?name }}"

[generation.rules.template]
inline = "Entity {}: {{{{ name }}}}"

"#,
            i, i, i, i
        ));
    }

    fs::write(&manifest_path, manifest_content).expect("Failed to write manifest");

    // Act: Execute sync
    let options = SyncOptions {
        manifest_path: manifest_path.clone(),
        output_dir: None,
        dry_run: false,
        validate_only: false,
        verbose: false,
        selected_rules: None,
        force: false,
        audit: true,
        output_format: OutputFormat::Text,
        watch: false,
        timeout_ms: 30000,
        use_cache: true,
        cache_dir: None,
        max_parallelism: None,
    };

    let executor = SyncExecutor::new(options);
    let result = executor.execute().expect("Sync should succeed");

    // Assert: Verify all 10 rules executed
    assert_eq!(
        result.generation_rules_executed, 10,
        "Should execute 10 generation rules"
    );

    let audit_path = PathBuf::from(result.audit_trail.unwrap());
    let content = fs::read_to_string(&audit_path).expect("Failed to read audit.json");
    let json: Value = serde_json::from_str(&content).expect("audit.json should be valid JSON");

    // Verify rules_executed is 10 (no inference rules, only generation)
    assert_eq!(
        json["rules_executed"].as_u64(),
        Some(10),
        "Should have executed 10 rules"
    );

    // Verify files_changed is 10
    assert_eq!(
        json["files_changed"].as_u64(),
        Some(10),
        "Should have changed 10 files"
    );

    // Verify file_hashes contains 10 entries
    let file_hashes = json["file_hashes"]
        .as_object()
        .expect("file_hashes should be object");
    assert_eq!(file_hashes.len(), 10, "Should have 10 file hashes");
}

/// Test that audit.json is readable and valid JSON after sync
#[test]
fn test_audit_json_readable_and_valid() {
    // Arrange: Minimal sync setup
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let manifest_path = temp_dir.path().join("ggen.toml");
    let ontology_path = temp_dir.path().join("ontology.ttl");
    let output_dir = temp_dir.path().join("output");

    fs::create_dir_all(&output_dir).expect("Failed to create output dir");

    fs::write(&ontology_path, "@prefix ex: <http://example.org/> .")
        .expect("Failed to write ontology");

    fs::write(
        &manifest_path,
        format!(
            r#"
[project]
name = "test-project"
version = "1.0.0"

[ontology]
source = "ontology.ttl"

[inference]
rules = []

[generation]
output_dir = "output"
require_audit_trail = true
rules = []
"#
        ),
    )
    .expect("Failed to write manifest");

    // Act: Execute sync (with no generation rules)
    let options = SyncOptions {
        manifest_path: manifest_path.clone(),
        output_dir: None,
        dry_run: false,
        validate_only: false,
        verbose: false,
        selected_rules: None,
        force: false,
        audit: true,
        output_format: OutputFormat::Text,
        watch: false,
        timeout_ms: 30000,
        use_cache: true,
        cache_dir: None,
        max_parallelism: None,
    };

    let executor = SyncExecutor::new(options);
    let result = executor.execute().expect("Sync should succeed");

    // Assert: Verify audit.json is readable
    let audit_path = PathBuf::from(result.audit_trail.unwrap());
    assert!(audit_path.exists(), "audit.json should exist");

    // Read and parse JSON
    let content = fs::read_to_string(&audit_path).expect("Should be able to read audit.json");
    let json: Value = serde_json::from_str(&content).expect("Should parse valid JSON");

    // Verify JSON structure
    assert!(json.is_object(), "Root should be JSON object");
    assert!(json["timestamp"].is_string(), "timestamp should be string");
    assert!(
        json["rules_executed"].is_number(),
        "rules_executed should be number"
    );
    assert!(
        json["files_changed"].is_number(),
        "files_changed should be number"
    );
    assert!(
        json["file_hashes"].is_object(),
        "file_hashes should be object"
    );
    assert!(json["metadata"].is_object(), "metadata should be object");

    // Verify metadata structure
    let metadata = &json["metadata"];
    assert!(
        metadata["ggen_version"].is_string(),
        "ggen_version should be string"
    );
    assert!(
        metadata["manifest_path"].is_string(),
        "manifest_path should be string"
    );
    assert!(
        metadata["ontology_path"].is_string(),
        "ontology_path should be string"
    );
    assert!(
        metadata["spec_hash"].is_string(),
        "spec_hash should be string"
    );
    assert!(
        metadata["duration_ms"].is_number(),
        "duration_ms should be number"
    );

    // Verify timestamp is RFC3339 format
    let timestamp = json["timestamp"]
        .as_str()
        .expect("timestamp should be string");
    assert!(
        timestamp.contains('T'),
        "timestamp should contain 'T' (RFC3339)"
    );
}
