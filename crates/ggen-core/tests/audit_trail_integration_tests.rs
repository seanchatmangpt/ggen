//! Integration tests for audit trail functionality using Chicago School TDD (AAA pattern)
//!
//! Tests verify observable state (files, JSON content) using real TempDir and files.
//! No mocks - tests verify actual system behavior.

use ggen_core::audit::{AuditTrail, writer::AuditTrailWriter};
use serde_json::Value;
use std::fs;
use tempfile::TempDir;

/// Test that audit trail is created and written to file system
#[test]
fn test_audit_trail_created_and_written() {
    // Arrange: Create audit trail with metadata
    let audit = AuditTrail::new("5.0.2", "ggen.toml", "ontology.ttl");
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let audit_path = temp_dir.path().join("audit.json");

    // Act: Write audit trail to file
    let result = AuditTrailWriter::write(&audit, &audit_path);

    // Assert: Verify file was created successfully
    assert!(result.is_ok(), "Audit writer should succeed");
    assert!(
        audit_path.exists(),
        "audit.json should exist at {:?}",
        audit_path
    );

    // Verify file is valid JSON
    let content = fs::read_to_string(&audit_path).expect("Failed to read audit.json");
    let json: Value =
        serde_json::from_str(&content).expect("audit.json should be valid JSON");

    assert!(
        json.is_object(),
        "audit.json should contain a JSON object"
    );
}

/// Test that audit.json contains all required metadata fields
#[test]
fn test_audit_json_contains_metadata() {
    // Arrange: Create audit trail with metadata
    let mut audit = AuditTrail::new("5.0.2", "test.toml", "test.ttl");
    audit.metadata.spec_hash = "abc123def456".to_string();
    audit.metadata.duration_ms = 150;

    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let audit_path = temp_dir.path().join("audit.json");

    // Act: Write audit trail to file
    AuditTrailWriter::write(&audit, &audit_path).expect("Failed to write audit trail");

    // Assert: Verify file exists and contains metadata
    assert!(
        audit_path.exists(),
        "audit.json should exist after write"
    );

    let content = fs::read_to_string(&audit_path).expect("Failed to read audit.json");
    let json: Value = serde_json::from_str(&content).expect("audit.json should be valid JSON");

    // Verify required metadata fields
    assert_eq!(
        json["metadata"]["ggen_version"].as_str(),
        Some("5.0.2"),
        "ggen_version should match"
    );
    assert_eq!(
        json["metadata"]["manifest_path"].as_str(),
        Some("test.toml"),
        "manifest_path should match"
    );
    assert_eq!(
        json["metadata"]["ontology_path"].as_str(),
        Some("test.ttl"),
        "ontology_path should match"
    );
    assert_eq!(
        json["metadata"]["spec_hash"].as_str(),
        Some("abc123def456"),
        "spec_hash should match"
    );
    assert_eq!(
        json["metadata"]["duration_ms"].as_u64(),
        Some(150),
        "duration_ms should match"
    );

    // Verify timestamp is RFC3339 format (can have Z or timezone offset like -08:00)
    let timestamp = json["timestamp"]
        .as_str()
        .expect("timestamp should be string");
    assert!(
        timestamp.contains('T') && (timestamp.contains('Z') || timestamp.contains('+') || timestamp.contains('-')),
        "timestamp should be RFC3339 format, got: {}",
        timestamp
    );
}

/// Test that audit contains executed inference rules count
#[test]
fn test_audit_contains_executed_rules() {
    // Arrange: Create audit trail and record rule executions
    let mut audit = AuditTrail::new("5.0.2", "test.toml", "test.ttl");

    // Act: Record multiple rule executions
    audit.record_rule_executed();
    audit.record_rule_executed();
    audit.record_rule_executed();

    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let audit_path = temp_dir.path().join("audit.json");
    AuditTrailWriter::write(&audit, &audit_path).expect("Failed to write audit trail");

    // Assert: Verify rules_executed count in JSON
    let content = fs::read_to_string(&audit_path).expect("Failed to read audit.json");
    let json: Value = serde_json::from_str(&content).expect("audit.json should be valid JSON");

    assert_eq!(
        json["rules_executed"].as_u64(),
        Some(3),
        "rules_executed should be 3"
    );

    // Verify initial files_changed is 0
    assert_eq!(
        json["files_changed"].as_u64(),
        Some(0),
        "files_changed should be 0 initially"
    );
}

/// Test that audit contains file hashes for generated files
#[test]
fn test_audit_contains_file_hashes() {
    // Arrange: Create audit trail
    let mut audit = AuditTrail::new("5.0.2", "test.toml", "test.ttl");

    // Act: Record file changes with SHA256 hashes
    audit.record_file_change(
        "src/generated/user.rs".to_string(),
        "abc123def456789".to_string(),
    );
    audit.record_file_change(
        "src/generated/product.rs".to_string(),
        "fedcba987654321".to_string(),
    );

    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let audit_path = temp_dir.path().join("audit.json");
    AuditTrailWriter::write(&audit, &audit_path).expect("Failed to write audit trail");

    // Assert: Verify file_hashes contains correct mappings
    let content = fs::read_to_string(&audit_path).expect("Failed to read audit.json");
    let json: Value = serde_json::from_str(&content).expect("audit.json should be valid JSON");

    let file_hashes = json["file_hashes"]
        .as_object()
        .expect("file_hashes should be object");

    assert_eq!(
        file_hashes.get("src/generated/user.rs").and_then(|v| v.as_str()),
        Some("abc123def456789"),
        "user.rs hash should match"
    );
    assert_eq!(
        file_hashes.get("src/generated/product.rs").and_then(|v| v.as_str()),
        Some("fedcba987654321"),
        "product.rs hash should match"
    );

    // Verify files_changed count matches
    assert_eq!(
        json["files_changed"].as_u64(),
        Some(2),
        "files_changed should be 2"
    );
}

/// Test that audit trail directory is created automatically
#[test]
fn test_audit_trail_creates_directory() {
    // Arrange: Create audit trail with nested output path
    let audit = AuditTrail::new("5.0.2", "test.toml", "test.ttl");
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let nested_path = temp_dir
        .path()
        .join("nested/dir/structure/audit.json");

    // Act: Write audit trail (should create parent directories)
    let result = AuditTrailWriter::write(&audit, &nested_path);

    // Assert: Verify directory was created and file exists
    assert!(
        result.is_ok(),
        "Writer should create parent directories"
    );
    assert!(
        nested_path.exists(),
        "audit.json should exist at nested path"
    );
    assert!(
        nested_path.parent().unwrap().exists(),
        "Parent directory should exist"
    );
}

/// Test audit trail serialization roundtrip
#[test]
fn test_audit_trail_serialization_roundtrip() {
    // Arrange: Create complex audit trail
    let mut audit = AuditTrail::new("5.0.2", "manifest.toml", "ontology.ttl");
    audit.metadata.spec_hash = "hash123".to_string();
    audit.metadata.duration_ms = 250;
    audit.record_rule_executed();
    audit.record_file_change("file.rs".to_string(), "filehash".to_string());

    // Act: Serialize to JSON and deserialize back
    let json = audit.to_json().expect("Serialization should succeed");
    let deserialized: AuditTrail =
        serde_json::from_str(&json).expect("Deserialization should succeed");

    // Assert: Verify roundtrip preserves all data
    assert_eq!(deserialized.rules_executed, 1);
    assert_eq!(deserialized.files_changed, 1);
    assert_eq!(deserialized.metadata.ggen_version, "5.0.2");
    assert_eq!(deserialized.metadata.spec_hash, "hash123");
    assert_eq!(deserialized.metadata.duration_ms, 250);
    assert_eq!(
        deserialized.file_hashes.get("file.rs"),
        Some(&"filehash".to_string())
    );
}

/// Test that audit trail tracks multiple file changes correctly
#[test]
fn test_audit_trail_tracks_multiple_files() {
    // Arrange: Create audit trail
    let mut audit = AuditTrail::new("5.0.2", "test.toml", "test.ttl");

    // Act: Record multiple file changes
    for i in 0..10 {
        audit.record_file_change(
            format!("file_{}.rs", i),
            format!("hash_{}", i),
        );
    }

    // Assert: Verify count and hashes
    assert_eq!(audit.files_changed, 10);
    assert_eq!(audit.file_hashes.len(), 10);

    // Verify specific files exist
    assert_eq!(
        audit.file_hashes.get("file_0.rs"),
        Some(&"hash_0".to_string())
    );
    assert_eq!(
        audit.file_hashes.get("file_9.rs"),
        Some(&"hash_9".to_string())
    );
}
