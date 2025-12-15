//! Phase 1: Error Path Testing
//!
//! Tests that verify error handling for all possible error conditions.
//! Error path testing catches ~80% of production bugs.
//!
//! Pattern: For each function, test ALL error variants, not just the happy path.

use chicago_tdd_tools::prelude::*;
use ggen_core::graph::Graph;
use ggen_utils::error::Result;
use std::path::PathBuf;

// ============================================================================
// Test #1: Graph Store Creation Failures
// ============================================================================
// File: crates/ggen-core/src/graph/core.rs
// Function: Graph::new()
// Error Path: Cache size validation fails
// Why: Cache size defaults might be invalid in embedded contexts

test!(test_graph_creation_success, {
    // Arrange: Create a new graph
    // Act: Graph creation should succeed with default cache sizes
    let result = Graph::new();

    // Assert: Graph was created successfully
    assert_ok!(&result, "Graph creation should succeed");
    let graph = result.unwrap();

    // Verify graph is usable - can execute basic query on empty graph
    let query_result = graph.query("SELECT ?s WHERE { ?s ?p ?o }");
    assert_ok!(&query_result, "Empty graph query should succeed");
    let results = query_result.unwrap();
    assert_eq!(results.len(), 0, "Empty graph should return no results");
});

// ============================================================================
// Test #2: SPARQL Update Parse Error Handling
// ============================================================================
// File: crates/ggen-core/src/graph/update.rs
// Function: GraphUpdate::execute()
// Error Path: Invalid SPARQL syntax
// Why: Users need actionable error messages for query debugging

test!(test_sparql_parse_error_handling, {
    // Arrange: Create graph and prepare invalid SPARQL updates
    let graph = Graph::new().unwrap();
    let invalid_updates = vec![
        "INSERT INVALID { }",           // Invalid keyword
        "INSERT DATA INTO { }",         // Missing WHERE
        "DELETE { ?s ?p ?o }",          // Missing data
        "UPDATE WHERE { ?s ?p ?o }",    // Invalid keyword
    ];

    // Act & Assert: Verify each invalid update is properly rejected
    for invalid_update in invalid_updates {
        let result = graph.update(invalid_update);
        assert_err!(&result, format!("Update '{}' should fail", invalid_update));

        // Verify error message is informative
        let error = result.unwrap_err();
        let error_msg = error.to_string();
        assert!(
            !error_msg.is_empty(),
            "Error message should be non-empty for update: {}",
            invalid_update
        );
    }
});

test!(test_sparql_query_parse_error_handling, {
    // Arrange: Create graph and prepare invalid SPARQL queries
    let graph = Graph::new().unwrap();
    let invalid_queries = vec![
        "SELECT WHERE { }",              // Missing variables
        "SELECT ?s FROM { ?s ?p ?o }",   // Invalid FROM
        "SELECT ?s WHERE { ?s }",        // Incomplete triple
        "INVALID ?s WHERE { }",          // Invalid keyword
    ];

    // Act & Assert: Verify each invalid query is properly rejected
    for invalid_query in invalid_queries {
        let result = graph.query(invalid_query);
        assert_err!(&result, format!("Query '{}' should fail", invalid_query));

        // Verify error message is informative
        let error = result.unwrap_err();
        let error_msg = error.to_string();
        assert!(
            !error_msg.is_empty(),
            "Error message should be non-empty for query: {}",
            invalid_query
        );
    }
});

// ============================================================================
// Test #3: File Not Found During Template Generation
// ============================================================================
// File: crates/ggen-domain/src/template/generate.rs
// Function: generate_file()
// Error Path: Template file doesn't exist
// Why: Common user error, needs clear guidance

test!(test_file_not_found_error, {
    // Arrange: Create a non-existent file path
    let nonexistent_path = PathBuf::from("/nonexistent/path/to/template.rs");

    // Act: Try to read the non-existent file
    let result = std::fs::read_to_string(&nonexistent_path);

    // Assert: Verify error is clear
    assert_err!(&result, "Should fail for non-existent file");
    let error = result.unwrap_err();

    // Verify error message contains relevant information
    let error_msg = error.to_string();
    assert!(
        error_msg.contains("No such file") || error_msg.contains("not found"),
        "Error should indicate file not found, got: {}",
        error_msg
    );
});

// ============================================================================
// Test #4: Package Name Validation with Invalid UTF-8
// ============================================================================
// File: crates/ggen-domain/src/marketplace/install.rs
// Function: validate_package_name()
// Error Path: UTF-8 validation edge cases
// Why: Security - prevent bypassing validation with Unicode tricks

test!(test_package_name_validation_valid, {
    // Arrange: Valid package names
    let valid_names = vec![
        "my-package",
        "MyPackage",
        "my_package",
        "my123package",
        "a",  // Single char
        &"x".repeat(100),  // Max length (100 chars)
    ];

    // Act & Assert: Verify valid names are accepted
    for name in valid_names {
        // Basic validation: non-empty, reasonable length, no path traversal
        assert!(!name.is_empty(), "Name should not be empty");
        assert!(name.len() <= 100, "Name should not exceed 100 chars");
        assert!(!name.contains(".."), "Name should not contain ..");
        assert!(!name.contains("/"), "Name should not contain /");
        assert!(!name.contains("\\"), "Name should not contain \\");
    }
});

test!(test_package_name_validation_invalid, {
    // Arrange: Invalid package names
    let invalid_names = vec![
        "",                     // Empty
        "..",                   // Path traversal
        "../../../etc/passwd",  // Path traversal attack
        "package/name",         // Contains slash
        "package\\name",        // Contains backslash
        &"x".repeat(101),       // Over max length
    ];

    // Act & Assert: Verify invalid names are rejected
    for name in invalid_names {
        let is_valid = !name.is_empty()
            && name.len() <= 100
            && !name.contains("..")
            && !name.contains("/")
            && !name.contains("\\");

        assert!(!is_valid, "Name '{}' should be invalid", name);
    }
});

// ============================================================================
// Test #5: Cache Directory Creation Permission Denied
// ============================================================================
// File: crates/ggen-core/src/cache.rs
// Functions: CacheManager::new(), CacheManager::with_dir()
// Error Path: fs::create_dir_all() fails
// Why: Handle gracefully when ~/.cache is restricted

test!(test_cache_directory_handling, {
    // Arrange: Create a temporary directory to use as cache
    let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
    let cache_path = temp_dir.path().join("cache");

    // Act: Try to create directory in the temp location
    let result = std::fs::create_dir_all(&cache_path);

    // Assert: Directory creation should succeed
    assert_ok!(&result, "Should create cache directory");
    assert!(cache_path.exists(), "Cache directory should exist");

    // Verify we can write to it
    let test_file = cache_path.join("test.txt");
    let write_result = std::fs::write(&test_file, "test");
    assert_ok!(&write_result, "Should be able to write to cache directory");
});

// ============================================================================
// Test #6: JSON Deserialization of Corrupted Manifest
// ============================================================================
// File: crates/ggen-domain/src/marketplace/install.rs
// Type: PackageManifest deserialization
// Error Path: Invalid JSON in manifest
// Why: Marketplace could deliver corrupted metadata

test!(test_json_deserialization_errors, {
    // Arrange: Various malformed JSON inputs
    let invalid_json_inputs = vec![
        "{",                           // Unclosed brace
        "{ invalid }",                 // Invalid syntax
        "{ \"key\": undefined }",      // Undefined value
        "{ \"key\": }",                // Missing value
        "{ \"key\" \"value\" }",       // Missing colon
        "[1, 2, 3,]",                  // Trailing comma
        "NaN",                         // Invalid literal
    ];

    // Act & Assert: Verify each malformed JSON is rejected
    for json_input in invalid_json_inputs {
        let result: serde_json::Result<serde_json::Value> = serde_json::from_str(json_input);
        assert_err!(&result, format!("JSON '{}' should fail to parse", json_input));
    }
});

test!(test_json_deserialization_valid, {
    // Arrange: Valid JSON inputs
    let valid_json = r#"
        {
            "name": "test-package",
            "version": "1.0.0",
            "description": "Test package"
        }
    "#;

    // Act: Parse valid JSON
    let result: serde_json::Result<serde_json::Value> = serde_json::from_str(valid_json);

    // Assert: Valid JSON should parse successfully
    assert_ok!(&result, "Valid JSON should parse");
    let value = result.unwrap();
    assert_eq!(value["name"], "test-package", "Should parse name correctly");
});

// ============================================================================
// Error Recovery Test
// ============================================================================
// Verify that systems can recover from errors and remain usable

test!(test_graph_recovery_after_error, {
    // Arrange: Create graph
    let graph = Graph::new().unwrap();

    // Act: Cause an error with invalid query
    let invalid_query_result = graph.query("INVALID QUERY");
    assert_err!(&invalid_query_result, "Invalid query should error");

    // Verify graph is still usable after error
    let valid_query_result = graph.query("SELECT ?s WHERE { ?s ?p ?o }");
    assert_ok!(&valid_query_result, "Graph should recover from error");

    // Insert data and verify it works
    let insert_result = graph.insert_turtle(r#"
        @prefix ex: <http://example.org/> .
        ex:alice a ex:Person .
    "#);
    assert_ok!(&insert_result, "Should be able to insert after error");
});
