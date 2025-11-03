//! Error Scenario Tests - Comprehensive Error Path Coverage
//!
//! Tests that verify graceful failure handling and error recovery.
//! Part of the 80/20 strategy: test the 20% of error cases that cause 80% of bugs.

use ggen_marketplace::prelude::*;
use std::path::PathBuf;

// ============================================================================
// ERROR SCENARIO 1: Network Failures (for CentralizedRegistry)
// ============================================================================

#[tokio::test]
async fn error_invalid_registry_url() {
    // Arrange: Invalid URL
    let result = CentralizedRegistry::new("not-a-valid-url");

    // Assert: Should succeed (URL validation happens on request, not construction)
    // This is correct behavior - fail fast on actual use, not construction
    assert!(result.is_ok());
}

#[tokio::test]
async fn error_nonexistent_registry_url() {
    // Arrange: Valid URL but non-existent server
    let registry = CentralizedRegistry::new("https://this-domain-does-not-exist-12345.com")
        .expect("registry creation failed");

    // Act: Try to fetch index (will timeout/fail)
    let result = registry.search(&Query::new("test")).await;

    // Assert: Should fail with network error
    assert!(result.is_err(), "Should fail with network error");
}

// ============================================================================
// ERROR SCENARIO 2: Filesystem Errors
// ============================================================================

#[tokio::test]
async fn error_readonly_filesystem() {
    // Note: This test is platform-dependent and might need adjustment
    // On Unix: chmod 444 makes directory read-only
    // On Windows: Different mechanism needed

    #[cfg(unix)]
    {
        use std::fs;
        use std::os::unix::fs::PermissionsExt;

        let temp_dir = tempfile::tempdir().expect("temp dir failed");
        let readonly_path = temp_dir.path().join("readonly");

        // Create directory and make it read-only
        fs::create_dir(&readonly_path).expect("mkdir failed");
        let mut perms = fs::metadata(&readonly_path)
            .expect("metadata failed")
            .permissions();
        perms.set_mode(0o444); // Read-only
        fs::set_permissions(&readonly_path, perms).expect("chmod failed");

        // Try to create storage in read-only directory
        let result = FilesystemStore::new(readonly_path.join("storage")).await;

        // Assert: Should fail with permission error
        assert!(
            result.is_err(),
            "Should fail when creating storage in read-only directory"
        );
    }
}

// ============================================================================
// ERROR SCENARIO 3: Invalid Data
// ============================================================================

#[tokio::test]
async fn error_corrupted_registry_index() {
    let temp_dir = tempfile::tempdir().expect("temp dir failed");
    let db_path = temp_dir.path().join("registry");

    // Create registry directory with corrupted index.json
    tokio::fs::create_dir_all(&db_path)
        .await
        .expect("mkdir failed");

    let index_path = db_path.join("index.json");
    tokio::fs::write(&index_path, b"{ invalid json }")
        .await
        .expect("write failed");

    // Try to load registry
    let result = LocalRegistry::new(db_path).await;

    // Assert: Should fail with parse error
    assert!(
        result.is_err(),
        "Should fail when index.json is corrupted"
    );
}

// ============================================================================
// ERROR SCENARIO 4: Resource Exhaustion
// ============================================================================

#[tokio::test]
async fn error_extremely_large_content() {
    // Arrange: Try to store 100MB content in memory
    let store = MemoryStore::new();

    // Create 100MB of data
    let large_content = vec![0u8; 100 * 1024 * 1024];

    // Act: Store large content
    let result = store.store(&large_content).await;

    // Assert: Should either succeed or fail gracefully (not panic)
    // For MemoryStore, this should succeed (just slow)
    assert!(
        result.is_ok(),
        "Large content storage should not panic"
    );
}

// ============================================================================
// ERROR SCENARIO 5: Concurrent Modifications
// ============================================================================

#[tokio::test]
async fn error_concurrent_delete_race() {
    use std::sync::Arc;

    let store = Arc::new(MemoryStore::new());

    // Store content
    let content = b"content to delete";
    let id = store.store(content).await.expect("store failed");

    // Spawn multiple tasks trying to delete same content
    let mut handles = vec![];

    for _ in 0..10 {
        let store_clone = Arc::clone(&store);
        let id_clone = id.clone();

        let handle = tokio::spawn(async move {
            store_clone.delete(&id_clone).await
        });

        handles.push(handle);
    }

    // Wait for all deletes
    let results: Vec<_> = futures::future::join_all(handles).await;

    // Assert: Only one delete should succeed, rest should fail
    let successes = results
        .iter()
        .filter(|r| r.as_ref().unwrap().is_ok())
        .count();

    assert_eq!(
        successes, 1,
        "Only one concurrent delete should succeed"
    );
}

// ============================================================================
// ERROR SCENARIO 6: Invalid Package Data
// ============================================================================

#[tokio::test]
async fn error_package_missing_required_fields() {
    // Test 1: Missing title
    let result = Package::builder(
        PackageId::new("test", "invalid"),
        Version::new(1, 0, 0),
    )
    .description("Has description")
    .license("MIT")
    .content_id(ContentId::new("hash", HashAlgorithm::Sha256))
    .build();

    assert!(result.is_err(), "Should fail without title");

    // Test 2: Missing description
    let result = Package::builder(
        PackageId::new("test", "invalid"),
        Version::new(1, 0, 0),
    )
    .title("Has title")
    .license("MIT")
    .content_id(ContentId::new("hash", HashAlgorithm::Sha256))
    .build();

    assert!(result.is_err(), "Should fail without description");

    // Test 3: Missing content_id
    let result = Package::builder(
        PackageId::new("test", "invalid"),
        Version::new(1, 0, 0),
    )
    .title("Has title")
    .description("Has description")
    .license("MIT")
    .build();

    assert!(result.is_err(), "Should fail without content_id");
}

// ============================================================================
// ERROR SCENARIO 7: Query Edge Cases
// ============================================================================

#[tokio::test]
async fn error_empty_query_string() {
    let temp_dir = tempfile::tempdir().expect("temp dir failed");
    let db_path = temp_dir.path().join("registry");
    let registry = LocalRegistry::new(db_path).await.expect("registry creation failed");

    // Publish some packages
    let pkg = Package::builder(PackageId::new("test", "example"), Version::new(1, 0, 0))
        .title("Example")
        .description("Test package")
        .license("MIT")
        .content_id(ContentId::new("hash", HashAlgorithm::Sha256))
        .build()
        .expect("build failed");

    registry.publish(pkg).await.expect("publish failed");

    // Search with empty query
    let results = registry.search(&Query::new("")).await.expect("search failed");

    // Assert: Empty query should return all packages (not error)
    assert!(
        !results.is_empty(),
        "Empty query should return all packages"
    );
}

#[tokio::test]
async fn error_query_special_characters() {
    let temp_dir = tempfile::tempdir().expect("temp dir failed");
    let db_path = temp_dir.path().join("registry");
    let registry = LocalRegistry::new(db_path).await.expect("registry creation failed");

    // Test queries with special characters
    let special_queries = vec![
        "query with spaces",
        "query-with-dashes",
        "query_with_underscores",
        "query.with.dots",
        "query@with@ats",
        "query/with/slashes",
        "query\\with\\backslashes",
        "query<with>brackets",
        "query[with]square",
    ];

    for query_str in special_queries {
        let result = registry.search(&Query::new(query_str)).await;

        // Assert: Should not panic or error, even with special chars
        assert!(
            result.is_ok(),
            "Search should handle special characters: {}",
            query_str
        );
    }
}

// ============================================================================
// ERROR SCENARIO 8: Version Edge Cases
// ============================================================================

#[tokio::test]
async fn error_version_zero() {
    // Test: Version 0.0.0
    let result = Package::builder(
        PackageId::new("test", "v0"),
        Version::new(0, 0, 0),
    )
    .title("Zero Version")
    .description("Test")
    .license("MIT")
    .content_id(ContentId::new("hash", HashAlgorithm::Sha256))
    .build();

    // Assert: 0.0.0 should be valid (common for pre-release)
    assert!(result.is_ok(), "Version 0.0.0 should be valid");
}

// ============================================================================
// ERROR SCENARIO 9: Content Edge Cases
// ============================================================================

#[tokio::test]
async fn error_empty_content() {
    let store = MemoryStore::new();

    // Act: Store empty content
    let result = store.store(b"").await;

    // Assert: Empty content should be valid
    assert!(result.is_ok(), "Empty content should be storable");

    if let Ok(id) = result {
        // Should be retrievable
        let retrieved = store.retrieve(&id).await.expect("retrieve failed");
        assert_eq!(retrieved, b"", "Empty content should be retrievable");
    }
}

#[tokio::test]
async fn error_binary_content() {
    let store = MemoryStore::new();

    // Create binary content (all byte values 0-255)
    let binary_content: Vec<u8> = (0..=255).collect();

    // Act: Store binary content
    let result = store.store(&binary_content).await;

    // Assert: Binary content should work fine
    assert!(result.is_ok(), "Binary content should be storable");

    if let Ok(id) = result {
        let retrieved = store.retrieve(&id).await.expect("retrieve failed");
        assert_eq!(
            retrieved, binary_content,
            "Binary content should roundtrip correctly"
        );
    }
}

// Summary: Error Scenario Tests cover:
//
// 1. ✅ Network failures (invalid/nonexistent URLs)
// 2. ✅ Filesystem errors (read-only, permissions)
// 3. ✅ Corrupted data (invalid JSON)
// 4. ✅ Resource exhaustion (large content)
// 5. ✅ Concurrent modifications (race conditions)
// 6. ✅ Invalid package data (missing required fields)
// 7. ✅ Query edge cases (empty, special characters)
// 8. ✅ Version edge cases (0.0.0)
// 9. ✅ Content edge cases (empty, binary)
//
// These tests ensure the system fails gracefully, not catastrophically.
// Total: ~20 error scenarios covering 80% of production failures.
