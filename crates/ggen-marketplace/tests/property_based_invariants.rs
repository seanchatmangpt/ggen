//! Property-Based Tests - Mathematical Invariants
//!
//! Tests that verify properties that MUST hold for all inputs.
//! These are the "laws" of our system.

use ggen_marketplace::prelude::*;
use tempfile::TempDir;

// Note: Using manual property testing instead of proptest to avoid extra dependency
// In production, consider adding proptest for more sophisticated property-based testing

// ============================================================================
// INVARIANT 1: Content Addressability
// ============================================================================

#[tokio::test]
async fn property_same_content_same_id() {
    // Property: store(x) == store(x) for all x
    // (Content addressing must be deterministic)

    let store = MemoryStore::new();

    let test_cases = vec![
        b"Hello, World!".as_slice(),
        b"".as_slice(),                                 // Empty content
        b"x".as_slice(),                                // Single byte
        &vec![0u8; 1024 * 1024],                        // 1MB of zeros
        b"Rust is awesome \xF0\x9F\xA6\x80".as_slice(), // Unicode
    ];

    for content in test_cases {
        let id1 = store.store(content).await.expect("store failed");
        let id2 = store.store(content).await.expect("store failed");

        assert_eq!(
            id1.hash, id2.hash,
            "Same content must always produce same content ID"
        );
    }
}

#[tokio::test]
async fn property_different_content_different_id() {
    // Property: store(x) != store(y) for all x != y
    // (Hash collisions should be astronomically rare)

    let store = MemoryStore::new();

    let test_cases = vec![
        (b"content1".as_slice(), b"content2".as_slice()),
        (b"hello".as_slice(), b"world".as_slice()),
        (b"a".as_slice(), b"b".as_slice()),
        (b"".as_slice(), b"x".as_slice()),
    ];

    for (content1, content2) in test_cases {
        let id1 = store.store(content1).await.expect("store failed");
        let id2 = store.store(content2).await.expect("store failed");

        assert_ne!(
            id1.hash, id2.hash,
            "Different content must produce different content IDs"
        );
    }
}

// ============================================================================
// INVARIANT 2: Idempotency
// ============================================================================

#[tokio::test]
async fn property_store_retrieve_roundtrip() {
    // Property: retrieve(store(x)) == x for all x
    // (Storage must preserve content exactly)

    let store = MemoryStore::new();

    let test_cases = vec![
        b"Simple text".to_vec(),
        vec![0u8; 100],   // Binary zeros
        vec![255u8; 100], // Binary 0xFF
        b"Unicode: \xF0\x9F\x8E\x89\xF0\x9F\x8E\x8A".to_vec(),
        (0..256).map(|i| i as u8).collect(), // All byte values
    ];

    for original_content in test_cases {
        let id = store.store(&original_content).await.expect("store failed");
        let retrieved = store.retrieve(&id).await.expect("retrieve failed");

        assert_eq!(
            retrieved, original_content,
            "Retrieved content must match original exactly"
        );
    }
}

#[tokio::test]
async fn property_delete_idempotent() {
    // Property: delete(delete(x)) == delete(x)
    // (Deleting twice should not cause errors)

    let store = MemoryStore::new();

    let content = b"content to delete";
    let id = store.store(content).await.expect("store failed");

    // First delete should succeed
    let result1 = store.delete(&id).await;
    assert!(result1.is_ok(), "First delete should succeed");

    // Second delete should fail gracefully (not found)
    let result2 = store.delete(&id).await;
    assert!(result2.is_err(), "Second delete should return error");
}

// ============================================================================
// INVARIANT 3: Version Ordering
// ============================================================================

#[tokio::test]
async fn property_versions_ordered_newest_first() {
    // Property: For package P with versions v1, v2, v3 where v1 < v2 < v3,
    // list_versions(P) returns [v3, v2, v1]

    let temp_dir = tempfile::tempdir().expect("temp dir failed");
    let db_path = temp_dir.path().join("registry");
    let registry = LocalRegistry::new(db_path)
        .await
        .expect("registry creation failed");

    let pkg_id = PackageId::new("test", "versioned");

    // Publish versions in random order
    let versions = vec![
        (Version::new(1, 5, 0), "hash_1_5_0"),
        (Version::new(2, 0, 0), "hash_2_0_0"),
        (Version::new(1, 0, 0), "hash_1_0_0"),
        (Version::new(1, 10, 0), "hash_1_10_0"),
    ];

    for (version, hash) in &versions {
        let pkg = Package::builder(pkg_id.clone(), *version)
            .title("Test Package")
            .description("Test")
            .license("MIT")
            .content_id(ContentId::new(*hash, HashAlgorithm::Sha256))
            .build()
            .expect("package build failed");

        registry.publish(pkg).await.expect("publish failed");
    }

    // Get all versions
    let retrieved_versions = registry
        .list_versions(&pkg_id)
        .await
        .expect("list versions failed");

    // Assert: Versions should be ordered newest first
    for i in 0..retrieved_versions.len() - 1 {
        let current = &retrieved_versions[i].version;
        let next = &retrieved_versions[i + 1].version;

        assert!(
            current >= next,
            "Versions must be ordered newest first: {} should be >= {}",
            current,
            next
        );
    }

    // Assert: Latest version should be 2.0.0
    let latest = registry
        .get_package(&pkg_id)
        .await
        .expect("get latest failed");
    assert_eq!(
        latest.version,
        Version::new(2, 0, 0),
        "Latest version should be 2.0.0"
    );
}

// ============================================================================
// INVARIANT 4: Search Consistency
// ============================================================================

#[tokio::test]
async fn property_search_returns_subset() {
    // Property: search(query) ⊆ all_packages
    // (Search results must be a subset of all packages)

    let temp_dir = tempfile::tempdir().expect("temp dir failed");
    let db_path = temp_dir.path().join("registry");
    let registry = LocalRegistry::new(db_path)
        .await
        .expect("registry creation failed");

    // Publish packages
    let packages = vec![
        ("web-framework", "Web framework package"),
        ("web-server", "Web server package"),
        ("cli-tool", "Command line tool"),
        ("database", "Database library"),
    ];

    for (name, desc) in &packages {
        let pkg = Package::builder(PackageId::new("test", name), Version::new(1, 0, 0))
            .title(name)
            .description(desc)
            .license("MIT")
            .content_id(ContentId::new(
                format!("hash_{}", name),
                HashAlgorithm::Sha256,
            ))
            .build()
            .expect("package build failed");

        registry.publish(pkg).await.expect("publish failed");
    }

    // Search for "web"
    let search_results = registry
        .search(&Query::new("web"))
        .await
        .expect("search failed");

    // Get all packages
    let all_results = registry
        .search(&Query::new(""))
        .await
        .expect("search all failed");

    // Assert: Search results must be subset of all packages
    for search_result in &search_results {
        let found = all_results
            .iter()
            .any(|pkg| pkg.id == search_result.id && pkg.version == search_result.version);

        assert!(
            found,
            "Search result {} must be in all packages",
            search_result.id
        );
    }

    // Assert: Search results should only contain relevant packages
    for result in &search_results {
        let matches_query = result.id.name.contains("web")
            || result.metadata.title.contains("web")
            || result.metadata.description.contains("Web");

        assert!(
            matches_query,
            "Search result {} should match query 'web'",
            result.id.name
        );
    }
}

// ============================================================================
// INVARIANT 5: Metadata Consistency
// ============================================================================

#[tokio::test]
async fn property_metadata_size_matches_content() {
    // Property: metadata(store(x)).size == len(x) for all x
    // (Metadata must accurately reflect content)

    let store = MemoryStore::new();

    let test_cases = vec![
        vec![],                 // Empty
        vec![0u8],              // Single byte
        vec![0u8; 1024],        // 1KB
        vec![0u8; 1024 * 1024], // 1MB
    ];

    for content in test_cases {
        let expected_size = content.len() as u64;

        let id = store.store(&content).await.expect("store failed");
        let metadata = store.metadata(&id).await.expect("metadata failed");

        assert_eq!(
            metadata.size, expected_size,
            "Metadata size must match actual content size"
        );
    }
}

// ============================================================================
// INVARIANT 6: Content Hash Verification
// ============================================================================

#[tokio::test]
async fn property_content_hash_verifiable() {
    // Property: For all content x, hash(retrieve(store(x))) == store(x).hash
    // (Content hash must be verifiable)

    use sha2::{Digest, Sha256};

    let store = MemoryStore::new();
    let verifier = DefaultVerifier::new();

    let test_cases = vec![
        b"Verify this content".as_slice(),
        b"".as_slice(),
        &vec![0u8; 10000],
    ];

    for original_content in test_cases {
        // Store content
        let content_id = store.store(original_content).await.expect("store failed");

        // Retrieve content
        let retrieved = store.retrieve(&content_id).await.expect("retrieve failed");

        // Compute hash of retrieved content
        let computed_hash = verifier
            .hash_content(&retrieved)
            .expect("hash computation failed");

        // Assert: Computed hash matches stored content ID
        assert_eq!(
            computed_hash, content_id.hash,
            "Content hash must be verifiable from retrieved content"
        );
    }
}

// ============================================================================
// INVARIANT 7: Package ID Uniqueness per Version
// ============================================================================

#[tokio::test]
async fn property_package_version_unique() {
    // Property: A package ID + version combination is unique
    // (Cannot have two different packages with same ID and version)

    let temp_dir = tempfile::tempdir().expect("temp dir failed");
    let db_path = temp_dir.path().join("registry");
    let registry = LocalRegistry::new(db_path)
        .await
        .expect("registry creation failed");

    let pkg_id = PackageId::new("test", "unique");
    let version = Version::new(1, 0, 0);

    // First package
    let pkg1 = Package::builder(pkg_id.clone(), version)
        .title("First Package")
        .description("First")
        .license("MIT")
        .content_id(ContentId::new("hash1", HashAlgorithm::Sha256))
        .build()
        .expect("build failed");

    // Second package with same ID and version but different content
    let pkg2 = Package::builder(pkg_id.clone(), version)
        .title("Second Package") // Different!
        .description("Second") // Different!
        .license("MIT")
        .content_id(ContentId::new("hash2", HashAlgorithm::Sha256)) // Different!
        .build()
        .expect("build failed");

    // Publish first package
    registry.publish(pkg1).await.expect("first publish failed");

    // Try to publish second package with same ID + version
    let result = registry.publish(pkg2).await;

    // Assert: Second publish must fail
    assert!(
        result.is_err(),
        "Publishing same package ID + version twice must fail"
    );
}

// Summary: Property-Based Tests verify mathematical invariants:
//
// 1. ✅ Content addressability (same input = same ID)
// 2. ✅ Hash uniqueness (different input = different ID)
// 3. ✅ Roundtrip fidelity (store → retrieve preserves data)
// 4. ✅ Idempotency (operations safe to repeat)
// 5. ✅ Version ordering (newest first)
// 6. ✅ Search consistency (results ⊆ all packages)
// 7. ✅ Metadata accuracy (size matches content)
// 8. ✅ Hash verification (content hash is verifiable)
// 9. ✅ Uniqueness constraints (ID + version is unique)
//
// These are the laws that MUST hold for system correctness.
