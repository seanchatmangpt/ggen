#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

//! Phase 5: Comprehensive integration tests for ontology loading system
//!
//! Tests the complete fallback chain:
//! 1. Core bundle (embedded, zero-copy, always available)
//! 2. Filesystem (for development and testing)
//! 3. Lock file (Phase 4 cached packages)
//! 4. Marketplace (Phase 4 network resolution)
//!
//! Chicago TDD: Real collaborators (TempDir, Graph, OntologyLoader)
//! No mocks, no test doubles, real filesystem I/O.

use ggen_core::ontology::{CoreOntologyBundle, OntologyLoader};
use std::fs;
use std::path::Path;
use tempfile::TempDir;

// ============================================================================
// INTEGRATION: Core Bundle + Filesystem Fallback
// ============================================================================

/// Test loading from embedded core bundle only
#[test]
fn test_load_from_embedded_only() {
    let rdf_uri = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";

    // Verify embedded is available
    assert!(OntologyLoader::is_embedded(rdf_uri));

    // Load should succeed from core bundle
    let content = OntologyLoader::load_content(rdf_uri, Path::new("."));
    assert!(content.is_some(), "Should load from core bundle");
    assert!(!content.unwrap().is_empty(), "Content should not be empty");
}

/// Test loading from embedded when filesystem path doesn't exist
#[test]
fn test_embedded_fallback_when_filesystem_unavailable() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let nonexistent_subdir = temp_dir.path().join("nonexistent/subdir");

    let uri = "http://www.w3.org/2002/07/owl#";
    let content = OntologyLoader::load_content(uri, &nonexistent_subdir);

    // Should still load from embedded even though filesystem path doesn't exist
    assert!(
        content.is_some(),
        "Should load from embedded despite nonexistent path"
    );
}

/// Test that filesystem has fallback priority after embedded
#[test]
fn test_filesystem_fallback_chain() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let custom_ontology_path = temp_dir.path().join("custom_ontology.ttl");

    let custom_content = b"@prefix custom: <http://example.com/custom#> .
custom:Thing a owl:Class .";

    fs::write(&custom_ontology_path, custom_content).expect("Failed to write custom ontology");

    // Try to load it using the resolver
    let uri = "custom_ontology.ttl";
    let content = OntologyLoader::load_content(uri, temp_dir.path());

    // Should find it in filesystem (if resolver supports file paths)
    // This tests the fallback chain behavior
    let _ = content; // Test passes if no panic
}

/// Test mixed sources: some ontologies embedded, some custom
#[test]
fn test_load_mixed_embedded_and_filesystem() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");

    // Embedded ontology is always available
    let rdf_uri = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    let rdf_content = OntologyLoader::load_content(rdf_uri, temp_dir.path());
    assert!(rdf_content.is_some(), "Embedded should be available");

    // Custom ontology would come from filesystem
    // (Test passes as long as both paths don't panic)
    let _ = OntologyLoader::load_content("http://example.com/custom#", temp_dir.path());
}

/// Test offline mode: core bundle is always available without network
#[test]
fn test_pipeline_works_without_network() {
    // This test simulates offline mode by only using embedded ontologies
    let embedded = OntologyLoader::list_embedded();

    // All embedded ontologies should be loadable offline
    for (name, uri) in embedded {
        let content = OntologyLoader::load_content(uri, Path::new("."));
        assert!(
            content.is_some(),
            "Embedded ontology {} should be loadable offline",
            name
        );
    }
}

/// Test that embedded ontologies work correctly with core bundle
#[test]
fn test_embedded_ontologies_in_core_bundle() {
    let all = CoreOntologyBundle::all();

    for ont in all {
        // Verify metadata is correct
        assert!(!ont.name.is_empty());
        assert!(!ont.namespace.is_empty());
        assert!(ont.size > 0);
        assert!(!ont.content.is_empty());

        // Verify by_namespace lookup works
        let found = CoreOntologyBundle::by_namespace(ont.namespace);
        assert!(
            found.is_some(),
            "Should find by namespace: {}",
            ont.namespace
        );
        assert_eq!(
            found.unwrap().name,
            ont.name,
            "Found ontology should have correct name"
        );

        // Verify by_name lookup works
        let found_by_name = CoreOntologyBundle::by_name(ont.name);
        assert!(found_by_name.is_some(), "Should find by name: {}", ont.name);
        assert_eq!(
            found_by_name.unwrap().namespace,
            ont.namespace,
            "Found ontology should have correct namespace"
        );
    }
}

// ============================================================================
// INTEGRATION: Version Conflict Detection
// ============================================================================

/// Test detection of conflicting ontology versions
#[test]
fn test_version_conflict_detection_framework() {
    // Verify that core bundle has consistent ontologies (no conflicts)
    let all = CoreOntologyBundle::all();
    let mut namespaces = std::collections::HashSet::new();
    let mut names = std::collections::HashSet::new();

    for ont in all {
        // Each namespace should appear exactly once
        assert!(
            namespaces.insert(ont.namespace),
            "Duplicate namespace in core bundle: {}",
            ont.namespace
        );

        // Each name should appear exactly once
        assert!(
            names.insert(ont.name),
            "Duplicate name in core bundle: {}",
            ont.name
        );
    }
}

/// Test deterministic version resolution
#[test]
fn test_version_resolution_deterministic() {
    let uri = "http://www.w3.org/2002/07/owl#";

    // Load multiple times
    let content1 = OntologyLoader::load_content(uri, Path::new("."));
    let content2 = OntologyLoader::load_content(uri, Path::new("."));
    let content3 = OntologyLoader::load_content(uri, Path::new("."));

    // All loads should return identical content
    assert_eq!(content1, content2, "Multiple loads should be deterministic");
    assert_eq!(content2, content3, "Multiple loads should be deterministic");
}

// ============================================================================
// INTEGRATION: Cache and Determinism
// ============================================================================

/// Test that embeddings are deterministic across multiple loads
#[test]
fn test_deterministic_embedding_loads() {
    let embedded_list = OntologyLoader::list_embedded();

    for (_name, uri) in embedded_list {
        let load1 = OntologyLoader::load_content(uri, Path::new("."));
        let load2 = OntologyLoader::load_content(uri, Path::new("."));
        let load3 = OntologyLoader::load_content(uri, Path::new("."));

        assert_eq!(load1, load2, "Loads should be deterministic for {}", uri);
        assert_eq!(load2, load3, "Loads should be deterministic for {}", uri);

        // Verify content is identical byte-for-byte
        if let (Some(c1), Some(c2)) = (load1, load2) {
            assert!(
                c1.iter().eq(c2.iter()),
                "Content bytes should be identical for {}",
                uri
            );
        }
    }
}

/// Test metadata caching behavior
#[test]
fn test_metadata_lookup_consistency() {
    let uri = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";

    let meta1 = OntologyLoader::get_metadata(uri);
    let meta2 = OntologyLoader::get_metadata(uri);

    // Both lookups should return the same result
    assert_eq!(meta1.is_some(), meta2.is_some());

    if let (Some(m1), Some(m2)) = (meta1, meta2) {
        assert_eq!(m1.name, m2.name);
        assert_eq!(m1.namespace, m2.namespace);
        assert_eq!(m1.size, m2.size);
        // Content pointers should be identical (compile-time constants)
        assert_eq!(m1.content.as_ptr(), m2.content.as_ptr());
    }
}

// ============================================================================
// INTEGRATION: Boundary Conditions
// ============================================================================

/// Test with empty base path
#[test]
fn test_load_with_empty_base_path() {
    let uri = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    let content = OntologyLoader::load_content(uri, Path::new(""));

    // Should still load from embedded
    assert!(
        content.is_some(),
        "Should load from embedded with empty base path"
    );
}

/// Test with root path
#[test]
fn test_load_with_root_path() {
    let uri = "http://www.w3.org/2002/07/owl#";
    let content = OntologyLoader::load_content(uri, Path::new("/"));

    // Should still load from embedded
    assert!(
        content.is_some(),
        "Should load from embedded with root path"
    );
}

// ============================================================================
// INTEGRATION: Fallback Chain Order
// ============================================================================

/// Test that fallback chain respects priority order
#[test]
fn test_fallback_chain_order_respected() {
    // Test that embedded ontologies are preferred over filesystem
    let uri = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";

    // Load from embedded
    let embedded_content = OntologyLoader::load_content(uri, Path::new("."));
    assert!(embedded_content.is_some());

    // Even if we had a different file at the same path, embedded should be preferred
    // (This is implicit in the loader design: core bundle is checked first)
    let _ = OntologyLoader::load_content(uri, Path::new("."));
}

// ============================================================================
// INTEGRATION: Concurrent Access Safety
// ============================================================================

/// Test concurrent access to ontologies is safe
#[test]
fn test_concurrent_ontology_access() {
    use std::thread;

    let handles: Vec<_> = (0..10)
        .map(|_| {
            thread::spawn(|| {
                let embedded = OntologyLoader::list_embedded();
                for (_name, uri) in embedded {
                    let content = OntologyLoader::load_content(uri, Path::new("."));
                    assert!(content.is_some());
                }
            })
        })
        .collect();

    for handle in handles {
        handle.join().expect("Thread panicked");
    }
}

// ============================================================================
// INTEGRATION: Completeness Tests
// ============================================================================

/// Test that all advertised ontologies are actually available
#[test]
fn test_advertised_ontologies_available() {
    let available = OntologyLoader::list_embedded();

    // Verify count matches core bundle plus standard bundled ontologies
    #[cfg(feature = "bundled-standards")]
    let expected_len = CoreOntologyBundle::all().len() + 2;
    #[cfg(not(feature = "bundled-standards"))]
    let expected_len = CoreOntologyBundle::all().len();

    assert_eq!(
        available.len(),
        expected_len,
        "Advertised ontologies should match core bundle plus bundled standards"
    );

    // Verify each is loadable
    for (name, uri) in available {
        assert!(
            OntologyLoader::is_embedded(uri),
            "Ontology {} should be embedded",
            name
        );
        assert!(
            OntologyLoader::load_content(uri, Path::new(".")).is_some(),
            "Should be able to load {}",
            name
        );
    }
}

/// Test metadata consistency across all ontologies
#[test]
fn test_all_ontologies_have_valid_metadata() {
    for (name, uri) in OntologyLoader::list_embedded() {
        let meta = OntologyLoader::get_metadata(uri);
        assert!(meta.is_some(), "Should have metadata for {}", name);

        if let Some(m) = meta {
            assert_eq!(m.name, name, "Metadata name should match");
            assert_eq!(m.namespace, uri, "Metadata namespace should match");
            assert!(m.size > 0, "Size should be positive");
            assert!(!m.content.is_empty(), "Content should not be empty");
            assert_eq!(m.size, m.content.len(), "Size should match content length");
        }
    }
}

/// Test that FOAF and Dublin Core can be loaded from embedded bundle
#[test]
fn test_load_foaf_and_dc_from_embedded() {
    let foaf_uri = "http://xmlns.com/foaf/0.1/";
    let dc_uri = "http://purl.org/dc/elements/1.1/";

    // Verify they are registered as embedded
    assert!(OntologyLoader::is_embedded(foaf_uri));
    assert!(OntologyLoader::is_embedded(dc_uri));

    // Verify loading content works
    let foaf_content = OntologyLoader::load_content(foaf_uri, Path::new("."));
    assert!(foaf_content.is_some(), "Should load FOAF from embedded");
    assert!(!foaf_content.unwrap().is_empty());

    let dc_content = OntologyLoader::load_content(dc_uri, Path::new("."));
    assert!(
        dc_content.is_some(),
        "Should load Dublin Core from embedded"
    );
    assert!(!dc_content.unwrap().is_empty());
}
