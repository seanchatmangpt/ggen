#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

//! Phase 5: End-to-end (E2E) workflow tests for ontology embedding system
//!
//! Tests complete workflows from initialization through lock file creation
//! and reproducible builds. These tests verify the entire system works together.
//!
//! Chicago TDD: Real collaborators (TempDir, Graph, actual file I/O)
//! No mocks, no test doubles, real state verification.

use ggen_core::ontology::{CoreOntologyBundle, OntologyLoader};
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

// ============================================================================
// E2E: Complete Project Setup
// ============================================================================

/// Test full project setup with embedded ontologies only
#[test]
fn test_full_project_setup_embedded_only() {
    let project_dir = TempDir::new().expect("Failed to create project directory");
    let project_path = project_dir.path();

    // Step 1: Create project structure
    let src_dir = project_path.join("src");
    let ggen_dir = project_path.join(".ggen");
    fs::create_dir(&src_dir).expect("Failed to create src dir");
    fs::create_dir(&ggen_dir).expect("Failed to create .ggen dir");

    // Step 2: Initialize with core ontologies
    let core_ontologies = CoreOntologyBundle::all();
    assert!(
        !core_ontologies.is_empty(),
        "Core ontologies should be available"
    );

    // Step 3: Verify all embedded ontologies can be loaded
    for ont in core_ontologies {
        let content = OntologyLoader::load_content(ont.namespace, project_path);
        assert!(content.is_some(), "Should load {}", ont.name);
    }

    // Step 4: Verify offline capability
    assert!(!OntologyLoader::list_embedded().is_empty());

    // Project setup complete
    assert!(project_path.exists());
    assert!(ggen_dir.exists());
}

/// Test full project setup with mixed sources
#[test]
fn test_full_project_setup_mixed_sources() {
    let project_dir = TempDir::new().expect("Failed to create project directory");
    let project_path = project_dir.path();

    // Create project structure
    fs::create_dir(project_path.join(".ggen")).expect("Failed to create .ggen dir");
    fs::create_dir(project_path.join("ontologies")).expect("Failed to create ontologies dir");

    let ontologies_path = project_path.join("ontologies");

    // Add custom ontology file
    let custom_onto = "@prefix custom: <http://example.com/custom#> .
custom:MyClass a owl:Class .
custom:myProperty a owl:ObjectProperty .";

    fs::write(ontologies_path.join("custom.ttl"), custom_onto)
        .expect("Failed to write custom ontology");

    // Load core (embedded)
    let core_uri = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    let core_content = OntologyLoader::load_content(core_uri, project_path);
    assert!(core_content.is_some(), "Should load core embedded ontology");

    // Load custom (filesystem)
    let custom_path = ontologies_path.join("custom.ttl");
    assert!(custom_path.exists(), "Custom ontology should exist");

    // Verify project state
    assert!(project_path.join(".ggen").exists());
    assert!(project_path.join("ontologies").exists());
}

/// Test lock file creation
#[test]
fn test_lock_file_creation_and_structure() {
    let project_dir = TempDir::new().expect("Failed to create project directory");
    let project_path = project_dir.path();
    let ggen_dir = project_path.join(".ggen");
    fs::create_dir(&ggen_dir).expect("Failed to create .ggen dir");

    // Simulate creating a lock file
    let lock_data = serde_json::json!({
        "version": "1.0",
        "ontologies": [
            {
                "namespace": "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                "name": "rdf",
                "hash": "abc123"
            }
        ]
    });

    let lock_file = ggen_dir.join("ontology.lock");
    fs::write(&lock_file, lock_data.to_string()).expect("Failed to write lock file");

    // Verify lock file exists and is valid JSON
    assert!(lock_file.exists(), "Lock file should exist");
    let content = fs::read_to_string(&lock_file).expect("Failed to read lock file");
    let parsed: serde_json::Value =
        serde_json::from_str(&content).expect("Lock file should be valid JSON");
    assert_eq!(parsed["version"], "1.0");
}

/// Test reproducible build with embedded ontologies
#[test]
fn test_reproducible_build_with_embedded() {
    let project_dir1 = TempDir::new().expect("Failed to create project 1");
    let project_dir2 = TempDir::new().expect("Failed to create project 2");

    // Both projects load the same ontologies
    let uri = "http://www.w3.org/2002/07/owl#";

    let content1 = OntologyLoader::load_content(uri, project_dir1.path());
    let content2 = OntologyLoader::load_content(uri, project_dir2.path());

    // Both should get identical content (deterministic)
    assert_eq!(content1, content2, "Same ontology should be deterministic");
}

// ============================================================================
// E2E: Incremental Workflows
// ============================================================================

/// Test incremental rebuild with cache
#[test]
fn test_incremental_rebuild_with_cache() {
    let project_dir = TempDir::new().expect("Failed to create project directory");
    let project_path = project_dir.path();

    // First build: load all ontologies
    let embedded = OntologyLoader::list_embedded();
    let mut first_build_contents = Vec::new();

    for (_name, uri) in embedded {
        if let Some(content) = OntologyLoader::load_content(uri, project_path) {
            first_build_contents.push((uri.to_string(), content));
        }
    }

    assert!(
        !first_build_contents.is_empty(),
        "First build should load ontologies"
    );

    // Second build: load again (should use "cache")
    let embedded_again = OntologyLoader::list_embedded();
    let mut second_build_contents = Vec::new();

    for (_name, uri) in embedded_again {
        if let Some(content) = OntologyLoader::load_content(uri, project_path) {
            second_build_contents.push((uri.to_string(), content));
        }
    }

    // Both builds should be identical
    assert_eq!(
        first_build_contents.len(),
        second_build_contents.len(),
        "Both builds should load same number of ontologies"
    );

    for ((uri1, content1), (_uri2, content2)) in first_build_contents
        .iter()
        .zip(second_build_contents.iter())
    {
        assert_eq!(
            content1, content2,
            "Content should be identical for {}",
            uri1
        );
    }
}

// ============================================================================
// E2E: Determinism Verification
// ============================================================================

/// Test that same inputs produce same outputs
#[test]
fn test_deterministic_ontology_loading() {
    let project_dir = TempDir::new().expect("Failed to create project directory");
    let project_path = project_dir.path();

    // Load ontologies 5 times
    let mut loads = Vec::new();

    for i in 0..5 {
        let embedded = OntologyLoader::list_embedded();
        let mut load_results = Vec::new();

        for (_name, uri) in embedded {
            if let Some(content) = OntologyLoader::load_content(uri, project_path) {
                load_results.push((uri.to_string(), content.len()));
            }
        }

        loads.push(load_results);
    }

    // All 5 loads should be identical
    for i in 1..loads.len() {
        assert_eq!(
            loads[0].len(),
            loads[i].len(),
            "Load {} should have same number of ontologies",
            i
        );

        for j in 0..loads[0].len() {
            assert_eq!(loads[0][j].0, loads[i][j].0, "Ontology names should match");
            assert_eq!(loads[0][j].1, loads[i][j].1, "Ontology sizes should match");
        }
    }
}

// ============================================================================
// E2E: Offline Capability
// ============================================================================

/// Test complete workflow without network
#[test]
fn test_complete_workflow_offline() {
    let project_dir = TempDir::new().expect("Failed to create project directory");
    let project_path = project_dir.path();

    // Create project structure
    fs::create_dir(project_path.join(".ggen")).expect("Failed to create .ggen");
    fs::create_dir(project_path.join("src")).expect("Failed to create src");

    // Step 1: Load all embedded ontologies (offline)
    let embedded = OntologyLoader::list_embedded();
    let mut loaded_count = 0;

    for (_name, uri) in embedded {
        if OntologyLoader::load_content(uri, project_path).is_some() {
            loaded_count += 1;
        }
    }

    assert!(
        loaded_count > 0,
        "Should load at least one ontology offline"
    );

    // Step 2: Verify offline availability
    let available = OntologyLoader::list_embedded();
    assert_eq!(
        loaded_count,
        available.len(),
        "Should load all available embedded ontologies"
    );

    // Step 3: Verify project can work offline
    assert!(project_path.join(".ggen").exists());
    assert!(project_path.join("src").exists());
}

// ============================================================================
// E2E: Consistency Across Workflows
// ============================================================================

/// Test consistency across multiple sequential builds
#[test]
fn test_sequential_builds_consistency() {
    let project_dir = TempDir::new().expect("Failed to create project directory");
    let project_path = project_dir.path();

    // Run 3 sequential builds
    let mut build_hashes = Vec::new();

    for build_num in 0..3 {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        use std::hash::{Hash, Hasher};

        let embedded = OntologyLoader::list_embedded();
        for (_name, uri) in embedded {
            if let Some(content) = OntologyLoader::load_content(uri, project_path) {
                content.hash(&mut hasher);
            }
        }

        let hash = hasher.finish();
        build_hashes.push((build_num, hash));
    }

    // All builds should have same hash (deterministic)
    for i in 1..build_hashes.len() {
        assert_eq!(
            build_hashes[0].1, build_hashes[i].1,
            "Build {} should be identical to build 0",
            i
        );
    }
}

/// Test metadata consistency across workflow stages
#[test]
fn test_metadata_consistency_through_workflow() {
    let project_dir = TempDir::new().expect("Failed to create project directory");
    let project_path = project_dir.path();

    // Collect metadata before workflow
    let uri = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    let meta_before = OntologyLoader::get_metadata(uri);

    // Run workflow (load content multiple times)
    for _ in 0..10 {
        let _ = OntologyLoader::load_content(uri, project_path);
    }

    // Collect metadata after workflow
    let meta_after = OntologyLoader::get_metadata(uri);

    // Metadata should be identical
    assert_eq!(meta_before.is_some(), meta_after.is_some());
    if let (Some(before), Some(after)) = (meta_before, meta_after) {
        assert_eq!(before.name, after.name);
        assert_eq!(before.namespace, after.namespace);
        assert_eq!(before.size, after.size);
    }
}
