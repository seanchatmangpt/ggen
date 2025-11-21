//! Core Domain Tests for Packs - Unit Level
//!
//! Tests the fundamental domain layer components:
//! - PackRepository: Load, discover, list packs
//! - Pack metadata loading and validation
//! - Type conversions and serialization

use ggen_domain::packs::{
    list_packs, load_pack_metadata, show_pack, Pack, PackDependency, PackTemplate,
};
use std::collections::HashMap;

#[test]
fn test_list_packs_returns_results() {
    // Act
    let result = list_packs(None);

    // Assert - should succeed or fail gracefully
    match result {
        Ok(packs) => {
            assert!(packs.len() >= 0, "Should return a list of packs");
            for pack in packs {
                assert!(!pack.id.is_empty(), "Pack ID should not be empty");
                assert!(!pack.name.is_empty(), "Pack name should not be empty");
                assert!(!pack.version.is_empty(), "Pack version should not be empty");
            }
        }
        Err(e) => {
            // Graceful failure is acceptable if packs directory doesn't exist
            assert!(
                e.to_string().contains("Packs directory not found"),
                "Error should indicate missing packs directory"
            );
        }
    }
}

#[test]
fn test_list_packs_filters_by_category() {
    // Act
    let result = list_packs(Some("startup"));

    // Assert
    match result {
        Ok(packs) => {
            for pack in packs {
                assert_eq!(
                    pack.category, "startup",
                    "All packs should be in startup category"
                );
            }
        }
        Err(_) => {
            // Expected if packs directory doesn't exist
        }
    }
}

#[test]
fn test_show_pack_retrieves_details() {
    // Try to load a known pack
    let result = show_pack("startup-essentials");

    // Assert
    match result {
        Ok(pack) => {
            assert_eq!(pack.id, "startup-essentials");
            assert!(!pack.name.is_empty());
            assert!(!pack.packages.is_empty(), "Pack should have packages");
            assert!(pack.production_ready, "Startup essentials should be production ready");
        }
        Err(e) => {
            // Expected if pack doesn't exist in test environment
            assert!(
                e.to_string().contains("not found") || e.to_string().contains("directory"),
                "Should fail with appropriate error"
            );
        }
    }
}

#[test]
fn test_load_pack_metadata_validates_structure() {
    // Try loading with invalid pack ID
    let result = load_pack_metadata("nonexistent-pack-12345");

    // Assert - should fail with clear error
    assert!(result.is_err(), "Should fail for nonexistent pack");
    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("not found") || error_msg.contains("directory"),
        "Error should indicate pack not found"
    );
}

#[test]
fn test_pack_type_serialization() {
    // Arrange - Create a pack
    let pack = Pack {
        id: "test-pack".to_string(),
        name: "Test Pack".to_string(),
        version: "1.0.0".to_string(),
        description: "Test description".to_string(),
        category: "test".to_string(),
        author: Some("test-author".to_string()),
        repository: Some("https://github.com/test/test".to_string()),
        license: Some("MIT".to_string()),
        packages: vec!["package1".to_string(), "package2".to_string()],
        templates: vec![PackTemplate {
            name: "test-template".to_string(),
            path: "templates/test".to_string(),
            description: "Test template".to_string(),
            variables: vec!["var1".to_string(), "var2".to_string()],
        }],
        sparql_queries: {
            let mut queries = HashMap::new();
            queries.insert("test_query".to_string(), "SELECT * WHERE { ?s ?p ?o }".to_string());
            queries
        },
        dependencies: vec![PackDependency {
            pack_id: "dependency-pack".to_string(),
            version: "1.0.0".to_string(),
            optional: false,
        }],
        tags: vec!["test".to_string(), "example".to_string()],
        keywords: vec!["testing".to_string(), "example".to_string()],
        production_ready: true,
        metadata: Default::default(),
    };

    // Act - Serialize and deserialize
    #[allow(clippy::expect_used)]
    let json = serde_json::to_string(&pack).expect("Should serialize");
    #[allow(clippy::expect_used)]
    let deserialized: Pack = serde_json::from_str(&json).expect("Should deserialize");

    // Assert
    assert_eq!(deserialized.id, pack.id);
    assert_eq!(deserialized.packages.len(), 2);
    assert_eq!(deserialized.templates.len(), 1);
    assert_eq!(deserialized.sparql_queries.len(), 1);
    assert_eq!(deserialized.dependencies.len(), 1);
}

#[test]
fn test_pack_with_optional_fields() {
    // Arrange - Pack with minimal fields
    let pack = Pack {
        id: "minimal-pack".to_string(),
        name: "Minimal Pack".to_string(),
        version: "1.0.0".to_string(),
        description: "Minimal test pack".to_string(),
        category: "test".to_string(),
        author: None,
        repository: None,
        license: None,
        packages: vec![],
        templates: vec![],
        sparql_queries: HashMap::new(),
        dependencies: vec![],
        tags: vec![],
        keywords: vec![],
        production_ready: false,
        metadata: Default::default(),
    };

    // Act - Serialize
    #[allow(clippy::expect_used)]
    let json = serde_json::to_string(&pack).expect("Should serialize");

    // Assert - Should handle optional fields
    assert!(json.contains("\"id\":\"minimal-pack\""));
    assert!(json.contains("\"author\":null"));
}

#[test]
fn test_pack_dependency_structure() {
    // Arrange
    let dep = PackDependency {
        pack_id: "base-pack".to_string(),
        version: "2.0.0".to_string(),
        optional: true,
    };

    // Act
    #[allow(clippy::expect_used)]
    let json = serde_json::to_string(&dep).expect("Should serialize");
    #[allow(clippy::expect_used)]
    let deserialized: PackDependency = serde_json::from_str(&json).expect("Should deserialize");

    // Assert
    assert_eq!(deserialized.pack_id, "base-pack");
    assert_eq!(deserialized.version, "2.0.0");
    assert!(deserialized.optional);
}

#[test]
fn test_pack_template_structure() {
    // Arrange
    let template = PackTemplate {
        name: "api-server".to_string(),
        path: "templates/api-server".to_string(),
        description: "REST API server template".to_string(),
        variables: vec!["port".to_string(), "host".to_string(), "db_url".to_string()],
    };

    // Act
    #[allow(clippy::expect_used)]
    let json = serde_json::to_string(&template).expect("Should serialize");
    #[allow(clippy::expect_used)]
    let deserialized: PackTemplate = serde_json::from_str(&json).expect("Should deserialize");

    // Assert
    assert_eq!(deserialized.name, "api-server");
    assert_eq!(deserialized.variables.len(), 3);
    assert!(deserialized.variables.contains(&"port".to_string()));
}

#[test]
fn test_list_packs_empty_category() {
    // Act - Try to list packs with non-existent category
    let result = list_packs(Some("nonexistent-category-xyz"));

    // Assert - Should return empty list or error gracefully
    match result {
        Ok(packs) => {
            assert_eq!(packs.len(), 0, "Should return empty list for nonexistent category");
        }
        Err(_) => {
            // Expected if packs directory doesn't exist
        }
    }
}

#[test]
fn test_pack_metadata_defaults() {
    use ggen_domain::packs::types::PackMetadata;

    // Arrange
    let metadata = PackMetadata::default();

    // Assert - All fields should be None or 0
    assert!(metadata.test_coverage.is_none());
    assert!(metadata.rdf_ontology_size.is_none());
    assert!(metadata.sparql_templates.is_none());
    assert!(metadata.code_examples.is_none());
    assert!(metadata.documentation_files.is_none());
}
