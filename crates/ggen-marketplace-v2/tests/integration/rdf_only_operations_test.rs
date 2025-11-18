//! RDF-Only Operations Integration Tests
//!
//! Tests operations that work exclusively with RDF/Turtle data,
//! without JSON or other formats.

use ggen_marketplace_v2::ontology::*;
use ggen_marketplace_v2::prelude::*;
use oxigraph::store::Store;

// ============================================================================
// RDF-Only CRUD Operations
// ============================================================================

#[tokio::test]
async fn test_rdf_create_package() {
    let registry = RdfRegistry::new();

    let package = create_test_package("rdf-pkg", "1.0.0");
    let result = registry.insert_package_rdf(&package).await;

    assert!(result.is_ok());
}

#[tokio::test]
async fn test_rdf_read_package() {
    let registry = RdfRegistry::new();

    let package = create_test_package("readable", "1.0.0");
    registry.insert_package_rdf(&package).await.unwrap();

    let pkg_id = PackageId::new("readable").unwrap();
    let retrieved = registry.get_package(&pkg_id).await.unwrap();

    assert_eq!(retrieved.metadata.id.as_str(), "readable");
}

#[tokio::test]
async fn test_rdf_update_package() {
    let registry = RdfRegistry::new();

    let mut package = create_test_package("updatable", "1.0.0");
    registry.insert_package_rdf(&package).await.unwrap();

    // Update
    package.quality_score = QualityScore::new(95).unwrap();
    registry.insert_package_rdf(&package).await.unwrap();

    let pkg_id = PackageId::new("updatable").unwrap();
    let retrieved = registry.get_package(&pkg_id).await.unwrap();

    assert_eq!(retrieved.quality_score.value(), 95);
}

#[tokio::test]
async fn test_rdf_query_all_packages() {
    let registry = RdfRegistry::new();

    let pkg1 = create_test_package("pkg1", "1.0.0");
    let pkg2 = create_test_package("pkg2", "1.0.0");

    registry.insert_package_rdf(&pkg1).await.unwrap();
    registry.insert_package_rdf(&pkg2).await.unwrap();

    let all = registry.all_packages().await.unwrap();
    assert!(all.len() >= 2);
}

#[tokio::test]
async fn test_rdf_sparql_query() {
    let registry = RdfRegistry::new();

    let package = create_test_package("sparql-test", "1.0.0");
    registry.insert_package_rdf(&package).await.unwrap();

    let query = format!(
        r#"
        PREFIX ggen: <{}>
        SELECT ?pkg WHERE {{
            ?pkg ggen:packageName "sparql-test" .
        }}
    "#,
        GGEN_NAMESPACE
    );

    let results = registry.query_sparql(&query).await.unwrap();
    assert!(!results.is_empty());
}

// ============================================================================
// RDF Namespace and Ontology Tests
// ============================================================================

#[tokio::test]
async fn test_rdf_namespace_resolution() {
    let package_class = format!("{}Package", GGEN_NAMESPACE);
    assert_eq!(package_class, "http://ggen.dev/ontology#Package");
}

#[tokio::test]
async fn test_rdf_ontology_properties() {
    let name_prop = GgenOntology::template_name();
    let version_prop = GgenOntology::template_version();

    assert!(name_prop.contains("templateName"));
    assert!(version_prop.contains("templateVersion"));
}

// ============================================================================
// RDF Batch Operations
// ============================================================================

#[tokio::test]
async fn test_rdf_batch_insert() {
    let registry = RdfRegistry::new();

    let packages = vec![
        create_test_package("batch1", "1.0.0"),
        create_test_package("batch2", "1.0.0"),
        create_test_package("batch3", "1.0.0"),
    ];

    let count = registry.batch_insert_packages(packages).await.unwrap();
    assert_eq!(count, 3);
}

// ============================================================================
// Helper Functions
// ============================================================================

fn create_test_package(name: &str, version: &str) -> Package {
    let manifest = Manifest {
        name: name.to_string(),
        version: PackageVersion::new(version).unwrap(),
        description: Some(format!("Test package {}", name)),
        authors: vec!["test@example.com".to_string()],
        dependencies: indexmap::IndexMap::new(),
        license: Some("MIT".to_string()),
    };

    Package::from_manifest(manifest).unwrap()
}
