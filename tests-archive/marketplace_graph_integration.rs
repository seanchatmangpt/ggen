//! Integration tests for marketplace lifecycle with new graph API
//!
//! Validates that marketplace package operations work correctly with
//! the new modular graph API structure.

use ggen_core::graph::{Graph, GraphExport, GraphQuery, GraphUpdate};
use ggen_core::lifecycle::{run_phase, Context};
use ggen_utils::error::Result;
use oxigraph::io::RdfFormat;
use std::path::PathBuf;
use tempfile::TempDir;

/// Test fixture for marketplace graph integration tests
struct MarketplaceGraphFixture {
    temp_dir: TempDir,
    graph: Graph,
}

impl MarketplaceGraphFixture {
    fn new() -> Result<Self> {
        let temp_dir = tempfile::tempdir()?;
        let graph = Graph::new()?;
        Ok(Self { temp_dir, graph })
    }

    fn project_path(&self) -> PathBuf {
        self.temp_dir.path().to_path_buf()
    }

    fn load_package_metadata(&self, turtle: &str) -> Result<()> {
        self.graph.insert_turtle(turtle)?;
        Ok(())
    }
}

#[test]
fn test_marketplace_package_metadata_in_graph() -> Result<()> {
    // Arrange: Create graph and load marketplace package metadata
    let fixture = MarketplaceGraphFixture::new()?;

    let package_metadata = r#"
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ggen: <http://ggen.io/ontology#> .

ggen:rust-axum-service
    a ggen:Package ;
    rdfs:label "Rust Axum Service" ;
    ggen:namespace "io.ggen" ;
    ggen:name "rust-axum-service" ;
    ggen:version "1.0.0" ;
    ggen:description "Production-ready Axum web service template" ;
    ggen:category "web", "rust", "async" ;
    ggen:templatePath "templates/main.tmpl" .

ggen:database-migrations
    a ggen:Package ;
    rdfs:label "Database Migrations" ;
    ggen:namespace "io.ggen" ;
    ggen:name "database-migrations" ;
    ggen:version "1.0.0" ;
    ggen:description "Database migration management" ;
    ggen:category "database", "migrations" .
"#;

    // Act: Load package metadata into graph
    fixture.load_package_metadata(package_metadata)?;

    // Assert: Query for packages
    let query = r#"
        PREFIX ggen: <http://ggen.io/ontology#>
        SELECT ?name ?description WHERE {
            ?package a ggen:Package ;
                     ggen:name ?name ;
                     ggen:description ?description .
        }
    "#;

    let results = fixture.graph.query(query)?;
    let mut found_packages = 0;
    for result in results {
        if let Ok(bindings) = result {
            found_packages += 1;
        }
    }
    assert!(found_packages > 0, "Should find packages in graph");

    // Verify specific package exists
    let axum_query = r#"
        PREFIX ggen: <http://ggen.io/ontology#>
        ASK WHERE {
            ?package a ggen:Package ;
                     ggen:name "rust-axum-service" ;
                     ggen:namespace "io.ggen" .
        }
    "#;

    let ask_results = fixture.graph.query(axum_query)?;
    let mut ask_found = false;
    for result in ask_results {
        if let Ok(_) = result {
            ask_found = true;
            break;
        }
    }
    assert!(ask_found, "Should find rust-axum-service package");

    Ok(())
}

#[test]
fn test_marketplace_graph_export_in_lifecycle() -> Result<()> {
    // Arrange: Create graph with package metadata
    let fixture = MarketplaceGraphFixture::new()?;

    let package_metadata = r#"
@prefix ggen: <http://ggen.io/ontology#> .

ggen:test-package
    a ggen:Package ;
    ggen:name "test-package" ;
    ggen:version "1.0.0" .
"#;

    fixture.load_package_metadata(package_metadata)?;

    // Act: Export graph to file (simulating lifecycle export step)
    let export_path = fixture.project_path().join("packages.ttl");
    let export = GraphExport::new(&fixture.graph);
    export.write_to_file(&export_path, RdfFormat::Turtle)?;

    // Assert: Verify file was created and contains data
    assert!(export_path.exists(), "Exported graph file should exist");

    let content = std::fs::read_to_string(&export_path)?;
    assert!(
        content.contains("test-package"),
        "Exported file should contain package data"
    );

    Ok(())
}

#[test]
fn test_marketplace_graph_query_in_lifecycle() -> Result<()> {
    // Arrange: Create graph with multiple packages
    let fixture = MarketplaceGraphFixture::new()?;

    let packages = r#"
@prefix ggen: <http://ggen.io/ontology#> .

ggen:package1 a ggen:Package ; ggen:name "package1" ; ggen:version "1.0.0" .
ggen:package2 a ggen:Package ; ggen:name "package2" ; ggen:version "2.0.0" .
ggen:package3 a ggen:Package ; ggen:name "package3" ; ggen:version "1.5.0" .
"#;

    fixture.load_package_metadata(packages)?;

    // Act: Use GraphQuery for advanced querying
    let query_builder = GraphQuery::new(&fixture.graph);
    let query = r#"
        PREFIX ggen: <http://ggen.io/ontology#>
        SELECT ?name ?version WHERE {
            ?package a ggen:Package ;
                     ggen:name ?name ;
                     ggen:version ?version .
        }
        ORDER BY ?name
    "#;

    let results = query_builder.execute(query)?;

    // Assert: Verify all packages are found
    let mut count = 0;
    for result in results {
        if let Ok(_) = result {
            count += 1;
        }
    }
    assert_eq!(count, 3, "Should find all 3 packages");

    Ok(())
}

#[test]
fn test_marketplace_graph_update_in_lifecycle() -> Result<()> {
    // Arrange: Create graph with initial package
    let fixture = MarketplaceGraphFixture::new()?;

    let initial = r#"
@prefix ggen: <http://ggen.io/ontology#> .

ggen:my-package
    a ggen:Package ;
    ggen:name "my-package" ;
    ggen:version "1.0.0" .
"#;

    fixture.load_package_metadata(initial)?;

    // Act: Use GraphUpdate to add new package version
    let update = GraphUpdate::new(&fixture.graph);
    // GraphUpdate::insert expects triples without INSERT DATA wrapper
    update.insert(
        r#"
        PREFIX ggen: <http://ggen.io/ontology#>
        ggen:my-package-v2 a ggen:Package .
        ggen:my-package-v2 ggen:name "my-package" .
        ggen:my-package-v2 ggen:version "2.0.0" .
    "#,
    )?;

    // Assert: Verify both versions exist
    let query = r#"
        PREFIX ggen: <http://ggen.io/ontology#>
        SELECT ?version WHERE {
            ?package a ggen:Package ;
                     ggen:name "my-package" ;
                     ggen:version ?version .
        }
    "#;

    let results = fixture.graph.query(query)?;
    let mut count = 0;
    for result in results {
        if let Ok(_) = result {
            count += 1;
        }
    }
    assert_eq!(count, 2, "Should find both package versions");

    Ok(())
}

#[test]
fn test_marketplace_graph_with_lifecycle_context() -> Result<()> {
    // Arrange: Create lifecycle context with graph
    let fixture = MarketplaceGraphFixture::new()?;

    // Create a simple make.toml for lifecycle
    let make_toml = r#"
[project]
name = "marketplace-graph-test"

[lifecycle.setup]
command = "echo 'Setup phase with graph'"
"#;

    let make_path = fixture.project_path().join("make.toml");
    std::fs::write(&make_path, make_toml)?;

    // Load package metadata into graph
    let packages = r#"
@prefix ggen: <http://ggen.io/ontology#> .

ggen:setup-package
    a ggen:Package ;
    ggen:name "setup-package" ;
    ggen:version "1.0.0" .
"#;

    fixture.load_package_metadata(packages)?;

    // Act: Create lifecycle context and run setup phase
    let ctx = Context::new(fixture.project_path())?;
    run_phase(&ctx, "setup")?;

    // Assert: Verify graph still works after lifecycle operations
    let query = r#"
        PREFIX ggen: <http://ggen.io/ontology#>
        SELECT ?name WHERE {
            ?package a ggen:Package ;
                     ggen:name ?name .
        }
    "#;

    let results = fixture.graph.query(query)?;
    let mut found = false;
    for result in results {
        if let Ok(_) = result {
            found = true;
            break;
        }
    }
    assert!(found, "Graph should still work after lifecycle operations");

    Ok(())
}

#[test]
fn test_marketplace_graph_export_all_formats() -> Result<()> {
    // Arrange: Create graph with package data
    let fixture = MarketplaceGraphFixture::new()?;

    let packages = r#"
@prefix ggen: <http://ggen.io/ontology#> .

ggen:export-test
    a ggen:Package ;
    ggen:name "export-test" ;
    ggen:version "1.0.0" .
"#;

    fixture.load_package_metadata(packages)?;

    // Act & Assert: Export in all supported formats
    let export = GraphExport::new(&fixture.graph);
    let formats = vec![
        RdfFormat::Turtle,
        RdfFormat::NTriples,
        RdfFormat::RdfXml,
        RdfFormat::TriG,
        RdfFormat::NQuads,
    ];

    for format in formats {
        let output = export.write_to_string(format)?;
        assert!(
            !output.is_empty(),
            "Export format {:?} should produce output",
            format
        );
    }

    Ok(())
}
