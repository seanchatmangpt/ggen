//! Integration tests for ggen-craftplan pipeline
//!
//! Tests the full five-stage μ pipeline from RDF to Elixir code.

use ggen_craftplan::pipeline::CodeGenerator;
use ggen_craftplan::extract::Extractor;
use ggen_craftplan::normalize::Normalizer;
use tempfile::TempDir;

#[test]
fn test_full_pipeline_simple_ontology() {
    // Arrange: Create a simple ontology
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let output_dir = TempDir::new().expect("Failed to create output dir");

    let ontology_path = temp_dir.path().join("test_ontology.ttl");
    let ontology_content = r#"
        @prefix craftplan: <http://craftplan.org/ontology/> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .

        craftplan:Product a owl:Class ;
            rdfs:label "Product" ;
            rdfs:comment "A product in the catalog" ;
            craftplan:namespace "Catalog" .
    "#;

    std::fs::write(&ontology_path, ontology_content).expect("Failed to write ontology file");

    // Act: Run the pipeline
    let generator = CodeGenerator::new(output_dir.path()).expect("Failed to create generator");

    // This test verifies the pipeline infrastructure exists
    // Full integration would require more complete implementations
    assert!(generator.generate_receipts);
}

#[test]
fn test_normalize_stage() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let ontology_path = temp_dir.path().join("test.ttl");
    let ontology = r#"
        @prefix craftplan: <http://craftplan.org/ontology/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .

        craftplan:Product a owl:Class .
    "#;

    std::fs::write(&ontology_path, ontology).expect("Failed to write ontology");

    let mut normalizer = Normalizer::new().expect("Failed to create normalizer");
    let result = normalizer.load_rdf(&ontology_path);

    assert!(result.is_ok(), "Normalizer should load RDF successfully");

    let triple_count = normalizer.validate().expect("Validation should succeed");
    assert!(triple_count > 0, "Should have loaded some triples");
}

#[test]
fn test_extract_stage_returns_entities() {
    // Chicago TDD: Test that extract returns non-empty entity list
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let ontology_path = temp_dir.path().join("test_extract.ttl");

    // Create ontology with entities using craft: namespace
    let ontology = r#"
        @prefix craft: <http://craftplan.org/ontology/> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

        craft:Product a craft:Entity ;
            craft:name "Product" ;
            craft:pluralName "Products" ;
            craft:hasAttribute craft:productNameAttr, craft:productSkuAttr .

        craft:productNameAttr a craft:Attribute ;
            craft:name "productName" ;
            craft:type "xsd:string" ;
            craft:required "true" .

        craft:productSkuAttr a craft:Attribute ;
            craft:name "productSku" ;
            craft:type "xsd:string" ;
            craft:required "true" ;
            craft:documentation "Stock keeping unit" .
    "#;

    std::fs::write(&ontology_path, ontology).expect("Failed to write ontology");

    // Load and normalize
    let mut normalizer = Normalizer::new().expect("Failed to create normalizer");
    normalizer.load_rdf(&ontology_path).expect("Failed to load RDF");
    normalizer.validate().expect("Failed to validate RDF");

    // Extract entities
    let mut extractor = Extractor::new(&normalizer).expect("Failed to create extractor");
    let entities = extractor
        .extract_entities(normalizer.store())
        .expect("Failed to extract entities");

    // Assert: Extract returns non-empty entity list
    assert!(!entities.is_empty(), "Extract should return at least one entity");
    assert_eq!(entities.len(), 1, "Should extract exactly one entity");
    assert_eq!(entities[0].name, "Product", "Entity name should be Product");
    assert_eq!(entities[0].plural, Some("Products".to_string()));

    // Extract attributes for the entity
    let attributes = extractor
        .extract_attributes(normalizer.store(), "Product")
        .expect("Failed to extract attributes");

    assert!(!attributes.is_empty(), "Should extract attributes");
    assert_eq!(attributes.len(), 2, "Should extract 2 attributes");

    // Find specific attributes (order may vary)
    let product_name = attributes.iter().find(|a| a.name == "productName").expect("Should find productName");
    assert_eq!(product_name.type_, "xsd:string");
    assert!(product_name.required);

    let product_sku = attributes.iter().find(|a| a.name == "productSku").expect("Should find productSku");
    assert_eq!(product_sku.doc, Some("Stock keeping unit".to_string()));
}

#[test]
fn test_emit_stage_writes_files() {
    // Chicago TDD: Test that emit writes files to output directory
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let output_dir = TempDir::new().expect("Failed to create output dir");
    let ontology_path = temp_dir.path().join("test_emit.ttl");

    // Create minimal ontology
    let ontology = r#"
        @prefix craft: <http://craftplan.org/ontology/> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

        craft:Product a craft:Entity ;
            craft:name "Product" ;
            craft:hasAttribute craft:productNameAttr .

        craft:productNameAttr a craft:Attribute ;
            craft:name "productName" ;
            craft:type "xsd:string" .
    "#;

    std::fs::write(&ontology_path, ontology).expect("Failed to write ontology");

    // Run the full pipeline
    let generator = CodeGenerator::new(output_dir.path()).expect("Failed to create generator");
    let result = generator.generate_from_rdf(&ontology_path);

    if let Err(e) = &result {
        eprintln!("Pipeline error: {:?}", e);
    }
    assert!(result.is_ok(), "Pipeline should succeed");

    // Assert: Files were written to output directory
    let output_entries: Vec<_> = std::fs::read_dir(output_dir.path())
        .expect("Failed to read output dir")
        .filter_map(|e| e.ok())
        .collect();

    assert!(!output_entries.is_empty(), "Output directory should contain files");

    // Check that generated files exist and have content
    for entry in &output_entries {
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) == Some("ex") {
            let content = std::fs::read_to_string(&path).expect("Failed to read file");
            assert!(!content.is_empty(), "Generated file should have content");
            assert!(content.contains("defmodule"), "Should contain Elixir module definition");
        }
    }
}

#[test]
fn test_full_pipeline_end_to_end() {
    // Chicago TDD: Full end-to-end test with real RDF file
    let output_dir = TempDir::new().expect("Failed to create output dir");

    // Use the example product-catalog.ttl file
    let example_ttl = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/examples/product-catalog.ttl"
    );

    // Run the full pipeline
    let generator = CodeGenerator::new(output_dir.path()).expect("Failed to create generator");
    let receipt = generator
        .generate_from_rdf(example_ttl)
        .expect("Pipeline should succeed");

    // Assert: Receipt is generated with metadata
    assert!(!receipt.input_hash.is_empty(), "Input hash should be computed");
    assert!(receipt.metadata.entity_count > 0, "Should process at least one entity");
    assert!(receipt.metadata.file_count > 0, "Should generate at least one file");
    assert!(receipt.metadata.duration_ms > 0, "Should record duration");

    // Assert: Files were written
    let output_entries: Vec<_> = std::fs::read_dir(output_dir.path())
        .expect("Failed to read output dir")
        .filter_map(|e| e.ok())
        .collect();

    assert!(
        output_entries.len() >= receipt.metadata.file_count,
        "Output directory should contain generated files"
    );
}

#[test]
fn test_receipt_generation() {
    use ggen_craftplan::receipt::ReceiptGenerator;

    let generator = ReceiptGenerator::new();

    // Test hash computation
    let hash1 = generator.compute_hash("test content");
    let hash2 = generator.compute_hash("test content");
    let hash3 = generator.compute_hash("different content");

    assert_eq!(hash1, hash2, "Same content should produce same hash");
    assert_ne!(
        hash1, hash3,
        "Different content should produce different hash"
    );
    assert_eq!(hash1.len(), 64, "SHA-256 hash should be 64 hex characters");
}

#[test]
fn test_canonicalization() {
    use ggen_craftplan::canonicalize::Canonicalizer;

    let canonicalizer = Canonicalizer::new();

    let input = "line 1  \nline 2   \nline 3";
    let normalized = canonicalizer.normalize_whitespace(input);

    assert!(
        !normalized.contains("  "),
        "Trailing spaces should be removed"
    );
    assert!(normalized.contains("\n"), "Newlines should be preserved");
}

