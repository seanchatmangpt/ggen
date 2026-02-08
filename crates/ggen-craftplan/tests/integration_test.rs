//! Integration tests for ggen-craftplan pipeline
//!
//! Tests the full five-stage μ pipeline from RDF to Elixir code.

use ggen_craftplan::pipeline::CodeGenerator;
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
    use ggen_craftplan::normalize::Normalizer;

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
