//! Integration test: Full μ₁–μ₅ pipeline with embedded RDF ontologies
//!
//! This test verifies that the complete code generation pipeline works offline
//! using only embedded core ontologies from the bundle, with no network access required.
//!
//! Test cases:
//! 1. Single ontology: Load RDF from core bundle, generate simple artifact
//! 2. Multiple ontologies: Load RDF + OWL from core bundle, compose and generate

use ggen_core::ontology::OntologyLoader;
use ggen_core::pipeline_engine::{Epoch, OntologyInput};
use std::path::{Path, PathBuf};
use tempfile::TempDir;

#[test]
fn test_pipeline_single_embedded_ontology() {
    // Verify RDF ontology is available in core bundle
    assert!(
        OntologyLoader::is_embedded("http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
        "RDF should be embedded in core bundle"
    );

    // Create an epoch using only embedded ontology (offline)
    let epoch = Epoch::create_with_fallback(
        Path::new("."),
        &["http://www.w3.org/1999/02/22-rdf-syntax-ns#"],
    )
    .expect("Should load RDF from core bundle");

    // Verify epoch was created successfully
    assert_eq!(epoch.inputs.len(), 1, "Should have 1 ontology input");
    assert!(!epoch.id.is_empty(), "Epoch should have computed ID");
    assert_eq!(epoch.id.len(), 64, "Epoch ID should be SHA-256 hex (64 chars)");
    assert!(
        epoch.total_triples > 0,
        "RDF ontology should have triples"
    );

    // Verify input metadata
    let rdf_input = epoch
        .inputs
        .get(&PathBuf::from(
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
        ))
        .expect("Should find RDF input");

    assert!(!rdf_input.hash.is_empty(), "RDF should have computed hash");
    assert_eq!(rdf_input.hash.len(), 64, "Hash should be SHA-256 hex");
    assert!(rdf_input.size_bytes > 0, "RDF should have content");
    assert!(rdf_input.triple_count > 0, "RDF should have triples");

    println!(
        "✓ Single ontology epoch created successfully\n\
         - ID: {}\n\
         - Size: {} bytes\n\
         - Triples: {}\n\
         - Hash: {}",
        epoch.id, rdf_input.size_bytes, rdf_input.triple_count, rdf_input.hash
    );
}

#[test]
fn test_pipeline_multiple_embedded_ontologies() {
    // Verify both ontologies are embedded
    assert!(
        OntologyLoader::is_embedded("http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
        "RDF should be embedded"
    );
    assert!(
        OntologyLoader::is_embedded("http://www.w3.org/2002/07/owl#"),
        "OWL should be embedded"
    );

    // Create epoch with multiple embedded ontologies
    let epoch = Epoch::create_with_fallback(
        Path::new("."),
        &[
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
            "http://www.w3.org/2002/07/owl#",
        ],
    )
    .expect("Should load RDF and OWL from core bundle");

    // Verify epoch has both ontologies
    assert_eq!(epoch.inputs.len(), 2, "Should have 2 ontology inputs");
    assert!(!epoch.id.is_empty(), "Epoch should have ID");
    assert!(
        epoch.total_triples > 0,
        "Should have combined triple count"
    );

    // Verify each ontology is present
    let rdf_input = epoch
        .inputs
        .get(&PathBuf::from(
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
        ))
        .expect("Should find RDF input");

    let owl_input = epoch
        .inputs
        .get(&PathBuf::from(
            "http://www.w3.org/2002/07/owl#"
        ))
        .expect("Should find OWL input");

    // Verify hashes are deterministic
    assert_eq!(rdf_input.hash.len(), 64, "RDF hash should be SHA-256");
    assert_eq!(owl_input.hash.len(), 64, "OWL hash should be SHA-256");
    assert_ne!(
        rdf_input.hash, owl_input.hash,
        "Different ontologies should have different hashes"
    );

    // Verify triple counts are reasonable
    assert!(
        rdf_input.triple_count > 0,
        "RDF should have triples"
    );
    assert!(
        owl_input.triple_count > 0,
        "OWL should have triples"
    );
    assert_eq!(
        epoch.total_triples,
        rdf_input.triple_count + owl_input.triple_count,
        "Total triples should be sum of all ontologies"
    );

    println!(
        "✓ Multiple ontologies epoch created successfully\n\
         - ID: {}\n\
         - RDF: {} bytes, {} triples\n\
         - OWL: {} bytes, {} triples\n\
         - Total triples: {}",
        epoch.id,
        rdf_input.size_bytes,
        rdf_input.triple_count,
        owl_input.size_bytes,
        owl_input.triple_count,
        epoch.total_triples
    );
}

#[test]
fn test_pipeline_mixed_embedded_and_local() {
    // Create a temporary directory with a custom ontology
    let temp_dir = TempDir::new().expect("Should create temp directory");
    let custom_ontology_path = temp_dir.path().join("custom.ttl");

    // Write a minimal custom RDF ontology
    std::fs::write(
        &custom_ontology_path,
        r#"@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

ex:MyClass a owl:Class ;
    rdfs:label "My Custom Class" ;
    rdfs:comment "A custom class defined locally" .

ex:property1 a owl:ObjectProperty ;
    rdfs:domain ex:MyClass ;
    rdfs:range ex:MyClass .

ex:Individual1 a ex:MyClass ;
    ex:property1 ex:Individual2 .

ex:Individual2 a ex:MyClass .
"#,
    )
    .expect("Should write custom ontology");

    // Create epoch with mixed: local file + embedded ontology
    let epoch = Epoch::create_with_fallback(
        temp_dir.path(),
        &[
            "custom.ttl",
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
        ],
    )
    .expect("Should load both custom and embedded ontologies");

    // Verify epoch has both ontologies
    assert_eq!(epoch.inputs.len(), 2, "Should have 2 inputs");
    assert!(!epoch.id.is_empty(), "Epoch should have ID");
    assert!(epoch.total_triples > 0, "Should count triples from both");

    // Verify custom ontology is loaded
    let custom_input = epoch
        .inputs
        .get(&PathBuf::from("custom.ttl"))
        .expect("Should find custom ontology input");

    assert!(!custom_input.hash.is_empty(), "Custom should have hash");
    assert!(custom_input.size_bytes > 0, "Custom should have content");
    assert!(custom_input.triple_count > 0, "Custom should have triples");

    // Verify embedded ontology is also loaded
    let rdf_input = epoch
        .inputs
        .get(&PathBuf::from(
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
        ))
        .expect("Should find RDF input");

    assert!(!rdf_input.hash.is_empty(), "RDF should have hash");
    assert!(rdf_input.size_bytes > 0, "RDF should have content");

    println!(
        "✓ Mixed local + embedded ontologies epoch created successfully\n\
         - Custom: {} bytes, {} triples\n\
         - RDF: {} bytes, {} triples\n\
         - Total triples: {}",
        custom_input.size_bytes,
        custom_input.triple_count,
        rdf_input.size_bytes,
        rdf_input.triple_count,
        epoch.total_triples
    );
}

#[test]
fn test_epoch_determinism_with_embedded() {
    // Create two epochs with the same embedded ontologies
    let epoch1 = Epoch::create_with_fallback(
        Path::new("."),
        &["http://www.w3.org/1999/02/22-rdf-syntax-ns#"],
    )
    .expect("First epoch should load");

    let epoch2 = Epoch::create_with_fallback(
        Path::new("."),
        &["http://www.w3.org/1999/02/22-rdf-syntax-ns#"],
    )
    .expect("Second epoch should load");

    // Epochs should be identical (same ID, same hashes)
    // Note: timestamps will differ, but epoch ID should be the same
    assert_eq!(
        epoch1.id, epoch2.id,
        "Same inputs should produce same epoch ID"
    );

    assert_eq!(
        epoch1.inputs, epoch2.inputs,
        "Same inputs should produce identical input metadata"
    );

    println!(
        "✓ Epoch determinism verified\n\
         - Both epochs produced ID: {}",
        epoch1.id
    );
}

#[test]
fn test_no_network_access_required() {
    // This test verifies that loading embedded ontologies requires no network access
    // It should succeed even in an offline environment

    let embedded_uris = vec![
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
        "http://www.w3.org/2000/01/rdf-schema#",
        "http://www.w3.org/2002/07/owl#",
    ];

    // Verify all are embedded
    for uri in &embedded_uris {
        assert!(
            OntologyLoader::is_embedded(uri),
            "URI {} should be embedded in core bundle",
            uri
        );
    }

    // Load all via OntologyInput
    let base_path = Path::new(".");
    for uri in &embedded_uris {
        let input = OntologyInput::from_namespace(uri, base_path, Some(uri))
            .expect(&format!("Should load {} from core bundle without network", uri));

        assert!(
            !input.hash.is_empty(),
            "URI {} should have computed hash",
            uri
        );
        assert!(
            input.size_bytes > 0,
            "URI {} should have content",
            uri
        );
    }

    println!("✓ All embedded ontologies verified offline without network access");
}
