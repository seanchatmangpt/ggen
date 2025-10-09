use ggen_core::rdf;
use std::path::Path;

#[test]
fn test_load_ttl_into_store() {
    let sources = vec!["tests/fixtures/sample.ttl".to_string()];
    let store = rdf::load_graph(&sources).expect("Failed to load graph");
    
    // Should have loaded some triples
    assert!(store.len() > 0);
}

#[test]
fn test_deterministic_nquads_output() {
    let sources = vec!["tests/fixtures/sample.ttl".to_string()];
    let store = rdf::load_graph(&sources).expect("Failed to load graph");
    
    let nquads1 = rdf::to_nquads(&store).expect("Failed to serialize");
    let nquads2 = rdf::to_nquads(&store).expect("Failed to serialize");
    
    // Should be identical
    assert_eq!(nquads1, nquads2);
    assert!(nquads1.contains("Test command"));
}
