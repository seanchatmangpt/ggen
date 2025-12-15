use chicago_tdd_tools::prelude::*;
use ggen_core::rdf;
use std::path::Path;

test!(test_load_ttl_into_store, {
    // Arrange
    let sources = vec!["tests/fixtures/sample.ttl".to_string()];
    
    // Act
    let store = rdf::load_graph(&sources).expect("Failed to load graph");
    
    // Assert
    assert!(store.len() > 0);
});

test!(test_deterministic_nquads_output, {
    // Arrange
    let sources = vec!["tests/fixtures/sample.ttl".to_string()];
    let store = rdf::load_graph(&sources).expect("Failed to load graph");
    
    // Act
    let nquads1 = rdf::to_nquads(&store).expect("Failed to serialize");
    let nquads2 = rdf::to_nquads(&store).expect("Failed to serialize");
    
    // Assert
    assert_eq!(nquads1, nquads2);
    assert!(nquads1.contains("Test command"));
});
