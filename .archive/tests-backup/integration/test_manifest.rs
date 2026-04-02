use chicago_tdd_tools::prelude::*;
use ggen_core::manifest::{ManifestInputs, compute_manifest_key};
use std::collections::BTreeMap;

test!(test_identical_inputs_yield_identical_hash, {
    // Arrange
    let inputs1 = ManifestInputs {
        canonical_graph: "test graph".to_string(),
        canonical_shapes: "test shapes".to_string(),
        canonical_frontmatter: "test frontmatter".to_string(),
        canonical_template: "test template".to_string(),
        seed: "test seed".to_string(),
        canonical_rows: "test rows".to_string(),
    };
    
    let inputs2 = ManifestInputs {
        canonical_graph: "test graph".to_string(),
        canonical_shapes: "test shapes".to_string(),
        canonical_frontmatter: "test frontmatter".to_string(),
        canonical_template: "test template".to_string(),
        seed: "test seed".to_string(),
        canonical_rows: "test rows".to_string(),
    };
    
    // Act
    let hash1 = compute_manifest_key(&inputs1);
    let hash2 = compute_manifest_key(&inputs2);
    
    // Assert
    assert_eq!(hash1, hash2);
    assert_eq!(hash1.len(), 64);
});

test!(test_different_inputs_yield_different_hash, {
    // Arrange
    let inputs1 = ManifestInputs {
        canonical_graph: "test graph".to_string(),
        canonical_shapes: "test shapes".to_string(),
        canonical_frontmatter: "test frontmatter".to_string(),
        canonical_template: "test template".to_string(),
        seed: "test seed".to_string(),
        canonical_rows: "test rows".to_string(),
    };
    
    let inputs2 = ManifestInputs {
        canonical_graph: "different graph".to_string(),
        canonical_shapes: "test shapes".to_string(),
        canonical_frontmatter: "test frontmatter".to_string(),
        canonical_template: "test template".to_string(),
        seed: "test seed".to_string(),
        canonical_rows: "test rows".to_string(),
    };
    
    // Act
    let hash1 = compute_manifest_key(&inputs1);
    let hash2 = compute_manifest_key(&inputs2);
    
    // Assert
    assert_ne!(hash1, hash2);
});
