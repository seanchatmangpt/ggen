use chicago_tdd_tools::prelude::*;
use ggen_core::{GenContext, Generator, Pipeline};
use std::collections::BTreeMap;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

// Import common test utilities
#[path = "../common/mod.rs"]
mod common;
use common::{create_temp_dir, write_file_in_temp};

test!(test_deterministic_generation, {
    // Arrange: Create template with same inputs
    let temp_dir = create_temp_dir();
    let template_content = r#"---
to: "output.txt"
---
Hello, {{ name }}!
Version: {{ version }}"#;
    
    let template_path = write_file_in_temp(&temp_dir, "template.tmpl", template_content);
    let output_dir = temp_dir.path().join("output");
    
    let mut vars1 = BTreeMap::new();
    vars1.insert("name".to_string(), "World".to_string());
    vars1.insert("version".to_string(), "1.0.0".to_string());
    
    let mut vars2 = BTreeMap::new();
    vars2.insert("name".to_string(), "World".to_string());
    vars2.insert("version".to_string(), "1.0.0".to_string());
    
    // Act: Generate twice with same inputs
    let pipeline1 = Pipeline::new()?;
    let ctx1 = GenContext::new(
        PathBuf::from(&template_path),
        output_dir.join("run1"),
    )
    .with_vars(vars1);
    
    let mut generator1 = Generator::new(pipeline1, ctx1);
    let output_path1 = generator1.generate()?;
    let output1 = fs::read_to_string(&output_path1)?;
    
    let pipeline2 = Pipeline::new()?;
    let ctx2 = GenContext::new(
        PathBuf::from(&template_path),
        output_dir.join("run2"),
    )
    .with_vars(vars2);
    
    let mut generator2 = Generator::new(pipeline2, ctx2);
    let output_path2 = generator2.generate()?;
    let output2 = fs::read_to_string(&output_path2)?;
    
    // Assert: Outputs must be byte-identical
    assert_eq!(output1, output2, "Identical inputs should produce identical outputs");
    assert_eq!(output1, "Hello, World!\nVersion: 1.0.0", "Output should match expected content");
    
    Ok::<(), ggen_utils::error::Error>(())
});

// Note: Removed mock tests that used simulate_deterministic_generation
// These tests didn't actually verify determinism. Real determinism tests
// should use actual generation code paths. See test_deterministic_generation
// above for a proper example.
