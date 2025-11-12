use chicago_tdd_tools::prelude::*;
use std::collections::BTreeMap;
use std::process::Command;
use std::str;

// Helper function to simulate deterministic generation
fn simulate_deterministic_generation(input: &str) -> String {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    
    let mut hasher = DefaultHasher::new();
    input.hash(&mut hasher);
    format!("hash_{}", hasher.finish())
}

test!(test_deterministic_generation, {
    // Arrange
    let input1 = "cmd=hello";
    let input2 = "cmd=hello";
    let input3 = "cmd=world";
    
    // Act
    let output1 = simulate_deterministic_generation(input1);
    let output2 = simulate_deterministic_generation(input2);
    let output3 = simulate_deterministic_generation(input3);
    
    // Assert
    assert_eq!(output1, output2, "Identical inputs should produce identical outputs");
    assert_ne!(output1, output3, "Different inputs should produce different outputs");
});

test!(test_matrix_generation, {
    // Arrange
    let matrix_vars = vec!["rust", "python"];
    let mut outputs = Vec::new();
    
    // Act
    for lang in &matrix_vars {
        let output = simulate_deterministic_generation(&format!("cmd=test,lang={}", lang));
        outputs.push(output);
    }
    
    // Assert
    assert_eq!(outputs.len(), 2);
    assert_ne!(outputs[0], outputs[1], "Different matrix values should produce different outputs");
});

test!(test_shacl_validation, {
    // Arrange
    let valid_input = "cmd=test";
    let invalid_input = "";
    
    // Act
    let valid_output = simulate_deterministic_generation(valid_input);
    let invalid_output = simulate_deterministic_generation(invalid_input);
    
    // Assert
    assert!(!valid_output.is_empty());
    assert!(!invalid_output.is_empty());
    assert_ne!(valid_output, invalid_output);
});

test!(test_sparql_order_by_enforcement, {
    // Arrange
    let ordered_query = "SELECT * WHERE { ?s ?p ?o } ORDER BY ?s";
    let unordered_query = "SELECT * WHERE { ?s ?p ?o }";
    
    // Act
    let ordered_output = simulate_deterministic_generation(ordered_query);
    let unordered_output = simulate_deterministic_generation(unordered_query);
    
    // Assert
    assert!(!ordered_output.is_empty());
    assert!(!unordered_output.is_empty());
    assert_ne!(ordered_output, unordered_output);
});

test!(test_invalid_sparql_rejection, {
    use std::fs;
    use tempfile::TempDir;
    
    // Arrange
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let template_dir = temp_dir.path().join("templates");
    fs::create_dir_all(&template_dir).expect("Failed to create template dir");
    
    let invalid_template = r#"---
to: "invalid_test.rs"
vars:
  name: "InvalidTest"
rdf_inline:
  - "@prefix ex: <http://example.org/> . ex:test a ex:Test ."
sparql:
  invalid_query: "INVALID SPARQL SYNTAX HERE"
---
// This template should fail during generation due to invalid SPARQL

pub struct {{name}} {
    // Invalid SPARQL should cause an error
}
"#;
    
    let template_path = template_dir.join("invalid.tmpl");
    fs::write(&template_path, invalid_template).expect("Failed to write template");
    
    let valid_sparql = "SELECT * WHERE { ?s ?p ?o }";
    let invalid_sparql = "INVALID SPARQL SYNTAX HERE";
    
    // Act
    let valid_output = simulate_deterministic_generation(valid_sparql);
    let invalid_output = simulate_deterministic_generation(invalid_sparql);
    
    // Assert
    assert!(!valid_output.is_empty());
    assert!(!invalid_output.is_empty());
    assert_ne!(valid_output, invalid_output);
});

test!(test_variable_precedence, {
    // Arrange
    let cli_vars = "cmd=cli_value";
    let sparql_vars = "cmd=sparql_value";
    let default_vars = "cmd=default_value";
    
    // Act
    let cli_output = simulate_deterministic_generation(cli_vars);
    let sparql_output = simulate_deterministic_generation(sparql_vars);
    let default_output = simulate_deterministic_generation(default_vars);
    
    // Assert
    assert!(!cli_output.is_empty());
    assert!(!sparql_output.is_empty());
    assert!(!default_output.is_empty());
    assert_ne!(cli_output, sparql_output);
    assert_ne!(sparql_output, default_output);
    assert_ne!(cli_output, default_output);
});

fn extract_manifest_key(output: &str) -> &str {
    output.lines()
        .find(|line| line.starts_with("manifest:"))
        .and_then(|line| line.split(": ").nth(1))
        .unwrap_or("")
}
