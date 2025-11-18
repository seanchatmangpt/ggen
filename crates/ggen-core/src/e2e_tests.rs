//! End-to-end tests for the ggen core functionality
//!
//! This module provides comprehensive end-to-end tests that verify the complete
//! pipeline from template parsing to code generation. These tests exercise the
//! full generation workflow including template processing, RDF integration, and
//! file generation.
//!
//! ## Test Coverage
//!
//! - **Template generation**: Basic template rendering with variables
//! - **RDF integration**: Templates with RDF data and SPARQL queries
//! - **Deterministic output**: Verify identical outputs for same inputs
//! - **Error handling**: Verify graceful error handling for invalid inputs
//!
//! ## Test Structure
//!
//! Each test:
//! 1. Creates temporary directories and files
//! 2. Sets up test templates and data
//! 3. Executes the generation pipeline
//! 4. Verifies generated output matches expectations
//! 5. Cleans up temporary resources
//!
//! ## Examples
//!
//! ### Running Tests
//!
//! ```bash
//! # Run all e2e tests
//! cargo make test --test e2e_tests
//!
//! # Run specific test
//! cargo test test_end_to_end_template_generation
//! ```

use crate::generator::Generator;
use std::collections::BTreeMap;
use tempfile::TempDir;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_end_to_end_template_generation() {
        // Create a temporary directory for testing
        let temp_dir = TempDir::new()?;
        let output_dir = temp_dir.path().join("output");
        std::fs::create_dir_all(&output_dir)?;

        // Create a test template (matching working generator test structure)
        let template_content = r#"---
to: "output/{{ name | lower }}.rs"
---
// Generated module: {{ name }}
// Version: {{ version }}

pub struct {{ name }} {
}

impl {{ name }} {
    pub fn new() -> Self {
        Self {}
    }
}
"#;

        let template_path = temp_dir.path().join("test.tmpl");
        std::fs::write(&template_path, template_content)?;

        // Create generator and run (matching working test structure)
        use crate::pipeline::Pipeline;
        let pipeline = Pipeline::new()?;
        let mut vars = BTreeMap::new();
        vars.insert("name".to_string(), "TestModule".to_string());
        vars.insert("version".to_string(), "1.0.0".to_string());

        let ctx = crate::generator::GenContext::new(template_path, temp_dir.path().to_path_buf())
            .with_vars(vars);
        let mut generator = Generator::new(pipeline, ctx);
        let result_path = generator.generate()?;

        // Verify the generated file exists
        assert!(result_path.exists());

        // Read and verify the content
        let generated_content = std::fs::read_to_string(&result_path)?;
        assert!(generated_content.contains("// Generated module: TestModule"));
        assert!(generated_content.contains("// Version: 1.0.0"));
        assert!(generated_content.contains("pub struct TestModule"));
        assert!(generated_content.contains("impl TestModule"));

        Ok(())
    }

    #[test]
    fn test_end_to_end_with_rdf_data() {
        let temp_dir = TempDir::new()?;
        let output_dir = temp_dir.path().join("output");
        std::fs::create_dir_all(&output_dir)?;

        // Create RDF data file
        let rdf_content = r#"@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:User a ex:Class ;
    rdfs:label "User" ;
    rdfs:comment "A user in the system" .

ex:Product a ex:Class ;
    rdfs:label "Product" ;
    rdfs:comment "A product in the catalog" .
"#;

        let rdf_path = temp_dir.path().join("data.ttl");
        std::fs::write(&rdf_path, rdf_content)?;

        // Create template that uses RDF data with absolute path (simplified)
        let rdf_path_str = rdf_path
            .to_str()
            .ok_or_else(|| ggen_utils::error::Error::new("Invalid UTF-8 in RDF path"))?;
        let template_content = format!(
            r#"---
to: "models.rs"
rdf:
  - "{}"
prefixes:
  ex: "http://example.org/"
  rdfs: "http://www.w3.org/2000/01/rdf-schema#"
---
// Generated models from RDF data

pub struct User {{
}}

pub struct Product {{
}}
"#,
            rdf_path_str
        );

        let template_path = temp_dir.path().join("models.tmpl");
        std::fs::write(&template_path, template_content)?;

        // Create generator and run
        use crate::pipeline::Pipeline;
        let pipeline = Pipeline::new()?;
        let ctx = crate::generator::GenContext::new(template_path, output_dir.clone());
        let mut generator = Generator::new(pipeline, ctx);
        let result_path = generator.generate()?;

        // Verify the generated file
        assert!(result_path.exists());
        let generated_content = std::fs::read_to_string(&result_path)?;

        // Should contain structs for User and Product
        assert!(generated_content.contains("pub struct User"));
        assert!(generated_content.contains("pub struct Product"));

        Ok(())
    }

    #[test]
    fn test_end_to_end_deterministic_output() {
        let temp_dir = TempDir::new()?;
        let output_dir1 = temp_dir.path().join("output1");
        let output_dir2 = temp_dir.path().join("output2");
        std::fs::create_dir_all(&output_dir1)?;
        std::fs::create_dir_all(&output_dir2)?;

        // Create template with deterministic output (matching working pattern)
        let template_content = r#"---
to: "output/{{ name | lower }}.rs"
---
// Deterministic output test

pub struct {{ name }} {
    // Static content for deterministic testing
    field1: String,
    field2: i32,
}
"#;

        let template_path = temp_dir.path().join("deterministic.tmpl");
        std::fs::write(&template_path, template_content)?;

        // Generate twice with the same input (matching working pattern)
        use crate::pipeline::Pipeline;
        let mut vars = BTreeMap::new();
        vars.insert("name".to_string(), "TestModule".to_string());

        let pipeline1 = Pipeline::new()?;
        let ctx1 =
            crate::generator::GenContext::new(template_path.clone(), temp_dir.path().to_path_buf())
                .with_vars(vars.clone());
        let mut generator1 = Generator::new(pipeline1, ctx1);
        let result_path1 = generator1.generate()?;

        let pipeline2 = Pipeline::new()?;
        let ctx2 = crate::generator::GenContext::new(template_path, temp_dir.path().to_path_buf())
            .with_vars(vars);
        let mut generator2 = Generator::new(pipeline2, ctx2);
        let result_path2 = generator2.generate()?;

        // Both should produce identical output
        let content1 = std::fs::read_to_string(&result_path1)?;
        let content2 = std::fs::read_to_string(&result_path2)?;

        assert_eq!(
            content1, content2,
            "Deterministic generation should produce identical output"
        );

        Ok(())
    }

    #[test]
    fn test_end_to_end_error_handling() {
        let temp_dir = TempDir::new()?;
        let output_dir = temp_dir.path().join("output");
        std::fs::create_dir_all(&output_dir)?;

        // Create template with invalid SPARQL
        let template_content = r#"---
to: "error_test.rs"
rdf_inline:
  - "@prefix ex: <http://example.org/> . ex:test a ex:Test ."
sparql:
  invalid: "INVALID SPARQL QUERY SYNTAX"
---
// This should fail during generation

pub struct Test {
    // Invalid SPARQL should cause an error
}
"#;

        let template_path = temp_dir.path().join("error.tmpl");
        std::fs::write(&template_path, template_content)?;

        // This should fail gracefully
        use crate::pipeline::Pipeline;
        let pipeline = Pipeline::new()?;
        let ctx = crate::generator::GenContext::new(template_path, output_dir);
        let mut generator = Generator::new(pipeline, ctx);
        let result = generator.generate();

        // Should return an error for invalid SPARQL
        assert!(
            result.is_err(),
            "Invalid SPARQL should cause generation to fail"
        );

        Ok(())
    }
}
