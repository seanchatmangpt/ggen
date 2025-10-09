//! End-to-end tests for the rgen core functionality
//! These tests verify the complete pipeline from template parsing to code generation

use std::collections::BTreeMap;
use tempfile::TempDir;
use utils::error::Result;

use crate::generator::Generator;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_end_to_end_template_generation() -> Result<()> {
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
    // TODO: Add implementation
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
        
        let ctx = crate::generator::GenContext::new(template_path, temp_dir.path().to_path_buf()).with_vars(vars);
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
    fn test_end_to_end_with_rdf_data() -> Result<()> {
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
        let rdf_path_str = rdf_path.to_str().unwrap();
        let template_content = format!(r#"---
to: "models.rs"
rdf:
  - "{}"
prefixes:
  ex: "http://example.org/"
  rdfs: "http://www.w3.org/2000/01/rdf-schema#"
---
// Generated models from RDF data

pub struct User {{
    // TODO: Add fields
}}

pub struct Product {{
    // TODO: Add fields
}}
"#, rdf_path_str);

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
    #[ignore] // SHACL validation not yet implemented
    fn test_end_to_end_with_shacl_validation() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let output_dir = temp_dir.path().join("output");
        std::fs::create_dir_all(&output_dir)?;

        // Create SHACL shape
        let shape_content = r#"@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:UserShape sh:targetClass ex:User ;
    sh:property [
        sh:path rdfs:label ;
        sh:datatype <http://www.w3.org/2001/XMLSchema#string> ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
    ] .
"#;

        let shape_path = temp_dir.path().join("shapes.ttl");
        std::fs::write(&shape_path, shape_content)?;

        // Create RDF data
        let rdf_content = r#"@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:alice a ex:User ;
    rdfs:label "Alice" .
"#;

        let rdf_path = temp_dir.path().join("data.ttl");
        std::fs::write(&rdf_path, rdf_content)?;

        // Create template with SHACL validation
        let template_content = r#"---
to: "validated_models.rs"
rdf:
  - "data.ttl"
shape:
  - "shapes.ttl"
---
// Models validated against SHACL shapes

pub struct User {
    pub name: String,
}

impl User {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}
"#;

        let template_path = temp_dir.path().join("validated.tmpl");
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
        assert!(generated_content.contains("pub struct User"));

        Ok(())
    }

    #[test]
    fn test_end_to_end_deterministic_output() -> Result<()> {
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
        let ctx1 = crate::generator::GenContext::new(template_path.clone(), temp_dir.path().to_path_buf()).with_vars(vars.clone());
        let mut generator1 = Generator::new(pipeline1, ctx1);
        let result_path1 = generator1.generate()?;

        let pipeline2 = Pipeline::new()?;
        let ctx2 = crate::generator::GenContext::new(template_path, temp_dir.path().to_path_buf()).with_vars(vars);
        let mut generator2 = Generator::new(pipeline2, ctx2);
        let result_path2 = generator2.generate()?;

        // Both should produce identical output
        let content1 = std::fs::read_to_string(&result_path1)?;
        let content2 = std::fs::read_to_string(&result_path2)?;
        
        assert_eq!(content1, content2, "Deterministic generation should produce identical output");

        Ok(())
    }

    #[test]
    fn test_end_to_end_error_handling() -> Result<()> {
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
        assert!(result.is_err(), "Invalid SPARQL should cause generation to fail");

        Ok(())
    }
}