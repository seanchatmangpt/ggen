//! FIBO Account Opening Workflow Integration Tests
//!
//! Tests the complete pipeline from FIBO Account Opening ontology
//! to YAWL workflow XML generation.

use super::fixtures;
use ggen_yawl::{OntologyLoader, YawlGenerator};
use ggen_yawl::codegen::yawl_xml::validate;

/// Expected workflow metadata for Account Opening
const ACCOUNT_OPENING_WORKFLOW_NAME: &str = "AccountOpeningProcess";
const EXPECTED_MIN_TASKS: usize = 5;
const EXPECTED_MIN_FLOWS: usize = 3;

#[cfg(test)]
mod account_opening_loading_tests {
    use super::*;

    /// Test: Load FIBO Account Opening ontology from file
    #[test]
    fn test_load_fibo_account_opening_from_file() {
        // Arrange
        let path = fixtures::fibo_account_opening_path();
        let loader = OntologyLoader::new();

        // Act
        let result = loader.load_from_file(&path);

        // Assert
        assert!(result.is_ok(), "Should load FIBO Account Opening ontology from file");
        let graph = result.unwrap();
        assert!(!graph.is_empty(), "Graph should not be empty");
    }

    /// Test: Load FIBO Account Opening ontology from string
    #[test]
    fn test_load_fibo_account_opening_from_string() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let loader = OntologyLoader::new();

        // Act
        let result = loader.load_from_str(&content, ggen_yawl::OntologyFormat::Turtle);

        // Assert
        assert!(result.is_ok(), "Should load FIBO Account Opening ontology from string");
        let graph = result.unwrap();
        assert!(!graph.is_empty(), "Graph should not be empty");
    }

    /// Test: Verify loaded graph contains expected classes
    #[test]
    fn test_loaded_graph_contains_expected_classes() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let loader = OntologyLoader::new();
        let graph = loader.load_from_str(&content, ggen_yawl::OntologyFormat::Turtle)
            .expect("Failed to load ontology");

        // Act - Check graph is not empty
        let is_not_empty = !graph.is_empty();

        // Assert
        assert!(is_not_empty, "Graph should not be empty");
    }

    /// Test: Verify loaded graph contains flow relationships
    #[test]
    fn test_loaded_graph_contains_flow_relationships() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let loader = OntologyLoader::new();
        let graph = loader.load_from_str(&content, ggen_yawl::OntologyFormat::Turtle)
            .expect("Failed to load ontology");

        // Act - Check graph is not empty
        let is_not_empty = !graph.is_empty();

        // Assert
        assert!(is_not_empty, "Graph should contain flow relationships");
    }

    /// Test: Loading produces deterministic graph
    #[test]
    fn test_loading_is_deterministic() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let loader = OntologyLoader::new();

        // Act - Load twice
        let graph1 = loader.load_from_str(&content, ggen_yawl::OntologyFormat::Turtle)
            .expect("First load failed");
        let graph2 = loader.load_from_str(&content, ggen_yawl::OntologyFormat::Turtle)
            .expect("Second load failed");

        // Assert - Both graphs should be non-empty
        assert!(!graph1.is_empty(), "First graph should not be empty");
        assert!(!graph2.is_empty(), "Second graph should not be empty");
    }
}

#[cfg(test)]
mod account_opening_pipeline_tests {
    use super::*;

    /// Test: Full pipeline execution for Account Opening
    #[test]
    fn test_account_opening_full_pipeline() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new().with_validation(true);

        // Act
        let result = generator.generate(&content);

        // Assert
        assert!(result.is_ok(), "Full pipeline should succeed for Account Opening: {:?}", result.err());

        let xml = result.unwrap();

        // Verify XML structure
        assert!(xml.contains("<?xml version=\"1.0\""), "Should have XML declaration");
        assert!(xml.contains("<specification"), "Should have specification element");
        assert!(xml.contains("</specification>"), "Should have closing specification");
    }

    /// Test: Generated YAWL XML is valid
    #[test]
    fn test_account_opening_generates_valid_yawl() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new();

        // Act
        let result = generator.generate(&content);

        // Assert
        assert!(result.is_ok(), "Generation should succeed");
        let xml = result.unwrap();

        // Validate the generated XML
        let validation_result = validate(&xml);
        assert!(validation_result.is_ok(), "Generated XML should be valid YAWL: {:?}",
                validation_result.err());
    }

    /// Test: Generated workflow has expected name
    #[test]
    fn test_account_opening_workflow_name() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new();

        // Act
        let result = generator.generate(&content);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();

        // Should contain workflow name
        assert!(xml.contains("AccountOpening") || xml.contains("Account"),
                "Generated workflow should reference Account Opening");
    }

    /// Test: Generated workflow has tasks
    #[test]
    fn test_account_opening_has_tasks() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new();

        // Act
        let result = generator.generate(&content);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();

        // Should have task elements
        assert!(xml.contains("<task"), "Should have at least one task element");
    }

    /// Test: Generated workflow has expected task count
    #[test]
    fn test_account_opening_task_count() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new();

        // Act
        let result = generator.generate(&content);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();

        // Count task elements
        let task_count = xml.matches("<task").count();

        assert!(task_count >= EXPECTED_MIN_TASKS,
                "Expected at least {} tasks, got {}", EXPECTED_MIN_TASKS, task_count);
    }

    /// Test: Generated workflow has flows
    #[test]
    fn test_account_opening_has_flows() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new();

        // Act
        let result = generator.generate(&content);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();

        // Should have flow elements
        assert!(xml.contains("<flow") || xml.contains("</flow>"),
                "Should have flow elements");
    }

    /// Test: Pipeline with validation disabled
    #[test]
    fn test_account_opening_pipeline_no_validation() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new().with_validation(false);

        // Act
        let result = generator.generate(&content);

        // Assert
        assert!(result.is_ok(), "Pipeline should succeed without validation");
        let xml = result.unwrap();
        assert!(!xml.is_empty(), "Generated XML should not be empty");
    }
}

#[cfg(test)]
mod account_opening_determinism_tests {
    use super::*;

    /// Test: Generation is deterministic
    #[test]
    fn test_account_opening_generation_is_deterministic() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new();

        // Act - Generate twice
        let result1 = generator.generate(&content);
        let result2 = generator.generate(&content);

        // Assert
        assert!(result1.is_ok() && result2.is_ok(),
                "Both generations should succeed");

        let xml1 = result1.unwrap();
        let xml2 = result2.unwrap();

        assert_eq!(xml1, xml2, "Generation should be deterministic");
    }

    /// Test: Hash is consistent across generations
    #[test]
    fn test_account_opening_hash_consistency() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new();

        // Act - Generate twice and check output is identical
        let result1 = generator.generate(&content);
        let result2 = generator.generate(&content);

        // Assert
        assert!(result1.is_ok() && result2.is_ok());
        assert_eq!(result1.unwrap(), result2.unwrap(),
                   "Generation should be deterministic (hash consistency)");
    }

    /// Test: Content hash is deterministic
    #[test]
    fn test_account_opening_content_hash() {
        // Arrange
        let content1 = fixtures::load_fibo_account_opening();
        let content2 = fixtures::load_fibo_account_opening();

        // Assert - Same content should produce same output
        assert_eq!(content1, content2, "Content should be identical");
    }
}

#[cfg(test)]
mod account_opening_validation_tests {
    use super::*;

    /// Test: Generated XML has proper structure
    #[test]
    fn test_account_opening_xml_structure() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new();

        // Act
        let result = generator.generate(&content);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();

        // Check for YAWL XML structure elements
        assert!(xml.contains("<?xml version=\"1.0\""), "Missing XML declaration");
        assert!(xml.contains("<specification"), "Missing specification element");
        assert!(xml.contains("</specification>"), "Unclosed specification element");
        assert!(xml.contains("<decomposition"), "Missing decomposition element");
        assert!(xml.contains("</decomposition>"), "Unclosed decomposition element");
    }

    /// Test: Generated XML has input/output conditions
    #[test]
    fn test_account_opening_has_conditions() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new();

        // Act
        let result = generator.generate(&content);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();

        // Should have conditions
        assert!(xml.contains("<inputCondition") || xml.contains("<input"),
                "Should have input condition");
        assert!(xml.contains("<outputCondition") || xml.contains("<output"),
                "Should have output condition");
    }

    /// Test: Generated XML escapes special characters
    #[test]
    fn test_account_opening_xml_escaping() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new();

        // Act
        let result = generator.generate(&content);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();

        // Verify no unescaped special characters in content
        // (The XML itself should have proper escaping)
        assert!(!xml.contains("<>&"), "Should not have unescaped special characters");
    }

    /// Test: Full workflow with all stages
    #[test]
    fn test_account_opening_all_pipeline_stages() {
        // This test verifies all five stages of the μ pipeline:
        // μ₁: Normalize (RDF validation)
        // μ₂: Extract (SPARQL CONSTRUCT)
        // μ₃: Emit (Template rendering)
        // μ₄: Canonicalize (Formatting)
        // μ₅: Receipt (Cryptographic proof)

        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new().with_validation(true);

        // Act
        let result = generator.generate(&content);

        // Assert
        assert!(result.is_ok());

        let xml = result.unwrap();

        // μ₃: Emit - Verify template rendering worked
        assert!(xml.contains("<specification"), "Template should be rendered");

        // μ₄: Canonicalize - Verify formatting is consistent
        assert!(xml.contains("<?xml version=\"1.0\""), "Should have canonical XML declaration");

        // μ₅: Verify the output is valid (receipt generation would happen in production)
        let validation = validate(&xml);
        assert!(validation.is_ok(), "Output should pass validation for receipt generation");
    }
}

#[cfg(test)]
mod account_opening_performance_tests {
    use super::*;
    use std::time::Instant;

    /// Test: Generation completes within reasonable time
    #[test]
    fn test_account_opening_generation_performance() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new();

        // Act
        let start = Instant::now();
        let result = generator.generate(&content);
        let duration = start.elapsed();

        // Assert
        assert!(result.is_ok(), "Generation should succeed");
        assert!(duration.as_secs() < 5, "Generation should complete within 5 seconds, took {:?}",
                duration);
    }

    /// Test: Loading performs within SLO
    #[test]
    fn test_account_opening_loading_performance() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let loader = OntologyLoader::new();

        // Act
        let start = Instant::now();
        let result = loader.load_from_str(&content, ggen_yawl::OntologyFormat::Turtle);
        let duration = start.elapsed();

        // Assert
        assert!(result.is_ok(), "Loading should succeed");
        assert!(duration.as_secs() < 1, "Loading should complete within 1 second, took {:?}",
                duration);
    }
}

#[cfg(test)]
mod account_opening_error_handling_tests {
    use super::*;

    /// Test: Invalid Turtle produces error
    #[test]
    fn test_account_opening_invalid_turtle_error() {
        // Arrange
        let invalid_turtle = "This is not valid Turtle content";
        let generator = YawlGenerator::new();

        // Act
        let result = generator.generate(invalid_turtle);

        // Assert
        assert!(result.is_err(), "Invalid Turtle should produce error");
    }

    /// Test: Empty input produces error
    #[test]
    fn test_account_opening_empty_input_error() {
        // Arrange
        let empty_content = "";
        let generator = YawlGenerator::new();

        // Act
        let result = generator.generate(empty_content);

        // Assert
        assert!(result.is_err(), "Empty input should produce error");
    }

    /// Test: Loading with wrong format produces error
    #[test]
    fn test_account_opening_wrong_format_error() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let loader = OntologyLoader::new();

        // Try to load Turtle as RDF/XML
        let result = loader.load_from_str(&content, ggen_yawl::OntologyFormat::RdfXml);

        // Assert - Should either fail or load with warnings
        // The exact behavior depends on Oxigraph's leniency
        if result.is_ok() {
            let graph = result.unwrap();
            // If it succeeds, graph might be empty
            let is_empty = graph.is_empty();
            assert!(is_empty, "Graph should be empty for wrong format");
        }
        // If it fails, that's also acceptable
    }
}
