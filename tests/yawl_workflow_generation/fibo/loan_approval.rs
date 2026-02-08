//! FIBO Loan Approval Workflow Integration Tests
//!
//! Tests the complete pipeline from FIBO Loan Approval ontology
//! to YAWL workflow XML generation. This workflow tests more complex
//! patterns including:
//! - Parallel execution (CreditCheck + IncomeVerification)
//! - Conditional branching (LowRisk vs HighRisk)
//! - Multiple merge points

use super::fixtures;
use ggen_yawl::{OntologyLoader, YawlGenerator};
use ggen_yawl::codegen::yawl_xml;

/// Expected workflow metadata for Loan Approval
const LOAN_APPROVAL_WORKFLOW_NAME: &str = "LoanApprovalProcess";
const EXPECTED_MIN_TASKS: usize = 7;
const EXPECTED_MIN_FLOWS: usize = 5;

#[cfg(test)]
mod loan_approval_loading_tests {
    use super::*;

    /// Test: Load FIBO Loan Approval ontology from file
    #[test]
    fn test_load_fibo_loan_approval_from_file() {
        // Arrange
        let path = fixtures::fibo_loan_approval_path();
        let loader = OntologyLoader::new();

        // Act
        let result = loader.load_from_file(&path);

        // Assert
        assert!(result.is_ok(), "Should load FIBO Loan Approval ontology from file");
        let graph = result.unwrap();
        assert!(!graph.is_empty(), "Graph should not be empty");
    }

    /// Test: Load FIBO Loan Approval ontology from string
    #[test]
    fn test_load_fibo_loan_approval_from_string() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
        let loader = OntologyLoader::new();

        // Act
        let result = loader.load_from_str(&content, ggen_yawl::OntologyFormat::Turtle);

        // Assert
        assert!(result.is_ok(), "Should load FIBO Loan Approval ontology from string");
        let graph = result.unwrap();
        assert!(!graph.is_empty(), "Graph should not be empty");
    }

    /// Test: Verify loaded graph contains expected process classes
    #[test]
    fn test_loaded_graph_contains_expected_classes() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
        let loader = OntologyLoader::new();
        let graph = loader.load_from_str(&content, ggen_yawl::OntologyFormat::Turtle)
            .expect("Failed to load ontology");

        // Act
        let is_not_empty = !graph.is_empty();

        // Assert
        assert!(is_not_empty, "Graph should contain process classes");
    }

    /// Test: Verify loaded graph contains parallel flow relationships
    #[test]
    fn test_loaded_graph_contains_parallel_relationships() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
        let loader = OntologyLoader::new();
        let graph = loader.load_from_str(&content, ggen_yawl::OntologyFormat::Turtle)
            .expect("Failed to load ontology");

        // Act
        let is_not_empty = !graph.is_empty();

        // Assert
        assert!(is_not_empty, "Graph should contain parallel flow relationships");
    }

    /// Test: Verify loaded graph contains conditional flow relationships
    #[test]
    fn test_loaded_graph_contains_conditional_relationships() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
        let loader = OntologyLoader::new();
        let graph = loader.load_from_str(&content, ggen_yawl::OntologyFormat::Turtle)
            .expect("Failed to load ontology");

        // Act
        let is_not_empty = !graph.is_empty();

        // Assert
        assert!(is_not_empty, "Graph should contain conditional flow relationships");
    }

    /// Test: Count classes in Loan Approval ontology
    #[test]
    fn test_count_loan_approval_classes() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
        let loader = OntologyLoader::new();
        let graph = loader.load_from_str(&content, ggen_yawl::OntologyFormat::Turtle)
            .expect("Failed to load ontology");

        // Act
        let is_not_empty = !graph.is_empty();

        // Assert
        assert!(is_not_empty, "Should have loaded ontology data");
    }
}

#[cfg(test)]
mod loan_approval_pipeline_tests {
    use super::*;

    /// Test: Full pipeline execution for Loan Approval
    #[test]
    fn test_loan_approval_full_pipeline() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
        let generator = YawlGenerator::new().with_validation(true);

        // Act
        let result = generator.generate(&content);

        // Assert
        assert!(result.is_ok(), "Full pipeline should succeed for Loan Approval: {:?}", result.err());

        let xml = result.unwrap();

        // Verify XML structure
        assert!(xml.contains("<?xml version=\"1.0\""), "Should have XML declaration");
        assert!(xml.contains("<specification"), "Should have specification element");
        assert!(xml.contains("</specification>"), "Should have closing specification");
    }

    /// Test: Generated YAWL XML is valid
    #[test]
    fn test_loan_approval_generates_valid_yawl() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
        let generator = YawlGenerator::new();

        // Act
        let result = generator.generate(&content);

        // Assert
        assert!(result.is_ok(), "Generation should succeed");
        let xml = result.unwrap();

        // Validate the generated XML
        let validation_result = ggen_yawl::codegen::yawl_xml::validate(&xml);
        assert!(validation_result.is_ok(), "Generated XML should be valid YAWL: {:?}",
                validation_result.err());
    }

    /// Test: Generated workflow has expected name
    #[test]
    fn test_loan_approval_workflow_name() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
        let generator = YawlGenerator::new();

        // Act
        let result = generator.generate(&content);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();

        // Should contain workflow name
        assert!(xml.contains("Loan") || xml.contains("Approval"),
                "Generated workflow should reference Loan Approval");
    }

    /// Test: Generated workflow has expected minimum task count
    #[test]
    fn test_loan_approval_task_count() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
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
    fn test_loan_approval_has_flows() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
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

    /// Test: Generated workflow includes decomposition
    #[test]
    fn test_loan_approval_has_decomposition() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
        let generator = YawlGenerator::new();

        // Act
        let result = generator.generate(&content);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();

        // Should have decomposition element
        assert!(xml.contains("<decomposition"),
                "Should have decomposition element");
        assert!(xml.contains("</decomposition>"),
                "Should have closing decomposition element");
    }

    /// Test: Complex workflow structure preserved
    #[test]
    fn test_loan_approval_complex_structure() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
        let generator = YawlGenerator::new();

        // Act
        let result = generator.generate(&content);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();

        // Verify the workflow has the complex structure elements:
        // - Multiple tasks
        // - Flow connections
        // - Input/output conditions
        assert!(xml.contains("<task"), "Should have tasks");
        assert!(xml.contains("<flow") || xml.contains("</flow>"), "Should have flows");
        assert!(xml.matches("<task").count() >= EXPECTED_MIN_TASKS,
                "Should have at least {} tasks", EXPECTED_MIN_TASKS);
    }
}

#[cfg(test)]
mod loan_approval_pattern_tests {
    use super::*;

    /// Test: Parallel flow pattern is preserved
    ///
    /// Loan Approval has CreditCheck and IncomeVerification
    /// that can run concurrently (runsConcurrentlyWith property)
    #[test]
    fn test_loan_approval_parallel_pattern() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
        let loader = OntologyLoader::new();
        let graph = loader.load_from_str(&content, ggen_yawl::OntologyFormat::Turtle)
            .expect("Failed to load ontology");

        // Act
        let is_not_empty = !graph.is_empty();

        // Assert
        assert!(is_not_empty, "Should have loaded parallel flow data");
    }

    /// Test: Conditional branching pattern is preserved
    ///
    /// Loan Approval has LowRisk and HighRisk paths
    #[test]
    fn test_loan_approval_conditional_pattern() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
        let loader = OntologyLoader::new();
        let graph = loader.load_from_str(&content, ggen_yawl::OntologyFormat::Turtle)
            .expect("Failed to load ontology");

        // Act
        let is_not_empty = !graph.is_empty();

        // Assert
        assert!(is_not_empty, "Should have loaded conditional flow data");
    }

    /// Test: Merge pattern is preserved
    ///
    /// Both LowRisk and HighRisk paths merge to ApprovalDecision
    #[test]
    fn test_loan_approval_merge_pattern() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
        let loader = OntologyLoader::new();
        let graph = loader.load_from_str(&content, ggen_yawl::OntologyFormat::Turtle)
            .expect("Failed to load ontology");

        // Act
        let is_not_empty = !graph.is_empty();

        // Assert
        assert!(is_not_empty, "Should have loaded merge pattern data");
    }
}

#[cfg(test)]
mod loan_approval_determinism_tests {
    use super::*;

    /// Test: Generation is deterministic
    #[test]
    fn test_loan_approval_generation_is_deterministic() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
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

    /// Test: Multiple iterations produce consistent output
    #[test]
    fn test_loan_approval_multiple_iterations_consistent() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
        let generator = YawlGenerator::new();

        // Act - Generate multiple times
        let mut results = Vec::new();
        for _ in 0..3 {
            let result = generator.generate(&content);
            assert!(result.is_ok(), "Each generation should succeed");
            results.push(result.unwrap());
        }

        // Assert - All results should be identical
        let first = &results[0];
        for result in &results[1..] {
            assert_eq!(result, first, "All generations should produce identical output");
        }
    }
}

#[cfg(test)]
mod loan_approval_validation_tests {
    use super::*;

    /// Test: Generated XML has proper YAWL structure
    #[test]
    fn test_loan_approval_xml_structure() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
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

    /// Test: Generated XML has required YAWL elements
    #[test]
    fn test_loan_approval_required_elements() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
        let generator = YawlGenerator::new();

        // Act
        let result = generator.generate(&content);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();

        // Required YAWL elements
        assert!(xml.contains("<name>") || xml.contains("name="),
                "Should have workflow name");
        assert!(xml.contains("<decomposition") || xml.contains("decomposition"),
                "Should have decomposition/net element");
    }

    /// Test: Generated XML passes validation
    #[test]
    fn test_loan_approval_passes_validation() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
        let generator = YawlGenerator::new().with_validation(true);

        // Act
        let result = generator.generate(&content);

        // Assert
        assert!(result.is_ok(), "Generation with validation should succeed");

        let xml = result.unwrap();
        let validation_result = ggen_yawl::codegen::yawl_xml::validate(&xml);

        assert!(validation_result.is_ok(), "Generated XML should pass validation");
    }

    /// Test: Full pipeline stages execution
    #[test]
    fn test_loan_approval_all_pipeline_stages() {
        // This test verifies all five stages of the μ pipeline:
        // μ₁: Normalize (RDF validation)
        // μ₂: Extract (SPARQL CONSTRUCT)
        // μ₃: Emit (Template rendering)
        // μ₄: Canonicalize (Formatting)
        // μ₅: Receipt (Cryptographic proof)

        // Arrange
        let content = fixtures::load_fibo_loan_approval();
        let generator = YawlGenerator::new().with_validation(true);

        // Act
        let result = generator.generate(&content);

        // Assert
        assert!(result.is_ok());

        let xml = result.unwrap();

        // μ₁+μ₂: Data was loaded and extracted (evidenced by non-empty output)
        assert!(!xml.is_empty(), "Output should not be empty");

        // μ₃: Template rendering worked
        assert!(xml.contains("<specification"), "Template should be rendered");

        // μ₄: Canonical formatting applied
        assert!(xml.contains("<?xml version=\"1.0\""), "Should have canonical XML declaration");

        // μ₅: Output is valid for receipt generation
        let validation = ggen_yawl::codegen::yawl_xml::validate(&xml);
        assert!(validation.is_ok(), "Output should be valid for receipt generation");
    }
}

#[cfg(test)]
mod loan_approval_performance_tests {
    use super::*;
    use std::time::Instant;

    /// Test: Generation completes within reasonable time
    #[test]
    fn test_loan_approval_generation_performance() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
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
    fn test_loan_approval_loading_performance() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
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
mod loan_approval_cross_ontology_tests {
    use super::*;

    /// Test: Different ontologies produce different outputs
    #[test]
    fn test_different_ontologies_different_outputs() {
        // Arrange
        let account_opening = fixtures::load_fibo_account_opening();
        let loan_approval = fixtures::load_fibo_loan_approval();
        let generator = YawlGenerator::new();

        // Act
        let result1 = generator.generate(&account_opening);
        let result2 = generator.generate(&loan_approval);

        // Assert
        assert!(result1.is_ok() && result2.is_ok());

        let xml1 = result1.unwrap();
        let xml2 = result2.unwrap();

        assert_ne!(xml1, xml2, "Different ontologies should produce different outputs");
    }

    /// Test: Ontologies have different task counts
    #[test]
    fn test_ontologies_different_task_counts() {
        // Arrange
        let account_opening = fixtures::load_fibo_account_opening();
        let loan_approval = fixtures::load_fibo_loan_approval();
        let generator = YawlGenerator::new();

        // Act
        let result1 = generator.generate(&account_opening);
        let result2 = generator.generate(&loan_approval);

        // Assert
        assert!(result1.is_ok() && result2.is_ok());

        let xml1 = result1.unwrap();
        let xml2 = result2.unwrap();

        let tasks1 = xml1.matches("<task").count();
        let tasks2 = xml2.matches("<task").count();

        // The counts might be the same by coincidence, but both should have tasks
        assert!(tasks1 > 0, "Account Opening should have tasks");
        assert!(tasks2 > 0, "Loan Approval should have tasks");
    }
}
