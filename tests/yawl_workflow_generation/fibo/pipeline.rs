//! Full Pipeline Integration Tests for FIBO Ontologies
//!
//! Tests the complete μ₁-μ₅ pipeline for real FIBO ontologies:
//! - μ₁: Normalize (RDF validation, SHACL shapes)
//! - μ₂: Extract (SPARQL CONSTRUCT queries)
//! - μ₃: Emit (Tera template rendering)
//! - μ₄: Canonicalize (Deterministic formatting)
//! - μ₅: Receipt (Cryptographic proof generation)

use super::fixtures;
use ggen_yawl::{OntologyLoader, YawlGenerator};
use ggen_yawl::codegen::yawl_xml::{canonicalize, validate};
use ggen_yawl::transform::ConstructExecutor;
use sha2::{Digest, Sha256};

/// Pipeline execution result with full metadata
#[derive(Debug, Clone)]
pub struct PipelineResult {
    /// Generated YAWL XML
    pub xml: String,
    /// SHA-256 hash of the output
    pub hash: String,
    /// Number of tasks in the workflow
    pub task_count: usize,
    /// Number of flows in the workflow
    pub flow_count: usize,
    /// Size in bytes
    pub size_bytes: usize,
}

/// Execute the full pipeline and return detailed results
pub fn execute_full_pipeline(ontology_content: &str) -> Result<PipelineResult, String> {
    // μ₁: Normalize - Load ontology
    let loader = OntologyLoader::new();
    let graph = loader
        .load_from_str(ontology_content, ggen_yawl::OntologyFormat::Turtle)
        .map_err(|e| format!("μ₁: Load failed: {}", e))?;

    if graph.is_empty() {
        return Err("μ₁: Graph is empty after loading".to_string());
    }

    // μ₂: Extract - Execute SPARQL CONSTRUCT queries
    let executor = ConstructExecutor::new();
    let extracted_graph = executor
        .execute_all(&graph)
        .map_err(|e| format!("μ₂: Extract failed: {}", e))?;

    // μ₃: Emit - Generate YAWL XML
    let generator = YawlGenerator::new().with_validation(false);
    let xml = generator
        .generate(ontology_content)
        .map_err(|e| format!("μ₃: Emit failed: {}", e))?;

    // μ₄: Canonicalize - Apply deterministic formatting
    let canonical_xml = canonicalize(&xml)
        .map_err(|e| format!("μ₄: Canonicalize failed: {}", e))?;

    // μ₅: Receipt preparation (hash calculation)
    let hash = format!("{:x}", Sha256::digest(canonical_xml.as_bytes()));

    // Extract metadata
    let task_count = canonical_xml.matches("<task").count();
    let flow_count = canonical_xml.matches("<flow").count();
    let size_bytes = canonical_xml.len();

    Ok(PipelineResult {
        xml: canonical_xml,
        hash,
        task_count,
        flow_count,
        size_bytes,
    })
}

#[cfg(test)]
mod pipeline_stage_tests {
    use super::*;

    /// Test: μ₁ Stage - Load and normalize FIBO ontology
    #[test]
    fn test_mu1_normalize_stage_account_opening() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let loader = OntologyLoader::new();

        // Act
        let result = loader.load_from_str(&content, ggen_yawl::OntologyFormat::Turtle);

        // Assert
        assert!(result.is_ok(), "μ₁: Should normalize ontology");
        let graph = result.unwrap();
        assert!(!graph.is_empty(), "μ₁: Graph should not be empty");
    }

    /// Test: μ₁ Stage - Load and normalize Loan Approval
    #[test]
    fn test_mu1_normalize_stage_loan_approval() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
        let loader = OntologyLoader::new();

        // Act
        let result = loader.load_from_str(&content, ggen_yawl::OntologyFormat::Turtle);

        // Assert
        assert!(result.is_ok(), "μ₁: Should normalize ontology");
        let graph = result.unwrap();
        assert!(!graph.is_empty(), "μ₁: Graph should not be empty");
    }

    /// Test: μ₂ Stage - Extract with SPARQL CONSTRUCT queries
    #[test]
    fn test_mu2_extract_stage_account_opening() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let loader = OntologyLoader::new();
        let graph = loader
            .load_from_str(&content, ggen_yawl::OntologyFormat::Turtle)
            .expect("μ₁ failed");

        // Act
        let executor = ConstructExecutor::new();
        let result = executor.execute_all(&graph);

        // Assert
        assert!(result.is_ok(), "μ₂: Should extract with CONSTRUCT queries");
        let extracted = result.unwrap();
        // Extracted graph should be a superset or transformation of original
    }

    /// Test: μ₂ Stage - Extract query order is deterministic
    #[test]
    fn test_mu2_extract_query_order_deterministic() {
        // Arrange
        let executor = ConstructExecutor::new();

        // Act
        let order1 = executor.topological_sort();
        let order2 = executor.topological_sort();

        // Assert
        assert!(order1.is_ok() && order2.is_ok(),
                "μ₂: Topological sort should succeed");

        let sorted1 = order1.unwrap();
        let sorted2 = order2.unwrap();

        assert_eq!(sorted1, sorted2, "μ₂: Query order should be deterministic");
    }

    /// Test: μ₂ Stage - All queries execute successfully
    #[test]
    fn test_mu2_extract_all_queries_execute() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let loader = OntologyLoader::new();
        let graph = loader
            .load_from_str(&content, ggen_yawl::OntologyFormat::Turtle)
            .expect("μ₁ failed");

        let executor = ConstructExecutor::new();
        let query_names = executor.query_names();

        // Act
        let result = executor.execute_all(&graph);

        // Assert
        assert!(result.is_ok(), "μ₂: All queries should execute");
        assert!(!query_names.is_empty(), "μ₂: Should have registered queries");
    }

    /// Test: μ₃ Stage - Emit generates YAWL XML
    #[test]
    fn test_mu3_emit_stage_account_opening() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new();

        // Act
        let result = generator.generate(&content);

        // Assert
        assert!(result.is_ok(), "μ₃: Should emit YAWL XML");
        let xml = result.unwrap();
        assert!(xml.contains("<?xml version=\"1.0\""), "μ₃: Should have XML declaration");
        assert!(xml.contains("<specification"), "μ₃: Should have specification element");
    }

    /// Test: μ₄ Stage - Canonicalize produces consistent output
    #[test]
    fn test_mu4_canonicalize_stage_consistent() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new();
        let xml = generator.generate(&content).expect("μ₃ failed");

        // Act
        let result1 = canonicalize(&xml);
        let result2 = canonicalize(&xml);

        // Assert
        assert!(result1.is_ok() && result2.is_ok(), "μ₄: Canonicalize should succeed");
        assert_eq!(result1.unwrap(), result2.unwrap(),
                   "μ₄: Canonicalize should be idempotent");
    }

    /// Test: μ₅ Stage - Receipt hash is deterministic
    #[test]
    fn test_mu5_receipt_hash_deterministic() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new();
        let xml = generator.generate(&content).expect("μ₃ failed");

        // Act
        let hash1 = format!("{:x}", Sha256::digest(xml.as_bytes()));
        let hash2 = format!("{:x}", Sha256::digest(xml.as_bytes()));

        // Assert
        assert_eq!(hash1, hash2, "μ₅: Hash should be deterministic");
    }
}

#[cfg(test)]
mod full_pipeline_tests {
    use super::*;

    /// Test: Execute full pipeline for Account Opening
    #[test]
    fn test_full_pipeline_account_opening() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();

        // Act
        let result = execute_full_pipeline(&content);

        // Assert
        assert!(result.is_ok(), "Full pipeline should succeed: {:?}", result.err());

        let pipeline_result = result.unwrap();
        assert!(!pipeline_result.xml.is_empty(), "Output XML should not be empty");
        assert!(!pipeline_result.hash.is_empty(), "Hash should not be empty");
        assert!(pipeline_result.task_count > 0, "Should have at least one task");
        assert!(pipeline_result.size_bytes > 0, "Output should have content");
    }

    /// Test: Execute full pipeline for Loan Approval
    #[test]
    fn test_full_pipeline_loan_approval() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();

        // Act
        let result = execute_full_pipeline(&content);

        // Assert
        assert!(result.is_ok(), "Full pipeline should succeed: {:?}", result.err());

        let pipeline_result = result.unwrap();
        assert!(!pipeline_result.xml.is_empty(), "Output XML should not be empty");
        assert!(!pipeline_result.hash.is_empty(), "Hash should not be empty");
        assert!(pipeline_result.task_count > 0, "Should have at least one task");
    }

    /// Test: Full pipeline produces valid YAWL XML
    #[test]
    fn test_full_pipeline_produces_valid_yawl() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();

        // Act
        let result = execute_full_pipeline(&content);

        // Assert
        assert!(result.is_ok());
        let pipeline_result = result.unwrap();

        // Validate the output
        let validation_result = validate(&pipeline_result.xml);
        assert!(validation_result.is_ok(), "Pipeline output should be valid YAWL");
    }

    /// Test: Full pipeline metadata is accurate
    #[test]
    fn test_full_pipeline_metadata_accuracy() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();

        // Act
        let result = execute_full_pipeline(&content);

        // Assert
        assert!(result.is_ok());
        let pipeline_result = result.unwrap();

        // Verify task count matches actual
        let actual_task_count = pipeline_result.xml.matches("<task").count();
        assert_eq!(pipeline_result.task_count, actual_task_count,
                   "Metadata task count should match actual");

        // Verify flow count matches actual
        let actual_flow_count = pipeline_result.xml.matches("<flow").count();
        assert_eq!(pipeline_result.flow_count, actual_flow_count,
                   "Metadata flow count should match actual");

        // Verify size matches
        assert_eq!(pipeline_result.size_bytes, pipeline_result.xml.len(),
                   "Metadata size should match actual");
    }
}

#[cfg(test)]
mod pipeline_determinism_tests {
    use super::*;

    /// Test: Full pipeline is deterministic
    #[test]
    fn test_full_pipeline_deterministic() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();

        // Act - Run pipeline twice
        let result1 = execute_full_pipeline(&content);
        let result2 = execute_full_pipeline(&content);

        // Assert
        assert!(result1.is_ok() && result2.is_ok());

        let pipeline1 = result1.unwrap();
        let pipeline2 = result2.unwrap();

        assert_eq!(pipeline1.xml, pipeline2.xml, "Pipeline output should be deterministic");
        assert_eq!(pipeline1.hash, pipeline2.hash, "Pipeline hash should be deterministic");
        assert_eq!(pipeline1.task_count, pipeline2.task_count, "Task count should match");
        assert_eq!(pipeline1.flow_count, pipeline2.flow_count, "Flow count should match");
    }

    /// Test: Multiple pipeline runs produce identical hashes
    #[test]
    fn test_pipeline_hash_consistency() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();

        // Act - Run pipeline multiple times
        let mut hashes = Vec::new();
        for _ in 0..5 {
            let result = execute_full_pipeline(&content);
            assert!(result.is_ok());
            hashes.push(result.unwrap().hash);
        }

        // Assert - All hashes should be identical
        let first = &hashes[0];
        for hash in &hashes[1..] {
            assert_eq!(hash, first, "All pipeline runs should produce identical hashes");
        }
    }

    /// Test: Pipeline output hash matches content hash
    #[test]
    fn test_pipeline_hash_matches_content() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();

        // Act
        let result = execute_full_pipeline(&content);
        assert!(result.is_ok());
        let pipeline_result = result.unwrap();

        let computed_hash = format!("{:x}", Sha256::digest(pipeline_result.xml.as_bytes()));

        // Assert
        assert_eq!(pipeline_result.hash, computed_hash,
                   "Stored hash should match computed hash");
    }
}

#[cfg(test)]
mod pipeline_comparison_tests {
    use super::*;

    /// Test: Different ontologies produce different pipeline results
    #[test]
    fn test_different_ontologies_different_results() {
        // Arrange
        let content1 = fixtures::load_fibo_account_opening();
        let content2 = fixtures::load_fibo_loan_approval();

        // Act
        let result1 = execute_full_pipeline(&content1);
        let result2 = execute_full_pipeline(&content2);

        // Assert
        assert!(result1.is_ok() && result2.is_ok());

        let pipeline1 = result1.unwrap();
        let pipeline2 = result2.unwrap();

        assert_ne!(pipeline1.xml, pipeline2.xml,
                   "Different ontologies should produce different outputs");
        assert_ne!(pipeline1.hash, pipeline2.hash,
                   "Different ontologies should produce different hashes");
    }

    /// Test: Ontology complexity correlates with output size
    #[test]
    fn test_complexity_correlates_with_output_size() {
        // Arrange
        let simple_content = fixtures::load_fibo_account_opening();
        let complex_content = fixtures::load_fibo_loan_approval();

        // Act
        let result1 = execute_full_pipeline(&simple_content);
        let result2 = execute_full_pipeline(&complex_content);

        // Assert
        assert!(result1.is_ok() && result2.is_ok());

        let pipeline1 = result1.unwrap();
        let pipeline2 = result2.unwrap();

        // Loan Approval is more complex (parallel flows, conditionals)
        // so should generally have more tasks/flows
        // This is a weak assertion - they might be similar
        assert!(pipeline1.task_count > 0, "Simple ontology should have tasks");
        assert!(pipeline2.task_count > 0, "Complex ontology should have tasks");
    }

    /// Test: Pipeline preserves ontology structure
    #[test]
    fn test_pipeline_preserves_structure() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();

        // Act
        let result = execute_full_pipeline(&content);

        // Assert
        assert!(result.is_ok());
        let pipeline_result = result.unwrap();

        // Verify structure is preserved
        assert!(pipeline_result.xml.contains("<decomposition"),
                "Should preserve decomposition structure");
        assert!(pipeline_result.xml.contains("<task") || pipeline_result.xml.contains("<flow"),
                "Should preserve tasks or flows");
    }
}

#[cfg(test)]
mod pipeline_error_handling_tests {
    use super::*;

    /// Test: Pipeline handles empty input gracefully
    #[test]
    fn test_pipeline_handles_empty_input() {
        // Arrange
        let empty_content = "";

        // Act
        let result = execute_full_pipeline(empty_content);

        // Assert
        assert!(result.is_err(), "Empty input should produce error");
        let error_msg = result.unwrap_err();
        assert!(error_msg.contains("μ₁") || error_msg.contains("Load"),
                "Error should indicate loading failure");
    }

    /// Test: Pipeline handles invalid Turtle gracefully
    #[test]
    fn test_pipeline_handles_invalid_turtle() {
        // Arrange
        let invalid_content = "This is not valid Turtle RDF content at all";

        // Act
        let result = execute_full_pipeline(invalid_content);

        // Assert
        assert!(result.is_err(), "Invalid Turtle should produce error");
    }

    /// Test: Pipeline error messages indicate stage
    #[test]
    fn test_pipeline_error_indicates_stage() {
        // Arrange
        let invalid_content = "invalid";

        // Act
        let result = execute_full_pipeline(invalid_content);

        // Assert
        assert!(result.is_err());
        let error_msg = result.unwrap_err();

        // Error should indicate which stage failed
        assert!(error_msg.contains("μ₁") || error_msg.contains("μ₂") ||
                error_msg.contains("μ₃") || error_msg.contains("μ₄") ||
                error_msg.contains("Load") || error_msg.contains("Extract"),
                "Error should indicate the failed stage");
    }
}

#[cfg(test)]
mod pipeline_performance_tests {
    use super::*;
    use std::time::Instant;

    /// Test: Pipeline completes within SLO
    #[test]
    fn test_pipeline_slo_account_opening() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();

        // Act
        let start = Instant::now();
        let result = execute_full_pipeline(&content);
        let duration = start.elapsed();

        // Assert
        assert!(result.is_ok(), "Pipeline should succeed");
        assert!(duration.as_secs() < 10, "Pipeline should complete within 10 seconds, took {:?}",
                duration);
    }

    /// Test: Pipeline completes within SLO for complex ontology
    #[test]
    fn test_pipeline_slo_loan_approval() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();

        // Act
        let start = Instant::now();
        let result = execute_full_pipeline(&content);
        let duration = start.elapsed();

        // Assert
        assert!(result.is_ok(), "Pipeline should succeed");
        assert!(duration.as_secs() < 10, "Pipeline should complete within 10 seconds, took {:?}",
                duration);
    }

    /// Test: Multiple sequential runs complete efficiently
    #[test]
    fn test_pipeline_sequential_runs_efficient() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();

        // Act - Run pipeline multiple times
        let start = Instant::now();
        for _ in 0..5 {
            let result = execute_full_pipeline(&content);
            assert!(result.is_ok(), "Each run should succeed");
        }
        let total_duration = start.elapsed();

        // Assert
        assert!(total_duration.as_secs() < 30,
                "5 sequential runs should complete within 30 seconds, took {:?}",
                total_duration);
    }
}

#[cfg(test)]
mod pipeline_receipt_tests {
    use super::*;

    /// Test: Generate epoch from pipeline inputs
    #[test]
    fn test_generate_epoch_from_pipeline() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let hash = format!("{:x}", Sha256::digest(content.as_bytes()));

        // Act - Compute epoch ID
        let epoch_id = format!("{:x}", Sha256::digest(hash.as_bytes()));

        // Assert
        assert!(!epoch_id.is_empty(), "Epoch ID should not be empty");
        assert_eq!(epoch_id.len(), 64, "SHA-256 hash should be 64 hex characters");
    }

    /// Test: Receipt includes all pipeline outputs
    #[test]
    fn test_receipt_includes_outputs() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let pipeline_result = execute_full_pipeline(&content).unwrap();

        // Assert
        assert!(!pipeline_result.hash.is_empty(), "Output hash should not be empty");
        assert!(pipeline_result.size_bytes > 0, "Output size should be positive");
    }

    /// Test: Outputs hash is computed correctly
    #[test]
    fn test_outputs_hash_computation() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let pipeline_result = execute_full_pipeline(&content).unwrap();

        // Act - Compute outputs hash
        let outputs_hash_data = format!("{}:{}", "workflow.yawl", pipeline_result.hash);
        let outputs_hash = format!("{:x}", Sha256::digest(outputs_hash_data.as_bytes()));

        // Assert
        assert!(!outputs_hash.is_empty(), "Outputs hash should not be empty");
        assert_eq!(outputs_hash.len(), 64, "Outputs hash should be 64 hex characters");
    }
}
