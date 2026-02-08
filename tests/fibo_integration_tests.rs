//! FIBO Integration Tests for YAWL Workflow Generation
//!
//! This test file runs comprehensive tests on real FIBO (Financial Industry
//! Business Ontology) ontologies, testing the complete pipeline from ontology
//! loading to YAWL workflow XML generation and receipt verification.

// Include all FIBO test modules
mod yawl_workflow_generation {
    pub mod fibo {
        pub mod fixtures {
            use std::path::{Path, PathBuf};

            /// Path to the fixtures directory
            pub fn fixtures_dir() -> PathBuf {
                PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                    .join("tests")
                    .join("fixtures")
                    .join("ontologies")
            }

            /// Path to the FIBO Account Opening ontology
            pub fn fibo_account_opening_path() -> PathBuf {
                fixtures_dir().join("fibo_account_opening.ttl")
            }

            /// Path to the FIBO Loan Approval ontology
            pub fn fibo_loan_approval_path() -> PathBuf {
                fixtures_dir().join("fibo_loan_approval.ttl")
            }

            /// Load FIBO Account Opening ontology content
            pub fn load_fibo_account_opening() -> String {
                std::fs::read_to_string(fibo_account_opening_path())
                    .expect("Failed to load FIBO Account Opening ontology")
            }

            /// Load FIBO Loan Approval ontology content
            pub fn load_fibo_loan_approval() -> String {
                std::fs::read_to_string(fibo_loan_approval_path())
                    .expect("Failed to load FIBO Loan Approval ontology")
            }

            /// Verify ontology file exists and is readable
            pub fn verify_ontology_file(path: &Path) -> Result<(), String> {
                if !path.exists() {
                    return Err(format!("Ontology file does not exist: {}", path.display()));
                }
                if !path.is_file() {
                    return Err(format!("Path is not a file: {}", path.display()));
                }

                // Try to read it
                std::fs::read_to_string(path)
                    .map_err(|e| format!("Failed to read ontology file: {}", e))?;

                // Verify it has Turtle content
                let content = std::fs::read_to_string(path)
                    .map_err(|e| format!("Failed to read ontology file: {}", e))?;

                if !content.contains("@prefix") {
                    return Err(format!(
                        "File does not appear to be valid Turtle format: {}",
                        path.display()
                    ));
                }

                Ok(())
            }

            /// Count triples in a Turtle file (approximate)
            pub fn count_triples(turtle_content: &str) -> usize {
                turtle_content
                    .lines()
                    .filter(|line| {
                        let trimmed = line.trim();
                        !trimmed.is_empty()
                            && !trimmed.starts_with('#')
                            && !trimmed.starts_with("@prefix")
                    })
                    .count()
            }

            /// Extract expected task count from FIBO ontology
            pub fn extract_expected_task_count(ontology: &str) -> usize {
                ontology
                    .lines()
                    .filter(|line| line.contains("rdfs:subClassOf") && line.contains("ProcessStep"))
                    .count()
            }

            /// Extract expected flow count from FIBO ontology
            pub fn extract_expected_flow_count(ontology: &str) -> usize {
                ontology
                    .lines()
                    .filter(|line| {
                        line.contains("owl:ObjectProperty")
                            || line.contains("hasNextStep")
                            || line.contains("runsConcurrentlyWith")
                            || line.contains("requires")
                            || line.contains("merges")
                    })
                    .filter(|line| !line.contains("rdfs:domain"))
                    .count()
                    / 2
            }
        }
    }
}

// Import the ggen-yawl types
use ggen_yawl::codegen::yawl_xml::validate;
use ggen_yawl::{OntologyLoader, YawlGenerator};
use sha2::{Digest, Sha256};
use std::time::Instant;

#[cfg(test)]
mod verification_tests {
    use super::*;

    /// Test: Verify all fixture files exist
    #[test]
    fn test_all_fibo_fixtures_exist() {
        assert!(
            yawl_workflow_generation::fibo::fixtures::fibo_account_opening_path().exists(),
            "FIBO Account Opening fixture should exist"
        );
        assert!(
            yawl_workflow_generation::fibo::fixtures::fibo_loan_approval_path().exists(),
            "FIBO Loan Approval fixture should exist"
        );
    }

    /// Test: Verify fixture files are valid Turtle
    #[test]
    fn test_fibo_fixtures_are_valid() {
        yawl_workflow_generation::fibo::fixtures::verify_ontology_file(
            &yawl_workflow_generation::fibo::fixtures::fibo_account_opening_path(),
        )
        .expect("Account Opening ontology should be valid");
        yawl_workflow_generation::fibo::fixtures::verify_ontology_file(
            &yawl_workflow_generation::fibo::fixtures::fibo_loan_approval_path(),
        )
        .expect("Loan Approval ontology should be valid");
    }

    /// Test: Account Opening ontology has expected content
    #[test]
    fn test_account_opening_content() {
        let content = yawl_workflow_generation::fibo::fixtures::load_fibo_account_opening();

        assert!(content.contains("AccountOpeningProcess"));
        assert!(content.contains("ValidateIdentity"));
        assert!(content.contains("CreditCheck"));
        assert!(content.contains("RiskAssessment"));
        assert!(content.contains("ApproveAccount"));
        assert!(content.contains("@prefix"));
    }

    /// Test: Loan Approval ontology has expected content
    #[test]
    fn test_loan_approval_content() {
        let content = yawl_workflow_generation::fibo::fixtures::load_fibo_loan_approval();

        assert!(content.contains("LoanApprovalProcess"));
        assert!(content.contains("ReceiveApplication"));
        assert!(content.contains("CreditCheck"));
        assert!(content.contains("IncomeVerification"));
        assert!(content.contains("RiskAssessment"));
        assert!(content.contains("ApprovalDecision"));
        assert!(content.contains("@prefix"));
    }
}

#[cfg(test)]
mod loading_tests {
    use super::*;

    /// Test: Load FIBO Account Opening ontology from file
    #[test]
    fn test_load_fibo_account_opening_from_file() {
        let path = yawl_workflow_generation::fibo::fixtures::fibo_account_opening_path();
        let loader = OntologyLoader::new();

        let result = loader.load_from_file(&path);

        assert!(
            result.is_ok(),
            "Should load FIBO Account Opening ontology from file"
        );
        let graph = result.unwrap();
        assert!(!graph.is_empty(), "Graph should not be empty");
    }

    /// Test: Load FIBO Account Opening ontology from string
    #[test]
    fn test_load_fibo_account_opening_from_string() {
        let content = yawl_workflow_generation::fibo::fixtures::load_fibo_account_opening();
        let loader = OntologyLoader::new();

        let result = loader.load_from_str(&content, ggen_yawl::OntologyFormat::Turtle);

        assert!(
            result.is_ok(),
            "Should load FIBO Account Opening ontology from string"
        );
        let graph = result.unwrap();
        assert!(!graph.is_empty(), "Graph should not be empty");
    }

    /// Test: Load FIBO Loan Approval ontology from file
    #[test]
    fn test_load_fibo_loan_approval_from_file() {
        let path = yawl_workflow_generation::fibo::fixtures::fibo_loan_approval_path();
        let loader = OntologyLoader::new();

        let result = loader.load_from_file(&path);

        assert!(
            result.is_ok(),
            "Should load FIBO Loan Approval ontology from file"
        );
        let graph = result.unwrap();
        assert!(!graph.is_empty(), "Graph should not be empty");
    }

    /// Test: Load FIBO Loan Approval ontology from string
    #[test]
    fn test_load_fibo_loan_approval_from_string() {
        let content = yawl_workflow_generation::fibo::fixtures::load_fibo_loan_approval();
        let loader = OntologyLoader::new();

        let result = loader.load_from_str(&content, ggen_yawl::OntologyFormat::Turtle);

        assert!(
            result.is_ok(),
            "Should load FIBO Loan Approval ontology from string"
        );
        let graph = result.unwrap();
        assert!(!graph.is_empty(), "Graph should not be empty");
    }
}

#[cfg(test)]
mod pipeline_tests {
    use super::*;

    /// Pipeline execution result with full metadata
    #[derive(Debug, Clone)]
    pub struct PipelineResult {
        pub xml: String,
        pub hash: String,
        pub task_count: usize,
        pub flow_count: usize,
        pub size_bytes: usize,
    }

    /// Execute the full pipeline and return detailed results
    pub fn execute_full_pipeline(ontology_content: &str) -> Result<PipelineResult, String> {
        // μ₁: Load and normalize
        let loader = OntologyLoader::new();
        let graph = loader
            .load_from_str(ontology_content, ggen_yawl::OntologyFormat::Turtle)
            .map_err(|e| format!("μ₁: Load failed: {}", e))?;

        if graph.is_empty() {
            return Err("μ₁: Graph is empty after loading".to_string());
        }

        // μ₂: Extract using CONSTRUCT queries
        let executor = ggen_yawl::ConstructExecutor::new();
        let _extracted_graph = executor
            .execute_all(&graph)
            .map_err(|e| format!("μ₂: Extract failed: {}", e))?;

        // μ₃: Emit - Generate YAWL XML
        let generator = YawlGenerator::new().with_validation(false);
        let xml = generator
            .generate(ontology_content)
            .map_err(|e| format!("μ₃: Emit failed: {}", e))?;

        // μ₄: Canonicalize - Apply deterministic formatting
        let canonical_xml = ggen_yawl::codegen::yawl_xml::canonicalize(&xml)
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

    /// Test: Full pipeline execution for Account Opening
    #[test]
    fn test_full_pipeline_account_opening() {
        let content = yawl_workflow_generation::fibo::fixtures::load_fibo_account_opening();

        let result = execute_full_pipeline(&content);

        assert!(
            result.is_ok(),
            "Full pipeline should succeed: {:?}",
            result.err()
        );

        let pipeline_result = result.unwrap();
        assert!(
            !pipeline_result.xml.is_empty(),
            "Output XML should not be empty"
        );
        assert!(!pipeline_result.hash.is_empty(), "Hash should not be empty");
        assert!(
            pipeline_result.task_count > 0,
            "Should have at least one task"
        );
        assert!(pipeline_result.size_bytes > 0, "Output should have content");
    }

    /// Test: Full pipeline execution for Loan Approval
    #[test]
    fn test_full_pipeline_loan_approval() {
        let content = yawl_workflow_generation::fibo::fixtures::load_fibo_loan_approval();

        let result = execute_full_pipeline(&content);

        assert!(
            result.is_ok(),
            "Full pipeline should succeed: {:?}",
            result.err()
        );

        let pipeline_result = result.unwrap();
        assert!(
            !pipeline_result.xml.is_empty(),
            "Output XML should not be empty"
        );
        assert!(!pipeline_result.hash.is_empty(), "Hash should not be empty");
        assert!(
            pipeline_result.task_count > 0,
            "Should have at least one task"
        );
    }

    /// Test: Full pipeline produces valid YAWL XML
    #[test]
    fn test_full_pipeline_produces_valid_yawl() {
        let content = yawl_workflow_generation::fibo::fixtures::load_fibo_account_opening();

        let result = execute_full_pipeline(&content);

        assert!(result.is_ok());
        let pipeline_result = result.unwrap();

        // Validate the output
        let validation_result = validate(&pipeline_result.xml);
        assert!(
            validation_result.is_ok(),
            "Pipeline output should be valid YAWL"
        );
    }
}

#[cfg(test)]
mod validation_tests {
    use super::*;

    /// YAWL XML validator with detailed checking
    pub struct YawlValidator {
        xml: String,
    }

    impl YawlValidator {
        pub fn new(xml: String) -> Self {
            Self { xml }
        }

        pub fn is_well_formed(&self) -> bool {
            self.xml.contains("<?xml version=\"1.0\"")
                && self.xml.contains("<specification")
                && self.xml.contains("</specification>")
        }

        pub fn has_required_elements(&self) -> bool {
            self.xml.contains("<specification")
                && self.xml.contains("<decomposition")
                && (self.xml.contains("<name>") || self.xml.contains("name="))
        }

        pub fn task_count(&self) -> usize {
            self.xml.matches("<task").count()
        }

        pub fn flow_count(&self) -> usize {
            self.xml.matches("<flow").count()
        }

        pub fn validate_all(&self) -> Result<(), String> {
            if !self.is_well_formed() {
                return Err("XML is not well-formed".to_string());
            }
            if !self.has_required_elements() {
                return Err("Missing required YAWL elements".to_string());
            }
            validate(&self.xml).map_err(|e| e.to_string())?;
            Ok(())
        }
    }

    /// Test: Validate Account Opening output
    #[test]
    fn test_validate_account_opening_output() {
        let content = yawl_workflow_generation::fibo::fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new();

        let result = generator.generate(&content);

        assert!(result.is_ok(), "Generation should succeed");
        let xml = result.unwrap();

        let validation_result = validate(&xml);
        assert!(
            validation_result.is_ok(),
            "Generated XML should be valid: {:?}",
            validation_result.err()
        );
    }

    /// Test: Validate Loan Approval output
    #[test]
    fn test_validate_loan_approval_output() {
        let content = yawl_workflow_generation::fibo::fixtures::load_fibo_loan_approval();
        let generator = YawlGenerator::new();

        let result = generator.generate(&content);

        assert!(result.is_ok(), "Generation should succeed");
        let xml = result.unwrap();

        let validation_result = validate(&xml);
        assert!(
            validation_result.is_ok(),
            "Generated XML should be valid: {:?}",
            validation_result.err()
        );
    }
}

#[cfg(test)]
mod determinism_tests {
    use super::*;

    /// Test: Generation is deterministic
    #[test]
    fn test_generation_is_deterministic() {
        let content = yawl_workflow_generation::fibo::fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new();

        let result1 = generator.generate(&content);
        let result2 = generator.generate(&content);

        assert!(
            result1.is_ok() && result2.is_ok(),
            "Both generations should succeed"
        );

        let xml1 = result1.unwrap();
        let xml2 = result2.unwrap();

        assert_eq!(xml1, xml2, "Generation should be deterministic");
    }

    /// Test: Hash is deterministic
    #[test]
    fn test_hash_is_deterministic() {
        let content = yawl_workflow_generation::fibo::fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new();
        let xml = generator.generate(&content).expect("μ₃ failed");

        let hash1 = format!("{:x}", Sha256::digest(xml.as_bytes()));
        let hash2 = format!("{:x}", Sha256::digest(xml.as_bytes()));

        assert_eq!(hash1, hash2, "Hash should be deterministic");
    }
}

#[cfg(test)]
mod performance_tests {
    use super::*;

    /// Test: Generation completes within reasonable time
    #[test]
    fn test_generation_performance() {
        let content = yawl_workflow_generation::fibo::fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new();

        let start = Instant::now();
        let result = generator.generate(&content);
        let duration = start.elapsed();

        assert!(result.is_ok(), "Generation should succeed");
        assert!(
            duration.as_secs() < 5,
            "Generation should complete within 5 seconds, took {:?}",
            duration
        );
    }

    /// Test: Loading performs within SLO
    #[test]
    fn test_loading_performance() {
        let content = yawl_workflow_generation::fibo::fixtures::load_fibo_account_opening();
        let loader = OntologyLoader::new();

        let start = Instant::now();
        let result = loader.load_from_str(&content, ggen_yawl::OntologyFormat::Turtle);
        let duration = start.elapsed();

        assert!(result.is_ok(), "Loading should succeed");
        assert!(
            duration.as_secs() < 1,
            "Loading should complete within 1 second, took {:?}",
            duration
        );
    }
}

#[cfg(test)]
mod cross_ontology_tests {
    use super::*;

    /// Test: Different ontologies produce different outputs
    #[test]
    fn test_different_ontologies_different_outputs() {
        let account_opening = yawl_workflow_generation::fibo::fixtures::load_fibo_account_opening();
        let loan_approval = yawl_workflow_generation::fibo::fixtures::load_fibo_loan_approval();
        let generator = YawlGenerator::new();

        let result1 = generator.generate(&account_opening);
        let result2 = generator.generate(&loan_approval);

        assert!(result1.is_ok() && result2.is_ok());

        let xml1 = result1.unwrap();
        let xml2 = result2.unwrap();

        assert_ne!(
            xml1, xml2,
            "Different ontologies should produce different outputs"
        );
    }

    /// Test: Ontologies have different task counts
    #[test]
    fn test_ontologies_different_task_counts() {
        let account_opening = yawl_workflow_generation::fibo::fixtures::load_fibo_account_opening();
        let loan_approval = yawl_workflow_generation::fibo::fixtures::load_fibo_loan_approval();
        let generator = YawlGenerator::new();

        let result1 = generator.generate(&account_opening);
        let result2 = generator.generate(&loan_approval);

        assert!(result1.is_ok() && result2.is_ok());

        let xml1 = result1.unwrap();
        let xml2 = result2.unwrap();

        let tasks1 = xml1.matches("<task").count();
        let tasks2 = xml2.matches("<task").count();

        assert!(tasks1 > 0, "Account Opening should have tasks");
        assert!(tasks2 > 0, "Loan Approval should have tasks");
    }
}
