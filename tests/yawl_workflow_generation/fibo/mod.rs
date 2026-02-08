//! FIBO Integration Tests for YAWL Workflow Generation
//!
//! This module tests the complete pipeline from real FIBO (Financial Industry
//! Business Ontology) ontologies to executable YAWL workflows.
//!
//! Test coverage:
//! - Loading real FIBO ontologies (AccountOpening, LoanApproval)
//! - Full pipeline execution (μ₁-μ₅)
//! - YAWL XML output validation
//! - Receipt generation and verification
//! - Determinism verification
//!
//! Tests follow Chicago TDD principles:
//! - AAA pattern (Arrange/Act/Assert)
//! - Real collaborators (no mocks)
//! - State-based verification
//! - Property-based testing where applicable

mod account_opening;
mod loan_approval;
mod pipeline;
mod receipts;
mod validation;

/// Common test utilities for FIBO integration tests
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
            return Err(format!("File does not appear to be valid Turtle format: {}", path.display()));
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
    ///
    /// This counts classes with rdfs:subClassOf pointing to the process class
    pub fn extract_expected_task_count(ontology: &str) -> usize {
        ontology
            .lines()
            .filter(|line| line.contains("rdfs:subClassOf") && line.contains("ProcessStep"))
            .count()
    }

    /// Extract expected flow count from FIBO ontology
    ///
    /// This counts object properties that represent flow relationships
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
            / 2  // Approximate: each property has ~2 lines
    }
}

#[cfg(test)]
mod verification_tests {
    use super::*;

    /// Test: Verify all fixture files exist
    #[test]
    fn test_all_fibo_fixtures_exist() {
        assert!(
            fixtures::fibo_account_opening_path().exists(),
            "FIBO Account Opening fixture should exist"
        );
        assert!(
            fixtures::fibo_loan_approval_path().exists(),
            "FIBO Loan Approval fixture should exist"
        );
    }

    /// Test: Verify fixture files are valid Turtle
    #[test]
    fn test_fibo_fixtures_are_valid() {
        fixtures::verify_ontology_file(&fixtures::fibo_account_opening_path())
            .expect("Account Opening ontology should be valid");
        fixtures::verify_ontology_file(&fixtures::fibo_loan_approval_path())
            .expect("Loan Approval ontology should be valid");
    }

    /// Test: Account Opening ontology has expected content
    #[test]
    fn test_account_opening_content() {
        let content = fixtures::load_fibo_account_opening();

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
        let content = fixtures::load_fibo_loan_approval();

        assert!(content.contains("LoanApprovalProcess"));
        assert!(content.contains("ReceiveApplication"));
        assert!(content.contains("CreditCheck"));
        assert!(content.contains("IncomeVerification"));
        assert!(content.contains("RiskAssessment"));
        assert!(content.contains("ApprovalDecision"));
        assert!(content.contains("@prefix"));
    }

    /// Test: Extract task count from Account Opening
    #[test]
    fn test_extract_account_opening_task_count() {
        let content = fixtures::load_fibo_account_opening();
        let task_count = fixtures::extract_expected_task_count(&content);

        // AccountOpening has: ValidateIdentity, CreditCheck, RiskAssessment,
        // ApproveAccount, VerifyDocuments, HighRiskProcess
        assert!(task_count >= 5, "Expected at least 5 tasks, got {}", task_count);
    }

    /// Test: Extract task count from Loan Approval
    #[test]
    fn test_extract_loan_approval_task_count() {
        let content = fixtures::load_fibo_loan_approval();
        let task_count = fixtures::extract_expected_task_count(&content);

        // LoanApproval has: ReceiveApplication, VerifyDocumentation, CreditCheck,
        // IncomeVerification, RiskAssessment, ApprovalDecision, LoanDisbursement,
        // LowRiskProcess, HighRiskProcess
        assert!(task_count >= 7, "Expected at least 7 tasks, got {}", task_count);
    }

    /// Test: Count triples in Account Opening
    #[test]
    fn test_count_account_opening_triples() {
        let content = fixtures::load_fibo_account_opening();
        let triple_count = fixtures::count_triples(&content);

        assert!(triple_count > 20, "Expected more than 20 triples, got {}", triple_count);
    }

    /// Test: Count triples in Loan Approval
    #[test]
    fn test_count_loan_approval_triples() {
        let content = fixtures::load_fibo_loan_approval();
        let triple_count = fixtures::count_triples(&content);

        assert!(triple_count > 30, "Expected more than 30 triples, got {}", triple_count);
    }
}
