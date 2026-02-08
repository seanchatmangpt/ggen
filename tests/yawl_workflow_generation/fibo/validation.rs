//! YAWL XML Output Validation Tests for FIBO Ontologies
//!
//! Comprehensive validation of generated YAWL XML including:
//! - Schema validation
//! - Well-formedness checks
//! - Required element presence
//! - Workflow structure validation
//! - Task and flow consistency

use super::fixtures;
use ggen_yawl::codegen::yawl_xml::validate;
use ggen_yawl::YawlGenerator;

/// YAWL XML validator with detailed checking
pub struct YawlValidator {
    xml: String,
}

impl YawlValidator {
    /// Create a new validator for the given XML
    pub fn new(xml: String) -> Self {
        Self { xml }
    }

    /// Check if XML is well-formed
    pub fn is_well_formed(&self) -> bool {
        // Basic well-formedness checks
        self.xml.contains("<?xml version=\"1.0\"")
            && self.xml.contains("<specification")
            && self.xml.contains("</specification>")
    }

    /// Check if required YAWL elements are present
    pub fn has_required_elements(&self) -> bool {
        self.xml.contains("<specification")
            && self.xml.contains("<decomposition")
            && (self.xml.contains("<name>") || self.xml.contains("name="))
    }

    /// Count tasks in the workflow
    pub fn task_count(&self) -> usize {
        self.xml.matches("<task").count()
    }

    /// Count flows in the workflow
    pub fn flow_count(&self) -> usize {
        self.xml.matches("<flow").count()
    }

    /// Check if workflow has input condition
    pub fn has_input_condition(&self) -> bool {
        self.xml.contains("<inputCondition") || self.xml.contains("<input")
    }

    /// Check if workflow has output condition
    pub fn has_output_condition(&self) -> bool {
        self.xml.contains("<outputCondition") || self.xml.contains("<output")
    }

    /// Validate task consistency
    pub fn validate_tasks(&self) -> Result<(), String> {
        let task_count = self.task_count();
        if task_count == 0 {
            return Err("Workflow must have at least one task".to_string());
        }

        // Check for split/join types
        if !self.xml.contains("<split") && !self.xml.contains("split=") {
            return Err("Tasks must have split types".to_string());
        }

        if !self.xml.contains("<join") && !self.xml.contains("join=") {
            return Err("Tasks must have join types".to_string());
        }

        Ok(())
    }

    /// Validate flow consistency
    pub fn validate_flows(&self) -> Result<(), String> {
        let flow_count = self.flow_count();

        // If we have flows, check their structure
        if flow_count > 0 {
            // Flows should have from and into attributes
            if !self.xml.contains("from=") || !self.xml.contains("into=") {
                return Err("Flows must have from and into attributes".to_string());
            }
        }

        Ok(())
    }

    /// Comprehensive validation
    pub fn validate_all(&self) -> Result<(), String> {
        // Well-formedness
        if !self.is_well_formed() {
            return Err("XML is not well-formed".to_string());
        }

        // Required elements
        if !self.has_required_elements() {
            return Err("Missing required YAWL elements".to_string());
        }

        // Tasks
        self.validate_tasks()?;

        // Flows
        self.validate_flows()?;

        // Use the built-in validator
        validate(&self.xml).map_err(|e| e.to_string())?;

        Ok(())
    }

    /// Get validation report as string
    pub fn validation_report(&self) -> String {
        let mut report = String::new();
        report.push_str("YAWL XML Validation Report\n");
        report.push_str("===========================\n\n");

        report.push_str(&format!("Well-formed: {}\n", self.is_well_formed()));
        report.push_str(&format!("Required elements: {}\n", self.has_required_elements()));
        report.push_str(&format!("Task count: {}\n", self.task_count()));
        report.push_str(&format!("Flow count: {}\n", self.flow_count()));
        report.push_str(&format!("Input condition: {}\n", self.has_input_condition()));
        report.push_str(&format!("Output condition: {}\n", self.has_output_condition()));

        report.push_str("\nDetailed validation:\n");
        match self.validate_all() {
            Ok(_) => report.push_str("✓ All validations passed"),
            Err(e) => report.push_str(&format!("✗ Validation failed: {}", e)),
        }

        report
    }
}

#[cfg(test)]
mod validation_basics_tests {
    use super::*;

    /// Test: Validate Account Opening output
    #[test]
    fn test_validate_account_opening_output() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new();

        // Act
        let result = generator.generate(&content);

        // Assert
        assert!(result.is_ok(), "Generation should succeed");
        let xml = result.unwrap();

        // Use built-in validation
        let validation_result = validate(&xml);
        assert!(validation_result.is_ok(), "Generated XML should be valid: {:?}",
                validation_result.err());
    }

    /// Test: Validate Loan Approval output
    #[test]
    fn test_validate_loan_approval_output() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
        let generator = YawlGenerator::new();

        // Act
        let result = generator.generate(&content);

        // Assert
        assert!(result.is_ok(), "Generation should succeed");
        let xml = result.unwrap();

        // Use built-in validation
        let validation_result = validate(&xml);
        assert!(validation_result.is_ok(), "Generated XML should be valid: {:?}",
                validation_result.err());
    }

    /// Test: Well-formedness check
    #[test]
    fn test_well_formedness_check() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new();
        let xml = generator.generate(&content).unwrap();

        // Act
        let validator = YawlValidator::new(xml);

        // Assert
        assert!(validator.is_well_formed(), "Generated XML should be well-formed");
    }

    /// Test: Required elements check
    #[test]
    fn test_required_elements_check() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
        let generator = YawlGenerator::new();
        let xml = generator.generate(&content).unwrap();

        // Act
        let validator = YawlValidator::new(xml);

        // Assert
        assert!(validator.has_required_elements(),
                "Generated XML should have required elements");
    }
}

#[cfg(test)]
mod validation_structure_tests {
    use super::*;

    /// Test: Account Opening has expected structure
    #[test]
    fn test_account_opening_structure() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new();
        let xml = generator.generate(&content).unwrap();
        let validator = YawlValidator::new(xml);

        // Act & Assert
        assert!(validator.task_count() > 0, "Should have tasks");
        assert!(validator.has_input_condition(), "Should have input condition");
        assert!(validator.has_output_condition(), "Should have output condition");
    }

    /// Test: Loan Approval has expected structure
    #[test]
    fn test_loan_approval_structure() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
        let generator = YawlGenerator::new();
        let xml = generator.generate(&content).unwrap();
        let validator = YawlValidator::new(xml);

        // Act & Assert
        assert!(validator.task_count() > 0, "Should have tasks");
        assert!(validator.has_input_condition(), "Should have input condition");
        assert!(validator.has_output_condition(), "Should have output condition");
    }

    /// Test: Task validation
    #[test]
    fn test_task_validation() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new();
        let xml = generator.generate(&content).unwrap();
        let validator = YawlValidator::new(xml);

        // Act
        let result = validator.validate_tasks();

        // Assert
        assert!(result.is_ok(), "Tasks should be valid: {:?}", result.err());
    }

    /// Test: Flow validation
    #[test]
    fn test_flow_validation() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
        let generator = YawlGenerator::new();
        let xml = generator.generate(&content).unwrap();
        let validator = YawlValidator::new(xml);

        // Act
        let result = validator.validate_flows();

        // Assert
        assert!(result.is_ok(), "Flows should be valid: {:?}", result.err());
    }

    /// Test: Comprehensive validation
    #[test]
    fn test_comprehensive_validation() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new();
        let xml = generator.generate(&content).unwrap();
        let validator = YawlValidator::new(xml);

        // Act
        let result = validator.validate_all();

        // Assert
        assert!(result.is_ok(), "Comprehensive validation should pass: {:?}", result.err());
    }
}

#[cfg(test)]
mod validation_comparison_tests {
    use super::*;

    /// Test: Both ontologies produce valid output
    #[test]
    fn test_both_ontologies_valid() {
        // Arrange
        let content1 = fixtures::load_fibo_account_opening();
        let content2 = fixtures::load_fibo_loan_approval();
        let generator = YawlGenerator::new();

        // Act
        let xml1 = generator.generate(&content1).unwrap();
        let xml2 = generator.generate(&content2).unwrap();

        // Assert
        let validator1 = YawlValidator::new(xml1.clone());
        let validator2 = YawlValidator::new(xml2.clone());

        assert!(validator1.validate_all().is_ok(), "Account Opening should be valid");
        assert!(validator2.validate_all().is_ok(), "Loan Approval should be valid");
    }

    /// Test: Different ontologies produce different structures
    #[test]
    fn test_different_ontologies_different_structures() {
        // Arrange
        let content1 = fixtures::load_fibo_account_opening();
        let content2 = fixtures::load_fibo_loan_approval();
        let generator = YawlGenerator::new();

        // Act
        let xml1 = generator.generate(&content1).unwrap();
        let xml2 = generator.generate(&content2).unwrap();

        // Assert - Structures may be similar but XML should differ
        assert_ne!(xml1, xml2, "Different ontologies should produce different XML");
    }
}

#[cfg(test)]
mod validation_report_tests {
    use super::*;

    /// Test: Generate validation report for Account Opening
    #[test]
    fn test_account_opening_validation_report() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new();
        let xml = generator.generate(&content).unwrap();
        let validator = YawlValidator::new(xml);

        // Act
        let report = validator.validation_report();

        // Assert
        assert!(report.contains("Well-formed:"), "Report should show well-formed status");
        assert!(report.contains("Task count:"), "Report should show task count");
        assert!(report.contains("Flow count:"), "Report should show flow count");
        assert!(report.contains("✓") || report.contains("✗"),
                "Report should show validation result");
    }

    /// Test: Generate validation report for Loan Approval
    #[test]
    fn test_loan_approval_validation_report() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
        let generator = YawlGenerator::new();
        let xml = generator.generate(&content).unwrap();
        let validator = YawlValidator::new(xml);

        // Act
        let report = validator.validation_report();

        // Assert
        assert!(report.contains("Validation Report"), "Report should have title");
        assert!(report.contains("YAWL XML"), "Report should mention YAWL");
    }
}

#[cfg(test)]
mod validation_edge_cases_tests {
    use super::*;

    /// Test: Empty string fails validation
    #[test]
    fn test_empty_string_fails_validation() {
        // Arrange
        let empty_xml = String::new();

        // Act
        let result = validate(&empty_xml);

        // Assert
        assert!(result.is_err(), "Empty string should fail validation");
    }

    /// Test: Invalid XML fails validation
    #[test]
    fn test_invalid_xml_fails_validation() {
        // Arrange
        let invalid_xml = "this is not valid xml at all";

        // Act
        let result = validate(invalid_xml);

        // Assert
        assert!(result.is_err(), "Invalid XML should fail validation");
    }

    /// Test: XML without declaration fails
    #[test]
    fn test_xml_without_declaration_fails() {
        // Arrange
        let xml_no_decl = "<specification></specification>";

        // Act
        let result = validate(xml_no_decl);

        // Assert
        assert!(result.is_err(), "XML without declaration should fail validation");
    }

    /// Test: XML with unclosed element fails
    #[test]
    fn test_unclosed_element_fails() {
        // Arrange
        let unclosed_xml = "<?xml version=\"1.0\"?><specification>";

        // Act
        let result = validate(unclosed_xml);

        // Assert
        assert!(result.is_err(), "Unclosed element should fail validation");
    }

    /// Test: Validator handles missing tasks gracefully
    #[test]
    fn test_validator_handles_missing_tasks() {
        // Arrange - Create minimal XML without tasks
        let minimal_xml = r#"<?xml version="1.0" encoding="UTF-8"?>
            <specification xmlns="http://www.yawlfoundation.org/yawlschema" version="2.0">
                <name>test</name>
                <decomposition id="test_net" type="WSNet">
                    <inputCondition id="input"/>
                    <outputCondition id="output"/>
                </decomposition>
            </specification>
        "#;

        let validator = YawlValidator::new(minimal_xml.to_string());

        // Act
        let result = validator.validate_tasks();

        // Assert
        assert!(result.is_err(), "Workflow without tasks should fail task validation");
    }
}

#[cfg(test)]
mod validation_consistency_tests {
    use super::*;

    /// Test: Validation is consistent across runs
    #[test]
    fn test_validation_consistency() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new();

        // Act - Generate twice and validate
        let xml1 = generator.generate(&content).unwrap();
        let xml2 = generator.generate(&content).unwrap();

        let result1 = validate(&xml1);
        let result2 = validate(&xml2);

        // Assert
        assert_eq!(result1.is_ok(), result2.is_ok(),
                   "Validation should be consistent");
    }

    /// Test: Task count is consistent
    #[test]
    fn test_task_count_consistency() {
        // Arrange
        let content = fixtures::load_fibo_loan_approval();
        let generator = YawlGenerator::new();

        // Act - Generate twice
        let xml1 = generator.generate(&content).unwrap();
        let xml2 = generator.generate(&content).unwrap();

        let validator1 = YawlValidator::new(xml1);
        let validator2 = YawlValidator::new(xml2);

        // Assert
        assert_eq!(validator1.task_count(), validator2.task_count(),
                   "Task count should be consistent");
    }

    /// Test: Validation report is consistent
    #[test]
    fn test_validation_report_consistency() {
        // Arrange
        let content = fixtures::load_fibo_account_opening();
        let generator = YawlGenerator::new();

        // Act - Generate twice
        let xml1 = generator.generate(&content).unwrap();
        let xml2 = generator.generate(&content).unwrap();

        let validator1 = YawlValidator::new(xml1);
        let validator2 = YawlValidator::new(xml2);

        // Assert
        let count1 = validator1.task_count();
        let count2 = validator2.task_count();

        assert_eq!(count1, count2, "Task count should match");
        assert_eq!(validator1.flow_count(), validator2.flow_count(),
                   "Flow count should match");
    }
}
