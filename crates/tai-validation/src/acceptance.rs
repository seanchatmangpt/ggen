//! Acceptance criteria validation and requirements traceability

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Acceptance criteria from RDF specifications
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AcceptanceCriteria {
    /// Criteria ID
    pub id: String,
    /// Requirement/user story ID
    pub requirement_id: String,
    /// Criteria description
    pub description: String,
    /// Acceptance scenario
    pub scenario: String,
    /// Test case ID that validates this
    pub test_case_id: Option<String>,
    /// Is satisfied
    pub satisfied: bool,
}

impl AcceptanceCriteria {
    /// Create new criteria
    pub fn new(id: String, requirement_id: String, description: String, scenario: String) -> Self {
        Self {
            id,
            requirement_id,
            description,
            scenario,
            test_case_id: None,
            satisfied: false,
        }
    }

    /// Link test case
    pub fn with_test_case(mut self, test_id: String) -> Self {
        self.test_case_id = Some(test_id);
        self.satisfied = true;
        self
    }

    /// Mark as satisfied
    pub fn mark_satisfied(mut self) -> Self {
        self.satisfied = true;
        self
    }
}

/// Acceptance criteria validator
pub struct CriteriaValidator {
    /// All acceptance criteria
    criteria: Vec<AcceptanceCriteria>,
    /// Requirement to test mappings
    mappings: HashMap<String, Vec<String>>,
}

impl CriteriaValidator {
    /// Create new validator
    pub fn new() -> Self {
        Self {
            criteria: Vec::new(),
            mappings: HashMap::new(),
        }
    }

    /// Add acceptance criteria
    pub fn add_criteria(&mut self, criteria: AcceptanceCriteria) {
        self.criteria.push(criteria);
    }

    /// Map requirement to test cases
    pub fn map_requirement_to_tests(&mut self, requirement_id: String, test_ids: Vec<String>) {
        self.mappings.insert(requirement_id, test_ids);
    }

    /// Parse acceptance criteria from RDF file (stub)
    pub async fn parse_from_rdf(&mut self, _rdf_file: &str) -> crate::error::Result<usize> {
        // In production, this would parse RDF/TTL files from .specify/ directory
        // and extract user stories and acceptance scenarios
        Ok(0)
    }

    /// Validate all criteria have test coverage
    pub fn validate_test_coverage(&self) -> ValidationReport {
        let mut uncovered_count = 0;
        let mut covered = 0;

        for criteria in &self.criteria {
            if criteria.test_case_id.is_some() {
                covered += 1;
            } else {
                uncovered_count += 1;
            }
        }

        ValidationReport {
            total_criteria: self.criteria.len(),
            covered_criteria: covered,
            uncovered_criteria: uncovered_count,
            coverage_percent: if self.criteria.is_empty() {
                0.0
            } else {
                (covered as f64 / self.criteria.len() as f64) * 100.0
            },
        }
    }

    /// Generate traceability matrix
    pub fn generate_traceability_matrix(&self) -> TraceabilityMatrix {
        let mut matrix = TraceabilityMatrix::new();

        for criteria in &self.criteria {
            let row = TraceabilityRow {
                requirement_id: criteria.requirement_id.clone(),
                criteria_id: criteria.id.clone(),
                test_case_id: criteria.test_case_id.clone(),
                status: if criteria.satisfied {
                    "Satisfied".to_string()
                } else {
                    "Not Satisfied".to_string()
                },
            };
            matrix.rows.push(row);
        }

        matrix
    }

    /// Get all criteria
    pub fn criteria(&self) -> &[AcceptanceCriteria] {
        &self.criteria
    }
}

impl Default for CriteriaValidator {
    fn default() -> Self {
        Self::new()
    }
}

/// Test coverage validation report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationReport {
    /// Total criteria
    pub total_criteria: usize,
    /// Covered by tests
    pub covered_criteria: usize,
    /// Not covered by tests
    pub uncovered_criteria: usize,
    /// Coverage percentage
    pub coverage_percent: f64,
}

impl ValidationReport {
    /// Is all criteria covered
    pub fn is_complete(&self) -> bool {
        self.uncovered_criteria == 0
    }
}

/// Requirements-to-test traceability matrix
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraceabilityMatrix {
    /// Traceability rows
    pub rows: Vec<TraceabilityRow>,
}

impl TraceabilityMatrix {
    /// Create new matrix
    pub fn new() -> Self {
        Self { rows: Vec::new() }
    }

    /// Get coverage
    pub fn coverage_percent(&self) -> f64 {
        if self.rows.is_empty() {
            return 0.0;
        }

        let covered = self
            .rows
            .iter()
            .filter(|r| r.test_case_id.is_some())
            .count();
        (covered as f64 / self.rows.len() as f64) * 100.0
    }
}

impl Default for TraceabilityMatrix {
    fn default() -> Self {
        Self::new()
    }
}

/// Single row in traceability matrix
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraceabilityRow {
    /// Requirement ID
    pub requirement_id: String,
    /// Criteria ID
    pub criteria_id: String,
    /// Test case ID (if any)
    pub test_case_id: Option<String>,
    /// Status
    pub status: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_criteria_creation() {
        let criteria = AcceptanceCriteria::new(
            "AC-1".to_string(),
            "REQ-1".to_string(),
            "User can login".to_string(),
            "Given user with valid credentials".to_string(),
        );
        assert_eq!(criteria.id, "AC-1");
        assert!(!criteria.satisfied);
    }

    #[test]
    fn test_criteria_with_test_case() {
        let criteria = AcceptanceCriteria::new(
            "AC-1".to_string(),
            "REQ-1".to_string(),
            "User can login".to_string(),
            "Given user with valid credentials".to_string(),
        )
        .with_test_case("TEST-1".to_string());
        assert!(criteria.satisfied);
        assert_eq!(criteria.test_case_id, Some("TEST-1".to_string()));
    }

    #[test]
    fn test_validator_add_criteria() {
        let mut validator = CriteriaValidator::new();
        let criteria = AcceptanceCriteria::new(
            "AC-1".to_string(),
            "REQ-1".to_string(),
            "Test".to_string(),
            "Scenario".to_string(),
        );
        validator.add_criteria(criteria);
        assert_eq!(validator.criteria().len(), 1);
    }

    #[test]
    fn test_coverage_validation() {
        let mut validator = CriteriaValidator::new();
        let criteria1 = AcceptanceCriteria::new(
            "AC-1".to_string(),
            "REQ-1".to_string(),
            "Test".to_string(),
            "Scenario".to_string(),
        )
        .with_test_case("TEST-1".to_string());
        let criteria2 = AcceptanceCriteria::new(
            "AC-2".to_string(),
            "REQ-1".to_string(),
            "Test".to_string(),
            "Scenario".to_string(),
        );

        validator.add_criteria(criteria1);
        validator.add_criteria(criteria2);

        let report = validator.validate_test_coverage();
        assert_eq!(report.total_criteria, 2);
        assert_eq!(report.covered_criteria, 1);
        assert!(!report.is_complete());
    }

    #[test]
    fn test_traceability_matrix() {
        let matrix = TraceabilityMatrix::new();
        assert_eq!(matrix.coverage_percent(), 0.0);
    }
}
