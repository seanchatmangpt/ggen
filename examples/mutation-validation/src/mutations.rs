//! Mutation types and results for mutation-based validation
//!
//! 7 agents perform mutation testing:
//! - Agents 1-3: Inject mutations (syntax, logic, boundary)
//! - Agents 4-5: Run tests to detect mutations
//! - Agent 6: Calculate mutation score
//! - Agent 7: Suggest test improvements

use serde::{Deserialize, Serialize};

/// Types of mutations agents can inject
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum MutationType {
    /// Syntax mutations (change operators, delete statements)
    Syntax,
    /// Logic mutations (flip conditionals, negate expressions)
    Logic,
    /// Boundary mutations (change array indices, loop bounds)
    Boundary,
    /// Value mutations (change constants, return values)
    Value,
    /// Arithmetic mutations (replace operators: +, -, *, /)
    Arithmetic,
    /// Relational mutations (replace comparisons: <, >, <=, >=)
    Relational,
    /// Conditional mutations (replace &&, ||)
    Conditional,
}

impl MutationType {
    /// All mutation types
    pub fn all() -> Vec<Self> {
        vec![
            Self::Syntax,
            Self::Logic,
            Self::Boundary,
            Self::Value,
            Self::Arithmetic,
            Self::Relational,
            Self::Conditional,
        ]
    }

    /// Agent responsible for this mutation type
    pub fn agent_id(&self) -> u8 {
        match self {
            Self::Syntax => 1,
            Self::Logic => 2,
            Self::Boundary => 3,
            Self::Value => 4,
            Self::Arithmetic => 5,
            Self::Relational => 6,
            Self::Conditional => 7,
        }
    }
}

/// A single mutation applied to code
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Mutation {
    /// Unique ID for this mutation
    pub id: String,

    /// Type of mutation
    pub mutation_type: MutationType,

    /// File path where mutation was applied
    pub file_path: String,

    /// Line number (0-indexed)
    pub line: usize,

    /// Original code (before mutation)
    pub original_code: String,

    /// Mutated code (after mutation)
    pub mutated_code: String,

    /// Description of what changed
    pub description: String,

    /// Whether mutation was detected (killed) or survived
    pub status: MutationStatus,

    /// Which test killed this mutation (if any)
    pub killed_by: Option<String>,
}

/// Mutation status after running tests
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum MutationStatus {
    /// Mutation was detected by tests (killed)
    Killed,
    /// Mutation survived all tests (survived)
    Survived,
    /// Tests timed out (possible infinite loop)
    Timeout,
    /// Tests couldn't run (compilation error, etc.)
    Error,
}

/// Result of mutation testing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MutationResult {
    /// All mutations applied
    pub mutations: Vec<Mutation>,

    /// Mutation score (percentage of mutations killed)
    pub mutation_score: f64,

    /// Total mutations
    pub total_mutations: usize,

    /// Mutations killed
    pub killed: usize,

    /// Mutations survived
    pub survived: usize,

    /// Mutations that caused errors
    pub errors: usize,

    /// Time taken for mutation testing (ms)
    pub duration_ms: u64,
}

impl MutationResult {
    /// Calculate mutation score (0.0 - 1.0)
    pub fn calculate_score(&mut self) {
        let detected = self.killed + self.errors;
        self.mutation_score = if self.total_mutations > 0 {
            detected as f64 / self.total_mutations as f64
        } else {
            0.0
        };
    }

    /// Get mutations by type
    pub fn get_by_type(&self, mutation_type: MutationType) -> Vec<&Mutation> {
        self.mutations
            .iter()
            .filter(|m| m.mutation_type == mutation_type)
            .collect()
    }

    /// Get surviving mutations (test gaps)
    pub fn surviving_mutations(&self) -> Vec<&Mutation> {
        self.mutations
            .iter()
            .filter(|m| m.status == MutationStatus::Survived)
            .collect()
    }

    /// Get killed mutations (good tests)
    pub fn killed_mutations(&self) -> Vec<&Mutation> {
        self.mutations
            .iter()
            .filter(|m| m.status == MutationStatus::Killed)
            .collect()
    }
}

/// Test improvement suggestions from Agent 7
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestSuggestion {
    /// Which surviving mutation this addresses
    pub mutation_id: String,

    /// Suggested test case
    pub suggested_test: String,

    /// Why this test would help
    pub rationale: String,

    /// Priority (Critical, High, Medium, Low)
    pub priority: String,
}

/// Final report with mutation analysis and test suggestions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MutationReport {
    /// Mutation testing results
    pub result: MutationResult,

    /// Test improvement suggestions
    pub suggestions: Vec<TestSuggestion>,

    /// Overall assessment
    pub assessment: String,

    /// Timestamp
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mutation_type_agent_ids() {
        assert_eq!(MutationType::Syntax.agent_id(), 1);
        assert_eq!(MutationType::Logic.agent_id(), 2);
        assert_eq!(MutationType::Boundary.agent_id(), 3);
        assert_eq!(MutationType::Value.agent_id(), 4);
        assert_eq!(MutationType::Arithmetic.agent_id(), 5);
        assert_eq!(MutationType::Relational.agent_id(), 6);
        assert_eq!(MutationType::Conditional.agent_id(), 7);
    }

    #[test]
    fn test_mutation_score_calculation() {
        let mut result = MutationResult {
            mutations: vec![],
            mutation_score: 0.0,
            total_mutations: 10,
            killed: 7,
            survived: 2,
            errors: 1,
            duration_ms: 1000,
        };

        result.calculate_score();
        assert_eq!(result.mutation_score, 0.8); // (7 + 1) / 10
    }

    #[test]
    fn test_surviving_mutations_filter() {
        let result = MutationResult {
            mutations: vec![
                Mutation {
                    id: "1".to_string(),
                    mutation_type: MutationType::Logic,
                    file_path: "test.rs".to_string(),
                    line: 0,
                    original_code: "true".to_string(),
                    mutated_code: "false".to_string(),
                    description: "Flip boolean".to_string(),
                    status: MutationStatus::Survived,
                    killed_by: None,
                },
                Mutation {
                    id: "2".to_string(),
                    mutation_type: MutationType::Logic,
                    file_path: "test.rs".to_string(),
                    line: 1,
                    original_code: "true".to_string(),
                    mutated_code: "false".to_string(),
                    description: "Flip boolean".to_string(),
                    status: MutationStatus::Killed,
                    killed_by: Some("test_foo".to_string()),
                },
            ],
            mutation_score: 0.5,
            total_mutations: 2,
            killed: 1,
            survived: 1,
            errors: 0,
            duration_ms: 100,
        };

        assert_eq!(result.surviving_mutations().len(), 1);
        assert_eq!(result.killed_mutations().len(), 1);
    }
}
