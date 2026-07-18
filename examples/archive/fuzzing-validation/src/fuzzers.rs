//! Fuzzer types and results for fuzzing-based validation
//!
//! 7 agents perform fuzzing to explore input space:
//! - Agent 1: Structure fuzzing (malformed inputs)
//! - Agent 2: Value fuzzing (boundary values)
//! - Agent 3: Protocol fuzzing (invalid messages)
//! - Agent 4: Chaos engineering (random failures)
//! - Agent 5: Performance fuzzing (extreme loads)
//! - Agent 6: Security fuzzing (attack vectors)
//! - Agent 7: Semantic fuzzing (edge cases)

use serde::{Deserialize, Serialize};

/// Types of fuzzing strategies
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum FuzzerType {
    /// Structure fuzzing (malformed RDF, broken syntax)
    Structure,
    /// Value fuzzing (boundary values, extreme numbers)
    Value,
    /// Protocol fuzzing (invalid SPARQL, broken queries)
    Protocol,
    /// Chaos engineering (random failures, timeouts)
    Chaos,
    /// Performance fuzzing (extreme loads, stress testing)
    Performance,
    /// Security fuzzing (injection attacks, path traversal)
    Security,
    /// Semantic fuzzing (edge cases, corner cases)
    Semantic,
}

impl FuzzerType {
    /// All fuzzer types
    pub fn all() -> Vec<Self> {
        vec![
            Self::Structure,
            Self::Value,
            Self::Protocol,
            Self::Chaos,
            Self::Performance,
            Self::Security,
            Self::Semantic,
        ]
    }

    /// Agent responsible for this fuzzer type
    pub fn agent_id(&self) -> u8 {
        match self {
            Self::Structure => 1,
            Self::Value => 2,
            Self::Protocol => 3,
            Self::Chaos => 4,
            Self::Performance => 5,
            Self::Security => 6,
            Self::Semantic => 7,
        }
    }
}

/// A single fuzzing input
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FuzzInput {
    /// Unique ID for this input
    pub id: String,

    /// Type of fuzzer that generated this input
    pub fuzzer_type: FuzzerType,

    /// Agent ID that generated this input
    pub agent_id: u8,

    /// The fuzzed input (string representation)
    pub input: String,

    /// Description of what makes this input interesting
    pub description: String,

    /// Whether the system handled this input correctly
    pub status: FuzzStatus,

    /// Failure details (if any)
    pub failure_details: Option<String>,
}

/// Status after fuzzing
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum FuzzStatus {
    /// System handled input correctly
    Passed,
    /// System crashed or panicked
    Crash,
    /// System hung or timed out
    Hang,
    /// System returned wrong result
    Incorrect,
    /// System threw unexpected error
    Error,
}

/// Result of fuzzing campaign
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FuzzResult {
    /// All fuzzing inputs tested
    pub inputs: Vec<FuzzInput>,

    /// Total inputs generated
    pub total_inputs: usize,

    /// Inputs passed
    pub passed: usize,

    /// Crashes found
    pub crashes: usize,

    /// Hangs found
    pub hangs: usize,

    /// Incorrect results found
    pub incorrect: usize,

    /// Errors found
    pub errors: usize,

    /// Time taken for fuzzing (ms)
    pub duration_ms: u64,

    /// Unique crashes found (deduplicated)
    pub unique_crashes: usize,
}

impl FuzzResult {
    /// Calculate crash rate (0.0 - 1.0)
    pub fn crash_rate(&self) -> f64 {
        if self.total_inputs > 0 {
            self.crashes as f64 / self.total_inputs as f64
        } else {
            0.0
        }
    }

    /// Calculate error rate (0.0 - 1.0)
    pub fn error_rate(&self) -> f64 {
        if self.total_inputs > 0 {
            (self.crashes + self.hangs + self.incorrect + self.errors) as f64
                / self.total_inputs as f64
        } else {
            0.0
        }
    }

    /// Get failed inputs (crashes, hangs, incorrect, errors)
    pub fn failed_inputs(&self) -> Vec<&FuzzInput> {
        self.inputs
            .iter()
            .filter(|i| matches!(i.status, FuzzStatus::Crash | FuzzStatus::Hang | FuzzStatus::Incorrect | FuzzStatus::Error))
            .collect()
    }

    /// Get crashes by fuzzer type
    pub fn crashes_by_type(&self) -> std::collections::HashMap<FuzzerType, usize> {
        let mut crashes = std::collections::HashMap::new();
        for input in &self.inputs {
            if input.status == FuzzStatus::Crash {
                *crashes.entry(input.fuzzer_type).or_insert(0) += 1;
            }
        }
        crashes
    }
}

/// Recommendation for improving robustness
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RobustnessRecommendation {
    /// Which fuzzer found this issue
    pub fuzzer_type: FuzzerType,

    /// Input that triggered the failure
    pub triggering_input: String,

    /// What went wrong
    pub issue: String,

    /// Suggested fix
    pub suggestion: String,

    /// Priority (Critical, High, Medium, Low)
    pub priority: String,
}

/// Final fuzzing report with analysis and recommendations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FuzzReport {
    /// Fuzzing results
    pub result: FuzzResult,

    /// Robustness improvement recommendations
    pub recommendations: Vec<RobustnessRecommendation>,

    /// Overall robustness assessment
    pub assessment: String,

    /// Timestamp
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fuzzer_type_agent_ids() {
        assert_eq!(FuzzerType::Structure.agent_id(), 1);
        assert_eq!(FuzzerType::Value.agent_id(), 2);
        assert_eq!(FuzzerType::Protocol.agent_id(), 3);
        assert_eq!(FuzzerType::Chaos.agent_id(), 4);
        assert_eq!(FuzzerType::Performance.agent_id(), 5);
        assert_eq!(FuzzerType::Security.agent_id(), 6);
        assert_eq!(FuzzerType::Semantic.agent_id(), 7);
    }

    #[test]
    fn test_crash_rate_calculation() {
        let result = FuzzResult {
            inputs: vec![],
            total_inputs: 100,
            passed: 90,
            crashes: 5,
            hangs: 2,
            incorrect: 2,
            errors: 1,
            duration_ms: 1000,
            unique_crashes: 3,
        };

        assert_eq!(result.crash_rate(), 0.05); // 5/100
        assert_eq!(result.error_rate(), 0.10); // 10/100
    }

    #[test]
    fn test_failed_inputs_filter() {
        let result = FuzzResult {
            inputs: vec![
                FuzzInput {
                    id: "1".to_string(),
                    fuzzer_type: FuzzerType::Structure,
                    agent_id: 1,
                    input: "valid".to_string(),
                    description: "Valid input".to_string(),
                    status: FuzzStatus::Passed,
                    failure_details: None,
                },
                FuzzInput {
                    id: "2".to_string(),
                    fuzzer_type: FuzzerType::Value,
                    agent_id: 2,
                    input: "malformed".to_string(),
                    description: "Malformed input".to_string(),
                    status: FuzzStatus::Crash,
                    failure_details: Some("Panic".to_string()),
                },
            ],
            total_inputs: 2,
            passed: 1,
            crashes: 1,
            hangs: 0,
            incorrect: 0,
            errors: 0,
            duration_ms: 100,
            unique_crashes: 1,
        };

        assert_eq!(result.failed_inputs().len(), 1);
        assert_eq!(result.failed_inputs()[0].id, "2");
    }
}
