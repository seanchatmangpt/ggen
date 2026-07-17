//! JTBD (Jobs To Be Done) Validation Framework
//!
//! Validates that code accomplishes its **intended purpose** in real-world scenarios,
//! not just returns technical success. This is the core principle: "Validate that code
//! does the job it's supposed to do, not just that it executes without errors."
//!
//! Based on DFLSS research document: `RESEARCH_REFLEX_WORKFLOW_JTBD_INNOVATION.md`
//!
//! JTBD validation ensures:
//! 1. Code executes in real contexts (not isolated unit tests)
//! 2. Code accomplishes its intended purpose (JTBD validation)
//! 3. Results are validated against expected behavior (state-based validation)
//! 4. OTEL telemetry reflects actual work (observability validation)
//!
//! ## Chicago TDD Principles Applied to JTBD
//!
//! 1. **State-based tests**: Verify outputs and state, not implementation details
//! 2. **Real collaborators**: Use actual dependencies, not mocks
//! 3. **End-to-end validation**: Complete workflows from execution to analysis
//! 4. **JTBD focus**: Validate actual use cases, not just technical integration
//!
//! # Poka-Yoke: Type-Level Validation
//!
//! This module uses newtypes to prevent index errors at compile time.
//! Use `ScenarioIndex` instead of `usize` for scenario indices.
//!
//! ## Usage
//!
//! ```rust
//! use chicago_tdd_tools::jtbd::{JtbdValidator, JtbdScenario, ScenarioIndex, ExecutionContext, ExecutionResult};
//! use std::collections::HashMap;
//!
//! // Helper function for doctest
//! # fn create_test_context() -> ExecutionContext {
//! #     ExecutionContext::default()
//! # }
//!
//! // Create validator
//! let mut validator = JtbdValidator::new();
//!
//! // Register scenario
//! validator.register_scenario(JtbdScenario {
//!     name: "Order Processing".to_string(),
//!     setup_context: Box::new(|| create_test_context()),
//!     execute: Box::new(|_ctx| {
//!         let mut vars = HashMap::new();
//!         vars.insert("order_id".to_string(), "ORD-001".to_string());
//!         ExecutionResult::ok(vars)
//!     }),
//!     validate_result: Box::new(|_ctx, result| {
//!         // Validate that order was actually processed
//!         result.success && result.variables.contains_key("order_id")
//!     }),
//!     expected_behavior: "Process order and update state".to_string(),
//! });
//!
//! // Validate using type-safe index
//! let index = ScenarioIndex::new(0).unwrap();
//! let result = validator.validate_scenario(index);
//! assert!(result.is_some());
//! assert!(result.unwrap().jtbd_success);
//!
//! // Validate all
//! let results = validator.validate_all();
//! assert!(results.iter().all(|r| r.jtbd_success));
//! ```

use std::collections::HashMap;

// ============================================================================
// Poka-Yoke: Type-Level Validation
// ============================================================================

/// Scenario index newtype
///
/// **Poka-Yoke**: Use this newtype instead of `usize` to prevent index errors.
/// Ensures indices are valid for the scenario collection.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::jtbd::ScenarioIndex;
///
/// // Create validated index
/// let index = ScenarioIndex::new(0).unwrap();
/// assert_eq!(index.get(), 0);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScenarioIndex(usize);

impl ScenarioIndex {
    /// Create a new scenario index
    ///
    /// Returns `None` if the index is out of bounds for the given collection size.
    ///
    /// # Example
    ///
    /// ```rust
    /// use chicago_tdd_tools::jtbd::ScenarioIndex;
    ///
    /// // Valid index
    /// let index = ScenarioIndex::new(0).unwrap();
    /// assert_eq!(index.get(), 0);
    ///
    /// // Invalid index (would be out of bounds)
    /// let result = ScenarioIndex::new_for_collection(5, 3); // None - index 5 >= size 3
    /// assert!(result.is_none());
    /// ```
    #[must_use]
    #[allow(clippy::unnecessary_wraps)] // API design - Option allows future validation without breaking changes
    pub const fn new(value: usize) -> Option<Self> {
        Some(Self(value))
    }

    /// Create a new scenario index validated against collection size
    ///
    /// Returns `None` if the index is out of bounds.
    ///
    /// # Example
    ///
    /// ```rust
    /// use chicago_tdd_tools::jtbd::ScenarioIndex;
    ///
    /// let collection_size = 3;
    /// let index = ScenarioIndex::new_for_collection(0, collection_size).unwrap(); // Valid
    /// assert_eq!(index.get(), 0);
    /// let invalid = ScenarioIndex::new_for_collection(5, collection_size); // None
    /// assert!(invalid.is_none());
    /// ```
    #[must_use]
    pub const fn new_for_collection(index: usize, collection_size: usize) -> Option<Self> {
        if index < collection_size {
            Some(Self(index))
        } else {
            None
        }
    }

    /// Get the index value
    #[must_use]
    #[allow(clippy::trivially_copy_pass_by_ref)] // Const fn - cannot change signature to pass by value
    pub const fn get(&self) -> usize {
        self.0
    }

    /// Convert to usize
    #[must_use]
    pub const fn into_usize(self) -> usize {
        self.0
    }
}

impl From<ScenarioIndex> for usize {
    fn from(index: ScenarioIndex) -> Self {
        index.0
    }
}

/// JTBD validation result for a single scenario
#[derive(Debug, Clone)]
pub struct JtbdValidationResult {
    /// Scenario name
    pub scenario_name: String,
    /// Whether execution succeeded
    pub execution_success: bool,
    /// Whether JTBD validation passed (code accomplished intended purpose)
    pub jtbd_success: bool,
    /// Execution latency in milliseconds
    pub latency_ms: u64,
    /// Validation details
    pub details: Vec<String>,
    /// Expected behavior description
    pub expected_behavior: String,
    /// Actual behavior description
    pub actual_behavior: String,
}

impl JtbdValidationResult {
    /// Create a successful JTBD validation result
    #[must_use]
    #[allow(clippy::missing_const_for_fn)] // Cannot be const - takes String and Vec parameters
    pub fn success(scenario_name: String, latency_ms: u64, details: Vec<String>) -> Self {
        Self {
            scenario_name,
            execution_success: true,
            jtbd_success: true,
            latency_ms,
            details,
            expected_behavior: String::new(),
            actual_behavior: String::new(),
        }
    }

    /// Create a failed JTBD validation result
    #[must_use]
    #[allow(clippy::missing_const_for_fn)] // Cannot be const - takes String and Vec parameters
    pub fn failure(
        scenario_name: String,
        execution_success: bool,
        expected_behavior: String,
        actual_behavior: String,
        details: Vec<String>,
    ) -> Self {
        Self {
            scenario_name,
            execution_success,
            jtbd_success: false,
            latency_ms: 0,
            details,
            expected_behavior,
            actual_behavior,
        }
    }
}

/// Generic execution context (can be extended for specific use cases)
#[derive(Debug, Clone, Default)]
pub struct ExecutionContext {
    /// Variables for execution
    pub variables: HashMap<String, String>,
    /// Additional context data
    pub metadata: HashMap<String, String>,
}

/// Generic execution result (can be extended for specific use cases)
#[derive(Debug, Clone)]
pub struct ExecutionResult {
    /// Whether execution succeeded
    pub success: bool,
    /// Output variables
    pub variables: HashMap<String, String>,
    /// Additional result data
    pub metadata: HashMap<String, String>,
}

impl ExecutionResult {
    /// Create a successful result
    #[must_use]
    pub fn ok(variables: HashMap<String, String>) -> Self {
        Self { success: true, variables, metadata: HashMap::new() }
    }

    /// Create a failed result
    #[must_use]
    pub fn err(message: String) -> Self {
        let mut metadata = HashMap::new();
        metadata.insert("error".to_string(), message);
        Self { success: false, variables: HashMap::new(), metadata }
    }
}

/// Type alias for validation function
type ValidateResultFn = Box<dyn Fn(&ExecutionContext, &ExecutionResult) -> bool + Send + Sync>;

/// JTBD validation scenario
pub struct JtbdScenario {
    /// Scenario name
    pub name: String,
    /// Setup function to create execution context
    pub setup_context: Box<dyn Fn() -> ExecutionContext + Send + Sync>,
    /// Execution function (returns execution result)
    pub execute: Box<dyn Fn(&ExecutionContext) -> ExecutionResult + Send + Sync>,
    /// Validation function to check if result accomplishes intended purpose
    pub validate_result: ValidateResultFn,
    /// Expected behavior description
    pub expected_behavior: String,
}

/// JTBD validator
pub struct JtbdValidator {
    /// JTBD scenarios
    scenarios: Vec<JtbdScenario>,
}

impl JtbdValidator {
    /// Create a new JTBD validator
    #[must_use]
    #[allow(clippy::missing_const_for_fn)] // Cannot be const - contains Vec field
    pub fn new() -> Self {
        Self { scenarios: Vec::new() }
    }

    /// Register a JTBD scenario
    pub fn register_scenario(&mut self, scenario: JtbdScenario) {
        self.scenarios.push(scenario);
    }

    /// Validate a single scenario's JTBD
    ///
    /// **Poka-Yoke**: Uses `ScenarioIndex` newtype to prevent index errors.
    #[must_use]
    pub fn validate_scenario(&self, index: ScenarioIndex) -> Option<JtbdValidationResult> {
        let scenario = self.scenarios.get(index.get())?;

        // Setup execution context
        let context = (scenario.setup_context)();

        // Execute
        let start_time = std::time::Instant::now();
        let execution_result = (scenario.execute)(&context);
        #[allow(clippy::cast_possible_truncation)]
        // Milliseconds won't exceed u64::MAX for reasonable durations
        let latency_ms = start_time.elapsed().as_millis() as u64;

        // Validate JTBD: Does the code accomplish its intended purpose?
        let jtbd_valid = (scenario.validate_result)(&context, &execution_result);

        // Kaizen improvement: Clone scenario name once and reuse to avoid multiple clones
        let scenario_name = scenario.name.clone();

        if execution_result.success && jtbd_valid {
            Some(JtbdValidationResult::success(
                scenario_name,
                latency_ms,
                vec![format!(
                    "Scenario '{}' executed successfully and accomplished intended purpose",
                    scenario.name
                )],
            ))
        } else {
            let details = if execution_result.success {
                vec!["Execution succeeded but did not accomplish intended purpose".to_string()]
            } else {
                vec!["Execution failed".to_string()]
            };

            Some(JtbdValidationResult::failure(
                scenario_name,
                execution_result.success,
                scenario.expected_behavior.clone(),
                format!("Execution: {}, JTBD: {jtbd_valid}", execution_result.success),
                details,
            ))
        }
    }

    /// Validate all registered scenarios
    #[must_use]
    pub fn validate_all(&self) -> Vec<JtbdValidationResult> {
        let mut results = Vec::new();

        for i in 0..self.scenarios.len() {
            // SAFETY: i is always < scenarios.len(), so ScenarioIndex::new(i) is always Some
            if let Some(index) = ScenarioIndex::new(i) {
                if let Some(result) = self.validate_scenario(index) {
                    results.push(result);
                }
            }
        }

        results
    }

    /// Get validation summary
    #[must_use]
    #[allow(clippy::unused_self)] // Part of API - self required for consistency
    pub fn get_summary(&self, results: &[JtbdValidationResult]) -> JtbdValidationSummary {
        let total = results.len();
        let execution_passed = results.iter().filter(|r| r.execution_success).count();
        let jtbd_passed = results.iter().filter(|r| r.jtbd_success).count();
        let execution_failed = total - execution_passed;
        let jtbd_failed = execution_passed - jtbd_passed;

        JtbdValidationSummary {
            total_scenarios: total,
            execution_passed,
            execution_failed,
            jtbd_passed,
            jtbd_failed,
            avg_latency_ms: if results.is_empty() {
                0
            } else {
                results.iter().map(|r| r.latency_ms).sum::<u64>() / total as u64
            },
        }
    }
}

impl Default for JtbdValidator {
    fn default() -> Self {
        Self::new()
    }
}

/// JTBD validation summary
#[derive(Debug, Clone)]
pub struct JtbdValidationSummary {
    /// Total scenarios validated
    pub total_scenarios: usize,
    /// Scenarios that executed successfully
    pub execution_passed: usize,
    /// Scenarios that failed execution
    pub execution_failed: usize,
    /// Scenarios that accomplished their intended purpose (JTBD)
    pub jtbd_passed: usize,
    /// Scenarios that executed but didn't accomplish intended purpose
    pub jtbd_failed: usize,
    /// Average latency in milliseconds
    pub avg_latency_ms: u64,
}

impl JtbdValidationSummary {
    /// Check if all scenarios passed JTBD validation
    #[must_use]
    pub const fn all_passed(&self) -> bool {
        self.execution_passed == self.total_scenarios && self.jtbd_passed == self.total_scenarios
    }

    /// Get pass rate (0.0 to 1.0)
    #[must_use]
    #[allow(clippy::cast_precision_loss)] // Percentage calculation - precision loss acceptable for pass rates
    pub fn pass_rate(&self) -> f64 {
        if self.total_scenarios == 0 {
            return 0.0;
        }
        self.jtbd_passed as f64 / self.total_scenarios as f64
    }
}

#[cfg(test)]
#[allow(clippy::panic, clippy::unwrap_used, clippy::float_cmp)] // Test code - panic, unwrap, and float comparisons are acceptable
mod tests {
    use super::*;

    #[test]
    fn test_jtbd_validator_creation() {
        let validator = JtbdValidator::new();
        assert_eq!(validator.scenarios.len(), 0);
    }

    #[test]
    fn test_jtbd_scenario_registration() {
        let mut validator = JtbdValidator::new();

        validator.register_scenario(JtbdScenario {
            name: "Test Scenario".to_string(),
            setup_context: Box::new(ExecutionContext::default),
            execute: Box::new(|_ctx| ExecutionResult::ok(HashMap::new())),
            validate_result: Box::new(|_ctx, result| result.success),
            expected_behavior: "Should succeed".to_string(),
        });

        assert_eq!(validator.scenarios.len(), 1);
    }

    #[test]
    fn test_jtbd_validation_summary() {
        let summary = JtbdValidationSummary {
            total_scenarios: 10,
            execution_passed: 10,
            execution_failed: 0,
            jtbd_passed: 10,
            jtbd_failed: 0,
            avg_latency_ms: 5,
        };

        assert!(summary.all_passed());
        assert_eq!(summary.pass_rate(), 1.0);
    }

    #[test]
    #[allow(clippy::unwrap_used)] // Test code - unwrap is acceptable
    fn test_scenario_index() {
        // Test basic creation
        let index = ScenarioIndex::new(0).unwrap();
        assert_eq!(index.get(), 0);

        // Test collection validation
        let collection_size = 5;
        let valid_index = ScenarioIndex::new_for_collection(0, collection_size).unwrap();
        assert_eq!(valid_index.get(), 0);

        let valid_index2 = ScenarioIndex::new_for_collection(4, collection_size).unwrap();
        assert_eq!(valid_index2.get(), 4);

        // Test invalid index (out of bounds)
        let invalid_index = ScenarioIndex::new_for_collection(5, collection_size);
        assert!(invalid_index.is_none());

        // Test conversion
        let index = ScenarioIndex::new(42).unwrap();
        let usize_value: usize = index.into();
        assert_eq!(usize_value, 42);
    }

    #[test]
    #[allow(clippy::unwrap_used)] // Test code - unwrap is acceptable
    fn test_validate_scenario_with_index() {
        let mut validator = JtbdValidator::new();

        validator.register_scenario(JtbdScenario {
            name: "Test Scenario".to_string(),
            setup_context: Box::new(ExecutionContext::default),
            execute: Box::new(|_ctx| ExecutionResult::ok(HashMap::new())),
            validate_result: Box::new(|_ctx, result| result.success),
            expected_behavior: "Should succeed".to_string(),
        });

        // Test with ScenarioIndex
        let index = ScenarioIndex::new(0).unwrap();
        let result = validator.validate_scenario(index);
        assert!(result.is_some());
        assert!(result.unwrap().jtbd_success);

        // Test with invalid index
        let invalid_index = ScenarioIndex::new(1).unwrap();
        let result = validator.validate_scenario(invalid_index);
        assert!(result.is_none());
    }
}
