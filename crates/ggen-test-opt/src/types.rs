//! Core data types for test optimization
//!
//! This module defines the foundational entities for test value scoring, budget enforcement,
//! and metadata tracking used in the 80/20 Pareto test selection algorithm.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;

// Re-export TestId from ggen-test-audit for consistency
pub type TestId = ggen_test_audit::TestId;
pub type TestType = ggen_test_audit::TestType;

// =============================================================================
// TestValueScore (T021) - Composite scoring for test selection
// =============================================================================

/// Test value score combining multiple factors
///
/// Uses industry-validated weights to compute composite value:
/// - 40% failure frequency (defect detection signal)
/// - 25% code coverage (unique lines covered)
/// - 15% execution speed (inverse of time)
/// - 15% criticality (domain expert weights)
/// - 5% budget penalty (performance compliance)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestValueScore {
    /// Test identifier
    pub test_id: TestId,
    /// Failure frequency score (0-100): failures/runs × 100
    pub failure_freq_score: f64,
    /// Coverage score (0-100): unique_lines/total × 100
    pub coverage_score: f64,
    /// Speed score (0-100): (1 - exec_time/budget) × 100
    pub speed_score: f64,
    /// Criticality score (0-100): domain expert weights
    pub criticality_score: f64,
    /// Budget penalty (0-100): penalty for exceeding budget
    pub budget_penalty: f64,
    /// Composite value: weighted sum of above scores
    pub composite_value: f64,
}

impl TestValueScore {
    /// Calculate composite value from component scores using industry weights
    #[must_use]
    pub fn calculate_composite(
        failure_freq: f64, coverage: f64, speed: f64, criticality: f64, budget_penalty: f64,
    ) -> f64 {
        let weights = ScoringWeights::default();
        (failure_freq * weights.failure_freq)
            + (coverage * weights.coverage)
            + (speed * weights.speed)
            + (criticality * weights.criticality)
            - (budget_penalty * weights.budget_penalty)
    }
}

// T025: Implement Ord and PartialOrd for sorting by composite_value
impl PartialEq for TestValueScore {
    fn eq(&self, other: &Self) -> bool {
        self.composite_value == other.composite_value
    }
}

impl Eq for TestValueScore {}

impl PartialOrd for TestValueScore {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TestValueScore {
    fn cmp(&self, other: &Self) -> Ordering {
        // Reverse order (higher score first)
        other
            .composite_value
            .partial_cmp(&self.composite_value)
            .unwrap_or(Ordering::Equal)
    }
}

// =============================================================================
// ScoringWeights (T024) - Industry-validated weights
// =============================================================================

/// Weights for test value scoring algorithm
///
/// Based on empirical research and DfLSS analysis:
/// - Failure frequency most predictive (0.40)
/// - Coverage second most important (0.25)
/// - Speed and criticality balanced (0.15 each)
/// - Budget penalty minimal (0.05)
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct ScoringWeights {
    /// Failure frequency weight (default: 0.40)
    pub failure_freq: f64,
    /// Code coverage weight (default: 0.25)
    pub coverage: f64,
    /// Execution speed weight (default: 0.15)
    pub speed: f64,
    /// Criticality weight (default: 0.15)
    pub criticality: f64,
    /// Budget penalty weight (default: 0.05)
    pub budget_penalty: f64,
}

impl Default for ScoringWeights {
    fn default() -> Self {
        Self {
            failure_freq: 0.40,
            coverage: 0.25,
            speed: 0.15,
            criticality: 0.15,
            budget_penalty: 0.05,
        }
    }
}

// =============================================================================
// TestMetadata (T023) - Historical test data
// =============================================================================

/// Metadata tracked for each test over time
///
/// Uses 30-day rolling window for failure tracking and execution metrics.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestMetadata {
    /// Test identifier
    pub test_id: TestId,
    /// Total failures in retention window
    pub failure_count: u32,
    /// Total runs in retention window
    pub run_count: u32,
    /// Last failure timestamp (None if never failed)
    pub last_failure_date: Option<DateTime<Utc>>,
    /// Average execution time in milliseconds
    pub avg_execution_time_ms: u64,
    /// Number of unique lines covered by this test
    pub unique_lines_covered: usize,
}

impl TestMetadata {
    /// Calculate failure frequency (failures / runs)
    #[must_use]
    pub fn failure_frequency(&self) -> f64 {
        if self.run_count == 0 {
            0.0
        } else {
            f64::from(self.failure_count) / f64::from(self.run_count)
        }
    }
}

// =============================================================================
// BudgetViolation (T022) - Performance budget tracking
// =============================================================================

/// Performance budget violation record
///
/// Tracks tests that exceed their allocated time budgets:
/// - Unit tests: ≤1s
/// - Integration tests: ≤10s
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BudgetViolation {
    /// Test identifier
    pub test_id: TestId,
    /// Actual execution time in milliseconds
    pub exec_time_ms: u64,
    /// Budget allocation in milliseconds
    pub budget_ms: u64,
    /// Excess time over budget in milliseconds
    pub excess_ms: u64,
    /// Violation severity
    pub severity: Severity,
}

impl BudgetViolation {
    /// Create a new budget violation
    ///
    /// Calculates severity automatically based on excess percentage:
    /// - Warning: 1-50% over budget
    /// - Error: 51-100% over budget
    /// - Critical: >100% over budget
    #[must_use]
    pub fn new(test_id: TestId, exec_time_ms: u64, budget_ms: u64) -> Self {
        let excess_ms = exec_time_ms.saturating_sub(budget_ms);
        let excess_pct = (f64::from(excess_ms as u32) / f64::from(budget_ms as u32)) * 100.0;

        let severity = if excess_pct <= 50.0 {
            Severity::Warning
        } else if excess_pct <= 100.0 {
            Severity::Error
        } else {
            Severity::Critical
        };

        Self {
            test_id,
            exec_time_ms,
            budget_ms,
            excess_ms,
            severity,
        }
    }
}

/// Severity of budget violation
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum Severity {
    /// Warning: 1-50% over budget
    Warning,
    /// Error: 51-100% over budget
    Error,
    /// Critical: >100% over budget
    Critical,
}

// =============================================================================
// Error Types (T027, T029)
// =============================================================================

/// Errors that can occur during test optimization operations
#[derive(Debug, thiserror::Error)]
pub enum OptimizationError {
    /// Budget exceeded
    #[error("Budget exceeded: {0}")]
    BudgetExceeded(String),

    /// Insufficient coverage
    #[error("Insufficient coverage: {0}")]
    InsufficientCoverage(String),

    /// No tests selected
    #[error("No tests selected: {0}")]
    NoTestsSelected(String),

    /// Invalid scoring weights
    #[error("Invalid weights: {0}")]
    InvalidWeights(String),

    /// I/O error during optimization
    #[error("I/O error: {0}")]
    IoError(#[from] std::io::Error),

    /// JSON serialization/deserialization error
    #[error("JSON error: {0}")]
    JsonError(#[from] serde_json::Error),
}

/// Type alias for optimization results (T029)
pub type OptResult<T> = Result<T, OptimizationError>;

/// Backward-compatible alias used by scorers and enforcers
pub type OptimizationResult<T> = OptResult<T>;
