//! Phase 8: Continuous Learning
//!
//! ML-inspired test optimization and prediction based on historical data.
//! Uses heuristics and pattern recognition to:
//! - Predict test failure likelihood
//! - Optimize test execution order
//! - Learn from τ patterns
//! - Suggest preventive tests

use crate::core::contract::TestContract;
use crate::core::receipt::{TestOutcome, TestReceipt};
use std::collections::HashMap;

/// Test execution history entry
#[derive(Debug, Clone)]
pub struct HistoryEntry {
    /// Contract name
    pub contract_name: String,
    /// Module paths covered
    pub modules: Vec<String>,
    /// Execution time (ticks)
    pub ticks: u64,
    /// Test outcome
    pub outcome: TestOutcome,
    /// Timestamp (monotonic counter)
    pub timestamp: u64,
}

/// Learned pattern from historical data
#[derive(Debug, Clone)]
pub struct LearnedPattern {
    /// Pattern identifier
    pub id: String,
    /// Modules involved
    pub modules: Vec<String>,
    /// Average τ for this pattern
    pub average_tau: f64,
    /// Failure rate (0.0 - 1.0)
    pub failure_rate: f64,
    /// Number of observations
    pub observations: usize,
    /// Confidence score (0.0 - 1.0)
    pub confidence: f64,
}

/// Test prediction
#[derive(Debug, Clone)]
pub struct TestPrediction {
    /// Contract name
    pub contract_name: String,
    /// Predicted failure probability (0.0 - 1.0)
    pub failure_probability: f64,
    /// Predicted τ (ticks)
    pub predicted_tau: u64,
    /// Confidence in prediction (0.0 - 1.0)
    pub confidence: f64,
    /// Recommended action
    pub recommendation: Recommendation,
}

/// Recommended action based on learning
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Recommendation {
    /// Run test immediately (high failure risk)
    RunImmediately,
    /// Run test with priority
    RunWithPriority,
    /// Run test normally
    RunNormally,
    /// Test likely passes, can defer
    CanDefer,
    /// Skip test (very low failure probability)
    CanSkip,
}

/// Continuous learning engine
///
/// Learns from test execution history to optimize future test runs
pub struct ContinuousLearner {
    /// Historical test executions
    history: Vec<HistoryEntry>,
    /// Learned patterns indexed by modules
    patterns: HashMap<String, LearnedPattern>,
    /// Monotonic timestamp counter
    timestamp: u64,
    /// Minimum observations for pattern confidence
    min_observations: usize,
}

impl ContinuousLearner {
    /// Create a new continuous learner
    #[must_use]
    pub fn new() -> Self {
        Self { history: Vec::new(), patterns: HashMap::new(), timestamp: 0, min_observations: 5 }
    }

    /// Record a test execution
    pub fn record_execution(&mut self, receipt: &TestReceipt) {
        self.timestamp += 1;

        let entry = HistoryEntry {
            contract_name: receipt.contract_name.clone(),
            modules: receipt.invariants_checked.clone(),
            ticks: receipt.timing.total_ticks,
            outcome: receipt.result,
            timestamp: self.timestamp,
        };

        self.history.push(entry);
        self.update_patterns();
    }

    /// Update learned patterns based on history
    fn update_patterns(&mut self) {
        // Group executions by module combinations
        let mut module_groups: HashMap<String, Vec<&HistoryEntry>> = HashMap::new();

        for entry in &self.history {
            let key = entry.modules.join(",");
            module_groups.entry(key).or_default().push(entry);
        }

        // Learn patterns from groups
        for (key, entries) in module_groups {
            if entries.len() < self.min_observations {
                continue;
            }

            let total_tau: u64 = entries.iter().map(|e| e.ticks).sum();
            // Precision loss acceptable for statistical calculations (f64 mantissa sufficient for practical ranges)
            #[allow(clippy::cast_precision_loss)]
            let average_tau = total_tau as f64 / entries.len() as f64;

            let failures =
                entries.iter().filter(|e| matches!(e.outcome, TestOutcome::Fail)).count();
            // Precision loss acceptable for statistical rate calculations
            #[allow(clippy::cast_precision_loss)]
            let failure_rate = failures as f64 / entries.len() as f64;

            // Precision loss acceptable for confidence calculation (bounded 0.0-1.0)
            #[allow(clippy::cast_precision_loss)]
            let confidence = (entries.len() as f64 / (entries.len() + 10) as f64).min(1.0);

            let modules: Vec<String> = key.split(',').map(String::from).collect();

            let pattern = LearnedPattern {
                id: key.clone(),
                modules,
                average_tau,
                failure_rate,
                observations: entries.len(),
                confidence,
            };

            self.patterns.insert(key, pattern);
        }
    }

    /// Predict test outcome
    #[must_use]
    pub fn predict(&self, contract: &TestContract) -> TestPrediction {
        // Create module key
        let module_key = contract.coverage.join(",");

        // Look for matching pattern
        self.patterns.get(&module_key).map_or_else(
            || {
                // No pattern found - recommend normal execution
                TestPrediction {
                    contract_name: contract.name.to_string(),
                    failure_probability: 0.5, // Unknown
                    predicted_tau: 100,       // Conservative estimate
                    confidence: 0.0,
                    recommendation: Recommendation::RunNormally,
                }
            },
            |pattern| {
                let recommendation = if pattern.failure_rate > 0.3 {
                    Recommendation::RunImmediately
                } else if pattern.failure_rate > 0.1 {
                    Recommendation::RunWithPriority
                } else if pattern.failure_rate > 0.01 {
                    Recommendation::RunNormally
                } else if pattern.failure_rate > 0.001 {
                    Recommendation::CanDefer
                } else {
                    Recommendation::CanSkip
                };

                TestPrediction {
                    contract_name: contract.name.to_string(),
                    failure_probability: pattern.failure_rate,
                    #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
                    predicted_tau: pattern.average_tau as u64,
                    confidence: pattern.confidence,
                    recommendation,
                }
            },
        )
    }

    /// Get optimal test execution order
    ///
    /// Orders tests by predicted failure probability (highest first)
    #[must_use]
    pub fn optimize_execution_order(&self, contracts: &[TestContract]) -> Vec<String> {
        let mut predictions: Vec<_> = contracts.iter().map(|c| (c.name, self.predict(c))).collect();

        // Sort by failure probability (descending) then by predicted tau (ascending)
        // Note: partial_cmp can return None for NaN, but failure_probability should never be NaN
        // in practice. We use unwrap_or to handle the edge case safely.
        predictions.sort_by(|a, b| {
            b.1.failure_probability
                .partial_cmp(&a.1.failure_probability)
                .unwrap_or(std::cmp::Ordering::Equal)
                .then(a.1.predicted_tau.cmp(&b.1.predicted_tau))
        });

        predictions.into_iter().map(|(name, _)| name.to_string()).collect()
    }

    /// Suggest preventive tests for changed modules
    #[must_use]
    pub fn suggest_preventive_tests(&self, changed_modules: &[&str]) -> Vec<String> {
        let mut suggested = Vec::new();

        for pattern in self.patterns.values() {
            // Check if any changed module is in this pattern
            let has_changed_module =
                pattern.modules.iter().any(|m| changed_modules.iter().any(|cm| m.contains(cm)));

            if has_changed_module && pattern.failure_rate > 0.05 {
                suggested.push(pattern.id.clone());
            }
        }

        suggested
    }

    /// Get learned patterns
    #[must_use]
    #[allow(clippy::missing_const_for_fn)]
    pub fn patterns(&self) -> &HashMap<String, LearnedPattern> {
        &self.patterns
    }

    /// Get historical entries
    #[must_use]
    pub fn history(&self) -> &[HistoryEntry] {
        &self.history
    }

    /// Get pattern for specific modules
    #[must_use]
    pub fn pattern_for_modules(&self, modules: &[&str]) -> Option<&LearnedPattern> {
        let key = modules.join(",");
        self.patterns.get(&key)
    }
}

impl Default for ContinuousLearner {
    fn default() -> Self {
        Self::new()
    }
}

/// Adaptive test selector
///
/// Selects optimal test subset based on learned patterns
pub struct AdaptiveTestSelector {
    learner: ContinuousLearner,
    /// Maximum tests to run
    max_tests: usize,
    /// Minimum failure probability to include
    min_failure_prob: f64,
}

impl AdaptiveTestSelector {
    /// Create a new adaptive test selector
    #[must_use]
    #[allow(clippy::missing_const_for_fn)]
    pub fn new(learner: ContinuousLearner) -> Self {
        Self { learner, max_tests: 100, min_failure_prob: 0.01 }
    }

    /// Select optimal test subset
    #[must_use]
    pub fn select_tests(&self, contracts: &[TestContract]) -> Vec<String> {
        let mut predictions: Vec<_> =
            contracts.iter().map(|c| (c.name, self.learner.predict(c))).collect();

        // Filter by minimum failure probability
        predictions.retain(|(_, pred)| pred.failure_probability >= self.min_failure_prob);

        // Sort by failure probability and predicted tau
        // Note: partial_cmp can return None for NaN, but failure_probability should never be NaN
        // in practice. We use unwrap_or to handle the edge case safely.
        predictions.sort_by(|a, b| {
            b.1.failure_probability
                .partial_cmp(&a.1.failure_probability)
                .unwrap_or(std::cmp::Ordering::Equal)
                .then(a.1.predicted_tau.cmp(&b.1.predicted_tau))
        });

        // Take top N tests
        predictions
            .into_iter()
            .take(self.max_tests)
            .map(|(name, _)| name.to_string())
            .collect()
    }

    /// Set maximum number of tests
    #[must_use]
    #[allow(clippy::missing_const_for_fn)]
    pub fn with_max_tests(mut self, max_tests: usize) -> Self {
        self.max_tests = max_tests;
        self
    }

    /// Set minimum failure probability
    #[must_use]
    #[allow(clippy::missing_const_for_fn)]
    pub fn with_min_failure_prob(mut self, min_failure_prob: f64) -> Self {
        self.min_failure_prob = min_failure_prob;
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::receipt::TimingMeasurement;

    #[test]
    fn test_continuous_learner_records_executions() {
        let mut learner = ContinuousLearner::new();
        assert_eq!(learner.history().len(), 0);

        let timing = TimingMeasurement::new(5, 1, "hot".to_string(), true, 8);
        let contract = TestContract::hot_path("test1", &["module1"]);
        let receipt = TestReceipt::from_contract(&contract, timing, TestOutcome::Pass);

        learner.record_execution(&receipt);
        assert_eq!(learner.history().len(), 1);
    }

    #[test]
    fn test_prediction_for_unknown_contract() {
        let learner = ContinuousLearner::new();
        let contract = TestContract::hot_path("test_unknown", &["unknown::module"]);
        let prediction = learner.predict(&contract);

        assert_eq!(prediction.confidence, 0.0);
        assert_eq!(prediction.recommendation, Recommendation::RunNormally);
    }

    #[test]
    fn test_execution_order_optimization() {
        let learner = ContinuousLearner::new();
        let contracts = vec![
            TestContract::hot_path("test1", &["module1"]),
            TestContract::hot_path("test2", &["module2"]),
        ];

        let order = learner.optimize_execution_order(&contracts);
        assert_eq!(order.len(), 2);
    }
}
