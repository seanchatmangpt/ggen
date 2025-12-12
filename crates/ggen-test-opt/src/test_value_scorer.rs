//! Test value scoring algorithm implementation
//!
//! This module implements the 80/20 Pareto test value scoring system.
//! Composite score = 40% failure_freq + 25% coverage + 15% speed + 15% criticality - 5% budget_penalty
//!
//! Industry-validated weights based on Microsoft Research and Google Testing Blog evidence.

use crate::types::{OptimizationResult, ScoringWeights, TestId, TestValueScore};
use std::collections::HashMap;

/// Test value scorer using weighted composite algorithm
#[derive(Debug)]
pub struct TestValueScorer {
    /// Scoring weights (default: industry-validated)
    weights: ScoringWeights,
}

impl TestValueScorer {
    /// Create a new test value scorer with default weights
    ///
    /// Default weights:
    /// - failure_freq: 0.40 (40%)
    /// - coverage: 0.25 (25%)
    /// - speed: 0.15 (15%)
    /// - criticality: 0.15 (15%)
    /// - budget_penalty: 0.05 (5%)
    pub fn new() -> Self {
        Self {
            weights: ScoringWeights::default(),
        }
    }

    /// Create scorer with custom weights
    ///
    /// # Arguments
    /// * `weights` - Custom scoring weights
    ///
    /// # Errors
    /// Returns `OptimizationError::InvalidWeights` if weights don't sum to 1.0 (±0.01)
    pub fn with_weights(weights: ScoringWeights) -> OptimizationResult<Self> {
        // Validate weights sum to 1.0 (excluding budget_penalty which is subtracted)
        let sum = weights.failure_freq + weights.coverage + weights.speed + weights.criticality;
        if (sum - 1.0).abs() > 0.01 {
            return Err(crate::types::OptimizationError::InvalidWeights(format!(
                "Weights sum to {}, expected 1.0 (±0.01)",
                sum
            )));
        }
        Ok(Self { weights })
    }

    /// Calculate failure frequency score (0.0-100.0)
    ///
    /// Formula: (failure_count / total_runs) * 100
    /// Higher score = more valuable (catches more bugs)
    ///
    /// # Arguments
    /// * `failure_count` - Number of times test failed historically
    /// * `total_runs` - Total number of test runs
    #[must_use]
    pub fn calculate_failure_freq_score(&self, failure_count: u32, total_runs: u32) -> f64 {
        if total_runs == 0 {
            return 0.0;
        }
        (f64::from(failure_count) / f64::from(total_runs)) * 100.0
    }

    /// Calculate code coverage score (0.0-100.0)
    ///
    /// Formula: (unique_lines_covered / total_codebase_lines) * 100
    /// Higher score = more valuable (covers more code)
    ///
    /// # Arguments
    /// * `unique_lines_covered` - Number of unique lines covered by this test
    /// * `total_codebase_lines` - Total lines in codebase
    #[must_use]
    pub fn calculate_coverage_score(
        &self, unique_lines_covered: usize, total_codebase_lines: usize,
    ) -> f64 {
        if total_codebase_lines == 0 {
            return 0.0;
        }
        (unique_lines_covered as f64 / total_codebase_lines as f64) * 100.0
    }

    /// Calculate speed score (0.0-100.0) - normalized inverse of execution time
    ///
    /// Formula: 100.0 - ((exec_time_ms / max_time_ms) * 100.0)
    /// Higher score = faster test = more valuable (runs more frequently)
    ///
    /// # Arguments
    /// * `exec_time_ms` - Test execution time in milliseconds
    /// * `max_time_ms` - Maximum observed execution time across all tests
    #[must_use]
    pub fn calculate_speed_score(&self, exec_time_ms: u64, max_time_ms: u64) -> f64 {
        if max_time_ms == 0 {
            return 100.0;
        }
        let normalized = (exec_time_ms as f64 / max_time_ms as f64) * 100.0;
        (100.0 - normalized).max(0.0)
    }

    /// Calculate criticality score (0.0-100.0)
    ///
    /// Based on critical path coverage:
    /// - RDF parsing: 100.0
    /// - Ontology projection: 90.0
    /// - Code generation: 85.0
    /// - ggen.toml config: 95.0
    /// - CLI commands: 70.0
    /// - Utilities: 50.0
    /// - Other: 30.0
    ///
    /// # Arguments
    /// * `critical_paths` - List of critical paths covered by this test
    #[must_use]
    pub fn calculate_criticality_score(&self, critical_paths: &[String]) -> f64 {
        if critical_paths.is_empty() {
            return 30.0; // Default for non-critical paths
        }

        let scores: Vec<f64> = critical_paths
            .iter()
            .map(|path| self.path_criticality_score(path))
            .collect();

        // Return maximum criticality (most critical path covered)
        scores.into_iter().fold(0.0, f64::max)
    }

    /// Map critical path to criticality score
    fn path_criticality_score(&self, path: &str) -> f64 {
        if path.contains("rdf") || path.contains("parser") {
            100.0
        } else if path.contains("ggen.toml") || path.contains("config") {
            95.0
        } else if path.contains("ontology") || path.contains("projection") {
            90.0
        } else if path.contains("generator") || path.contains("codegen") {
            85.0
        } else if path.contains("cli") || path.contains("command") {
            70.0
        } else if path.contains("util") {
            50.0
        } else {
            30.0
        }
    }

    /// Calculate budget penalty (0.0-100.0)
    ///
    /// Formula: (excess_time_ms / budget_ms) * 100.0
    /// Higher penalty = test exceeds budget more severely
    ///
    /// # Arguments
    /// * `exec_time_ms` - Test execution time
    /// * `budget_ms` - Performance budget for this test type
    #[must_use]
    pub fn calculate_budget_penalty(&self, exec_time_ms: u64, budget_ms: u64) -> f64 {
        if exec_time_ms <= budget_ms {
            return 0.0; // No penalty if within budget
        }

        let excess_ms = exec_time_ms - budget_ms;
        (excess_ms as f64 / budget_ms as f64) * 100.0
    }

    /// Calculate composite test value score
    ///
    /// Formula: (failure_freq * 0.40) + (coverage * 0.25) + (speed * 0.15)
    ///          + (criticality * 0.15) - (budget_penalty * 0.05)
    ///
    /// # Arguments
    /// * `test_id` - Test identifier
    /// * `failure_freq` - Failure frequency score (0.0-100.0)
    /// * `coverage` - Coverage score (0.0-100.0)
    /// * `speed` - Speed score (0.0-100.0)
    /// * `criticality` - Criticality score (0.0-100.0)
    /// * `budget_penalty` - Budget penalty (0.0-100.0)
    #[must_use]
    pub fn calculate_composite_score(
        &self, test_id: TestId, failure_freq: f64, coverage: f64, speed: f64, criticality: f64,
        budget_penalty: f64,
    ) -> TestValueScore {
        let composite_value = (failure_freq * self.weights.failure_freq)
            + (coverage * self.weights.coverage)
            + (speed * self.weights.speed)
            + (criticality * self.weights.criticality)
            - (budget_penalty * self.weights.budget_penalty);

        TestValueScore {
            test_id,
            failure_freq_score: failure_freq,
            coverage_score: coverage,
            speed_score: speed,
            criticality_score: criticality,
            budget_penalty,
            composite_value: composite_value.max(0.0), // Floor at 0.0
        }
    }

    /// Batch score multiple tests
    ///
    /// # Arguments
    /// * `test_data` - Map of test_id to (failure_count, total_runs, unique_lines, exec_time_ms, critical_paths)
    /// * `total_codebase_lines` - Total lines in codebase
    /// * `max_exec_time_ms` - Maximum execution time across all tests
    /// * `budget_ms` - Performance budget for test type
    pub fn score_tests(
        &self,
        test_data: &HashMap<
            TestId,
            (u32, u32, usize, u64, Vec<String>), // (fail_count, runs, lines, time, paths)
        >,
        total_codebase_lines: usize, max_exec_time_ms: u64, budget_ms: u64,
    ) -> Vec<TestValueScore> {
        test_data
            .iter()
            .map(|(test_id, (fail_count, runs, lines, time, paths))| {
                let failure_freq = self.calculate_failure_freq_score(*fail_count, *runs);
                let coverage = self.calculate_coverage_score(*lines, total_codebase_lines);
                let speed = self.calculate_speed_score(*time, max_exec_time_ms);
                let criticality = self.calculate_criticality_score(paths);
                let penalty = self.calculate_budget_penalty(*time, budget_ms);

                self.calculate_composite_score(
                    test_id.clone(),
                    failure_freq,
                    coverage,
                    speed,
                    criticality,
                    penalty,
                )
            })
            .collect()
    }
}

impl Default for TestValueScorer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scorer_creation_with_default_weights() {
        let scorer = TestValueScorer::new();
        assert_eq!(scorer.weights.failure_freq, 0.40);
        assert_eq!(scorer.weights.coverage, 0.25);
        assert_eq!(scorer.weights.speed, 0.15);
        assert_eq!(scorer.weights.criticality, 0.15);
        assert_eq!(scorer.weights.budget_penalty, 0.05);
    }

    #[test]
    fn test_failure_freq_score_100_percent() {
        let scorer = TestValueScorer::new();
        let score = scorer.calculate_failure_freq_score(100, 100);
        assert_eq!(score, 100.0);
    }

    #[test]
    fn test_failure_freq_score_50_percent() {
        let scorer = TestValueScorer::new();
        let score = scorer.calculate_failure_freq_score(50, 100);
        assert_eq!(score, 50.0);
    }

    #[test]
    fn test_failure_freq_score_zero_runs() {
        let scorer = TestValueScorer::new();
        let score = scorer.calculate_failure_freq_score(10, 0);
        assert_eq!(score, 0.0);
    }

    #[test]
    fn test_coverage_score_100_percent() {
        let scorer = TestValueScorer::new();
        let score = scorer.calculate_coverage_score(1000, 1000);
        assert_eq!(score, 100.0);
    }

    #[test]
    fn test_coverage_score_25_percent() {
        let scorer = TestValueScorer::new();
        let score = scorer.calculate_coverage_score(250, 1000);
        assert_eq!(score, 25.0);
    }

    #[test]
    fn test_speed_score_fastest() {
        let scorer = TestValueScorer::new();
        let score = scorer.calculate_speed_score(1, 1000);
        assert!((score - 99.9).abs() < 0.1); // ~99.9 for fastest
    }

    #[test]
    fn test_speed_score_slowest() {
        let scorer = TestValueScorer::new();
        let score = scorer.calculate_speed_score(1000, 1000);
        assert_eq!(score, 0.0); // Slowest gets 0.0
    }

    #[test]
    fn test_criticality_score_rdf_parsing() {
        let scorer = TestValueScorer::new();
        let score = scorer.calculate_criticality_score(&["crates/ggen-rdf/src/parser.rs".into()]);
        assert_eq!(score, 100.0);
    }

    #[test]
    fn test_criticality_score_ggen_toml() {
        let scorer = TestValueScorer::new();
        let score =
            scorer.calculate_criticality_score(&["crates/ggen-config/src/ggen.toml".into()]);
        assert_eq!(score, 95.0);
    }

    #[test]
    fn test_budget_penalty_within_budget() {
        let scorer = TestValueScorer::new();
        let penalty = scorer.calculate_budget_penalty(500, 1000);
        assert_eq!(penalty, 0.0);
    }

    #[test]
    fn test_budget_penalty_50_percent_over() {
        let scorer = TestValueScorer::new();
        let penalty = scorer.calculate_budget_penalty(1500, 1000);
        assert_eq!(penalty, 50.0);
    }

    #[test]
    fn test_composite_score_high_value_test() {
        let scorer = TestValueScorer::new();
        let test_id = TestId::new("high_value_test").unwrap();

        let score = scorer.calculate_composite_score(
            test_id, 80.0,  // High failure freq
            60.0,  // Good coverage
            90.0,  // Fast
            100.0, // Critical path
            0.0,   // No penalty
        );

        // Expected: (80*0.40) + (60*0.25) + (90*0.15) + (100*0.15) - 0 = 74.5
        assert!((score.composite_value - 74.5).abs() < 0.1);
    }

    #[test]
    fn test_composite_score_with_penalty() {
        let scorer = TestValueScorer::new();
        let test_id = TestId::new("slow_test").unwrap();

        let score = scorer.calculate_composite_score(
            test_id, 50.0,  // Medium failure freq
            40.0,  // Medium coverage
            20.0,  // Slow
            70.0,  // Medium criticality
            100.0, // Heavy penalty
        );

        // Expected: (50*0.40) + (40*0.25) + (20*0.15) + (70*0.15) - (100*0.05) = 38.5
        assert!((score.composite_value - 38.5).abs() < 0.1);
    }
}
