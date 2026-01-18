//! 80/20 Pareto test selection implementation
//!
//! This module implements intelligent test selection using the Pareto principle:
//! Select the top 20% of tests (by value score) that catch 80%+ of bugs.
//!
//! Follows constitutional principle of zero-cost abstractions and type-first thinking.

use crate::types::{OptResult, TestValueScore};
use crate::TestId;
use std::collections::{HashMap, HashSet};

/// Pareto selector for 80/20 test selection
#[derive(Debug)]
pub struct ParetoSelector {
    /// Minimum bug detection rate to maintain (default: 0.80)
    min_bug_detection_rate: f64,
    /// Target test count (default: 200 for ggen's 1,178 tests)
    target_test_count: usize,
}

impl ParetoSelector {
    /// Create new Pareto selector with defaults
    ///
    /// Default configuration:
    /// - min_bug_detection_rate: 0.80 (80%)
    /// - target_test_count: 200 (17% of 1,178 tests)
    pub fn new() -> Self {
        Self {
            min_bug_detection_rate: 0.80,
            target_test_count: 200,
        }
    }

    /// Create selector with custom configuration
    pub fn with_config(min_bug_detection_rate: f64, target_test_count: usize) -> Self {
        Self {
            min_bug_detection_rate,
            target_test_count,
        }
    }

    /// Rank tests by composite value score (descending order)
    ///
    /// # Arguments
    /// * `scores` - Vector of test value scores
    ///
    /// # Returns
    /// Sorted vector with highest value tests first
    pub fn rank_tests(&self, mut scores: Vec<TestValueScore>) -> Vec<TestValueScore> {
        scores.sort_by(|a, b| {
            b.composite_value
                .partial_cmp(&a.composite_value)
                .unwrap_or(std::cmp::Ordering::Equal)
        });
        scores
    }

    /// Select top N tests from ranked list
    ///
    /// # Arguments
    /// * `ranked_scores` - Pre-sorted test scores (highest value first)
    ///
    /// # Returns
    /// Top N tests by value score
    #[must_use]
    pub fn select_top_n(&self, ranked_scores: &[TestValueScore]) -> Vec<TestValueScore> {
        ranked_scores
            .iter()
            .take(self.target_test_count)
            .cloned()
            .collect()
    }

    /// Validate that selected tests maintain minimum bug detection rate
    ///
    /// This uses a simplified heuristic based on failure frequency scores.
    /// A more sophisticated implementation would use historical bug detection data.
    ///
    /// # Arguments
    /// * `selected_tests` - Tests selected by Pareto algorithm
    /// * `all_tests` - Complete test suite
    ///
    /// # Returns
    /// `Ok(detection_rate)` if validation passes, `Err` otherwise
    pub fn validate_coverage(
        &self, selected_tests: &[TestValueScore], all_tests: &[TestValueScore],
    ) -> OptResult<f64> {
        if selected_tests.is_empty() {
            return Err(crate::types::OptimizationError::NoTestsSelected(
                "Cannot validate coverage with zero selected tests".into(),
            ));
        }

        // Calculate weighted bug detection based on failure frequency
        let selected_detection: f64 = selected_tests.iter().map(|t| t.failure_freq_score).sum();

        let total_detection: f64 = all_tests.iter().map(|t| t.failure_freq_score).sum();

        let detection_rate = if total_detection > 0.0 {
            selected_detection / total_detection
        } else {
            0.0
        };

        if detection_rate < self.min_bug_detection_rate {
            return Err(crate::types::OptimizationError::InsufficientCoverage(
                format!(
                    "Bug detection rate {:.1}% below threshold {:.1}%",
                    detection_rate * 100.0,
                    self.min_bug_detection_rate * 100.0
                ),
            ));
        }

        Ok(detection_rate)
    }

    /// Generate justification for test selection decisions
    ///
    /// # Arguments
    /// * `selected_tests` - Tests included in optimized suite
    /// * `excluded_tests` - Tests excluded from optimized suite
    ///
    /// # Returns
    /// Map of test_id to justification string
    pub fn generate_justification(
        &self, selected_tests: &[TestValueScore], excluded_tests: &[TestValueScore],
    ) -> HashMap<TestId, String> {
        let mut justifications = HashMap::new();

        // Justifications for selected tests
        for test in selected_tests {
            let reason = self.justify_inclusion(test);
            justifications.insert(test.test_id.clone(), reason);
        }

        // Justifications for excluded tests
        for test in excluded_tests {
            let reason = self.justify_exclusion(test);
            justifications.insert(test.test_id.clone(), reason);
        }

        justifications
    }

    /// Justify why test was included
    fn justify_inclusion(&self, test: &TestValueScore) -> String {
        let mut reasons = Vec::new();

        if test.failure_freq_score >= 50.0 {
            reasons.push(format!(
                "high failure rate ({:.1}%)",
                test.failure_freq_score
            ));
        }

        if test.coverage_score >= 50.0 {
            reasons.push(format!("good coverage ({:.1}%)", test.coverage_score));
        }

        if test.criticality_score >= 85.0 {
            reasons.push(format!(
                "critical path coverage ({:.1})",
                test.criticality_score
            ));
        }

        if test.speed_score >= 70.0 {
            reasons.push("fast execution".into());
        }

        if reasons.is_empty() {
            format!("Composite value: {:.1}", test.composite_value)
        } else {
            format!(
                "INCLUDED (value: {:.1}) - {}",
                test.composite_value,
                reasons.join(", ")
            )
        }
    }

    /// Justify why test was excluded
    fn justify_exclusion(&self, test: &TestValueScore) -> String {
        let mut reasons = Vec::new();

        if test.failure_freq_score < 10.0 {
            reasons.push("low failure rate (rarely catches bugs)");
        }

        if test.coverage_score < 10.0 {
            reasons.push("minimal coverage (redundant with other tests)");
        }

        if test.criticality_score < 50.0 {
            reasons.push("non-critical path");
        }

        if test.budget_penalty > 50.0 {
            reasons.push("slow execution (high budget penalty)");
        }

        if reasons.is_empty() {
            format!(
                "EXCLUDED (value: {:.1}) - below selection threshold",
                test.composite_value
            )
        } else {
            format!(
                "EXCLUDED (value: {:.1}) - {}",
                test.composite_value,
                reasons.join(", ")
            )
        }
    }

    /// Execute complete Pareto selection workflow
    ///
    /// # Arguments
    /// * `all_scores` - Complete set of test value scores
    ///
    /// # Returns
    /// Selected tests with validation and justifications
    pub fn execute_selection(
        &self, all_scores: Vec<TestValueScore>,
    ) -> OptResult<ParetoSelectionResult> {
        // Step 1: Rank all tests by value
        let ranked = self.rank_tests(all_scores);

        // Step 2: Select top N tests
        let selected = self.select_top_n(&ranked);

        // Step 3: Validate coverage
        let detection_rate = self.validate_coverage(&selected, &ranked)?;

        // Step 4: Generate justifications
        let excluded: Vec<_> = ranked
            .iter()
            .skip(self.target_test_count)
            .cloned()
            .collect();
        let justifications = self.generate_justification(&selected, &excluded);

        Ok(ParetoSelectionResult {
            selected_tests: selected,
            excluded_tests: excluded,
            bug_detection_rate: detection_rate,
            justifications,
            total_tests: ranked.len(),
            selected_count: self.target_test_count,
        })
    }
}

impl Default for ParetoSelector {
    fn default() -> Self {
        Self::new()
    }
}

/// Result of Pareto selection process
#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct ParetoSelectionResult {
    /// Tests included in optimized suite
    pub selected_tests: Vec<TestValueScore>,
    /// Tests excluded from optimized suite
    pub excluded_tests: Vec<TestValueScore>,
    /// Estimated bug detection rate
    pub bug_detection_rate: f64,
    /// Justifications for each test (selected + excluded)
    pub justifications: HashMap<TestId, String>,
    /// Total number of tests analyzed
    pub total_tests: usize,
    /// Number of tests selected
    pub selected_count: usize,
}

impl ParetoSelectionResult {
    /// Calculate reduction percentage
    #[must_use]
    pub fn reduction_percentage(&self) -> f64 {
        if self.total_tests == 0 {
            return 0.0;
        }
        ((self.total_tests - self.selected_count) as f64 / self.total_tests as f64) * 100.0
    }

    /// Get selected test IDs
    pub fn selected_test_ids(&self) -> HashSet<TestId> {
        self.selected_tests
            .iter()
            .map(|t| t.test_id.clone())
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_score(id: &str, composite: f64, failure_freq: f64) -> TestValueScore {
        TestValueScore {
            test_id: TestId::new(id).unwrap(),
            failure_freq_score: failure_freq,
            coverage_score: 50.0,
            speed_score: 70.0,
            criticality_score: 60.0,
            budget_penalty: 0.0,
            composite_value: composite,
        }
    }

    #[test]
    fn test_selector_creation() {
        let selector = ParetoSelector::new();
        assert_eq!(selector.min_bug_detection_rate, 0.80);
        assert_eq!(selector.target_test_count, 200);
    }

    #[test]
    fn test_rank_tests_descending_order() {
        let selector = ParetoSelector::new();
        let scores = vec![
            create_test_score("test1", 50.0, 30.0),
            create_test_score("test2", 80.0, 50.0),
            create_test_score("test3", 30.0, 10.0),
        ];

        let ranked = selector.rank_tests(scores);

        assert_eq!(ranked[0].test_id.as_str(), "test2"); // Highest value
        assert_eq!(ranked[1].test_id.as_str(), "test1");
        assert_eq!(ranked[2].test_id.as_str(), "test3"); // Lowest value
    }

    #[test]
    fn test_select_top_n() {
        let selector = ParetoSelector::with_config(0.80, 2);
        let scores = vec![
            create_test_score("test1", 80.0, 50.0),
            create_test_score("test2", 70.0, 40.0),
            create_test_score("test3", 60.0, 30.0),
        ];

        let ranked = selector.rank_tests(scores);
        let selected = selector.select_top_n(&ranked);

        assert_eq!(selected.len(), 2);
        assert_eq!(selected[0].test_id.as_str(), "test1");
        assert_eq!(selected[1].test_id.as_str(), "test2");
    }

    #[test]
    fn test_validate_coverage_success() {
        let selector = ParetoSelector::with_config(0.50, 2);

        let all_tests = vec![
            create_test_score("test1", 80.0, 50.0), // High failure freq
            create_test_score("test2", 70.0, 30.0),
            create_test_score("test3", 60.0, 20.0),
        ];

        let selected = vec![
            create_test_score("test1", 80.0, 50.0),
            create_test_score("test2", 70.0, 30.0),
        ];

        let result = selector.validate_coverage(&selected, &all_tests);
        assert!(result.is_ok());

        let rate = result.unwrap();
        assert!(rate >= 0.50); // Should be (50+30)/(50+30+20) = 0.80
    }

    #[test]
    fn test_validate_coverage_failure() {
        let selector = ParetoSelector::with_config(0.90, 1);

        let all_tests = vec![
            create_test_score("test1", 80.0, 30.0),
            create_test_score("test2", 70.0, 70.0), // High failure freq but not selected
        ];

        let selected = vec![create_test_score("test1", 80.0, 30.0)];

        let result = selector.validate_coverage(&selected, &all_tests);
        assert!(result.is_err()); // Should fail (30/100 = 0.30 < 0.90)
    }

    #[test]
    fn test_execute_selection_workflow() {
        let selector = ParetoSelector::with_config(0.60, 2);

        let scores = vec![
            create_test_score("test1", 80.0, 50.0),
            create_test_score("test2", 70.0, 40.0),
            create_test_score("test3", 60.0, 10.0),
        ];

        let result = selector.execute_selection(scores);
        assert!(result.is_ok());

        let selection = result.unwrap();
        assert_eq!(selection.selected_count, 2);
        assert_eq!(selection.total_tests, 3);
        assert!(selection.bug_detection_rate >= 0.60);
        assert_eq!(selection.reduction_percentage(), (1.0 / 3.0) * 100.0);
    }
}
