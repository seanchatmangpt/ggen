use crate::{BuildResult, Crate, TestResult};
use std::collections::{HashMap, VecDeque};
use std::time::Instant;

/// Agents coordinating multi-workspace operations
pub struct BuildCoordinator;

impl BuildCoordinator {
    /// Coordinate parallel builds respecting dependencies
    pub fn coordinate_builds(
        crates: &[Crate],
        build_order: &[String],
    ) -> BuildCoordinationPlan {
        let mut plan = BuildCoordinationPlan::new();

        for crate_name in build_order {
            if let Some(crate_info) = crates.iter().find(|c| c.name == *crate_name) {
                plan.add_build_step(BuildStep {
                    crate_name: crate_info.name.clone(),
                    dependencies: crate_info.dependencies.clone(),
                    parallel_compatible: Self::can_parallelize(crate_info, crates),
                });
            }
        }

        plan
    }

    fn can_parallelize(crate_info: &Crate, all_crates: &[Crate]) -> bool {
        // A crate can be parallelized if its dependencies are independent
        crate_info.dependencies.len() <= 1
    }
}

/// Plan for coordinating builds
#[derive(Debug, Clone)]
pub struct BuildCoordinationPlan {
    pub steps: Vec<BuildStep>,
    pub parallel_groups: Vec<Vec<String>>,
}

impl BuildCoordinationPlan {
    pub fn new() -> Self {
        Self {
            steps: Vec::new(),
            parallel_groups: Vec::new(),
        }
    }

    pub fn add_build_step(&mut self, step: BuildStep) {
        self.steps.push(step);
    }

    pub fn compute_parallel_groups(&mut self) {
        let mut groups = Vec::new();
        let mut current_group = Vec::new();

        for step in &self.steps {
            if step.parallel_compatible && current_group.len() < 4 {
                current_group.push(step.crate_name.clone());
            } else {
                if !current_group.is_empty() {
                    groups.push(current_group.clone());
                    current_group.clear();
                }
                current_group.push(step.crate_name.clone());
            }
        }

        if !current_group.is_empty() {
            groups.push(current_group);
        }

        self.parallel_groups = groups;
    }
}

impl Default for BuildCoordinationPlan {
    fn default() -> Self {
        Self::new()
    }
}

/// Single build step
#[derive(Debug, Clone)]
pub struct BuildStep {
    pub crate_name: String,
    pub dependencies: Vec<String>,
    pub parallel_compatible: bool,
}

/// Test aggregation across crates
pub struct TestAggregator;

impl TestAggregator {
    /// Detect flaky tests (tests that fail intermittently)
    pub fn detect_flaky_tests(
        results: &[TestResult],
        threshold: f32,
    ) -> Vec<FlakyTestReport> {
        let mut flaky_reports = Vec::new();

        for result in results {
            if result.failed > 0 && result.total_tests() > 0 {
                let failure_rate = result.failed as f32 / result.total_tests() as f32;
                if failure_rate < threshold {
                    flaky_reports.push(FlakyTestReport {
                        crate_name: result.crate_name.clone(),
                        failure_rate,
                        failed_count: result.failed,
                        total_count: result.total_tests(),
                    });
                }
            }
        }

        flaky_reports.sort_by(|a, b| {
            b.failure_rate
                .partial_cmp(&a.failure_rate)
                .unwrap_or(std::cmp::Ordering::Equal)
        });
        flaky_reports
    }

    /// Generate unified test report
    pub fn generate_report(
        results: &[TestResult],
        build_results: &[BuildResult],
    ) -> AggregatedTestReport {
        let total_tests: usize = results.iter().map(|r| r.total_tests()).sum();
        let total_passed: usize = results.iter().map(|r| r.passed).sum();
        let total_failed: usize = results.iter().map(|r| r.failed).sum();
        let total_skipped: usize = results.iter().map(|r| r.skipped).sum();
        let avg_coverage: f32 = if results.is_empty() {
            0.0
        } else {
            results.iter().map(|r| r.coverage_percentage).sum::<f32>() / results.len() as f32
        };

        let total_duration_ms: u64 = results.iter().map(|r| r.duration_ms).sum();
        let build_duration_ms: u64 = build_results.iter().map(|r| r.duration_ms).sum();

        AggregatedTestReport {
            total_crates: results.len(),
            total_tests,
            total_passed,
            total_failed,
            total_skipped,
            average_coverage: avg_coverage,
            test_duration_ms: total_duration_ms,
            build_duration_ms,
            success_rate: if total_tests == 0 {
                100.0
            } else {
                (total_passed as f32 / total_tests as f32) * 100.0
            },
            crate_results: results.to_vec(),
        }
    }

    /// Identify coverage gaps across workspace
    pub fn identify_coverage_gaps(
        results: &[TestResult],
        minimum_target: f32,
    ) -> Vec<CoverageGap> {
        let mut gaps = Vec::new();

        for result in results {
            if result.coverage_percentage < minimum_target {
                gaps.push(CoverageGap {
                    crate_name: result.crate_name.clone(),
                    current_coverage: result.coverage_percentage,
                    target_coverage: minimum_target,
                    gap_percentage: minimum_target - result.coverage_percentage,
                });
            }
        }

        gaps.sort_by(|a, b| {
            b.gap_percentage
                .partial_cmp(&a.gap_percentage)
                .unwrap_or(std::cmp::Ordering::Equal)
        });
        gaps
    }
}

/// Report of flaky tests
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct FlakyTestReport {
    pub crate_name: String,
    pub failure_rate: f32,
    pub failed_count: usize,
    pub total_count: usize,
}

/// Aggregated test report across workspace
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AggregatedTestReport {
    pub total_crates: usize,
    pub total_tests: usize,
    pub total_passed: usize,
    pub total_failed: usize,
    pub total_skipped: usize,
    pub average_coverage: f32,
    pub test_duration_ms: u64,
    pub build_duration_ms: u64,
    pub success_rate: f32,
    pub crate_results: Vec<TestResult>,
}

/// Coverage gap in a crate
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct CoverageGap {
    pub crate_name: String,
    pub current_coverage: f32,
    pub target_coverage: f32,
    pub gap_percentage: f32,
}

/// Metrics tracking for workspace health
pub struct MetricsTracker {
    history: VecDeque<MetricsSnapshot>,
}

/// Snapshot of metrics at a point in time
#[derive(Debug, Clone)]
pub struct MetricsSnapshot {
    pub timestamp: Instant,
    pub success_rate: f32,
    pub average_coverage: f32,
    pub total_duration_ms: u64,
}

impl MetricsTracker {
    pub fn new() -> Self {
        Self {
            history: VecDeque::new(),
        }
    }

    pub fn record_snapshot(&mut self, snapshot: MetricsSnapshot) {
        self.history.push_back(snapshot);
        // Keep last 100 snapshots
        if self.history.len() > 100 {
            self.history.pop_front();
        }
    }

    pub fn calculate_trend(&self) -> MetricsTrend {
        if self.history.len() < 2 {
            return MetricsTrend::Stable;
        }

        let first = &self.history[0];
        let last = &self.history[self.history.len() - 1];

        let coverage_delta = last.average_coverage - first.average_coverage;
        let success_delta = last.success_rate - first.success_rate;

        if coverage_delta > 5.0 || success_delta > 5.0 {
            MetricsTrend::Improving
        } else if coverage_delta < -5.0 || success_delta < -5.0 {
            MetricsTrend::Degrading
        } else {
            MetricsTrend::Stable
        }
    }

    pub fn get_history(&self) -> Vec<MetricsSnapshot> {
        self.history.iter().cloned().collect()
    }
}

impl Default for MetricsTracker {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum MetricsTrend {
    Improving,
    Degrading,
    Stable,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_build_coordinator_plan_creation() {
        let crates = vec![
            Crate::new("core".to_string(), "crates/core".to_string()),
            Crate::new("cli".to_string(), "crates/cli".to_string())
                .with_dependencies(vec!["core".to_string()]),
        ];

        let plan = BuildCoordinator::coordinate_builds(&crates, &["core".to_string(), "cli".to_string()]);
        assert_eq!(plan.steps.len(), 2);
    }

    #[test]
    fn test_build_coordination_parallel_groups() {
        let mut plan = BuildCoordinationPlan::new();
        plan.add_build_step(BuildStep {
            crate_name: "core".to_string(),
            dependencies: vec![],
            parallel_compatible: true,
        });
        plan.add_build_step(BuildStep {
            crate_name: "cli".to_string(),
            dependencies: vec!["core".to_string()],
            parallel_compatible: true,
        });

        plan.compute_parallel_groups();
        assert!(!plan.parallel_groups.is_empty());
    }

    #[test]
    fn test_flaky_test_detection() {
        let results = vec![
            TestResult::new("core".to_string(), 98, 2, 0, 2000, 90.0),
            TestResult::new("cli".to_string(), 50, 0, 0, 1500, 80.0),
        ];

        let flaky = TestAggregator::detect_flaky_tests(&results, 0.1);
        assert!(!flaky.is_empty());
        assert_eq!(flaky[0].crate_name, "core");
    }

    #[test]
    fn test_aggregated_test_report() {
        let test_results = vec![
            TestResult::new("core".to_string(), 30, 0, 0, 2000, 90.0),
            TestResult::new("cli".to_string(), 20, 0, 0, 1500, 80.0),
        ];

        let build_results = vec![
            BuildResult::success("core".to_string(), 1000),
            BuildResult::success("cli".to_string(), 1200),
        ];

        let report = TestAggregator::generate_report(&test_results, &build_results);
        assert_eq!(report.total_crates, 2);
        assert_eq!(report.total_tests, 50);
        assert_eq!(report.success_rate, 100.0);
        assert_eq!(report.average_coverage, 85.0);
    }

    #[test]
    fn test_coverage_gap_identification() {
        let results = vec![
            TestResult::new("core".to_string(), 30, 0, 0, 2000, 95.0),
            TestResult::new("cli".to_string(), 20, 0, 0, 1500, 60.0),
        ];

        let gaps = TestAggregator::identify_coverage_gaps(&results, 85.0);
        assert_eq!(gaps.len(), 1);
        assert_eq!(gaps[0].crate_name, "cli");
    }

    #[test]
    fn test_metrics_tracker_trend_improving() {
        let mut tracker = MetricsTracker::new();

        tracker.record_snapshot(MetricsSnapshot {
            timestamp: Instant::now(),
            success_rate: 80.0,
            average_coverage: 70.0,
            total_duration_ms: 5000,
        });

        tracker.record_snapshot(MetricsSnapshot {
            timestamp: Instant::now(),
            success_rate: 95.0,
            average_coverage: 85.0,
            total_duration_ms: 5000,
        });

        assert!(matches!(tracker.calculate_trend(), MetricsTrend::Improving));
    }

    #[test]
    fn test_metrics_tracker_trend_degrading() {
        let mut tracker = MetricsTracker::new();

        tracker.record_snapshot(MetricsSnapshot {
            timestamp: Instant::now(),
            success_rate: 95.0,
            average_coverage: 85.0,
            total_duration_ms: 5000,
        });

        tracker.record_snapshot(MetricsSnapshot {
            timestamp: Instant::now(),
            success_rate: 70.0,
            average_coverage: 65.0,
            total_duration_ms: 5000,
        });

        assert!(matches!(tracker.calculate_trend(), MetricsTrend::Degrading));
    }

    #[test]
    fn test_metrics_tracker_history() {
        let mut tracker = MetricsTracker::new();

        for i in 0..5 {
            tracker.record_snapshot(MetricsSnapshot {
                timestamp: Instant::now(),
                success_rate: 80.0 + (i as f32),
                average_coverage: 70.0 + (i as f32),
                total_duration_ms: 5000,
            });
        }

        let history = tracker.get_history();
        assert_eq!(history.len(), 5);
    }
}
