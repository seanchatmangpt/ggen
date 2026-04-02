//! Test coverage metrics and gap analysis

use serde::{Deserialize, Serialize};

/// Coverage metrics for a module/crate
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoverageMetrics {
    /// Module/crate name
    pub name: String,
    /// Coverage percentage (0-100)
    pub percentage: f64,
    /// Lines covered
    pub lines_covered: u32,
    /// Total lines
    pub total_lines: u32,
    /// Functions covered
    pub functions_covered: u32,
    /// Total functions
    pub total_functions: u32,
    /// Branches covered
    pub branches_covered: u32,
    /// Total branches
    pub total_branches: u32,
}

impl CoverageMetrics {
    /// Create new metrics
    pub fn new(name: String) -> Self {
        Self {
            name,
            percentage: 0.0,
            lines_covered: 0,
            total_lines: 0,
            functions_covered: 0,
            total_functions: 0,
            branches_covered: 0,
            total_branches: 0,
        }
    }

    /// Calculate percentage from coverage data
    pub fn calculate_percentage(&mut self) {
        if self.total_lines > 0 {
            self.percentage = (self.lines_covered as f64 / self.total_lines as f64) * 100.0;
        }
    }

    /// Is above threshold
    pub fn meets_threshold(&self, threshold: f64) -> bool {
        self.percentage >= threshold
    }
}

/// Coverage gap analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoverageGapAnalysis {
    /// Total coverage percentage
    pub total_coverage: f64,
    /// Coverage by module
    pub module_coverage: Vec<CoverageMetrics>,
    /// Modules below threshold
    pub gap_modules: Vec<GapModule>,
    /// Target threshold
    pub threshold: f64,
    /// Total gap points
    pub total_gap_points: f64,
}

/// Module with coverage gap
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GapModule {
    /// Module name
    pub name: String,
    /// Current coverage
    pub current_coverage: f64,
    /// Gap to threshold
    pub gap_to_threshold: f64,
    /// Recommendation
    pub recommendation: String,
}

impl CoverageGapAnalysis {
    /// Create new analysis
    pub fn new(threshold: f64) -> Self {
        Self {
            total_coverage: 0.0,
            module_coverage: Vec::new(),
            gap_modules: Vec::new(),
            threshold,
            total_gap_points: 0.0,
        }
    }

    /// Add module coverage
    pub fn add_module(&mut self, metrics: CoverageMetrics) {
        if metrics.percentage < self.threshold {
            let gap = self.threshold - metrics.percentage;
            self.gap_modules.push(GapModule {
                name: metrics.name.clone(),
                current_coverage: metrics.percentage,
                gap_to_threshold: gap,
                recommendation: Self::recommend_for_gap(&metrics.name, gap),
            });
            self.total_gap_points += gap;
        }
        self.module_coverage.push(metrics);
    }

    /// Calculate total coverage
    pub fn calculate_total_coverage(&mut self) {
        if self.module_coverage.is_empty() {
            return;
        }

        let total: f64 = self.module_coverage.iter().map(|m| m.percentage).sum();
        self.total_coverage = total / self.module_coverage.len() as f64;
    }

    /// Recommend actions for gap
    fn recommend_for_gap(module: &str, gap: f64) -> String {
        if gap > 50.0 {
            format!(
                "Add comprehensive test suite for {}. Current coverage is critically low.",
                module
            )
        } else if gap > 20.0 {
            format!(
                "Increase coverage for {} with integration tests and error path testing.",
                module
            )
        } else {
            format!(
                "Add unit tests for edge cases and error scenarios in {}.",
                module
            )
        }
    }

    /// Get gap summary
    pub fn summary(&self) -> String {
        format!(
            "Coverage: {:.1}% (target: {:.1}%). {} modules below threshold with {:.1} total gap points.",
            self.total_coverage, self.threshold, self.gap_modules.len(), self.total_gap_points
        )
    }

    /// Get high-priority gaps (>50% gap)
    pub fn high_priority_gaps(&self) -> Vec<&GapModule> {
        self.gap_modules
            .iter()
            .filter(|g| g.gap_to_threshold > 50.0)
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_coverage_metrics() {
        let mut metrics = CoverageMetrics::new("ggen-core".to_string());
        metrics.lines_covered = 800;
        metrics.total_lines = 1000;
        metrics.calculate_percentage();
        assert_eq!(metrics.percentage, 80.0);
    }

    #[test]
    fn test_meets_threshold() {
        let mut metrics = CoverageMetrics::new("ggen-core".to_string());
        metrics.lines_covered = 800;
        metrics.total_lines = 1000;
        metrics.calculate_percentage();
        assert!(metrics.meets_threshold(80.0));
        assert!(!metrics.meets_threshold(85.0));
    }

    #[test]
    fn test_gap_analysis() {
        let mut analysis = CoverageGapAnalysis::new(80.0);

        let mut metrics1 = CoverageMetrics::new("module1".to_string());
        metrics1.percentage = 85.0;
        analysis.add_module(metrics1);

        let mut metrics2 = CoverageMetrics::new("module2".to_string());
        metrics2.percentage = 60.0;
        analysis.add_module(metrics2);

        analysis.calculate_total_coverage();
        assert_eq!(analysis.gap_modules.len(), 1);
        assert!(analysis.total_gap_points > 0.0);
    }

    #[test]
    fn test_gap_recommendation() {
        let recommendation = CoverageGapAnalysis::recommend_for_gap("module", 60.0);
        assert!(!recommendation.is_empty());
    }

    #[test]
    fn test_high_priority_gaps() {
        let mut analysis = CoverageGapAnalysis::new(80.0);

        let mut metrics = CoverageMetrics::new("module".to_string());
        metrics.percentage = 20.0; // 60 point gap
        analysis.add_module(metrics);

        let high_priority = analysis.high_priority_gaps();
        assert_eq!(high_priority.len(), 1);
    }
}
