//! Fixed test reporting for cleanroom testing framework
//!
//! This module provides comprehensive test reporting following core team best practices:
//! - Test execution tracking
//! - Performance metrics collection
//! - Coverage data integration
//! - Snapshot testing results
//! - Comprehensive reporting and analysis

use crate::error::{Result, CleanroomError};
use crate::serializable_instant::SerializableInstant;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::Instant;
use uuid::Uuid;

/// Test report for comprehensive test results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestReport {
    /// Report ID
    pub report_id: Uuid,
    /// Session ID
    pub session_id: Uuid,
    /// Report data
    pub data: ReportData,
    /// Generated timestamp
    pub generated_at: SerializableInstant,
}

/// Report data structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReportData {
    /// Test summary
    pub test_summary: TestSummary,
    /// Performance metrics
    pub performance_metrics: Vec<PerformanceMetric>,
    /// Coverage data
    pub coverage_data: Option<CoverageData>,
    /// Snapshot data
    pub snapshot_data: Option<SnapshotData>,
    /// Tracing data
    pub tracing_data: Option<TracingData>,
    /// Redaction data
    pub redaction_data: Option<RedactionData>,
    /// Recommendations
    pub recommendations: Vec<String>,
    /// Metadata
    pub metadata: HashMap<String, String>,
}

/// Test summary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestSummary {
    /// Total tests
    pub total_tests: usize,
    /// Passed tests
    pub passed_tests: usize,
    /// Failed tests
    pub failed_tests: usize,
    /// Skipped tests
    pub skipped_tests: usize,
    /// Test duration
    pub test_duration: std::time::Duration,
    /// Start time
    pub start_time: SerializableInstant,
    /// End time
    pub end_time: SerializableInstant,
    /// Success rate
    pub success_rate: f64,
    /// Average test duration
    pub average_test_duration: f64,
}

/// Performance metric
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceMetric {
    /// Metric name
    pub name: String,
    /// Metric value
    pub value: f64,
    /// Metric unit
    pub unit: String,
    /// Metric timestamp
    pub timestamp: SerializableInstant,
    /// Metric category
    pub category: String,
    /// Metric description
    pub description: Option<String>,
}

/// Coverage data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoverageData {
    /// Overall coverage percentage
    pub overall_coverage: f64,
    /// Line coverage percentage
    pub line_coverage: f64,
    /// Branch coverage percentage
    pub branch_coverage: f64,
    /// Function coverage percentage
    pub function_coverage: f64,
    /// Total lines
    pub total_lines: usize,
    /// Covered lines
    pub covered_lines: usize,
    /// Total branches
    pub total_branches: usize,
    /// Covered branches
    pub covered_branches: usize,
    /// Total functions
    pub total_functions: usize,
    /// Covered functions
    pub covered_functions: usize,
}

/// Snapshot data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SnapshotData {
    /// Total snapshots
    pub total_snapshots: usize,
    /// Validated snapshots
    pub validated_snapshots: usize,
    /// Failed validations
    pub failed_validations: usize,
    /// Total data size
    pub total_data_size: usize,
    /// Average snapshot size
    pub average_snapshot_size: f64,
}

/// Tracing data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TracingData {
    /// Total traces
    pub total_traces: usize,
    /// Active traces
    pub active_traces: usize,
    /// Completed traces
    pub completed_traces: usize,
    /// Failed traces
    pub failed_traces: usize,
    /// Total events
    pub total_events: usize,
    /// Average trace duration
    pub average_trace_duration: f64,
}

/// Redaction data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RedactionData {
    /// Total redactions
    pub total_redactions: usize,
    /// Redaction patterns used
    pub redaction_patterns: Vec<String>,
    /// Redacted data size
    pub redacted_data_size: usize,
    /// Redaction success rate
    pub redaction_success_rate: f64,
}

/// Comprehensive report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComprehensiveReport {
    /// Report ID
    pub report_id: Uuid,
    /// Session ID
    pub session_id: Uuid,
    /// Test reports
    pub test_reports: Vec<TestReport>,
    /// Overall summary
    pub overall_summary: OverallSummary,
    /// Generated timestamp
    pub generated_at: SerializableInstant,
}

/// Overall summary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OverallSummary {
    /// Total duration
    pub duration: std::time::Duration,
    /// Overall success rate
    pub overall_success_rate: f64,
    /// Coverage percentage
    pub coverage_percentage: f64,
    /// Performance score
    pub performance_score: f64,
    /// Summary text
    pub summary: String,
}

impl TestReport {
    /// Create a new test report
    pub fn new(session_id: Uuid) -> Self {
        Self {
            report_id: Uuid::new_v4(),
            session_id,
            data: ReportData::new(),
            generated_at: SerializableInstant::from(Instant::now()),
        }
    }

    /// Update test summary
    pub fn update_test_summary(&mut self, summary: TestSummary) {
        self.data.test_summary = summary;
    }

    /// Add performance metric
    pub fn add_performance_metric(&mut self, metric: PerformanceMetric) {
        self.data.performance_metrics.push(metric);
    }

    /// Update coverage data
    pub fn update_coverage_data(&mut self, coverage_data: CoverageData) {
        self.data.coverage_data = Some(coverage_data);
    }

    /// Update snapshot data
    pub fn update_snapshot_data(&mut self, snapshot_data: SnapshotData) {
        self.data.snapshot_data = Some(snapshot_data);
    }

    /// Update tracing data
    pub fn update_tracing_data(&mut self, tracing_data: TracingData) {
        self.data.tracing_data = Some(tracing_data);
    }

    /// Update redaction data
    pub fn update_redaction_data(&mut self, redaction_data: RedactionData) {
        self.data.redaction_data = Some(redaction_data);
    }

    /// Add recommendation
    pub fn add_recommendation(&mut self, recommendation: String) {
        self.data.recommendations.push(recommendation);
    }

    /// Add metadata
    pub fn add_metadata(&mut self, key: String, value: String) {
        self.data.metadata.insert(key, value);
    }

    /// Finalize the report
    pub fn finalize(&mut self) -> Result<()> {
        self.generated_at = SerializableInstant::from(Instant::now());
        
        // Calculate success rate
        let total = self.data.test_summary.total_tests;
        if total > 0 {
            self.data.test_summary.success_rate = 
                (self.data.test_summary.passed_tests as f64 / total as f64) * 100.0;
        }

        // Calculate average test duration
        if total > 0 {
            self.data.test_summary.average_test_duration = 
                self.data.test_summary.test_duration.as_millis() as f64 / total as f64;
        }

        Ok(())
    }

    /// Generate report
    pub fn generate_report(&self) -> String {
        format!(
            "Test Report Summary:\n\
            Report ID: {}\n\
            Session ID: {}\n\
            Total Tests: {}\n\
            Passed: {}\n\
            Failed: {}\n\
            Skipped: {}\n\
            Success Rate: {:.2}%\n\
            Duration: {:?}\n\
            Average Test Duration: {:.2}ms\n\
            Performance Metrics: {}\n\
            Coverage Data: {}\n\
            Snapshot Data: {}\n\
            Tracing Data: {}\n\
            Redaction Data: {}\n\
            Recommendations: {}\n\
            Generated At: {}",
            self.report_id,
            self.session_id,
            self.data.test_summary.total_tests,
            self.data.test_summary.passed_tests,
            self.data.test_summary.failed_tests,
            self.data.test_summary.skipped_tests,
            self.data.test_summary.success_rate,
            self.data.test_summary.test_duration,
            self.data.test_summary.average_test_duration,
            if self.data.coverage_data.is_some() { "Available" } else { "Not Available" },
            if self.data.snapshot_data.is_some() { "Available" } else { "Not Available" },
            if self.data.tracing_data.is_some() { "Available" } else { "Not Available" },
            if self.data.redaction_data.is_some() { "Available" } else { "Not Available" },
            self.data.recommendations.len(),
            self.generated_at
        )
    }

    /// Generate recommendations
    pub fn generate_recommendations(&self) -> Vec<String> {
        let mut recommendations = Vec::new();

        // Test success rate recommendations
        if self.data.test_summary.success_rate < 80.0 {
            recommendations.push("Test success rate is below 80%. Consider improving test quality and fixing failing tests.".to_string());
        }

        // Performance recommendations
        if self.data.test_summary.average_test_duration > 1000.0 {
            recommendations.push("Average test duration is high. Consider optimizing test performance.".to_string());
        }

        // Coverage recommendations
        if let Some(coverage) = &self.data.coverage_data {
            if coverage.overall_coverage < 80.0 {
                recommendations.push("Overall coverage is below 80%. Consider adding more tests.".to_string());
            }
        }

        // Snapshot recommendations
        if let Some(snapshots) = &self.data.snapshot_data {
            if snapshots.failed_validations > 0 {
                recommendations.push("Some snapshots failed validation. Review and update snapshots as needed.".to_string());
            }
        }

        // Tracing recommendations
        if let Some(tracing) = &self.data.tracing_data {
            if tracing.failed_traces > 0 {
                recommendations.push("Some traces failed. Review trace execution and error handling.".to_string());
            }
        }

        recommendations
    }

    /// Generate cleanroom recommendations
    pub fn generate_cleanroom_recommendations(&self) -> Vec<String> {
        let mut recommendations = Vec::new();

        // Resource usage recommendations
        if self.data.test_summary.total_tests > 100 {
            recommendations.push("Large number of tests detected. Consider using parallel execution for better performance.".to_string());
        }

        // Security recommendations
        if let Some(redaction) = &self.data.redaction_data {
            if redaction.redaction_success_rate < 95.0 {
                recommendations.push("Redaction success rate is low. Review redaction patterns and sensitive data handling.".to_string());
            }
        }

        // Performance recommendations
        let total_duration_ms = self.data.test_summary.test_duration.as_millis() as f64;
        if total_duration_ms > 300000.0 { // 5 minutes
            recommendations.push("Total test duration is long. Consider optimizing test execution and resource usage.".to_string());
        }

        recommendations
    }

    /// Record test execution
    pub fn record_test_execution(
        &mut self,
        test_name: String,
        status: TestStatus,
        duration: std::time::Duration,
    ) -> Result<()> {
        match status {
            TestStatus::Passed => self.data.test_summary.passed_tests += 1,
            TestStatus::Failed => self.data.test_summary.failed_tests += 1,
            TestStatus::Skipped => self.data.test_summary.skipped_tests += 1,
        }
        
        self.data.test_summary.total_tests += 1;
        self.data.test_summary.test_duration += duration;
        
        Ok(())
    }

    /// Convert to JSON
    pub fn to_json(&self) -> Result<String> {
        serde_json::to_string_pretty(self)
            .map_err(|e| CleanroomError::serialization_error(e.to_string()))
    }

    /// Convert to TOML
    pub fn to_toml(&self) -> Result<String> {
        toml::to_string_pretty(self)
            .map_err(|e| CleanroomError::serialization_error(e.to_string()))
    }
}

impl ReportData {
    /// Create new report data
    pub fn new() -> Self {
        Self {
            test_summary: TestSummary::new(),
            performance_metrics: Vec::new(),
            coverage_data: None,
            snapshot_data: None,
            tracing_data: None,
            redaction_data: None,
            recommendations: Vec::new(),
            metadata: HashMap::new(),
        }
    }
}

impl TestSummary {
    /// Create new test summary
    pub fn new() -> Self {
        Self {
            total_tests: 0,
            passed_tests: 0,
            failed_tests: 0,
            skipped_tests: 0,
            test_duration: std::time::Duration::from_secs(0),
            start_time: SerializableInstant::from(Instant::now()),
            end_time: SerializableInstant::from(Instant::now()),
            success_rate: 0.0,
            average_test_duration: 0.0,
        }
    }
}

impl ComprehensiveReport {
    /// Create new comprehensive report
    pub fn new(session_id: Uuid) -> Self {
        Self {
            report_id: Uuid::new_v4(),
            session_id,
            test_reports: Vec::new(),
            overall_summary: OverallSummary::new(),
            generated_at: SerializableInstant::from(Instant::now()),
        }
    }

    /// Add test report
    pub fn add_test_report(&mut self, report: TestReport) {
        self.test_reports.push(report);
    }

    /// Calculate overall summary
    pub fn calculate_overall_summary(&mut self) {
        if self.test_reports.is_empty() {
            return;
        }

        let total_tests: usize = self.test_reports.iter().map(|r| r.data.test_summary.total_tests).sum();
        let passed_tests: usize = self.test_reports.iter().map(|r| r.data.test_summary.passed_tests).sum();
        let total_duration: std::time::Duration = self.test_reports.iter().map(|r| r.data.test_summary.test_duration).sum();

        let overall_success_rate = if total_tests > 0 {
            (passed_tests as f64 / total_tests as f64) * 100.0
        } else {
            0.0
        };

        let coverage_percentage = if let Some(report) = self.test_reports.first() {
            report.data.coverage_data.as_ref().map(|c| c.overall_coverage).unwrap_or(0.0)
        } else {
            0.0
        };

        let performance_score = if let Some(report) = self.test_reports.first() {
            let avg_duration = report.data.test_summary.average_test_duration;
            if avg_duration > 0.0 {
                100.0 - (avg_duration / 1000.0).min(100.0)
            } else {
                100.0
            }
        } else {
            100.0
        };

        self.overall_summary = OverallSummary {
            duration: total_duration,
            overall_success_rate,
            coverage_percentage,
            performance_score,
            summary: format!(
                "Overall Test Summary: {} tests executed, {:.1}% success rate, {:.1}% coverage, {:.1}% performance score",
                total_tests, overall_success_rate, coverage_percentage, performance_score
            ),
        };
    }

    /// Get duration
    pub fn duration(&self) -> std::time::Duration {
        self.overall_summary.duration
    }

    /// Get overall success rate
    pub fn overall_success_rate(&self) -> f64 {
        self.overall_summary.overall_success_rate
    }

    /// Get coverage percentage
    pub fn coverage_percentage(&self) -> f64 {
        self.overall_summary.coverage_percentage
    }

    /// Get performance score
    pub fn performance_score(&self) -> f64 {
        self.overall_summary.performance_score
    }

    /// Get summary
    pub fn summary(&self) -> &str {
        &self.overall_summary.summary
    }

    /// Convert to JSON
    pub fn to_json(&self) -> Result<String> {
        serde_json::to_string_pretty(self)
            .map_err(|e| CleanroomError::serialization_error(e.to_string()))
    }

    /// Convert to TOML
    pub fn to_toml(&self) -> Result<String> {
        toml::to_string_pretty(self)
            .map_err(|e| CleanroomError::serialization_error(e.to_string()))
    }
}

impl OverallSummary {
    /// Create new overall summary
    pub fn new() -> Self {
        Self {
            duration: std::time::Duration::from_secs(0),
            overall_success_rate: 0.0,
            coverage_percentage: 0.0,
            performance_score: 0.0,
            summary: String::new(),
        }
    }
}

/// Test status enumeration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TestStatus {
    /// Test passed
    Passed,
    /// Test failed
    Failed,
    /// Test skipped
    Skipped,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_test_report_creation() {
        let session_id = Uuid::new_v4();
        let report = TestReport::new(session_id);
        
        assert_eq!(report.session_id, session_id);
        assert!(!report.report_id.is_nil());
        assert_eq!(report.data.test_summary.total_tests, 0);
    }

    #[test]
    fn test_test_report_update_test_summary() {
        let session_id = Uuid::new_v4();
        let mut report = TestReport::new(session_id);
        
        let summary = TestSummary {
            total_tests: 10,
            passed_tests: 8,
            failed_tests: 1,
            skipped_tests: 1,
            test_duration: std::time::Duration::from_secs(30),
            start_time: SerializableInstant::from(Instant::now()),
            end_time: SerializableInstant::from(Instant::now()),
            success_rate: 80.0,
            average_test_duration: 3000.0,
        };
        
        report.update_test_summary(summary);
        assert_eq!(report.data.test_summary.total_tests, 10);
        assert_eq!(report.data.test_summary.passed_tests, 8);
    }

    #[test]
    fn test_test_report_add_performance_metric() {
        let session_id = Uuid::new_v4();
        let mut report = TestReport::new(session_id);
        
        let metric = PerformanceMetric {
            name: "cpu_usage".to_string(),
            value: 75.5,
            unit: "percent".to_string(),
            timestamp: SerializableInstant::from(Instant::now()),
            category: "system".to_string(),
            description: Some("CPU usage during tests".to_string()),
        };
        
        report.add_performance_metric(metric);
        assert_eq!(report.data.performance_metrics.len(), 1);
        assert_eq!(report.data.performance_metrics[0].name, "cpu_usage");
    }

    #[test]
    fn test_test_report_update_coverage_data() {
        let session_id = Uuid::new_v4();
        let mut report = TestReport::new(session_id);
        
        let coverage_data = CoverageData {
            overall_coverage: 85.5,
            line_coverage: 90.0,
            branch_coverage: 80.0,
            function_coverage: 86.5,
            total_lines: 1000,
            covered_lines: 900,
            total_branches: 500,
            covered_branches: 400,
            total_functions: 100,
            covered_functions: 86,
        };
        
        report.update_coverage_data(coverage_data);
        assert!(report.data.coverage_data.is_some());
        assert_eq!(report.data.coverage_data.unwrap().overall_coverage, 85.5);
    }

    #[test]
    fn test_test_report_finalize() {
        let session_id = Uuid::new_v4();
        let mut report = TestReport::new(session_id);
        
        // Set up test summary
        report.data.test_summary.total_tests = 10;
        report.data.test_summary.passed_tests = 8;
        report.data.test_summary.test_duration = std::time::Duration::from_secs(30);
        
        assert!(report.finalize().is_ok());
        assert_eq!(report.data.test_summary.success_rate, 80.0);
        assert_eq!(report.data.test_summary.average_test_duration, 3000.0);
    }

    #[test]
    fn test_test_report_generate_report() {
        let session_id = Uuid::new_v4();
        let mut report = TestReport::new(session_id);
        
        // Set up test summary
        report.data.test_summary.total_tests = 10;
        report.data.test_summary.passed_tests = 8;
        report.data.test_summary.failed_tests = 1;
        report.data.test_summary.skipped_tests = 1;
        report.data.test_summary.test_duration = std::time::Duration::from_secs(30);
        
        let report_text = report.generate_report();
        assert!(report_text.contains("Test Report Summary"));
        assert!(report_text.contains("Total Tests: 10"));
        assert!(report_text.contains("Passed: 8"));
        assert!(report_text.contains("Failed: 1"));
    }

    #[test]
    fn test_test_report_generate_recommendations() {
        let session_id = Uuid::new_v4();
        let mut report = TestReport::new(session_id);
        
        // Set up low success rate
        report.data.test_summary.total_tests = 10;
        report.data.test_summary.passed_tests = 6; // 60% success rate
        report.data.test_summary.average_test_duration = 1500.0; // High duration
        
        // Set up low coverage
        report.data.coverage_data = Some(CoverageData {
            overall_coverage: 70.0,
            line_coverage: 75.0,
            branch_coverage: 65.0,
            function_coverage: 70.0,
            total_lines: 1000,
            covered_lines: 700,
            total_branches: 500,
            covered_branches: 325,
            total_functions: 100,
            covered_functions: 70,
        });
        
        let recommendations = report.generate_recommendations();
        assert!(!recommendations.is_empty());
        assert!(recommendations.iter().any(|r| r.contains("success rate")));
        assert!(recommendations.iter().any(|r| r.contains("coverage")));
    }

    #[test]
    fn test_test_report_record_test_execution() {
        let session_id = Uuid::new_v4();
        let mut report = TestReport::new(session_id);
        
        // Record passed test
        assert!(report.record_test_execution("test1".to_string(), TestStatus::Passed, std::time::Duration::from_secs(1)).is_ok());
        assert_eq!(report.data.test_summary.total_tests, 1);
        assert_eq!(report.data.test_summary.passed_tests, 1);
        
        // Record failed test
        assert!(report.record_test_execution("test2".to_string(), TestStatus::Failed, std::time::Duration::from_secs(2)).is_ok());
        assert_eq!(report.data.test_summary.total_tests, 2);
        assert_eq!(report.data.test_summary.failed_tests, 1);
        
        // Record skipped test
        assert!(report.record_test_execution("test3".to_string(), TestStatus::Skipped, std::time::Duration::from_secs(0)).is_ok());
        assert_eq!(report.data.test_summary.total_tests, 3);
        assert_eq!(report.data.test_summary.skipped_tests, 1);
    }

    #[test]
    fn test_test_report_serialization() {
        let session_id = Uuid::new_v4();
        let mut report = TestReport::new(session_id);
        
        // Set up some data
        report.data.test_summary.total_tests = 5;
        report.data.test_summary.passed_tests = 4;
        report.add_recommendation("Test recommendation".to_string());
        report.add_metadata("key".to_string(), "value".to_string());
        
        // Test JSON serialization
        let json = report.to_json().unwrap();
        assert!(json.contains("report_id"));
        assert!(json.contains("session_id"));
        assert!(json.contains("total_tests"));
        
        // Test TOML serialization
        let toml = report.to_toml().unwrap();
        assert!(toml.contains("report_id"));
        assert!(toml.contains("session_id"));
        assert!(toml.contains("total_tests"));
    }

    #[test]
    fn test_comprehensive_report_creation() {
        let session_id = Uuid::new_v4();
        let report = ComprehensiveReport::new(session_id);
        
        assert_eq!(report.session_id, session_id);
        assert!(!report.report_id.is_nil());
        assert!(report.test_reports.is_empty());
    }

    #[test]
    fn test_comprehensive_report_add_test_report() {
        let session_id = Uuid::new_v4();
        let mut comprehensive_report = ComprehensiveReport::new(session_id);
        
        let test_report = TestReport::new(session_id);
        comprehensive_report.add_test_report(test_report);
        
        assert_eq!(comprehensive_report.test_reports.len(), 1);
    }

    #[test]
    fn test_comprehensive_report_calculate_overall_summary() {
        let session_id = Uuid::new_v4();
        let mut comprehensive_report = ComprehensiveReport::new(session_id);
        
        // Add test report
        let mut test_report = TestReport::new(session_id);
        test_report.data.test_summary.total_tests = 10;
        test_report.data.test_summary.passed_tests = 8;
        test_report.data.test_summary.test_duration = std::time::Duration::from_secs(30);
        test_report.data.coverage_data = Some(CoverageData {
            overall_coverage: 85.0,
            line_coverage: 90.0,
            branch_coverage: 80.0,
            function_coverage: 85.0,
            total_lines: 1000,
            covered_lines: 900,
            total_branches: 500,
            covered_branches: 400,
            total_functions: 100,
            covered_functions: 85,
        });
        
        comprehensive_report.add_test_report(test_report);
        comprehensive_report.calculate_overall_summary();
        
        assert_eq!(comprehensive_report.overall_summary.overall_success_rate, 80.0);
        assert_eq!(comprehensive_report.overall_summary.coverage_percentage, 85.0);
        assert!(comprehensive_report.overall_summary.summary.contains("80.0% success rate"));
    }

    #[test]
    fn test_comprehensive_report_serialization() {
        let session_id = Uuid::new_v4();
        let mut comprehensive_report = ComprehensiveReport::new(session_id);
        
        // Add test report
        let test_report = TestReport::new(session_id);
        comprehensive_report.add_test_report(test_report);
        comprehensive_report.calculate_overall_summary();
        
        // Test JSON serialization
        let json = comprehensive_report.to_json().unwrap();
        assert!(json.contains("report_id"));
        assert!(json.contains("session_id"));
        assert!(json.contains("test_reports"));
        
        // Test TOML serialization
        let toml = comprehensive_report.to_toml().unwrap();
        assert!(toml.contains("report_id"));
        assert!(toml.contains("session_id"));
        assert!(toml.contains("test_reports"));
    }

    #[test]
    fn test_performance_metric_serialization() {
        let metric = PerformanceMetric {
            name: "cpu_usage".to_string(),
            value: 75.5,
            unit: "percent".to_string(),
            timestamp: SerializableInstant::from(Instant::now()),
            category: "system".to_string(),
            description: Some("CPU usage during tests".to_string()),
        };

        let json = serde_json::to_string(&metric).unwrap();
        let deserialized: PerformanceMetric = serde_json::from_str(&json).unwrap();
        
        assert_eq!(metric.name, deserialized.name);
        assert_eq!(metric.value, deserialized.value);
        assert_eq!(metric.unit, deserialized.unit);
        assert_eq!(metric.category, deserialized.category);
        assert_eq!(metric.description, deserialized.description);
    }

    #[test]
    fn test_test_status_serialization() {
        let statuses = vec![
            TestStatus::Passed,
            TestStatus::Failed,
            TestStatus::Skipped,
        ];

        for status in statuses {
            let json = serde_json::to_string(&status).unwrap();
            let deserialized: TestStatus = serde_json::from_str(&json).unwrap();
            assert_eq!(status, deserialized);
        }
    }

    #[test]
    fn test_coverage_data_serialization() {
        let coverage_data = CoverageData {
            overall_coverage: 85.5,
            line_coverage: 90.0,
            branch_coverage: 80.0,
            function_coverage: 86.5,
            total_lines: 1000,
            covered_lines: 900,
            total_branches: 500,
            covered_branches: 400,
            total_functions: 100,
            covered_functions: 86,
        };

        let json = serde_json::to_string(&coverage_data).unwrap();
        let deserialized: CoverageData = serde_json::from_str(&json).unwrap();
        
        assert_eq!(coverage_data.overall_coverage, deserialized.overall_coverage);
        assert_eq!(coverage_data.line_coverage, deserialized.line_coverage);
        assert_eq!(coverage_data.branch_coverage, deserialized.branch_coverage);
        assert_eq!(coverage_data.function_coverage, deserialized.function_coverage);
    }

    #[test]
    fn test_snapshot_data_serialization() {
        let snapshot_data = SnapshotData {
            total_snapshots: 10,
            validated_snapshots: 8,
            failed_validations: 2,
            total_data_size: 1024000,
            average_snapshot_size: 102400.0,
        };

        let json = serde_json::to_string(&snapshot_data).unwrap();
        let deserialized: SnapshotData = serde_json::from_str(&json).unwrap();
        
        assert_eq!(snapshot_data.total_snapshots, deserialized.total_snapshots);
        assert_eq!(snapshot_data.validated_snapshots, deserialized.validated_snapshots);
        assert_eq!(snapshot_data.failed_validations, deserialized.failed_validations);
        assert_eq!(snapshot_data.total_data_size, deserialized.total_data_size);
        assert_eq!(snapshot_data.average_snapshot_size, deserialized.average_snapshot_size);
    }

    #[test]
    fn test_tracing_data_serialization() {
        let tracing_data = TracingData {
            total_traces: 25,
            active_traces: 5,
            completed_traces: 20,
            failed_traces: 2,
            total_events: 150,
            average_trace_duration: 250.5,
        };

        let json = serde_json::to_string(&tracing_data).unwrap();
        let deserialized: TracingData = serde_json::from_str(&json).unwrap();
        
        assert_eq!(tracing_data.total_traces, deserialized.total_traces);
        assert_eq!(tracing_data.active_traces, deserialized.active_traces);
        assert_eq!(tracing_data.completed_traces, deserialized.completed_traces);
        assert_eq!(tracing_data.failed_traces, deserialized.failed_traces);
        assert_eq!(tracing_data.total_events, deserialized.total_events);
        assert_eq!(tracing_data.average_trace_duration, deserialized.average_trace_duration);
    }

    #[test]
    fn test_redaction_data_serialization() {
        let redaction_data = RedactionData {
            total_redactions: 50,
            redaction_patterns: vec!["password".to_string(), "token".to_string()],
            redacted_data_size: 5120,
            redaction_success_rate: 95.5,
        };

        let json = serde_json::to_string(&redaction_data).unwrap();
        let deserialized: RedactionData = serde_json::from_str(&json).unwrap();
        
        assert_eq!(redaction_data.total_redactions, deserialized.total_redactions);
        assert_eq!(redaction_data.redaction_patterns, deserialized.redaction_patterns);
        assert_eq!(redaction_data.redacted_data_size, deserialized.redacted_data_size);
        assert_eq!(redaction_data.redaction_success_rate, deserialized.redaction_success_rate);
    }
}