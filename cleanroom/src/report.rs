//! Test report generation for cleanroom testing
//!
//! This module provides test report generation following core team best practices:
//! - Comprehensive test reporting
//! - Performance metrics
//! - Coverage analysis
//! - Recommendations

use crate::error::{Result, CleanroomError};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::Mutex;
use uuid::Uuid;

/// Test report generator for cleanroom testing
#[derive(Debug)]
pub struct TestReport {
    /// Session ID
    session_id: Uuid,
    /// Report data
    report_data: Arc<Mutex<ReportData>>,
}

/// Report data structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReportData {
    /// Session ID
    pub session_id: Uuid,
    /// Start time
    pub start_time: Instant,
    /// End time
    pub end_time: Option<Instant>,
    /// Test summary
    pub test_summary: TestSummary,
    /// Performance metrics
    pub performance_metrics: HashMap<String, f64>,
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
    /// Report metadata
    pub metadata: HashMap<String, String>,
}

/// Test summary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestSummary {
    /// Total tests
    pub total_tests: u32,
    /// Passed tests
    pub passed_tests: u32,
    /// Failed tests
    pub failed_tests: u32,
    /// Skipped tests
    pub skipped_tests: u32,
    /// Test duration
    pub test_duration: Duration,
    /// Success rate
    pub success_rate: f64,
    /// Average test duration
    pub average_test_duration: Duration,
}

/// Coverage data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoverageData {
    /// Overall coverage percentage
    pub overall_coverage_percentage: f64,
    /// Line coverage percentage
    pub line_coverage_percentage: f64,
    /// Function coverage percentage
    pub function_coverage_percentage: f64,
    /// Branch coverage percentage
    pub branch_coverage_percentage: f64,
    /// Total lines
    pub total_lines: u32,
    /// Covered lines
    pub covered_lines: u32,
    /// Total functions
    pub total_functions: u32,
    /// Covered functions
    pub covered_functions: u32,
    /// Total branches
    pub total_branches: u32,
    /// Covered branches
    pub covered_branches: u32,
}

/// Snapshot data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SnapshotData {
    /// Total snapshots
    pub total_snapshots: u32,
    /// Valid snapshots
    pub valid_snapshots: u32,
    /// Invalid snapshots
    pub invalid_snapshots: u32,
    /// New snapshots
    pub new_snapshots: u32,
    /// Pending snapshots
    pub pending_snapshots: u32,
    /// Total size in bytes
    pub total_size_bytes: u64,
    /// Average size in bytes
    pub average_size_bytes: u64,
}

/// Tracing data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TracingData {
    /// Total spans
    pub total_spans: u32,
    /// Completed spans
    pub completed_spans: u32,
    /// Failed spans
    pub failed_spans: u32,
    /// Running spans
    pub running_spans: u32,
    /// Total metrics
    pub total_metrics: u32,
    /// Total logs
    pub total_logs: u32,
    /// Average span duration
    pub average_span_duration_ms: f64,
    /// Total trace duration
    pub total_trace_duration_ms: f64,
}

/// Redaction data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RedactionData {
    /// Total operations
    pub total_operations: u32,
    /// Total matches
    pub total_matches: u32,
    /// Total bytes processed
    pub total_bytes_processed: u64,
    /// Average operation duration
    pub average_operation_duration_ms: f64,
    /// Rules applied
    pub rules_applied: HashMap<String, u32>,
}

impl TestReport {
    /// Create a new test report generator
    pub fn new(session_id: Uuid) -> Self {
        Self {
            session_id,
            report_data: Arc::new(Mutex::new(ReportData::new(session_id))),
        }
    }
    
    /// Update test summary
    pub async fn update_test_summary(&self, summary: TestSummary) -> Result<()> {
        let mut data = self.report_data.lock().await;
        data.test_summary = summary;
        Ok(())
    }
    
    /// Add performance metric
    pub async fn add_performance_metric(&self, name: String, value: f64) -> Result<()> {
        let mut data = self.report_data.lock().await;
        data.performance_metrics.insert(name, value);
        Ok(())
    }
    
    /// Update coverage data
    pub async fn update_coverage_data(&self, coverage_data: CoverageData) -> Result<()> {
        let mut data = self.report_data.lock().await;
        data.coverage_data = Some(coverage_data);
        Ok(())
    }
    
    /// Update snapshot data
    pub async fn update_snapshot_data(&self, snapshot_data: SnapshotData) -> Result<()> {
        let mut data = self.report_data.lock().await;
        data.snapshot_data = Some(snapshot_data);
        Ok(())
    }
    
    /// Update tracing data
    pub async fn update_tracing_data(&self, tracing_data: TracingData) -> Result<()> {
        let mut data = self.report_data.lock().await;
        data.tracing_data = Some(tracing_data);
        Ok(())
    }
    
    /// Update redaction data
    pub async fn update_redaction_data(&self, redaction_data: RedactionData) -> Result<()> {
        let mut data = self.report_data.lock().await;
        data.redaction_data = Some(redaction_data);
        Ok(())
    }
    
    /// Add recommendation
    pub async fn add_recommendation(&self, recommendation: String) -> Result<()> {
        let mut data = self.report_data.lock().await;
        data.recommendations.push(recommendation);
        Ok(())
    }
    
    /// Add metadata
    pub async fn add_metadata(&self, key: String, value: String) -> Result<()> {
        let mut data = self.report_data.lock().await;
        data.metadata.insert(key, value);
        Ok(())
    }
    
    /// Finalize report
    pub async fn finalize(&self) -> Result<()> {
        let mut data = self.report_data.lock().await;
        data.end_time = Some(Instant::now());
        
        // Generate recommendations based on data
        self.generate_recommendations(&mut data).await;
        
        Ok(())
    }
    
    /// Generate comprehensive report
    pub async fn generate_report(&self, metrics: &crate::cleanroom::CleanroomMetrics) -> Result<ComprehensiveReport> {
        let data = self.report_data.lock().await;
        
        let mut report = ComprehensiveReport {
            session_id: data.session_id,
            start_time: data.start_time,
            end_time: data.end_time,
            test_summary: data.test_summary.clone(),
            performance_metrics: data.performance_metrics.clone(),
            coverage_data: data.coverage_data.clone(),
            snapshot_data: data.snapshot_data.clone(),
            tracing_data: data.tracing_data.clone(),
            redaction_data: data.redaction_data.clone(),
            recommendations: data.recommendations.clone(),
            metadata: data.metadata.clone(),
            cleanroom_metrics: metrics.clone(),
            report_generated_at: Instant::now(),
        };
        
        // Generate additional recommendations based on cleanroom metrics
        self.generate_cleanroom_recommendations(&mut report).await;
        
        Ok(report)
    }
    
    /// Generate recommendations
    async fn generate_recommendations(&self, data: &mut ReportData) {
        // Test success rate recommendation
        if data.test_summary.success_rate < 90.0 {
            data.recommendations.push(format!(
                "Test success rate is {:.1}%, consider improving test reliability",
                data.test_summary.success_rate
            ));
        }
        
        // Performance recommendation
        if let Some(avg_duration) = data.performance_metrics.get("average_test_duration_ms") {
            if *avg_duration > 1000.0 {
                data.recommendations.push(format!(
                    "Average test duration is {:.1}ms, consider optimizing slow tests",
                    avg_duration
                ));
            }
        }
        
        // Coverage recommendation
        if let Some(ref coverage) = data.coverage_data {
            if coverage.overall_coverage_percentage < 80.0 {
                data.recommendations.push(format!(
                    "Overall coverage is {:.1}%, consider adding more tests",
                    coverage.overall_coverage_percentage
                ));
            }
        }
        
        // Snapshot recommendation
        if let Some(ref snapshots) = data.snapshot_data {
            if snapshots.invalid_snapshots > 0 {
                data.recommendations.push(format!(
                    "{} invalid snapshots detected, review and update snapshots",
                    snapshots.invalid_snapshots
                ));
            }
        }
        
        // Tracing recommendation
        if let Some(ref tracing) = data.tracing_data {
            if tracing.failed_spans > 0 {
                data.recommendations.push(format!(
                    "{} spans failed, review and fix failed operations",
                    tracing.failed_spans
                ));
            }
        }
        
        // Redaction recommendation
        if let Some(ref redaction) = data.redaction_data {
            if redaction.total_matches > 100 {
                data.recommendations.push(format!(
                    "{} sensitive data matches found, consider improving data handling",
                    redaction.total_matches
                ));
            }
        }
    }
    
    /// Generate cleanroom-specific recommendations
    async fn generate_cleanroom_recommendations(&self, report: &mut ComprehensiveReport) {
        // Container performance recommendation
        if report.cleanroom_metrics.containers_started > 10 {
            report.recommendations.push(format!(
                "{} containers started, consider using singleton pattern for better performance",
                report.cleanroom_metrics.containers_started
            ));
        }
        
        // Resource usage recommendation
        if report.cleanroom_metrics.resource_usage.peak_cpu_usage_percent > 80.0 {
            report.recommendations.push(format!(
                "Peak CPU usage was {:.1}%, consider optimizing resource-intensive operations",
                report.cleanroom_metrics.resource_usage.peak_cpu_usage_percent
            ));
        }
        
        if report.cleanroom_metrics.resource_usage.peak_memory_usage_bytes > 1024 * 1024 * 1024 {
            report.recommendations.push(format!(
                "Peak memory usage was {} bytes, consider optimizing memory usage",
                report.cleanroom_metrics.resource_usage.peak_memory_usage_bytes
            ));
        }
        
        // Error count recommendation
        if report.cleanroom_metrics.error_count > 0 {
            report.recommendations.push(format!(
                "{} errors occurred during testing, review and fix error conditions",
                report.cleanroom_metrics.error_count
            ));
        }
        
        // Performance metrics recommendation
        if let Some(duration) = report.cleanroom_metrics.performance_metrics.get("total_duration_ms") {
            if *duration > 300000.0 { // 5 minutes
                report.recommendations.push(format!(
                    "Total test duration was {:.1}ms, consider optimizing test execution time",
                    duration
                ));
            }
        }
    }
}

impl ReportData {
    /// Create new report data
    pub fn new(session_id: Uuid) -> Self {
        Self {
            session_id,
            start_time: Instant::now(),
            end_time: None,
            test_summary: TestSummary {
                total_tests: 0,
                passed_tests: 0,
                failed_tests: 0,
                skipped_tests: 0,
                test_duration: Duration::from_secs(0),
                success_rate: 0.0,
                average_test_duration: Duration::from_secs(0),
            },
            performance_metrics: HashMap::new(),
            coverage_data: None,
            snapshot_data: None,
            tracing_data: None,
            redaction_data: None,
            recommendations: Vec::new(),
            metadata: HashMap::new(),
        }
    }
}

/// Comprehensive test report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComprehensiveReport {
    /// Session ID
    pub session_id: Uuid,
    /// Start time
    pub start_time: Instant,
    /// End time
    pub end_time: Option<Instant>,
    /// Test summary
    pub test_summary: TestSummary,
    /// Performance metrics
    pub performance_metrics: HashMap<String, f64>,
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
    /// Cleanroom metrics
    pub cleanroom_metrics: crate::cleanroom::CleanroomMetrics,
    /// Report generated at
    pub report_generated_at: Instant,
}

impl ComprehensiveReport {
    /// Get report duration
    pub fn duration(&self) -> Duration {
        if let Some(end_time) = self.end_time {
            end_time.duration_since(self.start_time)
        } else {
            self.report_generated_at.duration_since(self.start_time)
        }
    }
    
    /// Get overall success rate
    pub fn overall_success_rate(&self) -> f64 {
        self.test_summary.success_rate
    }
    
    /// Get coverage percentage
    pub fn coverage_percentage(&self) -> Option<f64> {
        self.coverage_data.map(|c| c.overall_coverage_percentage)
    }
    
    /// Get performance score
    pub fn performance_score(&self) -> f64 {
        let mut score = 100.0;
        
        // Deduct points for slow tests
        if let Some(avg_duration) = self.performance_metrics.get("average_test_duration_ms") {
            if *avg_duration > 1000.0 {
                score -= (*avg_duration - 1000.0) / 100.0;
            }
        }
        
        // Deduct points for high resource usage
        if self.cleanroom_metrics.resource_usage.peak_cpu_usage_percent > 80.0 {
            score -= (self.cleanroom_metrics.resource_usage.peak_cpu_usage_percent - 80.0) / 2.0;
        }
        
        // Deduct points for errors
        if self.cleanroom_metrics.error_count > 0 {
            score -= self.cleanroom_metrics.error_count as f64 * 5.0;
        }
        
        score.max(0.0)
    }
    
    /// Get report summary
    pub fn summary(&self) -> String {
        format!(
            "Test Report Summary:\n\
            Session ID: {}\n\
            Duration: {:?}\n\
            Tests: {} total, {} passed, {} failed, {} skipped\n\
            Success Rate: {:.1}%\n\
            Coverage: {:.1}%\n\
            Performance Score: {:.1}/100\n\
            Containers: {} started, {} stopped\n\
            Errors: {}\n\
            Recommendations: {}",
            self.session_id,
            self.duration(),
            self.test_summary.total_tests,
            self.test_summary.passed_tests,
            self.test_summary.failed_tests,
            self.test_summary.skipped_tests,
            self.overall_success_rate(),
            self.coverage_percentage().unwrap_or(0.0),
            self.performance_score(),
            self.cleanroom_metrics.containers_started,
            self.cleanroom_metrics.containers_stopped,
            self.cleanroom_metrics.error_count,
            self.recommendations.len()
        )
    }
    
    /// Export report to JSON
    pub fn to_json(&self) -> Result<String> {
        serde_json::to_string_pretty(self)
            .map_err(|e| CleanroomError::serialization_error(format!("Failed to serialize report: {}", e)))
    }
    
    /// Export report to TOML
    pub fn to_toml(&self) -> Result<String> {
        toml::to_string_pretty(self)
            .map_err(|e| CleanroomError::serialization_error(format!("Failed to serialize report: {}", e)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_test_report_creation() {
        let session_id = Uuid::new_v4();
        let report = TestReport::new(session_id);
        assert_eq!(report.session_id, session_id);
    }
    
    #[tokio::test]
    async fn test_update_test_summary() {
        let session_id = Uuid::new_v4();
        let report = TestReport::new(session_id);
        
        let summary = TestSummary {
            total_tests: 10,
            passed_tests: 8,
            failed_tests: 1,
            skipped_tests: 1,
            test_duration: Duration::from_secs(60),
            success_rate: 80.0,
            average_test_duration: Duration::from_secs(6),
        };
        
        report.update_test_summary(summary).await.unwrap();
        
        let data = report.report_data.lock().await;
        assert_eq!(data.test_summary.total_tests, 10);
        assert_eq!(data.test_summary.passed_tests, 8);
        assert_eq!(data.test_summary.success_rate, 80.0);
    }
    
    #[tokio::test]
    async fn test_add_performance_metric() {
        let session_id = Uuid::new_v4();
        let report = TestReport::new(session_id);
        
        report.add_performance_metric("test_duration_ms".to_string(), 1000.0).await.unwrap();
        
        let data = report.report_data.lock().await;
        assert_eq!(data.performance_metrics.get("test_duration_ms"), Some(&1000.0));
    }
    
    #[tokio::test]
    async fn test_add_recommendation() {
        let session_id = Uuid::new_v4();
        let report = TestReport::new(session_id);
        
        report.add_recommendation("Test recommendation".to_string()).await.unwrap();
        
        let data = report.report_data.lock().await;
        assert_eq!(data.recommendations.len(), 1);
        assert_eq!(data.recommendations[0], "Test recommendation");
    }
    
    #[tokio::test]
    async fn test_finalize_report() {
        let session_id = Uuid::new_v4();
        let report = TestReport::new(session_id);
        
        report.finalize().await.unwrap();
        
        let data = report.report_data.lock().await;
        assert!(data.end_time.is_some());
    }
    
    #[tokio::test]
    async fn test_generate_comprehensive_report() {
        let session_id = Uuid::new_v4();
        let report = TestReport::new(session_id);
        
        let summary = TestSummary {
            total_tests: 5,
            passed_tests: 4,
            failed_tests: 1,
            skipped_tests: 0,
            test_duration: Duration::from_secs(30),
            success_rate: 80.0,
            average_test_duration: Duration::from_secs(6),
        };
        
        report.update_test_summary(summary).await.unwrap();
        
        let metrics = crate::cleanroom::CleanroomMetrics {
            session_id,
            start_time: Instant::now(),
            end_time: Some(Instant::now()),
            total_duration_ms: 30000,
            containers_started: 2,
            containers_stopped: 2,
            tests_executed: 5,
            tests_passed: 4,
            tests_failed: 1,
            coverage_percentage: 85.0,
            resource_usage: crate::cleanroom::ResourceUsage {
                peak_cpu_usage_percent: 50.0,
                peak_memory_usage_bytes: 512 * 1024 * 1024,
                peak_disk_usage_bytes: 1024 * 1024 * 1024,
                network_bytes_transferred: 1024 * 1024,
                container_count: 2,
            },
            performance_metrics: HashMap::new(),
            error_count: 1,
            warning_count: 0,
        };
        
        let comprehensive_report = report.generate_report(&metrics).await.unwrap();
        assert_eq!(comprehensive_report.session_id, session_id);
        assert_eq!(comprehensive_report.test_summary.total_tests, 5);
        assert_eq!(comprehensive_report.overall_success_rate(), 80.0);
    }
    
    #[tokio::test]
    async fn test_comprehensive_report_methods() {
        let session_id = Uuid::new_v4();
        let report = TestReport::new(session_id);
        
        let summary = TestSummary {
            total_tests: 10,
            passed_tests: 9,
            failed_tests: 1,
            skipped_tests: 0,
            test_duration: Duration::from_secs(60),
            success_rate: 90.0,
            average_test_duration: Duration::from_secs(6),
        };
        
        report.update_test_summary(summary).await.unwrap();
        
        let coverage_data = CoverageData {
            overall_coverage_percentage: 85.0,
            line_coverage_percentage: 80.0,
            function_coverage_percentage: 90.0,
            branch_coverage_percentage: 75.0,
            total_lines: 1000,
            covered_lines: 850,
            total_functions: 100,
            covered_functions: 90,
            total_branches: 200,
            covered_branches: 150,
        };
        
        report.update_coverage_data(coverage_data).await.unwrap();
        
        let metrics = crate::cleanroom::CleanroomMetrics {
            session_id,
            start_time: Instant::now(),
            end_time: Some(Instant::now()),
            total_duration_ms: 60000,
            containers_started: 3,
            containers_stopped: 3,
            tests_executed: 10,
            tests_passed: 9,
            tests_failed: 1,
            coverage_percentage: 85.0,
            resource_usage: crate::cleanroom::ResourceUsage {
                peak_cpu_usage_percent: 60.0,
                peak_memory_usage_bytes: 256 * 1024 * 1024,
                peak_disk_usage_bytes: 512 * 1024 * 1024,
                network_bytes_transferred: 512 * 1024,
                container_count: 3,
            },
            performance_metrics: HashMap::new(),
            error_count: 1,
            warning_count: 0,
        };
        
        let comprehensive_report = report.generate_report(&metrics).await.unwrap();
        
        assert_eq!(comprehensive_report.overall_success_rate(), 90.0);
        assert_eq!(comprehensive_report.coverage_percentage(), Some(85.0));
        assert!(comprehensive_report.performance_score() > 0.0);
        
        let summary_text = comprehensive_report.summary();
        assert!(summary_text.contains("Test Report Summary"));
        assert!(summary_text.contains("90.0%"));
        assert!(summary_text.contains("85.0%"));
    }
}