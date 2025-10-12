//! Coverage tracking for cleanroom testing
//!
//! This module provides coverage tracking following core team best practices:
//! - Test coverage measurement
//! - Code path tracking
//! - Coverage reporting
//! - Coverage analysis

use crate::error::{Result, CleanroomError};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Instant;
use tokio::sync::RwLock;
use uuid::Uuid;

/// Coverage tracker for cleanroom testing
#[derive(Debug)]
pub struct CoverageTracker {
    /// Session ID
    session_id: Uuid,
    /// Coverage data
    coverage_data: Arc<RwLock<CoverageData>>,
    /// Enabled flag
    enabled: bool,
}

/// Coverage data structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoverageData {
    /// Session ID
    pub session_id: Uuid,
    /// Start time
    pub start_time: Instant,
    /// End time
    pub end_time: Option<Instant>,
    /// Test coverage information
    pub test_coverage: HashMap<String, TestCoverage>,
    /// Code coverage information
    pub code_coverage: HashMap<String, CodeCoverage>,
    /// Overall coverage percentage
    pub overall_coverage_percentage: f64,
    /// Coverage statistics
    pub statistics: CoverageStatistics,
}

/// Test coverage information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestCoverage {
    /// Test name
    pub test_name: String,
    /// Test file path
    pub test_file_path: String,
    /// Lines covered
    pub lines_covered: Vec<u32>,
    /// Lines not covered
    pub lines_not_covered: Vec<u32>,
    /// Coverage percentage
    pub coverage_percentage: f64,
    /// Execution time
    pub execution_time_ms: u64,
    /// Test status
    pub test_status: TestStatus,
}

/// Code coverage information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CodeCoverage {
    /// File path
    pub file_path: String,
    /// Total lines
    pub total_lines: u32,
    /// Covered lines
    pub covered_lines: u32,
    /// Coverage percentage
    pub coverage_percentage: f64,
    /// Function coverage
    pub function_coverage: HashMap<String, FunctionCoverage>,
    /// Branch coverage
    pub branch_coverage: HashMap<String, BranchCoverage>,
}

/// Function coverage information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionCoverage {
    /// Function name
    pub function_name: String,
    /// Function signature
    pub function_signature: String,
    /// Lines covered
    pub lines_covered: Vec<u32>,
    /// Coverage percentage
    pub coverage_percentage: f64,
    /// Call count
    pub call_count: u32,
}

/// Branch coverage information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BranchCoverage {
    /// Branch condition
    pub branch_condition: String,
    /// True branch covered
    pub true_branch_covered: bool,
    /// False branch covered
    pub false_branch_covered: bool,
    /// Coverage percentage
    pub coverage_percentage: f64,
    /// Execution count
    pub execution_count: u32,
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
    /// Test pending
    Pending,
}

/// Coverage statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoverageStatistics {
    /// Total tests
    pub total_tests: u32,
    /// Passed tests
    pub passed_tests: u32,
    /// Failed tests
    pub failed_tests: u32,
    /// Skipped tests
    pub skipped_tests: u32,
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

impl CoverageTracker {
    /// Create a new coverage tracker
    pub fn new(session_id: Uuid) -> Self {
        Self {
            session_id,
            coverage_data: Arc::new(RwLock::new(CoverageData::new(session_id))),
            enabled: true,
        }
    }
    
    /// Create a disabled coverage tracker
    pub fn disabled() -> Self {
        Self {
            session_id: Uuid::new_v4(),
            coverage_data: Arc::new(RwLock::new(CoverageData::new(Uuid::new_v4()))),
            enabled: false,
        }
    }
    
    /// Check if coverage tracking is enabled
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }
    
    /// Start coverage tracking for a test
    pub async fn start_test(&self, test_name: String, test_file_path: String) -> Result<()> {
        if !self.enabled {
            return Ok(());
        }
        
        let mut data = self.coverage_data.write().await;
        let test_coverage = TestCoverage {
            test_name: test_name.clone(),
            test_file_path,
            lines_covered: Vec::new(),
            lines_not_covered: Vec::new(),
            coverage_percentage: 0.0,
            execution_time_ms: 0,
            test_status: TestStatus::Pending,
        };
        
        data.test_coverage.insert(test_name, test_coverage);
        Ok(())
    }
    
    /// End coverage tracking for a test
    pub async fn end_test(&self, test_name: &str, status: TestStatus, execution_time_ms: u64) -> Result<()> {
        if !self.enabled {
            return Ok(());
        }
        
        let mut data = self.coverage_data.write().await;
        if let Some(test_coverage) = data.test_coverage.get_mut(test_name) {
            test_coverage.test_status = status;
            test_coverage.execution_time_ms = execution_time_ms;
            
            // Calculate coverage percentage
            let total_lines = test_coverage.lines_covered.len() + test_coverage.lines_not_covered.len();
            if total_lines > 0 {
                test_coverage.coverage_percentage = (test_coverage.lines_covered.len() as f64 / total_lines as f64) * 100.0;
            }
        }
        
        // Update statistics
        self.update_statistics(&mut data).await;
        
        Ok(())
    }
    
    /// Record line coverage
    pub async fn record_line_coverage(&self, test_name: &str, line_number: u32, covered: bool) -> Result<()> {
        if !self.enabled {
            return Ok(());
        }
        
        let mut data = self.coverage_data.write().await;
        if let Some(test_coverage) = data.test_coverage.get_mut(test_name) {
            if covered {
                if !test_coverage.lines_covered.contains(&line_number) {
                    test_coverage.lines_covered.push(line_number);
                }
            } else {
                if !test_coverage.lines_not_covered.contains(&line_number) {
                    test_coverage.lines_not_covered.push(line_number);
                }
            }
        }
        
        Ok(())
    }
    
    /// Record function coverage
    pub async fn record_function_coverage(
        &self,
        file_path: &str,
        function_name: String,
        function_signature: String,
        lines_covered: Vec<u32>,
        call_count: u32,
    ) -> Result<()> {
        if !self.enabled {
            return Ok(());
        }
        
        let mut data = self.coverage_data.write().await;
        let code_coverage = data.code_coverage.entry(file_path.to_string()).or_insert_with(|| {
            CodeCoverage {
                file_path: file_path.to_string(),
                total_lines: 0,
                covered_lines: 0,
                coverage_percentage: 0.0,
                function_coverage: HashMap::new(),
                branch_coverage: HashMap::new(),
            }
        });
        
        let function_coverage = FunctionCoverage {
            function_name: function_name.clone(),
            function_signature,
            lines_covered: lines_covered.clone(),
            coverage_percentage: 0.0,
            call_count,
        };
        
        code_coverage.function_coverage.insert(function_name, function_coverage);
        
        // Update code coverage statistics
        self.update_code_coverage_statistics(code_coverage).await;
        
        Ok(())
    }
    
    /// Record branch coverage
    pub async fn record_branch_coverage(
        &self,
        file_path: &str,
        branch_condition: String,
        true_branch_covered: bool,
        false_branch_covered: bool,
        execution_count: u32,
    ) -> Result<()> {
        if !self.enabled {
            return Ok(());
        }
        
        let mut data = self.coverage_data.write().await;
        let code_coverage = data.code_coverage.entry(file_path.to_string()).or_insert_with(|| {
            CodeCoverage {
                file_path: file_path.to_string(),
                total_lines: 0,
                covered_lines: 0,
                coverage_percentage: 0.0,
                function_coverage: HashMap::new(),
                branch_coverage: HashMap::new(),
            }
        });
        
        let coverage_percentage = if true_branch_covered && false_branch_covered {
            100.0
        } else if true_branch_covered || false_branch_covered {
            50.0
        } else {
            0.0
        };
        
        let branch_coverage = BranchCoverage {
            branch_condition: branch_condition.clone(),
            true_branch_covered,
            false_branch_covered,
            coverage_percentage,
            execution_count,
        };
        
        code_coverage.branch_coverage.insert(branch_condition, branch_coverage);
        
        // Update code coverage statistics
        self.update_code_coverage_statistics(code_coverage).await;
        
        Ok(())
    }
    
    /// Get coverage data
    pub async fn get_coverage_data(&self) -> CoverageData {
        let data = self.coverage_data.read().await;
        data.clone()
    }
    
    /// Get overall coverage percentage
    pub async fn get_overall_coverage_percentage(&self) -> f64 {
        let data = self.coverage_data.read().await;
        data.overall_coverage_percentage
    }
    
    /// Generate coverage report
    pub async fn generate_coverage_report(&self) -> Result<CoverageReport> {
        if !self.enabled {
            return Err(CleanroomError::coverage_error("Coverage tracking is disabled"));
        }
        
        let data = self.coverage_data.read().await;
        
        let mut report = CoverageReport {
            session_id: data.session_id,
            start_time: data.start_time,
            end_time: data.end_time,
            overall_coverage_percentage: data.overall_coverage_percentage,
            test_coverage_summary: Vec::new(),
            code_coverage_summary: Vec::new(),
            statistics: data.statistics.clone(),
            recommendations: Vec::new(),
        };
        
        // Generate test coverage summary
        for (test_name, test_coverage) in &data.test_coverage {
            report.test_coverage_summary.push(TestCoverageSummary {
                test_name: test_name.clone(),
                test_file_path: test_coverage.test_file_path.clone(),
                coverage_percentage: test_coverage.coverage_percentage,
                execution_time_ms: test_coverage.execution_time_ms,
                test_status: test_coverage.test_status.clone(),
                lines_covered_count: test_coverage.lines_covered.len(),
                lines_not_covered_count: test_coverage.lines_not_covered.len(),
            });
        }
        
        // Generate code coverage summary
        for (file_path, code_coverage) in &data.code_coverage {
            report.code_coverage_summary.push(CodeCoverageSummary {
                file_path: file_path.clone(),
                coverage_percentage: code_coverage.coverage_percentage,
                total_lines: code_coverage.total_lines,
                covered_lines: code_coverage.covered_lines,
                function_count: code_coverage.function_coverage.len(),
                branch_count: code_coverage.branch_coverage.len(),
            });
        }
        
        // Generate recommendations
        self.generate_recommendations(&data, &mut report).await;
        
        Ok(report)
    }
    
    /// Update statistics
    async fn update_statistics(&self, data: &mut CoverageData) {
        let mut stats = CoverageStatistics {
            total_tests: 0,
            passed_tests: 0,
            failed_tests: 0,
            skipped_tests: 0,
            total_lines: 0,
            covered_lines: 0,
            total_functions: 0,
            covered_functions: 0,
            total_branches: 0,
            covered_branches: 0,
        };
        
        // Count test statistics
        for test_coverage in data.test_coverage.values() {
            stats.total_tests += 1;
            match test_coverage.test_status {
                TestStatus::Passed => stats.passed_tests += 1,
                TestStatus::Failed => stats.failed_tests += 1,
                TestStatus::Skipped => stats.skipped_tests += 1,
                TestStatus::Pending => {}
            }
        }
        
        // Count code coverage statistics
        for code_coverage in data.code_coverage.values() {
            stats.total_lines += code_coverage.total_lines;
            stats.covered_lines += code_coverage.covered_lines;
            stats.total_functions += code_coverage.function_coverage.len() as u32;
            stats.covered_functions += code_coverage.function_coverage.values()
                .filter(|f| f.coverage_percentage > 0.0)
                .count() as u32;
            stats.total_branches += code_coverage.branch_coverage.len() as u32;
            stats.covered_branches += code_coverage.branch_coverage.values()
                .filter(|b| b.coverage_percentage > 0.0)
                .count() as u32;
        }
        
        data.statistics = stats;
        
        // Calculate overall coverage percentage
        if stats.total_lines > 0 {
            data.overall_coverage_percentage = (stats.covered_lines as f64 / stats.total_lines as f64) * 100.0;
        }
    }
    
    /// Update code coverage statistics
    async fn update_code_coverage_statistics(&self, code_coverage: &mut CodeCoverage) {
        // Calculate total lines
        let mut total_lines = 0;
        let mut covered_lines = 0;
        
        for function_coverage in code_coverage.function_coverage.values() {
            total_lines += function_coverage.lines_covered.len() as u32;
            covered_lines += function_coverage.lines_covered.len() as u32;
        }
        
        code_coverage.total_lines = total_lines;
        code_coverage.covered_lines = covered_lines;
        
        // Calculate coverage percentage
        if total_lines > 0 {
            code_coverage.coverage_percentage = (covered_lines as f64 / total_lines as f64) * 100.0;
        }
    }
    
    /// Generate recommendations
    async fn generate_recommendations(&self, data: &CoverageData, report: &mut CoverageReport) {
        // Low coverage recommendation
        if data.overall_coverage_percentage < 80.0 {
            report.recommendations.push(format!(
                "Overall coverage is {:.1}%, consider adding more tests to reach 80% coverage",
                data.overall_coverage_percentage
            ));
        }
        
        // Failed tests recommendation
        if data.statistics.failed_tests > 0 {
            report.recommendations.push(format!(
                "{} tests failed, review and fix failing tests",
                data.statistics.failed_tests
            ));
        }
        
        // Low function coverage recommendation
        if data.statistics.total_functions > 0 {
            let function_coverage_percentage = (data.statistics.covered_functions as f64 / data.statistics.total_functions as f64) * 100.0;
            if function_coverage_percentage < 90.0 {
                report.recommendations.push(format!(
                    "Function coverage is {:.1}%, consider testing more functions",
                    function_coverage_percentage
                ));
            }
        }
        
        // Low branch coverage recommendation
        if data.statistics.total_branches > 0 {
            let branch_coverage_percentage = (data.statistics.covered_branches as f64 / data.statistics.total_branches as f64) * 100.0;
            if branch_coverage_percentage < 80.0 {
                report.recommendations.push(format!(
                    "Branch coverage is {:.1}%, consider testing more code paths",
                    branch_coverage_percentage
                ));
            }
        }
    }
}

impl CoverageData {
    /// Create new coverage data
    pub fn new(session_id: Uuid) -> Self {
        Self {
            session_id,
            start_time: Instant::now(),
            end_time: None,
            test_coverage: HashMap::new(),
            code_coverage: HashMap::new(),
            overall_coverage_percentage: 0.0,
            statistics: CoverageStatistics {
                total_tests: 0,
                passed_tests: 0,
                failed_tests: 0,
                skipped_tests: 0,
                total_lines: 0,
                covered_lines: 0,
                total_functions: 0,
                covered_functions: 0,
                total_branches: 0,
                covered_branches: 0,
            },
        }
    }
}

/// Coverage report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoverageReport {
    /// Session ID
    pub session_id: Uuid,
    /// Start time
    pub start_time: Instant,
    /// End time
    pub end_time: Option<Instant>,
    /// Overall coverage percentage
    pub overall_coverage_percentage: f64,
    /// Test coverage summary
    pub test_coverage_summary: Vec<TestCoverageSummary>,
    /// Code coverage summary
    pub code_coverage_summary: Vec<CodeCoverageSummary>,
    /// Statistics
    pub statistics: CoverageStatistics,
    /// Recommendations
    pub recommendations: Vec<String>,
}

/// Test coverage summary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestCoverageSummary {
    /// Test name
    pub test_name: String,
    /// Test file path
    pub test_file_path: String,
    /// Coverage percentage
    pub coverage_percentage: f64,
    /// Execution time in milliseconds
    pub execution_time_ms: u64,
    /// Test status
    pub test_status: TestStatus,
    /// Lines covered count
    pub lines_covered_count: usize,
    /// Lines not covered count
    pub lines_not_covered_count: usize,
}

/// Code coverage summary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CodeCoverageSummary {
    /// File path
    pub file_path: String,
    /// Coverage percentage
    pub coverage_percentage: f64,
    /// Total lines
    pub total_lines: u32,
    /// Covered lines
    pub covered_lines: u32,
    /// Function count
    pub function_count: usize,
    /// Branch count
    pub branch_count: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[tokio::test]
    async fn test_coverage_tracker_creation() {
        let session_id = Uuid::new_v4();
        let tracker = CoverageTracker::new(session_id);
        assert!(tracker.is_enabled());
        assert_eq!(tracker.session_id, session_id);
    }
    
    #[tokio::test]
    async fn test_coverage_tracker_disabled() {
        let tracker = CoverageTracker::disabled();
        assert!(!tracker.is_enabled());
    }
    
    #[tokio::test]
    async fn test_coverage_tracking() {
        let session_id = Uuid::new_v4();
        let tracker = CoverageTracker::new(session_id);
        
        // Start test
        tracker.start_test("test1".to_string(), "test1.rs".to_string()).await.unwrap();
        
        // Record line coverage
        tracker.record_line_coverage("test1", 10, true).await.unwrap();
        tracker.record_line_coverage("test1", 11, false).await.unwrap();
        tracker.record_line_coverage("test1", 12, true).await.unwrap();
        
        // End test
        tracker.end_test("test1", TestStatus::Passed, 100).await.unwrap();
        
        // Check coverage data
        let data = tracker.get_coverage_data().await;
        assert_eq!(data.test_coverage.len(), 1);
        assert!(data.test_coverage.contains_key("test1"));
        
        let test_coverage = &data.test_coverage["test1"];
        assert_eq!(test_coverage.test_name, "test1");
        assert_eq!(test_coverage.lines_covered.len(), 2);
        assert_eq!(test_coverage.lines_not_covered.len(), 1);
        assert_eq!(test_coverage.coverage_percentage, 66.66666666666667);
    }
    
    #[tokio::test]
    async fn test_function_coverage() {
        let session_id = Uuid::new_v4();
        let tracker = CoverageTracker::new(session_id);
        
        // Record function coverage
        tracker.record_function_coverage(
            "src/lib.rs",
            "test_function".to_string(),
            "fn test_function() -> bool".to_string(),
            vec![10, 11, 12],
            5,
        ).await.unwrap();
        
        // Check coverage data
        let data = tracker.get_coverage_data().await;
        assert!(data.code_coverage.contains_key("src/lib.rs"));
        
        let code_coverage = &data.code_coverage["src/lib.rs"];
        assert!(code_coverage.function_coverage.contains_key("test_function"));
        
        let function_coverage = &code_coverage.function_coverage["test_function"];
        assert_eq!(function_coverage.function_name, "test_function");
        assert_eq!(function_coverage.lines_covered.len(), 3);
        assert_eq!(function_coverage.call_count, 5);
    }
    
    #[tokio::test]
    async fn test_branch_coverage() {
        let session_id = Uuid::new_v4();
        let tracker = CoverageTracker::new(session_id);
        
        // Record branch coverage
        tracker.record_branch_coverage(
            "src/lib.rs",
            "if x > 0".to_string(),
            true,
            false,
            10,
        ).await.unwrap();
        
        // Check coverage data
        let data = tracker.get_coverage_data().await;
        assert!(data.code_coverage.contains_key("src/lib.rs"));
        
        let code_coverage = &data.code_coverage["src/lib.rs"];
        assert!(code_coverage.branch_coverage.contains_key("if x > 0"));
        
        let branch_coverage = &code_coverage.branch_coverage["if x > 0"];
        assert_eq!(branch_coverage.branch_condition, "if x > 0");
        assert!(branch_coverage.true_branch_covered);
        assert!(!branch_coverage.false_branch_covered);
        assert_eq!(branch_coverage.coverage_percentage, 50.0);
        assert_eq!(branch_coverage.execution_count, 10);
    }
    
    #[tokio::test]
    async fn test_coverage_report() {
        let session_id = Uuid::new_v4();
        let tracker = CoverageTracker::new(session_id);
        
        // Start and end test
        tracker.start_test("test1".to_string(), "test1.rs".to_string()).await.unwrap();
        tracker.record_line_coverage("test1", 10, true).await.unwrap();
        tracker.end_test("test1", TestStatus::Passed, 100).await.unwrap();
        
        // Generate report
        let report = tracker.generate_coverage_report().await.unwrap();
        assert_eq!(report.session_id, session_id);
        assert_eq!(report.test_coverage_summary.len(), 1);
        assert_eq!(report.test_coverage_summary[0].test_name, "test1");
    }
}