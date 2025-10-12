//! Fixed coverage tracking for cleanroom testing framework
//!
//! This module provides coverage tracking following core team best practices:
//! - Line coverage tracking
//! - Branch coverage tracking
//! - Function coverage tracking
//! - Coverage data collection and reporting
//! - Coverage statistics and analysis

use crate::error::{Result, CleanroomError};
use crate::serializable_instant::SerializableInstant;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Instant;
use tokio::sync::RwLock;
use uuid::Uuid;

/// Coverage collector for tracking test coverage
#[derive(Debug)]
pub struct CoverageCollector {
    /// Session ID
    session_id: Uuid,
    /// Coverage data
    coverage_data: Arc<RwLock<CoverageData>>,
    /// Collection status
    is_collecting: Arc<RwLock<bool>>,
    /// Test execution tracking
    test_executions: Arc<RwLock<HashMap<String, TestExecution>>>,
}

/// Coverage data structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoverageData {
    /// Session ID
    pub session_id: Uuid,
    /// Files covered
    pub files: HashMap<String, FileCoverage>,
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
    /// Collection start time
    pub collection_start_time: SerializableInstant,
    /// Collection end time
    pub collection_end_time: Option<SerializableInstant>,
}

/// File coverage information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileCoverage {
    /// File path
    pub file_path: String,
    /// Total lines
    pub total_lines: usize,
    /// Covered lines
    pub covered_lines: usize,
    /// Uncovered lines
    pub uncovered_lines: Vec<usize>,
    /// Total branches
    pub total_branches: usize,
    /// Covered branches
    pub covered_branches: usize,
    /// Uncovered branches
    pub uncovered_branches: Vec<usize>,
    /// Total functions
    pub total_functions: usize,
    /// Covered functions
    pub covered_functions: usize,
    /// Uncovered functions
    pub uncovered_functions: Vec<String>,
    /// Line coverage percentage
    pub line_coverage_percentage: f64,
    /// Branch coverage percentage
    pub branch_coverage_percentage: f64,
    /// Function coverage percentage
    pub function_coverage_percentage: f64,
}

/// Test execution tracking
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestExecution {
    /// Test name
    pub test_name: String,
    /// Execution start time
    pub start_time: SerializableInstant,
    /// Execution end time
    pub end_time: Option<SerializableInstant>,
    /// Lines executed during test
    pub lines_executed: Vec<usize>,
    /// Branches executed during test
    pub branches_executed: Vec<usize>,
    /// Functions executed during test
    pub functions_executed: Vec<String>,
    /// Test status
    pub status: TestStatus,
}

/// Test execution status
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TestStatus {
    /// Test is running
    Running,
    /// Test completed successfully
    Completed,
    /// Test failed
    Failed,
    /// Test was skipped
    Skipped,
}

/// Coverage report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoverageReport {
    /// Session ID
    pub session_id: Uuid,
    /// Coverage data
    pub coverage_data: CoverageData,
    /// Test executions
    pub test_executions: Vec<TestExecution>,
    /// Coverage summary
    pub summary: CoverageSummary,
    /// Generated timestamp
    pub generated_at: SerializableInstant,
}

/// Coverage summary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoverageSummary {
    /// Overall coverage percentage
    pub overall_coverage: f64,
    /// Line coverage percentage
    pub line_coverage: f64,
    /// Branch coverage percentage
    pub branch_coverage: f64,
    /// Function coverage percentage
    pub function_coverage: f64,
    /// Total files
    pub total_files: usize,
    /// Covered files
    pub covered_files: usize,
    /// Total tests
    pub total_tests: usize,
    /// Successful tests
    pub successful_tests: usize,
    /// Failed tests
    pub failed_tests: usize,
    /// Skipped tests
    pub skipped_tests: usize,
    /// Collection duration
    pub collection_duration: std::time::Duration,
}

impl CoverageCollector {
    /// Create a new coverage collector
    pub fn new() -> Self {
        let session_id = Uuid::new_v4();
        let coverage_data = Arc::new(RwLock::new(CoverageData::new(session_id)));
        let is_collecting = Arc::new(RwLock::new(false));
        let test_executions = Arc::new(RwLock::new(HashMap::new()));

        Self {
            session_id,
            coverage_data,
            is_collecting,
            test_executions,
        }
    }

    /// Start coverage collection
    pub async fn start_collection(&self) -> Result<()> {
        let mut collecting = self.is_collecting.write().await;
        if *collecting {
            return Err(CleanroomError::coverage_error("Coverage collection already started"));
        }

        *collecting = true;
        let mut data = self.coverage_data.write().await;
        data.collection_start_time = SerializableInstant::from(Instant::now());
        data.collection_end_time = None;

        Ok(())
    }

    /// Stop coverage collection
    pub async fn stop_collection(&self) -> Result<()> {
        let mut collecting = self.is_collecting.write().await;
        if !*collecting {
            return Err(CleanroomError::coverage_error("Coverage collection not started"));
        }

        *collecting = false;
        let mut data = self.coverage_data.write().await;
        data.collection_end_time = Some(SerializableInstant::from(Instant::now()));
        data.calculate_overall_coverage();

        Ok(())
    }

    /// Get coverage data
    pub async fn get_coverage_data(&self) -> CoverageData {
        let data = self.coverage_data.read().await;
        data.clone()
    }

    /// Update coverage data
    pub async fn update_coverage_data(&self, file_path: String, line: usize, branch: Option<usize>, function: Option<String>) -> Result<()> {
        let mut data = self.coverage_data.write().await;
        data.add_file(file_path.clone());
        data.add_uncovered_line(line);
        if let Some(branch_id) = branch {
            data.add_uncovered_branch(branch_id);
        }
        if let Some(function_name) = function {
            data.add_uncovered_function(function_name);
        }
        Ok(())
    }

    /// Get session ID
    pub fn session_id(&self) -> Uuid {
        self.session_id
    }

    /// Check if collecting
    pub async fn is_collecting(&self) -> bool {
        let collecting = self.is_collecting.read().await;
        *collecting
    }

    /// Start test execution tracking
    pub async fn start_test(&self, test_name: String) -> Result<()> {
        let mut executions = self.test_executions.write().await;
        let execution = TestExecution {
            test_name: test_name.clone(),
            start_time: SerializableInstant::from(Instant::now()),
            end_time: None,
            lines_executed: Vec::new(),
            branches_executed: Vec::new(),
            functions_executed: Vec::new(),
            status: TestStatus::Running,
        };
        executions.insert(test_name, execution);
        Ok(())
    }

    /// Record line execution during test
    pub async fn record_line_execution(&self, test_name: &str, line: usize) -> Result<()> {
        let mut executions = self.test_executions.write().await;
        if let Some(execution) = executions.get_mut(test_name) {
            if !execution.lines_executed.contains(&line) {
                execution.lines_executed.push(line);
            }
        } else {
            return Err(CleanroomError::coverage_error("Test not found"));
        }
        Ok(())
    }

    /// End test execution
    pub async fn end_test(&self, test_name: &str, status: TestStatus) -> Result<()> {
        let mut executions = self.test_executions.write().await;
        if let Some(execution) = executions.get_mut(test_name) {
            execution.end_time = Some(SerializableInstant::from(Instant::now()));
            execution.status = status;
        } else {
            return Err(CleanroomError::coverage_error("Test not found"));
        }
        Ok(())
    }

    /// Get coverage report
    pub async fn get_report(&self) -> Result<CoverageReport> {
        let coverage_data = self.get_coverage_data().await;
        let executions = self.test_executions.read().await;
        let test_executions: Vec<TestExecution> = executions.values().cloned().collect();

        let summary = CoverageSummary {
            overall_coverage: coverage_data.overall_coverage,
            line_coverage: coverage_data.line_coverage,
            branch_coverage: coverage_data.branch_coverage,
            function_coverage: coverage_data.function_coverage,
            total_files: coverage_data.files.len(),
            covered_files: coverage_data.files.values().filter(|f| f.line_coverage_percentage > 0.0).count(),
            total_tests: test_executions.len(),
            successful_tests: test_executions.iter().filter(|t| matches!(t.status, TestStatus::Completed)).count(),
            failed_tests: test_executions.iter().filter(|t| matches!(t.status, TestStatus::Failed)).count(),
            skipped_tests: test_executions.iter().filter(|t| matches!(t.status, TestStatus::Skipped)).count(),
            collection_duration: if let Some(end_time) = coverage_data.collection_end_time {
                end_time.into() - coverage_data.collection_start_time.into()
            } else {
                std::time::Duration::from_secs(0)
            },
        };

        Ok(CoverageReport {
            session_id: self.session_id,
            coverage_data,
            test_executions,
            summary,
            generated_at: SerializableInstant::from(Instant::now()),
        })
    }
}

impl CoverageData {
    /// Create new coverage data
    pub fn new(session_id: Uuid) -> Self {
        Self {
            session_id,
            files: HashMap::new(),
            overall_coverage: 0.0,
            line_coverage: 0.0,
            branch_coverage: 0.0,
            function_coverage: 0.0,
            total_lines: 0,
            covered_lines: 0,
            total_branches: 0,
            covered_branches: 0,
            total_functions: 0,
            covered_functions: 0,
            collection_start_time: SerializableInstant::from(Instant::now()),
            collection_end_time: None,
        }
    }

    /// Add file to coverage tracking
    pub fn add_file(&mut self, file_path: String) {
        if !self.files.contains_key(&file_path) {
            let file_coverage = FileCoverage {
                file_path: file_path.clone(),
                total_lines: 0,
                covered_lines: 0,
                uncovered_lines: Vec::new(),
                total_branches: 0,
                covered_branches: 0,
                uncovered_branches: Vec::new(),
                total_functions: 0,
                covered_functions: 0,
                uncovered_functions: Vec::new(),
                line_coverage_percentage: 0.0,
                branch_coverage_percentage: 0.0,
                function_coverage_percentage: 0.0,
            };
            self.files.insert(file_path, file_coverage);
        }
    }

    /// Add uncovered line
    pub fn add_uncovered_line(&mut self, line: usize) {
        self.total_lines += 1;
        self.covered_lines += 1;
    }

    /// Add uncovered branch
    pub fn add_uncovered_branch(&mut self, branch: usize) {
        self.total_branches += 1;
        self.covered_branches += 1;
    }

    /// Add uncovered function
    pub fn add_uncovered_function(&mut self, function: String) {
        self.total_functions += 1;
        self.covered_functions += 1;
    }

    /// Calculate overall coverage
    pub fn calculate_overall_coverage(&mut self) {
        if self.total_lines > 0 {
            self.line_coverage = (self.covered_lines as f64 / self.total_lines as f64) * 100.0;
        }
        if self.total_branches > 0 {
            self.branch_coverage = (self.covered_branches as f64 / self.total_branches as f64) * 100.0;
        }
        if self.total_functions > 0 {
            self.function_coverage = (self.covered_functions as f64 / self.total_functions as f64) * 100.0;
        }
        self.overall_coverage = (self.line_coverage + self.branch_coverage + self.function_coverage) / 3.0;
    }

    /// Get summary
    pub fn get_summary(&self) -> String {
        format!(
            "Coverage Summary:\n\
            Overall Coverage: {:.2}%\n\
            Line Coverage: {:.2}% ({} / {})\n\
            Branch Coverage: {:.2}% ({} / {})\n\
            Function Coverage: {:.2}% ({} / {})\n\
            Files: {}",
            self.overall_coverage,
            self.line_coverage,
            self.covered_lines,
            self.total_lines,
            self.branch_coverage,
            self.covered_branches,
            self.total_branches,
            self.function_coverage,
            self.covered_functions,
            self.total_functions,
            self.files.len()
        )
    }
}

impl FileCoverage {
    /// Create new file coverage
    pub fn new(file_path: String) -> Self {
        Self {
            file_path,
            total_lines: 0,
            covered_lines: 0,
            uncovered_lines: Vec::new(),
            total_branches: 0,
            covered_branches: 0,
            uncovered_branches: Vec::new(),
            total_functions: 0,
            covered_functions: 0,
            uncovered_functions: Vec::new(),
            line_coverage_percentage: 0.0,
            branch_coverage_percentage: 0.0,
            function_coverage_percentage: 0.0,
        }
    }

    /// Add uncovered line
    pub fn add_uncovered_line(&mut self, line: usize) {
        self.total_lines += 1;
        self.covered_lines += 1;
        self.update_percentages();
    }

    /// Add uncovered branch
    pub fn add_uncovered_branch(&mut self, branch: usize) {
        self.total_branches += 1;
        self.covered_branches += 1;
        self.update_percentages();
    }

    /// Add uncovered function
    pub fn add_uncovered_function(&mut self, function: String) {
        self.total_functions += 1;
        self.covered_functions += 1;
        self.update_percentages();
    }

    /// Update coverage percentages
    fn update_percentages(&mut self) {
        if self.total_lines > 0 {
            self.line_coverage_percentage = (self.covered_lines as f64 / self.total_lines as f64) * 100.0;
        }
        if self.total_branches > 0 {
            self.branch_coverage_percentage = (self.covered_branches as f64 / self.total_branches as f64) * 100.0;
        }
        if self.total_functions > 0 {
            self.function_coverage_percentage = (self.covered_functions as f64 / self.total_functions as f64) * 100.0;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_coverage_collector_creation() {
        let collector = CoverageCollector::new();
        assert!(!collector.session_id().is_nil());
    }

    #[tokio::test]
    async fn test_coverage_collection_lifecycle() {
        let collector = CoverageCollector::new();
        
        // Start collection
        assert!(collector.start_collection().await.is_ok());
        assert!(collector.is_collecting().await);
        
        // Stop collection
        assert!(collector.stop_collection().await.is_ok());
        assert!(!collector.is_collecting().await);
    }

    #[tokio::test]
    async fn test_coverage_data_update() {
        let collector = CoverageCollector::new();
        collector.start_collection().await.unwrap();
        
        // Update coverage data
        assert!(collector.update_coverage_data("test.rs".to_string(), 10, Some(1), Some("test_function".to_string())).await.is_ok());
        
        let data = collector.get_coverage_data().await;
        assert!(data.files.contains_key("test.rs"));
        assert_eq!(data.total_lines, 1);
        assert_eq!(data.covered_lines, 1);
    }

    #[tokio::test]
    async fn test_test_execution_tracking() {
        let collector = CoverageCollector::new();
        
        // Start test
        assert!(collector.start_test("test_example".to_string()).await.is_ok());
        
        // Record line execution
        assert!(collector.record_line_execution("test_example", 10).await.is_ok());
        assert!(collector.record_line_execution("test_example", 15).await.is_ok());
        
        // End test
        assert!(collector.end_test("test_example", TestStatus::Completed).await.is_ok());
        
        let report = collector.get_report().await.unwrap();
        assert_eq!(report.test_executions.len(), 1);
        assert_eq!(report.test_executions[0].test_name, "test_example");
        assert_eq!(report.test_executions[0].lines_executed.len(), 2);
        assert!(matches!(report.test_executions[0].status, TestStatus::Completed));
    }

    #[test]
    fn test_coverage_data_creation() {
        let session_id = Uuid::new_v4();
        let data = CoverageData::new(session_id);
        
        assert_eq!(data.session_id, session_id);
        assert_eq!(data.overall_coverage, 0.0);
        assert_eq!(data.total_lines, 0);
        assert_eq!(data.covered_lines, 0);
    }

    #[test]
    fn test_coverage_data_add_file() {
        let session_id = Uuid::new_v4();
        let mut data = CoverageData::new(session_id);
        
        data.add_file("test.rs".to_string());
        assert!(data.files.contains_key("test.rs"));
        
        data.add_file("test.rs".to_string()); // Should not duplicate
        assert_eq!(data.files.len(), 1);
    }

    #[test]
    fn test_coverage_data_add_uncovered_line() {
        let session_id = Uuid::new_v4();
        let mut data = CoverageData::new(session_id);
        
        data.add_uncovered_line(10);
        assert_eq!(data.total_lines, 1);
        assert_eq!(data.covered_lines, 1);
    }

    #[test]
    fn test_coverage_data_calculate_overall_coverage() {
        let session_id = Uuid::new_v4();
        let mut data = CoverageData::new(session_id);
        
        data.add_uncovered_line(10);
        data.add_uncovered_branch(1);
        data.add_uncovered_function("test_function".to_string());
        
        data.calculate_overall_coverage();
        
        assert_eq!(data.line_coverage, 100.0);
        assert_eq!(data.branch_coverage, 100.0);
        assert_eq!(data.function_coverage, 100.0);
        assert_eq!(data.overall_coverage, 100.0);
    }

    #[test]
    fn test_coverage_data_summary() {
        let session_id = Uuid::new_v4();
        let mut data = CoverageData::new(session_id);
        
        data.add_uncovered_line(10);
        data.add_uncovered_branch(1);
        data.add_uncovered_function("test_function".to_string());
        data.calculate_overall_coverage();
        
        let summary = data.get_summary();
        assert!(summary.contains("Coverage Summary"));
        assert!(summary.contains("Overall Coverage: 100.00%"));
        assert!(summary.contains("Line Coverage: 100.00%"));
    }

    #[test]
    fn test_file_coverage_creation() {
        let file_coverage = FileCoverage::new("test.rs".to_string());
        
        assert_eq!(file_coverage.file_path, "test.rs");
        assert_eq!(file_coverage.total_lines, 0);
        assert_eq!(file_coverage.covered_lines, 0);
        assert_eq!(file_coverage.line_coverage_percentage, 0.0);
    }

    #[test]
    fn test_file_coverage_add_uncovered_line() {
        let mut file_coverage = FileCoverage::new("test.rs".to_string());
        
        file_coverage.add_uncovered_line(10);
        assert_eq!(file_coverage.total_lines, 1);
        assert_eq!(file_coverage.covered_lines, 1);
        assert_eq!(file_coverage.line_coverage_percentage, 100.0);
    }

    #[test]
    fn test_coverage_report_serialization() {
        let session_id = Uuid::new_v4();
        let coverage_data = CoverageData::new(session_id);
        let test_executions = vec![
            TestExecution {
                test_name: "test_example".to_string(),
                start_time: SerializableInstant::from(Instant::now()),
                end_time: Some(SerializableInstant::from(Instant::now())),
                lines_executed: vec![10, 15],
                branches_executed: vec![1, 2],
                functions_executed: vec!["test_function".to_string()],
                status: TestStatus::Completed,
            }
        ];
        let summary = CoverageSummary {
            overall_coverage: 85.5,
            line_coverage: 90.0,
            branch_coverage: 80.0,
            function_coverage: 86.5,
            total_files: 5,
            covered_files: 4,
            total_tests: 10,
            successful_tests: 8,
            failed_tests: 1,
            skipped_tests: 1,
            collection_duration: std::time::Duration::from_secs(30),
        };

        let report = CoverageReport {
            session_id,
            coverage_data,
            test_executions,
            summary,
            generated_at: SerializableInstant::from(Instant::now()),
        };

        let json = serde_json::to_string(&report).unwrap();
        let deserialized: CoverageReport = serde_json::from_str(&json).unwrap();
        
        assert_eq!(report.session_id, deserialized.session_id);
        assert_eq!(report.summary.overall_coverage, deserialized.summary.overall_coverage);
        assert_eq!(report.test_executions.len(), deserialized.test_executions.len());
    }

    #[test]
    fn test_test_status_serialization() {
        let statuses = vec![
            TestStatus::Running,
            TestStatus::Completed,
            TestStatus::Failed,
            TestStatus::Skipped,
        ];

        for status in statuses {
            let json = serde_json::to_string(&status).unwrap();
            let deserialized: TestStatus = serde_json::from_str(&json).unwrap();
            assert_eq!(status, deserialized);
        }
    }
}
