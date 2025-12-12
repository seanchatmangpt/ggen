//! Test metadata collection for intelligent selection
//!
//! This module collects historical test execution data:
//! - Execution times from cargo-nextest JSON reports
//! - Coverage data from cargo-tarpaulin
//! - Failure history from previous test runs
//!
//! Metadata is stored in `.ggen/test-metadata/` for persistent tracking.

use crate::types::{OptResult, OptimizationError, TestId, TestType};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

/// Test metadata collector for historical data gathering
#[derive(Debug)]
pub struct MetadataCollector {
    /// Path to .ggen/test-metadata/ directory
    metadata_dir: PathBuf,
}

impl MetadataCollector {
    /// Create new metadata collector
    ///
    /// # Arguments
    /// * `metadata_dir` - Path to metadata storage directory (default: `.ggen/test-metadata/`)
    pub fn new<P: AsRef<Path>>(metadata_dir: P) -> Self {
        Self {
            metadata_dir: metadata_dir.as_ref().to_path_buf(),
        }
    }

    /// Create collector with default metadata directory
    pub fn with_defaults() -> Self {
        Self::new(".ggen/test-metadata")
    }

    /// Ensure metadata directory exists
    fn ensure_metadata_dir(&self) -> OptResult<()> {
        fs::create_dir_all(&self.metadata_dir)?;
        Ok(())
    }

    /// Collect execution times from cargo-nextest JSON report
    ///
    /// Parses `target/nextest/default/results.json` and extracts execution times
    /// for each test. Returns map of TestId → (TestType, execution_time_ms).
    ///
    /// # Arguments
    /// * `nextest_json_path` - Path to cargo-nextest JSON report
    ///
    /// # Returns
    /// Map of test_id → (test_type, exec_time_ms)
    pub fn collect_execution_times<P: AsRef<Path>>(
        &self,
        nextest_json_path: P,
    ) -> OptResult<HashMap<TestId, (TestType, u64)>> {
        let json_path = nextest_json_path.as_ref();

        if !json_path.exists() {
            return Err(OptimizationError::IoError(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("Nextest JSON not found: {}", json_path.display()),
            )));
        }

        let content = fs::read_to_string(json_path)?;
        let report: NextestReport = serde_json::from_str(&content)?;

        let mut execution_times = HashMap::new();

        for (test_name, test_result) in report.test_results.iter() {
            let test_id = TestId::new(test_name)?;
            let test_type = self.infer_test_type(test_name);
            let exec_time_ms = test_result.exec_time_ms;

            execution_times.insert(test_id, (test_type, exec_time_ms));
        }

        Ok(execution_times)
    }

    /// Collect coverage data from cargo-tarpaulin
    ///
    /// Parses `target/tarpaulin/coverage.json` and extracts line coverage
    /// per test. Returns map of TestId → (lines_covered, total_lines).
    ///
    /// # Arguments
    /// * `tarpaulin_json_path` - Path to tarpaulin JSON report
    ///
    /// # Returns
    /// Map of test_id → (lines_covered, total_lines)
    pub fn collect_coverage_data<P: AsRef<Path>>(
        &self,
        tarpaulin_json_path: P,
    ) -> OptResult<HashMap<TestId, (usize, usize)>> {
        let json_path = tarpaulin_json_path.as_ref();

        if !json_path.exists() {
            return Err(OptimizationError::IoError(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("Tarpaulin JSON not found: {}", json_path.display()),
            )));
        }

        let content = fs::read_to_string(json_path)?;
        let report: TarpaulinReport = serde_json::from_str(&content)?;

        let mut coverage_data = HashMap::new();

        for (test_name, coverage) in report.test_coverage.iter() {
            let test_id = TestId::new(test_name)?;
            coverage_data.insert(test_id, (coverage.lines_covered, coverage.total_lines));
        }

        Ok(coverage_data)
    }

    /// Collect failure history from historical test runs
    ///
    /// Reads `.ggen/test-metadata/failure_history.json` and returns
    /// map of TestId → (failure_count, total_runs).
    ///
    /// # Returns
    /// Map of test_id → (failure_count, total_runs)
    pub fn collect_failure_history(&self) -> OptResult<HashMap<TestId, (u32, u32)>> {
        let history_path = self.metadata_dir.join("failure_history.json");

        if !history_path.exists() {
            // No history yet - return empty map
            return Ok(HashMap::new());
        }

        let content = fs::read_to_string(&history_path)?;
        let history: FailureHistory = serde_json::from_str(&content)?;

        let mut failure_data = HashMap::new();

        for (test_name, stats) in history.test_stats.iter() {
            let test_id = TestId::new(test_name)?;
            failure_data.insert(test_id, (stats.failure_count, stats.total_runs));
        }

        Ok(failure_data)
    }

    /// Update failure history with latest test results
    ///
    /// Appends results from latest test run to `.ggen/test-metadata/failure_history.json`.
    ///
    /// # Arguments
    /// * `test_results` - Map of test_id → (passed: bool)
    pub fn update_failure_history(
        &self,
        test_results: &HashMap<TestId, bool>,
    ) -> OptResult<()> {
        self.ensure_metadata_dir()?;

        let history_path = self.metadata_dir.join("failure_history.json");

        // Load existing history or create new
        let mut history = if history_path.exists() {
            let content = fs::read_to_string(&history_path)?;
            serde_json::from_str::<FailureHistory>(&content)?
        } else {
            FailureHistory {
                test_stats: HashMap::new(),
            }
        };

        // Update stats for each test
        for (test_id, passed) in test_results {
            let stats = history
                .test_stats
                .entry(test_id.as_str().to_string())
                .or_insert(TestStats {
                    failure_count: 0,
                    total_runs: 0,
                });

            stats.total_runs += 1;
            if !passed {
                stats.failure_count += 1;
            }
        }

        // Write updated history
        let json = serde_json::to_string_pretty(&history)?;
        fs::write(&history_path, json)?;

        Ok(())
    }

    /// Infer test type from test name
    ///
    /// # Arguments
    /// * `test_name` - Full test name (e.g., "ggen_core::parser::tests::test_parse_rdf")
    ///
    /// # Returns
    /// TestType::Unit or TestType::Integration based on naming convention
    fn infer_test_type(&self, test_name: &str) -> TestType {
        // Integration tests typically have "integration" in path or are in tests/ directory
        if test_name.contains("integration") || test_name.starts_with("tests::") {
            TestType::Integration
        } else {
            TestType::Unit
        }
    }

    /// Collect all metadata (execution times, coverage, failure history)
    ///
    /// Convenience method that calls all collection methods and combines results.
    ///
    /// # Arguments
    /// * `nextest_json` - Path to cargo-nextest JSON report
    /// * `tarpaulin_json` - Path to tarpaulin JSON report
    ///
    /// # Returns
    /// Combined metadata for all tests
    pub fn collect_all_metadata<P1: AsRef<Path>, P2: AsRef<Path>>(
        &self,
        nextest_json: P1,
        tarpaulin_json: P2,
    ) -> OptResult<TestMetadata> {
        let execution_times = self.collect_execution_times(nextest_json)?;
        let coverage_data = self.collect_coverage_data(tarpaulin_json)?;
        let failure_history = self.collect_failure_history()?;

        Ok(TestMetadata {
            execution_times,
            coverage_data,
            failure_history,
        })
    }
}

/// Combined test metadata from all sources
#[derive(Debug)]
pub struct TestMetadata {
    /// Test execution times: test_id → (test_type, exec_time_ms)
    pub execution_times: HashMap<TestId, (TestType, u64)>,
    /// Coverage data: test_id → (lines_covered, total_lines)
    pub coverage_data: HashMap<TestId, (usize, usize)>,
    /// Failure history: test_id → (failure_count, total_runs)
    pub failure_history: HashMap<TestId, (u32, u32)>,
}

/// cargo-nextest JSON report structure
#[derive(Debug, Deserialize)]
struct NextestReport {
    test_results: HashMap<String, TestResult>,
}

#[derive(Debug, Deserialize)]
struct TestResult {
    #[serde(rename = "exec_time")]
    exec_time_ms: u64,
}

/// cargo-tarpaulin JSON report structure
#[derive(Debug, Deserialize)]
struct TarpaulinReport {
    test_coverage: HashMap<String, CoverageData>,
}

#[derive(Debug, Deserialize)]
struct CoverageData {
    lines_covered: usize,
    total_lines: usize,
}

/// Failure history storage format
#[derive(Debug, Serialize, Deserialize)]
struct FailureHistory {
    test_stats: HashMap<String, TestStats>,
}

#[derive(Debug, Serialize, Deserialize)]
struct TestStats {
    failure_count: u32,
    total_runs: u32,
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn create_temp_collector() -> (MetadataCollector, TempDir) {
        let temp_dir = TempDir::new().unwrap();
        let collector = MetadataCollector::new(temp_dir.path());
        (collector, temp_dir)
    }

    #[test]
    fn test_collector_creation() {
        let (collector, _temp) = create_temp_collector();
        assert!(collector.metadata_dir.to_string_lossy().contains("test"));
    }

    #[test]
    fn test_ensure_metadata_dir() {
        let (collector, temp) = create_temp_collector();
        assert!(!temp.path().exists() || temp.path().read_dir().unwrap().count() == 0);

        collector.ensure_metadata_dir().unwrap();
        assert!(temp.path().exists());
    }

    #[test]
    fn test_infer_test_type_unit() {
        let (collector, _temp) = create_temp_collector();
        let test_type = collector.infer_test_type("ggen_core::parser::tests::test_parse");
        assert!(matches!(test_type, TestType::Unit));
    }

    #[test]
    fn test_infer_test_type_integration() {
        let (collector, _temp) = create_temp_collector();
        let test_type = collector.infer_test_type("tests::integration::test_full_workflow");
        assert!(matches!(test_type, TestType::Integration));
    }

    #[test]
    fn test_collect_failure_history_empty() {
        let (collector, _temp) = create_temp_collector();
        let history = collector.collect_failure_history().unwrap();
        assert_eq!(history.len(), 0);
    }

    #[test]
    fn test_update_failure_history() {
        let (collector, _temp) = create_temp_collector();

        let mut results = HashMap::new();
        results.insert(TestId::new("test1").unwrap(), true); // Passed
        results.insert(TestId::new("test2").unwrap(), false); // Failed

        collector.update_failure_history(&results).unwrap();

        let history = collector.collect_failure_history().unwrap();
        assert_eq!(history.len(), 2);

        let (failures, total) = history.get(&TestId::new("test1").unwrap()).unwrap();
        assert_eq!(*failures, 0);
        assert_eq!(*total, 1);

        let (failures, total) = history.get(&TestId::new("test2").unwrap()).unwrap();
        assert_eq!(*failures, 1);
        assert_eq!(*total, 1);
    }

    #[test]
    fn test_collect_execution_times_file_not_found() {
        let (collector, _temp) = create_temp_collector();
        let result = collector.collect_execution_times("/nonexistent/path.json");
        assert!(result.is_err());
    }

    #[test]
    fn test_collect_coverage_data_file_not_found() {
        let (collector, _temp) = create_temp_collector();
        let result = collector.collect_coverage_data("/nonexistent/path.json");
        assert!(result.is_err());
    }
}
