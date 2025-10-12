//! Coverage collection and merging for deterministic testing
//!
//! Handles collection of coverage data from containers with path remapping
//! and merging on the host for accurate coverage reporting.
//!
//! WIP: Implement coverage collection and merging

use crate::error::{CoverageError, Result};
use std::fs;
use std::path::PathBuf;
use std::process::Command;

/// Coverage collector for gathering and merging coverage data
pub struct CoverageCollector {
    /// Working directory for coverage files
    work_dir: PathBuf,
    /// LLVM tools path (defaults to system PATH)
    llvm_tools_path: Option<PathBuf>,
}

impl CoverageCollector {
    /// Create a new coverage collector
    pub fn new() -> Result<Self> {
        let work_dir = std::env::temp_dir().join("cleanroom-coverage");
        fs::create_dir_all(&work_dir).map_err(|e| {
            CoverageError::Msg(format!("Failed to create coverage work directory: {}", e))
        })?;

        Ok(Self {
            work_dir,
            llvm_tools_path: None,
        })
    }

    /// Set custom LLVM tools path
    pub fn with_llvm_tools(mut self, path: PathBuf) -> Self {
        self.llvm_tools_path = Some(path);
        self
    }

    /// Collect coverage data from a container
    pub fn collect_from_container(&self, container_id: &str) -> Result<CoverageData> {
        // Create a unique filename for this collection
        let timestamp = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs();
        let filename = format!("coverage_{}_{}.profraw", container_id, timestamp);
        let local_path = self.work_dir.join(&filename);

        // Copy coverage file from container
        let copy_output = Command::new("docker")
            .arg("cp")
            .arg(format!("{}:/workdir/coverage.profraw", container_id))
            .arg(&local_path)
            .output()
            .map_err(|e| {
                CoverageError::Msg(format!("Failed to copy coverage from container: {}", e))
            })?;

        if !copy_output.status.success() {
            return Err(CoverageError::Msg(format!(
                "Failed to copy coverage from container: {}",
                String::from_utf8_lossy(&copy_output.stderr)
            ))
            .into());
        }

        // Verify the file exists and has content
        if !local_path.exists() {
            return Err(CoverageError::Msg("Coverage file not found after copy".into()).into());
        }

        let metadata = fs::metadata(&local_path).map_err(|e| {
            CoverageError::Msg(format!("Failed to read coverage file metadata: {}", e))
        })?;

        if metadata.len() == 0 {
            return Err(CoverageError::Msg("Coverage file is empty".into()).into());
        }

        Ok(CoverageData {
            path: local_path,
            format: CoverageFormat::Profraw,
        })
    }

    /// Merge coverage data with path remapping
    pub fn merge_with_remap(&self, data: CoverageData, remap: PathRemap) -> Result<MergedCoverage> {
        // Create merged coverage file
        let merged_filename = format!(
            "merged_{}.profdata",
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_secs()
        );
        let merged_path = self.work_dir.join(&merged_filename);

        // Use llvm-profdata to merge coverage data
        let mut profdata_cmd = Command::new("llvm-profdata");
        if let Some(ref llvm_path) = self.llvm_tools_path {
            profdata_cmd = Command::new(llvm_path.join("llvm-profdata"));
        }

        let profdata_output = profdata_cmd
            .arg("merge")
            .arg("-sparse")
            .arg(&data.path)
            .arg("-o")
            .arg(&merged_path)
            .output()
            .map_err(|e| {
                CoverageError::MergeFailed(format!("Failed to merge coverage data: {}", e))
            })?;

        if !profdata_output.status.success() {
            return Err(CoverageError::MergeFailed(format!(
                "Failed to merge coverage data: {}",
                String::from_utf8_lossy(&profdata_output.stderr)
            ))
            .into());
        }

        // Generate coverage report with path remapping
        let report_filename = format!(
            "report_{}.txt",
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_secs()
        );
        let report_path = self.work_dir.join(&report_filename);

        let mut cov_cmd = Command::new("llvm-cov");
        if let Some(ref llvm_path) = self.llvm_tools_path {
            cov_cmd = Command::new(llvm_path.join("llvm-cov"));
        }

        let cov_output = cov_cmd
            .arg("show")
            .arg("-format=text")
            .arg("-instr-profile")
            .arg(&merged_path)
            .arg("-path-equivalence")
            .arg(format!("{},{}", remap.from.display(), remap.to.display()))
            .arg("-o")
            .arg(&report_path)
            .output()
            .map_err(|e| {
                CoverageError::MergeFailed(format!("Failed to generate coverage report: {}", e))
            })?;

        if !cov_output.status.success() {
            return Err(CoverageError::MergeFailed(format!(
                "Failed to generate coverage report: {}",
                String::from_utf8_lossy(&cov_output.stderr)
            ))
            .into());
        }

        // Parse coverage report to extract statistics
        let report_content = fs::read_to_string(&report_path).map_err(|e| {
            CoverageError::MergeFailed(format!("Failed to read coverage report: {}", e))
        })?;

        let mut lines_covered = 0u64;
        let mut lines_total = 0u64;

        // Simple parsing of llvm-cov text output
        // This is a basic implementation - in production, you'd want more robust parsing
        for line in report_content.lines() {
            if line.contains("Lines executed:") {
                // Extract coverage statistics from the report
                // Format: "Lines executed: X% of Y"
                if let Some(percent_start) = line.find("Lines executed:") {
                    let after_percent = &line[percent_start + 15..];
                    if let Some(percent_end) = after_percent.find('%') {
                        if let Some(of_start) = after_percent.find("of ") {
                            if let Ok(total) = after_percent[of_start + 3..].trim().parse::<u64>() {
                                lines_total = total;
                                if let Ok(percent) =
                                    after_percent[..percent_end].trim().parse::<f64>()
                                {
                                    lines_covered = ((percent / 100.0) * total as f64) as u64;
                                }
                            }
                        }
                    }
                }
            }
        }

        let mut merged = MergedCoverage {
            lines_covered,
            lines_total,
            percentage: 0.0,
        };
        merged.calculate_percentage();

        Ok(merged)
    }

    /// Clean up temporary coverage files
    pub fn cleanup(&self) -> Result<()> {
        if self.work_dir.exists() {
            fs::remove_dir_all(&self.work_dir).map_err(|e| {
                CoverageError::Msg(format!("Failed to cleanup coverage directory: {}", e))
            })?;
        }
        Ok(())
    }
}

/// Raw coverage data from a container
pub struct CoverageData {
    /// Path to coverage file
    pub path: PathBuf,
    /// Coverage format
    pub format: CoverageFormat,
}

/// Coverage format types
#[derive(Debug, Clone)]
pub enum CoverageFormat {
    /// LLVM profraw format
    Profraw,
    /// Other formats as needed
    Other(String),
}

/// Path remapping configuration
pub struct PathRemap {
    /// Source path in container
    pub from: PathBuf,
    /// Target path on host
    pub to: PathBuf,
}

/// Merged coverage data ready for reporting
pub struct MergedCoverage {
    /// Total lines covered
    pub lines_covered: u64,
    /// Total lines in codebase
    pub lines_total: u64,
    /// Coverage percentage
    pub percentage: f64,
}

impl MergedCoverage {
    /// Calculate coverage percentage
    pub fn calculate_percentage(&mut self) {
        if self.lines_total > 0 {
            self.percentage = (self.lines_covered as f64 / self.lines_total as f64) * 100.0;
        } else {
            self.percentage = 0.0;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_coverage_collector_creation() {
        let collector = CoverageCollector::new().unwrap();
        assert!(collector.work_dir.exists());
        assert!(collector.llvm_tools_path.is_none());
    }

    #[test]
    fn test_coverage_collector_with_llvm_tools() {
        let llvm_path = PathBuf::from("/usr/local/bin");
        let collector = CoverageCollector::new()
            .unwrap()
            .with_llvm_tools(llvm_path.clone());
        assert_eq!(collector.llvm_tools_path, Some(llvm_path));
    }

    #[test]
    fn test_merged_coverage_calculation() {
        let mut coverage = MergedCoverage {
            lines_covered: 75,
            lines_total: 100,
            percentage: 0.0,
        };
        coverage.calculate_percentage();
        assert_eq!(coverage.percentage, 75.0);
    }

    #[test]
    fn test_merged_coverage_zero_total() {
        let mut coverage = MergedCoverage {
            lines_covered: 0,
            lines_total: 0,
            percentage: 0.0,
        };
        coverage.calculate_percentage();
        assert_eq!(coverage.percentage, 0.0);
    }

    #[test]
    fn test_path_remap_creation() {
        let remap = PathRemap {
            from: PathBuf::from("/workdir/src"),
            to: PathBuf::from("/home/user/src"),
        };
        assert_eq!(remap.from, PathBuf::from("/workdir/src"));
        assert_eq!(remap.to, PathBuf::from("/home/user/src"));
    }

    #[test]
    fn test_coverage_data_creation() {
        let data = CoverageData {
            path: PathBuf::from("/tmp/coverage.profraw"),
            format: CoverageFormat::Profraw,
        };
        assert_eq!(data.path, PathBuf::from("/tmp/coverage.profraw"));
        assert!(matches!(data.format, CoverageFormat::Profraw));
    }

    #[test]
    fn test_coverage_format_other() {
        let format = CoverageFormat::Other("custom".to_string());
        assert!(matches!(format, CoverageFormat::Other(ref s) if s == "custom"));
    }
}
