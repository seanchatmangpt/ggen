//! Cross-platform output comparison
//!
//! Compares test results across multiple platforms to verify determinism.

use crate::error::Result;
use crate::platform::Platform;
use crate::result::TestResult;
use std::collections::HashMap;
use std::path::PathBuf;

/// A difference found between platform outputs
#[derive(Debug, Clone)]
pub struct PlatformDifference {
    /// File path where difference was found
    pub file: PathBuf,
    /// First platform
    pub platform_a: Platform,
    /// Second platform
    pub platform_b: Platform,
    /// Unified diff
    pub diff: String,
}

impl PlatformDifference {
    /// Create a new platform difference
    pub fn new(file: PathBuf, platform_a: Platform, platform_b: Platform, diff: String) -> Self {
        PlatformDifference {
            file,
            platform_a,
            platform_b,
            diff,
        }
    }

    /// Display the difference
    pub fn display(&self) -> String {
        format!(
            "Difference in {} between {} and {}:\n{}",
            self.file.display(),
            self.platform_a,
            self.platform_b,
            self.diff
        )
    }
}

/// Comparison results across multiple platforms
#[derive(Debug)]
pub struct CrossPlatformComparison {
    /// Test results per platform
    pub results: HashMap<Platform, TestResult>,
    /// Reference platform for comparison
    pub reference_platform: Platform,
    /// Differences found
    pub differences: Vec<PlatformDifference>,
}

impl CrossPlatformComparison {
    /// Create a new comparison with a reference platform
    pub fn new(reference: Platform) -> Self {
        CrossPlatformComparison {
            results: HashMap::new(),
            reference_platform: reference,
            differences: Vec::new(),
        }
    }

    /// Add a test result
    pub fn add_result(&mut self, platform: Platform, result: TestResult) {
        self.results.insert(platform, result);
    }

    /// Compute differences between platforms (placeholder)
    pub fn compute_differences(&mut self) -> Result<()> {
        // This will be implemented in Phase 3
        Ok(())
    }

    /// Check if all platforms produced identical output (deterministic)
    pub fn is_deterministic(&self) -> bool {
        // If there are differences, it's not deterministic
        self.differences.is_empty() && self.all_tests_passed()
    }

    /// Check if all tests passed across platforms
    pub fn all_tests_passed(&self) -> bool {
        self.results.values().all(|r| r.is_success())
    }

    /// Get test results for a specific platform
    pub fn get_result(&self, platform: &Platform) -> Option<&TestResult> {
        self.results.get(platform)
    }

    /// Get the number of platforms tested
    pub fn platform_count(&self) -> usize {
        self.results.len()
    }

    /// Get all platforms that were tested
    pub fn platforms(&self) -> Vec<&Platform> {
        self.results.keys().collect()
    }

    /// Display summary
    pub fn summary(&self) -> String {
        let mut summary = String::new();
        summary.push_str(&format!(
            "Cross-platform comparison ({} platforms):\n",
            self.platform_count()
        ));

        for (platform, result) in &self.results {
            summary.push_str(&format!(
                "  - {}: {} {}\n",
                platform,
                result.status,
                result
                    .execution
                    .duration()
                    .map(|d| format!("({:?})", d))
                    .unwrap_or_default()
            ));
        }

        if self.is_deterministic() {
            summary.push_str("\n✓ All platforms produced identical output (deterministic)\n");
        } else if !self.differences.is_empty() {
            summary.push_str(&format!(
                "\n✗ {} platform difference(s) found (non-deterministic)\n",
                self.differences.len()
            ));
        } else {
            summary.push_str("\n⚠ Some tests failed\n");
        }

        summary
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::Utc;
    use uuid::Uuid;

    #[test]
    fn test_platform_difference_creation() {
        let p1 = Platform {
            name: "Linux".to_string(),
            os: crate::platform::Os::Linux,
            arch: crate::platform::Arch::X86_64,
            docker_available: true,
        };
        let p2 = Platform {
            name: "macOS".to_string(),
            os: crate::platform::Os::Darwin,
            arch: crate::platform::Arch::Aarch64,
            docker_available: true,
        };

        let diff = PlatformDifference::new(
            PathBuf::from("output.txt"),
            p1.clone(),
            p2.clone(),
            "--- expected\n+ actual".to_string(),
        );

        assert_eq!(diff.file, PathBuf::from("output.txt"));
        assert_eq!(diff.platform_a, p1);
        assert_eq!(diff.platform_b, p2);
    }

    #[test]
    fn test_cross_platform_comparison_creation() {
        let platform = Platform {
            name: "Linux".to_string(),
            os: crate::platform::Os::Linux,
            arch: crate::platform::Arch::X86_64,
            docker_available: true,
        };

        let comparison = CrossPlatformComparison::new(platform.clone());
        assert_eq!(comparison.reference_platform, platform);
        assert_eq!(comparison.platform_count(), 0);
    }

    #[test]
    fn test_cross_platform_comparison_deterministic() {
        let platform = Platform {
            name: "test".to_string(),
            os: crate::platform::Os::Linux,
            arch: crate::platform::Arch::X86_64,
            docker_available: true,
        };

        let mut comparison = CrossPlatformComparison::new(platform.clone());

        let exec = crate::result::TestExecution {
            id: Uuid::new_v4(),
            fixture: "test".to_string(),
            platform: platform.clone(),
            started_at: Utc::now(),
            ended_at: Some(Utc::now()),
            container_id: None,
        };

        let result = TestResult::passed(exec, vec![]);
        comparison.add_result(platform, result);

        assert!(comparison.all_tests_passed());
        assert!(comparison.is_deterministic());
    }

    #[test]
    fn test_cross_platform_comparison_summary() {
        let platform = Platform {
            name: "test".to_string(),
            os: crate::platform::Os::Linux,
            arch: crate::platform::Arch::X86_64,
            docker_available: true,
        };

        let comparison = CrossPlatformComparison::new(platform.clone());
        let summary = comparison.summary();

        assert!(summary.contains("Cross-platform comparison"));
    }
}
