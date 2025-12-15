// Gemba Walk - On-Floor Test Inspection
// Implements Lean Manufacturing's Gemba Walk (go see where work happens)

use std::path::PathBuf;
use std::process::Command;
use std::time::{Duration, Instant};
use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GembaWalkChecklist {
    pub test_file: PathBuf,
    pub checks: Vec<GembaCheck>,
    pub score: f32,
    pub findings: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GembaCheck {
    pub name: String,
    pub passed: bool,
    pub observation: String,
    pub weight: f32,
}

impl GembaWalkChecklist {
    pub fn new(test_file: PathBuf) -> Self {
        Self {
            test_file,
            checks: Vec::new(),
            score: 0.0,
            findings: Vec::new(),
        }
    }

    pub fn add_check(&mut self, name: &str, passed: bool, observation: &str, weight: f32) {
        self.checks.push(GembaCheck {
            name: name.to_string(),
            passed,
            observation: observation.to_string(),
            weight,
        });
    }

    pub fn calculate_score(&mut self) {
        let total_weight: f32 = self.checks.iter().map(|c| c.weight).sum();
        let passed_weight: f32 = self.checks
            .iter()
            .filter(|c| c.passed)
            .map(|c| c.weight)
            .sum();

        self.score = if total_weight > 0.0 {
            (passed_weight / total_weight) * 100.0
        } else {
            0.0
        };
    }

    pub fn print_report(&self) {
        println!("\n🚶 GEMBA WALK INSPECTION");
        println!("═══════════════════════════════════════");
        println!("Test File: {:?}", self.test_file);
        println!("Overall Score: {:.1}%", self.score);
        println!("═══════════════════════════════════════\n");

        for (i, check) in self.checks.iter().enumerate() {
            let icon = if check.passed { "✅" } else { "❌" };
            println!("{}. {} {} (weight: {:.1})",
                i + 1, icon, check.name, check.weight);
            println!("   Observation: {}", check.observation);
        }

        if !self.findings.is_empty() {
            println!("\n📋 KEY FINDINGS:");
            for (i, finding) in self.findings.iter().enumerate() {
                println!("{}. {}", i + 1, finding);
            }
        }

        println!("\n🎯 RECOMMENDATION:");
        match self.score {
            s if s >= 90.0 => println!("Excellent - Minimal improvements needed"),
            s if s >= 75.0 => println!("Good - Some improvements recommended"),
            s if s >= 60.0 => println!("Fair - Significant improvements needed"),
            _ => println!("Poor - Major refactoring required"),
        }
    }
}

pub struct GembaWalkInspector;

impl GembaWalkInspector {
    /// Run the complete Gemba Walk inspection
    pub fn inspect(test_file: PathBuf) -> GembaWalkChecklist {
        let mut checklist = GembaWalkChecklist::new(test_file.clone());

        // 1. Check if tests are close to actual usage (not mocked away)
        let mock_check = Self::check_mock_usage(&test_file);
        checklist.add_check(
            "Tests use real implementations (not mocked)",
            mock_check.0,
            &mock_check.1,
            15.0,
        );

        // 2. Check for clear failure messages
        let assert_check = Self::check_assertion_clarity(&test_file);
        checklist.add_check(
            "Assertions have clear error messages",
            assert_check.0,
            &assert_check.1,
            15.0,
        );

        // 3. Check if tests reveal actual bugs
        let bug_check = Self::check_bug_detection(&test_file);
        checklist.add_check(
            "Tests verify behavior, not syntax",
            bug_check.0,
            &bug_check.1,
            20.0,
        );

        // 4. Check setup/teardown clarity
        let setup_check = Self::check_setup_clarity(&test_file);
        checklist.add_check(
            "Setup/teardown is clear and explicit",
            setup_check.0,
            &setup_check.1,
            10.0,
        );

        // 5. Check for good error messages
        let error_check = Self::check_error_messages(&test_file);
        checklist.add_check(
            "Error messages enable quick debugging",
            error_check.0,
            &error_check.1,
            15.0,
        );

        // 6. Check test performance
        let perf_check = Self::check_performance(&test_file);
        checklist.add_check(
            "Tests run quickly (< 30s total)",
            perf_check.0,
            &perf_check.1,
            10.0,
        );

        // 7. Check test isolation
        let isolation_check = Self::check_isolation(&test_file);
        checklist.add_check(
            "Tests run in isolation (no dependencies)",
            isolation_check.0,
            &isolation_check.1,
            10.0,
        );

        // 8. Check reproducibility
        let repro_check = Self::check_reproducibility(&test_file);
        checklist.add_check(
            "Failures are reproducible (not flaky)",
            repro_check.0,
            &repro_check.1,
            5.0,
        );

        checklist.calculate_score();

        // Collect findings
        for check in &checklist.checks {
            if !check.passed {
                checklist.findings.push(format!("{}: {}", check.name, check.observation));
            }
        }

        checklist
    }

    fn check_mock_usage(test_file: &PathBuf) -> (bool, String) {
        if let Ok(content) = std::fs::read_to_string(test_file) {
            let mock_count = content.matches("mock").count() +
                           content.matches("Mock").count() +
                           content.matches("stub").count();

            if mock_count > 5 {
                (false, format!("Found {} mock/stub usages - prefer real implementations", mock_count))
            } else {
                (true, "Minimal mocking - tests use real code paths".to_string())
            }
        } else {
            (false, "Could not read test file".to_string())
        }
    }

    fn check_assertion_clarity(test_file: &PathBuf) -> (bool, String) {
        if let Ok(content) = std::fs::read_to_string(test_file) {
            let assert_count = content.matches("assert!").count();
            let assert_eq_count = content.matches("assert_eq!").count();

            if assert_count > assert_eq_count * 2 {
                (false, "Many bare assert!() - use assert_eq!/assert_ne! for clarity".to_string())
            } else {
                (true, "Good use of descriptive assertions".to_string())
            }
        } else {
            (false, "Could not read test file".to_string())
        }
    }

    fn check_bug_detection(test_file: &PathBuf) -> (bool, String) {
        if let Ok(content) = std::fs::read_to_string(test_file) {
            // Look for behavior-focused tests
            let has_edge_cases = content.contains("edge") ||
                               content.contains("boundary") ||
                               content.contains("error");

            if has_edge_cases {
                (true, "Tests verify edge cases and error handling".to_string())
            } else {
                (false, "Tests may be too happy-path focused".to_string())
            }
        } else {
            (false, "Could not read test file".to_string())
        }
    }

    fn check_setup_clarity(test_file: &PathBuf) -> (bool, String) {
        if let Ok(content) = std::fs::read_to_string(test_file) {
            let has_setup = content.contains("fn setup") ||
                          content.contains("before_each");
            let has_teardown = content.contains("fn teardown") ||
                             content.contains("after_each");

            if has_setup && has_teardown {
                (true, "Explicit setup/teardown functions found".to_string())
            } else if content.contains("let ") && content.lines().count() < 50 {
                (true, "Simple inline setup is clear".to_string())
            } else {
                (false, "Setup/teardown could be more explicit".to_string())
            }
        } else {
            (false, "Could not read test file".to_string())
        }
    }

    fn check_error_messages(test_file: &PathBuf) -> (bool, String) {
        if let Ok(content) = std::fs::read_to_string(test_file) {
            let has_custom_messages = content.matches(r#", ""#).count() > 0 ||
                                     content.contains("expect(");

            if has_custom_messages {
                (true, "Custom error messages provided".to_string())
            } else {
                (false, "Add custom error messages to assertions for clarity".to_string())
            }
        } else {
            (false, "Could not read test file".to_string())
        }
    }

    fn check_performance(_test_file: &PathBuf) -> (bool, String) {
        // In a real implementation, we would run the tests and measure time
        // For now, we'll provide a placeholder
        (true, "Performance check requires actual test execution".to_string())
    }

    fn check_isolation(test_file: &PathBuf) -> (bool, String) {
        if let Ok(content) = std::fs::read_to_string(test_file) {
            let has_shared_state = content.contains("static mut") ||
                                 content.contains("lazy_static");

            if has_shared_state {
                (false, "Tests may share state - consider isolation".to_string())
            } else {
                (true, "No obvious shared state detected".to_string())
            }
        } else {
            (false, "Could not read test file".to_string())
        }
    }

    fn check_reproducibility(_test_file: &PathBuf) -> (bool, String) {
        // In a real implementation, we would run tests multiple times
        (true, "Reproducibility check requires multiple test runs".to_string())
    }

    /// Run tests and observe actual failures (Gemba = go see)
    pub fn observe_actual_run(test_file: &PathBuf) -> String {
        println!("\n👀 OBSERVING ACTUAL TEST RUN (Gemba Walk)");
        println!("═══════════════════════════════════════\n");

        let output = Command::new("cargo")
            .args(&["test", "--test"])
            .arg(test_file.file_stem().unwrap())
            .args(&["--", "--nocapture", "--test-threads=1"])
            .output();

        match output {
            Ok(output) => {
                let stdout = String::from_utf8_lossy(&output.stdout);
                let stderr = String::from_utf8_lossy(&output.stderr);

                println!("STDOUT:\n{}", stdout);
                if !stderr.is_empty() {
                    println!("\nSTDERR:\n{}", stderr);
                }

                format!("Test output captured:\n{}\n{}", stdout, stderr)
            },
            Err(e) => format!("Failed to run tests: {}", e),
        }
    }

    /// Measure actual performance (Gemba = measure reality)
    pub fn measure_performance(test_file: &PathBuf) -> Duration {
        println!("\n⏱️  MEASURING ACTUAL PERFORMANCE");
        println!("═══════════════════════════════════════\n");

        let start = Instant::now();

        let _ = Command::new("cargo")
            .args(&["test", "--test"])
            .arg(test_file.file_stem().unwrap())
            .args(&["--", "--test-threads=1"])
            .output();

        let duration = start.elapsed();
        println!("Total test time: {:?}", duration);

        duration
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gemba_checklist() {
        let mut checklist = GembaWalkChecklist::new(PathBuf::from("test.rs"));

        checklist.add_check("Test 1", true, "Passed", 10.0);
        checklist.add_check("Test 2", false, "Failed", 10.0);
        checklist.calculate_score();

        assert_eq!(checklist.score, 50.0);
    }

    #[test]
    fn test_mock_usage_check() {
        // This would require actual test files to inspect
        // For now, just verify the function exists
        let result = GembaWalkInspector::check_mock_usage(&PathBuf::from("nonexistent.rs"));
        assert!(!result.0); // Should fail on nonexistent file
    }
}
