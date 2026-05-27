use super::checks::Check;
#[cfg(test)]
use super::checks::CheckError;
use crate::validation::AndonSignal;
use std::fmt;

#[derive(Debug, Clone)]
pub struct CheckResult {
    pub check_name: String,
    pub passed: bool,
    pub error: Option<String>,
    pub signal: AndonSignal,
}

impl CheckResult {
    pub fn passed(check_name: impl Into<String>) -> Self {
        Self {
            check_name: check_name.into(),
            passed: true,
            error: None,
            signal: AndonSignal::Green,
        }
    }

    pub fn failed(check_name: impl Into<String>, error: impl Into<String>) -> Self {
        Self {
            check_name: check_name.into(),
            passed: false,
            error: Some(error.into()),
            signal: AndonSignal::Red,
        }
    }
}

pub struct QualityGate {
    checks: Vec<Box<dyn Check>>,
}

impl QualityGate {
    pub fn new() -> Self {
        Self { checks: Vec::new() }
    }

    pub fn add_check(mut self, check: Box<dyn Check>) -> Self {
        self.checks.push(check);
        self
    }

    pub fn verify(&self) -> QualityGateResult {
        let mut results = Vec::new();
        let mut all_passed = true;

        for check in &self.checks {
            let result = check.run();
            let signal = check.signal(&result);

            let check_result = match result {
                Ok(()) => CheckResult::passed(check.name()),
                Err(e) => {
                    all_passed = false;
                    CheckResult::failed(check.name(), e.to_string())
                }
            };

            results.push(check_result);

            if signal.is_error() {
                break;
            }
        }

        QualityGateResult {
            results,
            all_passed,
            signal: if all_passed {
                AndonSignal::Green
            } else {
                AndonSignal::Red
            },
        }
    }

    pub fn checks_count(&self) -> usize {
        self.checks.len()
    }
}

impl Default for QualityGate {
    fn default() -> Self {
        Self::new()
    }
}

pub struct QualityGateResult {
    pub results: Vec<CheckResult>,
    pub all_passed: bool,
    pub signal: AndonSignal,
}

impl QualityGateResult {
    pub fn passed_count(&self) -> usize {
        self.results.iter().filter(|r| r.passed).count()
    }

    pub fn failed_count(&self) -> usize {
        self.results.iter().filter(|r| !r.passed).count()
    }

    pub fn first_failure(&self) -> Option<&CheckResult> {
        self.results.iter().find(|r| !r.passed)
    }
}

impl fmt::Display for QualityGateResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{} Quality Gate Report", self.signal.emoji())?;
        writeln!(f, "Passed: {}/{}", self.passed_count(), self.results.len())?;

        for result in &self.results {
            let status = if result.passed { "✓" } else { "✗" };
            write!(f, "  {} {}", status, result.check_name)?;
            if let Some(error) = &result.error {
                write!(f, ": {}", error)?;
            }
            writeln!(f)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Chicago TDD: Real checks that verify actual observable behavior
    struct FileExistsCheck {
        file_path: std::path::PathBuf,
    }

    impl Check for FileExistsCheck {
        fn name(&self) -> &str {
            "file_exists"
        }

        fn description(&self) -> &str {
            "Verify required file exists"
        }

        fn run(&self) -> Result<(), CheckError> {
            if self.file_path.exists() {
                Ok(())
            } else {
                Err(CheckError::Unknown(format!(
                    "File not found: {}",
                    self.file_path.display()
                )))
            }
        }
    }

    struct FileContentCheck {
        file_path: std::path::PathBuf,
        expected_content: String,
    }

    impl Check for FileContentCheck {
        fn name(&self) -> &str {
            "file_content"
        }

        fn description(&self) -> &str {
            "Verify file contains expected content"
        }

        fn run(&self) -> Result<(), CheckError> {
            if !self.file_path.exists() {
                return Err(CheckError::Unknown(format!(
                    "File not found: {}",
                    self.file_path.display()
                )));
            }

            match std::fs::read_to_string(&self.file_path) {
                Ok(content) => {
                    if content.contains(&self.expected_content) {
                        Ok(())
                    } else {
                        Err(CheckError::Unknown(
                            "Expected content not found in file".into(),
                        ))
                    }
                }
                Err(e) => Err(CheckError::Unknown(format!("Failed to read file: {}", e))),
            }
        }
    }

    #[test]
    fn test_all_checks_pass() {
        // Chicago TDD: Create real files and verify gates work with real checks
        use tempfile::TempDir;
        use std::fs;

        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let test_file = temp_dir.path().join("test.txt");

        // Create real file that will pass the check
        fs::write(&test_file, "test content").expect("Failed to write file");

        let gate = QualityGate::new()
            .add_check(Box::new(FileExistsCheck {
                file_path: test_file.clone(),
            }))
            .add_check(Box::new(FileContentCheck {
                file_path: test_file.clone(),
                expected_content: "test".to_string(),
            }));

        let result = gate.verify();
        assert!(result.all_passed, "All checks should pass");
        assert_eq!(result.signal, AndonSignal::Green);
        assert_eq!(result.passed_count(), 2);
    }

    #[test]
    fn test_check_failure() {
        // Chicago TDD: Verify gates detect real check failures
        use tempfile::TempDir;

        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let missing_file = temp_dir.path().join("nonexistent.txt");
        let existing_file = temp_dir.path().join("exists.txt");

        // Create one file but not the other
        std::fs::write(&existing_file, "content").expect("Failed to write file");

        let gate = QualityGate::new()
            .add_check(Box::new(FileExistsCheck {
                file_path: existing_file,
            }))
            .add_check(Box::new(FileExistsCheck {
                file_path: missing_file,
            }));

        let result = gate.verify();
        assert!(!result.all_passed, "Should fail when a check fails");
        assert_eq!(result.signal, AndonSignal::Red);
        assert_eq!(result.failed_count(), 1);
        assert!(result.first_failure().is_some(), "Should report first failure");
    }

    #[test]
    fn test_gate_with_content_validation() {
        // Chicago TDD: Verify file content checks work correctly
        use tempfile::TempDir;
        use std::fs;

        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let target_file = temp_dir.path().join("output.txt");

        // Write file with expected content
        fs::write(&target_file, "fn main() { println!(\"hello\"); }")
            .expect("Failed to write file");

        let gate = QualityGate::new().add_check(Box::new(FileContentCheck {
            file_path: target_file,
            expected_content: "fn main()".to_string(),
        }));

        let result = gate.verify();
        assert!(result.all_passed, "Content check should pass");
        assert_eq!(result.signal, AndonSignal::Green);
    }
}
