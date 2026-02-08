use super::checks::Check;
#[cfg(test)]
use super::checks::CheckError;
use crate::signals::AndonSignal;
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

    struct MockCheck {
        name: String,
        should_pass: bool,
    }

    impl Check for MockCheck {
        fn name(&self) -> &str {
            &self.name
        }

        fn description(&self) -> &str {
            "Mock check"
        }

        fn run(&self) -> Result<(), CheckError> {
            if self.should_pass {
                Ok(())
            } else {
                Err(CheckError::Unknown("mock failure".into()))
            }
        }
    }

    #[test]
    fn test_all_checks_pass() {
        let gate = QualityGate::new()
            .add_check(Box::new(MockCheck {
                name: "check1".into(),
                should_pass: true,
            }))
            .add_check(Box::new(MockCheck {
                name: "check2".into(),
                should_pass: true,
            }));

        let result = gate.verify();
        assert!(result.all_passed);
        assert_eq!(result.signal, AndonSignal::Green);
        assert_eq!(result.passed_count(), 2);
    }

    #[test]
    fn test_check_failure() {
        let gate = QualityGate::new()
            .add_check(Box::new(MockCheck {
                name: "check1".into(),
                should_pass: true,
            }))
            .add_check(Box::new(MockCheck {
                name: "check2".into(),
                should_pass: false,
            }));

        let result = gate.verify();
        assert!(!result.all_passed);
        assert_eq!(result.signal, AndonSignal::Red);
        assert_eq!(result.failed_count(), 1);
        assert!(result.first_failure().is_some());
    }
}
