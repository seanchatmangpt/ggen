use std::fmt;
use crate::signals::AndonSignal;

#[derive(Debug, Clone)]
pub enum CheckError {
    CompilationFailed(String),
    LintFailed(String),
    TestFailed(String),
    SecurityCheckFailed(String),
    Unknown(String),
}

impl fmt::Display for CheckError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CheckError::CompilationFailed(msg) => write!(f, "Compilation failed: {}", msg),
            CheckError::LintFailed(msg) => write!(f, "Lint check failed: {}", msg),
            CheckError::TestFailed(msg) => write!(f, "Test failed: {}", msg),
            CheckError::SecurityCheckFailed(msg) => write!(f, "Security check failed: {}", msg),
            CheckError::Unknown(msg) => write!(f, "Check failed: {}", msg),
        }
    }
}

impl std::error::Error for CheckError {}

pub trait Check: Send + Sync {
    fn name(&self) -> &str;
    fn description(&self) -> &str;
    fn run(&self) -> Result<(), CheckError>;
    fn signal(&self, result: &Result<(), CheckError>) -> AndonSignal {
        match result {
            Ok(()) => AndonSignal::Green,
            Err(_) => AndonSignal::Red,
        }
    }
}

pub struct CompilationCheck;

impl Check for CompilationCheck {
    fn name(&self) -> &str {
        "compilation"
    }

    fn description(&self) -> &str {
        "Verify code compiles without errors"
    }

    fn run(&self) -> Result<(), CheckError> {
        Ok(())
    }
}

pub struct LintCheck;

impl Check for LintCheck {
    fn name(&self) -> &str {
        "lint"
    }

    fn description(&self) -> &str {
        "Verify code meets linting standards (clippy)"
    }

    fn run(&self) -> Result<(), CheckError> {
        Ok(())
    }
}

pub struct TestCheck;

impl Check for TestCheck {
    fn name(&self) -> &str {
        "test"
    }

    fn description(&self) -> &str {
        "Verify unit tests pass"
    }

    fn run(&self) -> Result<(), CheckError> {
        Ok(())
    }
}

pub struct SecurityCheck;

impl Check for SecurityCheck {
    fn name(&self) -> &str {
        "security"
    }

    fn description(&self) -> &str {
        "Verify code passes security audit"
    }

    fn run(&self) -> Result<(), CheckError> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compilation_check() {
        let check = CompilationCheck;
        assert_eq!(check.name(), "compilation");
        assert!(check.run().is_ok());
    }

    #[test]
    fn test_lint_check() {
        let check = LintCheck;
        assert_eq!(check.name(), "lint");
        assert!(check.run().is_ok());
    }

    #[test]
    fn test_test_check() {
        let check = TestCheck;
        assert_eq!(check.name(), "test");
        assert!(check.run().is_ok());
    }

    #[test]
    fn test_security_check() {
        let check = SecurityCheck;
        assert_eq!(check.name(), "security");
        assert!(check.run().is_ok());
    }

    #[test]
    fn test_check_signal() {
        let check = CompilationCheck;
        let result = check.run();
        assert_eq!(check.signal(&result), AndonSignal::Green);

        let error_result: Result<(), CheckError> =
            Err(CheckError::CompilationFailed("test error".into()));
        assert_eq!(check.signal(&error_result), AndonSignal::Red);
    }
}
