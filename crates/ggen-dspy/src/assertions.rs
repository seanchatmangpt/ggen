//! Runtime assertions and constraints for LLM outputs

use crate::Result;
use thiserror::Error;

/// Assertion error
#[derive(Error, Debug)]
#[error("Assertion failed: {message}")]
pub struct AssertionError {
    /// Error message
    pub message: String,
}

impl AssertionError {
    /// Create a new assertion error
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

/// Assert trait for runtime validation
pub trait Assert {
    /// Assert a condition on the output
    fn assert(&self, value: &str) -> Result<()>;

    /// Get assertion description
    fn description(&self) -> &str;
}

/// Suggest trait for retry hints
pub trait Suggest {
    /// Suggest a fix for failed assertion
    fn suggest(&self, value: &str) -> String;
}

/// Length constraint assertion
pub struct LengthAssertion {
    min: Option<usize>,
    max: Option<usize>,
}

impl LengthAssertion {
    /// Create a new length assertion
    pub fn new(min: Option<usize>, max: Option<usize>) -> Self {
        Self { min, max }
    }

    /// Create min-only assertion
    pub fn min(min: usize) -> Self {
        Self::new(Some(min), None)
    }

    /// Create max-only assertion
    pub fn max(max: usize) -> Self {
        Self::new(None, Some(max))
    }

    /// Create range assertion
    pub fn range(min: usize, max: usize) -> Self {
        Self::new(Some(min), Some(max))
    }
}

impl Assert for LengthAssertion {
    fn assert(&self, value: &str) -> Result<()> {
        let len = value.len();

        if let Some(min) = self.min {
            if len < min {
                return Err(crate::DspyError::assertion(format!(
                    "Length {} is less than minimum {}",
                    len, min
                )));
            }
        }

        if let Some(max) = self.max {
            if len > max {
                return Err(crate::DspyError::assertion(format!(
                    "Length {} exceeds maximum {}",
                    len, max
                )));
            }
        }

        Ok(())
    }

    fn description(&self) -> &str {
        "Length constraint"
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_length_assertion() {
        let assertion = LengthAssertion::range(5, 10);
        assert!(assertion.assert("hello").is_ok());
        assert!(assertion.assert("hi").is_err());
        assert!(assertion.assert("this is too long").is_err());
    }

    #[test]
    fn test_length_assertion_min() {
        let assertion = LengthAssertion::min(5);
        assert!(assertion.assert("hello").is_ok());
        assert!(assertion.assert("hi").is_err());
    }
}
