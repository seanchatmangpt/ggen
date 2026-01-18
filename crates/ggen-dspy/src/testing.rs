//! Test utilities and fixtures for ggen-dspy

use crate::{Module, ModuleOutput, Result};

/// Mock module for testing
pub struct MockModule {
    name: String,
    output: ModuleOutput,
}

impl MockModule {
    /// Create a new mock module
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            output: ModuleOutput::new(),
        }
    }

    /// Set output for testing
    pub fn with_output(mut self, output: ModuleOutput) -> Self {
        self.output = output;
        self
    }
}

#[async_trait::async_trait]
impl Module for MockModule {
    async fn forward(&self, _inputs: &[(&str, &str)]) -> Result<ModuleOutput> {
        Ok(self.output.clone())
    }

    fn name(&self) -> &str {
        &self.name
    }
}

/// Create a test module output
pub fn test_output(fields: &[(&str, &str)]) -> ModuleOutput {
    let mut output = ModuleOutput::new();
    for (key, value) in fields {
        output.set(*key, *value);
    }
    output
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_mock_module() {
        let output = test_output(&[("answer", "42")]);
        let module = MockModule::new("test").with_output(output);

        let result = module.forward(&[]).await;
        assert!(result.is_ok());

        let output = result.unwrap();
        assert_eq!(output.get("answer").ok(), Some("42"));
    }
}
