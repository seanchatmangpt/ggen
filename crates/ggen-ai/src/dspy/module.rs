//! DSPy Module - Base abstraction for composable modules

use crate::dspy::signature::Signature;
use serde_json::Value;
use std::collections::HashMap;

/// Module: A composable unit of computation
///
/// Trait that defines how a module processes inputs to produce outputs.
/// Analogous to DSPy's ChainOfThought, etc., but trait-based for composability.
#[async_trait::async_trait]
pub trait Module: Send + Sync {
    /// Get the module's signature (interface definition)
    fn signature(&self) -> &Signature;

    /// Process inputs and produce outputs
    ///
    /// # Arguments
    /// * `inputs` - HashMap of input field names to values
    ///
    /// # Returns
    /// HashMap of output field names to values
    async fn forward(
        &self, inputs: HashMap<String, Value>,
    ) -> Result<HashMap<String, Value>, ModuleError>;

    /// Get module name
    fn name(&self) -> &str {
        &self.signature().name
    }

    /// Validate that all required inputs are present
    fn validate_inputs(&self, inputs: &HashMap<String, Value>) -> Result<(), ModuleError> {
        for input_field in &self.signature().inputs {
            if !inputs.contains_key(input_field.name()) {
                return Err(ModuleError::MissingInput(input_field.name().to_string()));
            }
        }
        Ok(())
    }

    /// Downcast to concrete type (for optimizer introspection)
    fn as_any(&self) -> &dyn std::any::Any;
}

/// Error type for module operations
#[derive(Debug, thiserror::Error)]
pub enum ModuleError {
    #[error("Missing required input: {0}")]
    MissingInput(String),

    #[error("Invalid input type for field '{0}': expected {1}")]
    InvalidInputType(String, String),

    #[error("LLM error: {0}")]
    LlmError(String),

    #[error("Module error: {0}")]
    Other(String),
}

pub type ModuleResult<T> = Result<T, ModuleError>;

/// Builder for creating modules with a signature
pub struct ModuleBuilder {
    signature: Signature,
}

impl ModuleBuilder {
    /// Create a new module builder
    pub fn new(name: impl Into<String>, description: impl Into<String>) -> Self {
        Self {
            signature: Signature::new(name, description),
        }
    }

    /// Build the signature
    pub fn build_signature(self) -> Signature {
        self.signature
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dspy::field::InputField;

    struct SimpleModule {
        sig: Signature,
    }

    #[async_trait::async_trait]
    impl Module for SimpleModule {
        fn signature(&self) -> &Signature {
            &self.sig
        }

        async fn forward(
            &self, _inputs: HashMap<String, Value>,
        ) -> Result<HashMap<String, Value>, ModuleError> {
            Ok(HashMap::new())
        }

        fn as_any(&self) -> &dyn std::any::Any {
            self
        }
    }

    #[test]
    fn test_module_validation() {
        let sig = Signature::new("Test", "Test")
            .with_input(InputField::new("input", "An input", "String"));

        let module = SimpleModule { sig };

        let mut inputs = HashMap::new();
        assert!(module.validate_inputs(&inputs).is_err());

        inputs.insert("input".to_string(), Value::String("test".to_string()));
        assert!(module.validate_inputs(&inputs).is_ok());
    }
}
