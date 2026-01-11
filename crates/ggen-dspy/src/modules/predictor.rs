//! Predictor module - basic LLM call with signature
//!
//! The fundamental building block for DSPy-style programming.

use crate::{Module, ModuleOutput, Result};
use ggen_ai::{InputField, OutputField, Signature};

/// Basic predictor module that executes an LLM call
pub struct Predictor {
    signature: Signature,
    name: String,
}

impl Predictor {
    /// Create a new predictor
    pub fn new(signature: Signature, name: impl Into<String>) -> Self {
        Self {
            signature,
            name: name.into(),
        }
    }

    /// Create a predictor with default name
    pub fn with_signature(signature: Signature) -> Self {
        Self::new(signature, "Predictor")
    }
}

#[async_trait::async_trait]
impl Module for Predictor {
    async fn forward(&self, _inputs: &[(&str, &str)]) -> Result<ModuleOutput> {
        // TODO: Implement actual LLM call through ggen-ai adapter
        // For now, return placeholder
        Ok(ModuleOutput::new())
    }

    fn name(&self) -> &str {
        &self.name
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_predictor_creation() {
        let sig = Signature::builder()
            .input(InputField::new("question", "A question"))
            .output(OutputField::new("answer", "The answer"))
            .build()
            .expect("Failed to build signature");

        let predictor = Predictor::with_signature(sig);
        assert_eq!(predictor.name(), "Predictor");
    }
}
