//! DSPy Predictor - LLM-backed prediction with signature interface

use crate::dspy::signature::Signature;
use crate::dspy::module::{Module, ModuleError, ModuleResult};
use serde_json::Value;
use std::collections::HashMap;

/// Predictor: LLM-backed prediction with signature interface
///
/// A concrete implementation of Module that uses an LLM to generate outputs
/// based on inputs defined by the signature.
pub struct Predictor {
    signature: Signature,
    llm_provider: String,
    temperature: f32,
}

impl Predictor {
    /// Create a new predictor with a signature
    pub fn new(signature: Signature) -> Self {
        Self {
            signature,
            llm_provider: "openai".to_string(),
            temperature: 0.7,
        }
    }

    /// Set the LLM provider
    pub fn with_provider(mut self, provider: impl Into<String>) -> Self {
        self.llm_provider = provider.into();
        self
    }

    /// Set the temperature (controls randomness)
    pub fn with_temperature(mut self, temperature: f32) -> Self {
        self.temperature = temperature.clamp(0.0, 1.0);
        self
    }

    /// Get the LLM provider
    pub fn provider(&self) -> &str {
        &self.llm_provider
    }

    /// Get the temperature
    pub fn temperature(&self) -> f32 {
        self.temperature
    }

    /// Build a prompt from inputs
    fn build_prompt(&self, inputs: &HashMap<String, Value>) -> Result<String, ModuleError> {
        let mut prompt = format!("{}\n\n", self.signature.description);

        if let Some(instructions) = &self.signature.instructions {
            prompt.push_str(&format!("Instructions: {}\n\n", instructions));
        }

        // Add input fields
        prompt.push_str("Input:\n");
        for input_field in &self.signature.inputs {
            if let Some(value) = inputs.get(input_field.name()) {
                let prefix = input_field.metadata.prefix.as_deref().unwrap_or("");
                prompt.push_str(&format!("{}{}: {}\n", prefix, input_field.name(), value));
            }
        }

        prompt.push_str("\nOutput:\n");
        for output_field in &self.signature.outputs {
            prompt.push_str(&format!("{}: ", output_field.name()));
        }

        Ok(prompt)
    }

    /// Parse output from LLM response
    fn parse_output(&self, response: &str) -> Result<HashMap<String, Value>, ModuleError> {
        let mut outputs = HashMap::new();

        // Simple parsing: extract output field values from response
        for output_field in &self.signature.outputs {
            // Try to extract field value from response
            if let Some(value) = self.extract_field_value(response, output_field.name()) {
                outputs.insert(output_field.name().to_string(), Value::String(value));
            }
        }

        if outputs.is_empty() {
            // If no structured output found, return entire response
            outputs.insert(
                self.signature.outputs.first()
                    .map(|f| f.name().to_string())
                    .unwrap_or_else(|| "output".to_string()),
                Value::String(response.to_string()),
            );
        }

        Ok(outputs)
    }

    /// Extract field value from response text
    fn extract_field_value(&self, response: &str, field_name: &str) -> Option<String> {
        let pattern = format!("{}: ", field_name);
        if let Some(start) = response.find(&pattern) {
            let text = &response[start + pattern.len()..];
            // Take until newline or end of string
            let value = text.lines().next().unwrap_or("").trim();
            if !value.is_empty() {
                return Some(value.to_string());
            }
        }
        None
    }
}

#[async_trait::async_trait]
impl Module for Predictor {
    fn signature(&self) -> &Signature {
        &self.signature
    }

    async fn forward(&self, inputs: HashMap<String, Value>) -> ModuleResult<HashMap<String, Value>> {
        // Validate inputs
        self.validate_inputs(&inputs)?;

        // Build prompt
        let _prompt = self.build_prompt(&inputs)?;

        // TODO: Call actual LLM
        // For now, return mock response
        let response = format!(
            "Mock response from {} with temperature {}",
            self.llm_provider, self.temperature
        );

        // Parse output
        self.parse_output(&response)
    }
}

/// ChainOfThought: Predictor with reasoning steps
pub struct ChainOfThought {
    predictor: Predictor,
}

impl ChainOfThought {
    /// Create a new chain-of-thought predictor
    pub fn new(signature: Signature) -> Self {
        let mut sig = signature;
        sig.instructions = Some(
            "Think through this step-by-step before providing your answer.".to_string()
        );

        Self {
            predictor: Predictor::new(sig),
        }
    }

    /// Get the underlying predictor
    pub fn predictor(&self) -> &Predictor {
        &self.predictor
    }

    /// Get mutable reference to predictor
    pub fn predictor_mut(&mut self) -> &mut Predictor {
        &mut self.predictor
    }
}

#[async_trait::async_trait]
impl Module for ChainOfThought {
    fn signature(&self) -> &Signature {
        self.predictor.signature()
    }

    async fn forward(&self, inputs: HashMap<String, Value>) -> ModuleResult<HashMap<String, Value>> {
        self.predictor.forward(inputs).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dspy::field::{InputField, OutputField};

    fn create_test_signature() -> Signature {
        Signature::new("QA", "Question answering")
            .with_input(InputField::new("question", "The question", "String"))
            .with_output(OutputField::new("answer", "The answer", "String"))
    }

    #[test]
    fn test_predictor_creation() {
        let sig = create_test_signature();
        let pred = Predictor::new(sig);
        assert_eq!(pred.provider(), "openai");
        assert_eq!(pred.temperature(), 0.7);
    }

    #[test]
    fn test_temperature_clamping() {
        let sig = create_test_signature();
        let pred = Predictor::new(sig)
            .with_temperature(2.0);
        assert_eq!(pred.temperature(), 1.0);
    }

    #[test]
    fn test_prompt_building() {
        let sig = create_test_signature();
        let pred = Predictor::new(sig);

        let mut inputs = HashMap::new();
        inputs.insert("question".to_string(), Value::String("What is Rust?".to_string()));

        let prompt = pred.build_prompt(&inputs).unwrap();
        // Value::String formats with quotes, so check for the JSON representation
        assert!(prompt.contains("question:") && prompt.contains("What is Rust?"));
        assert!(prompt.contains("answer:"));
    }

    #[test]
    fn test_chain_of_thought() {
        let sig = create_test_signature();
        let cot = ChainOfThought::new(sig);
        assert!(cot.signature().instructions.is_some());
        assert!(cot.signature().instructions.as_ref().unwrap().contains("step-by-step"));
    }
}
