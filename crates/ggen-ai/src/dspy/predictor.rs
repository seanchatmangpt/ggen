//! DSPy Predictor - LLM-backed prediction with signature interface

use crate::dspy::signature::Signature;
use crate::dspy::module::{Module, ModuleError, ModuleResult};
use serde_json::Value;
use std::collections::HashMap;
use genai::{Client, chat::{ChatMessage, ChatOptions, ChatRequest}};
use tracing::{debug, warn};

/// Predictor: LLM-backed prediction with signature interface
///
/// A concrete implementation of Module that uses an LLM to generate outputs
/// based on inputs defined by the signature.
pub struct Predictor {
    signature: Signature,
    model: String,  // Model name (from ggen.toml or CLI)
    temperature: f32,
}

impl Predictor {
    /// Create a new predictor with a signature and model name
    pub fn new(signature: Signature) -> Self {
        // Get model from environment or use empty string (will error if not set)
        let model = std::env::var("GGEN_LLM_MODEL")
            .or_else(|_| std::env::var("DEFAULT_MODEL"))
            .unwrap_or_else(|_| "".to_string());

        Self {
            signature,
            model,
            temperature: 0.7,
        }
    }

    /// Create a predictor with an explicit model
    pub fn with_model(signature: Signature, model: impl Into<String>) -> Self {
        Self {
            signature,
            model: model.into(),
            temperature: 0.7,
        }
    }

    /// Set the model name
    pub fn model(mut self, model: impl Into<String>) -> Self {
        self.model = model.into();
        self
    }

    /// Set the temperature (controls randomness, 0.0-2.0)
    pub fn with_temperature(mut self, temperature: f32) -> Self {
        self.temperature = temperature.clamp(0.0, 2.0);
        self
    }

    /// Get the model name
    pub fn model_name(&self) -> &str {
        &self.model
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

    /// Call the LLM with the given prompt using genai
    async fn call_llm(&self, prompt: &str) -> Result<String, ModuleError> {
        let client = Client::default();

        if self.model.is_empty() {
            return Err(ModuleError::LlmError(
                "Model name not set. Set GGEN_LLM_MODEL or DEFAULT_MODEL env var, or use .model()".to_string()
            ));
        }

        debug!("Calling LLM model: {}", self.model);

        // Create chat request with system prompt and user input
        let chat_req = ChatRequest::new(vec![
            ChatMessage::system("You are a helpful assistant. Provide responses in the exact format requested."),
            ChatMessage::user(prompt.to_string()),
        ]);

        // Set chat options
        let chat_options = ChatOptions::default()
            .with_temperature(self.temperature as f64)
            .with_max_tokens(4096);

        // Execute the request
        match client.exec_chat(&self.model, chat_req, Some(&chat_options)).await {
            Ok(response) => {
                let content = response
                    .first_text()
                    .unwrap_or_default()
                    .to_string();
                debug!("LLM response received, length: {}", content.len());
                Ok(content)
            }
            Err(e) => {
                warn!("LLM call failed: {}", e);
                Err(ModuleError::LlmError(format!("LLM call failed: {}", e)))
            }
        }
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
        let prompt = self.build_prompt(&inputs)?;
        debug!("Built prompt for {} with model {}", self.signature.name, self.model);

        // Call actual LLM
        let response = self.call_llm(&prompt).await?;
        debug!("Received LLM response: {}", response);

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
        let pred = Predictor::with_model(sig, "test-model");
        assert_eq!(pred.model_name(), "test-model");
        assert_eq!(pred.temperature(), 0.7);
    }

    #[test]
    fn test_temperature_clamping() {
        let sig = create_test_signature();
        let pred = Predictor::with_model(sig, "test-model")
            .with_temperature(3.0);
        assert_eq!(pred.temperature(), 2.0);
    }

    #[test]
    fn test_model_setter() {
        let sig = create_test_signature();
        let pred = Predictor::new(sig).model("my-model");
        assert_eq!(pred.model_name(), "my-model");
    }

    #[test]
    fn test_prompt_building() {
        let sig = create_test_signature();
        let pred = Predictor::with_model(sig, "test-model");

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
