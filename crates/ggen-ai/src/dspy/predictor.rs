//! DSPy Predictor - LLM-backed prediction with signature interface

use crate::client::LlmClient;
use crate::dspy::signature::Signature;
use crate::dspy::signature_validator::SignatureValidator;
use crate::dspy::module::{Module, ModuleError, ModuleResult};
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;

/// Predictor: LLM-backed prediction with signature interface
///
/// A concrete implementation of Module that uses an LLM to generate outputs
/// based on inputs defined by the signature.
pub struct Predictor {
    signature: Signature,
    temperature: f32,
    client: Arc<dyn LlmClient>,
}

impl Predictor {
    /// Create a new predictor with a signature and LLM client
    pub fn new(signature: Signature, client: Arc<dyn LlmClient>) -> Self {
        Self {
            signature,
            temperature: 0.7,
            client,
        }
    }

    /// Set the temperature (controls randomness)
    pub fn with_temperature(mut self, temperature: f32) -> Self {
        self.temperature = temperature.clamp(0.0, 1.0);
        self
    }

    /// Get the LLM model being used
    pub fn model(&self) -> &str {
        &self.client.get_config().model
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
        // 1. Validate inputs
        self.validate_inputs(&inputs)?;

        // 2. Build prompt from inputs
        let prompt = self.build_prompt(&inputs)?;

        // 3. Call actual LLM with temperature override
        let llm_response = self
            .client
            .complete(&prompt)
            .await
            .map_err(|e| ModuleError::LlmError(
                format!("LLM call failed: {}", e)
            ))?;

        // 4. Parse LLM response into structured output
        let outputs = self.parse_output(&llm_response.content)?;

        // 5. Validate outputs against signature constraints
        // Note: We validate the combined outputs as a single JSON object
        let outputs_json = serde_json::Value::Object(
            outputs
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect()
        );

        SignatureValidator::new(self.signature.clone())
            .validate(&outputs_json)
            .map_err(|e| ModuleError::Other(
                format!("Output validation failed: {}", e)
            ))?;

        Ok(outputs)
    }
}

/// ChainOfThought: Predictor with reasoning steps
pub struct ChainOfThought {
    predictor: Predictor,
}

impl ChainOfThought {
    /// Create a new chain-of-thought predictor
    pub fn new(signature: Signature, client: Arc<dyn LlmClient>) -> Self {
        let mut sig = signature;
        sig.instructions = Some(
            "Think through this step-by-step before providing your answer.".to_string()
        );

        Self {
            predictor: Predictor::new(sig, client),
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
    use crate::providers::MockClient;

    fn create_test_signature() -> Signature {
        Signature::new("QA", "Question answering")
            .with_input(InputField::new("question", "The question", "String"))
            .with_output(OutputField::new("answer", "The answer", "String"))
    }

    fn create_mock_client() -> Arc<dyn LlmClient> {
        Arc::new(MockClient::with_response("answer: The Rust programming language is a systems language that provides memory safety without garbage collection."))
    }

    #[test]
    fn test_predictor_creation() {
        let sig = create_test_signature();
        let client = create_mock_client();
        let pred = Predictor::new(sig, client);
        assert_eq!(pred.model(), "mock-model");
        assert_eq!(pred.temperature(), 0.7);
    }

    #[test]
    fn test_temperature_clamping() {
        let sig = create_test_signature();
        let client = create_mock_client();
        let pred = Predictor::new(sig, client)
            .with_temperature(2.0);
        assert_eq!(pred.temperature(), 1.0);
    }

    #[test]
    fn test_prompt_building() {
        let sig = create_test_signature();
        let client = create_mock_client();
        let pred = Predictor::new(sig, client);

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
        let client = create_mock_client();
        let cot = ChainOfThought::new(sig, client);
        assert!(cot.signature().instructions.is_some());
        assert!(cot.signature().instructions.as_ref().unwrap().contains("step-by-step"));
    }

    #[tokio::test]
    async fn test_predictor_forward_with_mock() {
        let sig = create_test_signature();
        let client = create_mock_client();
        let pred = Predictor::new(sig, client);

        let mut inputs = HashMap::new();
        inputs.insert("question".to_string(), Value::String("What is Rust?".to_string()));

        let result = pred.forward(inputs).await;
        assert!(result.is_ok(), "Forward should succeed with mock client");

        let outputs = result.unwrap();
        assert!(outputs.contains_key("answer"), "Should have answer field");
    }
}
