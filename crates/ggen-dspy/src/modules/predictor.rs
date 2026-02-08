//! Predictor module - basic LLM call with signature
//!
//! The fundamental building block for DSPy-style programming.
//! Implements signature-based prompting with configurable LLM parameters.

use crate::{DspyError, Module, ModuleOutput, Result};
use async_trait::async_trait;
use ggen_ai::dspy::Signature;
use tracing::{debug, warn};

/// Basic predictor module that executes an LLM call
///
/// The Predictor is the core building block for DSPy-style programming.
/// It takes a Signature (input/output specification) and generates prompts
/// that guide the LLM to produce structured outputs.
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_dspy::{Predictor, Result};
/// use ggen_ai::dspy::{Signature, InputField, OutputField};
///
/// # async fn example() -> Result<()> {
/// let signature = Signature::new("QA", "Answer questions")
///     .with_input(InputField::new("question", "The question", "String"))
///     .with_output(OutputField::new("answer", "The answer", "String"));
///
/// let predictor = Predictor::new(signature)
///     .with_temperature(0.7)
///     .with_max_tokens(1024);
///
/// let inputs = vec![("question", "What is Rust?")];
/// let output = predictor.forward(&inputs).await?;
/// println!("Answer: {}", output.get("answer")?);
/// # Ok(())
/// # }
/// ```
pub struct Predictor {
    /// Signature defining inputs and outputs
    signature: Signature,
    /// Module name
    name: String,
    /// Temperature for LLM sampling (0.0-2.0)
    temperature: f64,
    /// Maximum tokens to generate
    max_tokens: usize,
    /// Model name (optional, falls back to ggen-ai default)
    model: Option<String>,
}

impl Predictor {
    /// Create a new predictor with a signature
    pub fn new(signature: Signature) -> Self {
        Self {
            name: signature.name.clone(),
            signature,
            temperature: 0.7,
            max_tokens: 1024,
            model: None,
        }
    }

    /// Create a predictor with custom name
    pub fn with_name(signature: Signature, name: impl Into<String>) -> Self {
        Self {
            signature,
            name: name.into(),
            temperature: 0.7,
            max_tokens: 1024,
            model: None,
        }
    }

    /// Set temperature for LLM sampling
    pub fn with_temperature(mut self, temperature: f64) -> Self {
        self.temperature = temperature.clamp(0.0, 2.0);
        self
    }

    /// Set maximum tokens to generate
    pub fn with_max_tokens(mut self, max_tokens: usize) -> Self {
        self.max_tokens = max_tokens;
        self
    }

    /// Set specific model name
    pub fn with_model(mut self, model: impl Into<String>) -> Self {
        self.model = Some(model.into());
        self
    }

    /// Get the signature
    pub fn signature(&self) -> &Signature {
        &self.signature
    }

    /// Build prompt from signature and inputs
    fn build_prompt(&self, inputs: &[(&str, &str)]) -> Result<String> {
        let mut prompt = String::new();

        // Add signature description
        if !self.signature.description.is_empty() {
            prompt.push_str(&format!("{}\n\n", self.signature.description));
        }

        // Add custom instructions if present
        if let Some(ref instructions) = self.signature.instructions {
            prompt.push_str(&format!("Instructions: {}\n\n", instructions));
        }

        // Add input section
        prompt.push_str("---\n\n");
        for input_field in &self.signature.inputs {
            let field_name = input_field.name();

            // Find matching input value
            let value = inputs
                .iter()
                .find(|(k, _)| *k == field_name)
                .map(|(_, v)| *v)
                .ok_or_else(|| DspyError::MissingInput(field_name.to_string()))?;

            // Add field with description
            prompt.push_str(&format!("[{}]: {}\n", field_name, input_field.desc()));
            prompt.push_str(&format!("{}\n\n", value));
        }

        // Add output section prompt
        prompt.push_str("---\n\n");
        prompt.push_str("Please provide the following outputs:\n\n");
        for output_field in &self.signature.outputs {
            prompt.push_str(&format!(
                "[{}]: {} (type: {})\n",
                output_field.name(),
                output_field.desc(),
                output_field.type_annotation()
            ));
        }

        prompt.push_str("\nRespond in the format:\n");
        for output_field in &self.signature.outputs {
            prompt.push_str(&format!("{}: <your response>\n", output_field.name()));
        }

        Ok(prompt)
    }

    /// Parse LLM response into structured output
    fn parse_output(&self, response: &str) -> Result<ModuleOutput> {
        let mut output = ModuleOutput::new();

        debug!("Parsing LLM response: {}", response);

        // Try to extract each output field
        for output_field in &self.signature.outputs {
            let field_name = output_field.name();

            if let Some(value) = self.extract_field_value(response, field_name) {
                output.set(field_name, value);
            } else {
                // If single output field and no structured response, use entire response
                if self.signature.outputs.len() == 1 {
                    output.set(field_name, response.trim());
                } else {
                    warn!("Could not extract field '{}' from response", field_name);
                }
            }
        }

        Ok(output)
    }

    /// Extract field value from response using pattern matching
    fn extract_field_value(&self, response: &str, field_name: &str) -> Option<String> {
        // Try pattern: "field_name: value"
        let pattern = format!("{}:", field_name);

        for line in response.lines() {
            let trimmed = line.trim();
            if trimmed.starts_with(&pattern) {
                let value = trimmed[pattern.len()..].trim();
                if !value.is_empty() {
                    return Some(value.to_string());
                }
            }
        }

        None
    }

    /// Call LLM using ggen-ai client
    async fn call_llm(&self, prompt: &str) -> Result<String> {
        // Use genai client directly (same as ggen-ai predictor)
        use genai::chat::{ChatMessage, ChatOptions, ChatRequest};
        use genai::Client;

        let client = Client::default();

        // Determine model to use
        let ggen_model = std::env::var("GGEN_LLM_MODEL").ok();
        let default_model = std::env::var("DEFAULT_MODEL").ok();
        let model = self
            .model
            .as_deref()
            .or_else(|| ggen_model.as_deref())
            .or_else(|| default_model.as_deref())
            .unwrap_or("");

        if model.is_empty() {
            return Err(DspyError::module(
                "No model specified. Set GGEN_LLM_MODEL env var or use .with_model()",
            ));
        }

        debug!(
            "Calling LLM model: {} with temperature: {}",
            model, self.temperature
        );

        // Build chat request
        let chat_req = ChatRequest::new(vec![
            ChatMessage::system("You are a helpful AI assistant. Provide clear, accurate responses in the requested format."),
            ChatMessage::user(prompt.to_string()),
        ]);

        // Configure chat options
        let chat_options = ChatOptions::default()
            .with_temperature(self.temperature)
            .with_max_tokens(self.max_tokens as u32);

        // Execute request
        match client.exec_chat(model, chat_req, Some(&chat_options)).await {
            Ok(response) => {
                let content = response.first_text().unwrap_or_default().to_string();
                debug!("LLM response length: {} chars", content.len());
                Ok(content)
            }
            Err(e) => {
                warn!("LLM call failed: {}", e);
                Err(DspyError::module(format!("LLM call failed: {}", e)))
            }
        }
    }
}

#[async_trait]
impl Module for Predictor {
    async fn forward(&self, inputs: &[(&str, &str)]) -> Result<ModuleOutput> {
        // Build prompt from signature
        let prompt = self.build_prompt(inputs)?;

        debug!(
            "Built prompt for module '{}': {} chars",
            self.name,
            prompt.len()
        );

        // Call LLM
        let response = self.call_llm(&prompt).await?;

        // Parse response into structured output
        self.parse_output(&response)
    }

    fn name(&self) -> &str {
        &self.name
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ggen_ai::dspy::{InputField, OutputField};

    // Arrange-Act-Assert pattern from chicago-tdd-tools

    #[test]
    fn test_predictor_creation() {
        // Arrange
        let signature = Signature::new("QA", "Question answering")
            .with_input(InputField::new("question", "The question", "String"))
            .with_output(OutputField::new("answer", "The answer", "String"));

        // Act
        let predictor = Predictor::new(signature);

        // Assert
        assert_eq!(predictor.name(), "QA");
        assert_eq!(predictor.temperature, 0.7);
        assert_eq!(predictor.max_tokens, 1024);
    }

    #[test]
    fn test_predictor_with_custom_name() {
        // Arrange
        let signature = Signature::new("QA", "Question answering")
            .with_input(InputField::new("question", "The question", "String"))
            .with_output(OutputField::new("answer", "The answer", "String"));

        // Act
        let predictor = Predictor::with_name(signature, "CustomPredictor");

        // Assert
        assert_eq!(predictor.name(), "CustomPredictor");
    }

    #[test]
    fn test_predictor_with_temperature() {
        // Arrange
        let signature = Signature::new("Test", "Test")
            .with_input(InputField::new("input", "Input", "String"))
            .with_output(OutputField::new("output", "Output", "String"));

        // Act
        let predictor = Predictor::new(signature).with_temperature(0.5);

        // Assert
        assert_eq!(predictor.temperature, 0.5);
    }

    #[test]
    fn test_predictor_temperature_clamping() {
        // Arrange
        let signature = Signature::new("Test", "Test")
            .with_input(InputField::new("input", "Input", "String"))
            .with_output(OutputField::new("output", "Output", "String"));

        // Act - temperature too high
        let predictor = Predictor::new(signature.clone()).with_temperature(3.0);

        // Assert
        assert_eq!(predictor.temperature, 2.0);

        // Act - temperature too low
        let predictor2 = Predictor::new(signature).with_temperature(-0.5);

        // Assert
        assert_eq!(predictor2.temperature, 0.0);
    }

    #[test]
    fn test_build_prompt() {
        // Arrange
        let signature = Signature::new("QA", "Answer questions accurately")
            .with_input(InputField::new(
                "question",
                "The question to answer",
                "String",
            ))
            .with_output(OutputField::new("answer", "The answer", "String"));

        let predictor = Predictor::new(signature);
        let inputs = vec![("question", "What is Rust?")];

        // Act
        let prompt = predictor.build_prompt(&inputs).unwrap();

        // Assert
        assert!(prompt.contains("Answer questions accurately"));
        assert!(prompt.contains("question"));
        assert!(prompt.contains("What is Rust?"));
        assert!(prompt.contains("answer:"));
    }

    #[test]
    fn test_build_prompt_missing_input() {
        // Arrange
        let signature = Signature::new("QA", "QA")
            .with_input(InputField::new("question", "Question", "String"))
            .with_output(OutputField::new("answer", "Answer", "String"));

        let predictor = Predictor::new(signature);
        let inputs = vec![]; // Missing required input

        // Act
        let result = predictor.build_prompt(&inputs);

        // Assert
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), DspyError::MissingInput(_)));
    }

    #[test]
    fn test_extract_field_value() {
        // Arrange
        let signature = Signature::new("Test", "Test")
            .with_input(InputField::new("input", "Input", "String"))
            .with_output(OutputField::new("result", "Result", "String"));

        let predictor = Predictor::new(signature);
        let response = "result: This is the extracted value\nother: data";

        // Act
        let value = predictor.extract_field_value(response, "result");

        // Assert
        assert_eq!(value, Some("This is the extracted value".to_string()));
    }

    #[test]
    fn test_parse_output_single_field() {
        // Arrange
        let signature = Signature::new("Test", "Test")
            .with_input(InputField::new("input", "Input", "String"))
            .with_output(OutputField::new("answer", "Answer", "String"));

        let predictor = Predictor::new(signature);
        let response = "answer: The response value";

        // Act
        let output = predictor.parse_output(response).unwrap();

        // Assert
        assert_eq!(output.get("answer").unwrap(), "The response value");
    }

    #[test]
    fn test_parse_output_multiple_fields() {
        // Arrange
        let signature = Signature::new("MultiOut", "Multiple outputs")
            .with_input(InputField::new("input", "Input", "String"))
            .with_output(OutputField::new("result", "Result", "String"))
            .with_output(OutputField::new("confidence", "Confidence", "String"));

        let predictor = Predictor::new(signature);
        let response = "result: Success\nconfidence: High";

        // Act
        let output = predictor.parse_output(response).unwrap();

        // Assert
        assert_eq!(output.get("result").unwrap(), "Success");
        assert_eq!(output.get("confidence").unwrap(), "High");
    }

    #[test]
    fn test_parse_output_fallback_single_field() {
        // Arrange
        let signature = Signature::new("Test", "Test")
            .with_input(InputField::new("input", "Input", "String"))
            .with_output(OutputField::new("answer", "Answer", "String"));

        let predictor = Predictor::new(signature);
        let response = "This is a plain response without structure";

        // Act
        let output = predictor.parse_output(response).unwrap();

        // Assert
        assert_eq!(
            output.get("answer").unwrap(),
            "This is a plain response without structure"
        );
    }
}
