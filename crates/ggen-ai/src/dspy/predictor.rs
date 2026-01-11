//! DSPy Predictor - LLM-backed prediction with signature interface
//!
//! This module implements the core prediction logic using LLMs.
//! Key improvements over naive implementations:
//! - Constraint-aware decoding (validates outputs against field constraints)
//! - Prompt-IR based prompt construction
//! - Model capabilities awareness

use crate::dspy::constraint::decode_and_validate;
use crate::dspy::model_capabilities::{Model, ModelCapabilities};
use crate::dspy::module::{Module, ModuleError, ModuleResult};
use crate::dspy::prompt_ir::{OutputFormat, PromptIR, PromptRenderer, RenderConfig, TextRenderer};
use crate::dspy::signature::Signature;
use genai::chat::{ChatMessage, ChatOptions, ChatRequest};
use genai::Client;
use serde_json::Value;
use std::collections::HashMap;
use tracing::{debug, warn};

/// Predictor: LLM-backed prediction with signature interface
///
/// A concrete implementation of Module that uses an LLM to generate outputs
/// based on inputs defined by the signature.
///
/// Key capabilities:
/// - Constraint-aware decoding: Validates outputs against field constraints
/// - Prompt-IR based: Structured prompt representation before rendering
/// - Model-aware: Uses model capabilities for optimal behavior
pub struct Predictor {
    signature: Signature,
    model: Model,
    temperature: f32,
    /// Whether to validate outputs against constraints
    validate_outputs: bool,
    /// Preferred output format
    output_format: OutputFormat,
    /// Render configuration for prompt generation
    render_config: RenderConfig,
}

impl Predictor {
    /// Create a new predictor with a signature and model name
    pub fn new(signature: Signature) -> Self {
        // Get model from environment or use empty string (will error if not set)
        let model_name = std::env::var("GGEN_LLM_MODEL")
            .or_else(|_| std::env::var("DEFAULT_MODEL"))
            .unwrap_or_default();

        Self {
            signature,
            model: Model::from_name(model_name),
            temperature: 0.7,
            validate_outputs: true,
            output_format: OutputFormat::Text,
            render_config: RenderConfig::new().with_prefixes(),
        }
    }

    /// Create a predictor with an explicit model name
    pub fn with_model(signature: Signature, model: impl Into<String>) -> Self {
        Self {
            signature,
            model: Model::from_name(model),
            temperature: 0.7,
            validate_outputs: true,
            output_format: OutputFormat::Text,
            render_config: RenderConfig::new().with_prefixes(),
        }
    }

    /// Create a predictor with a typed Model
    pub fn with_typed_model(signature: Signature, model: Model) -> Self {
        Self {
            signature,
            model,
            temperature: 0.7,
            validate_outputs: true,
            output_format: OutputFormat::Text,
            render_config: RenderConfig::new().with_prefixes(),
        }
    }

    /// Set the model by name
    pub fn model(mut self, model: impl Into<String>) -> Self {
        self.model = Model::from_name(model);
        self
    }

    /// Set a typed Model
    pub fn typed_model(mut self, model: Model) -> Self {
        self.model = model;
        self
    }

    /// Set the temperature (controls randomness, 0.0-2.0)
    pub fn with_temperature(mut self, temperature: f32) -> Self {
        self.temperature = temperature.clamp(0.0, 2.0);
        self
    }

    /// Enable or disable output validation
    pub fn with_output_validation(mut self, validate: bool) -> Self {
        self.validate_outputs = validate;
        self
    }

    /// Set the output format
    pub fn with_output_format(mut self, format: OutputFormat) -> Self {
        self.output_format = format;
        self
    }

    /// Configure constraint embedding in prompts
    pub fn with_constraints_in_prompt(mut self) -> Self {
        self.render_config = self.render_config.with_constraints();
        self
    }

    /// Get the model name
    pub fn model_name(&self) -> &str {
        &self.model.name
    }

    /// Get the typed Model
    pub fn model_ref(&self) -> &Model {
        &self.model
    }

    /// Get the temperature
    pub fn temperature(&self) -> f32 {
        self.temperature
    }

    /// Get model capabilities
    pub fn capabilities(&self) -> &ModelCapabilities {
        &self.model.capabilities
    }

    /// Build a prompt from inputs using Prompt-IR
    fn build_prompt(&self, inputs: &HashMap<String, Value>) -> Result<String, ModuleError> {
        let input_vec: Vec<(String, Value)> = inputs
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();

        let mut ir = PromptIR::from_signature(&self.signature, &input_vec);

        // Set output format if JSON is preferred
        if matches!(self.output_format, OutputFormat::Json) {
            ir = ir.with_output_format(OutputFormat::Json);
        }

        // Render using the text renderer
        let renderer = TextRenderer;
        Ok(renderer.render(&ir, &self.render_config))
    }

    /// Parse and validate output from LLM response using constraint-aware decoding
    fn parse_output(&self, response: &str) -> Result<HashMap<String, Value>, ModuleError> {
        if self.validate_outputs {
            // Use constraint-aware decoding
            decode_and_validate(response, &self.signature.outputs).map_err(|e| {
                ModuleError::Other(format!("Output validation failed: {}", e))
            })
        } else {
            // Fall back to simple extraction without validation
            self.parse_output_simple(response)
        }
    }

    /// Simple output parsing without constraint validation (fallback)
    fn parse_output_simple(&self, response: &str) -> Result<HashMap<String, Value>, ModuleError> {
        let mut outputs = HashMap::new();

        // Try JSON parsing first
        if let Ok(json_value) = serde_json::from_str::<Value>(response) {
            if let Some(obj) = json_value.as_object() {
                for output_field in &self.signature.outputs {
                    if let Some(value) = obj.get(output_field.name()) {
                        outputs.insert(output_field.name().to_string(), value.clone());
                    }
                }
                if !outputs.is_empty() {
                    return Ok(outputs);
                }
            }
        }

        // Fall back to text parsing
        for output_field in &self.signature.outputs {
            if let Some(value) = self.extract_field_value(response, output_field.name()) {
                outputs.insert(output_field.name().to_string(), Value::String(value));
            }
        }

        if outputs.is_empty() {
            // If no structured output found, return entire response
            outputs.insert(
                self.signature
                    .outputs
                    .first()
                    .map(|f| f.name().to_string())
                    .unwrap_or_else(|| "output".to_string()),
                Value::String(response.to_string()),
            );
        }

        Ok(outputs)
    }

    /// Extract field value from response text
    fn extract_field_value(&self, response: &str, field_name: &str) -> Option<String> {
        let patterns = [
            format!("{}: ", field_name),
            format!("{}:", field_name),
            format!("**{}**:", field_name),
            format!("**{}**: ", field_name),
        ];

        for pattern in &patterns {
            if let Some(start) = response.find(pattern) {
                let text = &response[start + pattern.len()..];
                let value = text.lines().next().unwrap_or("").trim();
                if !value.is_empty() {
                    return Some(value.to_string());
                }
            }
        }
        None
    }

    /// Call the LLM with the given prompt using genai
    async fn call_llm(&self, prompt: &str) -> Result<String, ModuleError> {
        let client = Client::default();

        if self.model.name.is_empty() {
            return Err(ModuleError::LlmError(
                "Model name not set. Set GGEN_LLM_MODEL or DEFAULT_MODEL env var, or use .model()"
                    .to_string(),
            ));
        }

        debug!(
            "Calling LLM model: {} (provider: {:?})",
            self.model.name, self.model.provider
        );

        // Build system message based on output format
        let system_msg = match self.output_format {
            OutputFormat::Json => {
                "You are a helpful assistant. Respond with valid JSON only."
            }
            _ => "You are a helpful assistant. Provide responses in the exact format requested.",
        };

        // Create chat request with system prompt and user input
        let chat_req = ChatRequest::new(vec![
            ChatMessage::system(system_msg.to_string()),
            ChatMessage::user(prompt.to_string()),
        ]);

        // Set chat options using model capabilities
        let max_tokens = self
            .model
            .capabilities
            .max_output_tokens
            .min(4096) as u32;

        let chat_options = ChatOptions::default()
            .with_temperature(self.temperature as f64)
            .with_max_tokens(max_tokens);

        // Execute the request
        match client
            .exec_chat(&self.model.name, chat_req, Some(&chat_options))
            .await
        {
            Ok(response) => {
                let content = response.first_text().unwrap_or_default().to_string();
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
        debug!("Built prompt for {} with model {}", self.signature.name, self.model.name);

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
    use crate::dspy::field::{FieldConstraints, InputField, OutputField};

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
        let pred = Predictor::with_model(sig, "test-model").with_temperature(3.0);
        assert_eq!(pred.temperature(), 2.0);
    }

    #[test]
    fn test_model_setter() {
        let sig = create_test_signature();
        let pred = Predictor::new(sig).model("my-model");
        assert_eq!(pred.model_name(), "my-model");
    }

    #[test]
    fn test_typed_model() {
        let sig = create_test_signature();
        let model = Model::from_name("gpt-4-turbo");
        let pred = Predictor::with_typed_model(sig, model);

        assert_eq!(pred.model_name(), "gpt-4-turbo");
        assert!(pred.capabilities().function_calling);
    }

    #[test]
    fn test_prompt_building() {
        let sig = create_test_signature();
        let pred = Predictor::with_model(sig, "test-model");

        let mut inputs = HashMap::new();
        inputs.insert(
            "question".to_string(),
            Value::String("What is Rust?".to_string()),
        );

        let prompt = pred.build_prompt(&inputs).unwrap();
        assert!(prompt.contains("question") && prompt.contains("What is Rust?"));
        assert!(prompt.contains("answer"));
    }

    #[test]
    fn test_prompt_with_constraints() {
        let sig = Signature::new("Constrained", "Test with constraints")
            .with_input(InputField::new("input", "Input", "String"))
            .with_output(
                OutputField::new("status", "Status", "String").add_constraints(
                    FieldConstraints::new()
                        .required(true)
                        .enum_values(vec!["active".to_string(), "inactive".to_string()]),
                ),
            );

        let pred = Predictor::with_model(sig, "test-model").with_constraints_in_prompt();

        let mut inputs = HashMap::new();
        inputs.insert("input".to_string(), Value::String("test".to_string()));

        let prompt = pred.build_prompt(&inputs).unwrap();
        assert!(prompt.contains("status"));
    }

    #[test]
    fn test_output_format_json() {
        let sig = create_test_signature();
        let pred = Predictor::with_model(sig, "test-model").with_output_format(OutputFormat::Json);

        let mut inputs = HashMap::new();
        inputs.insert(
            "question".to_string(),
            Value::String("What is Rust?".to_string()),
        );

        let prompt = pred.build_prompt(&inputs).unwrap();
        assert!(prompt.contains("JSON"));
    }

    #[test]
    fn test_output_validation_toggle() {
        let sig = create_test_signature();
        let pred = Predictor::with_model(sig, "test-model").with_output_validation(false);

        // Parse simple response without validation
        let result = pred.parse_output_simple("answer: This is the answer").unwrap();
        assert!(result.contains_key("answer"));
    }

    #[test]
    fn test_chain_of_thought() {
        let sig = create_test_signature();
        let cot = ChainOfThought::new(sig);
        assert!(cot.signature().instructions.is_some());
        assert!(cot
            .signature()
            .instructions
            .as_ref()
            .unwrap()
            .contains("step-by-step"));
    }

    #[test]
    fn test_parse_json_response() {
        let sig = create_test_signature();
        let pred = Predictor::with_model(sig, "test-model").with_output_validation(false);

        let json_response = r#"{"answer": "42 is the answer"}"#;
        let result = pred.parse_output_simple(json_response).unwrap();

        assert_eq!(result.get("answer").unwrap(), &Value::String("42 is the answer".to_string()));
    }

    #[test]
    fn test_parse_markdown_response() {
        let sig = create_test_signature();
        let pred = Predictor::with_model(sig, "test-model").with_output_validation(false);

        let md_response = "**answer**: The answer is 42";
        let result = pred.parse_output_simple(md_response).unwrap();

        assert!(result.get("answer").unwrap().as_str().unwrap().contains("42"));
    }

    #[test]
    fn test_model_capabilities_access() {
        let sig = create_test_signature();
        let pred = Predictor::with_model(sig, "claude-3-opus");

        // Claude 3 Opus should have 200k context
        assert_eq!(pred.capabilities().max_context_tokens, 200_000);
        assert!(pred.capabilities().vision);
    }
}
