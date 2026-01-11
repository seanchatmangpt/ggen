//! DSPy Optimizer - Bootstrap Few-Shot Learning
//!
//! Provides optimization strategies for DSPy modules:
//! - `BootstrapFewShot` - Bootstrap demonstrations from validation set
//! - `Example` - Training/validation example with inputs and outputs
//! - `Demonstration` - Successful example used as few-shot prompt
//! - `OptimizedPredictor` - Predictor augmented with demonstrations

use crate::dspy::{Module, ModuleError, Signature};
use crate::dspy::module::ModuleResult;
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;
use tracing::{debug, info, warn};

/// Example for training/validation with inputs and expected outputs
///
/// Represents a single training or validation example consisting of
/// input fields and expected output fields. Used by optimizers to
/// bootstrap demonstrations.
#[derive(Debug, Clone)]
pub struct Example {
    /// Input field values
    pub inputs: HashMap<String, Value>,

    /// Expected output field values (ground truth)
    pub outputs: HashMap<String, Value>,
}

impl Example {
    /// Create a new example with inputs and expected outputs
    pub fn new(inputs: HashMap<String, Value>, outputs: HashMap<String, Value>) -> Self {
        Self { inputs, outputs }
    }

    /// Create example from just inputs (for inference)
    pub fn from_inputs(inputs: HashMap<String, Value>) -> Self {
        Self {
            inputs,
            outputs: HashMap::new(),
        }
    }
}

/// Demonstration - A successful example used as few-shot prompt
///
/// Represents a bootstrapped demonstration that will be included
/// in the few-shot prompt to guide the model's behavior.
#[derive(Debug, Clone)]
pub struct Demonstration {
    /// Input field values
    pub inputs: HashMap<String, Value>,

    /// Successful output field values
    pub outputs: HashMap<String, Value>,
}

impl Demonstration {
    /// Create a new demonstration
    pub fn new(inputs: HashMap<String, Value>, outputs: HashMap<String, Value>) -> Self {
        Self { inputs, outputs }
    }

    /// Format demonstration as text for prompt
    pub fn format(&self, signature: &Signature) -> String {
        let mut text = String::new();

        // Format inputs
        for input_field in &signature.inputs {
            if let Some(value) = self.inputs.get(input_field.name()) {
                let value_str = match value {
                    Value::String(s) => s.clone(),
                    _ => value.to_string(),
                };
                text.push_str(&format!("{}: {}\n", input_field.name(), value_str));
            }
        }

        // Format outputs
        for output_field in &signature.outputs {
            if let Some(value) = self.outputs.get(output_field.name()) {
                let value_str = match value {
                    Value::String(s) => s.clone(),
                    _ => value.to_string(),
                };
                text.push_str(&format!("{}: {}\n", output_field.name(), value_str));
            }
        }

        text
    }
}

/// Metric function type for evaluating examples
///
/// Takes an example (with expected outputs) and actual outputs from the model,
/// returns whether the outputs are acceptable.
pub type MetricFn = Arc<dyn Fn(&Example, &HashMap<String, Value>) -> Result<bool, ModuleError> + Send + Sync>;

/// BootstrapFewShot optimizer
///
/// Implements the Bootstrap Few-Shot algorithm from DSPy:
/// 1. Collect examples from training/validation set
/// 2. Bootstrap demonstrations by running teacher (or student) on examples
/// 3. Select successful demonstrations using a metric function
/// 4. Create optimized predictor with demonstrations in prompt
///
/// # Algorithm
///
/// For each example in the training set (up to max_bootstrapped_demos):
/// 1. Run teacher predictor (or student if no teacher) on example inputs
/// 2. Evaluate output using metric function
/// 3. If metric passes, save as demonstration
/// 4. Continue until max demonstrations collected
///
/// The resulting OptimizedPredictor includes demonstrations in its prompt
/// to guide the model toward successful behavior patterns.
pub struct BootstrapFewShot {
    /// Metric function to evaluate if an example is successful
    metric: MetricFn,

    /// Maximum number of bootstrapped demonstrations to collect
    max_bootstrapped_demos: usize,

    /// Maximum number of labeled examples to use (for future use)
    max_labeled_demos: usize,

    /// Optional teacher model (if None, uses student)
    teacher: Option<Arc<dyn Module>>,
}

impl BootstrapFewShot {
    /// Create a new BootstrapFewShot optimizer with a metric function
    ///
    /// # Arguments
    /// * `metric` - Function that evaluates if outputs are acceptable
    ///
    /// # Example
    /// ```ignore
    /// use std::sync::Arc;
    /// use ggen_ai::dspy::optimizer::{BootstrapFewShot, Example};
    ///
    /// let metric = Arc::new(|example: &Example, output: &HashMap<String, Value>| {
    ///     // Check if output matches expected
    ///     Ok(example.outputs.get("answer") == output.get("answer"))
    /// });
    ///
    /// let optimizer = BootstrapFewShot::new(metric);
    /// ```
    pub fn new(metric: MetricFn) -> Self {
        Self {
            metric,
            max_bootstrapped_demos: 4,
            max_labeled_demos: 16,
            teacher: None,
        }
    }

    /// Set maximum number of bootstrapped demonstrations
    pub fn with_max_bootstrapped_demos(mut self, max: usize) -> Self {
        self.max_bootstrapped_demos = max;
        self
    }

    /// Set maximum number of labeled demonstrations
    pub fn with_max_labeled_demos(mut self, max: usize) -> Self {
        self.max_labeled_demos = max;
        self
    }

    /// Set teacher model for bootstrapping
    ///
    /// If not set, uses the student model for bootstrapping.
    pub fn with_teacher(mut self, teacher: Arc<dyn Module>) -> Self {
        self.teacher = Some(teacher);
        self
    }

    /// Compile/optimize a student module using training examples
    ///
    /// # Arguments
    /// * `student` - The module to optimize
    /// * `trainset` - Training examples with inputs and expected outputs
    ///
    /// # Returns
    /// An `OptimizedPredictor` with bootstrapped demonstrations
    ///
    /// # Errors
    /// Returns `ModuleError` if:
    /// - Training set is empty
    /// - Forward pass fails on any example
    /// - Metric evaluation fails
    pub async fn compile(
        &self,
        student: &dyn Module,
        trainset: &[Example],
    ) -> Result<OptimizedPredictor, ModuleError> {
        if trainset.is_empty() {
            return Err(ModuleError::Other(
                "Training set is empty. Provide at least one example.".to_string()
            ));
        }

        info!("Starting BootstrapFewShot compilation with {} examples", trainset.len());

        // Bootstrap demonstrations
        let mut demonstrations = Vec::new();
        let max_attempts = trainset.len().min(self.max_bootstrapped_demos * 3); // Try up to 3x

        for (idx, example) in trainset.iter().enumerate().take(max_attempts) {
            if demonstrations.len() >= self.max_bootstrapped_demos {
                break;
            }

            debug!("Bootstrapping example {}/{}", idx + 1, max_attempts);

            // Use teacher if available, otherwise use student
            let predictor = self.teacher.as_ref()
                .map(|t| t.as_ref())
                .unwrap_or(student);

            // Forward pass
            let output = match predictor.forward(example.inputs.clone()).await {
                Ok(out) => out,
                Err(e) => {
                    warn!("Forward pass failed for example {}: {}", idx, e);
                    continue;
                }
            };

            // Evaluate with metric
            let is_successful = match (self.metric)(example, &output) {
                Ok(success) => success,
                Err(e) => {
                    warn!("Metric evaluation failed for example {}: {}", idx, e);
                    continue;
                }
            };

            if is_successful {
                debug!("Example {} passed metric, adding as demonstration", idx);
                demonstrations.push(Demonstration::new(
                    example.inputs.clone(),
                    output,
                ));
            } else {
                debug!("Example {} failed metric, skipping", idx);
            }
        }

        info!(
            "Bootstrap complete: collected {}/{} demonstrations",
            demonstrations.len(),
            self.max_bootstrapped_demos
        );

        if demonstrations.is_empty() {
            warn!("No successful demonstrations found. Returning optimizer with no examples.");
        }

        Ok(OptimizedPredictor::new(
            student.signature().clone(),
            demonstrations,
        ))
    }
}

/// Optimized predictor with few-shot demonstrations
///
/// A predictor that includes bootstrapped demonstrations in its prompt
/// to improve performance through few-shot learning.
pub struct OptimizedPredictor {
    signature: Signature,
    demonstrations: Vec<Demonstration>,
    model: String,
    temperature: f32,
}

impl OptimizedPredictor {
    /// Create a new optimized predictor
    pub fn new(signature: Signature, demonstrations: Vec<Demonstration>) -> Self {
        let model = std::env::var("GGEN_LLM_MODEL")
            .or_else(|_| std::env::var("DEFAULT_MODEL"))
            .unwrap_or_else(|_| "".to_string());

        Self {
            signature,
            demonstrations,
            model,
            temperature: 0.7,
        }
    }

    /// Set the model name
    pub fn with_model(mut self, model: impl Into<String>) -> Self {
        self.model = model.into();
        self
    }

    /// Set the temperature
    pub fn with_temperature(mut self, temperature: f32) -> Self {
        self.temperature = temperature.clamp(0.0, 2.0);
        self
    }

    /// Get the number of demonstrations
    pub fn demonstration_count(&self) -> usize {
        self.demonstrations.len()
    }

    /// Get demonstrations
    pub fn demonstrations(&self) -> &[Demonstration] {
        &self.demonstrations
    }

    /// Build few-shot prompt with demonstrations
    fn build_prompt(&self, inputs: &HashMap<String, Value>) -> Result<String, ModuleError> {
        let mut prompt = format!("{}\n\n", self.signature.description);

        if let Some(instructions) = &self.signature.instructions {
            prompt.push_str(&format!("Instructions: {}\n\n", instructions));
        }

        // Add demonstrations as examples
        if !self.demonstrations.is_empty() {
            prompt.push_str("Examples:\n\n");
            for (idx, demo) in self.demonstrations.iter().enumerate() {
                prompt.push_str(&format!("--- Example {} ---\n", idx + 1));
                prompt.push_str(&demo.format(&self.signature));
                prompt.push_str("\n");
            }
            prompt.push_str("--- Your Turn ---\n\n");
        }

        // Add current input
        prompt.push_str("Input:\n");
        for input_field in &self.signature.inputs {
            if let Some(value) = inputs.get(input_field.name()) {
                let value_str = match value {
                    Value::String(s) => s.clone(),
                    _ => value.to_string(),
                };
                prompt.push_str(&format!("{}: {}\n", input_field.name(), value_str));
            }
        }

        prompt.push_str("\nOutput:\n");
        for output_field in &self.signature.outputs {
            prompt.push_str(&format!("{}: ", output_field.name()));
        }

        Ok(prompt)
    }

    /// Parse output from LLM response (same as Predictor)
    fn parse_output(&self, response: &str) -> Result<HashMap<String, Value>, ModuleError> {
        let mut outputs = HashMap::new();

        for output_field in &self.signature.outputs {
            if let Some(value) = self.extract_field_value(response, output_field.name()) {
                outputs.insert(output_field.name().to_string(), Value::String(value));
            }
        }

        if outputs.is_empty() {
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
            let value = text.lines().next().unwrap_or("").trim();
            if !value.is_empty() {
                return Some(value.to_string());
            }
        }
        None
    }

    /// Call the LLM with the given prompt
    async fn call_llm(&self, prompt: &str) -> Result<String, ModuleError> {
        use genai::{Client, chat::{ChatMessage, ChatOptions, ChatRequest}};

        let client = Client::default();

        if self.model.is_empty() {
            return Err(ModuleError::LlmError(
                "Model name not set. Set GGEN_LLM_MODEL or DEFAULT_MODEL env var, or use .with_model()".to_string()
            ));
        }

        debug!("Calling LLM model: {} with {} demonstrations", self.model, self.demonstrations.len());

        let chat_req = ChatRequest::new(vec![
            ChatMessage::system("You are a helpful assistant. Follow the examples provided and respond in the exact format requested."),
            ChatMessage::user(prompt.to_string()),
        ]);

        let chat_options = ChatOptions::default()
            .with_temperature(self.temperature as f64)
            .with_max_tokens(4096);

        match client.exec_chat(&self.model, chat_req, Some(&chat_options)).await {
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
impl Module for OptimizedPredictor {
    fn signature(&self) -> &Signature {
        &self.signature
    }

    async fn forward(&self, inputs: HashMap<String, Value>) -> ModuleResult<HashMap<String, Value>> {
        // Validate inputs
        self.validate_inputs(&inputs)?;

        // Build few-shot prompt with demonstrations
        let prompt = self.build_prompt(&inputs)?;
        debug!("Built few-shot prompt for {} with {} demonstrations",
               self.signature.name, self.demonstrations.len());

        // Call LLM
        let response = self.call_llm(&prompt).await?;
        debug!("Received LLM response: {}", response);

        // Parse output
        self.parse_output(&response)
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

    // ===== Example Tests =====

    #[test]
    fn test_example_creation() {
        let mut inputs = HashMap::new();
        inputs.insert("question".to_string(), Value::String("What is Rust?".to_string()));

        let mut outputs = HashMap::new();
        outputs.insert("answer".to_string(), Value::String("A systems programming language".to_string()));

        let example = Example::new(inputs.clone(), outputs.clone());

        assert_eq!(example.inputs.len(), 1);
        assert_eq!(example.outputs.len(), 1);
    }

    #[test]
    fn test_example_from_inputs() {
        let mut inputs = HashMap::new();
        inputs.insert("question".to_string(), Value::String("What is DSPy?".to_string()));

        let example = Example::from_inputs(inputs);

        assert_eq!(example.inputs.len(), 1);
        assert_eq!(example.outputs.len(), 0);
    }

    // ===== Demonstration Tests =====

    #[test]
    fn test_demonstration_creation() {
        let mut inputs = HashMap::new();
        inputs.insert("question".to_string(), Value::String("What is 2+2?".to_string()));

        let mut outputs = HashMap::new();
        outputs.insert("answer".to_string(), Value::String("4".to_string()));

        let demo = Demonstration::new(inputs, outputs);

        assert_eq!(demo.inputs.len(), 1);
        assert_eq!(demo.outputs.len(), 1);
    }

    #[test]
    fn test_demonstration_format() {
        let sig = create_test_signature();

        let mut inputs = HashMap::new();
        inputs.insert("question".to_string(), Value::String("What is Rust?".to_string()));

        let mut outputs = HashMap::new();
        outputs.insert("answer".to_string(), Value::String("A programming language".to_string()));

        let demo = Demonstration::new(inputs, outputs);
        let formatted = demo.format(&sig);

        assert!(formatted.contains("question:"));
        assert!(formatted.contains("What is Rust?"));
        assert!(formatted.contains("answer:"));
        assert!(formatted.contains("A programming language"));
    }

    // ===== BootstrapFewShot Tests =====

    #[test]
    fn test_bootstrap_creation() {
        let metric = Arc::new(|_example: &Example, _output: &HashMap<String, Value>| {
            Ok(true)
        });

        let optimizer = BootstrapFewShot::new(metric);
        assert_eq!(optimizer.max_bootstrapped_demos, 4);
        assert_eq!(optimizer.max_labeled_demos, 16);
    }

    #[test]
    fn test_bootstrap_builder_methods() {
        let metric = Arc::new(|_example: &Example, _output: &HashMap<String, Value>| {
            Ok(true)
        });

        let optimizer = BootstrapFewShot::new(metric)
            .with_max_bootstrapped_demos(8)
            .with_max_labeled_demos(32);

        assert_eq!(optimizer.max_bootstrapped_demos, 8);
        assert_eq!(optimizer.max_labeled_demos, 32);
    }

    // ===== OptimizedPredictor Tests =====

    #[test]
    fn test_optimized_predictor_creation() {
        let sig = create_test_signature();
        let demos = vec![];

        let predictor = OptimizedPredictor::new(sig, demos);

        assert_eq!(predictor.demonstration_count(), 0);
        assert_eq!(predictor.temperature, 0.7);
    }

    #[test]
    fn test_optimized_predictor_with_demonstrations() {
        let sig = create_test_signature();

        let mut inputs = HashMap::new();
        inputs.insert("question".to_string(), Value::String("Example question".to_string()));

        let mut outputs = HashMap::new();
        outputs.insert("answer".to_string(), Value::String("Example answer".to_string()));

        let demo = Demonstration::new(inputs, outputs);
        let predictor = OptimizedPredictor::new(sig, vec![demo]);

        assert_eq!(predictor.demonstration_count(), 1);
    }

    #[test]
    fn test_optimized_predictor_builder() {
        let sig = create_test_signature();
        let predictor = OptimizedPredictor::new(sig, vec![])
            .with_model("test-model")
            .with_temperature(0.5);

        assert_eq!(predictor.model, "test-model");
        assert_eq!(predictor.temperature, 0.5);
    }

    #[test]
    fn test_optimized_predictor_temperature_clamping() {
        let sig = create_test_signature();
        let predictor = OptimizedPredictor::new(sig, vec![])
            .with_temperature(3.0);

        assert_eq!(predictor.temperature, 2.0);
    }

    #[test]
    fn test_build_prompt_no_demonstrations() {
        let sig = create_test_signature();
        let predictor = OptimizedPredictor::new(sig, vec![]);

        let mut inputs = HashMap::new();
        inputs.insert("question".to_string(), Value::String("What is Rust?".to_string()));

        let prompt = predictor.build_prompt(&inputs).unwrap();

        assert!(prompt.contains("Question answering"));
        assert!(prompt.contains("question:"));
        assert!(prompt.contains("What is Rust?"));
        assert!(!prompt.contains("Examples:"));
    }

    #[test]
    fn test_build_prompt_with_demonstrations() {
        let sig = create_test_signature();

        let mut demo_inputs = HashMap::new();
        demo_inputs.insert("question".to_string(), Value::String("What is 2+2?".to_string()));

        let mut demo_outputs = HashMap::new();
        demo_outputs.insert("answer".to_string(), Value::String("4".to_string()));

        let demo = Demonstration::new(demo_inputs, demo_outputs);
        let predictor = OptimizedPredictor::new(sig, vec![demo]);

        let mut inputs = HashMap::new();
        inputs.insert("question".to_string(), Value::String("What is 3+3?".to_string()));

        let prompt = predictor.build_prompt(&inputs).unwrap();

        assert!(prompt.contains("Examples:"));
        assert!(prompt.contains("Example 1"));
        assert!(prompt.contains("What is 2+2?"));
        assert!(prompt.contains("Your Turn"));
        assert!(prompt.contains("What is 3+3?"));
    }

    #[test]
    fn test_extract_field_value() {
        let sig = create_test_signature();
        let predictor = OptimizedPredictor::new(sig, vec![]);

        let response = "answer: Rust is a systems programming language\nother: stuff";
        let value = predictor.extract_field_value(response, "answer");

        assert_eq!(value, Some("Rust is a systems programming language".to_string()));
    }

    #[test]
    fn test_extract_field_value_missing() {
        let sig = create_test_signature();
        let predictor = OptimizedPredictor::new(sig, vec![]);

        let response = "something: else";
        let value = predictor.extract_field_value(response, "answer");

        assert_eq!(value, None);
    }

    #[test]
    fn test_parse_output_with_fields() {
        let sig = create_test_signature();
        let predictor = OptimizedPredictor::new(sig, vec![]);

        let response = "answer: Rust is awesome";
        let outputs = predictor.parse_output(response).unwrap();

        assert_eq!(outputs.len(), 1);
        assert_eq!(
            outputs.get("answer").unwrap(),
            &Value::String("Rust is awesome".to_string())
        );
    }

    #[test]
    fn test_parse_output_fallback() {
        let sig = create_test_signature();
        let predictor = OptimizedPredictor::new(sig, vec![]);

        let response = "This is a response without structured fields";
        let outputs = predictor.parse_output(response).unwrap();

        assert_eq!(outputs.len(), 1);
        assert!(outputs.contains_key("answer"));
    }

    // ===== Integration Tests =====

    #[test]
    fn test_demonstrations_getter() {
        let sig = create_test_signature();

        let mut inputs = HashMap::new();
        inputs.insert("question".to_string(), Value::String("Test".to_string()));

        let mut outputs = HashMap::new();
        outputs.insert("answer".to_string(), Value::String("Response".to_string()));

        let demo = Demonstration::new(inputs, outputs);
        let predictor = OptimizedPredictor::new(sig, vec![demo.clone()]);

        let demos = predictor.demonstrations();
        assert_eq!(demos.len(), 1);
    }

    #[test]
    fn test_metric_function_signature() {
        // Test that we can create a metric function with proper signature
        let metric: MetricFn = Arc::new(|example: &Example, output: &HashMap<String, Value>| {
            // Simple exact match metric
            if let (Some(expected), Some(actual)) =
                (example.outputs.get("answer"), output.get("answer")) {
                Ok(expected == actual)
            } else {
                Ok(false)
            }
        });

        let mut example_inputs = HashMap::new();
        example_inputs.insert("question".to_string(), Value::String("Test".to_string()));

        let mut example_outputs = HashMap::new();
        example_outputs.insert("answer".to_string(), Value::String("42".to_string()));

        let example = Example::new(example_inputs, example_outputs.clone());

        // Test metric with matching output
        let result = metric(&example, &example_outputs);
        assert!(result.is_ok());
        assert!(result.unwrap());

        // Test metric with non-matching output
        let mut different_outputs = HashMap::new();
        different_outputs.insert("answer".to_string(), Value::String("wrong".to_string()));

        let result = metric(&example, &different_outputs);
        assert!(result.is_ok());
        assert!(!result.unwrap());
    }
}
