//! Chain of Thought module - adds reasoning step before final answer
//!
//! Extends Predictor with explicit reasoning, asking the LLM to think
//! step-by-step before providing the final answer.

use crate::{Module, ModuleOutput, Predictor, Result};
use async_trait::async_trait;
use ggen_ai::dspy::{OutputField, Signature};
use tracing::debug;

/// Chain of Thought module
///
/// Enhances Predictor by adding a "rationale" output field that captures
/// the LLM's step-by-step reasoning process before generating the final answer.
/// This often improves accuracy on complex reasoning tasks.
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_dspy::{ChainOfThought, Result};
/// use ggen_ai::dspy::{Signature, InputField, OutputField};
///
/// # async fn example() -> Result<()> {
/// let signature = Signature::new("MathSolver", "Solve math problems")
///     .with_input(InputField::new("problem", "The math problem", "String"))
///     .with_output(OutputField::new("answer", "The solution", "String"));
///
/// let cot = ChainOfThought::new(signature)
///     .with_temperature(0.7);
///
/// let inputs = vec![("problem", "What is 15% of 80?")];
/// let output = cot.forward(&inputs).await?;
///
/// // Access both reasoning and answer
/// println!("Reasoning: {}", output.get("rationale")?);
/// println!("Answer: {}", output.get("answer")?);
/// # Ok(())
/// # }
/// ```
pub struct ChainOfThought {
    /// Underlying predictor
    predictor: Predictor,
}

impl ChainOfThought {
    /// Create a new Chain of Thought module from a signature
    ///
    /// Automatically adds a "rationale" output field for reasoning
    /// and updates instructions to encourage step-by-step thinking.
    pub fn new(mut signature: Signature) -> Self {
        // Add rationale field if not present
        if !signature.outputs.iter().any(|f| f.name() == "rationale") {
            signature = signature.with_output(OutputField::new(
                "rationale",
                "Step-by-step reasoning process",
                "String",
            ));
        }

        // Set CoT instructions
        signature.instructions = Some(
            "Think through this step-by-step:\n\
             1. Break down the problem\n\
             2. Consider relevant information\n\
             3. Reason through the solution\n\
             4. Provide your final answer\n\n\
             First, provide your reasoning in the 'rationale' field, then your answer."
                .to_string(),
        );

        Self {
            predictor: Predictor::new(signature),
        }
    }

    /// Create with custom name
    pub fn with_name(signature: Signature, name: impl Into<String>) -> Self {
        let mut cot = Self::new(signature);
        cot.predictor = Predictor::with_name(cot.predictor.signature().clone(), name);
        cot
    }

    /// Set temperature for LLM sampling
    pub fn with_temperature(mut self, temperature: f64) -> Self {
        self.predictor = self.predictor.with_temperature(temperature);
        self
    }

    /// Set maximum tokens to generate
    pub fn with_max_tokens(mut self, max_tokens: usize) -> Self {
        self.predictor = self.predictor.with_max_tokens(max_tokens);
        self
    }

    /// Set specific model name
    pub fn with_model(mut self, model: impl Into<String>) -> Self {
        self.predictor = self.predictor.with_model(model);
        self
    }

    /// Get the signature
    pub fn signature(&self) -> &Signature {
        self.predictor.signature()
    }

    /// Get reference to underlying predictor
    pub fn predictor(&self) -> &Predictor {
        &self.predictor
    }
}

#[async_trait]
impl Module for ChainOfThought {
    async fn forward(&self, inputs: &[(&str, &str)]) -> Result<ModuleOutput> {
        debug!("ChainOfThought module '{}' executing", self.name());

        // Delegate to predictor which handles the CoT prompting
        self.predictor.forward(inputs).await
    }

    fn name(&self) -> &str {
        self.predictor.name()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ggen_ai::dspy::InputField;

    #[test]
    fn test_cot_creation() {
        // Arrange
        let signature = Signature::new("MathSolver", "Solve math problems")
            .with_input(InputField::new("problem", "Math problem", "String"))
            .with_output(OutputField::new("answer", "Solution", "String"));

        // Act
        let cot = ChainOfThought::new(signature);

        // Assert
        assert_eq!(cot.name(), "MathSolver");
    }

    #[test]
    fn test_cot_adds_rationale_field() {
        // Arrange
        let signature = Signature::new("Reasoning", "Reasoning task")
            .with_input(InputField::new("question", "Question", "String"))
            .with_output(OutputField::new("answer", "Answer", "String"));

        // Act
        let cot = ChainOfThought::new(signature);

        // Assert
        let sig = cot.signature();
        assert!(sig.outputs.iter().any(|f| f.name() == "rationale"));
        assert!(sig.outputs.iter().any(|f| f.name() == "answer"));
    }

    #[test]
    fn test_cot_preserves_existing_rationale() {
        // Arrange
        let signature = Signature::new("Reasoning", "Reasoning task")
            .with_input(InputField::new("question", "Question", "String"))
            .with_output(OutputField::new("rationale", "Custom reasoning", "String"))
            .with_output(OutputField::new("answer", "Answer", "String"));

        // Act
        let cot = ChainOfThought::new(signature);

        // Assert
        let sig = cot.signature();
        // Should not duplicate rationale field
        assert_eq!(
            sig.outputs.iter().filter(|f| f.name() == "rationale").count(),
            1
        );
    }

    #[test]
    fn test_cot_sets_instructions() {
        // Arrange
        let signature = Signature::new("Test", "Test")
            .with_input(InputField::new("input", "Input", "String"))
            .with_output(OutputField::new("output", "Output", "String"));

        // Act
        let cot = ChainOfThought::new(signature);

        // Assert
        let sig = cot.signature();
        assert!(sig.instructions.is_some());
        let instructions = sig.instructions.as_ref().unwrap();
        assert!(instructions.contains("step-by-step"));
        assert!(instructions.contains("rationale"));
    }

    #[test]
    fn test_cot_with_custom_name() {
        // Arrange
        let signature = Signature::new("Original", "Original")
            .with_input(InputField::new("input", "Input", "String"))
            .with_output(OutputField::new("output", "Output", "String"));

        // Act
        let cot = ChainOfThought::with_name(signature, "CustomCoT");

        // Assert
        assert_eq!(cot.name(), "CustomCoT");
    }

    #[test]
    fn test_cot_with_temperature() {
        // Arrange
        let signature = Signature::new("Test", "Test")
            .with_input(InputField::new("input", "Input", "String"))
            .with_output(OutputField::new("output", "Output", "String"));

        // Act
        let cot = ChainOfThought::new(signature).with_temperature(0.5);

        // Assert
        // Access via predictor()
        assert_eq!(cot.predictor().signature().name, "Test");
    }

    #[test]
    fn test_cot_builder_chaining() {
        // Arrange
        let signature = Signature::new("Test", "Test")
            .with_input(InputField::new("input", "Input", "String"))
            .with_output(OutputField::new("output", "Output", "String"));

        // Act
        let cot = ChainOfThought::new(signature)
            .with_temperature(0.8)
            .with_max_tokens(2048)
            .with_model("gpt-4");

        // Assert
        assert!(cot.predictor().signature().outputs.len() >= 2); // At least rationale + output
    }

    #[test]
    fn test_cot_output_field_order() {
        // Arrange
        let signature = Signature::new("Test", "Test")
            .with_input(InputField::new("input", "Input", "String"))
            .with_output(OutputField::new("answer", "Answer", "String"));

        // Act
        let cot = ChainOfThought::new(signature);

        // Assert - rationale should be added (order may vary but both should exist)
        let sig = cot.signature();
        let output_names: Vec<&str> = sig.outputs.iter().map(|f| f.name()).collect();
        assert!(output_names.contains(&"rationale"));
        assert!(output_names.contains(&"answer"));
    }
}
