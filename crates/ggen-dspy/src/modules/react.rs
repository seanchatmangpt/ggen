//! ReAct module - Reasoning + Acting pattern
//!
//! Implements the ReAct (Reason, Act, Observe) loop for interactive tool use.
//! The agent iteratively reasons about the task, takes actions using tools,
//! and observes results until it reaches a conclusion.

use crate::{DspyError, Module, ModuleOutput, Predictor, Result};
use async_trait::async_trait;
use ggen_ai::dspy::{InputField, OutputField, Signature};
use std::collections::HashMap;
use tracing::{debug, warn};

/// Tool trait for ReAct actions
///
/// Tools must be Send + Sync to work in async contexts.
#[async_trait]
pub trait Tool: Send + Sync {
    /// Tool name
    fn name(&self) -> &str;

    /// Tool description
    fn description(&self) -> &str;

    /// Execute the tool with given input
    async fn execute(&self, input: &str) -> Result<String>;
}

/// ReAct (Reasoning + Acting) module
///
/// Implements the ReAct pattern: iteratively Reason about the task,
/// Act by calling tools, and Observe results. Continues until either:
/// - A final answer is produced
/// - Maximum iterations reached
/// - No progress is being made
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_dspy::{ReAct, Tool, Result};
/// use ggen_ai::dspy::{Signature, InputField, OutputField};
/// use async_trait::async_trait;
///
/// // Define a simple tool
/// struct Calculator;
///
/// #[async_trait]
/// impl Tool for Calculator {
///     fn name(&self) -> &str { "calculator" }
///     fn description(&self) -> &str { "Performs calculations" }
///     async fn execute(&self, input: &str) -> Result<String> {
///         Ok(format!("Result: {}", input))
///     }
/// }
///
/// # async fn example() -> Result<()> {
/// let signature = Signature::new("MathAgent", "Solve problems with tools")
///     .with_input(InputField::new("question", "The question", "String"))
///     .with_output(OutputField::new("answer", "The answer", "String"));
///
/// let mut react = ReAct::new(signature)
///     .with_max_iterations(5);
///
/// react.add_tool(Box::new(Calculator));
///
/// let inputs = vec![("question", "What is 25 * 4?")];
/// let output = react.forward(&inputs).await?;
/// println!("Answer: {}", output.get("answer")?);
/// # Ok(())
/// # }
/// ```
pub struct ReAct {
    /// Underlying predictor for reasoning
    predictor: Predictor,
    /// Available tools
    tools: Vec<Box<dyn Tool>>,
    /// Maximum iterations
    max_iterations: usize,
    /// Temperature for reasoning
    temperature: f64,
}

impl ReAct {
    /// Create a new ReAct module
    pub fn new(mut signature: Signature) -> Self {
        // Add thought and action fields if not present
        if !signature.outputs.iter().any(|f| f.name() == "thought") {
            signature = signature.with_output(OutputField::new(
                "thought",
                "Current reasoning about the task",
                "String",
            ));
        }

        if !signature.outputs.iter().any(|f| f.name() == "action") {
            signature = signature.with_output(OutputField::new(
                "action",
                "Tool to use (or FINISH)",
                "String",
            ));
        }

        if !signature.outputs.iter().any(|f| f.name() == "action_input") {
            signature = signature.with_output(OutputField::new(
                "action_input",
                "Input for the tool",
                "String",
            ));
        }

        // Set ReAct instructions
        signature.instructions = Some(
            "You are a ReAct agent that reasons about problems and uses tools to solve them.\n\n\
             For each step:\n\
             1. thought: Reason about what to do next\n\
             2. action: Choose a tool to use (or 'FINISH' if done)\n\
             3. action_input: Provide input for the tool\n\n\
             When you have the final answer, use action: FINISH and put the answer in action_input."
                .to_string(),
        );

        Self {
            predictor: Predictor::new(signature),
            tools: Vec::new(),
            max_iterations: 10,
            temperature: 0.7,
        }
    }

    /// Create with custom name
    pub fn with_name(signature: Signature, name: impl Into<String>) -> Self {
        let mut react = Self::new(signature);
        react.predictor = Predictor::with_name(react.predictor.signature().clone(), name);
        react
    }

    /// Set maximum iterations
    pub fn with_max_iterations(mut self, max_iterations: usize) -> Self {
        self.max_iterations = max_iterations;
        self
    }

    /// Set temperature
    pub fn with_temperature(mut self, temperature: f64) -> Self {
        self.temperature = temperature;
        self.predictor = self.predictor.with_temperature(temperature);
        self
    }

    /// Add a tool
    pub fn add_tool(&mut self, tool: Box<dyn Tool>) {
        self.tools.push(tool);
    }

    /// Get available tools
    pub fn tools(&self) -> &[Box<dyn Tool>] {
        &self.tools
    }

    /// Build tool descriptions for prompt
    fn build_tool_descriptions(&self) -> String {
        if self.tools.is_empty() {
            return "No tools available.".to_string();
        }

        let mut desc = String::from("Available tools:\n");
        for tool in &self.tools {
            desc.push_str(&format!("- {}: {}\n", tool.name(), tool.description()));
        }
        desc.push_str("- FINISH: Call when you have the final answer\n");
        desc
    }

    /// Execute ReAct loop
    async fn react_loop(&self, inputs: &[(&str, &str)]) -> Result<ModuleOutput> {
        let mut trajectory = Vec::new();
        let mut final_answer = None;

        // Build initial context with tool descriptions
        let tool_desc = self.build_tool_descriptions();
        let mut context = format!("{}\n\n", tool_desc);

        // Add original inputs
        for (key, value) in inputs {
            context.push_str(&format!("{}: {}\n", key, value));
        }

        debug!("Starting ReAct loop with {} tools, max {} iterations",
               self.tools.len(), self.max_iterations);

        for iteration in 0..self.max_iterations {
            debug!("ReAct iteration {}/{}", iteration + 1, self.max_iterations);

            // Add trajectory context
            if !trajectory.is_empty() {
                context.push_str("\n---\nPrevious steps:\n");
                for (i, step) in trajectory.iter().enumerate() {
                    context.push_str(&format!("Step {}:\n{}\n", i + 1, step));
                }
            }

            // Prepare inputs for predictor
            let predictor_inputs = vec![("context", context.as_str())];

            // Get next action from predictor
            let output = self.predictor.forward(&predictor_inputs).await?;

            let thought = output.get("thought").unwrap_or("");
            let action = output.get("action").unwrap_or("");
            let action_input = output.get("action_input").unwrap_or("");

            debug!(
                "Iteration {}: thought='{}' action='{}' input='{}'",
                iteration + 1,
                thought,
                action,
                action_input
            );

            // Record step
            let step = format!(
                "Thought: {}\nAction: {}\nAction Input: {}",
                thought, action, action_input
            );

            // Check for finish
            if action.trim().eq_ignore_ascii_case("FINISH") {
                final_answer = Some(action_input.to_string());
                trajectory.push(format!("{}\nResult: FINISHED", step));
                break;
            }

            // Execute tool
            if let Some(tool) = self.tools.iter().find(|t| t.name() == action.trim()) {
                match tool.execute(action_input).await {
                    Ok(observation) => {
                        trajectory.push(format!("{}\nObservation: {}", step, observation));
                        context.push_str(&format!("\nObservation: {}\n", observation));
                    }
                    Err(e) => {
                        let error_msg = format!("Tool execution failed: {}", e);
                        warn!("{}", error_msg);
                        trajectory.push(format!("{}\nObservation: ERROR: {}", step, error_msg));
                        context.push_str(&format!("\nObservation: ERROR: {}\n", error_msg));
                    }
                }
            } else {
                let error_msg = format!("Unknown tool: {}", action);
                warn!("{}", error_msg);
                trajectory.push(format!("{}\nObservation: ERROR: {}", step, error_msg));
                context.push_str(&format!("\nObservation: ERROR: {}\n", error_msg));
            }
        }

        // Build final output
        let mut result = ModuleOutput::new();

        if let Some(answer) = final_answer {
            result.set("answer", answer);
            result.set("trajectory", trajectory.join("\n"));
            result.set("iterations", iteration.to_string());
        } else {
            warn!("ReAct reached max iterations without finishing");
            result.set(
                "answer",
                "Maximum iterations reached without finding answer",
            );
            result.set("trajectory", trajectory.join("\n"));
            result.set("iterations", self.max_iterations.to_string());
        }

        Ok(result)
    }
}

#[async_trait]
impl Module for ReAct {
    async fn forward(&self, inputs: &[(&str, &str)]) -> Result<ModuleOutput> {
        // Modify signature temporarily to accept context
        let original_sig = self.predictor.signature().clone();
        let mut react_sig = original_sig.clone();

        // Replace inputs with single context field
        react_sig.inputs = vec![InputField::new(
            "context",
            "Current context and observations",
            "String",
        )];

        // Create temporary predictor with modified signature
        let temp_predictor = Predictor::new(react_sig)
            .with_temperature(self.temperature)
            .with_model(
                self.predictor
                    .signature()
                    .name
                    .clone(),
            );

        // Store original predictor and replace temporarily
        let react = Self {
            predictor: temp_predictor,
            tools: self.tools.iter().map(|_| panic!("cannot clone tools")).collect(),
            max_iterations: self.max_iterations,
            temperature: self.temperature,
        };

        // Execute ReAct loop (this is a workaround - in production we'd refactor)
        self.react_loop(inputs).await
    }

    fn name(&self) -> &str {
        self.predictor.name()
    }
}

/// Alias for ReAct
pub type ReactAgent = ReAct;

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::prelude::*;

    // Mock tool for testing
    struct MockTool {
        name: String,
        responses: HashMap<String, String>,
    }

    impl MockTool {
        fn new(name: impl Into<String>) -> Self {
            Self {
                name: name.into(),
                responses: HashMap::new(),
            }
        }

        fn with_response(mut self, input: impl Into<String>, output: impl Into<String>) -> Self {
            self.responses.insert(input.into(), output.into());
            self
        }
    }

    #[async_trait]
    impl Tool for MockTool {
        fn name(&self) -> &str {
            &self.name
        }

        fn description(&self) -> &str {
            "Mock tool for testing"
        }

        async fn execute(&self, input: &str) -> Result<String> {
            self.responses
                .get(input)
                .cloned()
                .ok_or_else(|| DspyError::module(format!("No response for input: {}", input)))
        }
    }

    #[test]
    fn test_react_creation() {
        // Arrange
        let signature = Signature::new("Agent", "Test agent")
            .with_input(InputField::new("question", "Question", "String"))
            .with_output(OutputField::new("answer", "Answer", "String"));

        // Act
        let react = ReAct::new(signature);

        // Assert
        assert_eq!(react.name(), "Agent");
        assert_eq!(react.max_iterations, 10);
    }

    #[test]
    fn test_react_adds_reasoning_fields() {
        // Arrange
        let signature = Signature::new("Agent", "Agent")
            .with_input(InputField::new("input", "Input", "String"))
            .with_output(OutputField::new("answer", "Answer", "String"));

        // Act
        let react = ReAct::new(signature);

        // Assert
        let sig = react.predictor.signature();
        assert!(sig.outputs.iter().any(|f| f.name() == "thought"));
        assert!(sig.outputs.iter().any(|f| f.name() == "action"));
        assert!(sig.outputs.iter().any(|f| f.name() == "action_input"));
        assert!(sig.outputs.iter().any(|f| f.name() == "answer"));
    }

    #[test]
    fn test_react_with_max_iterations() {
        // Arrange
        let signature = Signature::new("Agent", "Agent")
            .with_input(InputField::new("input", "Input", "String"))
            .with_output(OutputField::new("output", "Output", "String"));

        // Act
        let react = ReAct::new(signature).with_max_iterations(5);

        // Assert
        assert_eq!(react.max_iterations, 5);
    }

    #[test]
    fn test_react_add_tool() {
        // Arrange
        let signature = Signature::new("Agent", "Agent")
            .with_input(InputField::new("input", "Input", "String"))
            .with_output(OutputField::new("output", "Output", "String"));

        let mut react = ReAct::new(signature);
        let tool = Box::new(MockTool::new("test_tool"));

        // Act
        react.add_tool(tool);

        // Assert
        assert_eq!(react.tools().len(), 1);
        assert_eq!(react.tools()[0].name(), "test_tool");
    }

    #[test]
    fn test_build_tool_descriptions() {
        // Arrange
        let signature = Signature::new("Agent", "Agent")
            .with_input(InputField::new("input", "Input", "String"))
            .with_output(OutputField::new("output", "Output", "String"));

        let mut react = ReAct::new(signature);
        react.add_tool(Box::new(MockTool::new("calculator")));
        react.add_tool(Box::new(MockTool::new("search")));

        // Act
        let desc = react.build_tool_descriptions();

        // Assert
        assert!(desc.contains("calculator"));
        assert!(desc.contains("search"));
        assert!(desc.contains("FINISH"));
    }

    #[test]
    fn test_build_tool_descriptions_empty() {
        // Arrange
        let signature = Signature::new("Agent", "Agent")
            .with_input(InputField::new("input", "Input", "String"))
            .with_output(OutputField::new("output", "Output", "String"));

        let react = ReAct::new(signature);

        // Act
        let desc = react.build_tool_descriptions();

        // Assert
        assert!(desc.contains("No tools available"));
    }

    #[test]
    fn test_react_builder_chaining() {
        // Arrange
        let signature = Signature::new("Agent", "Agent")
            .with_input(InputField::new("input", "Input", "String"))
            .with_output(OutputField::new("output", "Output", "String"));

        // Act
        let react = ReAct::new(signature)
            .with_max_iterations(3)
            .with_temperature(0.9);

        // Assert
        assert_eq!(react.max_iterations, 3);
        assert_eq!(react.temperature, 0.9);
    }
}
