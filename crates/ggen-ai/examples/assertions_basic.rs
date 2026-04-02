//! Basic DSPy assertion example
//!
//! Demonstrates how to use the DSPy assertion system to validate module outputs
//! with automatic retry and self-correction.
//!
//! Run with: cargo run --example assertions_basic

use ggen_ai::dspy::{
    assertions::{
        AllValidator, AssertableModule, Assertion, BacktrackExecutor, BoxedValidator,
        ContainsValidator, LengthValidator, NotEmptyValidator,
    },
    field::{InputField, OutputField},
    Module, ModuleError, Signature,
};
use serde_json::Value;
use std::collections::HashMap;

/// Simple mock module that simulates improving responses on retry
struct QAModule {
    sig: Signature,
    attempt_count: std::sync::Arc<std::sync::Mutex<usize>>,
}

impl QAModule {
    fn new() -> Self {
        let sig = Signature::new("QA", "Question Answering")
            .with_input(InputField::new("question", "Question to answer", "String"))
            .with_output(OutputField::new("answer", "Generated answer", "String"));

        Self {
            sig,
            attempt_count: std::sync::Arc::new(std::sync::Mutex::new(0)),
        }
    }

    fn get_response(&self, attempt: usize) -> &'static str {
        match attempt {
            0 => "Rust",  // Too short - will fail length validation
            1 => "Rust is a programming language",  // Better, but might still fail detailed check
            _ => "Rust is a systems programming language focused on safety and performance. It provides memory safety without garbage collection.",
        }
    }
}

#[async_trait::async_trait]
impl Module for QAModule {
    fn signature(&self) -> &Signature {
        &self.sig
    }

    async fn forward(
        &self, inputs: HashMap<String, Value>,
    ) -> Result<HashMap<String, Value>, ModuleError> {
        let mut count = self.attempt_count.lock().unwrap();
        let attempt = *count;
        *count += 1;

        let _question = inputs
            .get("question")
            .and_then(|v| v.as_str())
            .unwrap_or("Unknown question");

        println!("  Attempt {}: Generating answer...", attempt + 1);

        let answer = self.get_response(attempt);

        let mut outputs = HashMap::new();
        outputs.insert("answer".to_string(), Value::String(answer.to_string()));
        Ok(outputs)
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("\n=== DSPy Assertion System - Basic Example ===\n");

    // Example 1: Single assertion (hard requirement)
    println!("1. Single Hard Assertion (minimum length):");
    println!("   Question: What is Rust?");
    {
        let module = QAModule::new();

        let assertion = Assertion::assert(LengthValidator::min(50))
            .with_feedback("Answer must be detailed (at least 50 characters)")
            .max_retries(3);

        let asserted_module = module.with_assertion(assertion);

        let mut inputs = HashMap::new();
        inputs.insert(
            "question".to_string(),
            Value::String("What is Rust?".to_string()),
        );

        match asserted_module.forward(inputs).await {
            Ok(output) => {
                let answer = output.get("answer").unwrap();
                println!("   Success! Answer: {}", answer);
            }
            Err(e) => {
                println!("   Failed: {}", e);
            }
        }
    }

    println!("\n2. Multiple Assertions (combined requirements):");
    println!("   Question: Explain Rust ownership");
    {
        let module = QAModule::new();

        // Combine multiple validators with AllValidator
        let validators: Vec<BoxedValidator> = vec![
            Box::new(NotEmptyValidator),
            Box::new(LengthValidator::between(30, 500)),
            Box::new(ContainsValidator::case_insensitive("safety")),
        ];

        let combined_validator = AllValidator::new(validators);

        let assertion = Assertion::assert(combined_validator)
            .with_feedback("Answer must be non-empty, 30-500 chars, and mention 'safety'")
            .max_retries(4);

        let asserted_module = module.with_assertion(assertion);

        let mut inputs = HashMap::new();
        inputs.insert(
            "question".to_string(),
            Value::String("Explain Rust ownership".to_string()),
        );

        match asserted_module.forward(inputs).await {
            Ok(output) => {
                let answer = output.get("answer").unwrap();
                println!("   Success! Answer: {}", answer);
            }
            Err(e) => {
                println!("   Failed: {}", e);
            }
        }
    }

    println!("\n3. Soft Suggestion (warns but doesn't fail):");
    println!("   Question: What is Rust? (with preference for detailed answers)");
    {
        let module = QAModule::new();

        // Suggestion doesn't fail execution, just warns
        let suggestion = Assertion::suggest(LengthValidator::min(100))
            .with_feedback("Ideally, answer should be very detailed (100+ characters)")
            .max_retries(2);

        let asserted_module = module.with_assertion(suggestion);

        let mut inputs = HashMap::new();
        inputs.insert(
            "question".to_string(),
            Value::String("What is Rust?".to_string()),
        );

        match asserted_module.forward(inputs).await {
            Ok(output) => {
                let answer = output.get("answer").unwrap();
                println!("   Success (with warnings): Answer: {}", answer);
            }
            Err(e) => {
                println!("   Failed: {}", e);
            }
        }
    }

    println!("\n4. BacktrackExecutor with multiple assertions:");
    {
        let module = QAModule::new();

        let assertions = vec![
            // Hard requirement
            Assertion::assert(NotEmptyValidator)
                .with_feedback("Answer cannot be empty")
                .max_retries(3),
            // Soft preference
            Assertion::suggest(LengthValidator::min(80))
                .with_feedback("Answer should be comprehensive")
                .max_retries(2),
        ];

        let mut executor = BacktrackExecutor::new(assertions);

        let mut inputs = HashMap::new();
        inputs.insert(
            "question".to_string(),
            Value::String("Explain Rust".to_string()),
        );

        match executor.execute(&module, inputs).await {
            Ok(output) => {
                let answer = output.get("answer").unwrap();
                println!("   Success! Answer: {}", answer);
                println!("   Warnings: {}", executor.warnings().len());
            }
            Err(e) => {
                println!("   Failed: {}", e);
            }
        }
    }

    println!("\n=== Example Complete ===\n");
    Ok(())
}
