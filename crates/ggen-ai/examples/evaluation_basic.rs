//! Example: Basic Evaluation Workflow
//!
//! Demonstrates the simplest evaluation pattern using the Evaluate struct.
//!
//! Run with: cargo run --example evaluation_basic --features live-llm-tests
//!
//! Prerequisites:
//! - Set GGEN_LLM_MODEL environment variable (e.g., "gpt-4")
//! - Set appropriate API key (OPENAI_API_KEY, ANTHROPIC_API_KEY, etc.)

use ggen_ai::dspy::{
    evaluation::{exact_match, Evaluate},
    Example, InputField, Module, ModuleError, OutputField, Predictor, Signature,
};
use serde_json::json;
use std::collections::HashMap;

// Mock module for testing without LLM
struct MockQA {
    sig: Signature,
}

#[async_trait::async_trait]
impl Module for MockQA {
    fn signature(&self) -> &Signature {
        &self.sig
    }

    async fn forward(
        &self,
        inputs: HashMap<String, serde_json::Value>,
    ) -> Result<HashMap<String, serde_json::Value>, ModuleError> {
        // Simple mock: just echo the question as the answer
        let mut outputs = HashMap::new();
        if let Some(question) = inputs.get("question") {
            outputs.insert("answer".to_string(), question.clone());
        } else {
            outputs.insert("answer".to_string(), json!("I don't know"));
        }
        Ok(outputs)
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    println!("=== Basic Evaluation Example ===\n");

    // Step 1: Create a signature
    let signature = Signature::new("QuestionAnswering", "Answer questions accurately")
        .with_input(InputField::new("question", "The question to answer", "String"))
        .with_output(OutputField::new("answer", "The answer", "String"));

    // Step 2: Create evaluation dataset
    let devset = vec![
        Example::new(
            [("question".to_string(), json!("What is 2+2?"))].into(),
            [("answer".to_string(), json!("4"))].into(),
        ),
        Example::new(
            [("question".to_string(), json!("What is the capital of France?"))].into(),
            [("answer".to_string(), json!("Paris"))].into(),
        ),
        Example::new(
            [("question".to_string(), json!("What color is the sky?"))].into(),
            [("answer".to_string(), json!("Blue"))].into(),
        ),
    ];

    println!("Created dataset with {} examples", devset.len());

    // Step 3: Create the program to evaluate
    // Using mock for demonstration (replace with Predictor::new(signature) for real LLM)
    let program = MockQA { sig: signature };

    // Step 4: Create evaluator with metric
    let metric = exact_match("answer");

    let evaluator = Evaluate::new(devset)
        .with_metric(metric)
        .with_display_progress(true)
        .with_display_table(3);

    println!("\nRunning evaluation...\n");

    // Step 5: Run evaluation
    let result = evaluator.evaluate(&program, None).await?;

    // Step 6: Display results
    println!("\n=== Results ===");
    println!("Overall Score: {:.2}%", result.score);
    println!("Successful: {}/{}", result.successful, result.results.len());
    println!("Failed: {}", result.failed);
    println!("Success Rate: {:.2}%", result.success_rate() * 100.0);
    println!("Elapsed Time: {:.2}s", result.elapsed.as_secs_f64());

    // Step 7: Analyze failed examples
    let failed = result.failed_examples();
    if !failed.is_empty() {
        println!("\nFailed Examples:");
        for (idx, point) in failed.iter().enumerate() {
            println!("  {}. Error: {:?}", idx + 1, point.error);
        }
    }

    println!("\n=== Example Complete ===");
    Ok(())
}
