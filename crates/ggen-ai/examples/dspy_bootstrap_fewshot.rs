//! Example: DSPy BootstrapFewShot Optimizer
//!
//! Demonstrates how to use the BootstrapFewShot optimizer to improve
//! a predictor's performance through few-shot learning.
//!
//! Run with: cargo run --example dspy_bootstrap_fewshot

use ggen_ai::dspy::{
    field::{InputField, OutputField},
    module::Module,
    optimizer::{BootstrapFewShot, Example},
    predictor::Predictor,
    signature::Signature,
};
use serde_json::{json, Value};
use std::collections::HashMap;
use std::sync::Arc;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing
    tracing_subscriber::fmt::init();

    println!("=== DSPy BootstrapFewShot Optimizer Example ===\n");

    // Step 1: Define the signature (task interface)
    let signature = Signature::new("QuestionAnswering", "Answer simple math questions")
        .with_input(InputField::new("question", "Math question", "String"))
        .with_output(OutputField::new(
            "answer",
            "Answer to the question",
            "String",
        ))
        .with_instructions("Answer the math question accurately.");

    println!("Signature: {}", signature.name);
    println!("Description: {}\n", signature.description);

    // Step 2: Create training examples
    let trainset = vec![
        Example::new(
            {
                let mut inputs = HashMap::new();
                inputs.insert("question".to_string(), json!("What is 2 + 2?"));
                inputs
            },
            {
                let mut outputs = HashMap::new();
                outputs.insert("answer".to_string(), json!("4"));
                outputs
            },
        ),
        Example::new(
            {
                let mut inputs = HashMap::new();
                inputs.insert("question".to_string(), json!("What is 5 * 3?"));
                inputs
            },
            {
                let mut outputs = HashMap::new();
                outputs.insert("answer".to_string(), json!("15"));
                outputs
            },
        ),
        Example::new(
            {
                let mut inputs = HashMap::new();
                inputs.insert("question".to_string(), json!("What is 10 - 3?"));
                inputs
            },
            {
                let mut outputs = HashMap::new();
                outputs.insert("answer".to_string(), json!("7"));
                outputs
            },
        ),
    ];

    println!("Created {} training examples\n", trainset.len());

    // Step 3: Define a metric function
    // This checks if the model's answer matches the expected answer
    let metric = Arc::new(|example: &Example, output: &HashMap<String, Value>| {
        let expected = example.outputs.get("answer");
        let actual = output.get("answer");

        match (expected, actual) {
            (Some(exp), Some(act)) => {
                let matches = exp.as_str() == act.as_str();
                println!(
                    "  Metric: expected={}, actual={}, match={}",
                    exp.as_str().unwrap_or("?"),
                    act.as_str().unwrap_or("?"),
                    matches
                );
                Ok(matches)
            }
            _ => Ok(false),
        }
    });

    println!("Defined metric function\n");

    // Step 4: Create the student predictor
    // Note: Set GGEN_LLM_MODEL environment variable to use an actual model
    let student = Predictor::new(signature.clone());
    println!("Created student predictor\n");

    // Step 5: Create and configure the optimizer
    let optimizer = BootstrapFewShot::new(metric)
        .with_max_bootstrapped_demos(3)
        .with_max_labeled_demos(10);

    println!("Created BootstrapFewShot optimizer");
    println!("  Max bootstrapped demos: 3");
    println!("  Max labeled demos: 10\n");

    // Step 6: Compile/optimize the student
    println!("Starting compilation (bootstrapping demonstrations)...\n");

    match optimizer.compile(&student, &trainset).await {
        Ok(optimized) => {
            println!("\n=== Optimization Complete ===");
            println!(
                "Collected {} demonstrations\n",
                optimized.demonstration_count()
            );

            // Step 7: Use the optimized predictor
            println!("Testing optimized predictor...\n");

            let test_input = {
                let mut inputs = HashMap::new();
                inputs.insert("question".to_string(), json!("What is 6 + 7?"));
                inputs
            };

            println!("Test question: What is 6 + 7?");

            // Note: This will fail if GGEN_LLM_MODEL is not set
            match optimized.forward(test_input).await {
                Ok(output) => {
                    println!(
                        "Answer: {}",
                        output
                            .get("answer")
                            .and_then(|v| v.as_str())
                            .unwrap_or("No answer")
                    );
                }
                Err(e) => {
                    println!("Error during inference: {}", e);
                    println!(
                        "\nNote: Set GGEN_LLM_MODEL environment variable to test with a real model"
                    );
                    println!("Example: export GGEN_LLM_MODEL=gpt-4");
                }
            }

            println!("\n=== Example Complete ===");
            println!(
                "The optimized predictor includes {} few-shot examples in its prompt",
                optimized.demonstration_count()
            );
        }
        Err(e) => {
            eprintln!("Error during optimization: {}", e);
            eprintln!("\nNote: Set GGEN_LLM_MODEL environment variable to test with a real model");
        }
    }

    Ok(())
}
