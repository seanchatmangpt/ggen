//! Example: Basic Question Answering with Predict and ChainOfThought
//!
//! Demonstrates the two fundamental DSPy modules:
//! - `Predictor`: Basic prediction with LLM
//! - `ChainOfThought`: Prediction with step-by-step reasoning
//!
//! Run with: cargo run --example basic_qa
//!
//! Prerequisites:
//! - Set GGEN_LLM_MODEL environment variable (e.g., gpt-4, claude-3-opus-20240229)
//! - Set appropriate API key (OPENAI_API_KEY, ANTHROPIC_API_KEY, etc.)

use ggen_ai::dspy::{
    field::{InputField, OutputField},
    module::Module,
    predictor::{ChainOfThought, Predictor},
    signature::Signature,
};
use serde_json::json;
use std::collections::HashMap;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing for observability
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    println!("=== Basic Question Answering Example ===\n");

    // Step 1: Define the signature (task interface)
    let signature = Signature::new(
        "QuestionAnswering",
        "Answer questions accurately and concisely",
    )
    .with_input(InputField::new(
        "question",
        "The question to answer",
        "String",
    ))
    .with_output(OutputField::new(
        "answer",
        "A clear, accurate answer",
        "String",
    ))
    .with_instructions(
        "Provide accurate, concise answers to questions. \
         Focus on clarity and correctness.",
    );

    println!("Defined signature: {}", signature.name);
    println!("Description: {}\n", signature.description);

    // Step 2: Test with basic Predictor
    println!("--- Part 1: Basic Predictor ---\n");

    let predictor = Predictor::new(signature.clone());

    let test_questions = vec![
        "What is the capital of France?",
        "How many planets are in our solar system?",
        "Who wrote 'To Kill a Mockingbird'?",
    ];

    for question in &test_questions {
        println!("Question: {}", question);

        let mut inputs = HashMap::new();
        inputs.insert("question".to_string(), json!(question));

        match predictor.forward(inputs).await {
            Ok(result) => {
                let answer = result
                    .get("answer")
                    .and_then(|v| v.as_str())
                    .unwrap_or("No answer provided");
                println!("Answer: {}\n", answer);
            }
            Err(e) => {
                eprintln!("Error: {}\n", e);
                if std::env::var("GGEN_LLM_MODEL").is_err() {
                    eprintln!("Note: Set GGEN_LLM_MODEL environment variable");
                    eprintln!("Example: export GGEN_LLM_MODEL=gpt-4\n");
                    break;
                }
            }
        }
    }

    // Step 3: Test with ChainOfThought (adds reasoning)
    println!("--- Part 2: Chain of Thought ---\n");

    // Create new signature with reasoning field
    let cot_signature = Signature::new(
        "QuestionAnsweringWithReasoning",
        "Answer questions with step-by-step reasoning",
    )
    .with_input(InputField::new(
        "question",
        "The question to answer",
        "String",
    ))
    .with_output(OutputField::new(
        "reasoning",
        "Step-by-step thought process",
        "String",
    ))
    .with_output(OutputField::new("answer", "The final answer", "String"))
    .with_instructions(
        "Think step-by-step to arrive at the correct answer. \
         Show your reasoning process before providing the final answer.",
    );

    let cot = ChainOfThought::new(cot_signature);

    // Test with questions that benefit from reasoning
    let reasoning_questions = vec![
        "If a train travels 60 miles per hour for 2.5 hours, how far does it go?",
        "Which is larger: 3/4 or 5/8?",
    ];

    for question in &reasoning_questions {
        println!("Question: {}", question);

        let mut inputs = HashMap::new();
        inputs.insert("question".to_string(), json!(question));

        match cot.forward(inputs).await {
            Ok(result) => {
                let reasoning = result
                    .get("reasoning")
                    .and_then(|v| v.as_str())
                    .unwrap_or("No reasoning provided");
                let answer = result
                    .get("answer")
                    .and_then(|v| v.as_str())
                    .unwrap_or("No answer provided");

                println!("Reasoning: {}", reasoning);
                println!("Answer: {}\n", answer);
            }
            Err(e) => {
                eprintln!("Error: {}\n", e);
            }
        }
    }

    println!("=== Example Complete ===");
    println!(
        "\nKey Differences:\n\
         - Predictor: Fast, single LLM call, direct answer\n\
         - ChainOfThought: Slower, shows reasoning, better for complex questions"
    );

    Ok(())
}
