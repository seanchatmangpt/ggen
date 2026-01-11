//! Basic Predictor example
//!
//! Demonstrates the most basic DSPy module - the Predictor.
//! Run with: cargo run --example basic_predictor

use ggen_dspy::{Predictor, Result};
use ggen_ai::dspy::{InputField, OutputField, Signature};

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize logging
    ggen_dspy::init_logging();

    // Define a simple QA signature
    let signature = Signature::new("QuestionAnswering", "Answer questions accurately")
        .with_input(InputField::new(
            "question",
            "The question to answer",
            "String",
        ))
        .with_output(OutputField::new(
            "answer",
            "The answer to the question",
            "String",
        ));

    // Create predictor with configuration
    let predictor = Predictor::new(signature)
        .with_temperature(0.7)
        .with_max_tokens(512)
        .with_model("gpt-3.5-turbo");

    // Example questions
    let questions = vec![
        "What is Rust programming language?",
        "What are the benefits of using DSPy?",
        "How does type safety help prevent bugs?",
    ];

    for question in questions {
        println!("\n--- Question: {} ---", question);

        let inputs = vec![("question", question)];

        match predictor.forward(&inputs).await {
            Ok(output) => {
                println!("Answer: {}", output.get("answer")?);
            }
            Err(e) => {
                eprintln!("Error: {}", e);
            }
        }
    }

    Ok(())
}
