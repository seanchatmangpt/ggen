//! Chain of Thought example
//!
//! Demonstrates reasoning before answering.
//! Run with: cargo run --example chain_of_thought

use ggen_ai::dspy::{InputField, OutputField, Signature};
use ggen_dspy::{ChainOfThought, Result};

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize logging
    ggen_dspy::init_logging();

    // Define a signature for complex reasoning tasks
    let signature = Signature::new("MathReasoning", "Solve math problems with reasoning")
        .with_input(InputField::new(
            "problem",
            "The math problem to solve",
            "String",
        ))
        .with_output(OutputField::new("answer", "The final answer", "String"));

    // Create Chain of Thought module
    let cot = ChainOfThought::new(signature)
        .with_temperature(0.7)
        .with_max_tokens(1024);

    // Math problems that benefit from step-by-step reasoning
    let problems = vec![
        "If a train travels at 60 mph for 2.5 hours, how far does it travel?",
        "What is 15% of 80, then add 12?",
        "If 3 apples cost $2, how much do 7 apples cost?",
    ];

    for problem in problems {
        println!("\n--- Problem: {} ---", problem);

        let inputs = vec![("problem", problem)];

        match cot.forward(&inputs).await {
            Ok(output) => {
                // ChainOfThought provides both reasoning and answer
                if let Ok(rationale) = output.get("rationale") {
                    println!("Reasoning: {}", rationale);
                }
                println!("Answer: {}", output.get("answer")?);
            }
            Err(e) => {
                eprintln!("Error: {}", e);
            }
        }
    }

    Ok(())
}
