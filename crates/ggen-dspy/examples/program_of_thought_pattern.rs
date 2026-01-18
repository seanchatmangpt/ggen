//! Example: Program of Thought pattern
//!
//! Demonstrates code generation and execution for computational problems.

use ggen_dspy::{Module, ProgramOfThought, ProgramOfThoughtConfig, CodeLanguage};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt::init();

    println!("=== Program of Thought Pattern Example ===\n");

    // Create PoT module with Python
    let config = ProgramOfThoughtConfig {
        language: CodeLanguage::Python,
        timeout_seconds: 5,
        enable_safety_checks: true,
        max_code_length: 10_000,
    };

    let pot = ProgramOfThought::new(config);

    // Example 1: Simple arithmetic
    println!("--- Example 1: Calculate sum of numbers ---\n");

    let problem1 = "Calculate the sum of numbers from 1 to 10";
    let inputs = vec![("problem", problem1)];
    let output = pot.forward(&inputs).await?;

    println!("Problem: {}", output.get("problem")?);
    println!("\nGenerated Code:");
    println!("{}", output.get("code")?);
    println!("\nExecution Result:");
    println!("Success: {}", output.get("success")?);
    println!("Exit Code: {}", output.get("exit_code")?);
    println!("Execution Time: {}ms", output.get("execution_time_ms")?);
    println!("\nOutput:");
    println!("{}", output.get("answer")?);

    // Example 2: Mathematical computation
    println!("\n\n--- Example 2: Calculate factorial ---\n");

    let problem2 = "Calculate factorial of 5";
    let inputs = vec![("problem", problem2)];
    let output = pot.forward(&inputs).await?;

    println!("Problem: {}", output.get("problem")?);
    println!("\nGenerated Code:");
    println!("{}", output.get("code")?);
    println!("\nAnswer: {}", output.get("answer")?);

    // Example 3: String processing
    println!("\n\n--- Example 3: String manipulation ---\n");

    let problem3 = "Reverse the string 'Hello World'";
    let inputs = vec![("problem", problem3)];
    let output = pot.forward(&inputs).await?;

    println!("Problem: {}", output.get("problem")?);
    println!("\nGenerated Code:");
    println!("{}", output.get("code")?);
    println!("\nAnswer: {}", output.get("answer")?);

    // Demonstrate safety check
    println!("\n\n--- Safety Check Demo ---");
    println!("PoT includes safety checks to prevent dangerous operations:");
    println!("- Code length limits");
    println!("- Dangerous pattern detection (os.system, eval, etc.)");
    println!("- Execution timeouts");
    println!("- Sandboxed execution");

    println!("\n=== Example Complete ===");

    Ok(())
}
