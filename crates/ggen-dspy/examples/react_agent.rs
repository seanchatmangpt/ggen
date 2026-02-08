//! ReAct Agent example
//!
//! Demonstrates the ReAct pattern with tool use.
//! Run with: cargo run --example react_agent

use async_trait::async_trait;
use ggen_ai::dspy::{InputField, OutputField, Signature};
use ggen_dspy::{ReAct, Result, Tool};

// Example Calculator tool
struct Calculator;

#[async_trait]
impl Tool for Calculator {
    fn name(&self) -> &str {
        "calculator"
    }

    fn description(&self) -> &str {
        "Performs basic arithmetic operations. Input format: '2 + 2' or '10 * 5'"
    }

    async fn execute(&self, input: &str) -> Result<String> {
        // Simple calculator implementation
        let parts: Vec<&str> = input.split_whitespace().collect();
        if parts.len() != 3 {
            return Ok(format!("Invalid input format: {}", input));
        }

        let a: f64 = parts[0].parse().unwrap_or(0.0);
        let op = parts[1];
        let b: f64 = parts[2].parse().unwrap_or(0.0);

        let result = match op {
            "+" => a + b,
            "-" => a - b,
            "*" => a * b,
            "/" => a / b,
            _ => return Ok(format!("Unknown operator: {}", op)),
        };

        Ok(result.to_string())
    }
}

// Example Search tool
struct Search;

#[async_trait]
impl Tool for Search {
    fn name(&self) -> &str {
        "search"
    }

    fn description(&self) -> &str {
        "Searches for information. Returns mock results for demonstration."
    }

    async fn execute(&self, input: &str) -> Result<String> {
        // Mock search results
        Ok(format!(
            "Search results for '{}': Found relevant information about the topic.",
            input
        ))
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize logging
    ggen_dspy::init_logging();

    // Define a signature for agent tasks
    let signature = Signature::new("AgentTask", "Solve problems using available tools")
        .with_input(InputField::new("task", "The task to accomplish", "String"))
        .with_output(OutputField::new("answer", "The final result", "String"));

    // Create ReAct agent
    let mut react = ReAct::new(signature)
        .with_max_iterations(5)
        .with_temperature(0.7);

    // Add tools
    react.add_tool(Box::new(Calculator));
    react.add_tool(Box::new(Search));

    // Example tasks
    let tasks = vec![
        "Calculate 25 * 4 and tell me the result",
        "What is 100 + 50 - 25?",
    ];

    for task in tasks {
        println!("\n--- Task: {} ---", task);

        let inputs = vec![("task", task)];

        match react.forward(&inputs).await {
            Ok(output) => {
                println!("Answer: {}", output.get("answer")?);
                if let Ok(trajectory) = output.get("trajectory") {
                    println!("\nAgent trajectory:");
                    println!("{}", trajectory);
                }
            }
            Err(e) => {
                eprintln!("Error: {}", e);
            }
        }
    }

    Ok(())
}
