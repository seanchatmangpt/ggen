//! Basic adapter usage example
//!
//! Demonstrates how to use adapters for formatting prompts and parsing responses.

use ggen_dspy::adapters::{ChatAdapter, Demonstration, JSONAdapter, LlmAdapter};
use serde_json::Value;
use std::collections::HashMap;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Basic Adapter Usage ===\n");

    // Example 1: ChatAdapter for natural language format
    example_chat_adapter()?;

    // Example 2: JSONAdapter for structured output
    example_json_adapter()?;

    // Example 3: Using demonstrations (few-shot learning)
    example_with_demonstrations()?;

    Ok(())
}

fn example_chat_adapter() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- ChatAdapter Example ---");

    let adapter = ChatAdapter::new();

    // Create inputs
    let mut inputs = HashMap::new();
    inputs.insert(
        "question".to_string(),
        Value::String("What is Rust?".to_string()),
    );

    // Define output fields
    let output_fields = vec!["answer".to_string()];

    // Format prompt
    let prompt = adapter.format_prompt(&inputs, &output_fields, None, None)?;
    println!("Generated Prompt:\n{}\n", prompt);

    // Simulate LLM response
    let llm_response = "[[ ## answer ## ]]\nRust is a systems programming language focused on safety and performance.";

    // Parse response
    let parsed = adapter.parse_response(llm_response, &output_fields)?;
    println!("Parsed Response:");
    for (key, value) in parsed {
        println!("  {}: {}", key, value);
    }
    println!();

    Ok(())
}

fn example_json_adapter() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- JSONAdapter Example ---");

    let adapter = JSONAdapter::new();

    // Create inputs
    let mut inputs = HashMap::new();
    inputs.insert(
        "text".to_string(),
        Value::String("Analyze this sentiment".to_string()),
    );

    // Define output fields
    let output_fields = vec!["sentiment".to_string(), "confidence".to_string()];

    // Format prompt with schema
    let prompt = adapter.format_prompt(&inputs, &output_fields, None, None)?;
    println!("Generated Prompt:\n{}\n", prompt);

    // Simulate LLM response (with markdown code block)
    let llm_response = r#"```json
{
    "sentiment": "neutral",
    "confidence": 0.75
}
```"#;

    // Parse response
    let parsed = adapter.parse_response(llm_response, &output_fields)?;
    println!("Parsed Response:");
    for (key, value) in parsed {
        println!("  {}: {}", key, value);
    }
    println!();

    Ok(())
}

fn example_with_demonstrations() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Demonstrations (Few-Shot) Example ---");

    let adapter = ChatAdapter::new();

    // Create demonstrations
    let mut demo1_in = HashMap::new();
    demo1_in.insert("number".to_string(), Value::String("4".to_string()));
    let mut demo1_out = HashMap::new();
    demo1_out.insert("squared".to_string(), Value::String("16".to_string()));

    let mut demo2_in = HashMap::new();
    demo2_in.insert("number".to_string(), Value::String("7".to_string()));
    let mut demo2_out = HashMap::new();
    demo2_out.insert("squared".to_string(), Value::String("49".to_string()));

    let demos = vec![
        Demonstration::new(demo1_in, demo1_out),
        Demonstration::new(demo2_in, demo2_out),
    ];

    // Create test input
    let mut inputs = HashMap::new();
    inputs.insert("number".to_string(), Value::String("9".to_string()));

    let output_fields = vec!["squared".to_string()];

    // Format prompt with demonstrations
    let prompt = adapter.format_prompt(&inputs, &output_fields, None, Some(&demos))?;
    println!("Generated Prompt with Demonstrations:\n{}\n", prompt);

    Ok(())
}
