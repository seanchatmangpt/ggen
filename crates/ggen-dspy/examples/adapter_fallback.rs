//! Adapter fallback mechanism example
//!
//! Demonstrates automatic adapter selection based on model capabilities.

use ggen_dspy::adapters::{AdapterWithFallback, LlmAdapter};
use serde_json::Value;
use std::collections::HashMap;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Adapter Fallback Mechanism ===\n");

    // Example 1: JSON-compatible model
    example_json_compatible()?;

    // Example 2: Non-JSON-compatible model (fallback to Chat)
    example_chat_fallback()?;

    Ok(())
}

fn example_json_compatible() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- JSON-Compatible Model (GPT-4) ---");

    // Create adapter for GPT-4 (supports JSON mode)
    let adapter = AdapterWithFallback::new("gpt-4");

    println!("Model: {}", adapter.model);
    println!("Selected Adapter: {}", adapter.name());
    println!("Expected: JSONAdapter\n");

    // Create inputs
    let mut inputs = HashMap::new();
    inputs.insert("task".to_string(), Value::String("Generate response".to_string()));

    let output_fields = vec!["response".to_string(), "metadata".to_string()];

    // Format prompt (will use JSON format)
    let prompt = adapter.format_prompt(&inputs, &output_fields, None, None)?;

    println!("Prompt Format:");
    if prompt.contains("JSON Schema") {
        println!("  ✓ Using JSON format with schema");
    } else {
        println!("  ✗ Not using JSON format");
    }
    println!();

    Ok(())
}

fn example_chat_fallback() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Non-JSON-Compatible Model (GPT-3.5) ---");

    // Create adapter for GPT-3.5 (does not support JSON mode reliably)
    let adapter = AdapterWithFallback::new("gpt-3.5-turbo");

    println!("Model: {}", adapter.model);
    println!("Selected Adapter: {}", adapter.name());
    println!("Expected: ChatAdapter (fallback)\n");

    // Create inputs
    let mut inputs = HashMap::new();
    inputs.insert("task".to_string(), Value::String("Generate response".to_string()));

    let output_fields = vec!["response".to_string()];

    // Format prompt (will use Chat format with field markers)
    let prompt = adapter.format_prompt(&inputs, &output_fields, None, None)?;

    println!("Prompt Format:");
    if prompt.contains("[[ ##") {
        println!("  ✓ Using Chat format with field markers");
    } else {
        println!("  ✗ Not using Chat format");
    }
    println!();

    // Demonstrate parsing
    let llm_response = "[[ ## response ## ]]\nThis is the generated response.";
    let parsed = adapter.parse_response(llm_response, &output_fields)?;

    println!("Parsed Response:");
    for (key, value) in parsed {
        println!("  {}: {}", key, value);
    }
    println!();

    Ok(())
}
