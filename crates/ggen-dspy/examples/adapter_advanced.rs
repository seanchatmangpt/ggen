//! Advanced adapter usage with retry, caching, and token tracking
//!
//! Demonstrates integration with ggen-ai client and advanced features.

use ggen_ai::{GenAiClient, LlmConfig};
use ggen_dspy::adapters::{GgenAiAdapter, RetryConfig};
use serde_json::Value;
use std::collections::HashMap;
use std::time::Duration;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Advanced Adapter Features ===\n");

    // Example 1: Basic adapter with retry
    example_with_retry().await?;

    // Example 2: Adapter with caching
    example_with_caching().await?;

    // Example 3: Token tracking
    example_token_tracking().await?;

    Ok(())
}

async fn example_with_retry() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Adapter with Retry Logic ---");

    // Create LLM client
    let config = LlmConfig {
        model: "gpt-4".to_string(),
        max_tokens: Some(100),
        temperature: Some(0.7),
        top_p: Some(0.9),
        stop: None,
        extra: HashMap::new(),
    };
    let client = GenAiClient::new(config)?;

    // Create adapter with custom retry configuration
    let retry_config = RetryConfig {
        max_retries: 5,
        initial_backoff: Duration::from_millis(200),
        max_backoff: Duration::from_secs(30),
        backoff_multiplier: 2.5,
    };

    let adapter = GgenAiAdapter::new(client).with_retry_config(retry_config);

    println!("Adapter configured with:");
    println!("  - Max retries: 5");
    println!("  - Initial backoff: 200ms");
    println!("  - Max backoff: 30s");
    println!("  - Backoff multiplier: 2.5x");
    println!();

    // Note: Actual LLM call would be here
    // let mut inputs = HashMap::new();
    // inputs.insert("question".to_string(), Value::String("...".to_string()));
    // let result = adapter.complete(&inputs, &["answer".to_string()]).await?;

    Ok(())
}

async fn example_with_caching() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Adapter with Response Caching ---");

    // Create LLM client
    let config = LlmConfig::default();
    let client = GenAiClient::new(config)?;

    // Create adapter with caching enabled
    let adapter = GgenAiAdapter::new(client).with_cache(
        Duration::from_secs(3600), // 1 hour TTL
        1000,                      // Max 1000 cached entries
    );

    println!("Adapter configured with:");
    println!("  - Cache TTL: 1 hour");
    println!("  - Max cache entries: 1000");
    println!("  - Cache reduces API calls for repeated prompts");
    println!();

    // Note: Actual LLM calls would be cached automatically
    // First call: hits LLM
    // let result1 = adapter.complete(&inputs, &output_fields).await?;
    // Second call with same inputs: cache hit, no LLM call
    // let result2 = adapter.complete(&inputs, &output_fields).await?;

    Ok(())
}

async fn example_token_tracking() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Token Usage Tracking ---");

    // Create adapter
    let config = LlmConfig::default();
    let client = GenAiClient::new(config)?;
    let adapter = GgenAiAdapter::new(client);

    // Simulate some LLM calls (would be actual calls in production)
    println!("Making LLM calls...");

    // Get token statistics
    let stats = adapter.get_token_stats();

    println!("\nToken Usage Statistics:");
    println!("  Total prompt tokens: {}", stats.prompt_tokens);
    println!("  Total completion tokens: {}", stats.completion_tokens);
    println!("  Total tokens: {}", stats.total_tokens);

    println!("\nPer-Model Usage:");
    for (model, usage) in stats.model_usage {
        println!("  Model: {}", model);
        println!("    Requests: {}", usage.request_count);
        println!("    Prompt tokens: {}", usage.prompt_tokens);
        println!("    Completion tokens: {}", usage.completion_tokens);
    }
    println!();

    Ok(())
}
