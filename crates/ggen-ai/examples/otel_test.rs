//! OTEL Test for LLM Integration
//!
//! This demonstrates proper OTEL span creation for LLM calls.
//! Run with: GROQ_API_KEY=xxx cargo run --example otel_test

use ggen_ai::{client::GenAiClient, config::LlmConfig, LlmClient};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing for observability
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .with_target(false)
        .with_thread_ids(true)
        .init();

    println!("🚀 OTEL LLM Integration Test");
    println!("============================");

    // Verify API key is available
    let api_key =
        std::env::var("GROQ_API_KEY").unwrap_or_else(|_| panic!("GROQ_API_KEY must be set"));

    if api_key.is_empty() {
        panic!("GROQ_API_KEY must not be empty");
    }

    println!("✅ GROQ_API_KEY available ({} chars)", api_key.len());

    // Create client with Groq configuration
    let config = LlmConfig {
        model: "groq::openai/gpt-oss-20b".to_string(),
        max_tokens: Some(100),
        temperature: Some(0.7),
        top_p: Some(0.9),
        stop: None,
        extra: std::collections::HashMap::new(),
    };

    println!("🔧 Creating LLM client with model: {}", config.model);
    let client = GenAiClient::new(config).expect("Failed to create LLM client");

    // Create a test prompt
    let prompt = "Hello! Please respond with a brief greeting and your name in one sentence.";

    println!("📝 Prompt: {}", prompt);
    println!();

    // Make the LLM call
    println!("🚀 Making LLM call...");
    println!("🔍 Check for OTEL spans in the output below:");
    println!();

    let response = client.complete(prompt).await.expect("LLM complete failed");

    // Verify response has expected content
    println!("✅ LLM Response:");
    println!("   Model: {}", response.model);
    println!("   Content: {}", response.content);

    if let Some(usage) = &response.usage {
        println!("   Usage:");
        println!("     Prompt tokens: {}", usage.prompt_tokens);
        println!("     Completion tokens: {}", usage.completion_tokens);
        println!("     Total tokens: {}", usage.total_tokens);
    }

    println!();
    println!("🎯 OTEL Span Verification:");
    println!();
    println!("If OTEL spans are properly implemented, you should see above:");
    println!("- 'llm.complete' span name");
    println!("- 'llm.model' attribute with the model name");
    println!("- Token count attributes recorded");
    println!("- Timing information in the spans");
    println!();

    // Verify this is a real API call
    if response.content.is_empty() {
        panic!("❌ Response content is empty - API call failed");
    }

    if response.content.contains("Mock response") {
        panic!("❌ Response appears to be from a mock, not real API");
    }

    println!("✅ Test completed successfully!");
    println!("   Real LLM API call was made");
    println!("   Response length: {} characters", response.content.len());

    Ok(())
}
