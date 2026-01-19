//! # GenAI Integration Example
//!
//! Demonstrates correct usage of rust-genai library through ggen-ai wrapper.
//! This example shows:
//! - Client initialization with configuration
//! - Proper model configuration
//! - Request/response handling
//! - Token counting
//! - Streaming responses
//!
//! ## Running the Example
//!
//! ```bash
//! # Using MockClient (no API keys needed):
//! cargo run --example genai_integration
//!
//! # Using real provider (requires API key):
//! export GEMINI_API_KEY=your_key_here
//! export DEFAULT_MODEL=gemini/gemini-2.0-flash-exp
//! cargo run --example genai_integration
//! ```

use ggen_ai::{GenAiClient, LlmClient, LlmConfig, MockClient};
use futures::StreamExt;

/// Main entry point demonstrating GenAI integration patterns
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== GenAI Integration Examples ===\n");

    // Example 1: Basic client initialization and configuration
    example_basic_client()?;

    // Example 2: Using MockClient for testing (no API keys needed)
    example_mock_client().await?;

    // Example 3: Real provider with proper error handling
    example_real_provider().await?;

    // Example 4: Streaming responses
    example_streaming().await?;

    // Example 5: Token counting and usage statistics
    example_token_counting().await?;

    // Example 6: Custom configuration
    example_custom_config().await?;

    println!("\n=== All Examples Completed Successfully ===");
    Ok(())
}

/// Example 1: Basic Client Initialization
///
/// Demonstrates:
/// - Creating LlmConfig with defaults
/// - Validating configuration
/// - Initializing GenAiClient
fn example_basic_client() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Example 1: Basic Client Initialization ---");

    // Create default configuration
    // This reads from environment variables:
    // - DEFAULT_MODEL (or defaults to "gemini/gemini-2.0-flash-exp")
    // - LLM_MAX_TOKENS (or defaults to 4096)
    // - LLM_TEMPERATURE (or defaults to 0.7)
    // - LLM_TOP_P (or defaults to 0.9)
    let config = LlmConfig::default();

    println!("Default Configuration:");
    println!("  Model: {}", config.model);
    println!("  Max Tokens: {:?}", config.max_tokens);
    println!("  Temperature: {:?}", config.temperature);
    println!("  Top-P: {:?}", config.top_p);

    // Validate configuration (checks bounds, required fields)
    config.validate()?;
    println!("  ✓ Configuration validated");

    // Initialize client
    // Note: This creates the client but doesn't make any API calls yet
    let _client = GenAiClient::new(config)?;
    println!("  ✓ Client initialized successfully\n");

    Ok(())
}

/// Example 2: MockClient for Testing
///
/// Demonstrates:
/// - Using MockClient to avoid API calls
/// - Predefined responses for deterministic testing
/// - No API keys required
async fn example_mock_client() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Example 2: MockClient (No API Keys Needed) ---");

    // Create mock client with predefined responses
    let mock_responses = vec![
        "Rust is a systems programming language focused on safety, concurrency, and performance.".to_string(),
        "The three core pillars are: memory safety, concurrency without data races, and zero-cost abstractions.".to_string(),
    ];

    let client = MockClient::new(mock_responses);

    // First request
    let response1 = client.complete("What is Rust?").await?;
    println!("Question: What is Rust?");
    println!("Answer: {}", response1.content);
    println!("  Model: {}", response1.model);

    // Token usage is available even with MockClient
    if let Some(usage) = &response1.usage {
        println!("  Tokens: {} prompt + {} completion = {} total",
            usage.prompt_tokens,
            usage.completion_tokens,
            usage.total_tokens
        );
    }

    // Second request (uses second predefined response)
    let response2 = client.complete("What are Rust's core pillars?").await?;
    println!("\nQuestion: What are Rust's core pillars?");
    println!("Answer: {}", response2.content);

    println!("  ✓ MockClient works without API keys\n");

    Ok(())
}

/// Example 3: Real Provider with Error Handling
///
/// Demonstrates:
/// - Checking for API keys
/// - Handling missing credentials gracefully
/// - Making actual API calls (when credentials available)
/// - Proper Result<T,E> error propagation
async fn example_real_provider() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Example 3: Real Provider (Requires API Key) ---");

    // Check if we have credentials configured
    let has_credentials = std::env::var("GEMINI_API_KEY").is_ok()
        || std::env::var("OPENAI_API_KEY").is_ok()
        || std::env::var("ANTHROPIC_API_KEY").is_ok();

    if !has_credentials {
        println!("  ⚠ No API keys found. Skipping real provider test.");
        println!("  To run this example, set one of:");
        println!("    export GEMINI_API_KEY=your_key");
        println!("    export OPENAI_API_KEY=your_key");
        println!("    export ANTHROPIC_API_KEY=your_key");
        println!("  And optionally:");
        println!("    export DEFAULT_MODEL=gemini/gemini-2.0-flash-exp");
        println!();
        return Ok(());
    }

    // Create client with default config (reads from environment)
    let config = LlmConfig::default();
    let client = GenAiClient::new(config)?;

    // Make a real API call
    println!("  Making API call...");
    match client.complete("Explain ownership in Rust in one sentence.").await {
        Ok(response) => {
            println!("  ✓ API call successful!");
            println!("  Response: {}", response.content);

            if let Some(usage) = response.usage {
                println!("  Tokens used: {}", usage.total_tokens);
            }
        }
        Err(e) => {
            println!("  ✗ API call failed: {}", e);
            println!("  This is expected if credentials are invalid or network is unavailable.");
        }
    }

    println!();
    Ok(())
}

/// Example 4: Streaming Responses
///
/// Demonstrates:
/// - Using complete_stream for real-time output
/// - Processing streaming chunks
/// - Handling stream completion
/// - Collecting usage stats from final chunk
async fn example_streaming() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Example 4: Streaming Responses ---");

    // Use MockClient for demonstration (works identically with real providers)
    let client = MockClient::with_response(
        "Streaming allows you to display responses as they're generated, \
         improving user experience for long-form content."
    );

    println!("  Question: Why use streaming?");
    print!("  Answer (streaming): ");

    // Get stream
    let mut stream = client.complete_stream("Why use streaming?").await?;

    let mut total_content = String::new();
    let mut final_usage = None;

    // Process chunks as they arrive
    while let Some(chunk) = stream.next().await {
        // In a real application, you would print chunks incrementally
        total_content.push_str(&chunk.content);

        // Usage stats are typically only in the final chunk
        if chunk.finish_reason.is_some() {
            final_usage = chunk.usage;
        }
    }

    println!("{}", total_content);

    if let Some(usage) = final_usage {
        println!("  Tokens: {} total", usage.total_tokens);
    }

    println!("  ✓ Streaming completed\n");

    Ok(())
}

/// Example 5: Token Counting and Usage Statistics
///
/// Demonstrates:
/// - Extracting usage statistics from responses
/// - Tracking token consumption
/// - Budget management
/// - Cost estimation
async fn example_token_counting() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Example 5: Token Counting ---");

    let client = MockClient::new(vec![
        "Short answer.".to_string(),
        "This is a much longer answer that will consume more tokens, demonstrating how token usage varies with response length.".to_string(),
    ]);

    let mut total_prompt_tokens = 0u32;
    let mut total_completion_tokens = 0u32;

    // First request
    let response1 = client.complete("Short question?").await?;
    if let Some(usage) = &response1.usage {
        total_prompt_tokens += usage.prompt_tokens;
        total_completion_tokens += usage.completion_tokens;
        println!("  Request 1: {} total tokens", usage.total_tokens);
    }

    // Second request
    let response2 = client.complete("Long question?").await?;
    if let Some(usage) = &response2.usage {
        total_prompt_tokens += usage.prompt_tokens;
        total_completion_tokens += usage.completion_tokens;
        println!("  Request 2: {} total tokens", usage.total_tokens);
    }

    // Summary
    println!("\n  Session Summary:");
    println!("    Total Prompt Tokens: {}", total_prompt_tokens);
    println!("    Total Completion Tokens: {}", total_completion_tokens);
    println!("    Total Tokens: {}", total_prompt_tokens + total_completion_tokens);

    // Example cost estimation (GPT-4 pricing as example)
    let estimated_cost = (total_prompt_tokens as f64 * 0.00003)
        + (total_completion_tokens as f64 * 0.00006);
    println!("    Estimated Cost (GPT-4 rates): ${:.6}", estimated_cost);

    println!("  ✓ Token tracking complete\n");

    Ok(())
}

/// Example 6: Custom Configuration
///
/// Demonstrates:
/// - Building custom LlmConfig
/// - Setting specific parameters
/// - Validation of configuration
/// - Different models and settings
async fn example_custom_config() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Example 6: Custom Configuration ---");

    // Configuration for creative writing (high temperature)
    let creative_config = LlmConfig {
        model: "gemini/gemini-2.0-flash-exp".to_string(),
        max_tokens: Some(2048),
        temperature: Some(1.2),  // Higher = more creative
        top_p: Some(0.95),
        stop: None,
        extra: std::collections::HashMap::new(),
    };

    // Validate before use
    creative_config.validate()?;
    println!("  Creative Config:");
    println!("    Model: {}", creative_config.model);
    println!("    Temperature: {:?} (high for creativity)", creative_config.temperature);
    println!("    Max Tokens: {:?}", creative_config.max_tokens);

    // Configuration for factual/analytical tasks (low temperature)
    let analytical_config = LlmConfig {
        model: "gemini/gemini-2.0-flash-exp".to_string(),
        max_tokens: Some(1024),
        temperature: Some(0.1),  // Lower = more deterministic
        top_p: Some(0.9),
        stop: Some(vec!["\n\n".to_string()]),  // Stop at paragraph breaks
        extra: std::collections::HashMap::new(),
    };

    analytical_config.validate()?;
    println!("\n  Analytical Config:");
    println!("    Model: {}", analytical_config.model);
    println!("    Temperature: {:?} (low for accuracy)", analytical_config.temperature);
    println!("    Stop sequences: {:?}", analytical_config.stop);

    println!("\n  ✓ Custom configurations validated\n");

    Ok(())
}
