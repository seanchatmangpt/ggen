//! ZAI (Zhipu AI) Integration Tests
//!
//! Tests for ZAI provider integration using genai::Client.
//!
//! ## Environment Setup
//!
//! Set the `ZAI_API_KEY` environment variable to run these tests:
//!
//! ```bash
//! export ZAI_API_KEY="your-api-key-here"
//! cargo test --test zai_integration -- --ignored
//! ```
//!
//! ## Running Tests
//!
//! Run all tests (requires API key):
//! ```bash
//! cargo test --test zai_integration -- --ignored
//! ```
//!
//! Run specific test:
//! ```bash
//! cargo test --test zai_integration zai_client_connects -- --ignored
//! ```

use ggen_ai::{client::LlmClient, GenAiClient, LlmConfig};
use std::collections::HashMap;

/// Check if ZAI API key is available
fn has_zai_api_key() -> bool {
    std::env::var("ZAI_API_KEY").is_ok()
}

/// Create a ZAI client configuration
fn create_zai_config() -> LlmConfig {
    LlmConfig {
        model: "zai-coding::glm-4.6".to_string(),
        max_tokens: Some(100),
        temperature: Some(0.7),
        top_p: Some(0.9),
        stop: None,
        extra: HashMap::new(),
    }
}

#[tokio::test]
#[ignore = "requires ZAI_API_KEY environment variable"]
async fn zai_client_connects() -> Result<(), Box<dyn std::error::Error>> {
    if !has_zai_api_key() {
        eprintln!("Skipping test: ZAI_API_KEY not set");
        return Ok(());
    }

    let config = create_zai_config();
    let client = GenAiClient::new(config)?;

    let response = client.complete("Say 'hello' and nothing else.").await?;

    assert!(!response.content.is_empty(), "Response should not be empty");
    assert!(
        response.content.to_lowercase().contains("hello"),
        "Response should contain 'hello': {}",
        response.content
    );

    println!("✅ ZAI client connected successfully");
    println!("Response: {}", response.content);

    Ok(())
}

#[tokio::test]
#[ignore = "requires ZAI_API_KEY environment variable"]
async fn zai_client_with_system_prompt() -> Result<(), Box<dyn std::error::Error>> {
    if !has_zai_api_key() {
        eprintln!("Skipping test: ZAI_API_KEY not set");
        return Ok(());
    }

    let config = create_zai_config();
    let client = GenAiClient::new(config)?;

    let response = client
        .complete("What is 2 + 2? Answer with just the number.")
        .await?;

    assert!(!response.content.is_empty(), "Response should not be empty");

    println!("✅ ZAI client with system prompt works");
    println!("Response: {}", response.content);

    Ok(())
}

#[tokio::test]
#[ignore = "requires ZAI_API_KEY environment variable"]
async fn zai_client_returns_usage_stats() -> Result<(), Box<dyn std::error::Error>> {
    if !has_zai_api_key() {
        eprintln!("Skipping test: ZAI_API_KEY not set");
        return Ok(());
    }

    let config = create_zai_config();
    let client = GenAiClient::new(config)?;

    let response = client.complete("Count to 5.").await?;

    assert!(
        response.usage.is_some(),
        "Response should include usage statistics"
    );

    if let Some(usage) = response.usage {
        assert!(usage.prompt_tokens > 0, "Should have prompt tokens");
        assert!(
            usage.completion_tokens > 0,
            "Should have completion tokens"
        );
        assert!(
            usage.total_tokens > 0,
            "Should have total tokens"
        );

        println!("✅ ZAI usage statistics:");
        println!("  Prompt tokens: {}", usage.prompt_tokens);
        println!(
            "  Completion tokens: {}",
            usage.completion_tokens
        );
        println!("  Total tokens: {}", usage.total_tokens);
    }

    Ok(())
}

#[tokio::test]
#[ignore = "requires ZAI_API_KEY environment variable"]
async fn zai_client_streams_response() -> Result<(), Box<dyn std::error::Error>> {
    if !has_zai_api_key() {
        eprintln!("Skipping test: ZAI_API_KEY not set");
        return Ok(());
    }

    let config = create_zai_config();
    let client = GenAiClient::new(config)?;

    let mut stream = client.complete_stream("Count from 1 to 5.").await?;

    let mut full_content = String::new();
    let mut chunk_count = 0;

    use futures::StreamExt;
    while let Some(chunk) = stream.next().await {
        chunk_count += 1;
        full_content.push_str(&chunk.content);

        if let Some(finish_reason) = chunk.finish_reason {
            println!("Stream finished: {}", finish_reason);
        }
    }

    assert!(
        chunk_count > 0,
        "Should receive at least one chunk"
    );
    assert!(
        !full_content.is_empty(),
        "Streamed content should not be empty"
    );

    println!("✅ ZAI streaming works");
    println!("Chunks received: {}", chunk_count);
    println!("Full response: {}", full_content);

    Ok(())
}

#[tokio::test]
#[ignore = "requires ZAI_API_KEY environment variable"]
async fn zai_client_with_temperature() -> Result<(), Box<dyn std::error::Error>> {
    if !has_zai_api_key() {
        eprintln!("Skipping test: ZAI_API_KEY not set");
        return Ok(());
    }

    // Low temperature = more deterministic
    let low_temp_config = LlmConfig {
        model: "zai-coding::glm-4.6".to_string(),
        max_tokens: Some(50),
        temperature: Some(0.0),
        top_p: Some(0.9),
        stop: None,
        extra: HashMap::new(),
    };

    let client = GenAiClient::new(low_temp_config)?;
    let response = client
        .complete("Generate a random number between 1 and 10.")
        .await?;

    assert!(!response.content.is_empty(), "Response should not be empty");

    println!("✅ ZAI client with temperature works");
    println!("Response: {}", response.content);

    Ok(())
}

#[test]
fn test_zai_config_validation() {
    let config = create_zai_config();
    assert!(config.validate().is_ok(), "Config should be valid");

    // Test invalid model
    let mut invalid_config = config.clone();
    invalid_config.model = String::new();
    assert!(invalid_config.validate().is_err(), "Empty model should fail");
}

#[test]
fn test_zai_model_constant() {
    use ggen_ai::constants::models;

    assert_eq!(
        models::ZAI_DEFAULT,
        "zai-coding::glm-4.6",
        "ZAI_DEFAULT should be set to the correct model"
    );
}

#[test]
fn test_global_config_zai_provider() {
    use ggen_ai::config::global::{GlobalLlmConfig, LlmProvider};

    let config = GlobalLlmConfig::default();

    // Check that ZAI provider is available
    assert!(
        config.providers.contains_key(&LlmProvider::Zai),
        "ZAI provider should be in default providers"
    );

    // Check that ZAI config uses correct model
    if let Some(zai_config) = config.get_provider_config(&LlmProvider::Zai) {
        assert_eq!(
            zai_config.model,
            "zai-coding::glm-4.6",
            "ZAI provider should use correct model"
        );
    }
}
