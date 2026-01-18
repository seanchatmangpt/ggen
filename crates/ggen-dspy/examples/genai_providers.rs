//! # GenAI Provider Configuration Example
//!
//! Demonstrates configuration for different LLM providers through rust-genai.
//! This example shows:
//! - Provider-specific configurations
//! - Environment variable management
//! - API key handling and security
//! - Model selection for different providers
//! - Provider-specific features
//!
//! ## Supported Providers
//!
//! - **Gemini** (Google)
//! - **OpenAI** (GPT models)
//! - **Anthropic** (Claude models)
//! - **Ollama** (Local models)
//! - **Groq** (Fast inference)
//! - **DeepSeek** (DeepSeek models)
//! - **Cohere** (Command models)
//!
//! ## Running the Example
//!
//! ```bash
//! # Set up environment for your preferred provider:
//!
//! # Gemini (recommended for testing - generous free tier)
//! export GEMINI_API_KEY=your_key_here
//! export DEFAULT_MODEL=gemini/gemini-2.0-flash-exp
//!
//! # OpenAI
//! export OPENAI_API_KEY=your_key_here
//! export DEFAULT_MODEL=openai/gpt-4o-mini
//!
//! # Anthropic
//! export ANTHROPIC_API_KEY=your_key_here
//! export DEFAULT_MODEL=anthropic/claude-3-5-sonnet-20241022
//!
//! # Ollama (local, no API key needed)
//! # Make sure Ollama is running: ollama serve
//! export DEFAULT_MODEL=ollama/llama3.2
//!
//! # Groq (fast inference)
//! export GROQ_API_KEY=your_key_here
//! export DEFAULT_MODEL=groq/llama-3.3-70b-versatile
//!
//! cargo run --example genai_providers
//! ```

use ggen_ai::{GenAiClient, LlmClient, LlmConfig, MockClient};
use std::collections::HashMap;

/// Main entry point demonstrating provider configurations
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== GenAI Provider Configuration Examples ===\n");

    // Show all provider configurations
    show_all_provider_configs();

    // Example 1: Gemini (Google) configuration
    example_gemini_config()?;

    // Example 2: OpenAI configuration
    example_openai_config()?;

    // Example 3: Anthropic (Claude) configuration
    example_anthropic_config()?;

    // Example 4: Ollama (local) configuration
    example_ollama_config()?;

    // Example 5: Groq configuration
    example_groq_config()?;

    // Example 6: Multiple provider fallback pattern
    example_multi_provider_fallback().await?;

    // Example 7: Environment-based provider selection
    example_env_based_selection()?;

    println!("\n=== All Provider Examples Completed ===");
    Ok(())
}

/// Display all provider configuration templates
fn show_all_provider_configs() {
    println!("--- Provider Configuration Reference ---\n");

    let providers = vec![
        ("Gemini", "gemini/gemini-2.0-flash-exp", "GEMINI_API_KEY"),
        ("OpenAI GPT-4o", "openai/gpt-4o", "OPENAI_API_KEY"),
        ("OpenAI GPT-4o-mini", "openai/gpt-4o-mini", "OPENAI_API_KEY"),
        ("Claude 3.5 Sonnet", "anthropic/claude-3-5-sonnet-20241022", "ANTHROPIC_API_KEY"),
        ("Claude 3.5 Haiku", "anthropic/claude-3-5-haiku-20241022", "ANTHROPIC_API_KEY"),
        ("Ollama Llama 3.2", "ollama/llama3.2", "none (local)"),
        ("Groq Llama 3.3", "groq/llama-3.3-70b-versatile", "GROQ_API_KEY"),
        ("DeepSeek Chat", "deepseek/deepseek-chat", "DEEPSEEK_API_KEY"),
        ("Cohere Command R", "cohere/command-r", "COHERE_API_KEY"),
    ];

    println!("  Supported Providers:");
    for (name, model, env_var) in providers {
        println!("    • {:20} model: {:40} env: {}", name, model, env_var);
    }
    println!();
}

/// Example 1: Gemini Configuration
///
/// Google's Gemini models offer:
/// - Generous free tier
/// - Fast inference
/// - Good quality responses
/// - Multi-modal support
fn example_gemini_config() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Example 1: Gemini (Google) Configuration ---");

    // Gemini Flash (fast, efficient)
    let gemini_flash = LlmConfig {
        model: "gemini/gemini-2.0-flash-exp".to_string(),
        max_tokens: Some(4096),
        temperature: Some(0.7),
        top_p: Some(0.95),
        stop: None,
        extra: HashMap::new(),
    };

    gemini_flash.validate()?;
    println!("  Gemini Flash Config:");
    println!("    Model: {}", gemini_flash.model);
    println!("    Use case: Fast responses, efficient");
    println!("    API Key: Set GEMINI_API_KEY environment variable");
    println!("    Get key: https://aistudio.google.com/app/apikey");

    // Check if API key is configured
    match std::env::var("GEMINI_API_KEY") {
        Ok(_) => println!("    Status: ✓ API key found"),
        Err(_) => println!("    Status: ✗ API key not set"),
    }

    println!();
    Ok(())
}

/// Example 2: OpenAI Configuration
///
/// OpenAI models (GPT-4, GPT-3.5) offer:
/// - Industry-leading quality
/// - Wide adoption and tooling
/// - Function calling support
/// - Multiple model tiers
fn example_openai_config() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Example 2: OpenAI Configuration ---");

    // GPT-4o mini (cost-effective)
    let gpt4o_mini = LlmConfig {
        model: "openai/gpt-4o-mini".to_string(),
        max_tokens: Some(4096),
        temperature: Some(0.7),
        top_p: Some(1.0),
        stop: None,
        extra: HashMap::new(),
    };

    gpt4o_mini.validate()?;
    println!("  GPT-4o Mini Config:");
    println!("    Model: {}", gpt4o_mini.model);
    println!("    Use case: Cost-effective, high quality");
    println!("    API Key: Set OPENAI_API_KEY environment variable");
    println!("    Get key: https://platform.openai.com/api-keys");

    // GPT-4o (most capable)
    let gpt4o = LlmConfig {
        model: "openai/gpt-4o".to_string(),
        max_tokens: Some(4096),
        temperature: Some(0.7),
        top_p: Some(1.0),
        stop: None,
        extra: HashMap::new(),
    };

    gpt4o.validate()?;
    println!("\n  GPT-4o Config:");
    println!("    Model: {}", gpt4o.model);
    println!("    Use case: Most capable, complex tasks");

    match std::env::var("OPENAI_API_KEY") {
        Ok(_) => println!("    Status: ✓ API key found"),
        Err(_) => println!("    Status: ✗ API key not set"),
    }

    println!();
    Ok(())
}

/// Example 3: Anthropic (Claude) Configuration
///
/// Anthropic's Claude models offer:
/// - Long context windows (200K+ tokens)
/// - Strong reasoning capabilities
/// - Safety-focused design
/// - High-quality outputs
fn example_anthropic_config() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Example 3: Anthropic (Claude) Configuration ---");

    // Claude 3.5 Sonnet (balanced)
    let claude_sonnet = LlmConfig {
        model: "anthropic/claude-3-5-sonnet-20241022".to_string(),
        max_tokens: Some(4096),
        temperature: Some(0.7),
        top_p: Some(1.0),
        stop: None,
        extra: HashMap::new(),
    };

    claude_sonnet.validate()?;
    println!("  Claude 3.5 Sonnet Config:");
    println!("    Model: {}", claude_sonnet.model);
    println!("    Use case: Balanced performance and cost");
    println!("    Context: 200K tokens");
    println!("    API Key: Set ANTHROPIC_API_KEY environment variable");
    println!("    Get key: https://console.anthropic.com/");

    // Claude 3.5 Haiku (fast)
    let claude_haiku = LlmConfig {
        model: "anthropic/claude-3-5-haiku-20241022".to_string(),
        max_tokens: Some(4096),
        temperature: Some(0.7),
        top_p: Some(1.0),
        stop: None,
        extra: HashMap::new(),
    };

    claude_haiku.validate()?;
    println!("\n  Claude 3.5 Haiku Config:");
    println!("    Model: {}", claude_haiku.model);
    println!("    Use case: Fast responses, lower cost");

    match std::env::var("ANTHROPIC_API_KEY") {
        Ok(_) => println!("    Status: ✓ API key found"),
        Err(_) => println!("    Status: ✗ API key not set"),
    }

    println!();
    Ok(())
}

/// Example 4: Ollama (Local) Configuration
///
/// Ollama allows running models locally:
/// - No API costs
/// - Privacy (data stays local)
/// - No internet required
/// - Multiple model options
fn example_ollama_config() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Example 4: Ollama (Local) Configuration ---");

    // Llama 3.2 (local)
    let ollama_config = LlmConfig {
        model: "ollama/llama3.2".to_string(),
        max_tokens: Some(2048),
        temperature: Some(0.7),
        top_p: Some(0.9),
        stop: None,
        extra: HashMap::new(),
    };

    ollama_config.validate()?;
    println!("  Ollama Config:");
    println!("    Model: {}", ollama_config.model);
    println!("    Use case: Local inference, privacy, no costs");
    println!("    Requirements:");
    println!("      1. Install Ollama: https://ollama.ai/");
    println!("      2. Pull model: ollama pull llama3.2");
    println!("      3. Start server: ollama serve");
    println!("    API Key: None needed (local)");

    // Other popular Ollama models
    println!("\n  Other Ollama Models:");
    let models = vec![
        ("ollama/llama3.2", "Meta's Llama 3.2 (8B)"),
        ("ollama/mistral", "Mistral 7B"),
        ("ollama/qwen2.5-coder", "Qwen 2.5 Coder (specialized for code)"),
        ("ollama/deepseek-r1", "DeepSeek R1 (reasoning)"),
    ];

    for (model, desc) in models {
        println!("    • {} - {}", model, desc);
    }

    println!();
    Ok(())
}

/// Example 5: Groq Configuration
///
/// Groq offers:
/// - Extremely fast inference
/// - Competitive pricing
/// - Popular open-source models
fn example_groq_config() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Example 5: Groq Configuration ---");

    let groq_config = LlmConfig {
        model: "groq/llama-3.3-70b-versatile".to_string(),
        max_tokens: Some(8000),
        temperature: Some(0.7),
        top_p: Some(1.0),
        stop: None,
        extra: HashMap::new(),
    };

    groq_config.validate()?;
    println!("  Groq Config:");
    println!("    Model: {}", groq_config.model);
    println!("    Use case: Fastest inference, real-time applications");
    println!("    API Key: Set GROQ_API_KEY environment variable");
    println!("    Get key: https://console.groq.com/");

    match std::env::var("GROQ_API_KEY") {
        Ok(_) => println!("    Status: ✓ API key found"),
        Err(_) => println!("    Status: ✗ API key not set"),
    }

    println!();
    Ok(())
}

/// Example 6: Multi-Provider Fallback Pattern
///
/// Demonstrates:
/// - Trying multiple providers in sequence
/// - Graceful degradation
/// - Error handling across providers
/// - Using MockClient as final fallback
async fn example_multi_provider_fallback() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Example 6: Multi-Provider Fallback Pattern ---");

    let prompt = "What is Rust?";

    // Define provider priority order
    let providers = vec![
        ("Gemini", "gemini/gemini-2.0-flash-exp", "GEMINI_API_KEY"),
        ("OpenAI", "openai/gpt-4o-mini", "OPENAI_API_KEY"),
        ("Anthropic", "anthropic/claude-3-5-haiku-20241022", "ANTHROPIC_API_KEY"),
    ];

    println!("  Attempting providers in order:");

    let mut response_content = None;

    // Try each provider
    for (name, model, env_var) in &providers {
        if std::env::var(env_var).is_ok() {
            println!("    Trying {}...", name);

            let config = LlmConfig {
                model: model.to_string(),
                max_tokens: Some(1024),
                temperature: Some(0.7),
                top_p: Some(0.9),
                stop: None,
                extra: HashMap::new(),
            };

            match GenAiClient::new(config) {
                Ok(client) => {
                    match client.complete(prompt).await {
                        Ok(response) => {
                            println!("      ✓ Success with {}", name);
                            response_content = Some(response.content);
                            break;
                        }
                        Err(e) => {
                            println!("      ✗ Failed: {}", e);
                        }
                    }
                }
                Err(e) => {
                    println!("      ✗ Client creation failed: {}", e);
                }
            }
        } else {
            println!("    Skipping {} (no API key)", name);
        }
    }

    // Fallback to MockClient if all providers fail
    if response_content.is_none() {
        println!("    Using MockClient as fallback...");
        let mock = MockClient::with_response(
            "Rust is a systems programming language focused on safety and performance."
        );
        let response = mock.complete(prompt).await?;
        response_content = Some(response.content);
        println!("      ✓ MockClient fallback successful");
    }

    if let Some(content) = response_content {
        println!("\n  Final Response: {}", content);
    }

    println!();
    Ok(())
}

/// Example 7: Environment-Based Provider Selection
///
/// Demonstrates:
/// - Automatic provider detection
/// - Environment-aware configuration
/// - Development vs production settings
fn example_env_based_selection() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Example 7: Environment-Based Provider Selection ---");

    // Check which providers are available
    let mut available_providers = Vec::new();

    if std::env::var("GEMINI_API_KEY").is_ok() {
        available_providers.push("Gemini");
    }
    if std::env::var("OPENAI_API_KEY").is_ok() {
        available_providers.push("OpenAI");
    }
    if std::env::var("ANTHROPIC_API_KEY").is_ok() {
        available_providers.push("Anthropic");
    }
    if std::env::var("GROQ_API_KEY").is_ok() {
        available_providers.push("Groq");
    }

    println!("  Available providers: {}",
        if available_providers.is_empty() {
            "None (using mock for testing)".to_string()
        } else {
            available_providers.join(", ")
        }
    );

    // Select based on environment
    let environment = std::env::var("APP_ENV").unwrap_or_else(|_| "development".to_string());

    println!("  Current environment: {}", environment);

    let recommended_config = match environment.as_str() {
        "production" => {
            println!("  Recommendation: Use Anthropic (Claude) for production");
            println!("    - Long context for complex tasks");
            println!("    - High reliability");
            println!("    - Strong safety features");

            LlmConfig {
                model: "anthropic/claude-3-5-sonnet-20241022".to_string(),
                max_tokens: Some(4096),
                temperature: Some(0.3),  // Lower for consistency
                top_p: Some(0.9),
                stop: None,
                extra: HashMap::new(),
            }
        }
        "development" => {
            println!("  Recommendation: Use Gemini or Ollama for development");
            println!("    - Gemini: Free tier, fast iteration");
            println!("    - Ollama: Local, no API costs");

            if std::env::var("GEMINI_API_KEY").is_ok() {
                LlmConfig {
                    model: "gemini/gemini-2.0-flash-exp".to_string(),
                    max_tokens: Some(2048),
                    temperature: Some(0.7),
                    top_p: Some(0.95),
                    stop: None,
                    extra: HashMap::new(),
                }
            } else {
                LlmConfig {
                    model: "ollama/llama3.2".to_string(),
                    max_tokens: Some(2048),
                    temperature: Some(0.7),
                    top_p: Some(0.9),
                    stop: None,
                    extra: HashMap::new(),
                }
            }
        }
        "testing" => {
            println!("  Recommendation: Use MockClient for testing");
            println!("    - Deterministic responses");
            println!("    - No API costs");
            println!("    - Fast execution");

            LlmConfig {
                model: "mock-model".to_string(),
                max_tokens: Some(1000),
                temperature: Some(0.0),
                top_p: Some(1.0),
                stop: None,
                extra: HashMap::new(),
            }
        }
        _ => {
            println!("  Using default configuration");
            LlmConfig::default()
        }
    };

    println!("\n  Selected Configuration:");
    println!("    Model: {}", recommended_config.model);
    println!("    Temperature: {:?}", recommended_config.temperature);

    println!();
    Ok(())
}
