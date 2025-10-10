//! Simple test for Ollama client with qwen3-coder:30b

use ggen_ai::providers::OllamaClient;
use ggen_ai::client::LlmClient;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🧪 Testing Ollama client with qwen3-coder:30b");
    println!("=============================================");

    let client = OllamaClient::new();
    
    // Test basic completion
    println!("📝 Testing basic completion...");
    let response = client.complete(
        "Generate a simple Rust struct for a User with id, name, and email fields",
        None
    ).await?;
    
    println!("✅ Response received:");
    println!("Model: {}", response.model);
    println!("Content: {}", response.content);
    
    if let Some(usage) = response.usage {
        println!("Usage: {} prompt tokens, {} completion tokens", 
                usage.prompt_tokens, usage.completion_tokens);
    }

    // Test with custom config
    println!("\n🔧 Testing with qwen3-coder:30b config...");
    let config = OllamaClient::qwen3_coder_config();
    let response2 = client.complete(
        "Create a TypeScript interface for a Product with name, price, and category",
        Some(config)
    ).await?;
    
    println!("✅ Response received:");
    println!("Model: {}", response2.model);
    println!("Content: {}", response2.content);

    println!("\n🎉 Ollama client test completed successfully!");

    Ok(())
}

