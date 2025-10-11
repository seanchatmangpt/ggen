use ggen_ai::client::{LlmClient, LlmConfig};
use ggen_ai::config::OllamaConfig;
use ggen_ai::providers::ollama::OllamaClient;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Testing Ollama integration...");

    // Create Ollama client
    let config = OllamaConfig::new();
    let client = OllamaClient::new(config)?;

    // Test with simple prompt
    let llm_config = LlmConfig {
        model: "qwen2.5-coder:latest".to_string(),
        max_tokens: Some(100),
        temperature: Some(0.3),
        ..Default::default()
    };

    println!("Sending prompt to Ollama...");
    let response = client
        .complete("Write a hello world in Rust", Some(llm_config))
        .await?;

    println!("\n✅ Response received:");
    println!("{}", response.content);
    println!("\nModel: {}", response.model);
    if let Some(usage) = response.usage {
        println!(
            "Tokens: {} prompt + {} completion = {} total",
            usage.prompt_tokens, usage.completion_tokens, usage.total_tokens
        );
    }

    Ok(())
}
