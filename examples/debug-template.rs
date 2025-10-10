//! Debug template generation to see raw responses

use ggen_ai::generators::TemplateGenerator;
use ggen_ai::providers::OllamaClient;
use ggen_ai::client::LlmClient;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Warn if running example without explicit confirmation
    if std::env::var("GGEN_ALLOW_LIVE_CALLS").is_err() {
        eprintln!("⚠️  This example makes live API calls to Ollama.");
        eprintln!("Set GGEN_ALLOW_LIVE_CALLS=1 to proceed.");
        eprintln!("Or use: GGEN_ALLOW_LIVE_CALLS=1 cargo run --example debug-template");
        return Ok(());
    }
    
    println!("🔍 Debug Template Generation");
    println!("============================");

    let client = OllamaClient::new();
    let generator = TemplateGenerator::with_ollama_qwen3_coder(Box::new(client));

    // Test direct client call to see raw response
    println!("📝 Testing direct client call...");
    let client = OllamaClient::new();
    let config = OllamaClient::qwen3_coder_config();
    let raw_response = client.complete(
        "Generate a ggen template for a Rust user service with JWT authentication. Use YAML frontmatter with 'to', 'vars', and 'rdf' fields, followed by the template body after '---'.",
        Some(config)
    ).await?;
    
    println!("Raw response:");
    println!("Model: {}", raw_response.model);
    println!("Content:\n{}", raw_response.content);
    println!("--- End of raw response ---\n");

    // Test template generation
    println!("📝 Testing template generation...");
    let template = generator.generate_template(
        "Create a user authentication service with JWT tokens",
        vec!["Include JWT tokens", "Support password hashing"]
    ).await?;
    
    println!("Generated template:");
    println!("Frontmatter: {:?}", template.front);
    println!("Body: '{}'", template.body);
    println!("Body length: {}", template.body.len());

    Ok(())
}
