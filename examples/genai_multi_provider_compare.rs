//! Multi-Provider Comparison Example
//!
//! Compare responses from Ollama qwen3-coder:30b with other providers
//! (if API keys are configured) for the same coding question.
//!
//! Usage:
//!   cargo run --example genai_multi_provider_compare

use genai::chat::{ChatMessage, ChatOptions, ChatRequest};
use genai::Client;
use std::time::Instant;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let question =
        "Write a Rust function that calculates the Fibonacci sequence recursively with memoization";

    let models = vec![
        ("qwen3-coder:30b", ""), // Ollama - no API key needed
        ("gpt-4o-mini", "OPENAI_API_KEY"),
        ("claude-3-haiku-20240307", "ANTHROPIC_API_KEY"),
        ("gemini-2.0-flash", "GEMINI_API_KEY"),
    ];

    let chat_req = ChatRequest::new(vec![
        ChatMessage::system("You are an expert Rust programmer. Provide concise, working code."),
        ChatMessage::user(question),
    ]);

    let client = Client::default();
    let chat_options = ChatOptions::default()
        .with_temperature(0.7)
        .with_max_tokens(1024);

    println!("========================================");
    println!("  Multi-Provider Comparison");
    println!("========================================");
    println!("Question: {}", question);
    println!();

    for (model, env_key) in models {
        // Skip if API key required but not set
        if !env_key.is_empty() && std::env::var(env_key).is_err() {
            println!("⏭️  Skipping {} (no {} set)", model, env_key);
            continue;
        }

        println!("\n📍 MODEL: {}", model);
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

        let start = Instant::now();

        match client
            .exec_chat(model, chat_req.clone(), Some(&chat_options))
            .await
        {
            Ok(chat_res) => {
                let duration = start.elapsed();

                if let Some(response) = chat_res.first_text() {
                    println!("{}", response);

                    if let Some(usage) = chat_res.usage {
                        println!();
                        println!("📊 Stats:");
                        println!("   - Time: {:?}", duration);
                        println!("   - Input tokens: {}", usage.prompt_tokens.unwrap_or(0));
                        println!(
                            "   - Output tokens: {}",
                            usage.completion_tokens.unwrap_or(0)
                        );
                        println!("   - Total tokens: {}", usage.total_tokens.unwrap_or(0));
                    }
                } else {
                    println!("(no response)");
                }
            }
            Err(e) => {
                eprintln!("❌ Error: {}", e);
            }
        }
    }

    println!("\n========================================");
    println!("  Comparison Complete");
    println!("========================================");

    Ok(())
}
