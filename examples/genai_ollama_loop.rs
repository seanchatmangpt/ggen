//! Ollama qwen3-coder:30b Loop Example
//!
//! This example demonstrates using the genai library to interact with
//! Ollama's qwen3-coder:30b model in a loop for code generation tasks.
//!
//! Prerequisites:
//!   1. Ollama installed and running: https://ollama.ai
//!   2. Pull the model: `ollama pull qwen3-coder:30b`
//!   3. Add genai to Cargo.toml: genai = "0.4"
//!
//! Usage:
//!   cargo run --example genai_ollama_loop

use genai::chat::{ChatMessage, ChatOptions, ChatRequest};
use genai::Client;
use std::io::{self, Write};

const MODEL: &str = "qwen3-coder:30b";

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("========================================");
    println!("  Ollama qwen3-coder:30b Loop Example");
    println!("========================================");
    println!("Model: {}", MODEL);
    println!("Provider: Ollama (via genai)");
    println!();
    println!("Commands:");
    println!("  - Type your coding question");
    println!("  - 'quit' or 'exit' to stop");
    println!("  - 'clear' to clear conversation history");
    println!("========================================");
    println!();

    // Check if Ollama is running
    match check_ollama_status().await {
        Ok(status) => println!("✅ Ollama status: {}", status),
        Err(e) => {
            eprintln!("❌ Ollama not accessible: {}", e);
            eprintln!("   Please ensure:");
            eprintln!("   1. Ollama is installed (https://ollama.ai)");
            eprintln!("   2. Ollama is running (ollama serve)");
            eprintln!("   3. Model is pulled (ollama pull qwen3-coder:30b)");
            return Err(e);
        }
    }

    // Initialize genai client
    let client = Client::default();

    // Configure chat options for code generation
    let chat_options = ChatOptions::default()
        .with_temperature(0.7)
        .with_max_tokens(2048);

    // Conversation history
    let mut conversation: Vec<ChatMessage> = vec![ChatMessage::system(
        "You are qwen3-coder:30b, an expert coding assistant. \
             Provide clear, concise, and well-documented code examples. \
             Explain your reasoning and best practices.",
    )];

    // Main interaction loop
    loop {
        // Get user input
        print!("\n💬 You: ");
        io::stdout().flush()?;

        let mut user_input = String::new();
        io::stdin().read_line(&mut user_input)?;
        let user_input = user_input.trim();

        // Handle commands
        match user_input.to_lowercase().as_str() {
            "quit" | "exit" => {
                println!("\n👋 Goodbye!");
                break;
            }
            "clear" => {
                conversation.truncate(1); // Keep only system message
                println!("🧹 Conversation cleared!");
                continue;
            }
            "" => continue,
            _ => {}
        }

        // Add user message to conversation
        conversation.push(ChatMessage::user(user_input));

        // Create chat request
        let chat_req = ChatRequest::new(conversation.clone());

        // Call Ollama via genai
        print!("🤖 qwen3-coder:30b: ");
        io::stdout().flush()?;

        match client.exec_chat(MODEL, chat_req, Some(&chat_options)).await {
            Ok(chat_res) => {
                if let Some(response_text) = chat_res.first_text() {
                    println!("{}", response_text);

                    // Add assistant response to conversation
                    conversation.push(ChatMessage::assistant(response_text));

                    // Show token usage if available
                    if let Some(usage) = chat_res.usage {
                        println!();
                        println!(
                            "📊 Tokens - Input: {}, Output: {}, Total: {}",
                            usage.prompt_tokens.unwrap_or(0),
                            usage.completion_tokens.unwrap_or(0),
                            usage.total_tokens.unwrap_or(0)
                        );
                    }
                } else {
                    println!("(no response)");
                }
            }
            Err(e) => {
                eprintln!("❌ Error: {}", e);
                eprintln!("   This could mean:");
                eprintln!("   - Model not loaded (run: ollama pull qwen3-coder:30b)");
                eprintln!("   - Ollama not running (run: ollama serve)");
                eprintln!("   - Network/connection issue");

                // Remove the failed message from conversation
                conversation.pop();
            }
        }
    }

    Ok(())
}

/// Check if Ollama is accessible
async fn check_ollama_status() -> Result<String, Box<dyn std::error::Error>> {
    let client = reqwest::Client::new();
    let response = client.get("http://localhost:11434/api/tags").send().await?;

    if response.status().is_success() {
        let body: serde_json::Value = response.json().await?;
        let models = body["models"].as_array().map(|m| m.len()).unwrap_or(0);
        Ok(format!("Running ({} models available)", models))
    } else {
        Err("Ollama not responding".into())
    }
}
