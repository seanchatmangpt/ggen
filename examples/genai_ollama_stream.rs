//! Ollama qwen3-coder:30b Streaming Example
//!
//! This example demonstrates streaming responses from Ollama's qwen3-coder:30b
//! model using the genai library for real-time code generation.
//!
//! Prerequisites:
//!   1. Ollama installed and running
//!   2. Model pulled: `ollama pull qwen3-coder:30b`
//!
//! Usage:
//!   cargo run --example genai_ollama_stream

use genai::chat::printer::{print_chat_stream, PrintChatStreamOptions};
use genai::chat::{ChatMessage, ChatOptions, ChatRequest};
use genai::Client;
use std::io::{self, Write};

const MODEL: &str = "qwen3-coder:30b";

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("========================================");
    println!("  Ollama Streaming Example");
    println!("========================================");
    println!("Model: {}", MODEL);
    println!("Mode: Streaming (real-time responses)");
    println!();
    println!("Commands:");
    println!("  - Type your question");
    println!("  - 'quit' or 'exit' to stop");
    println!("========================================");
    println!();

    let client = Client::default();

    let chat_options = ChatOptions::default()
        .with_temperature(0.7)
        .with_max_tokens(2048);

    let print_options = PrintChatStreamOptions::from_print_events(false);

    let mut conversation: Vec<ChatMessage> = vec![ChatMessage::system(
        "You are qwen3-coder:30b, an expert Rust coding assistant. \
             Provide clear, well-documented code with explanations.",
    )];

    loop {
        print!("\n💬 You: ");
        io::stdout().flush()?;

        let mut user_input = String::new();
        io::stdin().read_line(&mut user_input)?;
        let user_input = user_input.trim();

        match user_input.to_lowercase().as_str() {
            "quit" | "exit" => {
                println!("\n👋 Goodbye!");
                break;
            }
            "" => continue,
            _ => {}
        }

        conversation.push(ChatMessage::user(user_input));

        let chat_req = ChatRequest::new(conversation.clone());

        print!("🤖 qwen3-coder:30b: ");
        io::stdout().flush()?;

        match client
            .exec_chat_stream(MODEL, chat_req, Some(&chat_options))
            .await
        {
            Ok(chat_stream) => {
                // Print stream in real-time
                match print_chat_stream(chat_stream, Some(&print_options)).await {
                    Ok(final_response) => {
                        println!(); // New line after stream completes

                        // Add response to conversation history
                        if let Some(response_text) = final_response.first_text() {
                            conversation.push(ChatMessage::assistant(response_text));
                        }
                    }
                    Err(e) => {
                        eprintln!("\n❌ Stream error: {}", e);
                        conversation.pop(); // Remove failed message
                    }
                }
            }
            Err(e) => {
                eprintln!("❌ Error starting stream: {}", e);
                conversation.pop();
            }
        }
    }

    Ok(())
}
