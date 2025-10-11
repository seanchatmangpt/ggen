//! List available AI models

use clap::Args;
use ggen_utils::error::Result;
use genai::adapter::AdapterKind;
use genai::Client;

#[derive(Debug, Args)]
pub struct ModelsArgs {
    /// Specific adapter to list models for
    #[arg(short, long)]
    pub adapter: Option<String>,
}

pub async fn run(args: &ModelsArgs) -> Result<()> {
    println!("Listing available AI models...");

    let client = Client::default();

    let adapters = if let Some(adapter_name) = &args.adapter {
        match adapter_name.as_str() {
            "openai" => vec![AdapterKind::OpenAI],
            "ollama" => vec![AdapterKind::Ollama],
            "anthropic" => vec![AdapterKind::Anthropic],
            "gemini" => vec![AdapterKind::Gemini],
            "groq" => vec![AdapterKind::Groq],
            "cohere" => vec![AdapterKind::Cohere],
            _ => {
                println!("Unknown adapter: {}. Available: openai, ollama, anthropic, gemini, groq, cohere", adapter_name);
                return Ok(());
            }
        }
    } else {
        vec![
            AdapterKind::OpenAI,
            AdapterKind::Ollama,
            AdapterKind::Anthropic,
            AdapterKind::Gemini,
            AdapterKind::Groq,
            AdapterKind::Cohere,
        ]
    };

    for adapter in adapters {
        println!("{}", format!("--- Models for {:?}", adapter));
        match client.all_model_names(adapter).await {
            Ok(models) => {
                for model in models {
                    println!("  - {}", model);
                }
            }
            Err(e) => {
                println!("  Error fetching models: {}", e);
            }
        }
    }

    Ok(())
}

