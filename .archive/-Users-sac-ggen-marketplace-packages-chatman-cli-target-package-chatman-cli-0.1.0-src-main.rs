//! ChatMan CLI - Main entry point

use anyhow::Result;
use chatman_cli::{ChatManager, Config, Message};
use clap::{Parser, Subcommand};
use colored::*;
use std::path::PathBuf;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    #[arg(short, long, global = true)]
    verbose: bool,
}

#[derive(Subcommand)]
enum Commands {
    Chat {
        #[arg(short, long, default_value = "openai")]
        provider: String,
        #[arg(short, long, default_value = "gpt-3.5-turbo")]
        model: String,
        #[arg(short, long)]
        export: Option<PathBuf>,
    },
    Ask {
        question: String,
        #[arg(short, long, default_value = "openai")]
        provider: String,
        #[arg(short, long, default_value = "gpt-3.5-turbo")]
        model: String,
    },
    Info,
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    if cli.verbose {
        tracing_subscriber::fmt()
            .with_env_filter("chatman_cli=debug")
            .init();
    } else {
        tracing_subscriber::fmt()
            .with_env_filter("chatman_cli=info")
            .init();
    }

    match cli.command {
        Commands::Chat { provider, model, export } => {
            run_chat(provider, model, export).await?;
        }
        Commands::Ask { question, provider, model } => {
            run_ask(question, provider, model).await?;
        }
        Commands::Info => {
            show_info();
        }
    }

    Ok(())
}

async fn run_chat(provider: String, model: String, export: Option<PathBuf>) -> Result<()> {
    println!("{}", "ChatMan CLI - Interactive Chat".bright_cyan().bold());
    println!("Provider: {} | Model: {}\n", provider.bright_yellow(), model.bright_yellow());

    let config = Config {
        provider,
        model,
        ..Default::default()
    };

    let mut manager = ChatManager::new(config)?;

    loop {
        use dialoguer::{Input, Confirm};

        let input: String = Input::new()
            .with_prompt("You")
            .interact_text()?;

        if input.trim().is_empty() {
            continue;
        }

        if input.trim().eq_ignore_ascii_case("exit") || input.trim().eq_ignore_ascii_case("quit") {
            break;
        }

        let message = Message::user(input);
        let response = manager.send_message(message).await?;

        println!("{}: {}\n", "Assistant".bright_green().bold(), response.content);

        if !Confirm::new()
            .with_prompt("Continue?")
            .default(true)
            .interact()?
        {
            break;
        }
    }

    if let Some(path) = export {
        let json = manager.export_json()?;
        std::fs::write(&path, json)?;
        println!("\n{} Conversation exported to: {}", "✓".bright_green(), path.display());
    }

    Ok(())
}

async fn run_ask(question: String, provider: String, model: String) -> Result<()> {
    let config = Config {
        provider,
        model,
        ..Default::default()
    };

    let mut manager = ChatManager::new(config)?;
    let message = Message::user(question);
    let response = manager.send_message(message).await?;

    println!("{}", response.content);

    Ok(())
}

fn show_info() {
    println!("{}", "ChatMan CLI".bright_cyan().bold());
    println!("Version: {}", env!("CARGO_PKG_VERSION"));
    println!("Authors: {}", env!("CARGO_PKG_AUTHORS"));
    println!("\n{}", env!("CARGO_PKG_DESCRIPTION"));
    println!("\nFeatures:");
    println!("  • Knowledge Hook integration");
    println!("  • Multi-provider AI support");
    println!("  • Semantic conversation understanding");
    println!("  • Production-grade error handling");
    println!("\nRepository: {}", env!("CARGO_PKG_REPOSITORY"));
}
