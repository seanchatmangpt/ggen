//! Advanced AI Integration Demo
//!
//! This example demonstrates comprehensive usage of ggen-ai features:
//! - Multi-provider LLM support (OpenAI, Anthropic, Ollama)
//! - TemplateGenerator, SparqlGenerator, OntologyGenerator
//! - RefactorAssistant, NaturalSearchGenerator
//! - LLM caching and streaming responses
//! - Production-ready error handling

use anyhow::Result;
use colored::*;
use std::io::{self, Write};

mod multi_provider;
mod natural_search;
mod ontology_gen;
mod refactor;
mod sparql_gen;
mod streaming;
mod template_gen;

#[tokio::main]
async fn main() -> Result<()> {
    // Load environment variables
    dotenvy::dotenv().ok();

    // Initialize logging
    ggen_ai::init_logging();

    println!("{}", "\n╔═══════════════════════════════════════════════════╗".bright_cyan());
    println!("{}", "║   Advanced AI Integration Demo - ggen-ai v1.0   ║".bright_cyan());
    println!("{}", "╚═══════════════════════════════════════════════════╝\n".bright_cyan());

    loop {
        display_menu();

        print!("\n{}", "Select an option: ".bright_yellow());
        io::stdout().flush()?;

        let mut input = String::new();
        io::stdin().read_line(&mut input)?;

        match input.trim() {
            "1" => {
                println!("\n{}", "=== Multi-Provider LLM Examples ===".bright_green());
                multi_provider::run_examples().await?;
            }
            "2" => {
                println!("\n{}", "=== Template Generation Examples ===".bright_green());
                template_gen::run_examples().await?;
            }
            "3" => {
                println!("\n{}", "=== SPARQL Query Generation Examples ===".bright_green());
                sparql_gen::run_examples().await?;
            }
            "4" => {
                println!("\n{}", "=== Ontology Generation Examples ===".bright_green());
                ontology_gen::run_examples().await?;
            }
            "5" => {
                println!("\n{}", "=== Refactoring Assistant Examples ===".bright_green());
                refactor::run_examples().await?;
            }
            "6" => {
                println!("\n{}", "=== Natural Search Examples ===".bright_green());
                natural_search::run_examples().await?;
            }
            "7" => {
                println!("\n{}", "=== Streaming Response Examples ===".bright_green());
                streaming::run_examples().await?;
            }
            "8" => {
                println!("\n{}", "=== All Examples (Sequential) ===".bright_green());
                run_all_examples().await?;
            }
            "0" => {
                println!("\n{}", "Goodbye!".bright_cyan());
                break;
            }
            _ => {
                println!("\n{}", "Invalid option. Please try again.".bright_red());
            }
        }

        println!("\n{}", "Press Enter to continue...".dimmed());
        let mut _pause = String::new();
        io::stdin().read_line(&mut _pause)?;
    }

    Ok(())
}

fn display_menu() {
    println!("\n{}", "Available Examples:".bright_white().bold());
    println!("  {} Multi-Provider LLM Usage (OpenAI, Anthropic, Ollama)", "1.".bright_blue());
    println!("  {} Template Generator Examples", "2.".bright_blue());
    println!("  {} SPARQL Query Generator Examples", "3.".bright_blue());
    println!("  {} Ontology Generator Examples", "4.".bright_blue());
    println!("  {} Refactoring Assistant Examples", "5.".bright_blue());
    println!("  {} Natural Language Search Examples", "6.".bright_blue());
    println!("  {} Streaming Response Examples", "7.".bright_blue());
    println!("  {} Run All Examples", "8.".bright_blue());
    println!("  {} Exit", "0.".bright_red());
}

async fn run_all_examples() -> Result<()> {
    println!("{}", "\nRunning all examples sequentially...\n".bright_yellow());

    multi_provider::run_examples().await?;
    template_gen::run_examples().await?;
    sparql_gen::run_examples().await?;
    ontology_gen::run_examples().await?;
    refactor::run_examples().await?;
    natural_search::run_examples().await?;
    streaming::run_examples().await?;

    println!("{}", "\n✓ All examples completed successfully!".bright_green());
    Ok(())
}
