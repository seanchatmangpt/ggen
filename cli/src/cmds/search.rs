use anyhow::Result;
use clap::Args;
use colored::*;
use rgen_core::RegistryClient;
use serde_json;

#[derive(Args, Debug)]
pub struct SearchArgs {
    /// Search query (searches in name, description, keywords, and tags)
    pub query: String,

    /// Filter by category (e.g., "rust", "python", "web", "cli")
    #[arg(short, long)]
    pub category: Option<String>,

    /// Filter by keyword (e.g., "api", "database", "auth")
    #[arg(short, long)]
    pub keyword: Option<String>,

    /// Filter by author/owner
    #[arg(short, long)]
    pub author: Option<String>,

    /// Show only stable versions
    #[arg(long)]
    pub stable: bool,

    /// Limit number of results
    #[arg(short, long, default_value = "20")]
    pub limit: usize,

    /// Output results as JSON
    #[arg(long)]
    pub json: bool,

    /// Show detailed information for each result
    #[arg(short, long)]
    pub detailed: bool,
}

pub async fn run(args: &SearchArgs) -> Result<()> {
    let registry_client = RegistryClient::new()?;

    // Build search parameters
    let search_params = rgen_core::registry::SearchParams {
        query: &args.query,
        category: args.category.as_deref(),
        keyword: args.keyword.as_deref(),
        author: args.author.as_deref(),
        stable_only: args.stable,
        limit: args.limit,
    };

    let results = registry_client.advanced_search(&search_params).await?;

    if args.json {
        // Output JSON format
        let json_output = serde_json::to_string_pretty(&results)?;
        println!("{}", json_output);
    } else {
        // Output human-readable format
        if results.is_empty() {
            println!("No rpacks found matching your criteria");
            return Ok(());
        }

        println!("Found {} rpack(s):", results.len());
        println!();

        if args.detailed {
            print_detailed_results(&results);
        } else {
            print_summary_results(&results);
        }
    }

    Ok(())
}

fn print_summary_results(results: &[rgen_core::SearchResult]) {
    // Header with colors
    println!(
        "{:<40} {:<12} {:<20} {}",
        "ID".bold(),
        "LATEST".bold(),
        "TAGS".bold(),
        "DESCRIPTION".bold()
    );
    println!("{}", "-".repeat(80).dimmed());

    // Results
    for result in results {
        let tags = result
            .tags
            .iter()
            .map(|tag| format!("{}", tag.cyan()))
            .collect::<Vec<_>>()
            .join(", ");

        let description = if result.description.len() > 30 {
            format!("{}...", &result.description[..27])
        } else {
            result.description.clone()
        };

        println!(
            "{:<40} {:<12} {:<20} {}",
            result.id.green(),
            result.latest_version.yellow(),
            tags,
            description
        );
    }
}

fn print_detailed_results(results: &[rgen_core::SearchResult]) {
    for (i, result) in results.iter().enumerate() {
        if i > 0 {
            println!();
        }

        println!("{}", format!("{}. {}", i + 1, result.name).bold().green());
        println!("  ID: {}", result.id.cyan());
        println!("  Version: {}", result.latest_version.yellow());
        println!("  Description: {}", result.description);

        if !result.tags.is_empty() {
            let tags = result
                .tags
                .iter()
                .map(|tag| format!("{}", tag.cyan()))
                .collect::<Vec<_>>()
                .join(", ");
            println!("  Tags: {}", tags);
        }

        if let Some(category) = &result.category {
            println!("  Category: {}", category.magenta());
        }

        if let Some(author) = &result.author {
            println!("  Author: {}", author.blue());
        }

        if let Some(downloads) = result.downloads {
            println!("  Downloads: {}", format!("{}", downloads).dimmed());
        }

        if let Some(updated) = &result.updated {
            println!("  Updated: {}", updated.format("%Y-%m-%d %H:%M:%S UTC"));
        }
    }
}
