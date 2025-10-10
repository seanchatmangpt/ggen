use anyhow::Result;
use clap::Args;
use colored::*;
use ggen_core::RegistryClient;
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
    #[arg(short = 'n', long, default_value = "10")]
    pub limit: usize,

    /// Output results as JSON
    #[arg(long)]
    pub json: bool,

    /// Show detailed information for each result
    #[arg(short, long)]
    pub detailed: bool,

    /// Sort by: relevance, downloads, updated, name
    #[arg(long, default_value = "relevance")]
    pub sort: String,
}

pub async fn run(args: &SearchArgs) -> Result<()> {
    let registry_client = RegistryClient::new()?;

    // Build search parameters
    let search_params = ggen_core::registry::SearchParams {
        query: &args.query,
        category: args.category.as_deref(),
        keyword: args.keyword.as_deref(),
        author: args.author.as_deref(),
        stable_only: args.stable,
        limit: args.limit,
    };

    let mut results = registry_client.advanced_search(&search_params).await?;

    // Sort results
    match args.sort.as_str() {
        "downloads" => {
            results.sort_by(|a, b| b.downloads.unwrap_or(0).cmp(&a.downloads.unwrap_or(0)))
        }
        "updated" => results.sort_by(|a, b| b.updated.cmp(&a.updated)),
        "name" => results.sort_by(|a, b| a.name.cmp(&b.name)),
        _ => {} // relevance (default order from search)
    }

    if args.json {
        // Output JSON format (for scripts/tools)
        let json_output = serde_json::to_string_pretty(&results)?;
        println!("{}", json_output);
    } else {
        // Human-friendly output (Rust core team style)
        print_rust_style_results(&results, args);
    }

    Ok(())
}

fn print_rust_style_results(results: &[ggen_core::SearchResult], args: &SearchArgs) {
    if results.is_empty() {
        println!(
            "{} No gpacks found matching '{}'",
            "note:".bold().cyan(),
            args.query.bold()
        );
        println!();
        println!("{}", "Try:".bold());
        println!("  • Broaden your search terms");
        println!("  • Use 'ggen categories' to browse available categories");
        println!("  • Check 'ggen search --help' for filter options");
        return;
    }

    // Header with result count
    let count_str = if results.len() == 1 {
        "1 gpack".to_string()
    } else {
        format!("{} gpacks", results.len())
    };

    println!(
        "{} {}",
        count_str.bold().green(),
        if args.category.is_some() || args.keyword.is_some() || args.author.is_some() {
            format!("(filtered)")
        } else {
            String::new()
        }
    );
    println!();

    if args.detailed {
        print_detailed_results(results);
    } else {
        print_compact_table(results);
    }

    // Helpful footer (like cargo search)
    println!();
    if results.len() >= args.limit {
        println!(
            "{} Showing {} results. Use {} to see more.",
            "note:".bold().cyan(),
            args.limit,
            format!("--limit {}", args.limit + 10).bold()
        );
    }

    // Show quick actions
    if !results.is_empty() {
        println!("{}", "Next steps:".bold());
        let first_id = &results[0].id;
        println!(
            "  {} {}",
            "Install:".bold(),
            format!("ggen add {}", first_id).cyan()
        );
        println!(
            "  {} {}",
            "Details:".bold(),
            format!("ggen show {}", first_id).cyan()
        );
        if !args.detailed {
            println!(
                "  {} {}",
                "Detailed view:".bold(),
                format!("ggen search {} --detailed", args.query).cyan()
            );
        }
    }
}

fn print_compact_table(results: &[ggen_core::SearchResult]) {
    // Calculate column widths dynamically
    let max_name_width = results
        .iter()
        .map(|r| r.name.len())
        .max()
        .unwrap_or(20)
        .min(40); // Cap at 40 chars

    // Header
    println!(
        "{:<width$}  {:<8}  {:<30}  {}",
        "NAME".bold(),
        "VERSION".bold(),
        "DESCRIPTION".bold(),
        "TAGS".bold(),
        width = max_name_width
    );
    println!("{}", "─".repeat(100).dimmed());

    // Results
    for result in results {
        let name = if result.name.len() > max_name_width {
            format!("{}…", &result.name[..max_name_width - 1])
        } else {
            result.name.clone()
        };

        let desc = if result.description.len() > 30 {
            format!("{}…", &result.description[..29])
        } else {
            result.description.clone()
        };

        // Show up to 3 tags
        let tags: Vec<_> = result
            .tags
            .iter()
            .take(3)
            .map(|t| t.cyan().to_string())
            .collect();
        let tag_str = if result.tags.len() > 3 {
            format!("{} +{}", tags.join(" "), result.tags.len() - 3)
        } else {
            tags.join(" ")
        };

        // Add download indicator for popular packages
        let popularity = if let Some(downloads) = result.downloads {
            if downloads > 1000 {
                format!(" {}", "⭐".yellow())
            } else {
                String::new()
            }
        } else {
            String::new()
        };

        println!(
            "{:<width$}  {:<8}  {:<30}  {}{}",
            name.green(),
            result.latest_version.dimmed(),
            desc,
            tag_str,
            popularity,
            width = max_name_width
        );
    }
}

fn print_detailed_results(results: &[ggen_core::SearchResult]) {
    for (i, result) in results.iter().enumerate() {
        if i > 0 {
            println!();
            println!("{}", "─".repeat(80).dimmed());
            println!();
        }

        // Package header (like cargo info)
        println!(
            "{} {}",
            result.name.bold().green(),
            result.latest_version.yellow()
        );
        println!("{}", result.id.dimmed());

        if !result.description.is_empty() {
            println!();
            println!("{}", result.description);
        }

        println!();

        // Metadata table
        if !result.tags.is_empty() {
            let tags = result
                .tags
                .iter()
                .map(|t| t.cyan().to_string())
                .collect::<Vec<_>>()
                .join(", ");
            println!("{:>12}: {}", "Tags".bold(), tags);
        }

        if let Some(category) = &result.category {
            println!("{:>12}: {}", "Category".bold(), category.magenta());
        }

        if let Some(author) = &result.author {
            println!("{:>12}: {}", "Author".bold(), author.blue());
        }

        if let Some(downloads) = result.downloads {
            let formatted = if downloads > 1000 {
                format!("{:.1}k", downloads as f32 / 1000.0)
            } else {
                downloads.to_string()
            };
            println!("{:>12}: {}", "Downloads".bold(), formatted.dimmed());
        }

        if let Some(updated) = &result.updated {
            println!(
                "{:>12}: {}",
                "Updated".bold(),
                updated.format("%Y-%m-%d").to_string().dimmed()
            );
        }

        // Installation command
        println!();
        println!(
            "{:>12}  {}",
            "Install:".bold(),
            format!("ggen add {}", result.id).cyan()
        );
    }
}
