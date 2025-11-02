//! Domain logic for marketplace package search
//!
//! This module contains the core business logic for searching packages,
//! separated from CLI concerns for better testability and reusability.

use clap::Args;
use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};

/// Search command arguments
#[derive(Debug, Args)]
pub struct SearchArgs {
    /// Search query
    pub query: String,

    /// Filter by category
    #[arg(short = 'c', long)]
    pub category: Option<String>,

    /// Filter by keyword
    #[arg(short = 'k', long)]
    pub keyword: Option<String>,

    /// Filter by author
    #[arg(short = 'a', long)]
    pub author: Option<String>,

    /// Enable fuzzy search
    #[arg(long)]
    pub fuzzy: bool,

    /// Show detailed information
    #[arg(short = 'd', long)]
    pub detailed: bool,

    /// Output as JSON
    #[arg(short = 'j', long)]
    pub json: bool,

    /// Limit number of results
    #[arg(short = 'l', long, default_value = "10")]
    pub limit: usize,
}

/// Search filters for package discovery
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SearchFilters {
    pub category: Option<String>,
    pub keyword: Option<String>,
    pub author: Option<String>,
    pub license: Option<String>,
    pub min_stars: Option<u32>,
    pub min_downloads: Option<u32>,
    pub sort: String,
    pub order: String,
    pub fuzzy: bool,
    pub limit: usize,
}

impl SearchFilters {
    pub fn new() -> Self {
        Self {
            sort: "relevance".to_string(),
            order: "desc".to_string(),
            limit: 10,
            ..Default::default()
        }
    }

    pub fn with_category(mut self, category: impl Into<String>) -> Self {
        self.category = Some(category.into());
        self
    }

    pub fn with_limit(mut self, limit: usize) -> Self {
        self.limit = limit;
        self
    }

    pub fn with_fuzzy(mut self, fuzzy: bool) -> Self {
        self.fuzzy = fuzzy;
        self
    }
}

/// Search result representing a package
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchResult {
    pub id: String,
    pub name: String,
    pub version: String,
    pub description: String,
    pub author: Option<String>,
    pub category: Option<String>,
    pub tags: Vec<String>,
    pub stars: u32,
    pub downloads: u32,
}

/// Search for packages in the marketplace
///
/// This is a placeholder implementation for Phase 1.
/// Phase 2 will implement actual search logic with registry integration.
pub async fn search_packages(
    query: &str,
    filters: &SearchFilters,
) -> Result<Vec<SearchResult>> {
    // Placeholder: Return empty results for now
    // In Phase 2, this will query the actual marketplace registry
    let _ = query;
    let _ = filters;

    Ok(vec![])
}

/// Search for packages and display results
///
/// This function bridges the CLI to the domain layer.
pub async fn search_and_display(
    query: &str,
    category: Option<&str>,
    keyword: Option<&str>,
    author: Option<&str>,
    fuzzy: bool,
    detailed: bool,
    json: bool,
    limit: usize,
) -> Result<()> {
    // Build search filters
    let mut filters = SearchFilters::new()
        .with_limit(limit)
        .with_fuzzy(fuzzy);

    if let Some(cat) = category {
        filters = filters.with_category(cat);
    }
    if let Some(kw) = keyword {
        filters.keyword = Some(kw.to_string());
    }
    if let Some(auth) = author {
        filters.author = Some(auth.to_string());
    }

    // Search packages
    let results = search_packages(query, &filters).await?;

    // Display results
    if json {
        let json_output = serde_json::to_string_pretty(&results)?;
        println!("{}", json_output);
    } else if results.is_empty() {
        println!("No packages found matching '{}'", query);
        println!("\nTry:");
        println!("  - Using broader search terms");
        println!("  - Removing filters");
        println!("  - Using --fuzzy for typo tolerance");
    } else {
        println!("Found {} package(s) matching '{}':\n", results.len(), query);

        for result in results {
            println!("📦 {} v{}", result.name, result.version);
            println!("   {}", result.description);

            if detailed {
                if let Some(author) = result.author {
                    println!("   Author: {}", author);
                }
                if let Some(category) = result.category {
                    println!("   Category: {}", category);
                }
                if !result.tags.is_empty() {
                    println!("   Tags: {}", result.tags.join(", "));
                }
                println!("   ⭐ {} stars  📥 {} downloads", result.stars, result.downloads);
            }

            println!();
        }
    }

    Ok(())
}

/// Run search command (sync wrapper for CLI)
pub fn run(args: &SearchArgs) -> Result<()> {
    crate::runtime::block_on(async {
        search_and_display(
            &args.query,
            args.category.as_deref(),
            args.keyword.as_deref(),
            args.author.as_deref(),
            args.fuzzy,
            args.detailed,
            args.json,
            args.limit,
        )
        .await
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_search_filters_builder() {
        let filters = SearchFilters::new()
            .with_category("web")
            .with_limit(5)
            .with_fuzzy(true);

        assert_eq!(filters.category, Some("web".to_string()));
        assert_eq!(filters.limit, 5);
        assert!(filters.fuzzy);
        assert_eq!(filters.sort, "relevance");
    }

    #[tokio::test]
    async fn test_search_packages_placeholder() {
        let filters = SearchFilters::new();
        let results = search_packages("test", &filters).await.unwrap();
        assert!(results.is_empty()); // Placeholder returns empty
    }
}
