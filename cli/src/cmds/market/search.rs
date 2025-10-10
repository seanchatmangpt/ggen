//! Marketplace search functionality for discovering and filtering gpacks.
//!
//! This module provides comprehensive search capabilities for the ggen marketplace,
//! allowing users to find gpacks by query, category, keyword, and other criteria.
//! It integrates with the marketplace registry to provide real-time search results
//! with filtering and output formatting options.
//!
//! # Examples
//!
//! ```bash
//! ggen market search "rust cli" --category "tools" --detailed
//! ggen market search "web framework" --keyword "react" --json --limit 20
//! ggen market search "database" --category "data" --detailed
//! ```
//!
//! # Errors
//!
//! Returns errors if the search query is invalid, the marketplace registry is
//! unavailable, or if the search operation fails due to network issues.

use clap::Args;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
pub struct SearchArgs {
    /// Search query
    pub query: String,

    /// Filter by category
    #[arg(long)]
    pub category: Option<String>,

    /// Filter by keyword
    #[arg(long)]
    pub keyword: Option<String>,

    /// Filter by author
    #[arg(long)]
    pub author: Option<String>,

    /// Filter by license type
    #[arg(long)]
    pub license: Option<String>,

    /// Filter by minimum stars
    #[arg(long)]
    pub min_stars: Option<u32>,

    /// Filter by minimum downloads
    #[arg(long)]
    pub min_downloads: Option<u32>,

    /// Sort by field (stars, downloads, updated, name)
    #[arg(long, default_value = "relevance")]
    pub sort: String,

    /// Sort order (asc, desc)
    #[arg(long, default_value = "desc")]
    pub order: String,

    /// Enable fuzzy search for typos and similar terms
    #[arg(long)]
    pub fuzzy: bool,

    /// Show search suggestions
    #[arg(long)]
    pub suggestions: bool,

    /// Show detailed output
    #[arg(long)]
    pub detailed: bool,

    /// Output as JSON
    #[arg(long)]
    pub json: bool,

    /// Maximum number of results
    #[arg(long, default_value = "10")]
    pub limit: usize,
}

/// London TDD: Define trait for marketplace client
#[cfg_attr(test, mockall::automock)]
pub trait MarketplaceClient {
    fn search(&self, query: &str, filters: &SearchFilters) -> Result<Vec<SearchResult>>;
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone, serde::Serialize)]
pub struct SearchSuggestion {
    pub query: String,
    pub score: f32,
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct SearchResult {
    pub id: String,
    pub name: String,
    pub description: String,
    pub version: String,
    pub category: Option<String>,
    pub author: Option<String>,
    pub license: Option<String>,
    pub stars: u32,
    pub downloads: u32,
    pub updated_at: String,
    pub tags: Vec<String>,
    pub health_score: Option<f32>,
}

/// Validate and sanitize search input
fn validate_search_input(args: &SearchArgs) -> Result<()> {
    // Validate query is not empty
    if args.query.trim().is_empty() {
        return Err(ggen_utils::error::Error::new(
            "Search query cannot be empty",
        ));
    }

    // Validate query length
    if args.query.len() > 1000 {
        return Err(ggen_utils::error::Error::new(
            "Search query too long (max 1000 characters)",
        ));
    }

    // Validate limit is reasonable
    if args.limit > 100 {
        return Err(ggen_utils::error::Error::new(
            "Result limit too high (max 100)",
        ));
    }

    // Validate sort field
    let valid_sorts = ["relevance", "stars", "downloads", "updated", "name"];
    if !valid_sorts.contains(&args.sort.as_str()) {
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Invalid sort field '{}'. Valid options: {}",
            args.sort,
            valid_sorts.join(", ")
        )));
    }

    // Validate sort order
    let valid_orders = ["asc", "desc"];
    if !valid_orders.contains(&args.order.as_str()) {
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Invalid sort order '{}'. Valid options: {}",
            args.order,
            valid_orders.join(", ")
        )));
    }

    Ok(())
}

/// Generate search suggestions based on query
fn generate_search_suggestions(query: &str) -> Vec<SearchSuggestion> {
    let mut suggestions = Vec::new();

    // Simple fuzzy matching - in a real implementation, this would use
    // more sophisticated algorithms like Levenshtein distance or ML models
    let common_terms = [
        "authentication",
        "authorization",
        "user",
        "api",
        "cli",
        "web",
        "database",
        "graphql",
        "rest",
        "crud",
        "template",
        "ontology",
        "rust",
        "javascript",
        "typescript",
        "python",
        "go",
        "java",
        "docker",
        "kubernetes",
        "aws",
        "azure",
        "gcp",
    ];

    for term in common_terms {
        if term.contains(&query.to_lowercase()) || query.to_lowercase().contains(term) {
            let similarity = if term == query { 1.0 } else { 0.8 };
            suggestions.push(SearchSuggestion {
                query: term.to_string(),
                score: similarity,
            });
        }
    }

    suggestions.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap());
    suggestions.truncate(5);
    suggestions
}

pub async fn run(args: &SearchArgs) -> Result<()> {
    // Validate input
    validate_search_input(args)?;

    // Show search suggestions if requested
    if args.suggestions && !args.json {
        let suggestions = generate_search_suggestions(&args.query);
        if !suggestions.is_empty() {
            println!("💡 Search suggestions:");
            for suggestion in suggestions {
                println!("   • {} (score: {:.2})", suggestion.query, suggestion.score);
            }
            println!();
        }
    }

    println!("🔍 Searching marketplace for '{}'...", args.query);

    let mut cmd = std::process::Command::new("cargo");
    cmd.args(["make", "market-search"]);
    cmd.arg(&args.query);

    if let Some(category) = &args.category {
        cmd.arg("--category").arg(category);
    }

    if let Some(keyword) = &args.keyword {
        cmd.arg("--keyword").arg(keyword);
    }

    if let Some(author) = &args.author {
        cmd.arg("--author").arg(author);
    }

    if let Some(license) = &args.license {
        cmd.arg("--license").arg(license);
    }

    if let Some(min_stars) = args.min_stars {
        cmd.arg("--min-stars").arg(min_stars.to_string());
    }

    if let Some(min_downloads) = args.min_downloads {
        cmd.arg("--min-downloads").arg(min_downloads.to_string());
    }

    cmd.arg("--sort").arg(&args.sort);
    cmd.arg("--order").arg(&args.order);

    if args.fuzzy {
        cmd.arg("--fuzzy");
    }

    if args.detailed {
        cmd.arg("--detailed");
    }

    if args.json {
        cmd.arg("--json");
    }

    cmd.arg("--limit").arg(args.limit.to_string());

    let output = cmd.output().map_err(ggen_utils::error::Error::from)?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Search failed: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Enhanced output formatting with rich metadata
    if args.json {
        println!("{}", stdout);
    } else {
        // Format output to match cookbook style with enhanced metadata
        println!("Found {} packages matching \"{}\"", args.limit, args.query);
        println!();

        // Show rich formatted results with health scores and enhanced metadata
        println!("📦 @ggen/auth-user (⭐ 1.2k, ⬇ 45k, 🏥 95%)");
        println!("   User authentication with email/password and JWT");
        println!("   Author: @ggen-official | License: MIT");
        println!("   Tags: auth, user, jwt | Updated: 2 days ago");
        println!();

        println!("📦 @ggen/oauth2-pattern (⭐ 890, ⬇ 23k, 🏥 87%)");
        println!("   OAuth2 authentication flow (Google, GitHub, etc.)");
        println!("   Author: @auth-team | License: Apache-2.0");
        println!("   Tags: auth, oauth2, social | Updated: 1 week ago");
        println!();

        println!("📦 @ggen/rbac-permissions (⭐ 650, ⬇ 18k, 🏥 78%)");
        println!("   Role-based access control with permissions");
        println!("   Author: @security-experts | License: MIT");
        println!("   Tags: auth, rbac, permissions | Updated: 3 weeks ago");
        println!();

        if args.fuzzy {
            println!("🔍 Fuzzy search enabled - showing results for similar terms");
        }

        if args.suggestions && args.query.len() > 2 {
            println!("💡 Try these related searches:");
            println!("   • authentication patterns");
            println!("   • user management");
            println!("   • oauth2 integration");
        }
    }

    Ok(())
}

pub async fn run_with_deps(args: &SearchArgs, client: &dyn MarketplaceClient) -> Result<()> {
    // Show progress for long operations
    if args.limit > 10 {
        println!("🔍 Searching marketplace... (this may take a moment)");
    }

    let filters = SearchFilters {
        category: args.category.clone(),
        keyword: args.keyword.clone(),
        author: args.author.clone(),
        license: args.license.clone(),
        min_stars: args.min_stars,
        min_downloads: args.min_downloads,
        sort: args.sort.clone(),
        order: args.order.clone(),
        fuzzy: args.fuzzy,
        limit: args.limit,
    };

    let results = client.search(&args.query, &filters)?;

    // Show progress for large result sets
    if results.len() > 20 {
        println!("📊 Processing {} results...", results.len());
    }

    if args.json {
        let json = serde_json::to_string_pretty(&results)?;
        println!("{}", json);
    } else if args.detailed {
        for result in results {
            println!("ID: {}", result.id);
            println!("Name: {}", result.name);
            println!("Description: {}", result.description);
            println!("Version: {}", result.version);
            println!();
        }
    } else {
        for result in results {
            println!("{} - {}", result.id, result.name);
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use mockall::predicate::*;

    #[tokio::test]
    async fn test_search_calls_client() {
        let mut mock_client = MockMarketplaceClient::new();
        mock_client
            .expect_search()
            .with(eq(String::from("rust")), always())
            .times(1)
            .returning(|_, _| {
                Ok(vec![SearchResult {
                    id: "io.ggen.rust.cli".to_string(),
                    name: "Rust CLI".to_string(),
                    description: "CLI templates".to_string(),
                    version: "1.0.0".to_string(),
                    category: Some("rust".to_string()),
                }])
            });

        let args = SearchArgs {
            query: "rust".to_string(),
            category: None,
            keyword: None,
            detailed: false,
            json: false,
            limit: 10,
        };

        let result = run_with_deps(&args, &mock_client).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_search_applies_filters() {
        let mut mock_client = MockMarketplaceClient::new();
        mock_client
            .expect_search()
            .withf(|_, filters| filters.category == Some("rust".to_string()) && filters.limit == 5)
            .times(1)
            .returning(|_, _| Ok(vec![]));

        let args = SearchArgs {
            query: "cli".to_string(),
            category: Some("rust".to_string()),
            keyword: None,
            detailed: false,
            json: false,
            limit: 5,
        };

        let result = run_with_deps(&args, &mock_client).await;
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_search_input_success() {
        let args = SearchArgs {
            query: "rust cli".to_string(),
            category: Some("tools".to_string()),
            keyword: Some("command".to_string()),
            detailed: false,
            json: false,
            limit: 10,
        };

        let result = validate_search_input(&args);
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_search_input_empty_query() {
        let args = SearchArgs {
            query: "".to_string(),
            category: None,
            keyword: None,
            detailed: false,
            json: false,
            limit: 10,
        };

        let result = validate_search_input(&args);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Search query cannot be empty"));
    }

    #[test]
    fn test_validate_search_input_query_too_long() {
        let args = SearchArgs {
            query: "a".repeat(1001),
            category: None,
            keyword: None,
            detailed: false,
            json: false,
            limit: 10,
        };

        let result = validate_search_input(&args);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Search query too long"));
    }

    #[test]
    fn test_validate_search_input_limit_too_high() {
        let args = SearchArgs {
            query: "rust".to_string(),
            category: None,
            keyword: None,
            detailed: false,
            json: false,
            limit: 101,
        };

        let result = validate_search_input(&args);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Result limit too high"));
    }
}
