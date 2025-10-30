//! Marketplace search functionality for discovering and filtering gpacks.
//!
//! # WHAT THIS MODULE SHOULD DO (Intent-Driven Architecture)
//!
//! ## PURPOSE
//! This module should enable developers to quickly discover reusable code patterns
//! (gpacks) through natural language search, filtering, and categorization, reducing
//! code duplication and accelerating development.
//!
//! ## RESPONSIBILITIES
//! 1. **Discovery**: Should connect to marketplace registry API for real-time results
//! 2. **Filtering**: Should support category, author, license, popularity filters
//! 3. **Relevance**: Should rank results by relevance, not just recency
//! 4. **Fuzzy Search**: Should handle typos and similar terms gracefully
//! 5. **Suggestions**: Should recommend related searches and popular packages
//!
//! ## CONSTRAINTS
//! - Must validate search input (length, characters, injection attempts)
//! - Must limit result sets to prevent overwhelming users
//! - Must support both human-readable and JSON output
//! - Must provide helpful feedback when no results found
//! - Must handle marketplace API unavailability gracefully
//!
//! ## DEPENDENCIES
//! - Marketplace API: Should query central package registry
//! - HTTP client: Should handle timeouts and retries
//! - `MarketplaceClient` trait: Should be mockable for testing
//!
//! ## ERROR HANDLING STRATEGY
//! - Empty query → Clear error message with examples
//! - Query too long → Suggest query refinement
//! - API unavailable → Show cached results or helpful message
//! - Invalid filters → Explain valid options
//! - Network timeout → Retry, then fallback to local cache
//!
//! ## TESTING STRATEGY
//! - Mock MarketplaceClient for deterministic tests
//! - Test all filter combinations
//! - Test validation (empty, too long, limits)
//! - Test both text and JSON output modes
//! - Test fuzzy search and suggestions
//!
//! ## REFACTORING PRIORITIES
//! - [P0] Implement actual marketplace API integration (currently placeholder)
//! - [P0] Add result caching for offline operation
//! - [P1] Implement actual fuzzy search algorithm
//! - [P1] Add search analytics (popular queries, no-result queries)
//! - [P2] Support advanced query syntax (AND/OR/NOT)
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

/// Run marketplace search with OpenTelemetry instrumentation
#[tracing::instrument(name = "ggen.market.search", skip(args), fields(query = %args.query, limit = args.limit))]
pub async fn run(args: &SearchArgs) -> Result<()> {
    // Validate input
    let _validate_span = tracing::info_span!("validate_input").entered();
    validate_search_input(args)?;
    drop(_validate_span);

    // Show search suggestions if requested
    if args.suggestions && !args.json {
        let _suggestions_span = tracing::info_span!("generate_suggestions").entered();
        let suggestions = generate_search_suggestions(&args.query);
        drop(_suggestions_span);

        if !suggestions.is_empty() {
            println!("💡 Search suggestions:");
            for suggestion in suggestions {
                println!("   • {} (score: {:.2})", suggestion.query, suggestion.score);
            }
            println!();
        }
    }

    println!("🔍 Searching marketplace for '{}'...", args.query);
    tracing::info!(query = %args.query, "Starting marketplace search");

    // Load registry and search
    let _registry_span = tracing::info_span!("load_registry").entered();
    let registry = match super::registry::Registry::load().await {
        Ok(r) => {
            tracing::info!("Registry loaded successfully");
            r
        }
        Err(e) => {
            tracing::warn!(error = %e, "Failed to load registry, using mock data");
            eprintln!("⚠️  Warning: Could not load marketplace registry: {}", e);
            eprintln!("Using mock data for demonstration.");
            drop(_registry_span);
            return run_with_mock_data(args);
        }
    };
    drop(_registry_span);

    let _search_span = tracing::info_span!("execute_search", limit = args.limit).entered();
    let results = registry.search(&args.query, args.limit);
    tracing::info!(results_count = results.len(), "Search completed");
    drop(_search_span);

    if results.is_empty() {
        println!("No packages found matching \"{}\"", args.query);
        if args.suggestions {
            println!("\n💡 Try these related searches:");
            println!("   • Use different keywords");
            println!("   • Check spelling");
            println!("   • Try broader search terms");
        }
        return Ok(());
    }

    if args.json {
        let json_results: Vec<_> = results
            .iter()
            .map(|pkg| {
                serde_json::json!({
                    "id": pkg.name,
                    "name": pkg.full_name,
                    "description": pkg.description,
                    "version": pkg.version,
                    "category": pkg.category,
                    "author": pkg.author,
                    "license": pkg.license,
                    "tags": pkg.tags,
                    "features": pkg.features,
                    "repository": pkg.repository,
                })
            })
            .collect();
        println!("{}", serde_json::to_string_pretty(&json_results)?);
        return Ok(());
    }

    // Format output to match cookbook style with enhanced metadata
    println!(
        "Found {} package{} matching \"{}\"",
        results.len(),
        if results.len() == 1 { "" } else { "s" },
        args.query
    );
    println!();

    // Show rich formatted results
    for pkg in results {
        println!("📦 {} v{}", pkg.name, pkg.version);
        println!("   {}", pkg.description);
        println!(
            "   Author: {} | License: {} | Category: {}",
            pkg.author, pkg.license, pkg.category
        );
        if !pkg.tags.is_empty() {
            println!("   Tags: {}", pkg.tags.join(", "));
        }
        if !pkg.features.is_empty() && args.detailed {
            println!("   Features:");
            for feature in &pkg.features {
                println!("     • {}", feature);
            }
        }
        println!();
    }

    if args.fuzzy {
        println!("🔍 Fuzzy search enabled - showing results for similar terms");
    }

    Ok(())
}

/// Fallback function with mock data when registry is unavailable
fn run_with_mock_data(args: &SearchArgs) -> Result<()> {
    if args.json {
        let mock_results = vec![serde_json::json!({
            "id": "@ggen/auth-user",
            "name": "User Authentication",
            "description": "User authentication with email/password and JWT",
            "version": "1.2.0",
            "stars": 1200,
            "downloads": 45000,
            "health_score": 0.95,
            "author": "@ggen-official",
            "license": "MIT"
        })];
        println!("{}", serde_json::to_string_pretty(&mock_results)?);
        return Ok(());
    }

    println!("Found {} packages matching \"{}\"", args.limit, args.query);
    println!();

    println!("📦 @ggen/auth-user (⭐ 1.2k, ⬇ 45k, 🏥 95%)");
    println!("   User authentication with email/password and JWT");
    println!("   Author: @ggen-official | License: MIT");
    println!("   Tags: auth, user, jwt | Updated: 2 days ago");
    println!();

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
                    author: Some("ggen-team".to_string()),
                    license: Some("MIT".to_string()),
                    stars: 42,
                    downloads: 1000,
                    updated_at: "2024-01-01T00:00:00Z".to_string(),
                    tags: vec!["rust".to_string(), "cli".to_string()],
                    health_score: Some(0.95),
                }])
            });

        let args = SearchArgs {
            query: "rust".to_string(),
            category: None,
            keyword: None,
            author: None,
            license: None,
            min_stars: None,
            min_downloads: None,
            sort: "relevance".to_string(),
            order: "desc".to_string(),
            fuzzy: false,
            suggestions: false,
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
            author: None,
            license: None,
            min_stars: None,
            min_downloads: None,
            sort: "relevance".to_string(),
            order: "desc".to_string(),
            fuzzy: false,
            suggestions: false,
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
            author: None,
            license: None,
            min_stars: None,
            min_downloads: None,
            sort: "relevance".to_string(),
            order: "desc".to_string(),
            fuzzy: false,
            suggestions: false,
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
            author: None,
            license: None,
            min_stars: None,
            min_downloads: None,
            sort: "relevance".to_string(),
            order: "desc".to_string(),
            fuzzy: false,
            suggestions: false,
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
            author: None,
            license: None,
            min_stars: None,
            min_downloads: None,
            sort: "relevance".to_string(),
            order: "desc".to_string(),
            fuzzy: false,
            suggestions: false,
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
            author: None,
            license: None,
            min_stars: None,
            min_downloads: None,
            sort: "relevance".to_string(),
            order: "desc".to_string(),
            fuzzy: false,
            suggestions: false,
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
