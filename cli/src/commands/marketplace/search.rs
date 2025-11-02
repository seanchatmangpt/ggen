//! Marketplace search command - CLI layer
//!
//! This module provides the CLI interface for searching the marketplace.
//! Uses clap-noun-verb v3.0.0 #[verb] pattern with Chicago TDD (Classicist School).

use clap::Args;
use ggen_utils::error::Result;
use crate::runtime;

/// Search for packages in the marketplace
///
/// # Examples
///
/// ```bash
/// ggen marketplace search "rust cli"
/// ggen marketplace search "web" --category api --limit 20
/// ggen marketplace search "database" --detailed --json
/// ```
#[derive(Args, Debug)]
#[command(name = "search", about = "Search for packages in the marketplace")]
pub struct SearchArgs {
    /// Search query
    #[arg(value_name = "QUERY")]
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

    /// Enable fuzzy search for typos
    #[arg(long)]
    pub fuzzy: bool,

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

/// Execute marketplace search command
///
/// This is a thin CLI wrapper that bridges to the async domain layer.
/// Uses runtime::execute() to handle async/sync bridging.
pub fn run(args: &SearchArgs) -> Result<()> {
    runtime::execute(async {
        // Bridge to domain layer
        crate::domain::marketplace::search::search_and_display(
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
    fn test_search_args_parsing() {
        let args = SearchArgs {
            query: "rust cli".to_string(),
            category: Some("tools".to_string()),
            keyword: None,
            author: None,
            fuzzy: true,
            detailed: false,
            json: false,
            limit: 10,
        };

        assert_eq!(args.query, "rust cli");
        assert_eq!(args.category, Some("tools".to_string()));
        assert!(args.fuzzy);
        assert_eq!(args.limit, 10);
    }
}
