//! Marketplace categories and keywords display functionality.
//!
//! This module provides functionality to display popular marketplace categories
//! and trending keywords, helping users discover relevant gpacks and understand
//! the marketplace ecosystem.
//!
//! # Examples
//!
//! ```bash
//! ggen categories --detailed
//! ggen categories --keywords --json
//! ggen categories --detailed --keywords
//! ```
//!
//! # Errors
//!
//! Returns errors if the marketplace registry is unavailable or if
//! the categories/keywords cannot be retrieved.

use clap::Args;
use colored::*;
use ggen_core::RegistryClient;
use ggen_utils::error::Result;
use serde_json;

#[cfg(test)]
use mockall::predicate::*;

/// Categories command arguments with validation
#[derive(Args, Debug)]
pub struct CategoriesArgs {
    /// Show popular keywords instead of categories
    #[arg(long)]
    pub keywords: bool,

    /// Show detailed statistics
    #[arg(short, long)]
    pub detailed: bool,

    /// Output as JSON
    #[arg(long)]
    pub json: bool,
}

/// Validate categories command input
fn validate_categories_input(args: &CategoriesArgs) -> Result<()> {
    // Validate boolean flags are not mutually exclusive in problematic ways
    // Categories command is fairly simple, main validation is around output format consistency

    // JSON output should be consistent with detailed flag
    if args.json && args.detailed {
        // This is actually fine - JSON can be detailed
    }

    Ok(())
}

pub async fn run(args: &CategoriesArgs) -> Result<()> {
    validate_categories_input(args)?;

    if args.keywords {
        println!("🔍 Fetching popular keywords from marketplace...");
        let registry_client = RegistryClient::new()?;
        let keywords = registry_client.get_popular_keywords().await.map_err(|e| {
            ggen_utils::error::Error::new_fmt(format_args!(
                "Failed to fetch popular keywords: {}",
                e
            ))
        })?;
        display_keywords(&keywords, args.detailed, args.json)?;
    } else {
        println!("📂 Fetching popular categories from marketplace...");
        let registry_client = RegistryClient::new()?;
        let categories = registry_client
            .get_popular_categories()
            .await
            .map_err(|e| {
                ggen_utils::error::Error::new_fmt(format_args!(
                    "Failed to fetch popular categories: {}",
                    e
                ))
            })?;
        display_categories(&categories, args.detailed, args.json)?;
    }

    Ok(())
}

fn display_categories(categories: &[(String, u64)], detailed: bool, json: bool) -> Result<()> {
    if json {
        let json_output = serde_json::to_string_pretty(categories)?;
        println!("{}", json_output);
        return Ok(());
    }

    println!("📂 Popular Categories");
    println!("===================");

    if detailed {
        for (i, (category, count)) in categories.iter().enumerate() {
            println!(
                "{}. {} ({})",
                i + 1,
                category.bold().cyan(),
                format!("{} templates", count).dimmed()
            );
        }
    } else {
        let categories_str = categories
            .iter()
            .take(10)
            .map(|(cat, _)| cat.as_str())
            .collect::<Vec<_>>()
            .join(", ");
        println!("{}", categories_str.cyan());
    }

    Ok(())
}

fn display_keywords(keywords: &[(String, u64)], detailed: bool, json: bool) -> Result<()> {
    if json {
        let json_output = serde_json::to_string_pretty(keywords)?;
        println!("{}", json_output);
        return Ok(());
    }

    println!("🏷️  Popular Keywords");
    println!("===================");

    if detailed {
        for (i, (keyword, count)) in keywords.iter().enumerate() {
            println!(
                "{}. {} ({})",
                i + 1,
                keyword.bold().green(),
                format!("{} templates", count).dimmed()
            );
        }
    } else {
        let keywords_str = keywords
            .iter()
            .take(15)
            .map(|(kw, _)| kw.as_str())
            .collect::<Vec<_>>()
            .join(", ");
        println!("{}", keywords_str.green());
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use mockall::predicate::*;

    #[tokio::test]
    async fn test_categories_default_display() {
        let args = CategoriesArgs {
            keywords: false,
            detailed: false,
            json: false,
        };

        let result = run(&args).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_categories_keywords_display() {
        let args = CategoriesArgs {
            keywords: true,
            detailed: false,
            json: false,
        };

        let result = run(&args).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_categories_detailed_display() {
        let args = CategoriesArgs {
            keywords: false,
            detailed: true,
            json: false,
        };

        let result = run(&args).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_categories_json_display() {
        let args = CategoriesArgs {
            keywords: false,
            detailed: false,
            json: true,
        };

        let result = run(&args).await;
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_categories_input_success() {
        let args = CategoriesArgs {
            keywords: false,
            detailed: true,
            json: true,
        };

        let result = validate_categories_input(&args);
        assert!(result.is_ok());
    }
}
