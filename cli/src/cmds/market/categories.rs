//! Marketplace categories listing and management.
//!
//! # WHAT THIS MODULE SHOULD DO (Intent-Driven Architecture)
//!
//! ## PURPOSE
//! This module should facilitate package discovery through browsing by providing
//! organized categories with package counts, making it easy for developers to
//! explore related gpacks in their domain of interest.
//!
//! ## RESPONSIBILITIES
//! 1. **Category Listing**: Should fetch current categories from marketplace
//! 2. **Package Counts**: Should show number of packages per category
//! 3. **Trending Info**: Should highlight trending categories
//! 4. **Navigation**: Should integrate with search (filter by category)
//! 5. **Metadata**: Should show category descriptions and examples
//!
//! ## CONSTRAINTS
//! - Must display categories in logical order (popular, alphabetical, etc.)
//! - Must handle marketplace API unavailability
//! - Must support both human-readable and machine-readable output
//! - Must update dynamically as marketplace evolves
//! - Must be fast enough for interactive browsing
//!
//! ## DEPENDENCIES
//! - Marketplace API: Should query category endpoint
//! - `CategoryLister` trait: Should be mockable for testing
//! - Cache: Should cache categories for offline browsing
//!
//! ## ERROR HANDLING STRATEGY
//! - API unavailable → Show cached categories
//! - Empty categories → Helpful message for new marketplace
//! - Network timeout → Fail fast with cached fallback
//!
//! ## TESTING STRATEGY
//! - Mock CategoryLister for deterministic tests
//! - Test with empty, small, and large category lists
//! - Test sorting options
//! - Test integration with search filters
//!
//! ## REFACTORING PRIORITIES
//! - [P0] Implement actual marketplace API integration (currently placeholder)
//! - [P0] Add category caching for offline use
//! - [P1] Show trending/popular indicators
//! - [P1] Add category descriptions and examples
//! - [P2] Support category hierarchies (parent/child)
//!
//! # Examples
//!
//! ```bash
//! ggen market categories
//! ```
//!
//! # Errors
//!
//! Returns errors if the marketplace registry is unavailable or if the
//! categories listing operation fails.

use clap::Args;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
pub struct CategoriesArgs {}

#[cfg_attr(test, mockall::automock)]
pub trait CategoryLister {
    fn list_categories(&self) -> Result<Vec<Category>>;
}

#[derive(Debug, Clone)]
pub struct Category {
    pub name: String,
    pub count: usize,
}

pub async fn run(_args: &CategoriesArgs) -> Result<()> {
    println!("📂 Fetching marketplace categories...");
    println!();

    // Load registry and show actual categories
    let registry = match super::registry::Registry::load_sync() {
        Ok(r) => r,
        Err(e) => {
            eprintln!("⚠️  Warning: Could not load marketplace registry: {}", e);
            println!("Popular categories:");
            println!("  • ai");
            println!("  • templates");
            println!("  • utilities");
            println!("  • cli");
            println!("  • web");
            return Ok(());
        }
    };

    // Collect unique categories from registry
    let mut categories: std::collections::HashMap<String, usize> = std::collections::HashMap::new();
    for package in &registry.packages {
        *categories.entry(package.category.clone()).or_insert(0) += 1;
    }

    // Sort categories by count
    let mut category_list: Vec<_> = categories.iter().collect();
    category_list.sort_by(|a, b| b.1.cmp(a.1));

    println!("Available categories:");
    for (category, count) in &category_list {
        println!("  • {} ({} packages)", category, count);
    }

    if category_list.is_empty() {
        println!("No categories found in registry.");
    }

    println!();
    println!("💡 Use 'ggen market search <query> --category <category>' to filter by category");

    Ok(())
}

pub async fn run_with_deps(_args: &CategoriesArgs, lister: &dyn CategoryLister) -> Result<()> {
    let categories = lister.list_categories()?;

    println!("Popular categories:");
    for category in categories {
        println!("  {} ({} gpacks)", category.name, category.count);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_categories_lists_all() {
        let mut mock_lister = MockCategoryLister::new();
        mock_lister.expect_list_categories().times(1).returning(|| {
            Ok(vec![
                Category {
                    name: "rust".to_string(),
                    count: 10,
                },
                Category {
                    name: "python".to_string(),
                    count: 8,
                },
            ])
        });

        let args = CategoriesArgs {};

        let result = run_with_deps(&args, &mock_lister).await;
        assert!(result.is_ok());
    }
}
