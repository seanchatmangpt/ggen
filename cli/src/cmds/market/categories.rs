//! Marketplace categories listing and management.
//!
//! This module provides functionality to list and explore marketplace categories,
//! helping users discover gpacks by browsing popular categories and their
//! associated package counts.
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

    let mut cmd = std::process::Command::new("cargo");
    cmd.args(["make", "market-categories"]);

    let output = cmd.output().map_err(ggen_utils::error::Error::from)?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Categories fetch failed: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    println!("{}", stdout);

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
