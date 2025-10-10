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
    println!("🚧 Placeholder: market categories");
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
        mock_lister
            .expect_list_categories()
            .times(1)
            .returning(|| {
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
