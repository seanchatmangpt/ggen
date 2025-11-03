#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::*;
    use chrono::Utc;
    use tempfile::TempDir;

    fn create_test_package(id: &str, name: &str, category: &str) -> Package {
        Package {
            id: id.to_string(),
            name: name.to_string(),
            description: format!("Test package {}", name),
            version: "1.0.0".to_string(),
            category: category.to_string(),
            language: "rust".to_string(),
            license: "MIT".to_string(),
            tags: vec!["test".to_string()],
            downloads: 100,
            rating: 4.0,
            created_at: Utc::now(),
            updated_at: Utc::now(),
            author: "test-author".to_string(),
            repository_url: Some("https://github.com/test/test".to_string()),
        }
    }

    #[tokio::test]
    async fn test_index_and_search() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let engine = TantivySearchEngine::new(temp_dir.path())?;

        // Index test packages
        let packages = vec![
            create_test_package("1", "rust-web-framework", "web"),
            create_test_package("2", "rust-cli-tool", "cli"),
            create_test_package("3", "web-scraper", "web"),
        ];

        engine.bulk_index(packages).await?;
        engine.commit().await?;

        // Search for "rust"
        let query = SearchQuery {
            query: "rust".to_string(),
            ..Default::default()
        };

        let results = engine.search(&query).await?;
        assert!(results.total >= 2);

        Ok(())
    }

    #[tokio::test]
    async fn test_faceted_search() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let engine = TantivySearchEngine::new(temp_dir.path())?;

        let packages = vec![
            create_test_package("1", "web-framework", "web"),
            create_test_package("2", "web-server", "web"),
            create_test_package("3", "cli-tool", "cli"),
        ];

        engine.bulk_index(packages).await?;
        engine.commit().await?;

        // Search with category filter
        let query = SearchQuery {
            query: "".to_string(),
            filters: SearchFilters {
                categories: vec!["web".to_string()],
                ..Default::default()
            },
            ..Default::default()
        };

        let results = engine.search(&query).await?;
        assert_eq!(results.total, 2);

        Ok(())
    }

    #[tokio::test]
    async fn test_fuzzy_search() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let engine = TantivySearchEngine::new(temp_dir.path())?;

        let package = create_test_package("1", "framework", "web");
        engine.index(&package).await?;
        engine.commit().await?;

        // Search with typo
        let query = SearchQuery {
            query: "framwork".to_string(), // Missing 'e'
            fuzzy: true,
            ..Default::default()
        };

        let results = engine.search(&query).await?;
        assert!(results.total >= 1);

        Ok(())
    }

    #[tokio::test]
    async fn test_update_and_remove() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let engine = TantivySearchEngine::new(temp_dir.path())?;

        let package = create_test_package("1", "test-package", "web");
        engine.index(&package).await?;
        engine.commit().await?;

        // Update
        let mut updated_package = package.clone();
        updated_package.name = "updated-package".to_string();
        engine.update(&updated_package).await?;
        engine.commit().await?;

        let query = SearchQuery {
            query: "updated".to_string(),
            ..Default::default()
        };
        let results = engine.search(&query).await?;
        assert!(results.total >= 1);

        // Remove
        engine.remove("1").await?;
        engine.commit().await?;

        let stats = engine.stats().await?;
        assert_eq!(stats.total_documents, 0);

        Ok(())
    }
}
