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
    pub limit: usize,
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct SearchResult {
    pub id: String,
    pub name: String,
    pub description: String,
    pub version: String,
    pub category: Option<String>,
}

pub async fn run(args: &SearchArgs) -> Result<()> {
    println!("🚧 Placeholder: market search");
    println!("  Query: {}", args.query);
    println!("  Category: {:?}", args.category);
    println!("  Detailed: {}", args.detailed);
    println!("  JSON: {}", args.json);
    Ok(())
}

pub async fn run_with_deps(args: &SearchArgs, client: &dyn MarketplaceClient) -> Result<()> {
    let filters = SearchFilters {
        category: args.category.clone(),
        keyword: args.keyword.clone(),
        limit: args.limit,
    };

    let results = client.search(&args.query, &filters)?;

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
            .with(eq("rust"), always())
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
            .withf(|_, filters| {
                filters.category == Some("rust".to_string()) && filters.limit == 5
            })
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
}
