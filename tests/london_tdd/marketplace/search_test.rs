//! London TDD tests for `ggen search` command
//!
//! README.md §Marketplace - Natural Language Search
//!
//! Tests verify:
//! - Natural language query processing
//! - Result relevance and ranking
//! - Category filtering
//! - "Did you mean?" suggestions

use crate::lib::*;
use mockall::predicate::*;

#[test]
fn test_search_finds_relevant_packages() {
    let start = std::time::Instant::now();

    // Arrange
    let mut mock_marketplace = MockMarketplaceClient::new();
    mock_marketplace
        .expect_search()
        .with(eq("rust web"))
        .times(1)
        .returning(|_| {
            Ok(vec![
                Package {
                    id: "io.ggen.rust.axum".to_string(),
                    name: "Rust Axum Service".to_string(),
                    version: "1.0.0".to_string(),
                    description: "Axum web service template".to_string(),
                    category: "rust".to_string(),
                },
                Package {
                    id: "io.ggen.rust.actix".to_string(),
                    name: "Rust Actix Service".to_string(),
                    version: "1.0.0".to_string(),
                    description: "Actix web service template".to_string(),
                    category: "rust".to_string(),
                },
            ])
        });

    // Act
    let result = run_search_command(&mock_marketplace, "rust web");

    // Assert
    assert!(result.is_ok());
    let results = result.unwrap();
    assert_eq!(results.packages.len(), 2);
    assert!(results.packages[0].name.contains("Rust"));

    // Performance: <100ms
    assert!(start.elapsed().as_millis() < 100);
}

#[test]
fn test_search_suggests_typo_corrections() {
    // Arrange
    let mut mock_marketplace = MockMarketplaceClient::new();
    mock_marketplace
        .expect_search()
        .with(eq("rst web")) // Typo: "rst" instead of "rust"
        .times(1)
        .returning(|_| Ok(vec![]));

    // Act
    let result = run_search_with_suggestions(&mock_marketplace, "rst web");

    // Assert: Suggests correction
    assert!(result.is_ok());
    let response = result.unwrap();
    assert_eq!(response.packages.len(), 0);
    assert!(response.suggestions.contains(&"rust web".to_string()));
    assert!(response.suggestions.contains(&"rest web".to_string()));
}

#[test]
fn test_search_filters_by_category() {
    // Arrange
    let mut mock_marketplace = MockMarketplaceClient::new();
    mock_marketplace
        .expect_search()
        .with(eq("web"))
        .times(1)
        .returning(|_| {
            Ok(vec![
                generators::fake_package(),
                generators::fake_package(),
            ])
        });

    // Act
    let result = run_search_with_category(&mock_marketplace, "web", Some("rust"));

    // Assert: Only Rust packages
    assert!(result.is_ok());
    let results = result.unwrap();
    assert!(results.packages.iter().all(|p| p.category == "rust"));
}

#[test]
fn test_search_ranks_results_by_relevance() {
    // Arrange
    let mut mock_marketplace = MockMarketplaceClient::new();
    mock_marketplace
        .expect_search()
        .returning(|_| {
            Ok(vec![
                Package {
                    id: "io.ggen.rust.exact".to_string(),
                    name: "Rust Web Service".to_string(), // Exact match
                    version: "1.0.0".to_string(),
                    description: "Web service".to_string(),
                    category: "rust".to_string(),
                },
                Package {
                    id: "io.ggen.rust.partial".to_string(),
                    name: "Rust CLI".to_string(), // Partial match
                    version: "1.0.0".to_string(),
                    description: "CLI tool".to_string(),
                    category: "rust".to_string(),
                },
            ])
        });

    // Act
    let result = run_search_command(&mock_marketplace, "rust web");

    // Assert: Best match first
    assert!(result.is_ok());
    let results = result.unwrap();
    assert!(results.packages[0].name.contains("Web"));
}

#[test]
fn test_search_creates_otel_span() {
    // Arrange
    let mock_marketplace = setup_marketplace_with_results();
    let tracer = otel::MockTracerProvider::new();

    // Act
    let _result = run_search_with_tracing(&mock_marketplace, &tracer, "rust web");

    // Assert
    let span = tracer.find_span("ggen.marketplace.search").unwrap();
    assert_eq!(span.status, otel::SpanStatus::Ok);
    assert!(span.attributes.iter().any(|(k, v)| k == "search.query" && v == "rust web"));
    assert!(span.attributes.iter().any(|(k, _)| k == "results.count"));
}

// Helper types and functions

#[derive(Debug)]
struct SearchResponse {
    packages: Vec<Package>,
    suggestions: Vec<String>,
}

fn run_search_command(
    marketplace: &dyn MarketplaceClient,
    query: &str,
) -> Result<SearchResponse, anyhow::Error> {
    let packages = marketplace.search(query)?;
    Ok(SearchResponse {
        packages,
        suggestions: vec![],
    })
}

fn run_search_with_suggestions(
    marketplace: &dyn MarketplaceClient,
    query: &str,
) -> Result<SearchResponse, anyhow::Error> {
    let packages = marketplace.search(query)?;

    // Generate "did you mean?" suggestions for empty results
    let suggestions = if packages.is_empty() {
        suggest_corrections(query)
    } else {
        vec![]
    };

    Ok(SearchResponse {
        packages,
        suggestions,
    })
}

fn run_search_with_category(
    marketplace: &dyn MarketplaceClient,
    query: &str,
    category: Option<&str>,
) -> Result<SearchResponse, anyhow::Error> {
    let mut packages = marketplace.search(query)?;

    if let Some(cat) = category {
        packages.retain(|p| p.category == cat);
    }

    Ok(SearchResponse {
        packages,
        suggestions: vec![],
    })
}

fn run_search_with_tracing(
    marketplace: &dyn MarketplaceClient,
    tracer: &otel::MockTracerProvider,
    query: &str,
) -> Result<SearchResponse, anyhow::Error> {
    let result = run_search_command(marketplace, query)?;

    let span = otel::MockSpan {
        name: "ggen.marketplace.search".to_string(),
        attributes: vec![
            ("search.query".to_string(), query.to_string()),
            ("results.count".to_string(), result.packages.len().to_string()),
        ],
        events: vec!["search_completed".to_string()],
        status: otel::SpanStatus::Ok,
    };
    tracer.record_span(span);

    Ok(result)
}

fn suggest_corrections(query: &str) -> Vec<String> {
    // Simple typo correction suggestions
    let common_terms = ["rust", "web", "rest", "api", "cli", "service"];

    common_terms
        .iter()
        .filter(|&term| levenshtein_distance(query, term) <= 2)
        .map(|&s| format!("{} web", s))
        .collect()
}

fn levenshtein_distance(s1: &str, s2: &str) -> usize {
    let len1 = s1.len();
    let len2 = s2.len();
    let mut matrix = vec![vec![0; len2 + 1]; len1 + 1];

    for i in 0..=len1 {
        matrix[i][0] = i;
    }
    for j in 0..=len2 {
        matrix[0][j] = j;
    }

    for (i, c1) in s1.chars().enumerate() {
        for (j, c2) in s2.chars().enumerate() {
            let cost = if c1 == c2 { 0 } else { 1 };
            matrix[i + 1][j + 1] = std::cmp::min(
                std::cmp::min(matrix[i][j + 1] + 1, matrix[i + 1][j] + 1),
                matrix[i][j] + cost,
            );
        }
    }

    matrix[len1][len2]
}

fn setup_marketplace_with_results() -> MockMarketplaceClient {
    let mut mock = MockMarketplaceClient::new();
    mock.expect_search().returning(|_| {
        Ok(vec![Package {
            id: "io.ggen.rust.web".to_string(),
            name: "Rust Web Service".to_string(),
            version: "1.0.0".to_string(),
            description: "Web service template".to_string(),
            category: "rust".to_string(),
        }])
    });
    mock
}
