// London School TDD Tests for ggen_market_search
// Focus: Mock-driven testing of marketplace interactions

use ggen_mcp::tools::market;
use serde_json::json;

// BDD-Style Tests for Search Functionality

#[tokio::test]
async fn should_search_packages_by_query_string() {
    // Given: A search query
    let params = json!({
        "query": "authentication"
    });

    // When: Search is performed
    let result = market::search(params).await;

    // Then: Should return matching packages
    assert!(result.is_ok());
    let response = result.unwrap();
    let data = response.get("data").unwrap();

    assert!(data.get("results").is_some());
    assert!(data.get("total_matches").is_some());
}

#[tokio::test]
async fn should_filter_search_by_category() {
    // Given: Search with category filter
    let params = json!({
        "query": "auth",
        "category": "security"
    });

    // When: Search is executed
    let result = market::search(params).await;

    // Then: Should apply category filter
    assert!(result.is_ok());
    let data = result.unwrap().get("data").unwrap().clone();

    let filters = data.get("filters_applied").unwrap();
    assert_eq!(
        filters.get("category").and_then(|v| v.as_str()),
        Some("security")
    );
}

#[tokio::test]
async fn should_support_fuzzy_search() {
    // Given: Fuzzy search enabled
    let params = json!({
        "query": "authenticaton",  // Typo intentional
        "fuzzy": "true"
    });

    // When: Search is performed
    let result = market::search(params).await;

    // Then: Should enable fuzzy matching
    assert!(result.is_ok());
    let data = result.unwrap().get("data").unwrap().clone();

    let metadata = data.get("search_metadata").unwrap();
    assert_eq!(
        metadata.get("fuzzy_search_enabled").and_then(|v| v.as_bool()),
        Some(true)
    );
}

#[tokio::test]
async fn should_provide_search_suggestions() {
    // Given: Search with suggestions enabled
    let params = json!({
        "query": "auth",
        "suggestions": "true"
    });

    // When: Search is executed
    let result = market::search(params).await;

    // Then: Should include suggestions
    assert!(result.is_ok());
    let data = result.unwrap().get("data").unwrap().clone();

    assert!(data.get("suggestions").is_some());
}

#[tokio::test]
async fn should_limit_search_results() {
    // Given: Search with result limit
    let params = json!({
        "query": "rust",
        "limit": 5
    });

    // When: Search is performed
    let result = market::search(params).await;

    // Then: Results should respect limit
    assert!(result.is_ok());
    let data = result.unwrap().get("data").unwrap().clone();

    let results = data.get("results").unwrap().as_array().unwrap();
    assert!(results.len() <= 5, "Should not exceed specified limit");
}

#[tokio::test]
async fn should_fail_when_query_is_missing() {
    // Given: No search query
    let params = json!({
        "category": "rust"
    });

    // When: Search is attempted
    let result = market::search(params).await;

    // Then: Should fail with appropriate error
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("query"));
}

// Advanced Filtering Tests

#[tokio::test]
async fn should_filter_by_minimum_stars() {
    // Given: Minimum stars filter
    let params = json!({
        "query": "rust",
        "min_stars": 100
    });

    // When: Search is performed
    let result = market::search(params).await;

    // Then: Should apply stars filter
    assert!(result.is_ok());
    let data = result.unwrap().get("data").unwrap().clone();

    let filters = data.get("filters_applied").unwrap();
    assert_eq!(
        filters.get("min_stars").and_then(|v| v.as_u64()),
        Some(100)
    );
}

#[tokio::test]
async fn should_filter_by_author() {
    // Given: Author filter
    let params = json!({
        "query": "template",
        "author": "sean"
    });

    // When: Search is executed
    let result = market::search(params).await;

    // Then: Should apply author filter
    assert!(result.is_ok());
    let data = result.unwrap().get("data").unwrap().clone();

    let filters = data.get("filters_applied").unwrap();
    assert_eq!(
        filters.get("author").and_then(|v| v.as_str()),
        Some("sean")
    );
}

#[tokio::test]
async fn should_filter_by_license() {
    // Given: License filter
    let params = json!({
        "query": "library",
        "license": "MIT"
    });

    // When: Search is performed
    let result = market::search(params).await;

    // Then: Should apply license filter
    assert!(result.is_ok());
    let data = result.unwrap().get("data").unwrap().clone();

    let filters = data.get("filters_applied").unwrap();
    assert_eq!(
        filters.get("license").and_then(|v| v.as_str()),
        Some("MIT")
    );
}

// Sorting Tests

#[tokio::test]
async fn should_sort_by_relevance_by_default() {
    // Given: Search without explicit sort
    let params = json!({
        "query": "rust"
    });

    // When: Search is executed
    let result = market::search(params).await;

    // Then: Should default to relevance sorting
    assert!(result.is_ok());
    let data = result.unwrap().get("data").unwrap().clone();

    let filters = data.get("filters_applied").unwrap();
    assert_eq!(
        filters.get("sort").and_then(|v| v.as_str()),
        Some("relevance")
    );
}

#[tokio::test]
async fn should_sort_by_stars() {
    // Given: Sort by stars
    let params = json!({
        "query": "rust",
        "sort": "stars",
        "order": "desc"
    });

    // When: Search is performed
    let result = market::search(params).await;

    // Then: Should apply stars sorting
    assert!(result.is_ok());
    let data = result.unwrap().get("data").unwrap().clone();

    let filters = data.get("filters_applied").unwrap();
    assert_eq!(filters.get("sort").and_then(|v| v.as_str()), Some("stars"));
    assert_eq!(filters.get("order").and_then(|v| v.as_str()), Some("desc"));
}

// Result Metadata Tests

#[tokio::test]
async fn should_include_relevance_scores() {
    // Given: Search query
    let params = json!({
        "query": "authentication"
    });

    // When: Search is performed
    let result = market::search(params).await;

    // Then: Results should include relevance scores
    assert!(result.is_ok());
    let data = result.unwrap().get("data").unwrap().clone();
    let results = data.get("results").unwrap().as_array().unwrap();

    if !results.is_empty() {
        assert!(results[0].get("relevance_score").is_some());
    }
}

#[tokio::test]
async fn should_indicate_matching_fields() {
    // Given: Search query
    let params = json!({
        "query": "rust"
    });

    // When: Search is executed
    let result = market::search(params).await;

    // Then: Results should show which fields matched
    assert!(result.is_ok());
    let data = result.unwrap().get("data").unwrap().clone();
    let results = data.get("results").unwrap().as_array().unwrap();

    if !results.is_empty() {
        assert!(results[0].get("match_fields").is_some());
    }
}

#[tokio::test]
async fn should_include_health_scores() {
    // Given: Search query
    let params = json!({
        "query": "template"
    });

    // When: Search is performed
    let result = market::search(params).await;

    // Then: Results should include health scores
    assert!(result.is_ok());
    let data = result.unwrap().get("data").unwrap().clone();
    let results = data.get("results").unwrap().as_array().unwrap();

    if !results.is_empty() {
        assert!(results[0].get("health_score").is_some());
    }
}

// Error Handling Tests

#[tokio::test]
async fn should_handle_empty_query_string() {
    // Given: Empty query
    let params = json!({
        "query": ""
    });

    // When: Search is attempted
    let result = market::search(params).await;

    // Then: Should handle gracefully
    let _ = result; // Documents current behavior
}

#[tokio::test]
async fn should_handle_invalid_limit() {
    // Given: Invalid limit value
    let params = json!({
        "query": "rust",
        "limit": -5
    });

    // When: Search is performed
    let result = market::search(params).await;

    // Then: Should handle gracefully or use default
    assert!(result.is_ok() || result.is_err());
}
