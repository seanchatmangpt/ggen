//! Search demo example
//!
//! NOTE: This example is currently disabled due to API changes.
//! It will be updated to match the current trait API.

#![cfg(never)] // Disabled until updated for current API

use anyhow::Result;
use chrono::Utc;
use ggen_marketplace::{Package, SearchQuery, TantivySearchEngine};

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize search engine
    let engine = TantivySearchEngine::new("./search_index")?;

    // Create sample packages
    let packages = vec![
        Package {
            id: "rust-axum".to_string(),
            name: "rust-axum-service".to_string(),
            description: "Production-ready Axum web service with PostgreSQL".to_string(),
            version: "1.0.0".to_string(),
            category: "web".to_string(),
            language: "rust".to_string(),
            license: "MIT".to_string(),
            tags: vec!["web".to_string(), "api".to_string(), "axum".to_string()],
            downloads: 5000,
            rating: 4.8,
            created_at: Utc::now(),
            updated_at: Utc::now(),
            author: "ggen-team".to_string(),
            repository_url: Some("https://github.com/example/axum-service".to_string()),
        },
        Package {
            id: "cli-tool".to_string(),
            name: "rust-cli-framework".to_string(),
            description: "Complete CLI framework with argument parsing and testing".to_string(),
            version: "2.1.0".to_string(),
            category: "cli".to_string(),
            language: "rust".to_string(),
            license: "Apache-2.0".to_string(),
            tags: vec!["cli".to_string(), "framework".to_string()],
            downloads: 3200,
            rating: 4.5,
            created_at: Utc::now(),
            updated_at: Utc::now(),
            author: "rust-community".to_string(),
            repository_url: Some("https://github.com/example/cli-framework".to_string()),
        },
    ];

    // Index packages
    println!("Indexing {} packages...", packages.len());
    engine.bulk_index(packages).await?;
    engine.commit().await?;

    // Example 1: Full-text search
    println!("\n=== Full-text search for 'rust web' ===");
    let query = SearchQuery {
        query: "rust web".to_string(),
        ..Default::default()
    };

    let results = engine.search(&query).await?;
    println!(
        "Found {} results in {}ms",
        results.total, results.query_time_ms
    );

    for scored_package in &results.packages {
        println!(
            "  - {} (score: {:.2})",
            scored_package.package.name, scored_package.score
        );
    }

    // Example 2: Faceted search
    println!("\n=== Faceted search by category ===");
    for (facet_name, facets) in &results.facets {
        println!("{}:", facet_name);
        for facet in facets {
            println!("  - {}: {}", facet.value, facet.count);
        }
    }

    // Example 3: Fuzzy search
    println!("\n=== Fuzzy search for 'axim' (typo for 'axum') ===");
    let fuzzy_query = SearchQuery {
        query: "axim".to_string(),
        fuzzy: true,
        ..Default::default()
    };

    let fuzzy_results = engine.search(&fuzzy_query).await?;
    println!("Found {} results with fuzzy matching", fuzzy_results.total);

    for scored_package in &fuzzy_results.packages {
        println!("  - {}", scored_package.package.name);
    }

    // Example 4: Stats
    println!("\n=== Index statistics ===");
    let stats = engine.stats().await?;
    println!("Total documents: {}", stats.total_documents);
    println!("Last updated: {}", stats.last_updated);

    Ok(())
}
