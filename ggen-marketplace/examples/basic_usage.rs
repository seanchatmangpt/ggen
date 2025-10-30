//! Basic usage example of ggen-marketplace
//!
//! This example demonstrates:
//! - Creating a marketplace client
//! - Searching for packages
//! - Publishing a package
//! - Downloading a package

use ggen_marketplace::prelude::*;
use anyhow::Result;
use bytes::Bytes;

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize logging
    tracing_subscriber::fmt::init();

    // Create marketplace with local backend
    let marketplace = MarketplaceBuilder::new()
        .with_local_registry("./data/registry.db")
        .with_file_storage("./data/packages")
        .with_tantivy_search("./data/search")
        .with_ed25519_crypto()
        .with_prometheus_metrics()
        .build()
        .await?;

    println!("✓ Marketplace initialized\n");

    // Example 1: Search for packages
    println!("=== Searching for packages ===");
    let search_query = SearchQuery {
        text: "web framework".to_string(),
        categories: vec![],
        tags: vec![],
        min_downloads: None,
        limit: 5,
        offset: 0,
    };

    let results = marketplace.search(&search_query).await?;
    println!("Found {} packages:\n", results.len());

    for (i, result) in results.iter().enumerate() {
        println!("{}. {} v{}", i + 1, result.package.id.name, result.package.id.version);
        println!("   {}", result.package.description);
        println!("   Score: {:.2}", result.score);
        println!();
    }

    // Example 2: Create and publish a package
    println!("=== Publishing a package ===");

    // Generate a keypair for signing
    let keypair = marketplace.crypto().generate_keypair().await?;

    // Create package metadata
    let package_metadata = PackageMetadata {
        id: PackageId {
            name: "example-package".to_string(),
            version: semver::Version::parse("1.0.0")?,
            namespace: Some("examples".to_string()),
        },
        description: "An example package for demonstration".to_string(),
        authors: vec!["example@example.com".to_string()],
        license: "MIT".to_string(),
        repository: Some("https://github.com/example/package".parse()?),
        homepage: None,
        keywords: vec!["example".to_string(), "demo".to_string()],
        categories: vec!["development".to_string()],
        dependencies: std::collections::HashMap::new(),
        checksums: Checksums::default(),
        signature: Signature::default(),
        published_at: chrono::Utc::now(),
    };

    // Create package content (in real scenario, this would be a tarball)
    let package_bytes = Bytes::from_static(b"Package content goes here");

    // Sign the package
    let signature = marketplace.crypto()
        .sign(&package_metadata, &keypair.private_key)
        .await?;

    let mut signed_metadata = package_metadata.clone();
    signed_metadata.signature = signature;

    // Publish to marketplace
    let package_id = marketplace.publish(&signed_metadata, package_bytes.clone()).await?;
    println!("✓ Published package: {} v{}\n", package_id.name, package_id.version);

    // Example 3: Download the package
    println!("=== Downloading package ===");

    let downloaded_bytes = marketplace.download(&package_id).await?;
    println!("✓ Downloaded {} bytes", downloaded_bytes.len());

    // Verify the package
    let is_valid = marketplace.verify(&package_id).await?;
    println!("✓ Package verification: {}\n", if is_valid { "PASSED" } else { "FAILED" });

    // Example 4: List all versions of a package
    println!("=== Listing package versions ===");

    let versions = marketplace.list_versions("example-package", Some("examples")).await?;
    println!("Available versions:");
    for version in versions {
        println!("  - {}", version);
    }
    println!();

    // Example 5: Get package statistics
    println!("=== Package statistics ===");

    let stats = marketplace.stats(&package_id).await?;
    println!("Total downloads: {}", stats.total_downloads);
    println!("Last 30 days: {}", stats.downloads_last_30_days);
    println!("Average rating: {:.1}", stats.average_rating);
    println!();

    // Example 6: Resolve dependencies
    println!("=== Resolving dependencies ===");

    let dep_graph = marketplace.resolve_dependencies(&package_id, true).await?;
    println!("Dependency tree:");
    print_dependency_tree(&dep_graph, 0);

    println!("\n✓ All examples completed successfully!");

    Ok(())
}

fn print_dependency_tree(graph: &DependencyGraph, indent: usize) {
    let indent_str = "  ".repeat(indent);
    println!("{}└─ {} v{}", indent_str, graph.package.name, graph.package.version);

    for dep in &graph.dependencies {
        print_dependency_tree(dep, indent + 1);
    }
}
