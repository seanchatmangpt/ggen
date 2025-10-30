// Example: Using the Marketplace Registry
//
// This example demonstrates how to use the marketplace registry
// for package discovery and management.

use ggen_marketplace::prelude::*;
use std::path::PathBuf;

#[tokio::main]
async fn main() -> Result<()> {
    // Example 1: Initialize a local registry
    println!("📦 Example 1: Initialize Local Registry");
    let registry = LocalRegistry::new(
        PathBuf::from("~/.ggen/registry")
    ).await?;
    println!("✅ Registry initialized\n");

    // Example 2: Publish a package
    println!("📦 Example 2: Publish a Package");
    let package = Package::builder(
        PackageId::new("ggen", "rust-web-service"),
        Version::new(1, 0, 0),
    )
    .title("Rust Web Service Template")
    .description("Production-ready Axum web service template")
    .license("MIT")
    .category(Category::WebService)
    .tag("rust")
    .tag("axum")
    .tag("web")
    .content_id(ContentId::new(
        "sha256:abc123...",
        ggen_marketplace::models::package::HashAlgorithm::Sha256,
    ))
    .build()?;

    registry.publish(package).await?;
    println!("✅ Package published\n");

    // Example 3: Search for packages
    println!("📦 Example 3: Search for Packages");
    let results = registry.search(&Query::new("rust web")).await?;
    println!("Found {} packages:", results.len());
    for pkg in &results {
        println!("  - {} v{}", pkg.id, pkg.version);
        println!("    {}", pkg.metadata.description);
    }
    println!();

    // Example 4: Get specific package
    println!("📦 Example 4: Get Specific Package");
    let pkg = registry.get_package(
        &PackageId::new("ggen", "rust-web-service")
    ).await?;
    println!("Package: {} v{}", pkg.id, pkg.version);
    println!("Title: {}", pkg.metadata.title);
    println!("License: {}", pkg.metadata.license);
    println!("Tags: {}", pkg.metadata.tags.join(", "));
    println!();

    // Example 5: List all versions
    println!("📦 Example 5: List All Versions");
    // First, publish a few more versions
    for minor in 1..=3 {
        let pkg = Package::builder(
            PackageId::new("ggen", "rust-web-service"),
            Version::new(1, minor, 0),
        )
        .title("Rust Web Service Template")
        .description("Production-ready Axum web service template")
        .license("MIT")
        .content_id(ContentId::new(
            format!("sha256:v1.{}.0", minor),
            ggen_marketplace::models::package::HashAlgorithm::Sha256,
        ))
        .build()?;
        registry.publish(pkg).await?;
    }

    let versions = registry.list_versions(
        &PackageId::new("ggen", "rust-web-service")
    ).await?;
    println!("Available versions:");
    for v in &versions {
        println!("  - v{} (published: {})", v.version, v.created_at);
    }
    println!();

    // Example 6: Get specific version
    println!("📦 Example 6: Get Specific Version");
    let v2 = registry.get_package_version(
        &PackageId::new("ggen", "rust-web-service"),
        "1.2.0"
    ).await?;
    println!("Got version: v{}", v2.version);
    println!();

    // Example 7: Check if package exists
    println!("📦 Example 7: Check Package Existence");
    let exists = registry.exists(
        &PackageId::new("ggen", "rust-web-service")
    ).await?;
    let not_exists = registry.exists(
        &PackageId::new("ggen", "nonexistent")
    ).await?;
    println!("rust-web-service exists: {}", exists);
    println!("nonexistent exists: {}", not_exists);
    println!();

    // Example 8: Search by category
    println!("📦 Example 8: Search by Category");
    // Publish packages in different categories
    let db_pkg = Package::builder(
        PackageId::new("ggen", "postgresql-db"),
        Version::new(1, 0, 0),
    )
    .title("PostgreSQL Database Template")
    .description("Production PostgreSQL setup")
    .license("MIT")
    .category(Category::Database)
    .tag("postgresql")
    .tag("database")
    .content_id(ContentId::new(
        "sha256:db123",
        ggen_marketplace::models::package::HashAlgorithm::Sha256,
    ))
    .build()?;
    registry.publish(db_pkg).await?;

    let all_results = registry.search(&Query::new("")).await?;
    let web_packages: Vec<_> = all_results.iter()
        .filter(|p| p.metadata.categories.contains(&Category::WebService))
        .collect();
    let db_packages: Vec<_> = all_results.iter()
        .filter(|p| p.metadata.categories.contains(&Category::Database))
        .collect();

    println!("Web Service packages: {}", web_packages.len());
    println!("Database packages: {}", db_packages.len());
    println!();

    // Example 9: Delete a version
    println!("📦 Example 9: Delete a Version");
    registry.delete(
        &PackageId::new("ggen", "rust-web-service"),
        "1.1.0"
    ).await?;
    println!("✅ Version 1.1.0 deleted");

    let remaining = registry.list_versions(
        &PackageId::new("ggen", "rust-web-service")
    ).await?;
    println!("Remaining versions: {}", remaining.len());
    println!();

    // Example 10: Get registry metadata
    println!("📦 Example 10: Registry Metadata");
    let metadata = registry.metadata().await?;
    println!("Registry: {}", metadata.name);
    println!("Total packages: {}", metadata.package_count);
    println!("API version: {}", metadata.api_version);
    println!("Features: {}", metadata.features.join(", "));
    println!();

    Ok(())
}

// Example: Using remote registry
async fn remote_registry_example() -> Result<()> {
    println!("🌐 Remote Registry Example");

    let remote = CentralizedRegistry::new(
        "https://marketplace.ggen.dev"
    )?;

    // Search remote registry
    let results = remote.search(&Query::new("rust")).await?;
    println!("Found {} packages on remote registry", results.len());

    Ok(())
}

// Example: Multi-registry setup with fallback
async fn multi_registry_example() -> Result<()> {
    println!("🔄 Multi-Registry with Fallback");

    let local = LocalRegistry::new(
        PathBuf::from("~/.ggen/registry")
    ).await?;

    let remote = CentralizedRegistry::new(
        "https://marketplace.ggen.dev"
    )?;

    // Try local first, fallback to remote
    async fn find_package(
        id: &PackageId,
        local: &LocalRegistry,
        remote: &CentralizedRegistry,
    ) -> Result<Package> {
        match local.get_package(id).await {
            Ok(pkg) => {
                println!("✅ Found in local registry");
                Ok(pkg)
            }
            Err(_) => {
                println!("⚠️  Not in local, trying remote...");
                let pkg = remote.get_package(id).await?;

                // Cache in local for future use
                local.publish(pkg.clone()).await?;
                println!("✅ Downloaded and cached locally");

                Ok(pkg)
            }
        }
    }

    let pkg = find_package(
        &PackageId::new("ggen", "rust-web-service"),
        &local,
        &remote,
    ).await?;

    println!("Got package: {}", pkg.id);

    Ok(())
}

// Example: Bulk operations
async fn bulk_operations_example() -> Result<()> {
    println!("📦 Bulk Operations Example");

    let registry = LocalRegistry::new(
        PathBuf::from("~/.ggen/registry")
    ).await?;

    // Publish multiple packages at once
    let packages = vec![
        ("rust-cli", Category::Utility),
        ("python-api", Category::WebService),
        ("docker-compose", Category::Deployment),
        ("monitoring-stack", Category::Monitoring),
    ];

    for (name, category) in packages {
        let pkg = Package::builder(
            PackageId::new("ggen", name),
            Version::new(1, 0, 0),
        )
        .title(format!("{} Template", name))
        .description(format!("Production-ready {} template", name))
        .license("MIT")
        .category(category)
        .content_id(ContentId::new(
            format!("sha256:{}", name),
            ggen_marketplace::models::package::HashAlgorithm::Sha256,
        ))
        .build()?;

        registry.publish(pkg).await?;
    }

    println!("✅ Published {} packages", packages.len());

    // Search across all
    let all = registry.search(&Query::new("")).await?;
    println!("Total packages in registry: {}", all.len());

    Ok(())
}

// Example: Error handling
async fn error_handling_example() -> Result<()> {
    println!("⚠️  Error Handling Example");

    let registry = LocalRegistry::new(
        PathBuf::from("~/.ggen/registry")
    ).await?;

    // Handle non-existent package
    match registry.get_package(&PackageId::new("ggen", "nonexistent")).await {
        Ok(pkg) => println!("Found: {}", pkg.id),
        Err(e) => println!("❌ Error: {}", e),
    }

    // Handle duplicate version
    let pkg = Package::builder(
        PackageId::new("ggen", "test"),
        Version::new(1, 0, 0),
    )
    .title("Test")
    .description("Test package")
    .license("MIT")
    .content_id(ContentId::new(
        "sha256:test",
        ggen_marketplace::models::package::HashAlgorithm::Sha256,
    ))
    .build()?;

    registry.publish(pkg.clone()).await?;

    match registry.publish(pkg).await {
        Ok(_) => println!("Published"),
        Err(e) => println!("❌ Error: {}", e),
    }

    Ok(())
}
