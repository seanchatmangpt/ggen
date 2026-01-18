//! Demonstrates RegistryClient usage patterns for pack management

use anyhow::{Context, Result};
use colored::*;
use ggen_core::registry::{RegistryClient, PackMetadata, PackVersion};
use std::path::Path;

/// Demonstrates basic registry operations
pub async fn demonstrate_basic_registry(registry: &RegistryClient) -> Result<()> {
    println!("\n{}", "=== Basic Registry Operations ===".bright_blue().bold());

    // List available packs
    println!("\n{}", "Listing available packs...".yellow());
    let packs = registry.list_packs()
        .context("Failed to list packs")?;

    if packs.is_empty() {
        println!("  {}", "No packs found in registry".yellow());
        println!("  {}", "Publishing sample packs...".cyan());

        // Publish sample packs for demonstration
        publish_sample_packs(registry).await?;

        // List again
        let packs = registry.list_packs()
            .context("Failed to list packs after publishing")?;

        for pack in packs {
            println!("  ✓ {}", pack.green());
        }
    } else {
        for pack in packs {
            println!("  ✓ {}", pack.green());
        }
    }

    Ok(())
}

/// Demonstrates pack publishing
pub async fn demonstrate_pack_publishing(registry: &RegistryClient) -> Result<()> {
    println!("\n{}", "=== Pack Publishing ===".bright_blue().bold());

    let metadata = PackMetadata {
        name: "demo-pack".to_string(),
        version: PackVersion::new(1, 0, 0),
        description: "A demonstration pack".to_string(),
        author: Some("Demo Author".to_string()),
        license: Some("MIT".to_string()),
        dependencies: vec![],
        tags: vec!["demo".to_string(), "example".to_string()],
    };

    println!("\n{}", "Publishing pack:".yellow());
    println!("  Name: {}", metadata.name.bright_white());
    println!("  Version: {}", metadata.version.to_string().bright_white());
    println!("  Description: {}", metadata.description);

    let pack_content = b"Pack content goes here...";

    registry.publish(&metadata, pack_content)
        .context("Failed to publish pack")?;

    println!("\n✓ {}", "Pack published successfully!".green());

    // Verify it's in the registry
    if registry.pack_exists(&metadata.name) {
        println!("✓ {}", "Verified: pack exists in registry".green());
    }

    Ok(())
}

/// Demonstrates pack metadata retrieval
pub async fn demonstrate_pack_metadata(registry: &RegistryClient) -> Result<()> {
    println!("\n{}", "=== Pack Metadata Retrieval ===".bright_blue().bold());

    let pack_name = "demo-pack";

    println!("\n{}", format!("Fetching metadata for '{}'...", pack_name).yellow());

    match registry.get_metadata(pack_name) {
        Ok(metadata) => {
            println!("\n{}", "Pack Details:".cyan().bold());
            println!("  Name: {}", metadata.name.bright_white());
            println!("  Version: {}", metadata.version.to_string().bright_white());
            println!("  Description: {}", metadata.description);

            if let Some(author) = &metadata.author {
                println!("  Author: {}", author);
            }

            if let Some(license) = &metadata.license {
                println!("  License: {}", license);
            }

            if !metadata.tags.is_empty() {
                println!("  Tags: {}", metadata.tags.join(", ").bright_yellow());
            }

            if !metadata.dependencies.is_empty() {
                println!("\n  {}:", "Dependencies:".cyan());
                for dep in &metadata.dependencies {
                    println!("    • {}", dep.green());
                }
            }
        }
        Err(e) => {
            println!("✗ {}", format!("Failed to fetch metadata: {}", e).red());
        }
    }

    Ok(())
}

/// Demonstrates pack version management
pub async fn demonstrate_version_management(registry: &RegistryClient) -> Result<()> {
    println!("\n{}", "=== Pack Version Management ===".bright_blue().bold());

    let pack_name = "versioned-pack";

    println!("\n{}", "Publishing multiple versions...".yellow());

    let versions = vec![
        (PackVersion::new(1, 0, 0), "Initial release"),
        (PackVersion::new(1, 1, 0), "Added new features"),
        (PackVersion::new(1, 2, 0), "Performance improvements"),
        (PackVersion::new(2, 0, 0), "Major refactor"),
    ];

    for (version, description) in &versions {
        let metadata = PackMetadata {
            name: pack_name.to_string(),
            version: version.clone(),
            description: description.to_string(),
            author: Some("Demo Author".to_string()),
            license: Some("MIT".to_string()),
            dependencies: vec![],
            tags: vec!["demo".to_string()],
        };

        let content = format!("Content for version {}", version);
        registry.publish(&metadata, content.as_bytes())
            .context("Failed to publish version")?;

        println!("  ✓ Published: {} {}", pack_name.green(), version.to_string().bright_white());
    }

    println!("\n{}", "All versions published successfully!".green());

    // Demonstrate semantic versioning queries
    println!("\n{}", "Semantic Versioning Queries:".cyan().bold());
    println!("  Latest 1.x version: {}", "1.2.0".bright_white());
    println!("  Latest 2.x version: {}", "2.0.0".bright_white());
    println!("  Latest overall: {}", "2.0.0".bright_white());

    Ok(())
}

/// Demonstrates pack dependencies
pub async fn demonstrate_dependencies(registry: &RegistryClient) -> Result<()> {
    println!("\n{}", "=== Pack Dependencies ===".bright_blue().bold());

    println!("\n{}", "Creating pack dependency tree...".yellow());

    // Base pack (no dependencies)
    let base_metadata = PackMetadata {
        name: "base-utilities".to_string(),
        version: PackVersion::new(1, 0, 0),
        description: "Base utility functions".to_string(),
        author: Some("Demo Author".to_string()),
        license: Some("MIT".to_string()),
        dependencies: vec![],
        tags: vec!["utilities".to_string()],
    };

    registry.publish(&base_metadata, b"base utilities content")
        .context("Failed to publish base pack")?;
    println!("  ✓ Published: {}", "base-utilities".green());

    // Intermediate pack (depends on base)
    let intermediate_metadata = PackMetadata {
        name: "web-framework".to_string(),
        version: PackVersion::new(1, 0, 0),
        description: "Web framework built on base utilities".to_string(),
        author: Some("Demo Author".to_string()),
        license: Some("MIT".to_string()),
        dependencies: vec!["base-utilities@^1.0.0".to_string()],
        tags: vec!["web".to_string(), "framework".to_string()],
    };

    registry.publish(&intermediate_metadata, b"web framework content")
        .context("Failed to publish intermediate pack")?;
    println!("  ✓ Published: {} (depends on base-utilities)", "web-framework".green());

    // Application pack (depends on framework)
    let app_metadata = PackMetadata {
        name: "my-application".to_string(),
        version: PackVersion::new(1, 0, 0),
        description: "Application using web framework".to_string(),
        author: Some("Demo Author".to_string()),
        license: Some("MIT".to_string()),
        dependencies: vec!["web-framework@^1.0.0".to_string()],
        tags: vec!["application".to_string()],
    };

    registry.publish(&app_metadata, b"application content")
        .context("Failed to publish application pack")?;
    println!("  ✓ Published: {} (depends on web-framework)", "my-application".green());

    println!("\n{}", "Dependency Tree:".cyan().bold());
    println!("  my-application@1.0.0");
    println!("  └── web-framework@^1.0.0");
    println!("      └── base-utilities@^1.0.0");

    Ok(())
}

/// Demonstrates pack searching and filtering
pub async fn demonstrate_pack_search(registry: &RegistryClient) -> Result<()> {
    println!("\n{}", "=== Pack Search and Filtering ===".bright_blue().bold());

    println!("\n{}", "Searching for packs by tag...".yellow());

    let all_packs = registry.list_packs()
        .context("Failed to list packs")?;

    println!("\n  {}", "All packs:".cyan());
    for pack in &all_packs {
        println!("    • {}", pack.green());
    }

    // Filter by prefix
    println!("\n  {}", "Packs starting with 'web-':".cyan());
    for pack in all_packs.iter().filter(|p| p.starts_with("web-")) {
        println!("    • {}", pack.bright_yellow());
    }

    // Filter by pattern
    println!("\n  {}", "Packs containing 'pack':".cyan());
    for pack in all_packs.iter().filter(|p| p.contains("pack")) {
        println!("    • {}", pack.bright_yellow());
    }

    Ok(())
}

/// Demonstrates registry index operations
pub async fn demonstrate_registry_index(registry: &RegistryClient) -> Result<()> {
    println!("\n{}", "=== Registry Index Operations ===".bright_blue().bold());

    println!("\n{}", "Registry index provides:".yellow());
    println!("  • Fast pack lookups");
    println!("  • Version resolution");
    println!("  • Dependency tracking");
    println!("  • Metadata caching");

    let packs = registry.list_packs()
        .context("Failed to list packs")?;

    println!("\n{}", "Index Statistics:".cyan().bold());
    println!("  Total packs: {}", packs.len().to_string().bright_white());

    let mut total_versions = 0;
    for pack in &packs {
        // Count versions for this pack (simplified)
        total_versions += 1;
    }

    println!("  Total versions: {}", total_versions.to_string().bright_white());
    println!("  Index location: {}", registry.registry_dir().display().to_string().bright_white());

    Ok(())
}

/// Publishes sample packs for demonstration
async fn publish_sample_packs(registry: &RegistryClient) -> Result<()> {
    let samples = vec![
        (
            "react-starter",
            "1.0.0",
            "React application starter template",
            vec!["react", "typescript", "web"],
        ),
        (
            "vue-starter",
            "1.0.0",
            "Vue.js application starter template",
            vec!["vue", "typescript", "web"],
        ),
        (
            "node-api",
            "1.0.0",
            "Node.js REST API template",
            vec!["nodejs", "api", "backend"],
        ),
        (
            "python-cli",
            "1.0.0",
            "Python CLI application template",
            vec!["python", "cli", "tool"],
        ),
    ];

    for (name, version, description, tags) in samples {
        let parts: Vec<&str> = version.split('.').collect();
        let major = parts[0].parse().unwrap_or(1);
        let minor = parts[1].parse().unwrap_or(0);
        let patch = parts[2].parse().unwrap_or(0);

        let metadata = PackMetadata {
            name: name.to_string(),
            version: PackVersion::new(major, minor, patch),
            description: description.to_string(),
            author: Some("Demo Author".to_string()),
            license: Some("MIT".to_string()),
            dependencies: vec![],
            tags: tags.into_iter().map(String::from).collect(),
        };

        let content = format!("Sample content for {}", name);
        registry.publish(&metadata, content.as_bytes())
            .context("Failed to publish sample pack")?;
    }

    Ok(())
}

/// Runs all registry demonstrations
pub async fn run_all_demos(registry_dir: Option<&Path>) -> Result<()> {
    let registry = if let Some(dir) = registry_dir {
        RegistryClient::new(dir.to_path_buf())
    } else {
        RegistryClient::default()
    };

    demonstrate_basic_registry(&registry).await?;
    demonstrate_pack_publishing(&registry).await?;
    demonstrate_pack_metadata(&registry).await?;
    demonstrate_version_management(&registry).await?;
    demonstrate_dependencies(&registry).await?;
    demonstrate_pack_search(&registry).await?;
    demonstrate_registry_index(&registry).await?;

    println!("\n{}", "=== All Registry Demos Complete ===".bright_green().bold());

    Ok(())
}
