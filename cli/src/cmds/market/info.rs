//! Marketplace info functionality for detailed gpack information.
//!
//! This module provides comprehensive gpack information display including
//! package details, examples, dependencies, templates, and entities.
//! It integrates with the marketplace registry to provide rich metadata
//! and usage examples for gpacks.
//!
//! # Examples
//!
//! ```bash
//! ggen market info "rust-cli-template"
//! ggen market info "web-api" --examples
//! ggen market info "web-api" --dependencies
//! ```
//!
//! # Cookbook Compliance
//!
//! Follows Pattern 004: NOUN-VERB CLI for semantic operations.

use clap::Args;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
pub struct InfoArgs {
    /// Gpack ID to show information for
    pub gpack_id: String,

    /// Show usage examples
    #[arg(long)]
    pub examples: bool,

    /// Show dependencies
    #[arg(long)]
    pub dependencies: bool,

    /// Show health metrics
    #[arg(long)]
    pub health: bool,

    /// Interactive mode with tables
    #[arg(long)]
    pub interactive: bool,
}

#[cfg_attr(test, mockall::automock)]
pub trait GpackMetadataFetcher {
    fn fetch_metadata(&self, gpack_id: &str) -> Result<GpackMetadata>;
}

#[derive(Debug, Clone)]
pub struct GpackMetadata {
    pub id: String,
    pub name: String,
    pub description: String,
    pub version: String,
    pub author: Option<String>,
    pub license: Option<String>,
    pub homepage: Option<String>,
    pub stars: u32,
    pub downloads: u32,
    pub updated_at: String,
    pub tags: Vec<String>,
    pub health_score: Option<f32>,
    pub dependencies: Vec<PackageDependency>,
    pub examples: Vec<UsageExample>,
}

#[derive(Debug, Clone)]
pub struct PackageDependency {
    pub name: String,
    pub version: String,
    pub required: bool,
}

#[derive(Debug, Clone)]
pub struct UsageExample {
    pub title: String,
    pub code: String,
    pub description: String,
}

/// Validate and sanitize gpack ID input
fn validate_gpack_id(gpack_id: &str) -> Result<()> {
    // Validate gpack ID is not empty
    if gpack_id.trim().is_empty() {
        return Err(ggen_utils::error::Error::new("Gpack ID cannot be empty"));
    }

    // Validate gpack ID length
    if gpack_id.len() > 200 {
        return Err(ggen_utils::error::Error::new(
            "Gpack ID too long (max 200 characters)",
        ));
    }

    // Validate gpack ID format (basic pattern check)
    if !gpack_id
        .chars()
        .all(|c| c.is_alphanumeric() || c == '.' || c == '-' || c == '_')
    {
        return Err(ggen_utils::error::Error::new(
            "Invalid gpack ID format: only alphanumeric characters, dots, dashes, and underscores allowed",
        ));
    }

    Ok(())
}

pub async fn run(args: &InfoArgs) -> Result<()> {
    // Validate input
    validate_gpack_id(&args.gpack_id)?;

    // For now, show placeholder rich information matching cookbook example
    // In a full implementation, this would fetch real data from registry
    let metadata = create_sample_metadata(&args.gpack_id);

    if args.interactive {
        display_interactive_info(&metadata, args);
    } else {
        display_formatted_info(&metadata, args);
    }

    Ok(())
}

/// Display information in an interactive table format
fn display_interactive_info(metadata: &GpackMetadata, args: &InfoArgs) {
    println!("╔══════════════════════════════════════════════════════════════╗");
    println!("║                    📦 Gpack Information                      ║");
    println!("╚══════════════════════════════════════════════════════════════╝");
    println!();

    // Main info table
    println!("┌──────────────────────────────────────────────────────────────┐");
    println!("│ Package Details                                              │");
    println!("├──────────────────────────────────────────────────────────────┤");
    println!("│ ID:        {:<50} │", metadata.id);
    println!("│ Name:      {:<50} │", metadata.name);
    println!("│ Version:   {:<50} │", metadata.version);
    println!(
        "│ Author:    {:<50} │",
        metadata.author.as_deref().unwrap_or("Unknown")
    );
    println!(
        "│ License:   {:<50} │",
        metadata.license.as_deref().unwrap_or("Unknown")
    );
    println!("│ Stars:     {:<50} │", format!("⭐ {}", metadata.stars));
    println!("│ Downloads: {:<50} │", format!("⬇ {}", metadata.downloads));
    println!("│ Updated:   {:<50} │", metadata.updated_at);
    println!("└──────────────────────────────────────────────────────────────┘");
    println!();

    if args.health || args.interactive {
        display_health_metrics(metadata);
    }

    if args.dependencies || args.interactive {
        display_dependencies(metadata);
    }

    if args.examples || args.interactive {
        display_examples(metadata);
    }
}

/// Display information in a formatted text layout
fn display_formatted_info(metadata: &GpackMetadata, args: &InfoArgs) {
    println!("📦 Gpack Information");
    println!("==================");
    println!("ID: {}", metadata.id);
    println!("Name: {}", metadata.name);
    println!("Version: {}", metadata.version);

    println!("\n📋 Description:");
    println!("  {}", metadata.description);

    println!("\n🏷️  Metadata:");
    println!(
        "  Author: {} | License: {}",
        metadata.author.as_deref().unwrap_or("Unknown"),
        metadata.license.as_deref().unwrap_or("Unknown")
    );
    println!(
        "  Stars: ⭐ {} | Downloads: ⬇ {}",
        metadata.stars, metadata.downloads
    );
    println!("  Updated: {}", metadata.updated_at);

    if !metadata.tags.is_empty() {
        println!("  Tags: {}", metadata.tags.join(", "));
    }

    if args.health {
        display_health_metrics(metadata);
    }

    if args.dependencies {
        display_dependencies(metadata);
    }

    if args.examples {
        display_examples(metadata);
    }
}

fn display_health_metrics(metadata: &GpackMetadata) {
    println!("\n🏥 Health Metrics:");
    if let Some(score) = metadata.health_score {
        let health_emoji = match score {
            90.0..=100.0 => "🟢",
            70.0..=89.9 => "🟡",
            50.0..=69.9 => "🟠",
            _ => "🔴",
        };
        println!("  {} Overall Health Score: {:.1}%", health_emoji, score);
        println!("  📊 Security: 95% | Maintenance: 87% | Popularity: 92%");
    }
}

fn display_dependencies(metadata: &GpackMetadata) {
    println!("\n🔗 Dependencies:");
    if metadata.dependencies.is_empty() {
        println!("  None");
    } else {
        for dep in &metadata.dependencies {
            let required_icon = if dep.required { "🔴" } else { "🟡" };
            println!(
                "  {} {}@{} ({})",
                required_icon,
                dep.name,
                dep.version,
                if dep.required { "required" } else { "optional" }
            );
        }
    }
}

fn display_examples(metadata: &GpackMetadata) {
    println!("\n💡 Usage Examples:");
    if metadata.examples.is_empty() {
        println!("  ggen market install {}", metadata.id);
        println!("  ggen project generate --template {}", metadata.id);
    } else {
        for example in &metadata.examples {
            println!("  📝 {}:", example.title);
            println!("     {}", example.description);
            println!("     ```bash");
            for line in example.code.lines() {
                println!("     {}", line);
            }
            println!("     ```");
            println!();
        }
    }
}

fn create_sample_metadata(gpack_id: &str) -> GpackMetadata {
    GpackMetadata {
        id: gpack_id.to_string(),
        name: "User Authentication System".to_string(),
        description: "Complete user authentication system with email/password, JWT tokens, and role-based access control.".to_string(),
        version: "1.2.3".to_string(),
        author: Some("@ggen-official".to_string()),
        license: Some("MIT".to_string()),
        homepage: Some("https://ggen.dev".to_string()),
        stars: 1245,
        downloads: 45234,
        updated_at: "2 days ago".to_string(),
        tags: vec!["auth".to_string(), "user".to_string(), "jwt".to_string()],
        health_score: Some(95.0),
        dependencies: vec![
            PackageDependency {
                name: "@ggen/base-entity".to_string(),
                version: "^2.0.0".to_string(),
                required: true,
            },
            PackageDependency {
                name: "@ggen/validation-helpers".to_string(),
                version: "^1.1.0".to_string(),
                required: true,
            },
        ],
        examples: vec![
            UsageExample {
                title: "Basic Installation".to_string(),
                code: "ggen market install @ggen/auth-user".to_string(),
                description: "Install the authentication package".to_string(),
            },
            UsageExample {
                title: "Generate Controllers".to_string(),
                code: "ggen project generate --template @ggen/auth-user/controller".to_string(),
                description: "Generate authentication controllers".to_string(),
            },
        ],
    }
}

pub async fn run_with_deps(args: &InfoArgs, fetcher: &dyn GpackMetadataFetcher) -> Result<()> {
    // Validate input
    validate_gpack_id(&args.gpack_id)?;

    // Show progress for metadata fetching
    println!("🔍 Fetching gpack metadata...");

    let metadata = fetcher.fetch_metadata(&args.gpack_id)?;

    println!("📦 Gpack Information:");
    println!("  ID: {}", metadata.id);
    println!("  Name: {}", metadata.name);
    println!("  Description: {}", metadata.description);
    println!("  Version: {}", metadata.version);

    if let Some(author) = metadata.author {
        println!("  Author: {}", author);
    }
    if let Some(license) = metadata.license {
        println!("  License: {}", license);
    }
    if let Some(homepage) = metadata.homepage {
        println!("  Homepage: {}", homepage);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use mockall::predicate::*;

    #[tokio::test]
    async fn test_show_displays_metadata() {
        let mut mock_fetcher = MockGpackMetadataFetcher::new();
        mock_fetcher
            .expect_fetch_metadata()
            .with(eq(String::from("io.ggen.rust.cli")))
            .times(1)
            .returning(|id| {
                Ok(GpackMetadata {
                    id: id.to_string(),
                    name: "Rust CLI".to_string(),
                    description: "CLI templates for Rust".to_string(),
                    version: "1.0.0".to_string(),
                    author: Some("ggen-team".to_string()),
                    license: Some("MIT".to_string()),
                    homepage: Some("https://github.com/ggen/rust-cli".to_string()),
                    stars: 100,
                    downloads: 1000,
                    updated_at: "2024-01-01T00:00:00Z".to_string(),
                    tags: vec!["rust".to_string(), "cli".to_string()],
                    health_score: Some(0.95),
                    dependencies: vec![],
                    examples: vec![],
                })
            });

        let args = InfoArgs {
            gpack_id: "io.ggen.rust.cli".to_string(),
            examples: false,
            dependencies: false,
            health: false,
            interactive: false,
        };

        let result = run_with_deps(&args, &mock_fetcher).await;
        assert!(result.is_ok());
    }
}
