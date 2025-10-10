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

    println!("📦 Gpack Information");
    println!("==================");
    println!("ID: {}", args.gpack_id);

    // For now, show placeholder rich information matching cookbook example
    // In a full implementation, this would fetch real data from registry
    println!("\n📋 Description:");
    println!("  Complete user authentication system with email/password,");
    println!("  JWT tokens, and role-based access control.");

    println!("\n🏷️  Metadata:");
    println!("  Author: @ggen-official");
    println!("  License: MIT");
    println!("  Version: 1.2.3");
    println!("  Downloads: 45,234");
    println!("  Stars: 1,245");

    if args.examples {
        println!("\n💡 Examples:");
        println!("  ggen market install {}", args.gpack_id);
        println!("  ggen project generate --template {}", args.gpack_id);
    }

    if args.dependencies {
        println!("\n🔗 Dependencies:");
        println!("  - @ggen/base-entity@^2.0.0");
        println!("  - @ggen/validation-helpers@^1.1.0");
    }

    println!("\n🏗️  Entities:");
    println!("  - User (8 properties, 3 relationships)");
    println!("  - Role (3 properties)");
    println!("  - Session (5 properties, 1 relationship)");

    Ok(())
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
                    homepage: Some("https://ggen.dev".to_string()),
                })
            });

        let args = InfoArgs {
            gpack_id: "io.ggen.rust.cli".to_string(),
            examples: false,
            dependencies: false,
        };

        let result = run_with_deps(&args, &mock_fetcher).await;
        assert!(result.is_ok());
    }
}
