use clap::Args;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
pub struct ShowArgs {
    /// Gpack ID to show
    pub gpack_id: String,
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

pub async fn run(args: &ShowArgs) -> Result<()> {
    println!("🚧 Placeholder: market show");
    println!("  Gpack ID: {}", args.gpack_id);
    Ok(())
}

pub async fn run_with_deps(args: &ShowArgs, fetcher: &dyn GpackMetadataFetcher) -> Result<()> {
    let metadata = fetcher.fetch_metadata(&args.gpack_id)?;

    println!("ID: {}", metadata.id);
    println!("Name: {}", metadata.name);
    println!("Description: {}", metadata.description);
    println!("Version: {}", metadata.version);

    if let Some(author) = metadata.author {
        println!("Author: {}", author);
    }
    if let Some(license) = metadata.license {
        println!("License: {}", license);
    }
    if let Some(homepage) = metadata.homepage {
        println!("Homepage: {}", homepage);
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
            .with(eq("io.ggen.rust.cli"))
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

        let args = ShowArgs {
            gpack_id: "io.ggen.rust.cli".to_string(),
        };

        let result = run_with_deps(&args, &mock_fetcher).await;
        assert!(result.is_ok());
    }
}
