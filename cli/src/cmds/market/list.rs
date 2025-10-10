use clap::Args;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
pub struct ListArgs {
    /// Show detailed information
    #[arg(long)]
    pub detailed: bool,
}

#[cfg_attr(test, mockall::automock)]
pub trait GpackLister {
    fn list_installed(&self) -> Result<Vec<InstalledGpack>>;
}

#[derive(Debug, Clone)]
pub struct InstalledGpack {
    pub id: String,
    pub version: String,
    pub sha256: String,
    pub source: String,
}

pub async fn run(args: &ListArgs) -> Result<()> {
    println!("🚧 Placeholder: market list");
    println!("  Detailed: {}", args.detailed);
    Ok(())
}

pub async fn run_with_deps(args: &ListArgs, lister: &dyn GpackLister) -> Result<()> {
    let gpacks = lister.list_installed()?;

    if gpacks.is_empty() {
        println!("No gpacks installed");
        return Ok(());
    }

    for gpack in gpacks {
        if args.detailed {
            println!("ID: {}", gpack.id);
            println!("Version: {}", gpack.version);
            println!("SHA256: {}", gpack.sha256);
            println!("Source: {}", gpack.source);
            println!();
        } else {
            println!("{} ({})", gpack.id, gpack.version);
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_list_displays_installed_gpacks() {
        let mut mock_lister = MockGpackLister::new();
        mock_lister
            .expect_list_installed()
            .times(1)
            .returning(|| {
                Ok(vec![InstalledGpack {
                    id: "io.ggen.rust.cli".to_string(),
                    version: "1.0.0".to_string(),
                    sha256: "abc123".to_string(),
                    source: "https://github.com/example/repo".to_string(),
                }])
            });

        let args = ListArgs { detailed: false };

        let result = run_with_deps(&args, &mock_lister).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_list_empty() {
        let mut mock_lister = MockGpackLister::new();
        mock_lister
            .expect_list_installed()
            .times(1)
            .returning(|| Ok(vec![]));

        let args = ListArgs { detailed: false };

        let result = run_with_deps(&args, &mock_lister).await;
        assert!(result.is_ok());
    }
}
