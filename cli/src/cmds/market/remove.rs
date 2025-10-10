use clap::Args;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
pub struct RemoveArgs {
    /// Gpack ID to remove
    pub gpack_id: String,
}

#[cfg_attr(test, mockall::automock)]
pub trait GpackUninstaller {
    fn uninstall(&self, gpack_id: &str) -> Result<bool>;
}

pub async fn run(args: &RemoveArgs) -> Result<()> {
    println!("🚧 Placeholder: market remove");
    println!("  Gpack ID: {}", args.gpack_id);
    Ok(())
}

pub async fn run_with_deps(args: &RemoveArgs, uninstaller: &dyn GpackUninstaller) -> Result<()> {
    let was_installed = uninstaller.uninstall(&args.gpack_id)?;

    if was_installed {
        println!("✅ Successfully removed gpack '{}'", args.gpack_id);
        Ok(())
    } else {
        Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Gpack '{}' is not installed",
            args.gpack_id
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mockall::predicate::*;

    #[tokio::test]
    async fn test_remove_uninstalls_gpack() {
        let mut mock_uninstaller = MockGpackUninstaller::new();
        mock_uninstaller
            .expect_uninstall()
            .with(eq("io.ggen.rust.cli"))
            .times(1)
            .returning(|_| Ok(true));

        let args = RemoveArgs {
            gpack_id: "io.ggen.rust.cli".to_string(),
        };

        let result = run_with_deps(&args, &mock_uninstaller).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_remove_fails_if_not_installed() {
        let mut mock_uninstaller = MockGpackUninstaller::new();
        mock_uninstaller
            .expect_uninstall()
            .returning(|_| Ok(false));

        let args = RemoveArgs {
            gpack_id: "io.ggen.nonexistent".to_string(),
        };

        let result = run_with_deps(&args, &mock_uninstaller).await;
        assert!(result.is_err());
    }
}
