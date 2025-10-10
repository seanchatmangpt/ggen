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

/// Validate and sanitize gpack ID input
fn validate_gpack_id(gpack_id: &str) -> Result<()> {
    // Validate gpack ID is not empty
    if gpack_id.trim().is_empty() {
        return Err(ggen_utils::error::Error::new(
            "Gpack ID cannot be empty",
        ));
    }
    
    // Validate gpack ID length
    if gpack_id.len() > 200 {
        return Err(ggen_utils::error::Error::new(
            "Gpack ID too long (max 200 characters)",
        ));
    }
    
    // Validate gpack ID format (basic pattern check)
    if !gpack_id.chars().all(|c| c.is_alphanumeric() || c == '.' || c == '-' || c == '_') {
        return Err(ggen_utils::error::Error::new(
            "Invalid gpack ID format: only alphanumeric characters, dots, dashes, and underscores allowed",
        ));
    }
    
    Ok(())
}

pub async fn run(args: &RemoveArgs) -> Result<()> {
    // Validate input
    validate_gpack_id(&args.gpack_id)?;
    
    println!("🚧 Placeholder: market remove");
    println!("  Gpack ID: {}", args.gpack_id.trim());
    Ok(())
}

pub async fn run_with_deps(args: &RemoveArgs, uninstaller: &dyn GpackUninstaller) -> Result<()> {
    // Validate input
    validate_gpack_id(&args.gpack_id)?;
    
    // Show progress for removal
    println!("🔍 Removing gpack...");
    
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
            .with(eq(String::from("io.ggen.rust.cli")))
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
        mock_uninstaller.expect_uninstall().returning(|_| Ok(false));

        let args = RemoveArgs {
            gpack_id: "io.ggen.nonexistent".to_string(),
        };

        let result = run_with_deps(&args, &mock_uninstaller).await;
        assert!(result.is_err());
    }
}
