use clap::Args;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
pub struct UpdateArgs {
    /// Specific gpack to update (updates all if not specified)
    pub gpack_id: Option<String>,
}

#[cfg_attr(test, mockall::automock)]
pub trait GpackUpdater {
    fn update(&self, gpack_id: Option<String>) -> Result<Vec<UpdateResult>>;
}

#[derive(Debug, Clone)]
pub struct UpdateResult {
    pub gpack_id: String,
    pub old_version: String,
    pub new_version: String,
    pub updated: bool,
}

/// Validate and sanitize gpack ID input (if provided)
fn validate_gpack_id(gpack_id: &Option<String>) -> Result<()> {
    if let Some(id) = gpack_id {
        // Validate gpack ID is not empty
        if id.trim().is_empty() {
            return Err(ggen_utils::error::Error::new("Gpack ID cannot be empty"));
        }

        // Validate gpack ID length
        if id.len() > 200 {
            return Err(ggen_utils::error::Error::new(
                "Gpack ID too long (max 200 characters)",
            ));
        }

        // Validate gpack ID format (basic pattern check)
        if !id
            .chars()
            .all(|c| c.is_alphanumeric() || c == '.' || c == '-' || c == '_')
        {
            return Err(ggen_utils::error::Error::new(
                "Invalid gpack ID format: only alphanumeric characters, dots, dashes, and underscores allowed",
            ));
        }
    }

    Ok(())
}

pub async fn run(args: &UpdateArgs) -> Result<()> {
    // Validate input
    validate_gpack_id(&args.gpack_id)?;

    println!("🚧 Placeholder: market update");
    if let Some(id) = &args.gpack_id {
        println!("  Gpack ID: {}", id.trim());
    } else {
        println!("  Updating all gpacks");
    }
    Ok(())
}

pub async fn run_with_deps(args: &UpdateArgs, updater: &dyn GpackUpdater) -> Result<()> {
    // Validate input
    validate_gpack_id(&args.gpack_id)?;

    // Show progress for update operation
    if args.gpack_id.is_some() {
        println!("🔍 Checking for updates...");
    } else {
        println!("🔍 Checking all gpacks for updates...");
    }

    let results = updater.update(args.gpack_id.clone())?;

    if results.is_empty() {
        println!("ℹ️  No gpacks to update");
        return Ok(());
    }

    // Show progress for large result sets
    if results.len() > 10 {
        println!("📊 Processing {} gpacks...", results.len());
    }

    for result in results {
        if result.updated {
            println!(
                "✅ Updated {} from {} to {}",
                result.gpack_id, result.old_version, result.new_version
            );
        } else {
            println!(
                "ℹ️  {} is already up to date ({})",
                result.gpack_id, result.old_version
            );
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use mockall::predicate::*;

    #[tokio::test]
    async fn test_update_all_gpacks() {
        let mut mock_updater = MockGpackUpdater::new();
        mock_updater
            .expect_update()
            .with(eq(None))
            .times(1)
            .returning(|_| {
                Ok(vec![UpdateResult {
                    gpack_id: "io.ggen.rust.cli".to_string(),
                    old_version: "1.0.0".to_string(),
                    new_version: "1.1.0".to_string(),
                    updated: true,
                }])
            });

        let args = UpdateArgs { gpack_id: None };

        let result = run_with_deps(&args, &mock_updater).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_update_specific_gpack() {
        let mut mock_updater = MockGpackUpdater::new();
        mock_updater
            .expect_update()
            .with(eq(Some(String::from("io.ggen.rust.cli"))))
            .times(1)
            .returning(|_| Ok(vec![]));

        let args = UpdateArgs {
            gpack_id: Some("io.ggen.rust.cli".to_string()),
        };

        let result = run_with_deps(&args, &mock_updater).await;
        assert!(result.is_ok());
    }
}
