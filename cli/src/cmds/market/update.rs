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

pub async fn run(args: &UpdateArgs) -> Result<()> {
    println!("🚧 Placeholder: market update");
    println!("  Gpack ID: {:?}", args.gpack_id);
    Ok(())
}

pub async fn run_with_deps(args: &UpdateArgs, updater: &dyn GpackUpdater) -> Result<()> {
    let results = updater.update(args.gpack_id.clone())?;

    if results.is_empty() {
        println!("No gpacks to update");
        return Ok(());
    }

    for result in results {
        if result.updated {
            println!(
                "Updated {} from {} to {}",
                result.gpack_id, result.old_version, result.new_version
            );
        } else {
            println!(
                "{} is already up to date ({})",
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
            .with(eq(Some("io.ggen.rust.cli")))
            .times(1)
            .returning(|_| Ok(vec![]));

        let args = UpdateArgs {
            gpack_id: Some("io.ggen.rust.cli".to_string()),
        };

        let result = run_with_deps(&args, &mock_updater).await;
        assert!(result.is_ok());
    }
}
