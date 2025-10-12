use clap::Args;
use ggen_utils::error::Result;
use std::collections::HashMap;
use std::path::PathBuf;

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

pub async fn run(args: &RemoveArgs) -> Result<()> {
    // Validate input
    validate_gpack_id(&args.gpack_id)?;

    // Create a default uninstaller implementation
    let uninstaller = DefaultGpackUninstaller::new()?;
    run_with_deps(args, &uninstaller).await
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

/// Default implementation of GpackUninstaller
pub struct DefaultGpackUninstaller {
    lockfile_path: PathBuf,
    gpack_dir: PathBuf,
}

impl DefaultGpackUninstaller {
    pub fn new() -> Result<Self> {
        let lockfile_path = PathBuf::from(".ggen/gpack.lock");
        let gpack_dir = PathBuf::from(".ggen/gpacks");

        Ok(Self {
            lockfile_path,
            gpack_dir,
        })
    }

    /// Load installed gpacks from lockfile
    fn load_installed_gpacks(&self) -> Result<HashMap<String, serde_json::Value>> {
        if !self.lockfile_path.exists() {
            return Ok(HashMap::new());
        }

        let content = std::fs::read_to_string(&self.lockfile_path).map_err(|e| {
            ggen_utils::error::Error::new_fmt(format_args!("Failed to read lockfile: {}", e))
        })?;

        let lockfile: serde_json::Value = serde_json::from_str(&content).map_err(|e| {
            ggen_utils::error::Error::new_fmt(format_args!("Failed to parse lockfile: {}", e))
        })?;

        let mut gpacks = HashMap::new();
        if let Some(installed) = lockfile.get("installed").and_then(|v| v.as_object()) {
            for (id, version_info) in installed {
                gpacks.insert(id.clone(), version_info.clone());
            }
        }

        Ok(gpacks)
    }

    /// Remove gpack from lockfile
    fn remove_from_lockfile(&self, gpack_id: &str) -> Result<()> {
        if !self.lockfile_path.exists() {
            return Ok(());
        }

        let content = std::fs::read_to_string(&self.lockfile_path).map_err(|e| {
            ggen_utils::error::Error::new_fmt(format_args!("Failed to read lockfile: {}", e))
        })?;

        let mut lockfile: serde_json::Value = serde_json::from_str(&content).map_err(|e| {
            ggen_utils::error::Error::new_fmt(format_args!("Failed to parse lockfile: {}", e))
        })?;

        // Remove from installed gpacks
        if let Some(installed) = lockfile
            .get_mut("installed")
            .and_then(|v| v.as_object_mut())
        {
            installed.remove(gpack_id);
        }

        // Write updated lockfile
        let content = serde_json::to_string_pretty(&lockfile).map_err(|e| {
            ggen_utils::error::Error::new_fmt(format_args!("Failed to serialize lockfile: {}", e))
        })?;

        std::fs::write(&self.lockfile_path, content).map_err(|e| {
            ggen_utils::error::Error::new_fmt(format_args!("Failed to write lockfile: {}", e))
        })?;

        Ok(())
    }

    /// Remove gpack files from filesystem
    fn remove_gpack_files(&self, gpack_id: &str) -> Result<()> {
        let gpack_path = self.gpack_dir.join(gpack_id);

        if gpack_path.exists() {
            std::fs::remove_dir_all(&gpack_path).map_err(|e| {
                ggen_utils::error::Error::new_fmt(format_args!(
                    "Failed to remove gpack directory '{}': {}",
                    gpack_path.display(),
                    e
                ))
            })?;
        }

        Ok(())
    }
}

impl GpackUninstaller for DefaultGpackUninstaller {
    fn uninstall(&self, gpack_id: &str) -> Result<bool> {
        // Load installed gpacks
        let installed_gpacks = self.load_installed_gpacks()?;

        // Check if gpack is installed
        if !installed_gpacks.contains_key(gpack_id) {
            return Ok(false);
        }

        // Remove from lockfile
        self.remove_from_lockfile(gpack_id)?;

        // Remove files
        self.remove_gpack_files(gpack_id)?;

        Ok(true)
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
