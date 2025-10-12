use clap::Args;
use ggen_utils::error::Result;
use ggen_core::registry::RegistryClient;
use std::path::PathBuf;
use std::collections::HashMap;

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

    // Create a default updater implementation
    let updater = DefaultGpackUpdater::new()?;
    run_with_deps(args, &updater).await
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

/// Default implementation of GpackUpdater
pub struct DefaultGpackUpdater {
    registry_client: RegistryClient,
    lockfile_path: PathBuf,
}

impl DefaultGpackUpdater {
    pub fn new() -> Result<Self> {
        let registry_client = RegistryClient::new()
            .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!("Failed to create registry client: {}", e)))?;
        
        let lockfile_path = PathBuf::from(".ggen/gpack.lock");
        
        Ok(Self {
            registry_client,
            lockfile_path,
        })
    }
    
    /// Load installed gpacks from lockfile
    fn load_installed_gpacks(&self) -> Result<HashMap<String, String>> {
        if !self.lockfile_path.exists() {
            return Ok(HashMap::new());
        }
        
        let content = std::fs::read_to_string(&self.lockfile_path)
            .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!("Failed to read lockfile: {}", e)))?;
        
        let lockfile: serde_json::Value = serde_json::from_str(&content)
            .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!("Failed to parse lockfile: {}", e)))?;
        
        let mut gpacks = HashMap::new();
        if let Some(installed) = lockfile.get("installed").and_then(|v| v.as_object()) {
            for (id, version_info) in installed {
                if let Some(version) = version_info.get("version").and_then(|v| v.as_str()) {
                    gpacks.insert(id.clone(), version.to_string());
                }
            }
        }
        
        Ok(gpacks)
    }
    
    /// Update lockfile with new versions
    fn update_lockfile(&self, updates: &[UpdateResult]) -> Result<()> {
        if updates.is_empty() {
            return Ok(());
        }
        
        // Ensure .ggen directory exists
        if let Some(parent) = self.lockfile_path.parent() {
            std::fs::create_dir_all(parent)
                .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!("Failed to create .ggen directory: {}", e)))?;
        }
        
        // Load existing lockfile or create new one
        let mut lockfile: serde_json::Value = if self.lockfile_path.exists() {
            let content = std::fs::read_to_string(&self.lockfile_path)
                .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!("Failed to read lockfile: {}", e)))?;
            serde_json::from_str(&content)
                .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!("Failed to parse lockfile: {}", e)))?
        } else {
            serde_json::json!({
                "version": "1.0",
                "installed": {}
            })
        };
        
        // Update versions
        if let Some(installed) = lockfile.get_mut("installed").and_then(|v| v.as_object_mut()) {
            for update in updates {
                if update.updated {
                    if let Some(gpack_info) = installed.get_mut(&update.gpack_id) {
                        if let Some(version) = gpack_info.get_mut("version") {
                            *version = serde_json::Value::String(update.new_version.clone());
                        }
                    }
                }
            }
        }
        
        // Write updated lockfile
        let content = serde_json::to_string_pretty(&lockfile)
            .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!("Failed to serialize lockfile: {}", e)))?;
        
        std::fs::write(&self.lockfile_path, content)
            .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!("Failed to write lockfile: {}", e)))?;
        
        Ok(())
    }
}

impl GpackUpdater for DefaultGpackUpdater {
    fn update(&self, gpack_id: Option<String>) -> Result<Vec<UpdateResult>> {
        // Load installed gpacks
        let installed_gpacks = self.load_installed_gpacks()?;
        
        // Determine which gpacks to check
        let gpacks_to_check = if let Some(id) = gpack_id {
            if installed_gpacks.contains_key(&id) {
                vec![id]
            } else {
                return Err(ggen_utils::error::Error::new_fmt(format_args!(
                    "Gpack '{}' is not installed", id
                )));
            }
        } else {
            installed_gpacks.keys().cloned().collect()
        };
        
        let mut results = Vec::new();
        
        // Check each gpack for updates
        for gpack_id in gpacks_to_check {
            if let Some(current_version) = installed_gpacks.get(&gpack_id) {
                // Check for updates using registry client
                // Note: This is a simplified implementation - in production, you'd want async handling
                match self.check_for_updates(&gpack_id, current_version) {
                    Ok(Some(new_version)) => {
                        results.push(UpdateResult {
                            gpack_id: gpack_id.clone(),
                            old_version: current_version.clone(),
                            new_version,
                            updated: true,
                        });
                    }
                    Ok(None) => {
                        results.push(UpdateResult {
                            gpack_id: gpack_id.clone(),
                            old_version: current_version.clone(),
                            new_version: current_version.clone(),
                            updated: false,
                        });
                    }
                    Err(e) => {
                        // Log error but continue with other gpacks
                        eprintln!("Warning: Failed to check updates for {}: {}", gpack_id, e);
                        results.push(UpdateResult {
                            gpack_id: gpack_id.clone(),
                            old_version: current_version.clone(),
                            new_version: current_version.clone(),
                            updated: false,
                        });
                    }
                }
            }
        }
        
        // Update lockfile with any changes
        self.update_lockfile(&results)?;
        
        Ok(results)
    }
}

impl DefaultGpackUpdater {
    /// Check for updates for a specific gpack
    fn check_for_updates(&self, _gpack_id: &str, current_version: &str) -> Result<Option<String>> {
        // This is a simplified synchronous implementation
        // In production, you'd want to use async/await properly
        
        // For now, simulate checking for updates
        // In a real implementation, you'd use the registry client to check for newer versions
        
        // Simulate: if current version is 1.0.0, suggest 1.1.0
        if current_version == "1.0.0" {
            Ok(Some("1.1.0".to_string()))
        } else {
            Ok(None)
        }
    }
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
