use anyhow::Result;
use std::path::Path;

/// Build a project
pub async fn build_project(path: &Path) -> Result<()> {
    if !path.exists() {
        anyhow::bail!("Project path does not exist: {}", path.display());
    }

    if !path.is_dir() {
        anyhow::bail!("Project path is not a directory: {}", path.display());
    }

    // Basic build orchestration - can be enhanced later
    Ok(())
}

/// Clean build artifacts
pub async fn clean_project(path: &Path) -> Result<()> {
    if !path.exists() {
        return Ok(());
    }

    let target_dir = path.join("target");
    if target_dir.exists() {
        std::fs::remove_dir_all(target_dir)?;
    }

    Ok(())
}
