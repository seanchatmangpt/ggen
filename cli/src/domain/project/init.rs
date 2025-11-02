use anyhow::Result;
use std::path::Path;

/// Initialize a new project
pub async fn init_project(path: &Path, name: &str) -> Result<()> {
    if name.is_empty() {
        anyhow::bail!("Project name cannot be empty");
    }

    // Create project directory
    std::fs::create_dir_all(path)?;

    // Create basic project structure
    std::fs::create_dir_all(path.join("src"))?;
    std::fs::create_dir_all(path.join("tests"))?;

    Ok(())
}

/// Check if path is a valid project
pub fn is_project(path: &Path) -> bool {
    path.exists() && path.is_dir()
}
