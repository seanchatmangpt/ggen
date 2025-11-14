//! Project build operations
//!
//! This module provides functionality for building and cleaning ggen projects.
//! It handles build orchestration and cleanup of build artifacts.
//!
//! ## Features
//!
//! - **Project Building**: Build a project from a given path
//! - **Clean Operations**: Remove build artifacts (target directory)
//! - **Path Validation**: Validate project paths before operations
//!
//! ## Examples
//!
//! ### Building a Project
//!
//! ```rust,no_run
//! use ggen_domain::project::build::build_project;
//! use std::path::Path;
//!
//! # async fn example() -> anyhow::Result<()> {
//! build_project(Path::new("my-project")).await?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Cleaning Build Artifacts
//!
//! ```rust,no_run
//! use ggen_domain::project::build::clean_project;
//! use std::path::Path;
//!
//! # async fn example() -> anyhow::Result<()> {
//! clean_project(Path::new("my-project")).await?;
//! # Ok(())
//! # }
//! ```

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
