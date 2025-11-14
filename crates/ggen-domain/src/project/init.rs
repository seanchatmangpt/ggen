//! Project initialization operations
//!
//! This module provides functionality for initializing new ggen projects and
//! validating existing project structures.
//!
//! ## Features
//!
//! - **Project Initialization**: Create new project structure with directories
//! - **Project Validation**: Check if a path is a valid project
//! - **Directory Creation**: Set up standard project directories (src, tests)
//!
//! ## Examples
//!
//! ### Initializing a New Project
//!
//! ```rust,no_run
//! use ggen_domain::project::init::init_project;
//! use std::path::Path;
//!
//! # async fn example() -> anyhow::Result<()> {
//! init_project(Path::new("my-new-project"), "my-new-project").await?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Validating a Project
//!
//! ```rust,no_run
//! use ggen_domain::project::init::is_project;
//! use std::path::Path;
//!
//! if is_project(Path::new("my-project")) {
//!     println!("Valid project!");
//! }
//! ```

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
