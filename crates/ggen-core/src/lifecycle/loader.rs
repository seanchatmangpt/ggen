//! Configuration loader for make.toml lifecycle files
//!
//! This module provides functionality for loading and validating `make.toml` configuration
//! files that define project lifecycle phases, hooks, and workspace structure. It includes
//! poka-yoke validation to prevent invalid configurations from being loaded.
//!
//! ## Features
//!
//! - **Configuration loading**: Load `make.toml` from file paths
//! - **Default fallback**: Provide default configuration when file doesn't exist
//! - **Poka-yoke validation**: Validate phases have commands and hooks are valid
//! - **Error handling**: Clear error messages for configuration issues
//!
//! ## Configuration Structure
//!
//! The `make.toml` file defines:
//! - **Project**: Project metadata (name, type, version, description)
//! - **Workspace**: Workspace structure for monorepos (optional)
//! - **Lifecycle**: Phase definitions with commands
//! - **Hooks**: Before/after hooks for phases (optional)
//!
//! ## Examples
//!
//! ### Loading Configuration
//!
//! ```rust,no_run
//! use ggen_core::lifecycle::loader::load_make;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let make = load_make("make.toml")?;
//! println!("Project: {}", make.project.name);
//! # Ok(())
//! # }
//! ```
//!
//! ### Loading with Default Fallback
//!
//! ```rust,no_run
//! use ggen_core::lifecycle::loader::load_make_or_default;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! // Returns default config if make.toml doesn't exist
//! let make = load_make_or_default(".")?;
//! # Ok(())
//! # }
//! ```

use super::error::{LifecycleError, Result};
use super::hooks::validate_hooks;
use super::model::Make;
use std::path::Path;

/// Load make.toml from path
pub fn load_make<P: AsRef<Path>>(path: P) -> Result<Make> {
    let path_ref = path.as_ref();
    let content =
        std::fs::read_to_string(path_ref).map_err(|e| LifecycleError::config_load(path_ref, e))?;

    let make: Make =
        toml::from_str(&content).map_err(|e| LifecycleError::config_parse(path_ref, e))?;

    // Validate phases have commands (poka-yoke)
    validate_phases(&make)?;

    // Validate hooks (poka-yoke)
    if make.hooks.is_some() {
        validate_hooks(&make)?;
    }

    Ok(make)
}

/// Validate that all phases have at least one command
///
/// **Poka-yoke**: Prevents phases with no commands from being loaded.
fn validate_phases(make: &Make) -> Result<()> {
    for (phase_name, phase) in &make.lifecycle {
        if phase.commands().is_empty() {
            return Err(LifecycleError::NoCommands {
                phase: phase_name.clone(),
            });
        }
    }
    Ok(())
}

/// Load make.toml from project root, with fallback to default
pub fn load_make_or_default<P: AsRef<Path>>(root: P) -> Result<Make> {
    let make_path = root.as_ref().join("make.toml");

    if make_path.exists() {
        load_make(make_path)
    } else {
        // Return minimal default configuration
        Ok(Make {
            project: super::model::Project {
                name: crate::lifecycle::model::defaults::DEFAULT_PROJECT_NAME.to_string(),
                project_type: None,
                version: Some(
                    crate::lifecycle::model::defaults::DEFAULT_PROJECT_VERSION.to_string(),
                ),
                description: None,
            },
            workspace: None,
            lifecycle: Default::default(),
            hooks: None,
        })
    }
}

// Unit tests removed - covered by integration_test.rs:
// - test_load_make_toml (loads and validates make.toml)
// - All tests use LifecycleTestFixture which exercises load_make()
// This provides better coverage with real file I/O
