//! Load make.toml configuration

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
                name: "unnamed".to_string(),
                project_type: None,
                version: Some("0.1.0".to_string()),
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
