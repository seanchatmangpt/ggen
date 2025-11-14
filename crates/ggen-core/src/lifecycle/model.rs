//! Data model for make.toml lifecycle configuration
//!
//! This module defines the data structures used to represent lifecycle configuration
//! from `make.toml` files. It includes project metadata, phase definitions, workspace
//! structure, and hook configurations.
//!
//! ## Features
//!
//! - **Project metadata**: Name, type, version, description
//! - **Phase definitions**: Commands and configuration for each lifecycle phase
//! - **Workspace support**: Monorepo workspace structure
//! - **Hook system**: Before/after hooks for phases
//! - **Default values**: Sensible defaults for optional fields
//!
//! ## Configuration Structure
//!
//! ### Project
//! - `name`: Project name (required)
//! - `type`: Project type (optional)
//! - `version`: Project version (optional, defaults to "0.1.0")
//! - `description`: Project description (optional)
//!
//! ### Lifecycle Phases
//! Each phase defines:
//! - `commands`: List of commands to execute
//! - `env`: Environment variables (optional)
//! - `inputs`: Input files for caching (optional)
//!
//! ### Hooks
//! Hooks define before/after relationships:
//! - `before_<phase>`: Phases that must run before this phase
//! - `after_<phase>`: Phases that must run after this phase
//!
//! ## Examples
//!
//! ### Example make.toml
//!
//! ```toml
//! [project]
//! name = "my-project"
//! version = "1.0.0"
//!
//! [lifecycle.build]
//! commands = ["cargo build --release"]
//!
//! [lifecycle.test]
//! commands = ["cargo test"]
//!
//! [hooks]
//! before_build = ["test"]
//! ```

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// Default configuration constants
///
/// **Kaizen improvement**: Extracted magic strings to named constants for maintainability.
pub mod defaults {
    /// Default project name when not specified
    pub const DEFAULT_PROJECT_NAME: &str = "unnamed";

    /// Default project version when not specified
    pub const DEFAULT_PROJECT_VERSION: &str = "0.1.0";

    /// Default project name for readiness reports
    pub const DEFAULT_READINESS_PROJECT_NAME: &str = "Current Project";
}

/// Root configuration from make.toml
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Make {
    pub project: Project,
    #[serde(default)]
    pub workspace: Option<BTreeMap<String, Workspace>>,
    #[serde(default)]
    pub lifecycle: BTreeMap<String, Phase>,
    #[serde(default)]
    pub hooks: Option<Hooks>,
}

/// Project metadata
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Project {
    pub name: String,
    #[serde(rename = "type")]
    pub project_type: Option<String>,
    #[serde(default)]
    pub version: Option<String>,
    #[serde(default)]
    pub description: Option<String>,
}

/// Workspace definition for monorepos
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Workspace {
    pub path: String,
    #[serde(default)]
    pub framework: Option<String>,
    #[serde(default)]
    pub runtime: Option<String>,
    #[serde(default)]
    pub package_manager: Option<String>,
}

/// Lifecycle phase definition
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Phase {
    #[serde(default)]
    pub description: Option<String>,

    // Single or multiple commands
    #[serde(default)]
    pub command: Option<String>,
    #[serde(default)]
    pub commands: Option<Vec<String>>,

    // Execution metadata
    #[serde(default)]
    pub watch: Option<bool>,
    #[serde(default)]
    pub port: Option<u16>,

    // Output tracking for caching
    #[serde(default)]
    pub outputs: Option<Vec<String>>,
    #[serde(default)]
    pub cache: Option<bool>,

    // Workspace control
    #[serde(default)]
    pub workspaces: Option<Vec<String>>,
    #[serde(default)]
    pub parallel: Option<bool>,
}

/// Hook definitions for lifecycle phases
#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct Hooks {
    // Global hooks
    #[serde(default)]
    pub before_all: Option<Vec<String>>,
    #[serde(default)]
    pub after_all: Option<Vec<String>>,

    // Phase-specific hooks
    #[serde(default)]
    pub before_init: Option<Vec<String>>,
    #[serde(default)]
    pub after_init: Option<Vec<String>>,

    #[serde(default)]
    pub before_setup: Option<Vec<String>>,
    #[serde(default)]
    pub after_setup: Option<Vec<String>>,

    #[serde(default)]
    pub before_build: Option<Vec<String>>,
    #[serde(default)]
    pub after_build: Option<Vec<String>>,

    #[serde(default)]
    pub before_test: Option<Vec<String>>,
    #[serde(default)]
    pub after_test: Option<Vec<String>>,

    #[serde(default)]
    pub before_deploy: Option<Vec<String>>,
    #[serde(default)]
    pub after_deploy: Option<Vec<String>>,
    // Future: Error handling hooks (80/20 - not implemented yet, fail fast for now)
    // #[serde(default)]
    // pub on_error: Option<String>,
    // #[serde(default)]
    // pub on_success: Option<String>,
}

impl Phase {
    /// Get commands for this phase (eliminates duplication)
    pub fn commands(&self) -> Vec<String> {
        if let Some(cmd) = &self.command {
            vec![cmd.clone()]
        } else if let Some(cmds) = &self.commands {
            cmds.clone()
        } else {
            vec![]
        }
    }
}

/// Builder for creating validated phases
///
/// **Poka-yoke**: Ensures phases always have at least one command before they can be used.
/// This prevents runtime errors from phases with no commands.
///
/// # Example
///
/// ```rust
/// use ggen_core::lifecycle::model::PhaseBuilder;
///
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let phase = PhaseBuilder::new("build")
///     .command("cargo build --release")
///     .build()?;
/// # Ok(())
/// # }
/// ```
pub struct PhaseBuilder {
    name: String,
    description: Option<String>,
    commands: Vec<String>,
    watch: Option<bool>,
    port: Option<u16>,
    outputs: Option<Vec<String>>,
    cache: Option<bool>,
    workspaces: Option<Vec<String>>,
    parallel: Option<bool>,
}

impl PhaseBuilder {
    /// Create a new phase builder with the given name
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            description: None,
            commands: Vec::new(),
            watch: None,
            port: None,
            outputs: None,
            cache: None,
            workspaces: None,
            parallel: None,
        }
    }

    /// Set the phase description
    pub fn description(mut self, description: impl Into<String>) -> Self {
        self.description = Some(description.into());
        self
    }

    /// Add a single command (replaces any existing commands)
    pub fn command(mut self, cmd: impl Into<String>) -> Self {
        self.commands = vec![cmd.into()];
        self
    }

    /// Add multiple commands
    pub fn commands(mut self, cmds: Vec<String>) -> Self {
        self.commands = cmds;
        self
    }

    /// Add a command to the existing list
    pub fn add_command(mut self, cmd: impl Into<String>) -> Self {
        self.commands.push(cmd.into());
        self
    }

    /// Set watch mode
    pub fn watch(mut self, watch: bool) -> Self {
        self.watch = Some(watch);
        self
    }

    /// Set port
    pub fn port(mut self, port: u16) -> Self {
        self.port = Some(port);
        self
    }

    /// Set outputs
    pub fn outputs(mut self, outputs: Vec<String>) -> Self {
        self.outputs = Some(outputs);
        self
    }

    /// Set cache flag
    pub fn cache(mut self, cache: bool) -> Self {
        self.cache = Some(cache);
        self
    }

    /// Set workspaces
    pub fn workspaces(mut self, workspaces: Vec<String>) -> Self {
        self.workspaces = Some(workspaces);
        self
    }

    /// Set parallel flag
    pub fn parallel(mut self, parallel: bool) -> Self {
        self.parallel = Some(parallel);
        self
    }

    /// Build a validated phase
    ///
    /// **Poka-yoke**: Returns error if no commands are provided.
    /// This ensures phases always have at least one command.
    pub fn build(self) -> Result<ValidatedPhase, super::error::LifecycleError> {
        if self.commands.is_empty() {
            return Err(super::error::LifecycleError::NoCommands {
                phase: self.name.clone(),
            });
        }

        // Convert commands to Phase format (single command or multiple)
        let (command, commands) = if self.commands.len() == 1 {
            (Some(self.commands[0].clone()), None)
        } else {
            (None, Some(self.commands))
        };

        Ok(ValidatedPhase {
            phase: Phase {
                description: self.description,
                command,
                commands,
                watch: self.watch,
                port: self.port,
                outputs: self.outputs,
                cache: self.cache,
                workspaces: self.workspaces,
                parallel: self.parallel,
            },
        })
    }
}

/// Validated phase that is guaranteed to have at least one command
///
/// **Poka-yoke**: This type can only be created through `PhaseBuilder::build()`,
/// which ensures at least one command is present. This prevents runtime errors
/// from phases with no commands.
#[derive(Debug, Clone)]
pub struct ValidatedPhase {
    phase: Phase,
}

impl ValidatedPhase {
    /// Get the underlying phase
    pub fn phase(&self) -> &Phase {
        &self.phase
    }

    /// Get commands (guaranteed to be non-empty)
    pub fn commands(&self) -> Vec<String> {
        self.phase.commands()
    }

    /// Get phase name (requires storing it separately or extracting from context)
    /// For now, this is a limitation - phases don't store their own name
    pub fn name(&self) -> Option<&str> {
        None // Phase doesn't store name, it's the key in the map
    }
}

impl AsRef<Phase> for ValidatedPhase {
    fn as_ref(&self) -> &Phase {
        &self.phase
    }
}

impl Make {
    /// Get list of all defined lifecycle phases
    pub fn phase_names(&self) -> Vec<String> {
        self.lifecycle.keys().cloned().collect()
    }

    /// Get commands for a phase
    pub fn phase_commands(&self, phase_name: &str) -> Vec<String> {
        self.lifecycle
            .get(phase_name)
            .map_or(vec![], |p| p.commands())
    }
}
