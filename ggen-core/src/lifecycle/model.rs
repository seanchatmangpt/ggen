//! Data model for make.toml lifecycle configuration

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

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
