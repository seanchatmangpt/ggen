//! Command implementations for MCP+ CLI

pub mod bundle;
pub mod drill;
pub mod epoch;
pub mod kill;
pub mod status;

use std::path::PathBuf;

/// Shared context for all commands
pub struct Context {
    pub base_dir: PathBuf,
    pub state_dir: PathBuf,
    pub evidence_dir: PathBuf,
    pub logs_dir: PathBuf,
}
