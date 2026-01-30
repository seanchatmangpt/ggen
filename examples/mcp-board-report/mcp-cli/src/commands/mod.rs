//! Command implementations for MCP+ CLI

pub mod bundle;
pub mod drill;
pub mod epoch;
pub mod kill;
pub mod status;

use std::path::PathBuf;

/// Shared context for all commands
pub struct Context {
    /// Base directory for all operations (retained for extensibility)
    #[allow(dead_code)]
    pub base_dir: PathBuf,
    /// State storage directory
    pub state_dir: PathBuf,
    /// Evidence bundle directory
    pub evidence_dir: PathBuf,
    /// Logs directory (retained for extensibility)
    #[allow(dead_code)]
    pub logs_dir: PathBuf,
}
