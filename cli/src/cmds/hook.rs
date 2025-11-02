//! Hook commands - clap-noun-verb auto-discovery
use clap::Subcommand;
use ggen_utils::error::Result;

use crate::domain::hook;

#[derive(Debug, Subcommand)]
pub enum HookCmd {
    /// Create a hook
    Create(hook::create::CreateArgs),
    /// List hooks
    List(hook::list::ListArgs),
    /// Remove a hook
    Remove(hook::remove::RemoveArgs),
    /// Monitor hooks
    Monitor(hook::monitor::MonitorArgs),
}

impl HookCmd {
    pub fn execute(&self) -> Result<()> {
        match self {
            Self::Create(args) => hook::create::run(args),
            Self::List(args) => hook::list::run(args),
            Self::Remove(args) => hook::remove::run(args),
            Self::Monitor(args) => hook::monitor::run(args),
        }
    }
}
