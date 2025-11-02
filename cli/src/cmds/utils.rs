//! Utility commands - clap-noun-verb auto-discovery
use clap::Subcommand;
use ggen_utils::error::Result;

use crate::domain::utils;

#[derive(Debug, Subcommand)]
pub enum UtilsCmd {
    /// Run system diagnostics
    Doctor(utils::doctor::DoctorArgs),
    /// Manage environment variables
    Env(utils::env::EnvArgs),
}

impl UtilsCmd {
    pub fn execute(&self) -> Result<()> {
        match self {
            Self::Doctor(args) => utils::doctor::run(args),
            Self::Env(args) => utils::env::run(args),
        }
    }
}
