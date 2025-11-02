//! Marketplace commands - CLI layer
//!
//! This module contains the CLI layer for all marketplace operations.
//! Follows clap-noun-verb v3.0.0 architecture with thin CLI wrappers.

pub mod search;
pub mod install;
pub mod publish;
pub mod list;
pub mod update;

use clap::Subcommand;
use ggen_utils::error::Result;

/// Marketplace noun - groups all marketplace verbs
#[derive(Subcommand, Debug)]
pub enum MarketplaceVerb {
    /// Search for packages in the marketplace
    Search(search::SearchArgs),

    /// Install a package from the marketplace
    Install(install::InstallArgs),

    /// Publish a package to the marketplace
    Publish(publish::PublishArgs),

    /// List installed marketplace packages
    List(list::ListArgs),

    /// Update marketplace packages
    Update(update::UpdateArgs),
}

impl MarketplaceVerb {
    /// Execute the marketplace verb
    pub fn run(&self) -> Result<()> {
        match self {
            Self::Search(args) => search::run(args),
            Self::Install(args) => install::run(args),
            Self::Publish(args) => publish::run(args),
            Self::List(args) => list::run(args),
            Self::Update(args) => update::run(args),
        }
    }
}
