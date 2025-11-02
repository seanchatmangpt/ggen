//! Marketplace commands - clap-noun-verb auto-discovery
use clap::Subcommand;
use ggen_utils::error::Result;

use crate::domain::marketplace;

#[derive(Debug, Subcommand)]
pub enum MarketplaceCmd {
    /// Install a package
    Install(marketplace::install::InstallArgs),
    /// List installed packages
    List(marketplace::list::ListArgs),
    /// Publish a package
    Publish(marketplace::publish::PublishArgs),
    /// Search for packages
    Search(marketplace::search::SearchArgs),
    /// Update packages
    Update(marketplace::update::UpdateArgs),
}

impl MarketplaceCmd {
    pub fn execute(&self) -> Result<()> {
        match self {
            Self::Install(args) => marketplace::install::run(args),
            Self::List(args) => marketplace::list::run(args),
            Self::Publish(args) => marketplace::publish::run(args),
            Self::Search(args) => marketplace::search::run(args),
            Self::Update(args) => marketplace::update::run(args),
        }
    }
}
