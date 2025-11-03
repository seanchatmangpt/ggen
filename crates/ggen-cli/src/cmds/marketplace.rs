//! Marketplace commands - clap-noun-verb auto-discovery
use clap::{Args, Subcommand};
use ggen_utils::error::Result;

use ggen_domain::marketplace;

/// Marketplace command arguments
#[derive(Debug, Args)]
pub struct MarketplaceArgs {
    #[command(subcommand)]
    pub command: MarketplaceCmd,
}

#[derive(Debug, Subcommand)]
pub enum MarketplaceCmd {
    /// Search for packages
    Search(marketplace::SearchArgs),
    /// Install a package
    Install(marketplace::InstallArgs),
    /// List installed packages
    List(marketplace::ListArgs),
    /// Publish a package
    Publish(marketplace::PublishArgs),
    /// Update packages
    Update(marketplace::UpdateArgs),
    /// P2P network operations
    P2p(marketplace::P2PArgs),
}

impl MarketplaceArgs {
    pub fn execute(&self) -> Result<()> {
        match &self.command {
            MarketplaceCmd::Search(args) => marketplace::search::run(args),
            MarketplaceCmd::Install(args) => marketplace::install::run(args),
            MarketplaceCmd::List(args) => marketplace::list::run(args),
            MarketplaceCmd::Publish(args) => marketplace::publish::run(args),
            MarketplaceCmd::Update(args) => marketplace::update::run(args),
            MarketplaceCmd::P2p(args) => {
                // P2P commands are async, so we need to use the runtime
                crate::runtime::execute(marketplace::execute_p2p_command(args.command.clone()))
            }
        }
    }
}
