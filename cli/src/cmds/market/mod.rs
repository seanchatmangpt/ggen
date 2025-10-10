use clap::{Args, Subcommand};
use ggen_utils::error::Result;

// Declare verb modules
pub mod add;
pub mod cache;
pub mod categories;
pub mod info;
pub mod list;
pub mod offline;
pub mod publish;
pub mod recommend;
pub mod remove;
pub mod search;
pub mod sync;
pub mod unpublish;
pub mod update;

#[derive(Args, Debug)]
pub struct MarketCmd {
    #[command(subcommand)]
    pub verb: Verb,
}

#[derive(Subcommand, Debug)]
pub enum Verb {
    /// Search for gpacks in the marketplace
    ///
    /// Examples:
    ///   ggen market search "rust cli"
    ///   ggen market search "web" --category api
    ///   ggen market search "database" --json
    Search(search::SearchArgs),

    /// Add a gpack from the marketplace to your project
    ///
    /// Examples:
    ///   ggen market add "rust-cli-template"
    ///   ggen market add "web-api@1.2.0"
    Add(add::AddArgs),

    /// Remove a gpack from your project
    ///
    /// Examples:
    ///   ggen market remove "rust-cli-template"
    Remove(remove::RemoveArgs),

    /// List all installed gpacks
    ///
    /// Examples:
    ///   ggen market list
    ///   ggen market list --json
    List(list::ListArgs),

    /// Update gpacks to their latest versions
    ///
    /// Examples:
    ///   ggen market update
    ///   ggen market update "rust-cli-template"
    Update(update::UpdateArgs),

    /// Show detailed information about a gpack
    ///
    /// Examples:
    ///   ggen market info "rust-cli-template"
    ///   ggen market info "web-api" --examples
    ///   ggen market info "web-api" --dependencies
    Info(info::InfoArgs),

    /// Get personalized package recommendations
    ///
    /// Examples:
    ///   ggen market recommend
    ///   ggen market recommend --based-on "rust-cli"
    ///   ggen market recommend --category "web" --limit 5
    Recommend(recommend::RecommendArgs),

    /// Browse marketplace offline using cached data
    ///
    /// Examples:
    ///   ggen market offline search "rust"
    ///   ggen market offline info "rust-cli"
    ///   ggen market offline categories
    Offline(offline::OfflineArgs),

    /// Manage marketplace cache (clear, stats, validate)
    ///
    /// Examples:
    ///   ggen market cache clear
    ///   ggen market cache stats
    ///   ggen market cache validate
    Cache(cache::CacheArgs),

    /// Synchronize with remote marketplace
    ///
    /// Examples:
    ///   ggen market sync
    ///   ggen market sync --category "rust"
    ///   ggen market sync --force
    Sync(sync::SyncArgs),

    /// Show popular categories and keywords
    ///
    /// Examples:
    ///   ggen market categories
    ///   ggen market categories --json
    Categories(categories::CategoriesArgs),

    /// Publish a gpack to the marketplace
    ///
    /// Examples:
    ///   ggen market publish
    ///   ggen market publish --tag beta
    ///   ggen market publish --dry-run
    Publish(publish::PublishArgs),

    /// Unpublish a gpack from the marketplace
    ///
    /// Examples:
    ///   ggen market unpublish "my-package@1.0.0"
    ///   ggen market unpublish "my-package" --force
    Unpublish(unpublish::UnpublishArgs),
}

impl MarketCmd {
    pub async fn run(&self) -> Result<()> {
        match &self.verb {
            Verb::Search(args) => search::run(args).await,
            Verb::Add(args) => add::run(args).await,
            Verb::Remove(args) => remove::run(args).await,
            Verb::List(args) => list::run(args).await,
            Verb::Update(args) => update::run(args).await,
            Verb::Info(args) => info::run(args).await,
            Verb::Recommend(args) => recommend::run(args).await,
            Verb::Offline(args) => offline::run(args).await,
            Verb::Cache(args) => cache::run(args).await,
            Verb::Sync(args) => sync::run(args).await,
            Verb::Categories(args) => categories::run(args).await,
            Verb::Publish(args) => publish::run(args).await,
            Verb::Unpublish(args) => unpublish::run(args).await,
        }
    }
}
