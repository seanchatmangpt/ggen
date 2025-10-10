use clap::{Args, Subcommand};
use ggen_utils::error::Result;

// Declare verb modules
pub mod add;
pub mod categories;
pub mod list;
pub mod remove;
pub mod search;
pub mod show;
pub mod update;

#[derive(Args, Debug)]
pub struct MarketCmd {
    #[command(subcommand)]
    pub verb: Verb,
}

#[derive(Subcommand, Debug)]
pub enum Verb {
    /// Search for gpacks in the marketplace
    Search(search::SearchArgs),

    /// Add a gpack from the marketplace to your project
    Add(add::AddArgs),

    /// Remove a gpack from your project
    Remove(remove::RemoveArgs),

    /// List all installed gpacks
    List(list::ListArgs),

    /// Update gpacks to their latest versions
    Update(update::UpdateArgs),

    /// Show detailed information about a gpack
    Show(show::ShowArgs),

    /// Show popular categories and keywords
    Categories(categories::CategoriesArgs),
}

impl MarketCmd {
    pub async fn run(&self) -> Result<()> {
        match &self.verb {
            Verb::Search(args) => search::run(args).await,
            Verb::Add(args) => add::run(args).await,
            Verb::Remove(args) => remove::run(args).await,
            Verb::List(args) => list::run(args).await,
            Verb::Update(args) => update::run(args).await,
            Verb::Show(args) => show::run(args).await,
            Verb::Categories(args) => categories::run(args).await,
        }
    }
}
