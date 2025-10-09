pub mod add;
pub mod categories;
pub mod completion;
pub mod gen;
pub mod graph;
pub mod hazard;
pub mod lint;
pub mod list;
pub mod packs;
pub mod remove;
pub mod search;
pub mod show;
pub mod update;

use clap::Subcommand;
use rgen_utils::project_config::RgenConfig;

#[derive(Subcommand, Debug)]
pub enum Commands {
    #[command(name = "search", about = "Search for rpacks in registry")]
    Search(search::SearchArgs),
    #[command(name = "categories", about = "Show popular categories and keywords")]
    Categories(categories::CategoriesArgs),
    #[command(name = "add", about = "Add an rpack to the project")]
    Add(add::AddArgs),
    #[command(name = "remove", about = "Remove an rpack from the project")]
    Remove(remove::RemoveArgs),
    #[command(name = "packs", about = "List installed rpacks")]
    Packs,
    #[command(name = "update", about = "Update rpacks to latest compatible versions")]
    Update(update::UpdateArgs),
    #[command(name = "gen", about = "Generate code from templates")]
    Gen(gen::GenArgs),
    #[command(name = "list", about = "List available templates")]
    List,
    #[command(name = "show", about = "Show template metadata")]
    Show(show::ShowArgs),
    #[command(name = "validate", about = "Validate template frontmatter")]
    #[command(name = "lint", about = "Lint template with schema validation")]
    Lint(lint::LintArgs),
    #[command(name = "graph", about = "Export RDF graph")]
    Graph(graph::GraphArgs),
    #[command(name = "hazard", about = "Generate hazard report")]
    Hazard,
    #[command(name = "completion", about = "Generate completion scripts")]
    Completion {
        #[command(subcommand)]
        subcommand: CompletionSubcommand,
    },
}

#[derive(Subcommand, PartialEq, Debug)]
pub enum CompletionSubcommand {
    #[command(about = "generate the autocompletion script for bash")]
    Bash,
    #[command(about = "generate the autocompletion script for zsh")]
    Zsh,
    #[command(about = "generate the autocompletion script for fish")]
    Fish,
}

impl Commands {
    pub async fn run(&self) -> utils::error::Result<()> {
        match self {
            Commands::Search(args) => Ok(search::run(args).await?),
            Commands::Categories(args) => Ok(categories::run(args).await?),
            Commands::Add(args) => Ok(add::run(args).await?),
            Commands::Remove(args) => Ok(remove::run(args)?),
            Commands::Packs => Ok(packs::run()?),
            Commands::Update(args) => Ok(update::run(args).await?),
            Commands::Gen(args) => Ok(gen::run(args)?),
            Commands::List => list::run(),
            Commands::Show(args) => show::run(args),
            Commands::Lint(args) => lint::run(args),
            Commands::Graph(args) => graph::run(args),
            Commands::Hazard => hazard::run(),
            Commands::Completion { subcommand } => completion::run(subcommand),
        }
    }

    pub async fn run_with_config(
        &self, _rgen_config: Option<RgenConfig>,
    ) -> utils::error::Result<()> {
        match self {
            Commands::Search(args) => Ok(search::run(args).await?),
            Commands::Categories(args) => Ok(categories::run(args).await?),
            Commands::Add(args) => Ok(add::run(args).await?),
            Commands::Remove(args) => Ok(remove::run(args)?),
            Commands::Packs => Ok(packs::run()?),
            Commands::Update(args) => Ok(update::run(args).await?),
            Commands::Gen(args) => Ok(gen::run(args)?),
            Commands::List => list::run(),
            Commands::Show(args) => show::run(args),
            Commands::Lint(args) => lint::run(args),
            Commands::Graph(args) => graph::run(args),
            Commands::Hazard => hazard::run(),
            Commands::Completion { subcommand } => completion::run(subcommand),
        }
    }
}
