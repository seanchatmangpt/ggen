// New noun-verb structure
pub mod ai;
pub mod audit;
pub mod ci;
pub mod graph;
pub mod hook;
pub mod market;
pub mod project;
pub mod shell;
pub mod template;

// Legacy flat commands (to be migrated)
pub mod add;
pub mod categories;
// pub mod completion; // COMMENTED OUT: Command line completion code
pub mod gen;
pub mod github;
pub mod hazard;
pub mod lint;
pub mod list;
pub mod packs;
pub mod remove;
pub mod search;
pub mod show;
pub mod update;

use clap::Subcommand;
use ggen_utils::UtilsGgenConfig as GgenConfig;

#[derive(Subcommand, Debug)]
pub enum Commands {
    // New noun-verb structure
        #[command(name = "ai", about = "AI-powered template generation and analysis")]
        Ai(ai::AiArgs),
    #[command(name = "audit", about = "Security and performance auditing")]
    Audit(audit::AuditCmd),
    #[command(name = "ci", about = "CI/CD operations and GitHub integration")]
    Ci(ci::CiCmd),
    #[command(name = "graph", about = "RDF graph operations")]
    Graph(graph::GraphCmd),
    #[command(
        name = "hook",
        about = "Knowledge hooks for autonomic graph regeneration"
    )]
    Hook(hook::HookCmd),
    #[command(name = "market", about = "Marketplace operations for gpacks")]
    Market(market::MarketCmd),
    #[command(name = "project", about = "Project scaffolding and generation")]
    Project(project::ProjectCmd),
    #[command(name = "shell", about = "Shell integration and completion")]
    Shell(shell::ShellCmd),
    #[command(name = "template", about = "Template management")]
    Template(template::TemplateCmd),

    // Legacy flat commands (deprecated, use nouns instead)
    #[command(
        name = "search",
        about = "Search for gpacks in registry (deprecated: use 'ggen market search')"
    )]
    Search(search::SearchArgs),
    #[command(
        name = "categories",
        about = "Show popular categories and keywords (deprecated: use 'ggen market categories')"
    )]
    Categories(categories::CategoriesArgs),
    #[command(
        name = "add",
        about = "Add an gpack to the project (deprecated: use 'ggen market add')"
    )]
    Add(add::AddArgs),
    #[command(
        name = "remove",
        about = "Remove an gpack from the project (deprecated: use 'ggen market remove')"
    )]
    Remove(remove::RemoveArgs),
    #[command(
        name = "packs",
        about = "List installed gpacks (deprecated: use 'ggen market list')"
    )]
    Packs,
    #[command(
        name = "update",
        about = "Update gpacks to latest compatible versions (deprecated: use 'ggen market update')"
    )]
    Update(update::UpdateArgs),
    #[command(
        name = "gen",
        about = "Generate code from templates (deprecated: use 'ggen project gen')"
    )]
    Gen(gen::GenArgs),
    #[command(
        name = "list",
        about = "List available templates (deprecated: use 'ggen template list')"
    )]
    List,
    #[command(
        name = "show",
        about = "Show template metadata (deprecated: use 'ggen template show')"
    )]
    Show(show::ShowArgs),
    #[command(
        name = "lint",
        about = "Lint template with schema validation (deprecated: use 'ggen template lint')"
    )]
    Lint(lint::LintArgs),
    #[command(name = "github", about = "GitHub API operations (Pages, workflows)")]
    GitHub(github::GitHubArgs),
    #[command(name = "hazard", about = "Generate hazard report")]
    Hazard,
    // #[command(name = "completion", about = "Generate completion scripts")] // COMMENTED OUT: Command line completion code
    // Completion {
    //     #[command(subcommand)]
    //     subcommand: CompletionSubcommand,
    // },
}

// #[derive(Subcommand, PartialEq, Debug)] // COMMENTED OUT: Command line completion code
// pub enum CompletionSubcommand {
//     #[command(about = "generate the autocompletion script for bash")]
//     Bash,
//     #[command(about = "generate the autocompletion script for zsh")]
//     Zsh,
//     #[command(about = "generate the autocompletion script for fish")]
//     Fish,
// }

impl Commands {
    pub async fn run(&self) -> ggen_utils::error::Result<()> {
        match self {
            // New noun-verb structure
            Commands::Ai(args) => ai::run(args).await,
            Commands::Audit(cmd) => cmd.run().await,
            Commands::Ci(cmd) => cmd.run().await,
            Commands::Graph(cmd) => cmd.run().await,
            Commands::Hook(cmd) => cmd.run().await,
            Commands::Market(cmd) => cmd.run().await,
            Commands::Project(cmd) => cmd.run().await,
            Commands::Shell(cmd) => cmd.run().await,
            Commands::Template(cmd) => cmd.run().await,

            // Legacy flat commands
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
            Commands::GitHub(args) => Ok(github::run(args).await?),
            Commands::Hazard => hazard::run(),
            // Commands::Completion { subcommand } => completion::run(subcommand), // COMMENTED OUT: Command line completion code
        }
    }

    pub async fn run_with_config(
        &self, _ggen_config: Option<GgenConfig>,
    ) -> ggen_utils::error::Result<()> {
        match self {
            // New noun-verb structure
            Commands::Ai(args) => ai::run(args).await,
            Commands::Audit(cmd) => cmd.run().await,
            Commands::Ci(cmd) => cmd.run().await,
            Commands::Graph(cmd) => cmd.run().await,
            Commands::Hook(cmd) => cmd.run().await,
            Commands::Market(cmd) => cmd.run().await,
            Commands::Project(cmd) => cmd.run().await,
            Commands::Shell(cmd) => cmd.run().await,
            Commands::Template(cmd) => cmd.run().await,

            // Legacy flat commands
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
            Commands::GitHub(args) => Ok(github::run(args).await?),
            Commands::Hazard => hazard::run(),
            // Commands::Completion { subcommand } => completion::run(subcommand), // COMMENTED OUT: Command line completion code
        }
    }
}
