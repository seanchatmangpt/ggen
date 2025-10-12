// Noun-verb command structure
pub mod ai;
pub mod audit;
pub mod ci;
pub mod graph;
pub mod hook;
pub mod lifecycle;
pub mod market;
pub mod project;
pub mod shell;
pub mod template;

use clap::Subcommand;
use ggen_utils::UtilsGgenConfig as GgenConfig;

#[derive(Subcommand, Debug)]
pub enum Commands {
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

    #[command(name = "lifecycle", about = "Universal lifecycle management")]
    Lifecycle(lifecycle::LifecycleArgs),

    #[command(name = "market", about = "Marketplace operations for gpacks")]
    Market(market::MarketCmd),

    #[command(name = "project", about = "Project scaffolding and generation")]
    Project(project::ProjectCmd),

    #[command(name = "shell", about = "Shell integration and completion")]
    Shell(shell::ShellCmd),

    #[command(name = "template", about = "Template management")]
    Template(template::TemplateCmd),
}

impl Commands {
    pub async fn run(&self) -> ggen_utils::error::Result<()> {
        match self {
            Commands::Ai(args) => ai::run(args).await,
            Commands::Audit(cmd) => cmd.run().await,
            Commands::Ci(cmd) => cmd.run().await,
            Commands::Graph(cmd) => cmd.run().await,
            Commands::Hook(cmd) => cmd.run().await,
            Commands::Lifecycle(args) => lifecycle::run(args.clone()).await,
            Commands::Market(cmd) => cmd.run().await,
            Commands::Project(cmd) => cmd.run().await,
            Commands::Shell(cmd) => cmd.run().await,
            Commands::Template(cmd) => cmd.run().await,
        }
    }

    pub async fn run_with_config(
        &self, _ggen_config: Option<GgenConfig>,
    ) -> ggen_utils::error::Result<()> {
        match self {
            Commands::Ai(args) => ai::run(args).await,
            Commands::Audit(cmd) => cmd.run().await,
            Commands::Ci(cmd) => cmd.run().await,
            Commands::Graph(cmd) => cmd.run().await,
            Commands::Hook(cmd) => cmd.run().await,
            Commands::Lifecycle(args) => lifecycle::run(args.clone()).await,
            Commands::Market(cmd) => cmd.run().await,
            Commands::Project(cmd) => cmd.run().await,
            Commands::Shell(cmd) => cmd.run().await,
            Commands::Template(cmd) => cmd.run().await,
        }
    }
}
