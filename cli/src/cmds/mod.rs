pub mod gen;
pub mod list;
pub mod show;
pub mod validate;
pub mod graph;
pub mod completion;
pub mod hazard;

use clap::Subcommand;
use utils::project_config::RgenConfig;

#[derive(Subcommand, Debug)]
pub enum Commands {
    #[command(name = "gen", about = "Generate code from templates")]
    Gen(gen::GenArgs),
    #[command(name = "list", about = "List available templates")]
    List,
    #[command(name = "show", about = "Show template metadata")]
    Show(show::ShowArgs),
    #[command(name = "validate", about = "Validate RDF/SHACL")]
    Validate(validate::ValidateArgs),
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
    pub fn run(&self) -> utils::error::Result<()> {
        match self {
            Commands::Gen(args) => Ok(gen::run(args)?),
            Commands::List => list::run(),
            Commands::Show(args) => show::run(args),
            Commands::Validate(args) => validate::run(args),
            Commands::Graph(args) => graph::run(args),
            Commands::Hazard => hazard::run(),
            Commands::Completion { subcommand } => completion::run(subcommand),
        }
    }

    pub fn run_with_config(&self, rgen_config: Option<RgenConfig>) -> utils::error::Result<()> {
        match self {
            Commands::Gen(args) => Ok(gen::run_with_config(args, rgen_config)?),
            Commands::List => list::run(),
            Commands::Show(args) => show::run(args),
            Commands::Validate(args) => validate::run(args),
            Commands::Graph(args) => graph::run(args),
            Commands::Hazard => hazard::run(),
            Commands::Completion { subcommand } => completion::run(subcommand),
        }
    }
}
