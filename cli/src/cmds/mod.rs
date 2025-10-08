pub mod hazard;
pub mod error;
pub mod config;
pub mod completion;

pub mod gen;
pub mod list;
pub mod show;
pub mod validate;
pub mod graph;

use clap::Subcommand;

#[derive(Subcommand, Debug)]
pub enum Commands {
    #[command(name = "gen", about = "Render templates into outputs")]
    Gen {
        scope: String,
        action: String,
        #[arg(short, long, value_name = "k=v")]
        vars: Vec<String>,
        #[arg(long)]
        dry_run: bool,
    },

    #[command(name = "list", about = "List available scopes/actions")]
    List,

    #[command(name = "show", about = "Show frontmatter and resolved context")]
    Show {
        scope: String,
        action: String,
        #[arg(short, long, value_name = "k=v")]
        vars: Vec<String>,
    },

    #[command(name = "validate", about = "Validate RDF/SHACL for an action")]
    Validate {
        scope: String,
        action: String,
        #[arg(short, long, value_name = "k=v")]
        vars: Vec<String>,
    },

    #[command(name = "graph", about = "Graph utilities")]
    Graph {
        #[command(subcommand)]
        subcommand: GraphSubcommand,
    },

    #[command(name = "hazard", about = "Generate a hazardous occurance")]
    Hazard,

    #[command(name = "error", about = "Simulate an error")]
    Error,

    #[command(name = "completion", about = "Generate completion scripts")]
    Completion {
        #[command(subcommand)]
        subcommand: CompletionSubcommand,
    },

    #[command(name = "config", about = "Show Configuration")]
    Config,
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

#[derive(Subcommand, PartialEq, Debug)]
pub enum GraphSubcommand {
    #[command(name = "export", about = "Export merged data graph")]
    Export {
        scope: String,
        action: String,
        #[arg(short, long, value_name = "k=v")]
        vars: Vec<String>,
        #[arg(long, value_parser = ["ttl","jsonld"], default_value = "ttl")]
        fmt: String,
    },
}

impl Commands {
    pub fn run(&self) -> utils::error::Result<()> {
        match self {
            Commands::Gen { scope, action, vars, dry_run } =>
                gen::run(scope, action, vars, *dry_run),
            Commands::List =>
                list::run(),
            Commands::Show { scope, action, vars } =>
                show::run(scope, action, vars),
            Commands::Validate { scope, action, vars } =>
                validate::run(scope, action, vars),
            Commands::Graph { subcommand } =>
                graph::run(subcommand),
            Commands::Hazard => hazard::run(),
            Commands::Error => error::run(),
            Commands::Completion { subcommand } => completion::run(subcommand),
            Commands::Config => config::run(),
        }
    }
}
