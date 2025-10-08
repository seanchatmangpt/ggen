use clap::{CommandFactory, Parser, Subcommand};
use std::path::PathBuf;

use utils::app_config::AppConfig;
use utils::error::Result;
use utils::project_config::RgenConfig;
use utils::types::LogLevel;

pub mod cmds;

#[derive(Parser, Debug)]
#[command(name = "rgen", author, about = "Graph-aware code generator", version)]
pub struct Cli {
    #[arg(short, long, value_name = "FILE")]
    pub config: Option<PathBuf>,

    #[arg(long, value_name = "PATH", help = "Path to rgen.toml manifest file")]
    pub manifest_path: Option<PathBuf>,

    #[arg(name = "debug", short, long = "debug", value_name = "DEBUG")]
    pub debug: Option<bool>,

    #[arg(
        name = "log_level",
        short,
        long = "log-level",
        value_name = "LOG_LEVEL"
    )]
    pub log_level: Option<LogLevel>,

    #[clap(subcommand)]
    pub command: cmds::Commands,
}

pub fn cli_match() -> Result<()> {
    let cli = Cli::parse();

    AppConfig::merge_config(cli.config.as_deref())?;
    let app = Cli::command();
    let matches = app.get_matches();
    AppConfig::merge_args(matches)?;

    // Load rgen.toml project manifest
    let rgen_config = if let Some(manifest_path) = &cli.manifest_path {
        Some(RgenConfig::load_from_path(manifest_path)?)
    } else {
        let cwd = std::env::current_dir()?;
        RgenConfig::find_and_load(&cwd)?
    };

    cli.command.run_with_config(rgen_config)
}
