use clap::{CommandFactory, Parser};
use std::path::PathBuf;

use rgen_utils::app_config::AppConfig;
use rgen_utils::error::Result;
use rgen_utils::types::LogLevel;

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

pub async fn cli_match() -> Result<()> {
    let cli = Cli::parse();

    AppConfig::merge_config(cli.config.as_deref())?;
    let app = Cli::command();
    let matches = app.get_matches();
    AppConfig::merge_args(matches)?;

    // For now, skip rgen.toml loading until we implement the methods
    cli.command.run().await
}
