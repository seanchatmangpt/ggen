use anyhow::Result;
use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(
    name = "cargo-project",
    version,
    about = "Production-grade Rust project scaffold",
    long_about = "A hexagonal-architecture scaffold with Axum HTTP, SQLite persistence,\n\
                  layered config, and MCP server — ready for production."
)]
struct Cli {
    /// Increase verbosity (use multiple times for more detail)
    #[arg(short, long, action = clap::ArgAction::Count, global = true)]
    verbose: u8,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Start the HTTP service
    Serve {
        /// Address to bind
        #[arg(short, long, default_value = "0.0.0.0:8080", env = "BIND_ADDR")]
        bind: String,
    },
    /// Run database migrations
    Migrate {
        /// Path to SQLite database
        #[arg(short, long, default_value = "app.db", env = "DATABASE_URL")]
        db: String,
    },
    /// Print active configuration
    Config,
    /// Start the MCP server
    Mcp {
        /// MCP transport (stdio | sse)
        #[arg(short, long, default_value = "stdio")]
        transport: String,
    },
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    cargo_project::init_tracing(cli.verbose);

    match cli.command {
        Commands::Serve { bind } => {
            tracing::info!(%bind, "starting HTTP service");
            println!("HTTP service bound to {bind}  (not yet wired; see crates/service/)");
        }
        Commands::Migrate { db } => {
            tracing::info!(%db, "running migrations");
            println!("Migrations applied to {db}  (not yet wired; see crates/sqlite/)");
        }
        Commands::Config => {
            let cfg = bp_config::AppConfig::from_env()?;
            println!("{cfg:#?}");
        }
        Commands::Mcp { transport } => {
            tracing::info!(%transport, "starting MCP server");
            println!("MCP server ({transport})  (not yet wired; see crates/mcp-server/)");
        }
    }

    Ok(())
}
