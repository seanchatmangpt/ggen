pub mod commands;
pub mod error;

use clap::{Parser, Subcommand};

pub use error::{CliError, Result};

#[derive(Debug, Parser)]
#[clap(name = "ggen-tps")]
#[clap(about = "A2A/TPS operations for Toyota Production System patterns", long_about = None)]
pub struct Cli {
    #[clap(subcommand)]
    pub command: Commands,
}

#[derive(Debug, Subcommand)]
pub enum Commands {
    /// Receipt operations (verify, chain, audit)
    #[clap(subcommand)]
    Receipt(commands::receipt::ReceiptCommands),

    /// Jidoka operations (andon signals, stop the line)
    #[clap(subcommand)]
    Jidoka(commands::jidoka::JidokaCommands),

    /// Packet operations (work order validation, routing)
    #[clap(subcommand)]
    Packet(commands::packet::PacketCommands),

    /// Backpressure operations (admission control, token pool)
    #[clap(subcommand)]
    Backpressure(commands::backpressure::BackpressureCommands),

    /// Supplier operations (quality scoring, rate limits)
    #[clap(subcommand)]
    Supplier(commands::supplier::SupplierCommands),

    /// A2A task state machine operations
    #[clap(subcommand)]
    A2a(commands::a2a::A2aCommands),

    /// Firewall operations (ingress control)
    #[clap(subcommand)]
    Firewall(commands::firewall::FirewallCommands),
}

impl Cli {
    pub async fn run(self) -> Result<()> {
        match self.command {
            Commands::Receipt(cmd) => cmd.execute().await,
            Commands::Jidoka(cmd) => cmd.execute().await,
            Commands::Packet(cmd) => cmd.execute().await,
            Commands::Backpressure(cmd) => cmd.execute().await,
            Commands::Supplier(cmd) => cmd.execute().await,
            Commands::A2a(cmd) => cmd.execute().await,
            Commands::Firewall(cmd) => cmd.execute().await,
        }
    }
}
