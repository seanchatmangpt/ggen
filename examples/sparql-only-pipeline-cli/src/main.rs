// Main entry point for the SPARQL-only Pipeline CLI
// Demonstrates clap-noun-verb pattern with 100% domain logic in SPARQL

use anyhow::Result;

#[tokio::main]
async fn main() -> Result<()> {
    sparql_only_pipeline_cli::cli_match().await
}
