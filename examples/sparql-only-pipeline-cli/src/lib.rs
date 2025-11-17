// Library root for SPARQL-only CLI
// All domain logic is in sparql/operations.sparql
// This library is pure CLI infrastructure

pub mod cmds;

pub async fn cli_match() -> anyhow::Result<()> {
    // Auto-discover all #[verb] decorated functions
    clap_noun_verb::run().await
}
