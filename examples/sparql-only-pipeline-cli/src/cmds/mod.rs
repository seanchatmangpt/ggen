// Command discovery module
// Exports all noun modules for auto-discovery

pub mod pipeline;

pub fn run_cli() -> anyhow::Result<()> {
    clap_noun_verb::run()
}
