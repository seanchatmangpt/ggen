use super::CompletionSubcommand;
use clap::CommandFactory;
use clap_complete::{
    generate,
    shells::{Bash, Fish, Zsh},
};

pub fn run(sub: &CompletionSubcommand) -> ggen_utils::error::Result<()> {
    let mut app = crate::Cli::command();
    match sub {
        CompletionSubcommand::Bash => generate(Bash, &mut app, "ggen", &mut std::io::stdout()),
        CompletionSubcommand::Zsh => generate(Zsh, &mut app, "ggen", &mut std::io::stdout()),
        CompletionSubcommand::Fish => generate(Fish, &mut app, "ggen", &mut std::io::stdout()),
    }
    Ok(())
}
