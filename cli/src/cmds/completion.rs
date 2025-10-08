use super::CompletionSubcommand;
use clap::CommandFactory;
use clap_complete::{
    generate,
    shells::{Bash, Fish, Zsh},
};

pub fn run(sub: &CompletionSubcommand) -> utils::error::Result<()> {
    let mut app = crate::Cli::command();
    match sub {
        CompletionSubcommand::Bash => generate(Bash, &mut app, "rgen", &mut std::io::stdout()),
        CompletionSubcommand::Zsh => generate(Zsh, &mut app, "rgen", &mut std::io::stdout()),
        CompletionSubcommand::Fish => generate(Fish, &mut app, "rgen", &mut std::io::stdout()),
    }
    Ok(())
}
