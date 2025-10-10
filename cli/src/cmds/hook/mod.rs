use clap::{Args, Subcommand};
use ggen_utils::error::Result;

// Declare verb modules
pub mod create;
pub mod list;
pub mod remove;
pub mod run;
pub mod validate;

#[derive(Args, Debug)]
pub struct HookCmd {
    #[command(subcommand)]
    pub verb: Verb,
}

#[derive(Subcommand, Debug)]
pub enum Verb {
    /// Create a new knowledge hook for automatic graph regeneration
    ///
    /// Examples:
    ///   ggen hook create "pre-commit" --trigger git-pre-commit --template graph-gen.tmpl
    ///   ggen hook create "post-merge" --trigger git-post-merge --template sync-graph.tmpl
    ///   ggen hook create "nightly" --trigger cron --schedule "0 2 * * *" --template full-rebuild.tmpl
    ///   ggen hook create "file-watch" --trigger file-watch --path "src/**/*.rs" --template incremental.tmpl --dry-run
    Create(create::CreateArgs),

    /// List all knowledge hooks (active, disabled, or all)
    ///
    /// Examples:
    ///   ggen hook list
    ///   ggen hook list --active
    ///   ggen hook list --disabled
    ///   ggen hook list --json
    List(list::ListArgs),

    /// Manually run a knowledge hook (for testing or manual execution)
    ///
    /// Examples:
    ///   ggen hook run "pre-commit"
    ///   ggen hook run "nightly" --dry-run
    ///   ggen hook run "file-watch" --var changed_file=src/main.rs
    Run(run::RunArgs),

    /// Remove a knowledge hook and uninstall it from the system
    ///
    /// Examples:
    ///   ggen hook remove "pre-commit"
    ///   ggen hook remove "nightly" --force
    Remove(remove::RemoveArgs),

    /// Validate a hook's configuration without running it
    ///
    /// Examples:
    ///   ggen hook validate "pre-commit"
    ///   ggen hook validate "nightly" --json
    Validate(validate::ValidateArgs),
}

impl HookCmd {
    pub async fn run(&self) -> Result<()> {
        match &self.verb {
            Verb::Create(args) => create::run(args).await,
            Verb::List(args) => list::run(args).await,
            Verb::Run(args) => run::run(args).await,
            Verb::Remove(args) => remove::run(args).await,
            Verb::Validate(args) => validate::run(args).await,
        }
    }
}
