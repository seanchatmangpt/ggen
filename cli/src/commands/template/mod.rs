//! Template commands - CLI layer for template operations

use clap::Subcommand;
use ggen_utils::error::Result;

pub mod generate;
pub mod list;
pub mod new;
pub mod generate_tree;
pub mod regenerate;
pub mod lint;
pub mod show;

/// Template noun commands
#[derive(Debug, clap::Args)]
pub struct TemplateArgs {
    #[command(subcommand)]
    pub command: TemplateCommand,
}

#[derive(Debug, Subcommand)]
pub enum TemplateCommand {
    /// Generate from template
    Generate(generate::GenerateArgs),
    /// List available templates
    List(list::ListCommand),
    /// Create a new template
    New(new::NewCommand),
    /// Generate file tree from template
    GenerateTree(generate_tree::GenerateTreeCommand),
    /// Regenerate code using delta-driven projection
    Regenerate(regenerate::RegenerateCommand),
    /// Lint a template for common issues
    Lint(lint::LintCommand),
    /// Show template details
    Show(show::ShowCommand),
}

impl TemplateArgs {
    pub async fn execute(&self) -> Result<()> {
        match &self.command {
            TemplateCommand::Generate(args) => generate::run(args),
            TemplateCommand::List(cmd) => cmd.execute().await,
            TemplateCommand::New(cmd) => cmd.execute().await,
            TemplateCommand::GenerateTree(cmd) => cmd.execute().await,
            TemplateCommand::Regenerate(cmd) => cmd.execute().await,
            TemplateCommand::Lint(cmd) => cmd.execute().await,
            TemplateCommand::Show(cmd) => cmd.execute().await,
        }
    }
}
