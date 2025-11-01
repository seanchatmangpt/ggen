use clap::Subcommand;
use ggen_utils::error::Result;

pub mod generate_tree;
pub mod lint;
pub mod list;
pub mod new;
pub mod regenerate;
pub mod show;

#[derive(clap::Args, Debug)]
pub struct TemplateCmd {
    #[command(subcommand)]
    pub verb: Verb,
}

#[derive(Subcommand, Debug)]
pub enum Verb {
    /// Create a new template
    New(new::NewArgs),
    /// List available templates
    List(list::ListArgs),
    /// Show template details
    Show(show::ShowArgs),
    /// Lint a template
    Lint(lint::LintArgs),
    /// Regenerate code using delta-driven projection
    Regenerate(regenerate::RegenerateArgs),
    /// Generate file tree from template
    GenerateTree(generate_tree::GenerateTreeArgs),
}

impl TemplateCmd {
    pub async fn run(&self) -> Result<()> {
        match &self.verb {
            Verb::New(args) => new::run(args).await,
            Verb::List(args) => list::run(args).await,
            Verb::Show(args) => show::run(args).await,
            Verb::Lint(args) => lint::run(args).await,
            Verb::Regenerate(args) => regenerate::run(args)
                .await
                .map_err(|e| ggen_utils::error::Error::new(&e.to_string())),
            Verb::GenerateTree(args) => generate_tree::run(args).await,
        }
    }
}
