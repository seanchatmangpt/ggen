//! Project generation CLI command
//!
//! Delegates to domain layer for business logic

use clap::Args;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
pub struct GenArgs {
    /// Template reference (e.g., "gpack-id:path/to.tmpl" or "local/path.tmpl")
    pub template_ref: String,

    /// Variables to pass to the template (key=value format)
    #[arg(short = 'v', long = "var")]
    pub vars: Vec<String>,

    /// Perform dry-run without creating files
    #[arg(long)]
    pub dry_run: bool,

    /// Deterministic seed for reproducible generation
    #[arg(long)]
    pub seed: Option<u64>,

    /// Force overwrite existing files
    #[arg(short = 'f', long)]
    pub force: bool,

    /// Use AI-powered generation
    #[arg(long)]
    pub ai: bool,

    /// AI model to use (openai, anthropic, ollama)
    #[arg(long, default_value = "ollama")]
    pub ai_provider: String,

    /// AI model name
    #[arg(long)]
    pub ai_model: Option<String>,

    /// Maximum iterations for AI validation
    #[arg(long, default_value = "3")]
    pub ai_max_iterations: usize,
}

pub async fn run(args: &GenArgs) -> Result<()> {
    println!("🚀 Generating project artifacts...");

    // Delegate to domain layer using sync wrapper
    let result = tokio::task::spawn_blocking({
        let args = args.clone();
        move || crate::domain::project::gen::generate_project(&args)
    })
    .await
    .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!("Task join error: {}", e)))??;

    println!("✅ Generation completed successfully!");
    println!("   Files created: {}", result.files_created);
    println!("   Dry run: {}", args.dry_run);

    Ok(())
}

// Make GenArgs cloneable for spawn_blocking
impl Clone for GenArgs {
    fn clone(&self) -> Self {
        Self {
            template_ref: self.template_ref.clone(),
            vars: self.vars.clone(),
            dry_run: self.dry_run,
            seed: self.seed,
            force: self.force,
            ai: self.ai,
            ai_provider: self.ai_provider.clone(),
            ai_model: self.ai_model.clone(),
            ai_max_iterations: self.ai_max_iterations,
        }
    }
}
