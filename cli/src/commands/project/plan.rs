//! Generation plan CLI command
//!
//! Delegates to domain layer for business logic

use clap::Args;
use clap_noun_verb_macros::verb;
use ggen_utils::error::Result;

#[derive(Args, Debug, Clone)]
pub struct PlanArgs {
    /// Template reference (e.g., "gpack-id:path/to.tmpl" or "local/path.tmpl")
    pub template_ref: String,

    /// Variables to pass to the template (key=value format)
    #[arg(short = 'v', long = "var")]
    pub vars: Vec<String>,

    /// Output file for the generated plan
    #[arg(short = 'o', long = "output")]
    pub output: Option<String>,

    /// Output format (json, yaml, toml)
    #[arg(long, default_value = "json")]
    pub format: String,
}

pub async fn run(args: &PlanArgs) -> Result<()> {
    println!("📋 Creating generation plan...");

    // Delegate to domain layer using sync wrapper
    let result = tokio::task::spawn_blocking({
        let args = args.clone();
        move || crate::domain::project::plan::create_plan(&args)
    })
    .await
    .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!("Task join error: {}", e)))??;

    println!("✅ Generation plan created: {}", result.output_path);
    println!("   Format: {}", args.format);
    println!("   Variables: {}", args.vars.len());

    Ok(())
}
