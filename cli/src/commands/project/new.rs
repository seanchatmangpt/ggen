//! Project creation CLI command
//!
//! Delegates to domain layer for business logic

use clap::Args;
use clap_noun_verb_macros::verb;
use ggen_utils::error::Result;
use std::path::PathBuf;

#[derive(Args, Debug, Clone)]
pub struct NewArgs {
    /// Project name
    #[arg(help = "Name of the project to create")]
    pub name: String,

    /// Project type: rust-web, rust-cli, rust-lib, nextjs, nuxt
    #[arg(short = 't', long = "type", help = "Project type to generate")]
    pub project_type: String,

    /// Framework (optional)
    #[arg(
        short = 'f',
        long = "framework",
        help = "Framework to use (e.g., axum, warp for rust-web)"
    )]
    pub framework: Option<String>,

    /// Output directory (defaults to current directory)
    #[arg(
        short = 'o',
        long = "output",
        default_value = ".",
        help = "Output directory for the project"
    )]
    pub output: PathBuf,

    /// Skip dependency installation
    #[arg(
        long = "skip-install",
        help = "Skip installing dependencies after generation",
        default_value = "false"
    )]
    pub skip_install: bool,
}

pub async fn run(args: &NewArgs) -> Result<()> {
    println!("🚀 Creating new project: {}", args.name);
    println!("   Type: {}", args.project_type);
    if let Some(framework) = &args.framework {
        println!("   Framework: {}", framework);
    }
    println!();

    // Delegate to domain layer using sync wrapper
    let result = tokio::task::spawn_blocking({
        let args = args.clone();
        move || crate::domain::project::new::create_project(&args)
    })
    .await
    .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!("Task join error: {}", e)))??;

    println!();
    println!("✅ Project created successfully!");
    println!();
    println!("Next steps:");
    println!("  cd {}", args.name);
    println!("{}", result.next_steps);

    Ok(())
}
