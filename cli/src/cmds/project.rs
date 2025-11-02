//! Project Noun Registration for clap-noun-verb
//!
//! This module defines the Args structs and run() functions for project commands,
//! delegating business logic to the domain layer.

use clap::{Args, Subcommand};
use ggen_utils::error::Result;
use std::path::PathBuf;

/// Project command arguments
#[derive(Debug, Args)]
pub struct ProjectArgs {
    #[command(subcommand)]
    pub command: ProjectCommand,
}

/// Project subcommands
#[derive(Debug, Subcommand)]
pub enum ProjectCommand {
    /// Create new project
    New(NewArgs),
    /// Generate project from template + RDF
    Gen(GenArgs),
    /// Apply generation plan
    Apply(ApplyArgs),
    /// Create generation plan
    Plan(PlanArgs),
    /// Initialize project
    Init(InitArgs),
}

/// New project command arguments
#[derive(Debug, clap::Args)]
pub struct NewArgs {
    /// Project name
    pub name: String,

    /// Project type (rust-cli, rust-web, rust-lib, nextjs, nuxt)
    #[arg(short = 't', long, default_value = "rust-cli")]
    pub project_type: String,

    /// Framework (optional, depends on project type)
    #[arg(short = 'f', long)]
    pub framework: Option<String>,

    /// Output directory
    #[arg(short = 'o', long, default_value = ".")]
    pub output: PathBuf,

    /// Skip npm/cargo install
    #[arg(long)]
    pub skip_install: bool,
}

/// Generate project command arguments
#[derive(Debug, clap::Args)]
pub struct GenArgs {
    /// Template reference (path or name)
    #[arg(short = 't', long)]
    pub template_ref: String,

    /// Variables (key=value format)
    #[arg(short = 'v', long)]
    pub vars: Vec<String>,

    /// Dry run (show plan without executing)
    #[arg(long)]
    pub dry_run: bool,
}

/// Apply plan command arguments
#[derive(Debug, clap::Args)]
pub struct ApplyArgs {
    /// Plan file path (.json, .yaml, .toml)
    pub plan_file: String,

    /// Auto-confirm without prompting
    #[arg(short = 'y', long)]
    pub auto_confirm: bool,

    /// Dry run (show what would be applied)
    #[arg(long)]
    pub dry_run: bool,
}

/// Plan command arguments
#[derive(Debug, clap::Args)]
pub struct PlanArgs {
    /// Template reference (path or name)
    #[arg(short = 't', long)]
    pub template_ref: String,

    /// Variables (key=value format)
    #[arg(short = 'v', long)]
    pub vars: Vec<String>,

    /// Output file path
    #[arg(short = 'o', long)]
    pub output: Option<String>,

    /// Output format (json, yaml, toml)
    #[arg(short = 'f', long, default_value = "json")]
    pub format: String,
}

/// Initialize project command arguments
#[derive(Debug, clap::Args)]
pub struct InitArgs {
    /// Project path
    #[arg(default_value = ".")]
    pub path: PathBuf,

    /// Project name
    #[arg(short = 'n', long)]
    pub name: Option<String>,
}

impl ProjectArgs {
    pub fn execute(&self) -> Result<()> {
        match &self.command {
            ProjectCommand::New(args) => run_new(args),
            ProjectCommand::Gen(args) => run_gen(args),
            ProjectCommand::Apply(args) => run_apply(args),
            ProjectCommand::Plan(args) => run_plan(args),
            ProjectCommand::Init(args) => run_init(args),
        }
    }
}

/// Run new project command
fn run_new(args: &NewArgs) -> Result<()> {
    use crate::domain::project;

    println!("🚀 Creating new {} project: {}", args.project_type, args.name);

    let result = project::new::create_project(args)?;

    println!("✅ Project created successfully!");
    println!("📁 Path: {}", result.project_path);
    println!("\n📋 Next steps:");
    println!("  cd {}", args.name);
    println!("{}", result.next_steps);

    Ok(())
}

/// Run generate command
fn run_gen(args: &GenArgs) -> Result<()> {
    use crate::domain::project;

    println!("🔧 Generating project from template: {}", args.template_ref);

    if args.dry_run {
        println!("🔍 Dry run mode - no files will be created");
    }

    let result = project::gen::generate_project(args)?;

    if args.dry_run {
        println!("\n📋 Generation Plan ({} operations):", result.operations.len());
        for op in &result.operations {
            match op {
                project::gen::Operation::Create { path, .. } => {
                    println!("  [CREATE] {}", path);
                }
                project::gen::Operation::Update { path, .. } => {
                    println!("  [UPDATE] {}", path);
                }
                project::gen::Operation::Delete { path } => {
                    println!("  [DELETE] {}", path);
                }
            }
        }
    } else {
        println!("✅ Generated {} files", result.files_created);
    }

    Ok(())
}

/// Run apply command
fn run_apply(args: &ApplyArgs) -> Result<()> {
    use crate::domain::project;

    println!("📦 Applying generation plan: {}", args.plan_file);

    let result = project::apply::apply_plan(args)?;

    if !args.dry_run && result.operations_count > 0 {
        println!("✅ Applied {} operations successfully", result.operations_count);
    } else if args.dry_run {
        println!("🔍 Dry run complete - {} operations would be applied", result.operations_count);
    } else {
        println!("ℹ️  No operations applied");
    }

    Ok(())
}

/// Run plan command
fn run_plan(args: &PlanArgs) -> Result<()> {
    use crate::domain::project;

    println!("📝 Creating generation plan from template: {}", args.template_ref);

    let result = project::plan::create_plan(args)?;

    println!("✅ Plan created successfully!");
    println!("📁 Saved to: {}", result.output_path);
    println!("📊 Variables: {}", result.variables_count);
    println!("\n📋 Next step:");
    println!("  ggen project apply {}", result.output_path);

    Ok(())
}

/// Run init command
fn run_init(args: &InitArgs) -> Result<()> {
    use crate::domain::project;

    let name = args.name.as_deref().unwrap_or("my-project");

    println!("🔧 Initializing project: {}", name);
    println!("📁 Path: {}", args.path.display());

    // Create runtime for async operation
    let runtime = tokio::runtime::Runtime::new()
        .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!("Runtime error: {}", e)))?;

    runtime.block_on(async {
        project::init::init_project(&args.path, name).await
    })
    .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!("Init failed: {}", e)))?;

    println!("✅ Project initialized successfully!");

    if project::init::is_project(&args.path) {
        println!("✓ Project structure validated");
    }

    Ok(())
}
