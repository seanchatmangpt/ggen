//! Template Noun Registration for clap-noun-verb
//!
//! This module defines the Args structs and run() functions for template commands,
//! delegating business logic to the domain layer.

use clap::{Args, Subcommand};
use ggen_utils::error::Result;
use std::path::PathBuf;

/// Template command arguments
#[derive(Debug, Args)]
pub struct TemplateArgs {
    #[command(subcommand)]
    pub command: TemplateCommand,
}

/// Template subcommands
#[derive(Debug, Subcommand)]
pub enum TemplateCommand {
    /// Generate from template
    Generate(GenerateArgs),
    /// Generate file tree from template
    GenerateTree(GenerateTreeArgs),
    /// Lint a template
    Lint(LintArgs),
    /// List templates
    List(ListArgs),
    /// Create new template
    New(NewArgs),
    /// Regenerate from template
    Regenerate(RegenerateArgs),
    /// Show template details
    Show(ShowArgs),
}

/// Generate command arguments
#[derive(Debug, clap::Args)]
pub struct GenerateArgs {
    /// Template file path
    #[arg(short = 't', long)]
    pub template: Option<PathBuf>,

    /// RDF/TTL file for context
    #[arg(short = 'r', long)]
    pub rdf: Option<PathBuf>,

    /// Output path
    #[arg(short = 'o', long)]
    pub output: Option<PathBuf>,

    /// Variables (key=value format)
    #[arg(short = 'v', long)]
    pub var: Vec<String>,

    /// Force overwrite
    #[arg(short = 'f', long)]
    pub force: bool,
}

/// Generate tree command arguments
#[derive(Debug, clap::Args)]
pub struct GenerateTreeArgs {
    /// Template file
    #[arg(short = 't', long)]
    pub template: Option<PathBuf>,

    /// Output directory
    #[arg(short = 'o', long)]
    pub output: Option<PathBuf>,
}

/// Lint command arguments
#[derive(Debug, clap::Args)]
pub struct LintArgs {
    /// Template file to lint
    pub template: PathBuf,
}

/// List command arguments
#[derive(Debug, clap::Args)]
pub struct ListArgs {
    /// Template directory
    #[arg(short = 'd', long)]
    pub directory: Option<PathBuf>,
}

/// New command arguments
#[derive(Debug, clap::Args)]
pub struct NewArgs {
    /// Template name
    pub name: String,

    /// Template type
    #[arg(short = 't', long)]
    pub template_type: Option<String>,
}

/// Regenerate command arguments
#[derive(Debug, clap::Args)]
pub struct RegenerateArgs {
    /// Template file
    #[arg(short = 't', long)]
    pub template: Option<PathBuf>,
}

/// Show command arguments
#[derive(Debug, clap::Args)]
pub struct ShowArgs {
    /// Template name or path
    pub template: String,
}

impl TemplateArgs {
    pub fn execute(&self) -> Result<()> {
        match &self.command {
            TemplateCommand::Generate(args) => run_generate(args),
            TemplateCommand::GenerateTree(args) => run_generate_tree(args),
            TemplateCommand::Lint(args) => run_lint(args),
            TemplateCommand::List(args) => run_list(args),
            TemplateCommand::New(args) => run_new(args),
            TemplateCommand::Regenerate(args) => run_regenerate(args),
            TemplateCommand::Show(args) => run_show(args),
        }
    }
}

/// Run generate command
fn run_generate(args: &GenerateArgs) -> Result<()> {
    use crate::domain::template;
    use std::collections::BTreeMap;

    // Parse variables
    let variables = parse_variables(&args.var)?;

    // Call domain layer
    let options = template::GenerateFileOptions {
        template_path: args.template.clone().unwrap_or_else(|| PathBuf::from("template.hbs")),
        output_path: args.output.clone().unwrap_or_else(|| PathBuf::from("output")),
        variables,
        force_overwrite: args.force,
    };

    let result = template::generate_file(&options)?;

    println!("✅ Generated {} ({} bytes)",
        result.output_path.display(),
        result.bytes_written
    );

    Ok(())
}

/// Run generate-tree command
fn run_generate_tree(_args: &GenerateTreeArgs) -> Result<()> {
    println!("🚧 generate-tree command placeholder");
    Ok(())
}

/// Run lint command
fn run_lint(_args: &LintArgs) -> Result<()> {
    println!("🚧 lint command placeholder");
    Ok(())
}

/// Run list command
fn run_list(_args: &ListArgs) -> Result<()> {
    println!("📋 Available templates:");
    println!("  • template.hbs (default)");
    Ok(())
}

/// Run new command
fn run_new(_args: &NewArgs) -> Result<()> {
    println!("🚧 new command placeholder");
    Ok(())
}

/// Run regenerate command
fn run_regenerate(_args: &RegenerateArgs) -> Result<()> {
    println!("🚧 regenerate command placeholder");
    Ok(())
}

/// Run show command
fn run_show(_args: &ShowArgs) -> Result<()> {
    println!("🚧 show command placeholder");
    Ok(())
}

/// Parse variable arguments (key=value format)
fn parse_variables(vars: &[String]) -> Result<BTreeMap<String, String>> {
    let mut map = BTreeMap::new();
    for var in vars {
        if let Some((key, value)) = var.split_once('=') {
            map.insert(key.to_string(), value.to_string());
        } else {
            return Err(ggen_utils::error::Error::new(&format!(
                "Invalid variable format: {}. Expected key=value",
                var
            )));
        }
    }
    Ok(map)
}

use std::collections::BTreeMap;
