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
    /// Generate CLI project from RDF/TTL file
    GenerateRdf(GenerateRdfArgs),
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

    /// RDF/TTL file(s) for context (can be specified multiple times)
    #[arg(short = 'r', long)]
    pub rdf: Vec<PathBuf>,

    /// Output path
    #[arg(short = 'o', long)]
    pub output: Option<PathBuf>,

    /// Variables (key=value format)
    #[arg(short = 'v', long)]
    pub var: Vec<String>,

    /// Force overwrite
    #[arg(short = 'f', long)]
    pub force: bool,

    /// Enable preprocessor for advanced template processing
    #[arg(long)]
    pub preprocessor: bool,

    /// Generate template FROM RDF metadata (reverse operation)
    #[arg(long)]
    pub from_rdf: bool,
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

/// Generate RDF command arguments
#[derive(Debug, clap::Args)]
pub struct GenerateRdfArgs {
    /// Path to TTL file containing CLI definition
    #[arg(value_name = "TTL_FILE")]
    pub ttl_file: PathBuf,

    /// Output directory for generated project
    #[arg(short = 'o', long = "output", default_value = ".")]
    pub output: PathBuf,

    /// Template directory containing rendering templates
    #[arg(long = "templates", default_value = "examples/clap-noun-verb-demo/templates")]
    pub templates: PathBuf,
}

impl TemplateArgs {
    pub fn execute(&self) -> Result<()> {
        match &self.command {
            TemplateCommand::Generate(args) => run_generate(args),
            TemplateCommand::GenerateRdf(args) => run_generate_rdf(args),
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

    // Handle reverse operation: generate template FROM RDF
    if args.from_rdf {
        if args.rdf.is_empty() {
            return Err(ggen_utils::error::Error::new(
                "No RDF files specified. Use --rdf to specify RDF metadata files."
            ));
        }

        let output_template = args.template.clone().unwrap_or_else(|| PathBuf::from("generated.tmpl"));

        let result_path = template::generate_from_rdf(args.rdf.clone(), output_template)?;

        println!("✅ Generated template from RDF: {}", result_path.display());
        return Ok(());
    }

    // Parse variables
    let variables = parse_variables(&args.var)?;

    // Use v2 RDF integration if RDF files provided, otherwise fall back to v1
    if !args.rdf.is_empty() {
        // v2 API: render with RDF integration
        let options = template::RenderWithRdfOptions {
            template_path: args.template.clone().unwrap_or_else(|| PathBuf::from("template.tmpl")),
            output_path: args.output.clone().unwrap_or_else(|| PathBuf::from("output")),
            rdf_files: args.rdf.clone(),
            variables,
            force_overwrite: args.force,
            use_preprocessor: args.preprocessor,
        };

        let result = template::render_with_rdf(&options)?;

        println!("✅ Generated {} ({} bytes)", result.output_path.display(), result.bytes_written);
        if result.rdf_files_loaded > 0 {
            println!("   📊 Loaded {} RDF file(s)", result.rdf_files_loaded);
        }
        if result.sparql_queries_executed > 0 {
            println!("   🔍 Executed {} SPARQL query(ies)", result.sparql_queries_executed);
        }
    } else {
        // v1 API: backward compatible file generation
        let options = template::GenerateFileOptions {
            template_path: args.template.clone().unwrap_or_else(|| PathBuf::from("template.tmpl")),
            output_path: args.output.clone().unwrap_or_else(|| PathBuf::from("output")),
            variables,
            force_overwrite: args.force,
        };

        let result = template::generate_file(&options)?;

        println!("✅ Generated {} ({} bytes)",
            result.output_path.display(),
            result.bytes_written
        );
    }

    Ok(())
}

/// Run generate-tree command
fn run_generate_tree(args: &GenerateTreeArgs) -> Result<()> {
    use crate::domain::template::generate_tree;
    use std::collections::HashMap;

    crate::runtime::execute(async move {
        // For now, use empty variables - full implementation needs var support
        let variables: HashMap<String, String> = HashMap::new();

        let template_path = args.template.as_ref().ok_or_else(|| {
            ggen_utils::error::Error::new("Template path required for generate-tree")
        })?;
        let output_path = args.output.as_ref().ok_or_else(|| {
            ggen_utils::error::Error::new("Output path required for generate-tree")
        })?;

        let _result = generate_tree::generate_file_tree(
            template_path,
            output_path,
            &variables,
            false,
        )?;

        println!("✅ Generated file tree");
        println!("📁 Output directory: {}", output_path.display());

        Ok(())
    })
}

/// Run lint command
fn run_lint(args: &LintArgs) -> Result<()> {
    use crate::domain::template::lint;

    crate::runtime::execute(async move {
        let options = lint::LintOptions {
            check_sparql: false,
            check_schema: false,
        };

        let report = lint::lint_template(&args.template.to_string_lossy(), &options)?;

        if report.has_errors() {
            println!("❌ Lint errors found:");
            for error in &report.errors {
                if let Some(line) = error.line {
                    println!("  Line {}: {}", line, error.message);
                } else {
                    println!("  {}", error.message);
                }
            }
        }

        if report.has_warnings() {
            println!("⚠️  Lint warnings:");
            for warning in &report.warnings {
                if let Some(line) = warning.line {
                    println!("  Line {}: {}", line, warning.message);
                } else {
                    println!("  {}", warning.message);
                }
            }
        }

        if !report.has_errors() && !report.has_warnings() {
            println!("✅ Template passed all linting checks");
        }

        Ok(())
    })
}

/// Run list command
fn run_list(args: &ListArgs) -> Result<()> {
    use crate::domain::template::list;

    crate::runtime::execute(async move {
        let filters = list::ListFilters {
            pattern: None,
            local_only: false,
            gpack_only: false,
        };

        let default_dir = PathBuf::from("templates");
        let templates_dir = args.directory.as_ref().unwrap_or(&default_dir);
        let templates = list::list_templates(templates_dir, &filters)?;

        if templates.is_empty() {
            println!("No templates found in {}", templates_dir.display());
            return Ok(());
        }

        println!("📋 Available templates ({} found):", templates.len());
        println!();

        for template in templates {
            let source_display = match &template.source {
                list::TemplateSource::Local => "local",
                list::TemplateSource::Gpack(name) => name.as_str(),
            };

            println!("  {} - {}", template.name, source_display);
            if let Some(desc) = &template.description {
                println!("    {}", desc);
            }
            println!("    Path: {}", template.path);
            println!();
        }

        Ok(())
    })
}

/// Run new command
fn run_new(args: &NewArgs) -> Result<()> {
    use crate::domain::template::new as template_new;
    use crate::domain::template::TemplateService;

    crate::runtime::execute(async move {
        let template_type = args.template_type.as_deref().unwrap_or("generic");
        let content = template_new::generate_template_content(&args.name, template_type)?;

        let service = TemplateService::default_instance();
        let path = service.write_template(&args.name, &content)?;

        println!("✅ Created new {} template: {}", template_type, path.display());
        println!("   Template name: {}", args.name);

        Ok(())
    })
}

/// Run regenerate command
fn run_regenerate(args: &RegenerateArgs) -> Result<()> {
    crate::runtime::execute(async move {
        let default_template = PathBuf::from("template.tmpl");
        let template_path = args.template.as_ref().unwrap_or(&default_template);

        // For now, use a simple regeneration without merge
        println!("🔄 Regenerating from template: {}", template_path.display());
        println!("⚠️  Merge strategies not yet fully implemented");
        println!("   Use 'ggen template generate' for basic regeneration");

        Ok(())
    })
}

/// Run show command
fn run_show(args: &ShowArgs) -> Result<()> {
    use crate::domain::template::show;

    crate::runtime::execute(async move {
        let metadata = show::show_template_metadata(&args.template)?;

        println!("📄 Template: {}", metadata.name);
        println!("   Path: {}", metadata.path);

        if let Some(desc) = &metadata.description {
            println!("   Description: {}", desc);
        }

        if let Some(output) = &metadata.output_path {
            println!("   Output: {}", output);
        }

        if !metadata.variables.is_empty() {
            println!("   Variables:");
            for var in &metadata.variables {
                println!("     • {}", var);
            }
        }

        if !metadata.rdf_sources.is_empty() {
            println!("   RDF Sources:");
            for source in &metadata.rdf_sources {
                println!("     • {}", source);
            }
        }

        if !metadata.sparql_queries.is_empty() {
            println!("   SPARQL Queries: {}", metadata.sparql_queries.len());
        }

        if let Some(seed) = metadata.determinism_seed {
            println!("   Determinism Seed: {}", seed);
        }

        Ok(())
    })
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

/// Run generate-rdf command
fn run_generate_rdf(args: &GenerateRdfArgs) -> Result<()> {
    use crate::domain::template::generate_rdf;

    crate::runtime::execute(async move {
        let options = generate_rdf::GenerateFromRdfOptions::new(
            args.ttl_file.clone(),
            args.output.clone(),
            args.templates.clone(),
        );

        let result = generate_rdf::generate_cli_from_rdf(&options)?;

        println!("✅ Generated CLI project from RDF");
        println!("   📁 Output directory: {}", result.output_dir.display());
        println!("   📄 Files generated: {}", result.files_generated);
        println!("   📦 Project name: {}", result.project_name);
        println!();
        println!("Next steps:");
        println!("   cd {}", result.output_dir.display());
        println!("   cargo build");
        println!("   cargo run -- --help");

        Ok(())
    })
}
