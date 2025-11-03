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
    /// Initialize project with file-based routing conventions
    Init(InitArgs),
    /// Generate code using zero-config conventions
    Generate(GenerateArgs),
    /// Watch for changes and auto-regenerate
    Watch(WatchArgs),
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

    /// Apply preset configuration (clap-noun-verb, custom)
    #[arg(long)]
    pub preset: Option<String>,
}

/// Generate command arguments (convention-based)
#[derive(Debug, clap::Args)]
pub struct GenerateArgs {
    /// Template name (optional, generates all if not specified)
    pub template: Option<String>,

    /// Project path
    #[arg(short = 'p', long, default_value = ".")]
    pub path: PathBuf,

    /// Output directory (optional, inferred from config or file markers)
    #[arg(short = 'o', long)]
    pub output: Option<String>,

    /// Force overwrite existing files
    #[arg(short = 'f', long)]
    pub force: bool,
}

/// Watch command arguments
#[derive(Debug, clap::Args)]
pub struct WatchArgs {
    /// Project path
    #[arg(short = 'p', long, default_value = ".")]
    pub path: PathBuf,

    /// Debounce delay in milliseconds
    #[arg(long, default_value = "300")]
    pub debounce: u64,
}

impl ProjectArgs {
    pub fn execute(&self) -> Result<()> {
        match &self.command {
            ProjectCommand::New(args) => run_new(args),
            ProjectCommand::Gen(args) => run_gen(args),
            ProjectCommand::Apply(args) => run_apply(args),
            ProjectCommand::Plan(args) => run_plan(args),
            ProjectCommand::Init(args) => run_init(args),
            ProjectCommand::Generate(args) => run_generate(args),
            ProjectCommand::Watch(args) => run_watch(args),
        }
    }
}

/// Run new project command
fn run_new(args: &NewArgs) -> Result<()> {
    use ggen_domain::project;

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
    use ggen_domain::project;

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
    use ggen_domain::project;

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
    use ggen_domain::project;

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
    use crate::conventions::presets;
    use std::fs;

    let name = args.name.as_deref().unwrap_or("my-project");

    println!("🔧 Initializing project with conventions: {}", name);
    println!("📁 Path: {}", args.path.display());

    // Create base project structure
    fs::create_dir_all(&args.path)
        .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!("Failed to create project directory: {}", e)))?;

    // Create .ggen directory
    let ggen_dir = args.path.join(".ggen");
    fs::create_dir_all(&ggen_dir)
        .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!("Failed to create .ggen directory: {}", e)))?;

    // Apply preset if specified
    if let Some(preset_name) = &args.preset {
        println!("📦 Applying preset: {}", preset_name);

        let preset = presets::get_preset(preset_name)
            .ok_or_else(|| ggen_utils::error::Error::new_fmt(format_args!(
                "Unknown preset: {}. Available: {:?}",
                preset_name,
                presets::list_presets()
            )))?;

        // Create project structure
        preset.create_structure(&args.path)
            .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!("Failed to apply preset: {}", e)))?;

        // Create RDF files
        let rdf_dir = ggen_dir.join("rdf");
        fs::create_dir_all(&rdf_dir)
            .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!("Failed to create RDF directory: {}", e)))?;

        for (path, content) in preset.rdf_files() {
            let file_path = rdf_dir.join(path);
            if let Some(parent) = file_path.parent() {
                fs::create_dir_all(parent)?;
            }
            fs::write(&file_path, content)
                .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!("Failed to write RDF file: {}", e)))?;
            println!("   ✓ Created {}", file_path.strip_prefix(&args.path).unwrap_or(&file_path).display());
        }

        // Create templates
        let templates_dir = ggen_dir.join("templates");
        fs::create_dir_all(&templates_dir)
            .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!("Failed to create templates directory: {}", e)))?;

        for (path, content) in preset.templates() {
            let file_path = templates_dir.join(path);
            if let Some(parent) = file_path.parent() {
                fs::create_dir_all(parent)?;
            }
            fs::write(&file_path, content)
                .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!("Failed to write template: {}", e)))?;
            println!("   ✓ Created {}", file_path.strip_prefix(&args.path).unwrap_or(&file_path).display());
        }

        // Create conventions config
        let config_path = ggen_dir.join("conventions.toml");
        fs::write(&config_path, preset.config_content())
            .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!("Failed to write config: {}", e)))?;

        println!("✅ Applied preset: {}", preset_name);
    } else {
        // Create basic structure without preset
        let dirs = ["domain", "templates", "queries", "generated"];
        for dir in &dirs {
            let dir_path = args.path.join(dir);
            fs::create_dir_all(&dir_path)
                .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!("Failed to create directory: {}", e)))?;
            println!("   ✓ Created {}/", dir);
        }
    }

    println!("✅ Project initialized successfully!");
    println!("\n📋 Next steps:");
    println!("  - Add RDF files to .ggen/rdf/");
    println!("  - Add templates to .ggen/templates/");
    println!("  - Run: ggen generate");

    Ok(())
}

/// Run generate command (zero-config auto-discovery)
fn run_generate(args: &GenerateArgs) -> Result<()> {
    use ggen_domain::template;
    use std::fs;

    println!("🔧 Generating using zero-config conventions...");
    println!("📁 Project: {}", args.path.display());

    // Auto-discover templates
    let templates_dir = args.path.join("templates");
    let mut templates = Vec::new();

    if templates_dir.exists() {
        for entry in fs::read_dir(&templates_dir).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to read templates directory: {}", e))
        })? {
            let entry = entry.map_err(|e| {
                ggen_utils::error::Error::new(&format!("Failed to read directory entry: {}", e))
            })?;
            let path = entry.path();
            if path.extension().and_then(|s| s.to_str()) == Some("tmpl") {
                templates.push(path);
            }
        }
    }

    // Filter by template name if specified
    let templates_to_generate: Vec<_> = if let Some(ref template_name) = args.template {
        templates
            .iter()
            .filter(|t| t.file_name()
                .and_then(|n| n.to_str())
                .map(|n| n.contains(template_name))
                .unwrap_or(false))
            .cloned()
            .collect()
    } else {
        templates
    };

    if templates_to_generate.is_empty() {
        return Err(ggen_utils::error::Error::new(
            "No templates found. Create templates in templates/ directory or specify template name."
        ));
    }

    println!("📊 Discovered {} template(s)", templates_to_generate.len());

    // Generate from each template
    let mut total_files = 0;
    for template_path in &templates_to_generate {
        println!("\n🔨 Generating from: {}", template_path.display());

        // Determine output path from file markers or config
        let output_dir = if let Some(ref dir) = args.output {
            args.path.join(dir)
        } else {
            // Try to infer from clnrm-v2-ggen.toml or use default
            let config_path = args.path.join("clnrm-v2-ggen.toml");
            if config_path.exists() {
                // Parse config for output_dir (simplified - just use default for now)
                args.path.join("crates/clnrm-v2-generated")
            } else {
                args.path.join("generated")
            }
        };

        // Generate template (RDF files loaded from frontmatter)
        let options = template::RenderWithRdfOptions {
            template_path: template_path.clone(),
            output_path: output_dir.join("placeholder"), // File marker splitting will use this as base dir
            rdf_files: Vec::new(), // Empty - use frontmatter rdf: field
            variables: std::collections::BTreeMap::new(),
            force_overwrite: args.force,
            use_preprocessor: false,
        };

        let result = template::render_with_rdf(&options)?;

        println!("   ✅ Generated {} file(s) ({})", result.files_created, result.bytes_written);
        if result.rdf_files_loaded > 0 {
            println!("      📊 Loaded {} RDF file(s) from frontmatter", result.rdf_files_loaded);
        }
        if result.sparql_queries_executed > 0 {
            println!("      🔍 Executed {} SPARQL query(ies)", result.sparql_queries_executed);
        }

        total_files += result.files_created;
    }

    println!("\n✅ Generation complete!");
    println!("📝 Generated {} total files", total_files);

    Ok(())
}

/// Run watch command
fn run_watch(args: &WatchArgs) -> Result<()> {
    use crate::conventions::ProjectWatcher;

    println!("👀 Starting watch mode...");
    println!("📁 Project: {}", args.path.display());
    println!("⏱️  Debounce: {}ms (note: watcher uses fixed debounce)", args.debounce);

    // Create watcher (uses fixed 300ms debounce internally)
    let mut watcher = ProjectWatcher::new(args.path.clone())
        .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!("Failed to create watcher: {}", e)))?;

    println!("\n✅ Ready! Edit files to trigger regeneration.");
    println!("   Press Ctrl+C to stop.\n");

    // Start watching (blocking)
    watcher.watch()
        .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!("Watch failed: {}", e)))?;

    println!("✅ Watch mode stopped");

    Ok(())
}
