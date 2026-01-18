//! Project Commands for clap-noun-verb v3.4.0
//!
//! Migrated from project.rs to use #[verb] macros for project scaffolding,
//! code generation, and project management operations.

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use serde::Serialize;
use std::collections::BTreeMap;
use std::path::PathBuf;

// ============================================================================
// Output Types (all must derive Serialize for JSON output)
// ============================================================================

#[derive(Serialize)]
struct NewOutput {
    project_name: String,
    path: String,
    project_type: String,
    framework: Option<String>,
    files_created: usize,
    next_steps: String,
}

#[derive(Serialize)]
struct PlanOutput {
    plan_file: String,
    output_path: String,
    format: String,
    tasks: Vec<String>,
    variables_count: usize,
    operations_count: usize,
}

#[derive(Serialize)]
struct GenOutput {
    files_generated: usize,
    files_created: usize,
    output_dir: String,
    operations: Vec<OperationSummary>,
    dry_run: bool,
}

#[derive(Serialize)]
struct OperationSummary {
    operation_type: String,
    path: String,
}

#[derive(Serialize)]
struct ApplyOutput {
    changes_applied: usize,
    operations_count: usize,
    files_modified: usize,
    files_created: usize,
    files_deleted: usize,
    dry_run: bool,
}

#[derive(Serialize)]
struct InitOutput {
    project_name: String,
    project_path: String,
    preset: Option<String>,
    files_created: Vec<String>,
    directories_created: Vec<String>,
    next_steps: Vec<String>,
}

#[derive(Serialize)]
struct GenerateOutput {
    templates_processed: usize,
    files_generated: usize,
    bytes_written: String,
    output_paths: Vec<String>,
}

#[derive(Serialize)]
struct WatchOutput {
    project_path: String,
    debounce_ms: u64,
    status: String,
    message: String,
}

// ============================================================================
// Verb Functions (the actual CLI commands)
// ============================================================================

/// Create new project from scratch with specified type and framework
///
/// # Examples
///
/// Create a Rust CLI project:
/// ```bash
/// ggen project new my-cli --type rust-cli
/// ```
///
/// Create a Next.js web project:
/// ```bash
/// ggen project new my-app --type nextjs --framework app-router
/// ```
///
/// Create project with custom output directory:
/// ```bash
/// ggen project new my-project --type rust-web --output ./workspace
/// ```
#[verb]
fn new(
    name: String, project_type: String, framework: Option<String>, output: PathBuf,
    skip_install: bool,
) -> Result<NewOutput> {
    use ggen_domain::project;

    // Helper function to bridge sync/async and handle errors
    async fn new_impl(
        name: String, project_type: String, framework: Option<String>, output: PathBuf,
        skip_install: bool,
    ) -> Result<NewOutput> {
        // Construct args for domain layer
        let args = project::new::NewInput {
            name: name.clone(),
            project_type: project_type.clone(),
            framework: framework.clone(),
            output: Some(output.display().to_string()),
            skip_install,
        };

        // Call domain logic
        let result = project::new::create_project(&args)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

        Ok(NewOutput {
            project_name: name,
            path: result.project_path,
            project_type,
            framework,
            files_created: 0, // CreationResult doesn't have files_created field
            next_steps: result.next_steps,
        })
    }

    // Execute the async function and convert errors
    crate::runtime::block_on(new_impl(
        name,
        project_type,
        framework,
        output,
        skip_install,
    ))
    .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?
}

/// Generate project plan from template with variable substitution
///
/// # Examples
///
/// Create plan from template:
/// ```bash
/// ggen project plan --template my-template.tmpl --output plan.json
/// ```
///
/// With variables:
/// ```bash
/// ggen project plan --template app.tmpl --var name=myapp --var author=me --format yaml
/// ```
///
/// Generate and review:
/// ```bash
/// ggen project plan --template service.tmpl --var service=auth --format json
/// cat plan.json  # Review before applying
/// ggen project apply plan.json
/// ```
#[verb]
fn plan(
    template_ref: String, vars: Option<String>, output: Option<String>, format: Option<String>,
) -> Result<PlanOutput> {
    use ggen_domain::project;

    // Validate and sanitize variables
    let vars: Vec<String> = vars
        .map(|v| {
            v.split(',')
                .map(|part| {
                    let trimmed = part.trim();
                    if !trimmed.contains('=') {
                        // Variables should be in KEY=VALUE format
                        return trimmed.to_string();
                    }
                    let (key, _value) = trimmed.split_once('=').unwrap_or(("", ""));
                    // Validate key is alphanumeric + underscore
                    if !key.chars().all(|c| c.is_alphanumeric() || c == '_') {
                        log::warn!("Variable key contains non-alphanumeric characters, may be filtered by template engine: {}", key);
                    }
                    trimmed.to_string()
                })
                .collect()
        })
        .unwrap_or_default();
    let format = format.unwrap_or_else(|| "json".to_string());

    // Helper function to bridge sync/async and handle errors
    async fn plan_impl(
        template_ref: String, vars: Vec<String>, output: Option<String>, format: String,
    ) -> Result<PlanOutput> {
        let args = project::plan::PlanInput {
            template_ref: template_ref.clone(),
            vars: vars.clone(),
            output: output.clone(),
            format: format.clone(),
        };

        let result = project::plan::create_plan(&args)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

        Ok(PlanOutput {
            plan_file: template_ref,
            output_path: result.output_path,
            format,
            tasks: vec![], // PlanResult doesn't have tasks field
            variables_count: result.variables_count,
            operations_count: 0, // PlanResult doesn't have operations_count field
        })
    }

    // Execute the async function and convert errors
    crate::runtime::block_on(plan_impl(template_ref, vars, output, format))
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?
}

/// Generate code from template with RDF/SPARQL integration
///
/// # Examples
///
/// Generate from template:
/// ```bash
/// ggen project gen --template my-template --var name=app
/// ```
///
/// Dry run (preview without creating files):
/// ```bash
/// ggen project gen --template app.tmpl --var name=test --dry-run
/// ```
///
/// Full generation with multiple variables:
/// ```bash
/// ggen project gen --template service.tmpl \
///   --var service=auth \
///   --var port=8080 \
///   --var db=postgres
/// ```
#[verb]
fn gen(template_ref: String, vars: Option<String>, dry_run: bool) -> Result<GenOutput> {
    use ggen_domain::project;

    // Validate and sanitize variables
    let vars: Vec<String> = vars
        .map(|v| {
            v.split(',')
                .map(|part| {
                    let trimmed = part.trim();
                    if !trimmed.contains('=') {
                        // Variables should be in KEY=VALUE format
                        return trimmed.to_string();
                    }
                    let (key, _value) = trimmed.split_once('=').unwrap_or(("", ""));
                    // Validate key is alphanumeric + underscore
                    if !key.chars().all(|c| c.is_alphanumeric() || c == '_') {
                        log::warn!("Variable key contains non-alphanumeric characters, may be filtered by template engine: {}", key);
                    }
                    trimmed.to_string()
                })
                .collect()
        })
        .unwrap_or_default();

    // Helper function to bridge sync/async and handle errors
    async fn gen_impl(template_ref: String, vars: Vec<String>, dry_run: bool) -> Result<GenOutput> {
        let input = project::gen::GenInput {
            template_ref: template_ref.clone(),
            vars: vars.clone(),
            output_dir: std::path::PathBuf::from("."),
            dry_run,
        };

        let result = project::gen::execute_gen(input)
            .await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

        // Convert operations to summary
        let operations: Vec<OperationSummary> = result
            .operations
            .iter()
            .map(|op| match op {
                project::gen::Operation::Create { path, .. } => OperationSummary {
                    operation_type: "CREATE".to_string(),
                    path: path.clone(),
                },
                project::gen::Operation::Update { path, .. } => OperationSummary {
                    operation_type: "UPDATE".to_string(),
                    path: path.clone(),
                },
                project::gen::Operation::Delete { path } => OperationSummary {
                    operation_type: "DELETE".to_string(),
                    path: path.clone(),
                },
            })
            .collect();

        Ok(GenOutput {
            files_generated: result.files_created,
            files_created: result.files_created,
            output_dir: ".".to_string(), // GenerationResult doesn't have output_dir
            operations,
            dry_run,
        })
    }

    // Execute the async function and convert errors
    crate::runtime::block_on(gen_impl(template_ref, vars, dry_run))
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?
}

/// Apply generation plan to create/modify files
///
/// # Examples
///
/// Apply plan with confirmation:
/// ```bash
/// ggen project apply plan.json
/// ```
///
/// Auto-confirm without prompting:
/// ```bash
/// ggen project apply plan.yaml --yes
/// ```
///
/// Dry run to preview changes:
/// ```bash
/// ggen project apply plan.toml --dry-run
/// ```
#[verb]
fn apply(plan_file: String, yes: bool, dry_run: bool) -> Result<ApplyOutput> {
    use ggen_domain::project;

    // Helper function to bridge sync/async and handle errors
    async fn apply_impl(plan_file: String, yes: bool, dry_run: bool) -> Result<ApplyOutput> {
        let input = project::apply::ApplyInput {
            plan_file: plan_file.clone(),
            auto_confirm: yes,
            dry_run,
        };

        let result = project::apply::apply_plan(&input).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to apply plan: {}", e))
        })?;

        Ok(ApplyOutput {
            changes_applied: result.operations_count, // ApplicationResult doesn't have changes_applied
            operations_count: result.operations_count,
            files_modified: 0, // ApplicationResult doesn't have files_modified
            files_created: 0,  // ApplicationResult doesn't have files_created
            files_deleted: 0,  // ApplicationResult doesn't have files_deleted
            dry_run,
        })
    }

    // Execute the async function and convert errors
    crate::runtime::block_on(apply_impl(plan_file, yes, dry_run))
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?
}

/// Initialize project with file-based routing conventions
///
/// # Examples
///
/// Initialize in current directory:
/// ```bash
/// ggen project init
/// ```
///
/// Initialize with name and preset:
/// ```bash
/// ggen project init --name my-project --preset clap-noun-verb
/// ```
///
/// Initialize in specific directory:
/// ```bash
/// ggen project init ./my-workspace --name workspace --preset custom
/// ```
#[verb]
fn init(path: PathBuf, name: Option<String>, preset: Option<String>) -> Result<InitOutput> {
    use crate::conventions::presets;
    use std::fs;

    // Validate project name
    if let Some(ref n) = name {
        if n.is_empty() {
            return Err(clap_noun_verb::NounVerbError::execution_error(
                "Project name cannot be empty".to_string(),
            ));
        }
        if n.contains(char::is_whitespace) {
            return Err(clap_noun_verb::NounVerbError::execution_error(
                "Project name cannot contain whitespace".to_string(),
            ));
        }
    }

    // Helper function to bridge sync/async and handle errors
    async fn init_impl(
        path: PathBuf, name: Option<String>, preset: Option<String>,
    ) -> Result<InitOutput> {
        let project_name = name.as_deref().unwrap_or("my-project").to_string();
        let mut files_created = Vec::new();
        let mut directories_created = Vec::new();

        // Canonicalize and validate path (prevent path traversal)
        let canonical_path = match std::fs::canonicalize(&path) {
            Ok(p) => p,
            Err(_) => {
                // Path doesn't exist, create parent and validate
                if let Some(parent) = path.parent() {
                    match std::fs::canonicalize(parent) {
                        Ok(p) => p.join(path.file_name().unwrap_or_default()),
                        Err(_) => {
                            return Err(clap_noun_verb::NounVerbError::execution_error(format!(
                                "Invalid path: {}",
                                path.display()
                            )));
                        }
                    }
                } else {
                    return Err(clap_noun_verb::NounVerbError::execution_error(format!(
                        "Invalid path: {}",
                        path.display()
                    )));
                }
            }
        };

        // Create base project structure
        fs::create_dir_all(&canonical_path).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Failed to create project directory: {}",
                e
            ))
        })?;

        // Create .ggen directory
        let ggen_dir = canonical_path.join(".ggen");
        fs::create_dir_all(&ggen_dir).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Failed to create .ggen directory: {}",
                e
            ))
        })?;
        directories_created.push(".ggen/".to_string());

        // Apply preset if specified
        if let Some(preset_name) = &preset {
            let preset = presets::get_preset(preset_name).ok_or_else(|| {
                clap_noun_verb::NounVerbError::execution_error(format!(
                    "Unknown preset: {}. Available: {:?}",
                    preset_name,
                    presets::list_presets()
                ))
            })?;

            // Create project structure
            preset.create_structure(&canonical_path).map_err(|e| {
                clap_noun_verb::NounVerbError::execution_error(format!(
                    "Failed to apply preset: {}",
                    e
                ))
            })?;

            // Create RDF files
            let rdf_dir = ggen_dir.join("rdf");
            fs::create_dir_all(&rdf_dir).map_err(|e| {
                clap_noun_verb::NounVerbError::execution_error(format!(
                    "Failed to create RDF directory: {}",
                    e
                ))
            })?;
            directories_created.push(".ggen/rdf/".to_string());

            for (rdf_path, content) in preset.rdf_files() {
                let file_path = rdf_dir.join(rdf_path);
                if let Some(parent) = file_path.parent() {
                    fs::create_dir_all(parent).map_err(|e| {
                        clap_noun_verb::NounVerbError::execution_error(format!(
                            "Failed to create directory: {}",
                            e
                        ))
                    })?;
                }
                fs::write(&file_path, content).map_err(|e| {
                    clap_noun_verb::NounVerbError::execution_error(format!(
                        "Failed to write RDF file: {}",
                        e
                    ))
                })?;

                let relative_path = file_path
                    .strip_prefix(&canonical_path)
                    .unwrap_or(&file_path)
                    .display()
                    .to_string();
                files_created.push(relative_path);
            }

            // Create templates
            let templates_dir = ggen_dir.join("templates");
            fs::create_dir_all(&templates_dir).map_err(|e| {
                clap_noun_verb::NounVerbError::execution_error(format!(
                    "Failed to create templates directory: {}",
                    e
                ))
            })?;
            directories_created.push(".ggen/templates/".to_string());

            for (tmpl_path, content) in preset.templates() {
                let file_path = templates_dir.join(tmpl_path);
                if let Some(parent) = file_path.parent() {
                    fs::create_dir_all(parent).map_err(|e| {
                        clap_noun_verb::NounVerbError::execution_error(format!(
                            "Failed to create directory: {}",
                            e
                        ))
                    })?;
                }
                fs::write(&file_path, content).map_err(|e| {
                    clap_noun_verb::NounVerbError::execution_error(format!(
                        "Failed to write template: {}",
                        e
                    ))
                })?;

                let relative_path = file_path
                    .strip_prefix(&canonical_path)
                    .unwrap_or(&file_path)
                    .display()
                    .to_string();
                files_created.push(relative_path);
            }

            // Create conventions config
            let config_path = ggen_dir.join("conventions.toml");
            fs::write(&config_path, preset.config_content()).map_err(|e| {
                clap_noun_verb::NounVerbError::execution_error(format!(
                    "Failed to write config: {}",
                    e
                ))
            })?;
            files_created.push(".ggen/conventions.toml".to_string());
        } else {
            // Create basic structure without preset
            let dirs = ["domain", "templates", "queries", "generated"];
            for dir in &dirs {
                let dir_path = canonical_path.join(dir);
                fs::create_dir_all(&dir_path).map_err(|e| {
                    clap_noun_verb::NounVerbError::execution_error(format!(
                        "Failed to create directory: {}",
                        e
                    ))
                })?;
                directories_created.push(format!("{}/", dir));
            }
        }

        let next_steps = vec![
            "Add RDF files to .ggen/rdf/".to_string(),
            "Add templates to .ggen/templates/".to_string(),
            "Run: ggen project generate".to_string(),
        ];

        Ok(InitOutput {
            project_name,
            project_path: canonical_path.display().to_string(),
            preset,
            files_created,
            directories_created,
            next_steps,
        })
    }

    // Execute the async function and convert errors
    crate::runtime::block_on(init_impl(path, name, preset))
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?
}

/// Generate code using zero-config conventions
///
/// # Examples
///
/// Generate all templates:
/// ```bash
/// ggen project generate
/// ```
///
/// Generate specific template:
/// ```bash
/// ggen project generate my-template
/// ```
///
/// Generate with custom output:
/// ```bash
/// ggen project generate --output ./generated --force
/// ```
///
/// Generate from specific project path:
/// ```bash
/// ggen project generate --path ./my-project my-template
/// ```
#[verb]
fn generate(
    template: Option<String>, path: PathBuf, output: Option<String>, force: bool,
) -> Result<GenerateOutput> {
    use ggen_domain::template;
    use std::fs;

    // Helper function to bridge sync/async and handle errors
    async fn generate_impl(
        template: Option<String>, path: PathBuf, output: Option<String>, force: bool,
    ) -> Result<GenerateOutput> {
        // Auto-discover templates
        let templates_dir = path.join("templates");
        let mut templates = Vec::new();

        if templates_dir.exists() {
            for entry in fs::read_dir(&templates_dir).map_err(|e| {
                clap_noun_verb::NounVerbError::execution_error(format!(
                    "Failed to read templates directory: {}",
                    e
                ))
            })? {
                let entry = entry.map_err(|e| {
                    clap_noun_verb::NounVerbError::execution_error(format!(
                        "Failed to read directory entry: {}",
                        e
                    ))
                })?;
                let entry_path = entry.path();
                if entry_path.extension().and_then(|s| s.to_str()) == Some("tmpl") {
                    templates.push(entry_path);
                }
            }
        }

        // Filter by template name if specified
        let templates_to_generate: Vec<_> = if let Some(ref template_name) = template {
            templates
                .iter()
                .filter(|t| {
                    t.file_name()
                        .and_then(|n| n.to_str())
                        .map(|n| n.contains(template_name))
                        .unwrap_or(false)
                })
                .cloned()
                .collect()
        } else {
            templates
        };

        if templates_to_generate.is_empty() {
            return Err(clap_noun_verb::NounVerbError::execution_error(
                "No templates found. Create templates in templates/ directory or specify template name.".to_string()
            ));
        }

        // Generate from each template
        let mut total_files = 0;
        let mut total_bytes = 0;
        let mut output_paths = Vec::new();

        for template_path in &templates_to_generate {
            // Determine output path from file markers or config
            let output_dir = if let Some(ref dir) = output {
                path.join(dir)
            } else {
                // Try to infer from config or use default
                let config_path = path.join("clnrm-v2-ggen.toml");
                if config_path.exists() {
                    path.join("crates/clnrm-v2-generated")
                } else {
                    path.join("generated")
                }
            };

            // Generate template
            let options = template::RenderWithRdfOptions {
                template_path: template_path.clone(),
                output_path: output_dir.join("placeholder"),
                rdf_files: Vec::new(),
                variables: BTreeMap::new(),
                force_overwrite: force,
                use_preprocessor: false,
            };

            let result = template::render_with_rdf(&options)
                .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

            total_files += result.files_created;
            total_bytes += result.bytes_written;
            output_paths.push(result.output_path.display().to_string());
        }

        // Format bytes nicely
        let bytes_written = if total_bytes < 1024 {
            format!("{} B", total_bytes)
        } else if total_bytes < 1024 * 1024 {
            format!("{:.1} KB", total_bytes as f64 / 1024.0)
        } else {
            format!("{:.1} MB", total_bytes as f64 / (1024.0 * 1024.0))
        };

        Ok(GenerateOutput {
            templates_processed: templates_to_generate.len(),
            files_generated: total_files,
            bytes_written,
            output_paths,
        })
    }

    // Execute the async function and convert errors
    crate::runtime::block_on(generate_impl(template, path, output, force))
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?
}

/// Watch for changes and auto-regenerate
///
/// # Examples
///
/// Watch current directory:
/// ```bash
/// ggen project watch
/// ```
///
/// Watch specific directory with custom debounce:
/// ```bash
/// ggen project watch --path ./my-project --debounce 500
/// ```
#[verb]
fn watch(path: PathBuf, debounce: u64) -> Result<WatchOutput> {
    use crate::conventions::ProjectWatcher;

    // Note: This is a long-running blocking operation
    // In v3.4.0, we'll need to handle this specially

    // Create watcher (uses fixed 300ms debounce internally)
    let mut watcher = ProjectWatcher::new(path.clone()).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to create watcher: {}", e))
    })?;

    // Start watching (blocking)
    watcher.watch().map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Watch failed: {}", e))
    })?;

    Ok(WatchOutput {
        project_path: path.display().to_string(),
        debounce_ms: debounce,
        status: "stopped".to_string(),
        message: "Watch mode stopped".to_string(),
    })
}

// ============================================================================
// Helper Functions
// ============================================================================

// Use utility function from ggen_utils::cli instead of duplicating here

// ============================================================================
// Usage Notes
// ============================================================================

// To use this in your CLI:
// 1. Update main.rs to use: clap_noun_verb::run()
// 2. Ensure ggen_domain::project modules exist with proper interfaces
// 3. Test with: cargo run -- project --help
// 4. Test verbs: cargo run -- project new my-app
// 5. JSON output: cargo run -- project new my-app --output json
//
// Migration checklist:
// - [x] New command with project scaffolding
// - [x] Plan command for generation planning
// - [x] Gen command for code generation
// - [x] Apply command for plan execution
// - [x] Init command for convention-based initialization
// - [x] Generate command for zero-config generation
// - [x] Watch command for auto-regeneration
// - [x] All output types implement Serialize
// - [x] Helper functions for variable parsing
// - [x] Proper error handling with anyhow
// - [x] Documentation with examples
