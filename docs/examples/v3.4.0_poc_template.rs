//! Proof-of-Concept: clap-noun-verb v3.4.0 Migration for Template Commands
//!
//! This file demonstrates the complete v3.4.0 pattern for template commands.
//! Use this as a reference when migrating other command groups.

use clap_noun_verb_macros::verb;
use clap_noun_verb::Result;
use serde::Serialize;
use std::path::PathBuf;
use std::collections::HashMap;

// ============================================================================
// Output Types (all must derive Serialize for JSON output)
// ============================================================================

#[derive(Serialize)]
struct GenerateOutput {
    output_path: String,
    bytes_written: usize,
    files_created: usize,
    rdf_files_loaded: usize,
    sparql_queries_executed: usize,
}

#[derive(Serialize)]
struct GenerateTreeOutput {
    output_dir: String,
    files_created: usize,
}

#[derive(Serialize)]
struct LintOutput {
    template: String,
    errors: usize,
    warnings: usize,
    passed: bool,
}

#[derive(Serialize)]
struct TemplateInfo {
    name: String,
    path: String,
    description: Option<String>,
    source: String,
}

#[derive(Serialize)]
struct ListOutput {
    templates: Vec<TemplateInfo>,
    count: usize,
}

#[derive(Serialize)]
struct NewOutput {
    name: String,
    path: String,
    template_type: String,
}

#[derive(Serialize)]
struct ShowOutput {
    name: String,
    path: String,
    description: Option<String>,
    variables: Vec<String>,
    rdf_sources: Vec<String>,
    sparql_queries: usize,
}

#[derive(Serialize)]
struct GenerateRdfOutput {
    output_dir: String,
    files_generated: usize,
    project_name: String,
}

// ============================================================================
// Verb Functions (the actual CLI commands)
// ============================================================================

/// Generate code from template with RDF/SPARQL support
///
/// # Examples
///
/// Basic generation:
/// ```bash
/// ggen template generate --template foo.tmpl --output output.txt
/// ```
///
/// With RDF context:
/// ```bash
/// ggen template generate --template foo.tmpl --rdf ontology.ttl --output result.txt
/// ```
///
/// With variables:
/// ```bash
/// ggen template generate --template foo.tmpl --var name=value --var key=data
/// ```
#[verb] // Verb "generate", noun "template" auto-inferred from filename
fn generate(
    /// Template file path
    #[arg(short = 't', long)]
    template: Option<PathBuf>,

    /// RDF/TTL file(s) for context (can be specified multiple times)
    #[arg(short = 'r', long)]
    rdf: Vec<PathBuf>,

    /// Output path
    #[arg(short = 'o', long)]
    output: Option<PathBuf>,

    /// Variables in key=value format (can be specified multiple times)
    /// Example: --var name=John --var age=30
    #[arg(short = 'v', long)]
    var: Vec<String>,

    /// Force overwrite existing files
    #[arg(short = 'f', long)]
    force: bool,

    /// Enable preprocessor for advanced template processing
    #[arg(long)]
    preprocessor: bool,

    /// Generate template FROM RDF metadata (reverse operation)
    #[arg(long)]
    from_rdf: bool,
) -> Result<GenerateOutput> {
    use ggen_domain::template;

    // Parse variables
    let variables = parse_variables(&var)?;

    // Handle reverse operation
    if from_rdf {
        if rdf.is_empty() {
            return Err(anyhow::anyhow!("No RDF files specified. Use --rdf to specify RDF metadata files.").into());
        }

        let output_template = template.unwrap_or_else(|| PathBuf::from("generated.tmpl"));
        let result_path = template::generate_from_rdf(rdf.clone(), output_template)?;

        return Ok(GenerateOutput {
            output_path: result_path.display().to_string(),
            bytes_written: 0, // TODO: track
            files_created: 1,
            rdf_files_loaded: rdf.len(),
            sparql_queries_executed: 0,
        });
    }

    // Regular generation with RDF integration
    if !rdf.is_empty() {
        let options = template::RenderWithRdfOptions {
            template_path: template.unwrap_or_else(|| PathBuf::from("template.tmpl")),
            output_path: output.unwrap_or_else(|| PathBuf::from("output")),
            rdf_files: rdf.clone(),
            variables,
            force_overwrite: force,
            use_preprocessor: preprocessor,
        };

        let result = template::render_with_rdf(&options)?;

        Ok(GenerateOutput {
            output_path: result.output_path.display().to_string(),
            bytes_written: result.bytes_written,
            files_created: result.files_created,
            rdf_files_loaded: result.rdf_files_loaded,
            sparql_queries_executed: result.sparql_queries_executed,
        })
    } else {
        // v1 API: backward compatible
        let options = template::GenerateFileOptions {
            template_path: template.unwrap_or_else(|| PathBuf::from("template.tmpl")),
            output_path: output.unwrap_or_else(|| PathBuf::from("output")),
            variables,
            force_overwrite: force,
        };

        let result = template::generate_file(&options)?;

        Ok(GenerateOutput {
            output_path: result.output_path.display().to_string(),
            bytes_written: result.bytes_written,
            files_created: 1,
            rdf_files_loaded: 0,
            sparql_queries_executed: 0,
        })
    }
}

/// Generate CLI project from RDF/TTL ontology
///
/// # Examples
///
/// ```bash
/// ggen template generate-rdf my-cli.ttl --output ./my-cli
/// ```
#[verb("generate-rdf")] // Explicit verb name (has hyphen)
fn generate_rdf(
    /// Path to TTL file containing CLI definition
    #[arg(index = 0)] // Positional argument
    ttl_file: PathBuf,

    /// Output directory for generated project
    #[arg(short = 'o', long, default_value = ".")]
    output: PathBuf,

    /// Template directory containing rendering templates
    #[arg(long, default_value = "examples/clap-noun-verb-demo/templates")]
    templates: PathBuf,
) -> Result<GenerateRdfOutput> {
    use ggen_domain::template::generate_rdf;

    let options = generate_rdf::GenerateFromRdfOptions::new(
        ttl_file,
        output.clone(),
        templates,
    );

    let result = generate_rdf::generate_cli_from_rdf(&options)?;

    Ok(GenerateRdfOutput {
        output_dir: result.output_dir.display().to_string(),
        files_generated: result.files_generated,
        project_name: result.project_name,
    })
}

/// Generate file tree from template directory
///
/// # Examples
///
/// ```bash
/// ggen template generate-tree --template ./templates --output ./output
/// ```
#[verb("generate-tree")]
fn generate_tree(
    /// Template directory
    #[arg(short = 't', long)]
    template: PathBuf,

    /// Output directory
    #[arg(short = 'o', long)]
    output: PathBuf,
) -> Result<GenerateTreeOutput> {
    use ggen_domain::template::generate_tree;

    crate::runtime::execute(async move {
        let variables: HashMap<String, String> = HashMap::new();

        let result = generate_tree::generate_file_tree(
            &template,
            &output,
            &variables,
            false,
        )?;

        Ok(GenerateTreeOutput {
            output_dir: output.display().to_string(),
            files_created: result.files_created,
        })
    })
}

/// Lint and validate template syntax
///
/// # Examples
///
/// ```bash
/// ggen template lint my-template.tmpl
/// ```
#[verb]
fn lint(
    /// Template file to lint
    #[arg(index = 0)] // Positional argument
    template: PathBuf,
) -> Result<LintOutput> {
    use ggen_domain::template::lint;

    crate::runtime::execute(async move {
        let options = lint::LintOptions {
            check_sparql: false,
            check_schema: false,
        };

        let report = lint::lint_template(&template.to_string_lossy(), &options)?;

        Ok(LintOutput {
            template: template.display().to_string(),
            errors: report.errors.len(),
            warnings: report.warnings.len(),
            passed: !report.has_errors(),
        })
    })
}

/// List available templates
///
/// # Examples
///
/// ```bash
/// ggen template list
/// ggen template list --directory ./my-templates
/// ```
#[verb]
fn list(
    /// Template directory to search
    #[arg(short = 'd', long)]
    directory: Option<PathBuf>,
) -> Result<ListOutput> {
    use ggen_domain::template::list;

    crate::runtime::execute(async move {
        let filters = list::ListFilters {
            pattern: None,
            local_only: false,
            gpack_only: false,
        };

        let default_dir = PathBuf::from("templates");
        let templates_dir = directory.as_ref().unwrap_or(&default_dir);
        let templates = list::list_templates(templates_dir, &filters)?;

        let template_info: Vec<TemplateInfo> = templates.into_iter().map(|t| {
            TemplateInfo {
                name: t.name,
                path: t.path,
                description: t.description,
                source: match t.source {
                    list::TemplateSource::Local => "local".to_string(),
                    list::TemplateSource::Gpack(name) => name,
                },
            }
        }).collect();

        let count = template_info.len();

        Ok(ListOutput {
            templates: template_info,
            count,
        })
    })
}

/// Create a new template from scratch
///
/// # Examples
///
/// ```bash
/// ggen template new my-template
/// ggen template new my-template --template-type rust
/// ```
#[verb]
fn new(
    /// Template name
    #[arg(index = 0)] // Positional argument
    name: String,

    /// Template type (generic, rust, python, etc.)
    #[arg(short = 't', long)]
    template_type: Option<String>,
) -> Result<NewOutput> {
    use ggen_domain::template::new as template_new;
    use ggen_domain::template::TemplateService;

    crate::runtime::execute(async move {
        let template_type_val = template_type.as_deref().unwrap_or("generic");
        let content = template_new::generate_template_content(&name, template_type_val)?;

        let service = TemplateService::default_instance();
        let path = service.write_template(&name, &content)?;

        Ok(NewOutput {
            name,
            path: path.display().to_string(),
            template_type: template_type_val.to_string(),
        })
    })
}

/// Show template metadata and details
///
/// # Examples
///
/// ```bash
/// ggen template show my-template
/// ```
#[verb]
fn show(
    /// Template name or path
    #[arg(index = 0)] // Positional argument
    template: String,
) -> Result<ShowOutput> {
    use ggen_domain::template::show;

    crate::runtime::execute(async move {
        let metadata = show::show_template_metadata(&template)?;

        Ok(ShowOutput {
            name: metadata.name,
            path: metadata.path,
            description: metadata.description,
            variables: metadata.variables,
            rdf_sources: metadata.rdf_sources,
            sparql_queries: metadata.sparql_queries.len(),
        })
    })
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Parse variable arguments from key=value format
fn parse_variables(vars: &[String]) -> anyhow::Result<HashMap<String, String>> {
    let mut map = HashMap::new();
    for var in vars {
        if let Some((key, value)) = var.split_once('=') {
            map.insert(key.to_string(), value.to_string());
        } else {
            return Err(anyhow::anyhow!(
                "Invalid variable format: {}. Expected key=value",
                var
            ));
        }
    }
    Ok(map)
}

// ============================================================================
// Usage Notes
// ============================================================================

// To use this in your CLI:
// 1. Replace the existing template.rs with this file
// 2. Update main.rs to use: clap_noun_verb::run()
// 3. Ensure all domain functions return proper types
// 4. Test with: cargo run -- template --help
// 5. JSON output: cargo run -- template generate --template foo.tmpl
