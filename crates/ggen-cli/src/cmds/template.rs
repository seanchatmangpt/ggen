//! Template Commands - clap-noun-verb v5.3.0 Migration
//!
//! This module implements template commands using the v5.3.0 #[verb(\"verb\", \"noun\")] pattern.
//!
//! ## Architecture: Three-Layer Pattern
//!
//! - **Layer 3 (CLI)**: Input validation, output formatting, thin routing
//! - **Layer 2 (Integration)**: Async coordination, resource management (via ggen_domain)
//! - **Layer 1 (Domain)**: Pure business logic from ggen_domain::template

// Standard library imports
use std::path::PathBuf;

// External crate imports
use clap_noun_verb::Result as NounVerbResult;
use clap_noun_verb_macros::verb;
use serde::Serialize;

// ============================================================================
// Output Types
// ============================================================================

#[derive(Serialize)]
struct ShowOutput {
    name: String,
    path: String,
    description: Option<String>,
    output_path: Option<String>,
    variables: Vec<String>,
    rdf_sources: Vec<String>,
    sparql_queries_count: usize,
    determinism_seed: Option<u64>,
}

#[derive(Serialize)]
struct NewOutput {
    template_name: String,
    template_type: String,
    path: String,
}

#[derive(Serialize)]
struct ListOutput {
    templates: Vec<TemplateInfo>,
    total: usize,
    directory: String,
}

#[derive(Serialize)]
struct TemplateInfo {
    name: String,
    source: String,
    description: Option<String>,
    path: String,
}

#[derive(Serialize)]
struct LintOutput {
    has_errors: bool,
    has_warnings: bool,
    errors: Vec<LintMessage>,
    warnings: Vec<LintMessage>,
}

#[derive(Serialize)]
struct LintMessage {
    line: Option<usize>,
    message: String,
}

#[derive(Serialize)]
struct GenerateOutput {
    output_path: String,
    files_created: usize,
    bytes_written: usize,
    rdf_files_loaded: usize,
    sparql_queries_executed: usize,
}

#[derive(Serialize)]
struct GenerateTreeOutput {
    output_directory: String,
}

// ============================================================================
// Verb Functions
// ============================================================================

/// Show template metadata
#[verb("show", "template")]
fn show(template: String) -> NounVerbResult<ShowOutput> {
    use ggen_domain::template::show;

    let metadata = show::show_template_metadata(&template).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to show template: {}", e))
    })?;

    Ok(ShowOutput {
        name: metadata.name,
        path: metadata.path,
        description: metadata.description,
        output_path: metadata.output_path,
        variables: metadata.variables,
        rdf_sources: metadata.rdf_sources,
        sparql_queries_count: metadata.sparql_queries.len(),
        determinism_seed: metadata.determinism_seed,
    })
}

/// Alias for show - common CLI pattern
#[verb("get", "template")]
fn get(template: String) -> NounVerbResult<ShowOutput> {
    show(template)
}

/// Create new template
#[verb("new", "template")]
fn new(name: String, template_type: Option<String>) -> NounVerbResult<NewOutput> {
    use ggen_domain::template::new as template_new;
    use ggen_domain::template::TemplateService;

    let template_type_str = template_type.as_deref().unwrap_or("generic");

    let content =
        template_new::generate_template_content(&name, template_type_str).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Failed to generate template: {}",
                e
            ))
        })?;

    let service = TemplateService::default_instance();
    let path = service.write_template(&name, &content).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to write template: {}", e))
    })?;

    Ok(NewOutput {
        template_name: name,
        template_type: template_type_str.to_string(),
        path: path.display().to_string(),
    })
}

/// List templates
#[verb("list", "template")]
fn list(directory: Option<PathBuf>) -> NounVerbResult<ListOutput> {
    use ggen_domain::template::list;

    let default_dir = PathBuf::from("templates");
    let templates_dir = directory.as_ref().unwrap_or(&default_dir);

    let filters = list::ListFilters {
        pattern: None,
        local_only: false,
        gpack_only: false,
    };

    let templates = list::list_templates(templates_dir, &filters).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to list templates: {}", e))
    })?;

    let template_infos = templates
        .into_iter()
        .map(|t| TemplateInfo {
            name: t.name,
            source: match t.source {
                list::TemplateSource::Local => "local".to_string(),
                list::TemplateSource::Gpack(name) => name,
            },
            description: t.description,
            path: t.path,
        })
        .collect::<Vec<_>>();

    let total = template_infos.len();

    Ok(ListOutput {
        templates: template_infos,
        total,
        directory: templates_dir.display().to_string(),
    })
}

/// Lint a template
#[verb("lint", "template")]
fn lint(template: String) -> NounVerbResult<LintOutput> {
    use ggen_domain::template::lint;

    let options = lint::LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    let report = lint::lint_template(&template, &options).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to lint template: {}", e))
    })?;

    let has_errors = report.has_errors();
    let has_warnings = report.has_warnings();

    let errors = report
        .errors
        .into_iter()
        .map(|e| LintMessage {
            line: e.line,
            message: e.message,
        })
        .collect();

    let warnings = report
        .warnings
        .into_iter()
        .map(|w| LintMessage {
            line: w.line,
            message: w.message,
        })
        .collect();

    Ok(LintOutput {
        has_errors,
        has_warnings,
        errors,
        warnings,
    })
}

/// Check if template content has RDF/SPARQL frontmatter
///
/// Returns true if the template contains `rdf:`, `rdf_inline:`, or `sparql:` in frontmatter.
/// This is a quick check to determine if we need the full RDF rendering path.
fn has_rdf_frontmatter(template_content: &str) -> bool {
    // Quick check for RDF-related fields in frontmatter
    // We look for lines that start with these field names followed by colon
    let frontmatter_end = template_content.find("\n---");
    let check_region = if let Some(end) = frontmatter_end {
        &template_content[..end]
    } else {
        template_content
    };

    check_region.contains("rdf:")
        || check_region.contains("rdf_inline:")
        || check_region.contains("sparql:")
}

/// Generate from template (basic version without Vec support)
#[verb("generate", "template")]
fn generate(
    template: Option<String>, output: Option<String>, vars: Option<String>, force: bool,
) -> NounVerbResult<GenerateOutput> {
    use ggen_domain::template;
    use ggen_domain::template::generate::parse_variables;
    use std::collections::BTreeMap;

    let template_path = template
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("template.tmpl"));
    let output_path = output
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("output"));

    // Parse variables from comma-separated key=value pairs
    let variables = if let Some(vars_str) = vars {
        let var_strings: Vec<String> = vars_str
            .split(',')
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty())
            .collect();
        parse_variables(&var_strings).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Invalid variables: {}", e))
        })?
    } else {
        BTreeMap::new()
    };

    // Read template content to detect RDF/SPARQL frontmatter
    let template_content = std::fs::read_to_string(&template_path).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!(
            "Failed to read template '{}': {}",
            template_path.display(),
            e
        ))
    })?;

    // Route to appropriate rendering path based on frontmatter
    if has_rdf_frontmatter(&template_content) {
        // Use RDF-aware rendering path
        let options =
            template::RenderWithRdfOptions::new(template_path, output_path).with_vars(variables);
        let options = if force { options.force() } else { options };

        let result = template::render_with_rdf(&options).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to generate: {}", e))
        })?;

        Ok(GenerateOutput {
            output_path: result.output_path.display().to_string(),
            files_created: result.files_created,
            bytes_written: result.bytes_written,
            rdf_files_loaded: result.rdf_files_loaded,
            sparql_queries_executed: result.sparql_queries_executed,
        })
    } else {
        // Use fast path without RDF processing
        let options = template::GenerateFileOptions {
            template_path,
            output_path,
            variables,
            force_overwrite: force,
        };

        let result = template::generate_file(&options).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to generate: {}", e))
        })?;

        Ok(GenerateOutput {
            output_path: result.output_path.display().to_string(),
            files_created: 1,
            bytes_written: result.bytes_written,
            rdf_files_loaded: 0,
            sparql_queries_executed: 0,
        })
    }
}

/// Generate file tree from template
#[verb("generate-tree", "template")]
fn generate_tree(
    template: Option<String>, output: Option<String>,
) -> NounVerbResult<GenerateTreeOutput> {
    use ggen_domain::template::generate_tree;
    use std::collections::HashMap;

    let template_path = template.ok_or_else(|| {
        clap_noun_verb::NounVerbError::argument_error("Template path required for generate-tree")
    })?;

    let output_path = output.ok_or_else(|| {
        clap_noun_verb::NounVerbError::argument_error("Output path required for generate-tree")
    })?;

    let template_pb = PathBuf::from(&template_path);
    let output_pb = PathBuf::from(&output_path);

    // For now, use empty variables - full implementation needs var support
    let variables: HashMap<String, String> = HashMap::new();

    generate_tree::generate_file_tree(&template_pb, &output_pb, &variables, false).map_err(
        |e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Failed to generate file tree: {}",
                e
            ))
        },
    )?;

    Ok(GenerateTreeOutput {
        output_directory: output_path,
    })
}

/// Regenerate from template
#[verb("regenerate", "template")]
fn regenerate(template: Option<String>) -> NounVerbResult<GenerateTreeOutput> {
    let template_path = template.unwrap_or_else(|| "template.tmpl".to_string());

    // For now, return placeholder - merge strategies not yet fully implemented
    Ok(GenerateTreeOutput {
        output_directory: template_path,
    })
}
// ============================================================================
// Helper Functions
// ============================================================================

// Use utility function from ggen_utils::cli instead of duplicating here
