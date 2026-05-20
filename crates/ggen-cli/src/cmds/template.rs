//! Template Commands - clap-noun-verb v3.4.0 Migration
//!
//! This module implements template commands using the v3.4.0 #[verb] pattern.

use clap_noun_verb::Result as NounVerbResult;
use clap_noun_verb_macros::verb;
use serde::Serialize;
use std::path::PathBuf;

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

// ============================================================================
// Verb Functions
// ============================================================================

/// Show template metadata
#[verb]
fn show(template: String) -> NounVerbResult<ShowOutput> {
    use ggen_core::domain::template::show;

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

/// Create new template
#[verb]
fn new(name: String, template_type: Option<String>) -> NounVerbResult<NewOutput> {
    use ggen_core::domain::template::new as template_new;
    use ggen_core::domain::template::TemplateService;

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
#[verb]
fn list(directory: Option<PathBuf>) -> NounVerbResult<ListOutput> {
    use ggen_core::domain::template::list;

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
#[verb]
fn lint(template: String) -> NounVerbResult<LintOutput> {
    use ggen_core::domain::template::lint;

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

// ============================================================================
// Helper Functions
// ============================================================================

// Use utility function from ggen_core::utils::cli instead of duplicating here
