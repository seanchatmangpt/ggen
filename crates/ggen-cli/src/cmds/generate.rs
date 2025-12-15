//! Generate Commands - N3/CONSTRUCT Semantic Code Generator
//!
//! This module implements the `ggen generate` command for semantic code generation.
//!
//! ## Architecture: Three-Layer Pattern
//!
//! - **Layer 3 (CLI)**: Input validation, output formatting, thin routing
//! - **Layer 2 (Integration)**: Async execution, error handling
//! - **Layer 1 (Domain)**: Pure generation logic from ggen_core::codegen

// Standard library imports
use std::path::PathBuf;

// External crate imports
use clap_noun_verb::Result as VerbResult;
use clap_noun_verb_macros::verb;
use serde::Serialize;

// Local crate imports
use crate::cmds::helpers::execute_async_op;

// ============================================================================
// Output Types (all must derive Serialize for JSON output)
// ============================================================================

/// Output for the generate run command
#[derive(Serialize)]
struct GenerateRunOutput {
    /// Project name from manifest
    project_name: String,
    /// Number of inference rules executed
    inference_rules_executed: usize,
    /// Number of generation rules executed
    generation_rules_executed: usize,
    /// Total files generated
    files_generated: usize,
    /// Output directory
    output_dir: String,
    /// Audit trail path (if enabled)
    audit_trail: Option<String>,
    /// Overall status
    status: String,
}

/// Output for validate command
#[derive(Serialize)]
struct ValidateOutput {
    /// Whether manifest is valid
    is_valid: bool,
    /// Manifest path
    manifest_path: String,
    /// Validation messages
    messages: Vec<String>,
}

/// Output for dry-run command
#[derive(Serialize)]
struct DryRunOutput {
    /// Files that would be generated
    files_to_generate: Vec<String>,
    /// Inference rules that would be executed
    inference_rules: Vec<String>,
    /// Generation rules that would be executed
    generation_rules: Vec<String>,
    /// Estimated operation count
    estimated_operations: usize,
}

// ============================================================================
// Verb Functions (the actual CLI commands)
// ============================================================================

/// Run the semantic code generation pipeline
///
/// Loads ggen.toml manifest, executes CONSTRUCT inference rules to enrich
/// the ontology graph, then generates code via Tera templates.
///
/// Exit codes:
///   0 - Success
///   1 - Validation error
///   2 - SPARQL error
///   3 - Template error
///   4 - Output error
///   5 - Timeout
///
/// Examples:
///   ggen generate run
///   ggen generate run --manifest custom.toml
///   ggen generate run --output-dir src/generated
///   ggen generate run --audit  # Generate audit.json
#[verb("run", "generate")]
fn run(
    manifest: Option<String>,
    output_dir: Option<String>,
    audit: Option<bool>,
    dry_run: Option<bool>,
) -> VerbResult<GenerateRunOutput> {
    use ggen_core::codegen::GenerationPipeline;
    use ggen_core::manifest::{ManifestParser, ManifestValidator};

    // Determine manifest path
    let manifest_path = PathBuf::from(manifest.unwrap_or_else(|| "ggen.toml".to_string()));

    // Validate manifest exists
    if !manifest_path.exists() {
        return Err(clap_noun_verb::NounVerbError::execution_error(format!(
            "Manifest not found: {}. Run `ggen generate init` to create one.",
            manifest_path.display()
        )));
    }

    let result = execute_async_op("generate_run", async move {
        // Parse manifest
        let manifest_data = ManifestParser::parse(&manifest_path).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Failed to parse manifest: {}",
                e
            ))
        })?;

        // Validate manifest
        let base_path = manifest_path.parent().unwrap_or(std::path::Path::new("."));
        let validator = ManifestValidator::new(&manifest_data, base_path);
        validator.validate().map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Manifest validation failed: {}",
                e
            ))
        })?;

        let project_name = manifest_data.project.name.clone();
        let output_directory = output_dir
            .map(PathBuf::from)
            .unwrap_or_else(|| manifest_data.generation.output_dir.clone());

        // Check for dry-run mode
        if dry_run.unwrap_or(false) {
            let inference_count = manifest_data.inference.rules.len();
            let generation_count = manifest_data.generation.rules.len();

            return Ok(GenerateRunOutput {
                project_name,
                inference_rules_executed: 0,
                generation_rules_executed: 0,
                files_generated: 0,
                output_dir: output_directory.display().to_string(),
                audit_trail: None,
                status: format!(
                    "DRY RUN: Would execute {} inference rules, {} generation rules",
                    inference_count, generation_count
                ),
            });
        }

        // Create and run the generation pipeline
        let mut pipeline = GenerationPipeline::new(manifest_data.clone(), base_path.to_path_buf());

        let state = pipeline.run().map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Pipeline execution failed: {}",
                e
            ))
        })?;

        // Determine audit trail path
        let audit_path = if audit.unwrap_or(manifest_data.generation.require_audit_trail) {
            Some(
                output_directory
                    .join("audit.json")
                    .display()
                    .to_string(),
            )
        } else {
            None
        };

        Ok(GenerateRunOutput {
            project_name,
            inference_rules_executed: state
                .executed_rules
                .iter()
                .filter(|r| r.rule_type == ggen_core::codegen::RuleType::Inference)
                .count(),
            generation_rules_executed: state
                .executed_rules
                .iter()
                .filter(|r| r.rule_type == ggen_core::codegen::RuleType::Generation)
                .count(),
            files_generated: state.generated_files.len(),
            output_dir: output_directory.display().to_string(),
            audit_trail: audit_path,
            status: "SUCCESS".to_string(),
        })
    })?;

    Ok(result)
}

/// Initialize a new ggen.toml manifest for semantic code generation
///
/// Creates a minimal ggen.toml with project configuration, ontology settings,
/// and example generation rules.
///
/// Examples:
///   ggen generate init
///   ggen generate init --name my-domain
///   ggen generate init --ontology domain/model.ttl
#[verb("init", "generate")]
fn init(name: Option<String>, ontology: Option<String>) -> VerbResult<ValidateOutput> {
    let project_name = name.unwrap_or_else(|| {
        std::env::current_dir()
            .ok()
            .and_then(|p| p.file_name().map(|s| s.to_string_lossy().to_string()))
            .unwrap_or_else(|| "my-project".to_string())
    });

    let ontology_path = ontology.unwrap_or_else(|| "domain/model.ttl".to_string());

    let manifest_content = format!(
        r#"# ggen.toml - Semantic Code Generation Manifest
# Generated by: ggen generate init

[project]
name = "{}"
version = "1.0.0"
description = "Generated from RDF ontology"

[ontology]
source = "{}"
imports = []
base_iri = "http://example.org/"

[ontology.prefixes]
rdfs = "http://www.w3.org/2000/01/rdf-schema#"
code = "http://ggen.dev/code#"

# Inference rules - CONSTRUCT queries that enrich the ontology
[[inference.rules]]
name = "auditable_fields"
description = "Add created_at/updated_at to auditable entities"
order = 1
construct = """
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX code: <http://ggen.dev/code#>
CONSTRUCT {{
    ?class code:hasField ?createdField .
    ?createdField code:fieldName "created_at" ;
                  code:fieldType "DateTime<Utc>" .
}}
WHERE {{
    ?class a rdfs:Class ;
           code:auditable true .
    BIND(IRI(CONCAT(STR(?class), "_created_at")) AS ?createdField)
}}
"""

# Generation rules - SELECT queries + Tera templates
[[generation.rules]]
name = "structs"
query = {{ inline = """
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX code: <http://ggen.dev/code#>
SELECT ?name ?iri ?derives ?docstring
WHERE {{
    ?iri a rdfs:Class ;
         rdfs:label ?name .
    OPTIONAL {{ ?iri code:derives ?derives }}
    OPTIONAL {{ ?iri rdfs:comment ?docstring }}
}}
ORDER BY ?name
""" }}
template = {{ file = "templates/struct.tera" }}
output_file = "src/models/{{{{name | snake_case}}}}.rs"
skip_empty = true

[generation]
output_dir = "src/generated"
max_sparql_timeout_ms = 5000
require_audit_trail = true

[validation]
validate_syntax = true
no_unsafe = true
"#,
        project_name, ontology_path
    );

    let manifest_path = PathBuf::from("ggen.toml");

    if manifest_path.exists() {
        return Err(clap_noun_verb::NounVerbError::execution_error(
            "ggen.toml already exists. Remove it first or use a different directory.",
        ));
    }

    std::fs::write(&manifest_path, manifest_content).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!(
            "Failed to write manifest: {}",
            e
        ))
    })?;

    // Create directories
    std::fs::create_dir_all("domain").ok();
    std::fs::create_dir_all("templates").ok();
    std::fs::create_dir_all("src/generated").ok();

    Ok(ValidateOutput {
        is_valid: true,
        manifest_path: manifest_path.display().to_string(),
        messages: vec![
            format!("Created ggen.toml for project '{}'", project_name),
            format!("Ontology source set to: {}", ontology_path),
            "Created directories: domain/, templates/, src/generated/".to_string(),
            "Next: Create your ontology file and run `ggen generate run`".to_string(),
        ],
    })
}

/// Validate a ggen.toml manifest without running generation
///
/// Checks manifest syntax, required fields, and referenced file paths.
///
/// Examples:
///   ggen generate validate
///   ggen generate validate --manifest custom.toml
#[verb("validate", "generate")]
fn validate(manifest: Option<String>) -> VerbResult<ValidateOutput> {
    use ggen_core::manifest::{ManifestParser, ManifestValidator};

    let manifest_path = PathBuf::from(manifest.unwrap_or_else(|| "ggen.toml".to_string()));

    if !manifest_path.exists() {
        return Err(clap_noun_verb::NounVerbError::execution_error(format!(
            "Manifest not found: {}",
            manifest_path.display()
        )));
    }

    let mut messages = Vec::new();

    // Parse manifest
    let manifest_data = match ManifestParser::parse(&manifest_path) {
        Ok(m) => m,
        Err(e) => {
            return Ok(ValidateOutput {
                is_valid: false,
                manifest_path: manifest_path.display().to_string(),
                messages: vec![format!("Parse error: {}", e)],
            });
        }
    };

    messages.push(format!("Project: {}", manifest_data.project.name));
    messages.push(format!("Version: {}", manifest_data.project.version));
    messages.push(format!(
        "Ontology: {}",
        manifest_data.ontology.source.display()
    ));
    messages.push(format!(
        "Inference rules: {}",
        manifest_data.inference.rules.len()
    ));
    messages.push(format!(
        "Generation rules: {}",
        manifest_data.generation.rules.len()
    ));

    // Validate manifest
    let base_path = manifest_path.parent().unwrap_or(std::path::Path::new("."));
    let validator = ManifestValidator::new(&manifest_data, base_path);

    match validator.validate() {
        Ok(()) => {
            messages.push("All validations passed".to_string());
            Ok(ValidateOutput {
                is_valid: true,
                manifest_path: manifest_path.display().to_string(),
                messages,
            })
        }
        Err(e) => {
            messages.push(format!("Validation failed: {}", e));
            Ok(ValidateOutput {
                is_valid: false,
                manifest_path: manifest_path.display().to_string(),
                messages,
            })
        }
    }
}

/// Show what would be generated without executing (dry run)
///
/// Analyzes the manifest and shows all operations that would be performed.
///
/// Examples:
///   ggen generate plan
///   ggen generate plan --manifest custom.toml
#[verb("plan", "generate")]
fn plan(manifest: Option<String>) -> VerbResult<DryRunOutput> {
    use ggen_core::manifest::ManifestParser;

    let manifest_path = PathBuf::from(manifest.unwrap_or_else(|| "ggen.toml".to_string()));

    if !manifest_path.exists() {
        return Err(clap_noun_verb::NounVerbError::execution_error(format!(
            "Manifest not found: {}",
            manifest_path.display()
        )));
    }

    let manifest_data = ManifestParser::parse(&manifest_path).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to parse manifest: {}", e))
    })?;

    let inference_rules: Vec<String> = manifest_data
        .inference
        .rules
        .iter()
        .map(|r| format!("{} (order: {})", r.name, r.order))
        .collect();

    let generation_rules: Vec<String> = manifest_data
        .generation
        .rules
        .iter()
        .map(|r| format!("{} -> {}", r.name, r.output_file))
        .collect();

    let files_to_generate: Vec<String> = manifest_data
        .generation
        .rules
        .iter()
        .map(|r| r.output_file.clone())
        .collect();

    let estimated_operations =
        1 + // Load ontology
        manifest_data.inference.rules.len() + // Inference rules
        manifest_data.generation.rules.len() + // Generation rules
        manifest_data.generation.rules.len(); // File writes

    Ok(DryRunOutput {
        files_to_generate,
        inference_rules,
        generation_rules,
        estimated_operations,
    })
}
