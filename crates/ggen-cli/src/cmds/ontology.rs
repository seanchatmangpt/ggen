//! Ontology Commands - Wired to Domain Layer
//!
//! This module provides ontology management commands wired to the domain layer.

use crate::runtime::block_on;
use clap_noun_verb::Result as VerbResult;
use clap_noun_verb_macros::verb;
use ggen_core::domain::ontology;
use serde::Serialize;
use std::path::PathBuf;

// ============================================================================
// Output Types
// ============================================================================

#[derive(Serialize)]
struct GenerateOutput {
    language: String,
    files_generated: usize,
    output_directory: String,
    primary_file: String,
}

#[derive(Serialize)]
struct ValidateOutput {
    is_valid: bool,
    classes_count: usize,
    properties_count: usize,
    warnings: Vec<String>,
    errors: Vec<String>,
}

#[derive(Serialize)]
struct InitOutput {
    project_name: String,
    ontology_file: String,
    config_file: String,
    generated_files: Vec<String>,
}

// ============================================================================
// Verb Functions
// ============================================================================

/// Generate code from ontology schema
#[verb]
fn generate(
    schema_file: String, language: String, output: Option<String>, zod: bool, utilities: bool,
) -> VerbResult<GenerateOutput> {
    let schema_path = PathBuf::from(&schema_file);
    let output_dir = PathBuf::from(output.clone().unwrap_or_else(|| ".".to_string()));

    let (files_generated, primary_file) = block_on(async {
        // First extract schema
        let schema = ontology::extract_ontology_schema(&schema_path, "http://example.org#")
            .await
            .map_err(|e| {
                ggen_core::utils::error::Error::new(&format!("Extraction failed: {}", e))
            })?;

        // Then generate code
        ontology::generate_code_from_ontology(&schema, &language, &output_dir, zod, utilities).await
    })
    .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Runtime error: {}", e)))?
    .map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Generation failed: {}", e))
    })?;

    Ok(GenerateOutput {
        language,
        files_generated,
        output_directory: output_dir.to_string_lossy().to_string(),
        primary_file,
    })
}

/// Validate ontology schema quality
#[verb]
fn validate(schema_file: String, strict: bool) -> VerbResult<ValidateOutput> {
    let schema_path = PathBuf::from(&schema_file);

    let (is_valid, warnings, errors, classes_count, properties_count) = block_on(async {
        let schema = ontology::extract_ontology_schema(&schema_path, "http://example.org#")
            .await
            .map_err(|e| {
                ggen_core::utils::error::Error::new(&format!("Extraction failed: {}", e))
            })?;

        let (valid, warnings, errors) = ontology::validate_ontology_schema(&schema, strict).await?;
        Ok((
            valid,
            warnings,
            errors,
            schema.classes.len(),
            schema.properties.len(),
        ))
    })
    .map_err(|e: ggen_core::utils::Error| {
        clap_noun_verb::NounVerbError::execution_error(format!("Runtime error: {}", e))
    })?
    .map_err(|e: ggen_core::utils::Error| {
        clap_noun_verb::NounVerbError::execution_error(format!("Validation failed: {}", e))
    })?;

    Ok(ValidateOutput {
        is_valid,
        classes_count,
        properties_count,
        warnings,
        errors,
    })
}

/// Initialize ontology project
#[verb]
fn init(project_name: String, template: Option<String>) -> VerbResult<InitOutput> {
    let (ontology_file, config_file, generated_files) = block_on(async {
        ontology::initialize_ontology_project(&project_name, template.as_deref()).await
    })
    .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Runtime error: {}", e)))?
    .map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Initialization failed: {}", e))
    })?;

    Ok(InitOutput {
        project_name,
        ontology_file,
        config_file,
        generated_files,
    })
}
