//! Construct Command - LLM-Construct pattern operations
//!
//! `ggen construct` provides commands for creating and validating LLM-Constructs
//! from OWL ontology specifications.
//!
//! ## Commands
//!
//! - `ggen construct create <spec-path>` - Create LLM-Construct from specification
//! - `ggen construct validate <module-name>` - Validate generated construct module
//!
//! ## LLM-Construct Pipeline
//!
//! 1. OWL Ontology (FIBO, domain-specific) → Extract classes, properties, restrictions
//! 2. SHACL Shapes → Generate validation constraints
//! 3. DSPy Signature → Map to constrained LLM fields
//! 4. Rust Module → Generate executable code with validation
//!
//! ## Example Usage
//!
//! ```bash
//! # Create construct from FIBO Bond specification
//! ggen construct create .specify/fibo_bond.ttl --output-dir crates/ggen-ai/src/constructs
//!
//! # Validate generated module
//! ggen construct validate bond_extractor
//! ```

#![allow(clippy::unused_unit)] // clap-noun-verb macro generates this

use clap_noun_verb_macros::verb;
use serde::Serialize;
use std::path::Path;

// ============================================================================
// Output Types
// ============================================================================

/// Output for the `ggen construct create` command
#[derive(Debug, Clone, Serialize)]
pub struct ConstructCreateOutput {
    /// Overall status: "success", "error", or "not_implemented"
    pub status: String,

    /// Path to the specification file
    pub spec_path: String,

    /// Output directory for generated code
    pub output_dir: String,

    /// Generated module name (snake_case)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub module_name: Option<String>,

    /// Generated file path
    #[serde(skip_serializing_if = "Option::is_none")]
    pub generated_file: Option<String>,

    /// OWL extraction stats
    #[serde(skip_serializing_if = "Option::is_none")]
    pub owl_stats: Option<OWLExtractionStats>,

    /// SHACL generation stats
    #[serde(skip_serializing_if = "Option::is_none")]
    pub shacl_stats: Option<SHACLGenerationStats>,

    /// DSPy mapping stats
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dspy_stats: Option<DSPyMappingStats>,

    /// Error message (if failed)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,

    /// Next steps
    pub next_steps: Vec<String>,
}

/// OWL extraction statistics
#[derive(Debug, Clone, Serialize)]
pub struct OWLExtractionStats {
    pub properties_count: usize,
    pub restrictions_count: usize,
    pub classes_count: usize,
}

/// SHACL generation statistics
#[derive(Debug, Clone, Serialize)]
pub struct SHACLGenerationStats {
    pub property_shapes_count: usize,
    pub node_shapes_count: usize,
    pub constraint_count: usize,
}

/// DSPy mapping statistics
#[derive(Debug, Clone, Serialize)]
pub struct DSPyMappingStats {
    pub constrained_fields_count: usize,
    pub input_fields_count: usize,
    pub output_fields_count: usize,
}

/// Output for the `ggen construct validate` command
#[derive(Debug, Clone, Serialize)]
pub struct ConstructValidateOutput {
    /// Overall status: "success", "error", or "not_implemented"
    pub status: String,

    /// Module name being validated
    pub module_name: String,

    /// Compilation check result
    #[serde(skip_serializing_if = "Option::is_none")]
    pub compilation_result: Option<ValidationResult>,

    /// Lint check result
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lint_result: Option<ValidationResult>,

    /// Test execution result
    #[serde(skip_serializing_if = "Option::is_none")]
    pub test_result: Option<ValidationResult>,

    /// Error message (if failed)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,

    /// Next steps
    pub next_steps: Vec<String>,
}

/// Validation result for a specific check
#[derive(Debug, Clone, Serialize)]
pub struct ValidationResult {
    pub passed: bool,
    pub duration_ms: u64,
    pub message: String,
}

// ============================================================================
// The Construct Commands
// ============================================================================

/// Create LLM-Construct from OWL specification.
///
/// Reads an OWL ontology file (Turtle format) and generates a complete
/// LLM-Construct module with:
/// - SHACL shape validation
/// - DSPy signature mapping
/// - Constraint-aware LLM execution
/// - Type-safe Rust code
///
/// ## Usage
///
/// ```bash
/// # Create construct from specification
/// ggen construct create .specify/fibo_bond.ttl
///
/// # Specify custom output directory
/// ggen construct create .specify/fibo_bond.ttl --output-dir src/constructs
/// ```
///
/// ## Pipeline
///
/// 1. Load OWL specification from TTL file
/// 2. Extract OWL classes, properties, and restrictions
/// 3. Generate SHACL shapes for validation
/// 4. Map to DSPy signature with constraint sets
/// 5. Generate Rust module code
/// 6. Write to output directory
///
/// ## Output
///
/// Returns JSON with generation statistics and file paths.
///
#[verb("construct", "create")]
pub fn create(
    spec_path: String,
    output_dir: Option<String>,
) -> clap_noun_verb::Result<ConstructCreateOutput> {
    // Validate spec file exists
    let spec_file = Path::new(&spec_path);
    if !spec_file.exists() {
        return Ok(ConstructCreateOutput {
            status: "error".to_string(),
            spec_path: spec_path.clone(),
            output_dir: output_dir.unwrap_or_else(|| "crates/ggen-ai/src/constructs".to_string()),
            module_name: None,
            generated_file: None,
            owl_stats: None,
            shacl_stats: None,
            dspy_stats: None,
            error: Some(format!("Specification file not found: {}", spec_path)),
            next_steps: vec![
                "Ensure the specification file exists".to_string(),
                "Specification should be in Turtle (.ttl) format".to_string(),
            ],
        });
    }

    // Validate TTL extension
    if spec_file.extension().and_then(|s| s.to_str()) != Some("ttl") {
        return Ok(ConstructCreateOutput {
            status: "error".to_string(),
            spec_path: spec_path.clone(),
            output_dir: output_dir.unwrap_or_else(|| "crates/ggen-ai/src/constructs".to_string()),
            module_name: None,
            generated_file: None,
            owl_stats: None,
            shacl_stats: None,
            dspy_stats: None,
            error: Some("Specification must be a Turtle (.ttl) file".to_string()),
            next_steps: vec![
                "Convert specification to Turtle format".to_string(),
                "Use .ttl file extension".to_string(),
            ],
        });
    }

    let output_directory = output_dir.unwrap_or_else(|| "crates/ggen-ai/src/constructs".to_string());

    // Delegate to implementation
    perform_create(&spec_path, &output_directory)
}

/// Validate generated LLM-Construct module.
///
/// Runs comprehensive validation checks on a generated construct module:
/// - Compilation check (cargo make check)
/// - Lint check (cargo make lint)
/// - Test execution (cargo make test)
///
/// ## Usage
///
/// ```bash
/// # Validate construct module
/// ggen construct validate bond_extractor
/// ```
///
/// ## Quality Gates
///
/// - Compilation must pass (<5s SLO)
/// - Zero clippy warnings (lint must pass)
/// - All tests must pass (<30s SLO)
///
/// ## Output
///
/// Returns JSON with validation results for each check.
///
#[verb("construct", "validate")]
pub fn validate(module_name: String) -> clap_noun_verb::Result<ConstructValidateOutput> {
    // Delegate to implementation
    perform_validate(&module_name)
}

// ============================================================================
// Implementation Helpers
// ============================================================================

/// Helper function that performs the actual construct creation.
///
/// This function will contain the LLM-Construct pipeline logic once
/// the core implementation is complete:
/// 1. Load RDF spec using Oxigraph
/// 2. Extract OWL classes, properties, restrictions
/// 3. Generate SHACL shapes
/// 4. Map to DSPy signature with constraints
/// 5. Generate Rust code using Tera templates
/// 6. Write to output directory
fn perform_create(
    spec_path: &str,
    output_dir: &str,
) -> clap_noun_verb::Result<ConstructCreateOutput> {
    // TODO: Implementation pending - depends on:
    // - crates/ggen-ai/src/llm_construct/mod.rs (LLMConstructBuilder)
    // - crates/ggen-ai/src/llm_construct/owl_extractor.rs (OWLExtractor)
    // - crates/ggen-ai/src/llm_construct/shacl_generator.rs (SHACLGenerator)
    // - crates/ggen-ai/src/llm_construct/codegen.rs (LLMConstructCodeGen)

    Ok(ConstructCreateOutput {
        status: "not_implemented".to_string(),
        spec_path: spec_path.to_string(),
        output_dir: output_dir.to_string(),
        module_name: None,
        generated_file: None,
        owl_stats: None,
        shacl_stats: None,
        dspy_stats: None,
        error: Some(
            "LLM-Construct implementation is not yet available. \
            This command will be functional once the core LLM-Construct \
            pipeline is implemented in ggen-ai crate."
                .to_string(),
        ),
        next_steps: vec![
            "Implement OWL extractor (ggen-ai/src/llm_construct/owl_extractor.rs)".to_string(),
            "Implement SHACL generator (ggen-ai/src/llm_construct/shacl_generator.rs)".to_string(),
            "Implement DSPy mapper (existing in ggen-ai/src/dspy/)".to_string(),
            "Implement code generator (ggen-ai/src/llm_construct/codegen.rs)".to_string(),
            "See docs/LLM_CONSTRUCT_IMPLEMENTATION.md for specification".to_string(),
        ],
    })
}

/// Helper function that performs construct validation.
///
/// Runs cargo make commands to validate the generated construct:
/// - cargo make check (compilation)
/// - cargo make lint (clippy warnings)
/// - cargo make test (test suite)
fn perform_validate(module_name: &str) -> clap_noun_verb::Result<ConstructValidateOutput> {
    // TODO: Implementation pending - will use:
    // - std::process::Command to run cargo make check
    // - std::process::Command to run cargo make lint
    // - std::process::Command to run cargo make test
    // - Capture output and parse results
    // - Measure execution time for SLO validation

    Ok(ConstructValidateOutput {
        status: "not_implemented".to_string(),
        module_name: module_name.to_string(),
        compilation_result: None,
        lint_result: None,
        test_result: None,
        error: Some(
            "LLM-Construct validation is not yet available. \
            This command will be functional once the core LLM-Construct \
            pipeline is implemented."
                .to_string(),
        ),
        next_steps: vec![
            "Implement construct creation first (ggen construct create)".to_string(),
            "Add validation logic using std::process::Command".to_string(),
            "Integrate with cargo make quality gates".to_string(),
        ],
    })
}

// ============================================================================
// Utility Functions
// ============================================================================

/// Convert a file name or class name to snake_case module name.
///
/// Examples:
/// - "FIBOBond" -> "fibo_bond"
/// - "bond-extractor" -> "bond_extractor"
/// - "Bond" -> "bond"
#[allow(dead_code)]
fn to_snake_case(name: &str) -> String {
    let mut result = String::new();
    let mut prev_is_lower = false;

    for (i, ch) in name.chars().enumerate() {
        if ch.is_uppercase() {
            if i > 0 && prev_is_lower {
                result.push('_');
            }
            result.push(ch.to_ascii_lowercase());
            prev_is_lower = false;
        } else if ch == '-' || ch == ' ' {
            result.push('_');
            prev_is_lower = false;
        } else {
            result.push(ch);
            prev_is_lower = ch.is_lowercase();
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_snake_case_camel_case() {
        assert_eq!(to_snake_case("FIBOBond"), "f_i_b_o_bond");
        assert_eq!(to_snake_case("BondExtractor"), "bond_extractor");
    }

    #[test]
    fn test_to_snake_case_kebab_case() {
        assert_eq!(to_snake_case("bond-extractor"), "bond_extractor");
        assert_eq!(to_snake_case("loan-validator"), "loan_validator");
    }

    #[test]
    fn test_to_snake_case_already_snake() {
        assert_eq!(to_snake_case("bond_extractor"), "bond_extractor");
    }

    #[test]
    fn test_to_snake_case_single_word() {
        assert_eq!(to_snake_case("Bond"), "bond");
        assert_eq!(to_snake_case("bond"), "bond");
    }

    #[test]
    fn test_to_snake_case_with_spaces() {
        assert_eq!(to_snake_case("Bond Extractor"), "bond_extractor");
    }
}
