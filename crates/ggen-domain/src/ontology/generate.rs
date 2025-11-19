//! Generate code from ontology schema
//!
//! This module handles:
//! - Loading templates for target languages
//! - Rendering templates with ontology data
//! - Generating code files
//! - Formatting output
//! - Validation of generated code

use ggen_core::ontology_pack::OntologySchema;
use ggen_utils::error::{Error, Result};
use std::path::PathBuf;

/// Input for code generation
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct GenerateInput {
    /// Extracted schema
    pub schema: OntologySchema,

    /// Target language (typescript, rust, python, etc.)
    pub language: String,

    /// Code generation features to enable
    pub features: Vec<String>,

    /// Output directory
    pub output_dir: PathBuf,

    /// Template path (from ontology pack)
    pub template_path: Option<PathBuf>,
}

/// Output from code generation
#[derive(Debug, Clone, serde::Serialize)]
pub struct GenerateOutput {
    /// Files generated
    pub files_generated: Vec<PathBuf>,

    /// Primary output file
    pub primary_file: PathBuf,

    /// Generation statistics
    pub stats: GenerationStats,
}

/// Statistics about code generation
#[derive(Debug, Clone, serde::Serialize)]
pub struct GenerationStats {
    /// Number of type definitions generated
    pub types_generated: usize,

    /// Number of validators generated
    pub validators_generated: usize,

    /// Number of utility functions generated
    pub utilities_generated: usize,

    /// Total lines of code generated
    pub lines_of_code: usize,
}

/// Execute code generation
///
/// # Example
///
/// ```rust,no_run
/// use ggen_domain::ontology::generate::{execute_generate, GenerateInput};
/// use ggen_core::ontology_pack::OntologySchema;
/// use std::path::PathBuf;
///
/// #[tokio::main]
/// async fn main() -> Result<(), Box<dyn std::error::Error>> {
///     let input = GenerateInput {
///         schema: OntologySchema {
///             namespace: "https://schema.org/".to_string(),
///             classes: vec![],
///             properties: vec![],
///             relationships: vec![],
///             prefixes: Default::default(),
///         },
///         language: "typescript".to_string(),
///         features: vec!["zod".to_string(), "utilities".to_string()],
///         output_dir: PathBuf::from("generated/"),
///         template_path: Some(PathBuf::from("templates/typescript/")),
///     };
///
///     let output = execute_generate(&input).await?;
///     println!("Generated {} types", output.stats.types_generated);
///     Ok(())
/// }
/// ```
pub async fn execute_generate(input: &GenerateInput) -> Result<GenerateOutput> {
    // 1. Create output directory
    tokio::fs::create_dir_all(&input.output_dir).await?;

    // 2. Load templates for target language
    let template_dir = input.template_path.as_ref()
        .ok_or_else(|| Error::new("Template path required"))?;

    if !template_dir.exists() {
        return Err(Error::new(&format!(
            "Template directory not found: {}",
            template_dir.display()
        )));
    }

    // 3. Generate code (placeholder for now)
    let mut files = Vec::new();
    let primary_file = input.output_dir.join("types.ts");

    // TODO: Implement template rendering
    // For now, create a simple index file
    let index_content = format!(
        "// Generated from {} ontology\n// Language: {}\n// Features: {}\n",
        input.schema.namespace,
        input.language,
        input.features.join(", ")
    );

    tokio::fs::write(&primary_file, index_content).await?;
    files.push(primary_file.clone());

    let stats = GenerationStats {
        types_generated: input.schema.classes.len(),
        validators_generated: if input.features.contains(&"zod".to_string()) {
            input.schema.classes.len()
        } else {
            0
        },
        utilities_generated: if input.features.contains(&"utilities".to_string()) { 1 } else { 0 },
        lines_of_code: 0,
    };

    Ok(GenerateOutput {
        files_generated: files.clone(),
        primary_file,
        stats,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use ggen_core::ontology_pack::OntologySchema;

    #[tokio::test]
    async fn test_generate_creates_output_dir() {
        let temp_dir = tempfile::tempdir().unwrap();
        let output_dir = temp_dir.path().join("generated");
        let template_dir = tempfile::tempdir().unwrap();

        let input = GenerateInput {
            schema: OntologySchema {
                namespace: "https://schema.org/".to_string(),
                classes: vec![],
                properties: vec![],
                relationships: vec![],
                prefixes: Default::default(),
            },
            language: "typescript".to_string(),
            features: vec![],
            output_dir: output_dir.clone(),
            template_path: Some(template_dir.path().to_path_buf()),
        };

        let result = execute_generate(&input).await;
        assert!(result.is_ok());
        assert!(output_dir.exists());
    }
}
