//! Project generation domain logic
//!
//! Chicago TDD: Pure business logic with REAL generation

use std::collections::HashMap;
use ggen_utils::error::Result;

/// Generation result with statistics
#[derive(Debug, Clone)]
pub struct GenerationResult {
    pub files_created: usize,
    pub operations: Vec<Operation>,
}

/// Generation operation
#[derive(Debug, Clone)]
pub enum Operation {
    Create { path: String, content: String },
    Update { path: String, content: String },
    Delete { path: String },
}

/// Template data
#[derive(Debug, Clone)]
pub struct Template {
    pub content: String,
    pub frontmatter: HashMap<String, String>,
}

/// Parse key=value pairs into HashMap
fn parse_vars(vars: &[String]) -> Result<HashMap<String, String>> {
    let mut map = HashMap::new();
    for var in vars {
        let parts: Vec<&str> = var.splitn(2, '=').collect();
        if parts.len() != 2 {
            return Err(ggen_utils::error::Error::new_fmt(format_args!(
                "Invalid variable format: '{}'. Expected 'key=value'",
                var
            )));
        }
        map.insert(parts[0].to_string(), parts[1].to_string());
    }
    Ok(map)
}

/// Validate generation input
fn validate_input(template_ref: &str, vars: &[String]) -> Result<()> {
    // Validate template reference
    if template_ref.trim().is_empty() {
        return Err(ggen_utils::error::Error::new(
            "Template reference cannot be empty",
        ));
    }

    if template_ref.len() > 500 {
        return Err(ggen_utils::error::Error::new(
            "Template reference too long (max 500 characters)",
        ));
    }

    // Validate variables format
    for var in vars {
        if !var.contains('=') {
            return Err(ggen_utils::error::Error::new_fmt(format_args!(
                "Invalid variable format: '{}'. Expected format: key=value",
                var
            )));
        }

        let parts: Vec<&str> = var.splitn(2, '=').collect();
        if parts.len() != 2 || parts[0].trim().is_empty() {
            return Err(ggen_utils::error::Error::new_fmt(format_args!(
                "Invalid variable format: '{}'. Key cannot be empty",
                var
            )));
        }
    }

    Ok(())
}

/// Project generation input (pure domain type - no CLI dependencies)
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct GenInput {
    /// Template reference (path or registry ID)
    pub template_ref: String,

    /// Variables (key=value format)
    pub vars: Vec<String>,

    /// Output directory
    pub output_dir: PathBuf,

    /// Dry run (don't write files)
    pub dry_run: bool,
}

/// Execute project generation with input (pure domain function)
pub async fn execute_gen(input: GenInput) -> Result<GenerationResult> {
    use ggen_core::{TemplateResolver, Template};
    use ggen_core::{Generator, GenContext, Pipeline};
    use std::collections::BTreeMap;

    // Step 1: Validate input
    validate_input(&input.template_ref, &input.vars)?;

    // Step 2: Parse variables
    let vars = parse_vars(&input.vars)?;

    // Step 3: Resolve template using ggen-core
    let resolver = TemplateResolver::new()?;
    let template_source = resolver.resolve(&input.template_ref)
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to resolve template: {}", e)))?;

    let template_path = match template_source {
        ggen_core::TemplateSource::File(path) => path,
        ggen_core::TemplateSource::Registry(_) => {
            return Err(ggen_utils::error::Error::new("Registry templates not yet implemented"));
        }
    };

    // Step 4: Generate using ggen-core Generator
    let pipeline = Pipeline::new()
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to create pipeline: {}", e)))?;
    
    let ctx = GenContext::new(template_path, input.output_dir.clone())
        .with_vars(vars)
        .dry(input.dry_run);

    let mut generator = Generator::new(pipeline, ctx);
    
    let output_path = generator.generate()
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to generate project: {}", e)))?;

    // Step 5: Collect operations from generated files
    let mut operations = Vec::new();
    if !input.dry_run {
        // Scan output directory for generated files
        let files = collect_generated_files(&input.output_dir)?;
        for file_path in files {
            let content = std::fs::read_to_string(&file_path)
                .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to read generated file: {}", e)))?;
            
            let relative_path = file_path.strip_prefix(&input.output_dir)
                .map(|p| p.to_string_lossy().to_string())
                .unwrap_or_else(|_| file_path.to_string_lossy().to_string());
            
            operations.push(Operation::Create {
                path: relative_path,
                content,
            });
        }
    }

    Ok(GenerationResult {
        files_created: operations.len(),
        operations,
    })
}

/// Collect all generated files from output directory
fn collect_generated_files(output_dir: &std::path::Path) -> Result<Vec<PathBuf>> {
    use std::fs;
    
    let mut files = Vec::new();
    
    if !output_dir.exists() {
        return Ok(files);
    }

    fn collect_recursive(dir: &std::path::Path, files: &mut Vec<PathBuf>) -> Result<()> {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            
            if path.is_dir() {
                collect_recursive(&path, files)?;
            } else if path.is_file() {
                files.push(path);
            }
        }
        Ok(())
    }

    collect_recursive(output_dir, &mut files)?;
    Ok(files)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_vars_valid() {
        let vars = vec!["name=Alice".to_string(), "age=30".to_string()];
        let result = parse_vars(&vars).unwrap();

        assert_eq!(result.get("name"), Some(&"Alice".to_string()));
        assert_eq!(result.get("age"), Some(&"30".to_string()));
    }

    #[test]
    fn test_parse_vars_invalid_format() {
        let vars = vec!["invalid".to_string()];
        let result = parse_vars(&vars);

        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Invalid variable format"));
    }

    #[test]
    fn test_parse_vars_with_equals_in_value() {
        let vars = vec!["url=https://example.com?foo=bar".to_string()];
        let result = parse_vars(&vars).unwrap();

        assert_eq!(
            result.get("url"),
            Some(&"https://example.com?foo=bar".to_string())
        );
    }

    #[test]
    fn test_validate_input_empty_template() {
        let result = validate_input("", &[]);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("cannot be empty"));
    }

    #[test]
    fn test_validate_input_invalid_var() {
        let result = validate_input("template.tmpl", &["invalid".to_string()]);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Expected format"));
    }
}
