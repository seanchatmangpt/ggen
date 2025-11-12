//! Domain logic for single-file template generation
//!
//! This module provides core business logic for generating individual files
//! from templates using the ggen-core TemplateEngine.

use ggen_core::{GenContext, Generator, Pipeline};
use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

/// Single file generation options
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct GenerateFileOptions {
    pub template_path: PathBuf,
    pub output_path: PathBuf,
    pub variables: BTreeMap<String, String>,
    pub force_overwrite: bool,
}

impl GenerateFileOptions {
    pub fn new(template_path: PathBuf, output_path: PathBuf) -> Self {
        Self {
            template_path,
            output_path,
            variables: BTreeMap::new(),
            force_overwrite: false,
        }
    }

    pub fn with_var(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.variables.insert(key.into(), value.into());
        self
    }

    pub fn with_vars(mut self, vars: BTreeMap<String, String>) -> Self {
        self.variables.extend(vars);
        self
    }

    pub fn force(mut self) -> Self {
        self.force_overwrite = true;
        self
    }
}

/// Single file generation result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerateFileResult {
    pub output_path: PathBuf,
    pub bytes_written: usize,
    pub template_path: PathBuf,
    pub variables_used: usize,
}

/// Generate a single file from a template
///
/// This function uses the real ggen-core TemplateEngine to process templates
/// with frontmatter, handlebars, and other transformations.
pub fn generate_file(options: &GenerateFileOptions) -> Result<GenerateFileResult> {
    // Validate template exists
    if !options.template_path.exists() {
        return Err(ggen_utils::error::Error::new(&format!(
            "Template not found: {}",
            options.template_path.display()
        )));
    }

    // Check if output exists and we're not forcing
    if options.output_path.exists() && !options.force_overwrite {
        return Err(ggen_utils::error::Error::new(&format!(
            "Output file already exists: {}. Use --force to overwrite.",
            options.output_path.display()
        )));
    }

    // Prepare output directory
    if let Some(parent) = options.output_path.parent() {
        std::fs::create_dir_all(parent).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to create output directory: {}", e))
        })?;
    }

    // Create generation context
    let output_dir = options
        .output_path
        .parent()
        .unwrap_or_else(|| Path::new("."))
        .to_path_buf();

    let ctx = GenContext::new(options.template_path.clone(), output_dir)
        .with_vars(options.variables.clone());

    // Initialize pipeline and generator
    let pipeline = Pipeline::new()?;
    let mut generator = Generator::new(pipeline, ctx);

    // Generate the file
    let result_path = generator.generate().map_err(|e| {
        ggen_utils::error::Error::new(&format!("Template generation failed: {}", e))
    })?;

    // Get file size
    let metadata = std::fs::metadata(&result_path).map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to read output metadata: {}", e))
    })?;

    Ok(GenerateFileResult {
        output_path: result_path,
        bytes_written: metadata.len() as usize,
        template_path: options.template_path.clone(),
        variables_used: options.variables.len(),
    })
}

/// Parse variables from key=value strings
pub fn parse_variables(var_strings: &[String]) -> Result<BTreeMap<String, String>> {
    let mut variables = BTreeMap::new();

    for var in var_strings {
        let parts: Vec<&str> = var.splitn(2, '=').collect();
        if parts.len() != 2 {
            return Err(ggen_utils::error::Error::new(&format!(
                "Invalid variable format: '{}'. Expected 'key=value'",
                var
            )));
        }
        variables.insert(parts[0].to_string(), parts[1].to_string());
    }

    Ok(variables)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_generate_file_options_builder() {
        let options =
            GenerateFileOptions::new(PathBuf::from("template.tmpl"), PathBuf::from("output.txt"))
                .with_var("name", "test")
                .with_var("version", "1.0")
                .force();

        assert_eq!(options.variables.len(), 2);
        assert_eq!(options.variables.get("name"), Some(&"test".to_string()));
        assert!(options.force_overwrite);
    }

    #[test]
    fn test_generate_file_options_with_vars() {
        let mut vars = BTreeMap::new();
        vars.insert("key1".to_string(), "value1".to_string());
        vars.insert("key2".to_string(), "value2".to_string());

        let options =
            GenerateFileOptions::new(PathBuf::from("tmpl"), PathBuf::from("out")).with_vars(vars);

        assert_eq!(options.variables.len(), 2);
    }

    #[test]
    fn test_parse_variables_valid() {
        let input = vec![
            "name=test".to_string(),
            "version=1.0".to_string(),
            "author=John Doe".to_string(),
        ];

        let vars = parse_variables(&input).unwrap();

        assert_eq!(vars.len(), 3);
        assert_eq!(vars.get("name"), Some(&"test".to_string()));
        assert_eq!(vars.get("version"), Some(&"1.0".to_string()));
        assert_eq!(vars.get("author"), Some(&"John Doe".to_string()));
    }

    #[test]
    fn test_parse_variables_with_equals() {
        let input = vec!["url=https://example.com?foo=bar".to_string()];
        let vars = parse_variables(&input).unwrap();

        assert_eq!(
            vars.get("url"),
            Some(&"https://example.com?foo=bar".to_string())
        );
    }

    #[test]
    fn test_parse_variables_invalid() {
        let input = vec!["invalid_no_equals".to_string()];
        let result = parse_variables(&input);

        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Invalid variable format"));
    }

    #[test]
    fn test_generate_file_template_not_found() {
        let options = GenerateFileOptions::new(
            PathBuf::from("/nonexistent/template.tmpl"),
            PathBuf::from("output.txt"),
        );

        let result = generate_file(&options);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Template not found"));
    }

    #[test]
    fn test_generate_file_creates_output_directory() {
        let temp_dir = TempDir::new().unwrap();
        let template_path = temp_dir.path().join("template.tmpl");
        let output_path = temp_dir.path().join("nested/dir/output.txt");

        // Create a simple template
        fs::write(
            &template_path,
            r#"---
to: "{{ to }}"
---
Hello, {{ name }}!"#,
        )
        .unwrap();

        let options = GenerateFileOptions::new(template_path, output_path.clone())
            .with_var("name", "World")
            .with_var("to", "output.txt");

        let result = generate_file(&options).unwrap();

        assert!(result.output_path.exists());
        assert!(result.output_path.parent().unwrap().exists());
    }

    #[test]
    fn test_generate_file_force_overwrite() {
        let temp_dir = TempDir::new().unwrap();
        let template_path = temp_dir.path().join("template.tmpl");
        let output_path = temp_dir.path().join("output.txt");

        // Create template and existing output
        fs::write(
            &template_path,
            r#"---
to: "{{ to }}"
---
New content"#,
        )
        .unwrap();
        fs::write(&output_path, "Old content").unwrap();

        // Without force should fail
        let options_no_force = GenerateFileOptions::new(template_path.clone(), output_path.clone())
            .with_var("to", "output.txt");
        let result = generate_file(&options_no_force);
        assert!(result.is_err());

        // With force should succeed
        let options_force = GenerateFileOptions::new(template_path, output_path.clone())
            .with_var("to", "output.txt")
            .force();
        let result = generate_file(&options_force);
        assert!(result.is_ok());
    }
}
