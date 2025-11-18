//! Template domain layer - business logic for template operations
//!
//! This module provides the core business logic for template management,
//! using the ggen-core TemplateEngine for REAL template processing.
//!
//! ## v2 Architecture Features
//! - RDF/SPARQL integration via `render_with_rdf` module
//! - Backward compatible with v1 template API
//! - TTL file → Template generation support
//! - Preprocessor integration for advanced template processing

use ggen_core::{GenContext, Generator, Pipeline};
use ggen_utils::error::Result;
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

pub mod generate;
pub mod generate_tree;
pub mod lint;
pub mod list;
pub mod new;
pub mod regenerate;
pub mod render_with_rdf;
pub mod show;

pub use generate::*;
pub use generate_tree::{generate_file_tree, GenerateTreeInput, GenerateTreeOutput};
pub use lint::{
    lint_template, LintError, LintInput, LintOptions, LintOutput, LintReport, LintWarning,
};
pub use list::{
    execute_list, list_templates, ListFilters, ListInput, TemplateInfo, TemplateSource,
};
pub use new::{execute_new, generate_template_content, NewInput, NewOutput};
pub use regenerate::*;
pub use render_with_rdf::*;
pub use show::{execute_show, show_template_metadata, ShowInput, ShowOutput, TemplateMetadata};

// Re-export run functions with specific names to avoid ambiguity
pub use generate_tree::run as generate_tree_run;
pub use lint::run as lint_run;
pub use new::run as new_run;
pub use show::run as show_run;

/// Template service for coordinating template operations
pub struct TemplateService {
    /// Base directory for templates
    pub templates_dir: PathBuf,
}

impl TemplateService {
    pub fn new(templates_dir: PathBuf) -> Self {
        Self { templates_dir }
    }

    pub fn default_instance() -> Self {
        Self::new(PathBuf::from("templates"))
    }

    /// Ensure templates directory exists
    pub fn ensure_templates_dir(&self) -> Result<()> {
        if !self.templates_dir.exists() {
            fs::create_dir_all(&self.templates_dir).map_err(|e| {
                ggen_utils::error::Error::new(&format!(
                    "Failed to create templates directory: {}",
                    e
                ))
            })?;
        }
        Ok(())
    }

    /// Get template path from name
    pub fn template_path(&self, name: &str) -> PathBuf {
        self.templates_dir.join(format!("{}.tmpl", name))
    }

    /// Check if template exists
    pub fn template_exists(&self, name: &str) -> bool {
        self.template_path(name).exists()
    }

    /// Read template content
    pub fn read_template(&self, name: &str) -> Result<String> {
        let path = self.template_path(name);
        fs::read_to_string(&path)
            .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to read template: {}", e)))
    }

    /// Write template content
    pub fn write_template(&self, name: &str, content: &str) -> Result<PathBuf> {
        self.ensure_templates_dir()?;
        let path = self.template_path(name);

        if path.exists() {
            return Err(ggen_utils::error::Error::new(&format!(
                "Template '{}' already exists at {}",
                name,
                path.display()
            )));
        }

        fs::write(&path, content).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to write template: {}", e))
        })?;

        Ok(path)
    }

    /// Render a template using the ggen-core engine
    pub fn render_template(
        &self, template_path: &Path, output_dir: &Path, vars: BTreeMap<String, String>,
    ) -> Result<PathBuf> {
        let pipeline = Pipeline::new()?;
        let ctx =
            GenContext::new(template_path.to_path_buf(), output_dir.to_path_buf()).with_vars(vars);

        let mut generator = Generator::new(pipeline, ctx);
        generator.generate().map_err(|e| {
            ggen_utils::error::Error::new(&format!("Template generation failed: {}", e))
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_service_creation() {
        let temp_dir = TempDir::new().unwrap();
        let service = TemplateService::new(temp_dir.path().to_path_buf());
        assert_eq!(service.templates_dir, temp_dir.path());
    }

    #[test]
    fn test_ensure_templates_dir() {
        let temp_dir = TempDir::new().unwrap();
        let templates_path = temp_dir.path().join("templates");
        let service = TemplateService::new(templates_path.clone());

        assert!(!templates_path.exists());
        service.ensure_templates_dir().unwrap();
        assert!(templates_path.exists());
    }

    #[test]
    fn test_write_and_read_template() {
        let temp_dir = TempDir::new().unwrap();
        let service = TemplateService::new(temp_dir.path().join("templates"));

        let content = r#"---
to: "output.txt"
---
Hello, {{ name }}!"#;

        let path = service.write_template("test", content).unwrap();
        assert!(path.exists());
        assert_eq!(service.read_template("test").unwrap(), content);
    }

    #[test]
    fn test_duplicate_write_fails() {
        let temp_dir = TempDir::new().unwrap();
        let service = TemplateService::new(temp_dir.path().join("templates"));

        service.write_template("test", "content").unwrap();
        let result = service.write_template("test", "content2");
        assert!(result.is_err());
    }
}
