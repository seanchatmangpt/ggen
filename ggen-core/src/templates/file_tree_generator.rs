//! File tree template parsing and generation
//!
//! Provides core functionality for parsing template formats and generating file trees.

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::Path;

use super::format::{FileTreeNode, TemplateFormat};

/// File tree template with metadata and RDF support
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileTreeTemplate {
    /// Template metadata
    pub format: TemplateFormat,

    /// RDF metadata as raw turtle (for future processing)
    #[serde(skip)]
    pub rdf_turtle: Option<String>,
}

impl FileTreeTemplate {
    /// Create a new file tree template
    pub fn new(format: TemplateFormat) -> Self {
        Self {
            format,
            rdf_turtle: None,
        }
    }

    /// Load from YAML file
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Self> {
        let content = std::fs::read_to_string(&path)
            .with_context(|| format!("Failed to read template file: {}", path.as_ref().display()))?;

        Self::from_yaml(&content)
    }

    /// Parse from YAML string
    pub fn from_yaml(yaml: &str) -> Result<Self> {
        let format = TemplateFormat::from_yaml(yaml)?;
        format.validate()?;

        let mut template = Self::new(format);

        // Initialize RDF graph if metadata is present
        if !template.format.rdf.is_empty() {
            template.initialize_graph()?;
        }

        Ok(template)
    }

    /// Initialize RDF graph from metadata
    fn initialize_graph(&mut self) -> Result<()> {
        // Collect all RDF metadata as turtle strings
        let mut turtle_lines = Vec::new();

        for (key, value) in &self.format.rdf {
            let turtle = self.rdf_value_to_turtle(key, value)?;
            if !turtle.is_empty() {
                turtle_lines.push(turtle);
            }
        }

        if !turtle_lines.is_empty() {
            self.rdf_turtle = Some(turtle_lines.join("\n"));
        }

        Ok(())
    }

    /// Convert RDF value to Turtle format
    fn rdf_value_to_turtle(&self, key: &str, value: &serde_yaml::Value) -> Result<String> {
        let base = format!("<http://example.org/template/{}>", self.format.name);

        match value {
            serde_yaml::Value::String(s) => {
                Ok(format!("{} <http://example.org/{}> \"{}\" .", base, key, s))
            }
            serde_yaml::Value::Sequence(seq) => {
                let values: Vec<String> = seq
                    .iter()
                    .filter_map(|v| v.as_str())
                    .map(|s| format!("\"{}\"", s))
                    .collect();

                if values.is_empty() {
                    return Ok(String::new());
                }

                Ok(format!(
                    "{} <http://example.org/{}> {} .",
                    base,
                    key,
                    values.join(", ")
                ))
            }
            _ => Ok(String::new()),
        }
    }

    /// Get required variables
    pub fn required_variables(&self) -> &[String] {
        &self.format.variables
    }

    /// Get default values
    pub fn defaults(&self) -> &BTreeMap<String, String> {
        &self.format.defaults
    }

    /// Validate the template
    pub fn validate(&self) -> Result<()> {
        self.format.validate()
    }

    /// Get the template name
    pub fn name(&self) -> &str {
        &self.format.name
    }

    /// Get the template description
    pub fn description(&self) -> Option<&str> {
        self.format.description.as_deref()
    }

    /// Get the file tree nodes
    pub fn nodes(&self) -> &[FileTreeNode] {
        &self.format.tree
    }
}

/// Template parser for different input formats
pub struct TemplateParser;

impl TemplateParser {
    /// Parse a template from YAML string
    pub fn parse_yaml(yaml: &str) -> Result<FileTreeTemplate> {
        FileTreeTemplate::from_yaml(yaml)
    }

    /// Parse a template from file
    pub fn parse_file<P: AsRef<Path>>(path: P) -> Result<FileTreeTemplate> {
        FileTreeTemplate::from_file(path)
    }

    /// Parse a simple template format (legacy support)
    ///
    /// Format:
    /// ```text
    /// [directory: "src"]
    ///   [file: "main.rs"]
    ///     content: |
    ///       fn main() {}
    /// ```
    pub fn parse_simple(content: &str) -> Result<FileTreeTemplate> {
        let nodes = Self::parse_simple_nodes(content)?;

        let mut format = TemplateFormat::new("simple-template");
        for node in nodes {
            format.add_node(node);
        }

        format.validate()?;
        Ok(FileTreeTemplate::new(format))
    }

    fn parse_simple_nodes(content: &str) -> Result<Vec<FileTreeNode>> {
        let mut nodes = Vec::new();
        let lines: Vec<&str> = content.lines().collect();
        let mut i = 0;

        while i < lines.len() {
            let line = lines[i].trim();

            if line.is_empty() || line.starts_with('#') {
                i += 1;
                continue;
            }

            if let Some(node) = Self::parse_simple_node(line, &lines, &mut i)? {
                nodes.push(node);
            }

            i += 1;
        }

        Ok(nodes)
    }

    fn parse_simple_node(
        line: &str,
        _lines: &[&str],
        _index: &mut usize,
    ) -> Result<Option<FileTreeNode>> {
        if line.starts_with("[directory:") {
            let name = Self::extract_name(line)?;
            Ok(Some(FileTreeNode::directory(name)))
        } else if line.starts_with("[file:") {
            let name = Self::extract_name(line)?;
            Ok(Some(FileTreeNode::file_with_content(name, "")))
        } else {
            Ok(None)
        }
    }

    fn extract_name(line: &str) -> Result<String> {
        let start = line.find('"').context("Missing opening quote")?;
        let end = line[start + 1..].find('"').context("Missing closing quote")?;
        Ok(line[start + 1..start + 1 + end].to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_template() {
        let format = TemplateFormat::new("test-template");
        let template = FileTreeTemplate::new(format);

        assert_eq!(template.name(), "test-template");
        assert!(template.description().is_none());
    }

    #[test]
    fn test_parse_yaml_template() {
        let yaml = r#"
name: test-template
description: A test template
variables:
  - service_name
  - port
defaults:
  port: "8080"
tree:
  - type: directory
    name: src
    children:
      - type: file
        name: main.rs
        content: "fn main() {}"
"#;

        let template = FileTreeTemplate::from_yaml(yaml).unwrap();

        assert_eq!(template.name(), "test-template");
        assert_eq!(template.description(), Some("A test template"));
        assert_eq!(template.required_variables(), &["service_name", "port"]);
        assert_eq!(template.defaults().get("port"), Some(&"8080".to_string()));
        assert_eq!(template.nodes().len(), 1);
    }

    #[test]
    fn test_parse_template_with_rdf() {
        let yaml = r#"
name: microservice-template
rdf:
  type: "ggen:MicroserviceTemplate"
  language: "rust"
variables:
  - service_name
tree:
  - type: directory
    name: src
    children: []
"#;

        let template = FileTreeTemplate::from_yaml(yaml).unwrap();

        assert_eq!(template.name(), "microservice-template");
        assert!(template.rdf_turtle.is_some());
    }

    #[test]
    fn test_template_validation() {
        let format = TemplateFormat::new("test");
        let template = FileTreeTemplate::new(format);

        // Should fail because tree is empty
        assert!(template.validate().is_err());
    }

    #[test]
    fn test_parser_extract_name() {
        let line = r#"[directory: "src"]"#;
        let name = TemplateParser::extract_name(line).unwrap();
        assert_eq!(name, "src");
    }

    #[test]
    fn test_parse_simple_format() {
        let content = r#"
[directory: "src"]
[file: "main.rs"]
"#;

        let template = TemplateParser::parse_simple(content).unwrap();
        assert_eq!(template.nodes().len(), 2);
    }
}
