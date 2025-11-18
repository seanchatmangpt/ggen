//! File tree template parsing and generation
//!
//! This module provides core functionality for parsing template formats and generating
//! file trees. It handles YAML template parsing, RDF metadata extraction, and template
//! validation.
//!
//! ## Features
//!
//! - **YAML Parsing**: Parse templates from YAML files or strings
//! - **RDF Metadata**: Extract and convert RDF metadata to Turtle format
//! - **Template Validation**: Validate template structure and required fields
//! - **Variable Management**: Track required variables and default values
//! - **Simple Format Support**: Legacy support for simple template format
//! - **File Tree Structure**: Represent hierarchical file and directory structures
//!
//! ## Template Format
//!
//! Templates are defined in YAML with the following structure:
//!
//! ```yaml
//! name: my-template
//! description: A sample template
//! variables:
//!   - service_name
//!   - port
//! defaults:
//!   port: "8080"
//! rdf:
//!   type: "ggen:MicroserviceTemplate"
//!   language: "rust"
//! tree:
//!   - type: directory
//!     name: src
//!     children:
//!       - type: file
//!         name: main.rs
//!         content: "fn main() {}"
//! ```
//!
//! ## Examples
//!
//! ### Loading a Template from File
//!
//! ```rust,no_run
//! use ggen_core::templates::file_tree_generator::FileTreeTemplate;
//! use std::path::Path;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let template = FileTreeTemplate::from_file(Path::new("template.yaml"))?;
//!
//! println!("Template: {}", template.name());
//! println!("Variables: {:?}", template.required_variables());
//! # Ok(())
//! # }
//! ```
//!
//! ### Parsing from YAML String
//!
//! ```rust,no_run
//! use ggen_core::templates::file_tree_generator::{FileTreeTemplate, TemplateParser};
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let yaml = r#"
//! name: my-template
//! variables:
//!   - name
//! tree:
//!   - type: directory
//!     name: src
//! "#;
//!
//! let template = TemplateParser::parse_yaml(yaml)?;
//! assert_eq!(template.name(), "my-template");
//! # Ok(())
//! # }
//! ```
//!
//! ### Working with RDF Metadata
//!
//! ```rust,no_run
//! use ggen_core::templates::file_tree_generator::FileTreeTemplate;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let yaml = r#"
//! name: microservice
//! rdf:
//!   type: "ggen:MicroserviceTemplate"
//!   language: "rust"
//! tree:
//!   - type: directory
//!     name: src
//! "#;
//!
//! let template = FileTreeTemplate::from_yaml(yaml)?;
//! // RDF metadata is automatically converted to Turtle format
//! assert!(template.rdf_turtle.is_some());
//! # Ok(())
//! # }
//! ```

use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::Path;

use super::format::{FileTreeNode, TemplateFormat};

/// File tree template with metadata and RDF support
///
/// Wraps a `TemplateFormat` with RDF metadata support. Provides methods for
/// loading templates from files, parsing YAML, and managing template metadata.
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_core::templates::file_tree_generator::FileTreeTemplate;
/// use std::path::Path;
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let template = FileTreeTemplate::from_file(Path::new("template.yaml"))?;
/// println!("Template: {}", template.name());
/// # Ok(())
/// # }
/// ```
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
    ///
    /// Creates a template from a `TemplateFormat`. RDF metadata will be
    /// initialized if present in the format.
    ///
    /// # Arguments
    ///
    /// * `format` - The template format to wrap
    ///
    /// # Returns
    ///
    /// A new `FileTreeTemplate` with the provided format.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::file_tree_generator::FileTreeTemplate;
    /// use ggen_core::templates::format::TemplateFormat;
    ///
    /// let format = TemplateFormat::new("my-template");
    /// let template = FileTreeTemplate::new(format);
    /// assert_eq!(template.name(), "my-template");
    /// ```
    pub fn new(format: TemplateFormat) -> Self {
        Self {
            format,
            rdf_turtle: None,
        }
    }

    /// Load from YAML file
    ///
    /// Reads a template from a YAML file and parses it into a `FileTreeTemplate`.
    /// RDF metadata is automatically extracted and converted to Turtle format.
    ///
    /// # Arguments
    ///
    /// * `path` - Path to the YAML template file
    ///
    /// # Returns
    ///
    /// A parsed `FileTreeTemplate` on success.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The file cannot be read
    /// - The YAML is invalid
    /// - Template validation fails
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::templates::file_tree_generator::FileTreeTemplate;
    /// use std::path::Path;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let template = FileTreeTemplate::from_file(Path::new("template.yaml"))?;
    /// println!("Template: {}", template.name());
    /// # Ok(())
    /// # }
    /// ```
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Self> {
        let content = std::fs::read_to_string(&path).map_err(|e| {
            Error::with_context(
                "Failed to read template file",
                &format!("{}: {}", path.as_ref().display(), e),
            )
        })?;

        Self::from_yaml(&content)
    }

    /// Parse from YAML string
    ///
    /// Parses a template from a YAML string. RDF metadata is automatically
    /// extracted and converted to Turtle format if present.
    ///
    /// # Arguments
    ///
    /// * `yaml` - YAML string to parse
    ///
    /// # Returns
    ///
    /// A parsed `FileTreeTemplate` on success.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The YAML is invalid
    /// - Template validation fails
    /// - RDF metadata processing fails
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::file_tree_generator::FileTreeTemplate;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let yaml = r#"
    /// name: my-template
    /// variables:
    ///   - service_name
    /// tree:
    ///   - name: src
    ///     type: directory
    /// "#;
    ///
    /// let template = FileTreeTemplate::from_yaml(yaml)?;
    /// assert_eq!(template.name(), "my-template");
    /// # Ok(())
    /// # }
    /// ```
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
    ///
    /// Returns a slice of all required variable names for this template.
    ///
    /// # Returns
    ///
    /// A slice of variable names that must be provided during generation.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::file_tree_generator::FileTreeTemplate;
    /// use ggen_core::templates::format::TemplateFormat;
    ///
    /// let mut format = TemplateFormat::new("my-template");
    /// format.add_variable("service_name")
    ///       .add_variable("port");
    ///
    /// let template = FileTreeTemplate::new(format);
    /// let vars = template.required_variables();
    /// assert_eq!(vars.len(), 2);
    /// ```
    pub fn required_variables(&self) -> &[String] {
        &self.format.variables
    }

    /// Get default values
    ///
    /// Returns a reference to the map of default variable values.
    ///
    /// # Returns
    ///
    /// A map of variable names to their default string values.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::file_tree_generator::FileTreeTemplate;
    /// use ggen_core::templates::format::TemplateFormat;
    ///
    /// let mut format = TemplateFormat::new("my-template");
    /// format.add_default("port", "8080");
    ///
    /// let template = FileTreeTemplate::new(format);
    /// let defaults = template.defaults();
    /// assert_eq!(defaults.get("port"), Some(&"8080".to_string()));
    /// ```
    pub fn defaults(&self) -> &BTreeMap<String, String> {
        &self.format.defaults
    }

    /// Validate the template
    ///
    /// Validates that the template format is well-formed. See `TemplateFormat::validate()`
    /// for validation rules.
    ///
    /// # Returns
    ///
    /// `Ok(())` if the template is valid.
    ///
    /// # Errors
    ///
    /// Returns an error if validation fails.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::file_tree_generator::FileTreeTemplate;
    /// use ggen_core::templates::format::{TemplateFormat, FileTreeNode};
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let mut format = TemplateFormat::new("my-template");
    /// format.add_node(FileTreeNode::directory("src"));
    ///
    /// let template = FileTreeTemplate::new(format);
    /// template.validate()?; // Ok
    /// # Ok(())
    /// # }
    /// ```
    pub fn validate(&self) -> Result<()> {
        self.format.validate()
    }

    /// Get the template name
    ///
    /// Returns the name of this template.
    ///
    /// # Returns
    ///
    /// The template name as a string slice.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::file_tree_generator::FileTreeTemplate;
    /// use ggen_core::templates::format::TemplateFormat;
    ///
    /// let format = TemplateFormat::new("my-template");
    /// let template = FileTreeTemplate::new(format);
    /// assert_eq!(template.name(), "my-template");
    /// ```
    pub fn name(&self) -> &str {
        &self.format.name
    }

    /// Get the template description
    ///
    /// Returns the optional description of this template.
    ///
    /// # Returns
    ///
    /// `Some(&str)` if a description exists, `None` otherwise.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::file_tree_generator::FileTreeTemplate;
    /// use ggen_core::templates::format::TemplateFormat;
    ///
    /// let format = TemplateFormat::new("my-template");
    /// let template = FileTreeTemplate::new(format);
    /// assert!(template.description().is_none());
    /// ```
    pub fn description(&self) -> Option<&str> {
        self.format.description.as_deref()
    }

    /// Get the file tree nodes
    ///
    /// Returns a slice of all root-level nodes in the file tree.
    ///
    /// # Returns
    ///
    /// A slice of root-level `FileTreeNode` instances.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::file_tree_generator::FileTreeTemplate;
    /// use ggen_core::templates::format::{TemplateFormat, FileTreeNode};
    ///
    /// let mut format = TemplateFormat::new("my-template");
    /// format.add_node(FileTreeNode::directory("src"));
    ///
    /// let template = FileTreeTemplate::new(format);
    /// let nodes = template.nodes();
    /// assert_eq!(nodes.len(), 1);
    /// ```
    pub fn nodes(&self) -> &[FileTreeNode] {
        &self.format.tree
    }
}

/// Template parser for different input formats
///
/// Provides parsing functionality for templates in various formats (YAML, simple format).
/// Supports both modern YAML format and legacy simple format for backward compatibility.
///
/// # Examples
///
/// ```rust
/// use ggen_core::templates::file_tree_generator::TemplateParser;
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let yaml = r#"
/// name: my-template
/// tree:
///   - name: src
///     type: directory
/// "#;
///
/// let template = TemplateParser::parse_yaml(yaml)?;
/// assert_eq!(template.name(), "my-template");
/// # Ok(())
/// # }
/// ```
pub struct TemplateParser;

impl TemplateParser {
    /// Parse a template from YAML string
    ///
    /// Parses a template from a YAML string. This is a convenience method
    /// that delegates to `FileTreeTemplate::from_yaml()`.
    ///
    /// # Arguments
    ///
    /// * `yaml` - YAML string to parse
    ///
    /// # Returns
    ///
    /// A parsed `FileTreeTemplate` on success.
    ///
    /// # Errors
    ///
    /// Returns an error if parsing or validation fails.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::file_tree_generator::TemplateParser;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let yaml = r#"
    /// name: my-template
    /// tree:
    ///   - name: src
    ///     type: directory
    /// "#;
    ///
    /// let template = TemplateParser::parse_yaml(yaml)?;
    /// assert_eq!(template.name(), "my-template");
    /// # Ok(())
    /// # }
    /// ```
    pub fn parse_yaml(yaml: &str) -> Result<FileTreeTemplate> {
        FileTreeTemplate::from_yaml(yaml)
    }

    /// Parse a template from file
    ///
    /// Reads and parses a template from a YAML file. This is a convenience method
    /// that delegates to `FileTreeTemplate::from_file()`.
    ///
    /// # Arguments
    ///
    /// * `path` - Path to the YAML template file
    ///
    /// # Returns
    ///
    /// A parsed `FileTreeTemplate` on success.
    ///
    /// # Errors
    ///
    /// Returns an error if the file cannot be read or parsing fails.
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::templates::file_tree_generator::TemplateParser;
    /// use std::path::Path;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let template = TemplateParser::parse_file(Path::new("template.yaml"))?;
    /// println!("Template: {}", template.name());
    /// # Ok(())
    /// # }
    /// ```
    pub fn parse_file<P: AsRef<Path>>(path: P) -> Result<FileTreeTemplate> {
        FileTreeTemplate::from_file(path)
    }

    /// Parse a simple template format (legacy support)
    ///
    /// Parses a legacy simple template format for backward compatibility.
    /// The format uses bracket notation to define directories and files.
    ///
    /// # Format
    ///
    /// ```text
    /// [directory: "src"]
    ///   [file: "main.rs"]
    ///     content: |
    ///       fn main() {}
    /// ```
    ///
    /// # Arguments
    ///
    /// * `content` - Simple format template string
    ///
    /// # Returns
    ///
    /// A parsed `FileTreeTemplate` on success.
    ///
    /// # Errors
    ///
    /// Returns an error if parsing fails.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::file_tree_generator::TemplateParser;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let content = r#"
    /// [directory: "src"]
    /// [file: "main.rs"]
    /// "#;
    ///
    /// let template = TemplateParser::parse_simple(content)?;
    /// assert_eq!(template.nodes().len(), 2);
    /// # Ok(())
    /// # }
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
        line: &str, _lines: &[&str], _index: &mut usize,
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
        let start = line
            .find('"')
            .ok_or_else(|| Error::new("Missing opening quote"))?;
        let end = line[start + 1..]
            .find('"')
            .ok_or_else(|| Error::new("Missing closing quote"))?;
        Ok(line[start + 1..start + 1 + end].to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    test!(test_create_template, {
        let format = TemplateFormat::new("test-template");
        let template = FileTreeTemplate::new(format);

        assert_eq!(template.name(), "test-template");
        assert!(template.description().is_none());
    });

    test!(test_parse_yaml_template, {
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
    });

    test!(test_parse_template_with_rdf, {
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
    });

    test!(test_template_validation, {
        let format = TemplateFormat::new("test");
        let template = FileTreeTemplate::new(format);

        // Should fail because tree is empty
        assert!(template.validate().is_err());
    });

    test!(test_parser_extract_name, {
        let line = r#"[directory: "src"]"#;
        let name = TemplateParser::extract_name(line).unwrap();
        assert_eq!(name, "src");
    });

    test!(test_parse_simple_format, {
        let content = r#"
[directory: "src"]
[file: "main.rs"]
"#;

        let template = TemplateParser::parse_simple(content).unwrap();
        assert_eq!(template.nodes().len(), 2);
    });
}
