//! Template format definitions
//!
//! Defines the structure and parsing of file tree templates.

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// Node type in the file tree template
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum NodeType {
    /// Directory node
    Directory,
    /// File node
    File,
}

/// A node in the file tree template
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileTreeNode {
    /// Type of node (file or directory)
    #[serde(rename = "type")]
    pub node_type: NodeType,

    /// Name of the file or directory (may contain template variables)
    pub name: String,

    /// Children nodes (for directories)
    #[serde(default)]
    pub children: Vec<FileTreeNode>,

    /// Inline content (for files)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub content: Option<String>,

    /// Template file reference (for files)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub template: Option<String>,

    /// File permissions (Unix mode)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mode: Option<u32>,
}

/// Template format with metadata and RDF support
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateFormat {
    /// Template name
    pub name: String,

    /// Template description
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,

    /// RDF metadata
    #[serde(default)]
    pub rdf: BTreeMap<String, serde_yaml::Value>,

    /// Required variables
    #[serde(default)]
    pub variables: Vec<String>,

    /// Default variable values
    #[serde(default)]
    pub defaults: BTreeMap<String, String>,

    /// Root nodes of the file tree
    pub tree: Vec<FileTreeNode>,
}

impl TemplateFormat {
    /// Create a new template format
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            description: None,
            rdf: BTreeMap::new(),
            variables: Vec::new(),
            defaults: BTreeMap::new(),
            tree: Vec::new(),
        }
    }

    /// Add a variable to the template
    pub fn add_variable(&mut self, var: impl Into<String>) -> &mut Self {
        self.variables.push(var.into());
        self
    }

    /// Add a default value for a variable
    pub fn add_default(&mut self, key: impl Into<String>, value: impl Into<String>) -> &mut Self {
        self.defaults.insert(key.into(), value.into());
        self
    }

    /// Add a tree node
    pub fn add_node(&mut self, node: FileTreeNode) -> &mut Self {
        self.tree.push(node);
        self
    }

    /// Parse from YAML string
    pub fn from_yaml(yaml: &str) -> Result<Self> {
        serde_yaml::from_str(yaml)
            .context("Failed to parse template format from YAML")
    }

    /// Serialize to YAML string
    pub fn to_yaml(&self) -> Result<String> {
        serde_yaml::to_string(self)
            .context("Failed to serialize template format to YAML")
    }

    /// Validate the template format
    pub fn validate(&self) -> Result<()> {
        if self.name.is_empty() {
            anyhow::bail!("Template name cannot be empty");
        }

        if self.tree.is_empty() {
            anyhow::bail!("Template must contain at least one tree node");
        }

        self.validate_nodes(&self.tree)?;

        Ok(())
    }

    fn validate_nodes(&self, nodes: &[FileTreeNode]) -> Result<()> {
        #![allow(clippy::only_used_in_recursion)]
        for node in nodes {
            if node.name.is_empty() {
                anyhow::bail!("Node name cannot be empty");
            }

            match node.node_type {
                NodeType::File => {
                    if node.content.is_none() && node.template.is_none() {
                        anyhow::bail!("File node '{}' must have either content or template", node.name);
                    }
                    if !node.children.is_empty() {
                        anyhow::bail!("File node '{}' cannot have children", node.name);
                    }
                }
                NodeType::Directory => {
                    if node.content.is_some() || node.template.is_some() {
                        anyhow::bail!("Directory node '{}' cannot have content or template", node.name);
                    }
                    self.validate_nodes(&node.children)?;
                }
            }
        }
        Ok(())
    }
}

impl FileTreeNode {
    /// Create a new directory node
    pub fn directory(name: impl Into<String>) -> Self {
        Self {
            node_type: NodeType::Directory,
            name: name.into(),
            children: Vec::new(),
            content: None,
            template: None,
            mode: None,
        }
    }

    /// Create a new file node with inline content
    pub fn file_with_content(name: impl Into<String>, content: impl Into<String>) -> Self {
        Self {
            node_type: NodeType::File,
            name: name.into(),
            children: Vec::new(),
            content: Some(content.into()),
            template: None,
            mode: None,
        }
    }

    /// Create a new file node with template reference
    pub fn file_with_template(name: impl Into<String>, template: impl Into<String>) -> Self {
        Self {
            node_type: NodeType::File,
            name: name.into(),
            children: Vec::new(),
            content: None,
            template: Some(template.into()),
            mode: None,
        }
    }

    /// Add a child node (for directories)
    pub fn add_child(&mut self, child: FileTreeNode) -> &mut Self {
        self.children.push(child);
        self
    }

    /// Set file permissions
    pub fn with_mode(mut self, mode: u32) -> Self {
        self.mode = Some(mode);
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_directory_node() {
        let node = FileTreeNode::directory("src");
        assert_eq!(node.node_type, NodeType::Directory);
        assert_eq!(node.name, "src");
        assert!(node.children.is_empty());
    }

    #[test]
    fn test_file_node_with_content() {
        let node = FileTreeNode::file_with_content("main.rs", "fn main() {}");
        assert_eq!(node.node_type, NodeType::File);
        assert_eq!(node.name, "main.rs");
        assert_eq!(node.content, Some("fn main() {}".to_string()));
        assert_eq!(node.template, None);
    }

    #[test]
    fn test_file_node_with_template() {
        let node = FileTreeNode::file_with_template("lib.rs", "templates/lib.rs.tera");
        assert_eq!(node.node_type, NodeType::File);
        assert_eq!(node.name, "lib.rs");
        assert_eq!(node.template, Some("templates/lib.rs.tera".to_string()));
        assert_eq!(node.content, None);
    }

    #[test]
    fn test_template_format_creation() {
        let mut format = TemplateFormat::new("test-template");
        format.add_variable("service_name");
        format.add_default("port", "8080");

        assert_eq!(format.name, "test-template");
        assert_eq!(format.variables, vec!["service_name"]);
        assert_eq!(format.defaults.get("port"), Some(&"8080".to_string()));
    }

    #[test]
    fn test_template_format_validation() {
        let mut format = TemplateFormat::new("test");
        format.add_node(FileTreeNode::directory("src"));

        assert!(format.validate().is_ok());
    }

    #[test]
    fn test_empty_template_validation_fails() {
        let format = TemplateFormat::new("test");
        assert!(format.validate().is_err());
    }
}
