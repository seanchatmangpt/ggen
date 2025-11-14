//! Template format definitions
//!
//! Defines the structure and parsing of file tree templates.
//!
//! ## Features
//!
//! - **File tree representation**: Hierarchical structure for directory and file nodes
//! - **Template format parsing**: Parse YAML/JSON template definitions
//! - **Node types**: Support for directories and files
//! - **Metadata support**: Attach metadata to nodes
//!
//! ## Examples
//!
//! ### Creating a File Tree Template
//!
//! ```rust,no_run
//! use ggen_core::templates::format::{FileTreeNode, NodeType, TemplateFormat};
//! use serde_json::json;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let template = TemplateFormat {
//!     nodes: vec![
//!         FileTreeNode {
//!             name: "src".to_string(),
//!             node_type: NodeType::Directory,
//!             content: None,
//!             children: vec![
//!                 FileTreeNode {
//!                     name: "main.rs".to_string(),
//!                     node_type: NodeType::File,
//!                     content: Some("fn main() {{ println!(\"Hello\"); }}".to_string()),
//!                     children: vec![],
//!                 }
//!             ],
//!         }
//!     ],
//! };
//! # Ok(())
//! # }
//! ```
//!
//! ### Parsing Template Format
//!
//! ```rust,no_run
//! use ggen_core::templates::format::TemplateFormat;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let yaml = r#"
//! nodes:
//!   - name: src
//!     type: directory
//!     children:
//!       - name: main.rs
//!         type: file
//!         content: "fn main() {}"
//! "#;
//!
//! let template: TemplateFormat = serde_yaml::from_str(yaml)?;
//! # Ok(())
//! # }
//! ```

use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// Node type in the file tree template
///
/// Represents whether a node in the file tree is a directory or a file.
///
/// # Examples
///
/// ```rust
/// use ggen_core::templates::format::NodeType;
///
/// let dir_type = NodeType::Directory;
/// let file_type = NodeType::File;
///
/// assert_ne!(dir_type, file_type);
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum NodeType {
    /// Directory node - can contain child nodes
    Directory,
    /// File node - contains content or references a template
    File,
}

/// A node in the file tree template
///
/// Represents either a file or directory in the generated file tree.
/// Directory nodes can contain children, while file nodes contain content
/// or reference template files.
///
/// # Examples
///
/// ## Creating a directory node
///
/// ```rust
/// use ggen_core::templates::format::FileTreeNode;
///
/// let dir = FileTreeNode::directory("src");
/// assert_eq!(dir.name, "src");
/// ```
///
/// ## Creating a file node with content
///
/// ```rust
/// use ggen_core::templates::format::FileTreeNode;
///
/// let file = FileTreeNode::file_with_content("main.rs", "fn main() {}");
/// assert_eq!(file.name, "main.rs");
/// assert_eq!(file.content, Some("fn main() {}".to_string()));
/// ```
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
///
/// Represents a complete file tree template with metadata, variables, defaults,
/// and RDF annotations. This is the top-level structure for file tree templates.
///
/// # Examples
///
/// ```rust
/// use ggen_core::templates::format::{TemplateFormat, FileTreeNode};
///
/// let mut format = TemplateFormat::new("my-template");
/// format.add_variable("service_name");
/// format.add_default("port", "8080");
/// format.add_node(FileTreeNode::directory("src"));
///
/// assert_eq!(format.name, "my-template");
/// ```
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
    ///
    /// Creates an empty template format with the given name. Variables, defaults,
    /// and nodes can be added using builder methods.
    ///
    /// # Arguments
    ///
    /// * `name` - Template name (must be non-empty)
    ///
    /// # Returns
    ///
    /// A new `TemplateFormat` with empty variables, defaults, and tree.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::format::TemplateFormat;
    ///
    /// let format = TemplateFormat::new("my-template");
    /// assert_eq!(format.name, "my-template");
    /// assert!(format.variables.is_empty());
    /// assert!(format.tree.is_empty());
    /// ```
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
    ///
    /// Adds a required variable to the template. Variables must be provided
    /// when generating from this template (unless they have defaults).
    /// Returns `&mut Self` for method chaining.
    ///
    /// # Arguments
    ///
    /// * `var` - Variable name to add
    ///
    /// # Returns
    ///
    /// `&mut Self` for method chaining.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::format::TemplateFormat;
    ///
    /// let mut format = TemplateFormat::new("my-template");
    /// format.add_variable("service_name")
    ///       .add_variable("port");
    ///
    /// assert_eq!(format.variables.len(), 2);
    /// assert!(format.variables.contains(&"service_name".to_string()));
    /// ```
    pub fn add_variable(&mut self, var: impl Into<String>) -> &mut Self {
        self.variables.push(var.into());
        self
    }

    /// Add a default value for a variable
    ///
    /// Sets a default value for a variable. Defaults are only applied if the
    /// variable is not provided during generation. Returns `&mut Self` for method chaining.
    ///
    /// # Arguments
    ///
    /// * `key` - Variable name
    /// * `value` - Default value (as string)
    ///
    /// # Returns
    ///
    /// `&mut Self` for method chaining.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::format::TemplateFormat;
    ///
    /// let mut format = TemplateFormat::new("my-template");
    /// format.add_variable("port")
    ///       .add_default("port", "8080");
    ///
    /// assert_eq!(format.defaults.get("port"), Some(&"8080".to_string()));
    /// ```
    pub fn add_default(&mut self, key: impl Into<String>, value: impl Into<String>) -> &mut Self {
        self.defaults.insert(key.into(), value.into());
        self
    }

    /// Add a tree node
    ///
    /// Adds a root-level node to the file tree. Returns `&mut Self` for method chaining.
    ///
    /// # Arguments
    ///
    /// * `node` - File tree node to add
    ///
    /// # Returns
    ///
    /// `&mut Self` for method chaining.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::format::{TemplateFormat, FileTreeNode};
    ///
    /// let mut format = TemplateFormat::new("my-template");
    /// format.add_node(FileTreeNode::directory("src"))
    ///       .add_node(FileTreeNode::directory("tests"));
    ///
    /// assert_eq!(format.tree.len(), 2);
    /// ```
    pub fn add_node(&mut self, node: FileTreeNode) -> &mut Self {
        self.tree.push(node);
        self
    }

    /// Parse from YAML string
    ///
    /// Parses a template format from a YAML string. The YAML should match
    /// the `TemplateFormat` structure.
    ///
    /// # Arguments
    ///
    /// * `yaml` - YAML string to parse
    ///
    /// # Returns
    ///
    /// A parsed `TemplateFormat` on success.
    ///
    /// # Errors
    ///
    /// Returns an error if the YAML is invalid or doesn't match the expected structure.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::format::TemplateFormat;
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
    /// let format = TemplateFormat::from_yaml(yaml)?;
    /// assert_eq!(format.name, "my-template");
    /// # Ok(())
    /// # }
    /// ```
    pub fn from_yaml(yaml: &str) -> Result<Self> {
        serde_yaml::from_str(yaml).map_err(|e| {
            Error::with_context("Failed to parse template format from YAML", &e.to_string())
        })
    }

    /// Serialize to YAML string
    ///
    /// Converts this template format to a YAML string representation.
    ///
    /// # Returns
    ///
    /// YAML string representation of the template format.
    ///
    /// # Errors
    ///
    /// Returns an error if serialization fails.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::format::{TemplateFormat, FileTreeNode};
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let mut format = TemplateFormat::new("my-template");
    /// format.add_node(FileTreeNode::directory("src"));
    ///
    /// let yaml = format.to_yaml()?;
    /// assert!(yaml.contains("name: my-template"));
    /// # Ok(())
    /// # }
    /// ```
    pub fn to_yaml(&self) -> Result<String> {
        serde_yaml::to_string(self).map_err(|e| {
            Error::with_context(
                "Failed to serialize template format to YAML",
                &e.to_string(),
            )
        })
    }

    /// Validate the template format
    ///
    /// Validates that the template format is well-formed:
    /// - Name is not empty
    /// - Tree contains at least one node
    /// - All nodes are valid (file nodes have content/template, directories don't)
    ///
    /// # Returns
    ///
    /// `Ok(())` if the template is valid.
    ///
    /// # Errors
    ///
    /// Returns an error if validation fails, with a message describing the issue.
    ///
    /// # Examples
    ///
    /// ## Success case
    ///
    /// ```rust
    /// use ggen_core::templates::format::{TemplateFormat, FileTreeNode};
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let mut format = TemplateFormat::new("my-template");
    /// format.add_node(FileTreeNode::directory("src"));
    ///
    /// format.validate()?; // Ok
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// ## Error case - empty tree
    ///
    /// ```rust
    /// use ggen_core::templates::format::TemplateFormat;
    ///
    /// let format = TemplateFormat::new("my-template");
    /// let result = format.validate();
    /// assert!(result.is_err());
    /// ```
    ///
    /// ## Error case - invalid file node
    ///
    /// ```rust
    /// use ggen_core::templates::format::{TemplateFormat, FileTreeNode, NodeType};
    ///
    /// let mut format = TemplateFormat::new("my-template");
    /// let mut invalid_file = FileTreeNode {
    ///     node_type: NodeType::File,
    ///     name: "test.rs".to_string(),
    ///     children: vec![],
    ///     content: None,
    ///     template: None,
    ///     mode: None,
    /// };
    /// format.add_node(invalid_file);
    ///
    /// let result = format.validate();
    /// assert!(result.is_err());
    /// ```
    pub fn validate(&self) -> Result<()> {
        if self.name.is_empty() {
            return Err(ggen_utils::error::Error::new(
                "Template name cannot be empty",
            ));
        }

        if self.tree.is_empty() {
            return Err(ggen_utils::error::Error::new(
                "Template must contain at least one tree node",
            ));
        }

        self.validate_nodes(&self.tree)?;

        Ok(())
    }

    fn validate_nodes(&self, nodes: &[FileTreeNode]) -> Result<()> {
        #![allow(clippy::only_used_in_recursion)]
        for node in nodes {
            if node.name.is_empty() {
                return Err(ggen_utils::error::Error::new("Node name cannot be empty"));
            }

            match node.node_type {
                NodeType::File => {
                    if node.content.is_none() && node.template.is_none() {
                        return Err(ggen_utils::error::Error::new(&format!(
                            "File node '{}' must have either content or template",
                            node.name
                        )));
                    }
                    if !node.children.is_empty() {
                        return Err(ggen_utils::error::Error::new(&format!(
                            "File node '{}' cannot have children",
                            node.name
                        )));
                    }
                }
                NodeType::Directory => {
                    if node.content.is_some() || node.template.is_some() {
                        return Err(ggen_utils::error::Error::new(&format!(
                            "Directory node '{}' cannot have content or template",
                            node.name
                        )));
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
    ///
    /// Creates a directory node with no children. Children can be added
    /// using `add_child()`.
    ///
    /// # Arguments
    ///
    /// * `name` - Directory name (may contain template variables like `{{ name }}`)
    ///
    /// # Returns
    ///
    /// A new `FileTreeNode` with `NodeType::Directory`.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::format::{FileTreeNode, NodeType};
    ///
    /// let dir = FileTreeNode::directory("src");
    /// assert_eq!(dir.node_type, NodeType::Directory);
    /// assert_eq!(dir.name, "src");
    /// assert!(dir.children.is_empty());
    /// ```
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
    ///
    /// Creates a file node with inline content that will be written directly
    /// to the generated file. The content may contain template variables.
    ///
    /// # Arguments
    ///
    /// * `name` - File name (may contain template variables)
    /// * `content` - File content (may contain template variables)
    ///
    /// # Returns
    ///
    /// A new `FileTreeNode` with `NodeType::File` and inline content.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::format::{FileTreeNode, NodeType};
    ///
    /// let file = FileTreeNode::file_with_content("main.rs", "fn main() {}");
    /// assert_eq!(file.node_type, NodeType::File);
    /// assert_eq!(file.name, "main.rs");
    /// assert_eq!(file.content, Some("fn main() {}".to_string()));
    /// ```
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
    ///
    /// Creates a file node that references an external template file.
    /// The template will be loaded and rendered during generation.
    ///
    /// # Arguments
    ///
    /// * `name` - File name (may contain template variables)
    /// * `template` - Path to template file (relative to template base directory)
    ///
    /// # Returns
    ///
    /// A new `FileTreeNode` with `NodeType::File` and a template reference.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::format::{FileTreeNode, NodeType};
    ///
    /// let file = FileTreeNode::file_with_template("lib.rs", "templates/lib.rs.tera");
    /// assert_eq!(file.node_type, NodeType::File);
    /// assert_eq!(file.name, "lib.rs");
    /// assert_eq!(file.template, Some("templates/lib.rs.tera".to_string()));
    /// ```
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
    ///
    /// Adds a child node to this directory. Only valid for directory nodes.
    /// Returns `&mut Self` for method chaining.
    ///
    /// # Arguments
    ///
    /// * `child` - Child node to add
    ///
    /// # Returns
    ///
    /// `&mut Self` for method chaining.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::format::FileTreeNode;
    ///
    /// let mut dir = FileTreeNode::directory("src");
    /// dir.add_child(FileTreeNode::file_with_content("main.rs", "fn main() {}"));
    ///
    /// assert_eq!(dir.children.len(), 1);
    /// assert_eq!(dir.children[0].name, "main.rs");
    /// ```
    pub fn add_child(&mut self, child: FileTreeNode) -> &mut Self {
        self.children.push(child);
        self
    }

    /// Set file permissions
    ///
    /// Sets Unix file permissions (mode) for this file node. Only valid for file nodes.
    /// Returns `Self` for method chaining.
    ///
    /// # Arguments
    ///
    /// * `mode` - Unix file mode (e.g., `0o755` for executable)
    ///
    /// # Returns
    ///
    /// `Self` for method chaining.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::format::FileTreeNode;
    ///
    /// let file = FileTreeNode::file_with_content("script.sh", "#!/bin/bash")
    ///     .with_mode(0o755);
    ///
    /// assert_eq!(file.mode, Some(0o755));
    /// ```
    pub fn with_mode(mut self, mode: u32) -> Self {
        self.mode = Some(mode);
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::{async_test, test};

    test!(test_directory_node, {
        let node = FileTreeNode::directory("src");
        assert_eq!(node.node_type, NodeType::Directory);
        assert_eq!(node.name, "src");
        assert!(node.children.is_empty());
    });

    test!(test_file_node_with_content, {
        let node = FileTreeNode::file_with_content("main.rs", "fn main() {}");
        assert_eq!(node.node_type, NodeType::File);
        assert_eq!(node.name, "main.rs");
        assert_eq!(node.content, Some("fn main() {}".to_string()));
        assert_eq!(node.template, None);
    });

    test!(test_file_node_with_template, {
        let node = FileTreeNode::file_with_template("lib.rs", "templates/lib.rs.tera");
        assert_eq!(node.node_type, NodeType::File);
        assert_eq!(node.name, "lib.rs");
        assert_eq!(node.template, Some("templates/lib.rs.tera".to_string()));
        assert_eq!(node.content, None);
    });

    test!(test_template_format_creation, {
        let mut format = TemplateFormat::new("test-template");
        format.add_variable("service_name");
        format.add_default("port", "8080");

        assert_eq!(format.name, "test-template");
        assert_eq!(format.variables, vec!["service_name"]);
        assert_eq!(format.defaults.get("port"), Some(&"8080".to_string()));
    });

    test!(test_template_format_validation, {
        let mut format = TemplateFormat::new("test");
        format.add_node(FileTreeNode::directory("src"));

        assert!(format.validate().is_ok());
    });

    test!(test_empty_template_validation_fails, {
        let format = TemplateFormat::new("test");
        assert!(format.validate().is_err());
    });
}
