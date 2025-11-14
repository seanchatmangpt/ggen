//! Template package models for marketplace
//!
//! This module provides data structures for representing template packages in the
//! marketplace. Template packages contain one or more template files along with
//! metadata about variables, dependencies, examples, and supported frameworks.
//!
//! ## Types
//!
//! - **TemplatePackage**: Complete template package metadata
//! - **TemplateInfo**: Information about individual template files
//! - **TemplateType**: Type of template (FileTree, SingleFile, Bundle)
//! - **TemplateVariable**: Variable definition with validation
//! - **TemplateExample**: Example usage of the template
//!
//! ## Examples
//!
//! ### Creating a Template Package
//!
//! ```rust,no_run
//! use ggen_marketplace::models::template_package::TemplatePackage;
//!
//! let package = TemplatePackage::new(
//!     "io.ggen.rust.api".to_string(),
//!     "web-service".to_string()
//! );
//! ```
//!
//! ### Adding Template Variables
//!
//! ```rust,no_run
//! use ggen_marketplace::models::template_package::{TemplatePackage, TemplateVariable};
//!
//! # fn example() {
//! let mut package = TemplatePackage::new(
//!     "io.ggen.rust.api".to_string(),
//!     "web-service".to_string()
//! );
//!
//! package.add_variable(TemplateVariable {
//!     name: "service_name".to_string(),
//!     description: "Name of the service".to_string(),
//!     default: Some("MyService".to_string()),
//!     required: true,
//!     pattern: Some(r"^[a-zA-Z][a-zA-Z0-9_]*$".to_string()),
//! });
//! # }
//! ```

use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Template metadata for marketplace packages
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplatePackage {
    /// Package ID
    pub package_id: String,

    /// Template files included in package
    pub templates: Vec<TemplateInfo>,

    /// Template category (e.g., "microservice", "web-app", "cli-tool")
    pub category: String,

    /// Supported languages/frameworks
    pub frameworks: Vec<String>,

    /// Template variables with defaults
    pub variables: Vec<TemplateVariable>,

    /// Dependencies on other packages
    pub dependencies: Vec<String>,

    /// Example usage
    pub examples: Vec<TemplateExample>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateInfo {
    /// Template file name
    pub name: String,

    /// Relative path within package
    pub path: PathBuf,

    /// Template description
    pub description: String,

    /// Template type (e.g., "file-tree", "single-file")
    pub template_type: TemplateType,

    /// Required variables
    pub required_vars: Vec<String>,
}

/// Template type classification
///
/// Represents the structure and organization of a template.
///
/// # Examples
///
/// ```rust
/// use ggen_marketplace::models::TemplateType;
///
/// # fn main() {
/// let template_type = TemplateType::FileTree;
/// match template_type {
///     TemplateType::FileTree => assert!(true),
///     TemplateType::SingleFile => assert!(true),
///     TemplateType::Bundle => assert!(true),
/// }
/// # }
/// ```
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum TemplateType {
    /// YAML file tree template
    FileTree,
    /// Single file template
    SingleFile,
    /// Multi-file template bundle
    Bundle,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateVariable {
    /// Variable name
    pub name: String,

    /// Description
    pub description: String,

    /// Default value (optional)
    pub default: Option<String>,

    /// Is required?
    pub required: bool,

    /// Validation pattern (regex)
    pub pattern: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateExample {
    /// Example name
    pub name: String,

    /// Example description
    pub description: String,

    /// Command to run
    pub command: String,

    /// Expected output description
    pub expected_output: String,
}

impl TemplatePackage {
    /// Create new template package
    pub fn new(package_id: String, category: String) -> Self {
        Self {
            package_id,
            templates: Vec::new(),
            category,
            frameworks: Vec::new(),
            variables: Vec::new(),
            dependencies: Vec::new(),
            examples: Vec::new(),
        }
    }

    /// Add template to package
    pub fn add_template(&mut self, template: TemplateInfo) {
        self.templates.push(template);
    }

    /// Add variable definition
    pub fn add_variable(&mut self, variable: TemplateVariable) {
        self.variables.push(variable);
    }

    /// Add dependency
    pub fn add_dependency(&mut self, dep: String) {
        self.dependencies.push(dep);
    }

    /// Find template by name
    pub fn find_template(&self, name: &str) -> Option<&TemplateInfo> {
        self.templates.iter().find(|t| t.name == name)
    }

    /// List all template names
    pub fn template_names(&self) -> Vec<String> {
        self.templates.iter().map(|t| t.name.clone()).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_template_package() {
        let package =
            TemplatePackage::new("rust-microservice".to_string(), "microservice".to_string());

        assert_eq!(package.package_id, "rust-microservice");
        assert_eq!(package.category, "microservice");
        assert!(package.templates.is_empty());
    }

    #[test]
    fn test_add_template() {
        let mut package =
            TemplatePackage::new("rust-microservice".to_string(), "microservice".to_string());

        let template = TemplateInfo {
            name: "service.yaml".to_string(),
            path: PathBuf::from("templates/service.yaml"),
            description: "Microservice template".to_string(),
            template_type: TemplateType::FileTree,
            required_vars: vec!["service_name".to_string()],
        };

        package.add_template(template);
        assert_eq!(package.templates.len(), 1);
    }

    #[test]
    fn test_find_template() {
        let mut package =
            TemplatePackage::new("rust-microservice".to_string(), "microservice".to_string());

        let template = TemplateInfo {
            name: "service.yaml".to_string(),
            path: PathBuf::from("templates/service.yaml"),
            description: "Microservice template".to_string(),
            template_type: TemplateType::FileTree,
            required_vars: vec!["service_name".to_string()],
        };

        package.add_template(template);

        assert!(package.find_template("service.yaml").is_some());
        assert!(package.find_template("nonexistent").is_none());
    }
}
