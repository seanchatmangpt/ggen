//! Architectural Integration Contracts
//!
//! This module defines trait-based contracts that enforce integration
//! boundaries between subsystems at compile time.
//!
//! # Design Principle
//!
//! Explicit contracts prevent integration failures by making API boundaries
//! type-safe and version-aware.

use std::path::Path;
use thiserror::Error;

// ============================================================================
// Core Contracts
// ============================================================================

/// Contract for any system that provides templates to the CLI
///
/// This trait defines the required interface that all template provider
/// implementations must satisfy. The compiler enforces that all methods
/// are implemented and type-safe.
///
/// # Examples
///
/// ```rust
/// use crate::prevention::contracts::TemplateProvider;
///
/// struct FilesystemProvider { /* ... */ }
///
/// impl TemplateProvider for FilesystemProvider {
///     fn discover(&self, path: &Path) -> Result<Vec<Template>, ProviderError> {
///         // Implementation
///     }
///     // ... other methods
/// }
/// ```
pub trait TemplateProvider: Send + Sync {
    /// Discover all templates in the given path
    ///
    /// # Errors
    ///
    /// Returns error if:
    /// - Path does not exist
    /// - Permission denied
    /// - IO error during discovery
    fn discover(&self, path: &Path) -> Result<Vec<Template>, ProviderError>;

    /// Validate a template's structure and syntax
    ///
    /// # Errors
    ///
    /// Returns error if:
    /// - Template syntax is invalid
    /// - Required fields missing
    /// - Constraints violated
    fn validate(&self, template: &Template) -> Result<(), ProviderError>;

    /// Render a template with the given context
    ///
    /// # Errors
    ///
    /// Returns error if:
    /// - Template is invalid
    /// - Context missing required variables
    /// - Rendering engine failure
    fn render(&self, template: &Template, context: Context) -> Result<String, ProviderError>;

    /// Get template metadata
    ///
    /// # Errors
    ///
    /// Returns error if template metadata cannot be read
    fn metadata(&self, template: &Template) -> Result<TemplateMetadata, ProviderError>;

    /// Get provider version (for compatibility checking)
    fn version(&self) -> Version {
        Version::new(1, 0, 0)
    }
}

/// Contract for CLI bridge implementations
///
/// This trait defines the interface between the CLI parser and the
/// command execution system.
pub trait CliBridge: Send + Sync {
    /// Parse CLI arguments into structured command
    ///
    /// # Errors
    ///
    /// Returns error if:
    /// - Invalid arguments
    /// - Missing required arguments
    /// - Parsing failure
    fn parse(&self, args: Vec<String>) -> Result<Command, BridgeError>;

    /// Execute command with template provider
    ///
    /// # Errors
    ///
    /// Returns error if:
    /// - Command execution fails
    /// - Provider error
    /// - Output formatting error
    fn execute<P: TemplateProvider>(
        &self,
        cmd: Command,
        provider: &P,
    ) -> Result<Output, BridgeError>;

    /// Format output for display
    fn format(&self, output: Output) -> String;

    /// Get bridge version (for compatibility checking)
    fn version(&self) -> Version {
        Version::new(1, 0, 0)
    }
}

/// Contract for rendering engines
///
/// This trait allows multiple rendering engine implementations
/// (Handlebars, Tera, Liquid, etc.) to be used interchangeably.
pub trait RenderEngine: Send + Sync {
    /// Render template content with context
    ///
    /// # Errors
    ///
    /// Returns error if rendering fails
    fn render(&self, content: &str, context: &Context) -> Result<String, RenderError>;

    /// Validate template syntax
    ///
    /// # Errors
    ///
    /// Returns error if syntax is invalid
    fn validate_syntax(&self, content: &str) -> Result<(), RenderError>;

    /// Get supported features
    fn features(&self) -> RenderFeatures;
}

// ============================================================================
// Supporting Types
// ============================================================================

/// Template representation
#[derive(Debug, Clone)]
pub struct Template {
    pub name: String,
    pub path: String,
    pub content: String,
}

/// Template metadata
#[derive(Debug, Clone)]
pub struct TemplateMetadata {
    pub name: String,
    pub version: Version,
    pub author: Option<String>,
    pub description: Option<String>,
    pub tags: Vec<String>,
}

/// Rendering context
#[derive(Debug, Clone, Default)]
pub struct Context {
    pub variables: std::collections::HashMap<String, serde_json::Value>,
}

/// CLI command representation
#[derive(Debug, Clone)]
pub struct Command {
    pub action: CommandAction,
    pub args: Vec<String>,
    pub flags: std::collections::HashMap<String, String>,
}

/// Command actions
#[derive(Debug, Clone)]
pub enum CommandAction {
    Generate,
    List,
    Validate,
    Search,
}

/// Command output
#[derive(Debug, Clone)]
pub struct Output {
    pub success: bool,
    pub message: String,
    pub data: Option<serde_json::Value>,
}

/// Version representation
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Version {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
}

impl Version {
    pub fn new(major: u32, minor: u32, patch: u32) -> Self {
        Self {
            major,
            minor,
            patch,
        }
    }

    /// Check if this version is compatible with another version
    ///
    /// Compatible if:
    /// - Major versions match (breaking changes)
    /// - This version >= other version
    pub fn is_compatible_with(&self, other: &Version) -> bool {
        self.major == other.major && self >= other
    }
}

impl std::fmt::Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

/// Render engine features
#[derive(Debug, Clone)]
pub struct RenderFeatures {
    pub supports_partials: bool,
    pub supports_helpers: bool,
    pub supports_filters: bool,
    pub supports_inheritance: bool,
}

// ============================================================================
// Error Types
// ============================================================================

#[derive(Error, Debug)]
pub enum ProviderError {
    #[error("Template not found: {path}")]
    TemplateNotFound { path: String },

    #[error("Invalid template syntax: {reason}")]
    InvalidSyntax { reason: String },

    #[error("Validation failed: {reason}")]
    ValidationFailed { reason: String },

    #[error("Rendering failed: {reason}")]
    RenderingFailed { reason: String },

    #[error("Version incompatibility: expected {expected}, got {actual}")]
    VersionIncompatible { expected: String, actual: String },

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

#[derive(Error, Debug)]
pub enum BridgeError {
    #[error("Invalid command: {command}")]
    InvalidCommand { command: String },

    #[error("Missing required argument: {arg}")]
    MissingArgument { arg: String },

    #[error("Execution failed: {reason}")]
    ExecutionFailed { reason: String },

    #[error("Provider error: {0}")]
    Provider(#[from] ProviderError),
}

#[derive(Error, Debug)]
pub enum RenderError {
    #[error("Syntax error at line {line}, column {column}: {message}")]
    SyntaxError {
        line: usize,
        column: usize,
        message: String,
    },

    #[error("Missing variable: {variable}")]
    MissingVariable { variable: String },

    #[error("Rendering failed: {reason}")]
    RenderingFailed { reason: String },
}

// ============================================================================
// Contract Verification
// ============================================================================

/// Verify that a template provider satisfies the contract
///
/// This function can be used in tests to ensure all implementations
/// behave correctly according to the contract.
///
/// # Examples
///
/// ```rust
/// #[test]
/// fn test_filesystem_provider_contract() {
///     let provider = FilesystemTemplateProvider::new();
///     verify_template_provider_contract(provider).unwrap();
/// }
/// ```
#[cfg(test)]
pub fn verify_template_provider_contract<P: TemplateProvider>(
    provider: P,
) -> Result<(), String> {
    use std::path::PathBuf;

    // Contract requirement: discover must return valid templates
    let test_path = PathBuf::from("tests/fixtures/templates");
    let templates = provider
        .discover(&test_path)
        .map_err(|e| format!("discover failed: {}", e))?;

    if templates.is_empty() {
        return Err("Contract violation: discover returned empty list for valid path".to_string());
    }

    // Contract requirement: all discovered templates must be valid
    for template in &templates {
        provider
            .validate(template)
            .map_err(|e| format!("validate failed for {}: {}", template.name, e))?;
    }

    // Contract requirement: render must succeed for valid template
    if let Some(template) = templates.first() {
        let context = Context::default();
        provider
            .render(template, context)
            .map_err(|e| format!("render failed: {}", e))?;
    }

    // Contract requirement: metadata must be available
    if let Some(template) = templates.first() {
        let _metadata = provider
            .metadata(template)
            .map_err(|e| format!("metadata failed: {}", e))?;
    }

    Ok(())
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version_compatibility() {
        let v1_0_0 = Version::new(1, 0, 0);
        let v1_1_0 = Version::new(1, 1, 0);
        let v2_0_0 = Version::new(2, 0, 0);

        // Same major version, newer minor version is compatible
        assert!(v1_1_0.is_compatible_with(&v1_0_0));

        // Different major version is not compatible
        assert!(!v2_0_0.is_compatible_with(&v1_0_0));

        // Older version is not compatible with newer
        assert!(!v1_0_0.is_compatible_with(&v1_1_0));
    }

    #[test]
    fn test_version_display() {
        let version = Version::new(1, 2, 3);
        assert_eq!(version.to_string(), "1.2.3");
    }

    // Real filesystem-based provider for testing (Chicago TDD - real collaborator)
    struct FilesystemTemplateProvider {
        base_path: std::path::PathBuf,
    }

    impl TemplateProvider for FilesystemTemplateProvider {
        fn discover(&self, path: &Path) -> Result<Vec<Template>, ProviderError> {
            // Real filesystem discovery - iterate actual files
            let mut templates = Vec::new();
            if path.exists() && path.is_dir() {
                if let Ok(entries) = std::fs::read_dir(path) {
                    for entry in entries.flatten() {
                        if let Ok(metadata) = entry.metadata() {
                            if metadata.is_file() {
                                if let Ok(filename) = entry.file_name().into_string() {
                                    if filename.ends_with(".tmpl") {
                                        if let Ok(content) = std::fs::read_to_string(entry.path()) {
                                            templates.push(Template {
                                                name: filename[..filename.len() - 5].to_string(),
                                                path: entry.path().to_string_lossy().to_string(),
                                                content,
                                            });
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            Ok(templates)
        }

        fn validate(&self, template: &Template) -> Result<(), ProviderError> {
            // Real validation - check template syntax is valid
            if template.content.is_empty() {
                return Err(ProviderError {
                    message: "Template content cannot be empty".to_string(),
                });
            }
            // Basic template syntax check
            if !template.content.contains("{{") {
                return Err(ProviderError {
                    message: "Template must contain template variables".to_string(),
                });
            }
            Ok(())
        }

        fn render(&self, template: &Template, _context: Context) -> Result<String, ProviderError> {
            // Real rendering - return actual content for now
            // In production, this would use a real template engine like Tera
            Ok(template.content.clone())
        }

        fn metadata(&self, template: &Template) -> Result<TemplateMetadata, ProviderError> {
            // Real metadata extraction
            Ok(TemplateMetadata {
                name: template.name.clone(),
                version: Version::new(1, 0, 0),
                author: None,
                description: None,
                tags: vec![],
            })
        }
    }

    #[test]
    fn test_filesystem_provider_satisfies_contract() {
        // Chicago TDD: Use real filesystem with tempfile
        use std::fs;
        use tempfile::TempDir;

        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let template_path = temp_dir.path().join("test.tmpl");

        // Real template file creation
        fs::write(&template_path, "Hello {{ name }}!")
            .expect("Failed to write template file");

        let provider = FilesystemTemplateProvider {
            base_path: temp_dir.path().to_path_buf(),
        };

        // Real discovery from filesystem
        let templates = provider
            .discover(temp_dir.path())
            .expect("Discovery should succeed");

        assert_eq!(templates.len(), 1, "Should discover one template");
        assert_eq!(templates[0].name, "test");
        assert!(templates[0].content.contains("{{ name }}"));

        // Real validation
        assert!(provider.validate(&templates[0]).is_ok(), "Valid template should pass");

        // Real metadata extraction
        let metadata = provider
            .metadata(&templates[0])
            .expect("Metadata should be readable");
        assert_eq!(metadata.name, "test");
    }

    #[test]
    fn test_template_validation_rejects_invalid_templates() {
        // Chicago TDD: Verify real validation logic
        use tempfile::TempDir;
        use std::fs;

        let temp_dir = TempDir::new().expect("Failed to create temp dir");

        let provider = FilesystemTemplateProvider {
            base_path: temp_dir.path().to_path_buf(),
        };

        // Invalid template: empty content
        let empty_template = Template {
            name: "empty".to_string(),
            path: "empty.tmpl".to_string(),
            content: "".to_string(),
        };
        assert!(
            provider.validate(&empty_template).is_err(),
            "Empty template should fail validation"
        );

        // Invalid template: no template variables
        let no_vars_template = Template {
            name: "no_vars".to_string(),
            path: "no_vars.tmpl".to_string(),
            content: "Just plain text".to_string(),
        };
        assert!(
            provider.validate(&no_vars_template).is_err(),
            "Template without variables should fail validation"
        );

        // Valid template: contains template variables
        let valid_template = Template {
            name: "valid".to_string(),
            path: "valid.tmpl".to_string(),
            content: "Hello {{ name }}!".to_string(),
        };
        assert!(
            provider.validate(&valid_template).is_ok(),
            "Valid template should pass validation"
        );
    }
}
