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
/// use ggen_core::prevention::contracts::TemplateProvider;
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
        &self, cmd: Command, provider: &P,
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
pub fn verify_template_provider_contract<P: TemplateProvider>(provider: P) -> Result<(), String> {
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

    // Example implementation for testing
    struct MockTemplateProvider;

    impl TemplateProvider for MockTemplateProvider {
        fn discover(&self, _path: &Path) -> Result<Vec<Template>, ProviderError> {
            Ok(vec![Template {
                name: "test".to_string(),
                path: "test.tmpl".to_string(),
                content: "{{ name }}".to_string(),
            }])
        }

        fn validate(&self, _template: &Template) -> Result<(), ProviderError> {
            Ok(())
        }

        fn render(&self, template: &Template, _context: Context) -> Result<String, ProviderError> {
            Ok(template.content.clone())
        }

        fn metadata(&self, template: &Template) -> Result<TemplateMetadata, ProviderError> {
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
    fn test_mock_provider_satisfies_contract() {
        let provider = MockTemplateProvider;
        verify_template_provider_contract(provider).unwrap();
    }
}
