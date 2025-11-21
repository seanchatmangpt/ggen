//! Compile-Time State Machine Prevention System
//!
//! This module implements PhantomData-based state machines to prevent
//! invalid state transitions at compile time.
//!
//! # Design Principle
//!
//! Make invalid states unrepresentable through the type system.
//! The compiler enforces correct usage at zero runtime cost.

use std::marker::PhantomData;
use std::path::Path;

// ============================================================================
// Type-Level States
// ============================================================================

/// Type-level state: Registry has been created but not initialized
pub struct Uninitialized;

/// Type-level state: Registry has been initialized with templates
pub struct Initialized;

/// Type-level state: Registry templates have been validated
pub struct Validated;

// ============================================================================
// Registry State Machine
// ============================================================================

/// Template registry with compile-time state tracking
///
/// # State Transitions
///
/// ```text
/// Uninitialized --[initialize]--> Initialized --[validate]--> Validated
/// ```
///
/// # Examples
///
/// ```rust
/// use ggen_core::prevention::state_machine::Registry;
/// use std::path::Path;
///
/// // ✅ VALID: Proper state transitions
/// let registry = Registry::new()
///     .initialize(Path::new("templates"))?
///     .validate()?;
/// registry.search("pattern")?;
///
/// // ❌ INVALID: Compiler prevents this
/// // let registry = Registry::new();
/// // registry.search("pattern")?;  // ERROR: No method `search` for Registry<Uninitialized>
/// ```
pub struct Registry<State = Uninitialized> {
    templates: Vec<Template>,
    _state: PhantomData<State>,
}

// ============================================================================
// Uninitialized State Methods
// ============================================================================

impl Registry<Uninitialized> {
    /// Create a new uninitialized registry
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::prevention::state_machine::Registry;
    ///
    /// let registry = Registry::new();
    /// // Can only call initialize() at this point
    /// ```
    pub fn new() -> Self {
        Registry {
            templates: Vec::new(),
            _state: PhantomData,
        }
    }

    /// Initialize registry by discovering templates at the given path
    ///
    /// # State Transition
    ///
    /// `Uninitialized` → `Initialized`
    ///
    /// # Errors
    ///
    /// Returns error if:
    /// - Path does not exist
    /// - No templates found
    /// - IO error during discovery
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::prevention::state_machine::Registry;
    /// use std::path::Path;
    ///
    /// let registry = Registry::new()
    ///     .initialize(Path::new("templates"))?;
    /// // Can now call validate()
    /// ```
    pub fn initialize(self, path: &Path) -> Result<Registry<Initialized>, RegistryError> {
        let templates = discover_templates(path)?;

        if templates.is_empty() {
            return Err(RegistryError::NoTemplatesFound {
                path: path.to_string_lossy().to_string(),
            });
        }

        Ok(Registry {
            templates,
            _state: PhantomData,
        })
    }
}

// ============================================================================
// Initialized State Methods
// ============================================================================

impl Registry<Initialized> {
    /// Validate all templates in the registry
    ///
    /// # State Transition
    ///
    /// `Initialized` → `Validated`
    ///
    /// # Errors
    ///
    /// Returns error if any template fails validation
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::prevention::state_machine::Registry;
    /// use std::path::Path;
    ///
    /// let registry = Registry::new()
    ///     .initialize(Path::new("templates"))?
    ///     .validate()?;
    /// // Can now call search() and render()
    /// ```
    pub fn validate(self) -> Result<Registry<Validated>, RegistryError> {
        for template in &self.templates {
            validate_template(template)?;
        }

        Ok(Registry {
            templates: self.templates,
            _state: PhantomData,
        })
    }

    /// Get number of templates (read-only operation, safe in Initialized state)
    pub fn count(&self) -> usize {
        self.templates.len()
    }
}

// ============================================================================
// Validated State Methods
// ============================================================================

impl Registry<Validated> {
    /// Search for templates matching the given query
    ///
    /// Only available in `Validated` state.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::prevention::state_machine::Registry;
    /// use std::path::Path;
    ///
    /// let registry = Registry::new()
    ///     .initialize(Path::new("templates"))?
    ///     .validate()?;
    ///
    /// let results = registry.search("rust")?;
    /// ```
    pub fn search(&self, query: &str) -> Result<Vec<&Template>, RegistryError> {
        let results = self
            .templates
            .iter()
            .filter(|t| t.matches_query(query))
            .collect();
        Ok(results)
    }

    /// Render a template with the given context
    ///
    /// Only available in `Validated` state.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::prevention::state_machine::Registry;
    /// use std::path::Path;
    ///
    /// let registry = Registry::new()
    ///     .initialize(Path::new("templates"))?
    ///     .validate()?;
    ///
    /// let template = registry.search("example")?.first().unwrap();
    /// let output = registry.render(template, context)?;
    /// ```
    pub fn render(&self, template: &Template, context: Context) -> Result<String, RegistryError> {
        render_template(template, context)
    }

    /// Get all templates (read-only operation)
    pub fn templates(&self) -> &[Template] {
        &self.templates
    }

    /// Get number of templates
    pub fn count(&self) -> usize {
        self.templates.len()
    }
}

// ============================================================================
// Supporting Types
// ============================================================================

/// Template representation
#[derive(Debug, Clone)]
pub struct Template {
    name: String,
    path: String,
    content: String,
}

impl Template {
    fn matches_query(&self, query: &str) -> bool {
        self.name.contains(query) || self.path.contains(query)
    }
}

/// Rendering context
#[derive(Debug, Clone, Default)]
pub struct Context {
    variables: std::collections::HashMap<String, String>,
}

// ============================================================================
// Error Types
// ============================================================================

use thiserror::Error;

#[derive(Error, Debug)]
pub enum RegistryError {
    #[error("No templates found at path: {path}")]
    NoTemplatesFound { path: String },

    #[error("Template validation failed: {reason}")]
    ValidationFailed { reason: String },

    #[error("Template not found: {name}")]
    TemplateNotFound { name: String },

    #[error("Rendering failed: {reason}")]
    RenderingFailed { reason: String },

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

// ============================================================================
// Internal Functions
// ============================================================================

fn discover_templates(path: &Path) -> Result<Vec<Template>, RegistryError> {
    // Implementation would scan directory for .tmpl files
    // For now, placeholder
    Ok(vec![Template {
        name: "example".to_string(),
        path: path.join("example.tmpl").to_string_lossy().to_string(),
        content: "{{ content }}".to_string(),
    }])
}

fn validate_template(template: &Template) -> Result<(), RegistryError> {
    // Implementation would validate template syntax
    // For now, placeholder
    if template.content.is_empty() {
        return Err(RegistryError::ValidationFailed {
            reason: "Template content is empty".to_string(),
        });
    }
    Ok(())
}

fn render_template(template: &Template, _context: Context) -> Result<String, RegistryError> {
    // Implementation would render template with context
    // For now, placeholder
    Ok(template.content.clone())
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[allow(clippy::expect_used)]
    fn test_state_machine_valid_transitions() {
        // ✅ This compiles and runs correctly
        let registry = Registry::new();
        let registry = registry
            .initialize(Path::new("templates"))
            .expect("initialize failed");
        let registry = registry.validate().expect("validate failed");
        let _results = registry.search("test").expect("search failed");
    }

    #[test]
    fn test_uninitialized_count_not_available() {
        // This test documents that count() is not available in Uninitialized state
        // Uncommenting the following would cause a compile error:
        // let registry = Registry::new();
        // let _ = registry.count();  // ERROR: No method `count` for Registry<Uninitialized>
    }

    #[test]
    fn test_initialized_search_not_available() {
        // This test documents that search() is not available in Initialized state
        // Uncommenting the following would cause a compile error:
        // let registry = Registry::new()
        //     .initialize(Path::new("templates"))
        //     .unwrap();
        // let _ = registry.search("test");  // ERROR: No method `search` for Registry<Initialized>
    }

    #[test]
    #[allow(clippy::expect_used)]
    fn test_validated_state_methods_available() {
        let registry = Registry::new()
            .initialize(Path::new("templates"))
            .expect("initialize failed")
            .validate()
            .expect("validate failed");

        // All methods available in Validated state
        assert!(registry.count() > 0);
        assert!(registry.templates().len() > 0);
        assert!(registry.search("example").is_ok());
    }
}
