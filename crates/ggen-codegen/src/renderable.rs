//! Trait for template rendering in code generation rules
//!
//! Abstracts template rendering with context bindings.

use crate::Result;
use std::collections::HashMap;

/// Trait for template rendering
///
/// Abstracts template rendering with context bindings.
/// Implementors transform query bindings into rendered output.
pub trait Renderable: Send + Sync {
    /// Render template with given context bindings
    ///
    /// # Arguments
    /// * `bindings` - Query variable bindings (e.g., {"className": "TestClass", ...})
    ///
    /// # Returns
    /// Rendered content (typically source code)
    fn render(&self, bindings: &HashMap<String, String>) -> Result<String>;

    /// Template name for logging/auditing
    fn name(&self) -> &str;

    /// Optional: template source for debugging
    fn source(&self) -> Option<&str> {
        None
    }
}
