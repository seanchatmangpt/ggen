//! Trait for query execution in code generation rules
//!
//! Abstracts the execution of queries (typically SPARQL SELECT) and binding extraction.

use crate::Result;
use std::collections::HashMap;

/// Trait for query execution
///
/// Abstracts the execution of queries and binding extraction.
/// Implementors return bindings (variable name → value) for template rendering.
pub trait Queryable: Send + Sync {
    /// Execute the query and return a vector of binding maps
    ///
    /// Each map represents one result row from the query.
    /// Keys are query variable names (e.g., "className", "fieldName").
    /// Values are result strings (IRIs, literals, etc.).
    fn execute(&self) -> Result<Vec<HashMap<String, String>>>;

    /// Query name for logging/auditing
    fn name(&self) -> &str;

    /// Optional: query source for debugging
    fn source(&self) -> Option<&str> {
        None
    }
}
