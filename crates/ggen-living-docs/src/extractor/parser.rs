//! Rust AST parser for code extraction

use super::visitor::EntityVisitor;
use crate::{Error, Result, ontology::CodeEntity};
use std::path::Path;
use syn::{parse_file, File};
use tracing::debug;

/// Rust code parser using syn
pub struct RustParser;

impl RustParser {
    /// Create a new Rust parser
    pub fn new() -> Self {
        Self
    }

    /// Parse a Rust file and extract entities
    pub fn parse_file(&self, content: &str, file_path: &Path) -> Result<Vec<CodeEntity>> {
        debug!("Parsing Rust file: {:?}", file_path);

        // Parse the syntax tree
        let syntax_tree: File = parse_file(content)
            .map_err(|e| Error::Parse(format!("Failed to parse file: {}", e)))?;

        // Visit the AST and extract entities
        let mut visitor = EntityVisitor::new(file_path);
        visitor.visit_file(&syntax_tree);

        Ok(visitor.entities())
    }
}

impl Default for RustParser {
    fn default() -> Self {
        Self::new()
    }
}
