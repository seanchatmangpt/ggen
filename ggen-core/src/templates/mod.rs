//! Template-to-file-tree generation system
//!
//! This module provides functionality for generating complete file trees from templates.
//! It supports:
//! - Template metadata with RDF annotations
//! - Variable substitution in paths and content
//! - Directory structure creation
//! - Inline content or template file references
//! - Integration with Tera template engine

pub mod file_tree_generator;
pub mod format;
pub mod generator;
pub mod context;

pub use file_tree_generator::{FileTreeTemplate, TemplateParser};
pub use format::{FileTreeNode, NodeType, TemplateFormat};
pub use generator::{FileTreeGenerator, GenerationResult, generate_file_tree};
pub use context::TemplateContext;
