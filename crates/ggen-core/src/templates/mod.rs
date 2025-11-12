//! Template-to-file-tree generation system
//!
//! This module provides functionality for generating complete file trees from templates.
//! It supports:
//! - Template metadata with RDF annotations
//! - Variable substitution in paths and content
//! - Directory structure creation
//! - Inline content or template file references
//! - Integration with Tera template engine

pub mod business_logic;
pub mod context;
pub mod file_tree_generator;
pub mod format;
pub mod frozen;
pub mod generator;

pub use business_logic::BusinessLogicSeparator;
pub use context::TemplateContext;
pub use file_tree_generator::{FileTreeTemplate, TemplateParser};
pub use format::{FileTreeNode, NodeType, TemplateFormat};
pub use frozen::{FrozenMerger, FrozenParser, FrozenSection};
pub use generator::{generate_file_tree, FileTreeGenerator, GenerationResult};
