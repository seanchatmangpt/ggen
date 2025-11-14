//! Template-to-file-tree generation system
//!
//! This module provides functionality for generating complete file trees from templates
//! with support for RDF annotations, variable substitution, and directory structure
//! creation. It enables generating entire project structures from declarative templates.
//!
//! ## Features
//!
//! - **File Tree Generation**: Generate complete directory structures from templates
//! - **RDF Annotations**: Template metadata stored in RDF format
//! - **Variable Substitution**: Dynamic path and content generation
//! - **Template Formats**: Support for multiple template formats (YAML, TOML, JSON)
//! - **Frozen Sections**: Preserve manual edits in generated files
//! - **Business Logic Separation**: Separate template structure from business logic
//! - **Tera Integration**: Full integration with Tera template engine
//!
//! ## Template Format
//!
//! Templates can be defined in YAML, TOML, or JSON format with support for:
//! - File and directory nodes
//! - Inline content or template file references
//! - Variable substitution in paths and content
//! - RDF metadata annotations
//! - Frozen section markers for manual edits
//!
//! ## Examples
//!
//! ### Generating a File Tree
//!
//! ```rust,no_run
//! use ggen_core::templates::{generate_file_tree, TemplateContext};
//! use std::path::Path;
//!
//! # fn main() -> anyhow::Result<()> {
//! let template_path = Path::new("project.tree.toml");
//! let output_dir = Path::new("output");
//! let mut ctx = TemplateContext::new();
//! ctx.insert("project_name", "my-app");
//!
//! let result = generate_file_tree(template_path, output_dir, &ctx)?;
//! println!("Generated {} files", result.files_created);
//! # Ok(())
//! # }
//! ```
//!
//! ### Using Template Parser
//!
//! ```rust,no_run
//! use ggen_core::templates::{TemplateParser, TemplateFormat};
//! use std::path::Path;
//!
//! # fn main() -> anyhow::Result<()> {
//! let parser = TemplateParser::new();
//! let template = parser.parse_file(Path::new("project.tree.toml"), TemplateFormat::Toml)?;
//!
//! println!("Template has {} nodes", template.nodes.len());
//! # Ok(())
//! # }
//! ```

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
