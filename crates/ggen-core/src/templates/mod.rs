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
//! use crate::templates::{generate_file_tree, TemplateContext, TemplateParser};
//!
//! # fn main() -> crate::utils::error::Result<()> {
//! let template = TemplateParser::parse_file("project.tree.yaml")?;
//! let mut ctx = TemplateContext::new();
//! ctx.set("project_name", "my-app")?;
//!
//! let result = generate_file_tree(template, ctx, "output")?;
//! println!("Generated {} files", result.files().len());
//! # Ok(())
//! # }
//! ```
//!
//! ### Using Template Parser
//!
//! ```rust,no_run
//! use crate::templates::TemplateParser;
//!
//! # fn main() -> crate::utils::error::Result<()> {
//! let template = TemplateParser::parse_file("project.tree.yaml")?;
//! println!("Parsed template: {}", template.format.name);
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
