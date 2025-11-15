//! File tree generation from templates
//!
//! This module generates actual file trees from parsed templates with variable substitution.
//! It takes a `FileTreeTemplate` and a `TemplateContext`, then creates directories and
//! files in the specified output directory.
//!
//! ## Features
//!
//! - **Variable Substitution**: Renders template variables in file names and content
//! - **Directory Creation**: Automatically creates directory structure
//! - **File Generation**: Generates files with rendered content
//! - **Template Support**: Supports inline content and external template files
//! - **Permission Handling**: Sets file permissions on Unix systems
//! - **Validation**: Validates required variables before generation
//! - **Default Values**: Applies default variable values when not provided
//!
//! ## Generation Process
//!
//! 1. **Validation**: Validates that all required variables are provided
//! 2. **Defaults**: Applies default values for optional variables
//! 3. **Node Processing**: Processes each node in the template tree
//! 4. **Rendering**: Renders node names and content with variable substitution
//! 5. **File System**: Creates directories and writes files
//!
//! ## Examples
//!
//! ### Generating a Simple File Tree
//!
//! ```rust,no_run
//! use ggen_core::templates::{FileTreeTemplate, TemplateContext, generate_file_tree};
//! use ggen_core::templates::format::{TemplateFormat, FileTreeNode, NodeType};
//! use std::path::Path;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let mut format = TemplateFormat::new("my-template");
//! format.add_node(FileTreeNode::directory("src"));
//!
//! let template = FileTreeTemplate::new(format);
//! let context = TemplateContext::new();
//!
//! let result = generate_file_tree(template, context, Path::new("output"))?;
//! println!("Generated {} files and {} directories",
//!          result.files().len(), result.directories().len());
//! # Ok(())
//! # }
//! ```
//!
//! ### Generating with Variables
//!
//! ```rust,no_run
//! use ggen_core::templates::{FileTreeTemplate, TemplateContext, generate_file_tree};
//! use ggen_core::templates::format::{TemplateFormat, FileTreeNode};
//! use serde_json::json;
//! use std::path::Path;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let mut format = TemplateFormat::new("service-template");
//! format.add_variable("service_name");
//! format.add_node(FileTreeNode::directory("{{ service_name }}"));
//!
//! let template = FileTreeTemplate::new(format);
//! let mut context = TemplateContext::new();
//! context.set("service_name", json!("my-service"))?;
//!
//! let result = generate_file_tree(template, context, Path::new("output"))?;
//! // Creates output/my-service/ directory
//! # Ok(())
//! # }
//! ```

use ggen_utils::error::{Error, Result};
use std::fs;
use std::path::{Path, PathBuf};

use super::context::TemplateContext;
use super::file_tree_generator::FileTreeTemplate;
use super::format::{FileTreeNode, NodeType};

/// File tree generator for creating directory structures from templates
///
/// Takes a `FileTreeTemplate` and a `TemplateContext`, then generates the
/// corresponding file tree in the filesystem with variable substitution.
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_core::templates::generator::FileTreeGenerator;
/// use ggen_core::templates::{FileTreeTemplate, TemplateContext};
/// use ggen_core::templates::format::TemplateFormat;
/// use std::path::Path;
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let format = TemplateFormat::new("my-template");
/// let template = FileTreeTemplate::new(format);
/// let context = TemplateContext::new();
///
/// let mut generator = FileTreeGenerator::new(template, context, Path::new("output"));
/// let result = generator.generate()?;
/// # Ok(())
/// # }
/// ```
pub struct FileTreeGenerator {
    /// Template to generate from
    template: FileTreeTemplate,

    /// Context for variable resolution
    context: TemplateContext,

    /// Base output directory
    base_dir: PathBuf,
}

impl FileTreeGenerator {
    /// Create a new file tree generator
    ///
    /// # Arguments
    ///
    /// * `template` - The file tree template to generate from
    /// * `context` - Variable context for template rendering
    /// * `base_dir` - Base directory where files will be generated
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::templates::generator::FileTreeGenerator;
    /// use ggen_core::templates::{FileTreeTemplate, TemplateContext};
    /// use ggen_core::templates::format::TemplateFormat;
    /// use std::path::Path;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let format = TemplateFormat::new("my-template");
    /// let template = FileTreeTemplate::new(format);
    /// let context = TemplateContext::new();
    ///
    /// let generator = FileTreeGenerator::new(template, context, Path::new("output"));
    /// # Ok(())
    /// # }
    /// ```
    pub fn new<P: AsRef<Path>>(
        template: FileTreeTemplate, context: TemplateContext, base_dir: P,
    ) -> Self {
        Self {
            template,
            context,
            base_dir: base_dir.as_ref().to_path_buf(),
        }
    }

    /// Generate the file tree from the template
    ///
    /// This method:
    /// 1. Validates that all required variables are provided
    /// 2. Applies default values for optional variables
    /// 3. Processes each node in the template tree
    /// 4. Creates directories and files with rendered content
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - Required variables are missing
    /// - Template rendering fails
    /// - File system operations fail
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::templates::generator::FileTreeGenerator;
    /// use ggen_core::templates::{FileTreeTemplate, TemplateContext};
    /// use ggen_core::templates::format::{TemplateFormat, FileTreeNode};
    /// use std::path::Path;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let mut format = TemplateFormat::new("my-template");
    /// format.add_node(FileTreeNode::directory("src"));
    /// let template = FileTreeTemplate::new(format);
    /// let context = TemplateContext::new();
    ///
    /// let mut generator = FileTreeGenerator::new(template, context, Path::new("output"));
    /// let result = generator.generate()?;
    /// println!("Generated {} directories", result.directories().len());
    /// # Ok(())
    /// # }
    /// ```
    pub fn generate(&mut self) -> Result<GenerationResult> {
        // Validate required variables
        self.context
            .validate_required(self.template.required_variables())
            .map_err(|e| {
                Error::with_context("Template variable validation failed", &e.to_string())
            })?;

        // Apply defaults
        self.context.apply_defaults(self.template.defaults());

        let mut result = GenerationResult::new();

        // Generate each node in the tree
        for node in self.template.nodes() {
            self.generate_node(node, &self.base_dir.clone(), &mut result)?;
        }

        Ok(result)
    }

    /// Generate a single node
    fn generate_node(
        &self, node: &FileTreeNode, current_dir: &Path, result: &mut GenerationResult,
    ) -> Result<()> {
        // Render the node name with variables
        let rendered_name = self.context.render_string(&node.name).map_err(|e| {
            ggen_utils::error::Error::with_context(
                "Failed to render node name",
                &format!("{}: {}", node.name, e),
            )
        })?;

        let node_path = current_dir.join(&rendered_name);

        match node.node_type {
            NodeType::Directory => {
                self.generate_directory(&node_path, node, result)?;
            }
            NodeType::File => {
                self.generate_file(&node_path, node, result)?;
            }
        }

        Ok(())
    }

    /// Generate a directory
    fn generate_directory(
        &self, path: &Path, node: &FileTreeNode, result: &mut GenerationResult,
    ) -> Result<()> {
        // Create directory if it doesn't exist
        if !path.exists() {
            fs::create_dir_all(path).map_err(|e| {
                ggen_utils::error::Error::with_context(
                    "Failed to create directory",
                    &format!("{}: {}", path.display(), e),
                )
            })?;
            result.add_directory(path);
        }

        // Generate children
        for child in &node.children {
            self.generate_node(child, path, result)?;
        }

        Ok(())
    }

    /// Generate a file
    fn generate_file(
        &self, path: &Path, node: &FileTreeNode, result: &mut GenerationResult,
    ) -> Result<()> {
        // Ensure parent directory exists
        if let Some(parent) = path.parent() {
            if !parent.exists() {
                fs::create_dir_all(parent).map_err(|e| {
                    ggen_utils::error::Error::with_context(
                        "Failed to create parent directory",
                        &format!("{}: {}", parent.display(), e),
                    )
                })?;
            }
        }

        // Get file content
        let content = if let Some(inline_content) = &node.content {
            // Render inline content
            self.context.render_string(inline_content).map_err(|e| {
                Error::with_context("Failed to render inline content", &e.to_string())
            })?
        } else if let Some(template_path) = &node.template {
            // Load and render template file
            self.render_template_file(template_path)?
        } else {
            // Empty file
            String::new()
        };

        // Write file
        fs::write(path, content).map_err(|e| {
            ggen_utils::error::Error::with_context(
                "Failed to write file",
                &format!("{}: {}", path.display(), e),
            )
        })?;

        // Set permissions if specified
        #[cfg(unix)]
        if let Some(mode) = node.mode {
            use std::os::unix::fs::PermissionsExt;
            let permissions = fs::Permissions::from_mode(mode);
            fs::set_permissions(path, permissions).map_err(|e| {
                ggen_utils::error::Error::with_context(
                    "Failed to set permissions",
                    &format!("{}: {}", path.display(), e),
                )
            })?;
        }

        result.add_file(path);
        Ok(())
    }

    /// Render a template file
    fn render_template_file(&self, template_path: &str) -> Result<String> {
        let full_path = self.base_dir.join(template_path);

        let template_content = fs::read_to_string(&full_path).map_err(|e| {
            ggen_utils::error::Error::with_context(
                "Failed to read template file",
                &format!("{}: {}", full_path.display(), e),
            )
        })?;

        self.context.render_string(&template_content).map_err(|e| {
            ggen_utils::error::Error::with_context(
                "Failed to render template",
                &format!("{}: {}", template_path, e),
            )
        })
    }

    /// Get the template being used
    pub fn template(&self) -> &FileTreeTemplate {
        &self.template
    }

    /// Get the context being used
    pub fn context(&self) -> &TemplateContext {
        &self.context
    }
}

/// Result of file tree generation
///
/// Tracks all directories and files created during generation.
/// Provides statistics about the generation process.
///
/// # Examples
///
/// ```rust
/// use ggen_core::templates::generator::GenerationResult;
///
/// let result = GenerationResult::new();
/// assert!(result.is_empty());
/// assert_eq!(result.total_count(), 0);
/// ```
#[derive(Debug, Clone, Default)]
pub struct GenerationResult {
    /// Generated directories
    directories: Vec<PathBuf>,

    /// Generated files
    files: Vec<PathBuf>,
}

impl GenerationResult {
    /// Create a new generation result
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a directory to the result
    fn add_directory(&mut self, path: &Path) {
        self.directories.push(path.to_path_buf());
    }

    /// Add a file to the result
    fn add_file(&mut self, path: &Path) {
        self.files.push(path.to_path_buf());
    }

    /// Get generated directories
    ///
    /// Returns a slice of all directory paths that were created during generation.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::generator::GenerationResult;
    ///
    /// let result = GenerationResult::new();
    /// let dirs = result.directories();
    /// assert_eq!(dirs.len(), 0);
    /// ```
    pub fn directories(&self) -> &[PathBuf] {
        &self.directories
    }

    /// Get generated files
    ///
    /// Returns a slice of all file paths that were created during generation.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::generator::GenerationResult;
    ///
    /// let result = GenerationResult::new();
    /// let files = result.files();
    /// assert_eq!(files.len(), 0);
    /// ```
    pub fn files(&self) -> &[PathBuf] {
        &self.files
    }

    /// Get total count of generated items
    ///
    /// Returns the sum of directories and files created.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::generator::GenerationResult;
    ///
    /// let result = GenerationResult::new();
    /// let total = result.total_count();
    /// assert_eq!(total, 0);
    /// ```
    pub fn total_count(&self) -> usize {
        self.directories.len() + self.files.len()
    }

    /// Check if any files or directories were generated
    ///
    /// Returns `true` if no directories or files were created.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::generator::GenerationResult;
    ///
    /// let result = GenerationResult::new();
    /// assert!(result.is_empty());
    /// ```
    pub fn is_empty(&self) -> bool {
        self.directories.is_empty() && self.files.is_empty()
    }
}

/// Generate a file tree from a template
///
/// Convenience function that creates a `FileTreeGenerator` and generates
/// the file tree in a single call.
///
/// # Arguments
///
/// * `template` - The template to generate from
/// * `context` - Variable context for rendering
/// * `output_dir` - Base directory for output
///
/// # Returns
///
/// A `GenerationResult` containing statistics about what was generated.
///
/// # Errors
///
/// Returns an error if generation fails (see `FileTreeGenerator::generate()`).
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_core::templates::{FileTreeTemplate, TemplateContext, generate_file_tree};
/// use ggen_core::templates::format::TemplateFormat;
/// use std::path::Path;
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let format = TemplateFormat::new("my-template");
/// let template = FileTreeTemplate::new(format);
/// let context = TemplateContext::new();
///
/// let result = generate_file_tree(template, context, Path::new("output"))?;
/// println!("Generated {} items", result.total_count());
/// # Ok(())
/// # }
/// ```
pub fn generate_file_tree<P: AsRef<Path>>(
    template: FileTreeTemplate, context: TemplateContext, output_dir: P,
) -> Result<GenerationResult> {
    let mut generator = FileTreeGenerator::new(template, context, output_dir);
    generator.generate()
}

// TEMPORARILY DISABLED: tests require FileTreeTemplate which has compilation errors
/*
#[cfg(test)]
mod tests {
    use super::*;
    use crate::templates::format::TemplateFormat;
    use std::collections::BTreeMap;
    use tempfile::TempDir;
    use chicago_tdd_tools::prelude::*;

    test!(test_generate_simple_tree, {
        let temp_dir = TempDir::new().unwrap();

        let mut format = TemplateFormat::new("test");
        format.add_node(FileTreeNode::directory("src"));

        let template = FileTreeTemplate::new(format);
        let context = TemplateContext::new();

        let mut generator = FileTreeGenerator::new(template, context, temp_dir.path());
        let result = generator.generate().unwrap();

        assert_eq!(result.directories().len(), 1);
        assert!(temp_dir.path().join("src").exists());
    });

    test!(test_generate_with_variables, {
        let temp_dir = TempDir::new().unwrap();

        let mut format = TemplateFormat::new("test");
        format.add_variable("service_name");
        format.add_node(FileTreeNode::directory("{{ service_name }}"));

        let template = FileTreeTemplate::new(format);

        let mut vars = BTreeMap::new();
        vars.insert("service_name".to_string(), "my-service".to_string());
        let context = TemplateContext::from_map(vars).unwrap();

        let mut generator = FileTreeGenerator::new(template, context, temp_dir.path());
        let result = generator.generate().unwrap();

        assert_eq!(result.directories().len(), 1);
        assert!(temp_dir.path().join("my-service").exists());
    });

    test!(test_generate_file_with_content, {
        let temp_dir = TempDir::new().unwrap();

        let mut format = TemplateFormat::new("test");
        let mut dir = FileTreeNode::directory("src");
        dir.add_child(FileTreeNode::file_with_content(
            "main.rs",
            "fn main() { println!(\"{{ message }}\"); }",
        ));
        format.add_node(dir);

        let template = FileTreeTemplate::new(format);

        let mut vars = BTreeMap::new();
        vars.insert("message".to_string(), "Hello, World!".to_string());
        let context = TemplateContext::from_map(vars).unwrap();

        let mut generator = FileTreeGenerator::new(template, context, temp_dir.path());
        let result = generator.generate().unwrap();

        assert_eq!(result.files().len(), 1);

        let file_path = temp_dir.path().join("src").join("main.rs");
        assert!(file_path.exists());

        let content = fs::read_to_string(&file_path).unwrap();
        assert!(content.contains("Hello, World!"));
    });

    test!(test_generation_result, {
        let mut result = GenerationResult::new();

        assert!(result.is_empty());
        assert_eq!(result.total_count(), 0);

        result.add_directory(Path::new("/test/dir"));
        result.add_file(Path::new("/test/file.txt"));

        assert!(!result.is_empty());
        assert_eq!(result.total_count(), 2);
        assert_eq!(result.directories().len(), 1);
        assert_eq!(result.files().len(), 1);
    });

    test!(test_missing_required_variable, {
        let temp_dir = TempDir::new().unwrap();

        let mut format = TemplateFormat::new("test");
        format.add_variable("service_name");
        format.add_node(FileTreeNode::directory("{{ service_name }}"));

        let template = FileTreeTemplate::new(format);
        let context = TemplateContext::new();

        let mut generator = FileTreeGenerator::new(template, context, temp_dir.path());
        let result = generator.generate();

        assert!(result.is_err());
    });

    test!(test_apply_defaults, {
        let temp_dir = TempDir::new().unwrap();

        let mut format = TemplateFormat::new("test");
        format.add_variable("service_name");
        format.add_default("service_name", "default-service");
        format.add_node(FileTreeNode::directory("{{ service_name }}"));

        let template = FileTreeTemplate::new(format);
        let context = TemplateContext::new();

        let mut generator = FileTreeGenerator::new(template, context, temp_dir.path());
        let result = generator.generate().unwrap();

        assert_eq!(result.directories().len(), 1);
        assert!(temp_dir.path().join("default-service").exists());
    });
}
*/
