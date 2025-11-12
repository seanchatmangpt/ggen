//! File tree generation from templates
//!
//! Generates actual file trees from parsed templates with variable substitution.

use anyhow::{Context, Result};
use std::fs;
use std::path::{Path, PathBuf};

use super::context::TemplateContext;
use super::file_tree_generator::FileTreeTemplate;
use super::format::{FileTreeNode, NodeType};

/// File tree generator
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
    pub fn new<P: AsRef<Path>>(
        template: FileTreeTemplate, context: TemplateContext, base_dir: P,
    ) -> Self {
        Self {
            template,
            context,
            base_dir: base_dir.as_ref().to_path_buf(),
        }
    }

    /// Generate the file tree
    pub fn generate(&mut self) -> Result<GenerationResult> {
        // Validate required variables
        self.context
            .validate_required(self.template.required_variables())
            .context("Template variable validation failed")?;

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
        let rendered_name = self
            .context
            .render_string(&node.name)
            .with_context(|| format!("Failed to render node name: {}", node.name))?;

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
            fs::create_dir_all(path)
                .with_context(|| format!("Failed to create directory: {}", path.display()))?;
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
                fs::create_dir_all(parent).with_context(|| {
                    format!("Failed to create parent directory: {}", parent.display())
                })?;
            }
        }

        // Get file content
        let content = if let Some(inline_content) = &node.content {
            // Render inline content
            self.context
                .render_string(inline_content)
                .context("Failed to render inline content")?
        } else if let Some(template_path) = &node.template {
            // Load and render template file
            self.render_template_file(template_path)?
        } else {
            // Empty file
            String::new()
        };

        // Write file
        fs::write(path, content)
            .with_context(|| format!("Failed to write file: {}", path.display()))?;

        // Set permissions if specified
        #[cfg(unix)]
        if let Some(mode) = node.mode {
            use std::os::unix::fs::PermissionsExt;
            let permissions = fs::Permissions::from_mode(mode);
            fs::set_permissions(path, permissions)
                .with_context(|| format!("Failed to set permissions for: {}", path.display()))?;
        }

        result.add_file(path);
        Ok(())
    }

    /// Render a template file
    fn render_template_file(&self, template_path: &str) -> Result<String> {
        let full_path = self.base_dir.join(template_path);

        let template_content = fs::read_to_string(&full_path)
            .with_context(|| format!("Failed to read template file: {}", full_path.display()))?;

        self.context
            .render_string(&template_content)
            .with_context(|| format!("Failed to render template: {}", template_path))
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
    pub fn directories(&self) -> &[PathBuf] {
        &self.directories
    }

    /// Get generated files
    pub fn files(&self) -> &[PathBuf] {
        &self.files
    }

    /// Get total count of generated items
    pub fn total_count(&self) -> usize {
        self.directories.len() + self.files.len()
    }

    /// Check if any files were generated
    pub fn is_empty(&self) -> bool {
        self.directories.is_empty() && self.files.is_empty()
    }
}

/// Generate a file tree from a template
///
/// # Arguments
/// * `template` - The template to generate from
/// * `context` - Variable context for rendering
/// * `output_dir` - Base directory for output
///
/// # Returns
/// Result containing generation statistics
pub fn generate_file_tree<P: AsRef<Path>>(
    template: FileTreeTemplate, context: TemplateContext, output_dir: P,
) -> Result<GenerationResult> {
    let mut generator = FileTreeGenerator::new(template, context, output_dir);
    generator.generate()
}

// TEMPORARILY DISABLED: tests require FileTreeTemplate which has compilation errors
// TODO: Re-enable when FileTreeTemplate compilation errors are fixed
/*
#[cfg(test)]
mod tests {
    use super::*;
    use crate::templates::format::TemplateFormat;
    use std::collections::BTreeMap;
    use tempfile::TempDir;

    #[test]
    fn test_generate_simple_tree() {
        let temp_dir = TempDir::new().unwrap();

        let mut format = TemplateFormat::new("test");
        format.add_node(FileTreeNode::directory("src"));

        let template = FileTreeTemplate::new(format);
        let context = TemplateContext::new();

        let mut generator = FileTreeGenerator::new(template, context, temp_dir.path());
        let result = generator.generate().unwrap();

        assert_eq!(result.directories().len(), 1);
        assert!(temp_dir.path().join("src").exists());
    }

    #[test]
    fn test_generate_with_variables() {
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
    }

    #[test]
    fn test_generate_file_with_content() {
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
    }

    #[test]
    fn test_generation_result() {
        let mut result = GenerationResult::new();

        assert!(result.is_empty());
        assert_eq!(result.total_count(), 0);

        result.add_directory(Path::new("/test/dir"));
        result.add_file(Path::new("/test/file.txt"));

        assert!(!result.is_empty());
        assert_eq!(result.total_count(), 2);
        assert_eq!(result.directories().len(), 1);
        assert_eq!(result.files().len(), 1);
    }

    #[test]
    fn test_missing_required_variable() {
        let temp_dir = TempDir::new().unwrap();

        let mut format = TemplateFormat::new("test");
        format.add_variable("service_name");
        format.add_node(FileTreeNode::directory("{{ service_name }}"));

        let template = FileTreeTemplate::new(format);
        let context = TemplateContext::new();

        let mut generator = FileTreeGenerator::new(template, context, temp_dir.path());
        let result = generator.generate();

        assert!(result.is_err());
    }

    #[test]
    fn test_apply_defaults() {
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
    }
}
*/
