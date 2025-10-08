use anyhow::{bail, Result};
use std::path::PathBuf;
use glob::glob;

/// Resolves generator/action patterns to template file paths.
/// 
/// Maps Hygen-style `rgen gen <generator> <action>` to template files
/// under `templates/<generator>/<action>/*.tmpl`.
pub struct TemplateResolver {
    templates_dir: PathBuf,
}

impl TemplateResolver {
    /// Create a new resolver for the given templates directory.
    pub fn new(templates_dir: PathBuf) -> Self {
        Self { templates_dir }
    }

    /// Resolve generator/action to template paths.
    /// 
    /// # Arguments
    /// * `generator` - The generator name (e.g., "cli")
    /// * `action` - The action name (e.g., "subcommand") 
    /// * `name` - Optional template name to filter to specific template
    /// 
    /// # Returns
    /// Vector of template file paths, sorted for deterministic output.
    /// 
    /// # Examples
    /// ```
    /// // Resolve all CLI subcommand templates
    /// let paths = resolver.resolve("cli", "subcommand", None)?;
    /// // Returns: ["templates/cli/subcommand/rust.tmpl", "templates/cli/subcommand/python.tmpl"]
    /// 
    /// // Resolve specific template
    /// let paths = resolver.resolve("cli", "subcommand", Some("rust"))?;
    /// // Returns: ["templates/cli/subcommand/rust.tmpl"]
    /// ```
    pub fn resolve(&self, generator: &str, action: &str, name: Option<&str>) -> Result<Vec<PathBuf>> {
        // Validate inputs
        if generator.is_empty() {
            bail!("Generator name cannot be empty");
        }
        if action.is_empty() {
            bail!("Action name cannot be empty");
        }
        
        // Sanitize inputs to prevent path traversal
        if generator.contains("..") || generator.contains('/') || generator.contains('\\') {
            bail!("Generator name '{}' contains invalid characters", generator);
        }
        if action.contains("..") || action.contains('/') || action.contains('\\') {
            bail!("Action name '{}' contains invalid characters", action);
        }
        if let Some(n) = name {
            if n.contains("..") || n.contains('/') || n.contains('\\') {
                bail!("Template name '{}' contains invalid characters", n);
            }
        }

        // Build the search pattern
        let pattern = if let Some(template_name) = name {
            // Specific template: templates/generator/action/name.tmpl
            self.templates_dir
                .join(generator)
                .join(action)
                .join(format!("{}.tmpl", template_name))
                .to_string_lossy()
                .to_string()
        } else {
            // All templates: templates/generator/action/*.tmpl
            self.templates_dir
                .join(generator)
                .join(action)
                .join("*.tmpl")
                .to_string_lossy()
                .to_string()
        };

        // Find matching files
        let mut paths = Vec::new();
        for entry in glob(&pattern)? {
            match entry {
                Ok(path) => {
                    if path.is_file() {
                        paths.push(path);
                    }
                }
                Err(e) => {
                    // Log glob errors but continue
                    eprintln!("Warning: glob error: {}", e);
                }
            }
        }

        // Sort for deterministic output
        paths.sort();

        // Provide helpful error messages
        if paths.is_empty() {
            if let Some(template_name) = name {
                bail!(
                    "Template '{}' not found in {}/{}/. Run `rgen list` to see available templates.",
                    template_name,
                    generator,
                    action
                );
            } else {
                bail!(
                    "No templates found in {}/{}/. Run `rgen list` to see available generators and actions.",
                    generator,
                    action
                );
            }
        }

        Ok(paths)
    }

    /// List all available generators (top-level directories in templates/).
    pub fn list_generators(&self) -> Result<Vec<String>> {
        if !self.templates_dir.exists() {
            return Ok(Vec::new());
        }

        let mut generators = Vec::new();
        for entry in std::fs::read_dir(&self.templates_dir)? {
            let entry = entry?;
            if entry.file_type()?.is_dir() {
                if let Some(name) = entry.file_name().to_str() {
                    generators.push(name.to_string());
                }
            }
        }

        generators.sort();
        Ok(generators)
    }

    /// List all available actions for a given generator.
    pub fn list_actions(&self, generator: &str) -> Result<Vec<String>> {
        let generator_dir = self.templates_dir.join(generator);
        if !generator_dir.exists() {
            return Ok(Vec::new());
        }

        let mut actions = Vec::new();
        for entry in std::fs::read_dir(&generator_dir)? {
            let entry = entry?;
            if entry.file_type()?.is_dir() {
                if let Some(name) = entry.file_name().to_str() {
                    actions.push(name.to_string());
                }
            }
        }

        actions.sort();
        Ok(actions)
    }

    /// List all available template names for a given generator/action.
    pub fn list_templates(&self, generator: &str, action: &str) -> Result<Vec<String>> {
        let action_dir = self.templates_dir.join(generator).join(action);
        if !action_dir.exists() {
            return Ok(Vec::new());
        }

        let mut templates = Vec::new();
        for entry in std::fs::read_dir(&action_dir)? {
            let entry = entry?;
            if entry.file_type()?.is_file() {
                if let Some(name) = entry.file_name().to_str() {
                    if name.ends_with(".tmpl") {
                        // Remove .tmpl extension
                        let template_name = &name[..name.len() - 5];
                        templates.push(template_name.to_string());
                    }
                }
            }
        }

        templates.sort();
        Ok(templates)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;
    use std::fs;

    fn create_test_templates() -> Result<TempDir> {
        let temp_dir = TempDir::new()?;
        let templates_dir = temp_dir.path().join("templates");
        
        // Create directory structure
        fs::create_dir_all(templates_dir.join("cli").join("subcommand"))?;
        fs::create_dir_all(templates_dir.join("api").join("endpoint"))?;
        
        // Create template files
        fs::write(templates_dir.join("cli").join("subcommand").join("rust.tmpl"), "Rust template")?;
        fs::write(templates_dir.join("cli").join("subcommand").join("python.tmpl"), "Python template")?;
        fs::write(templates_dir.join("api").join("endpoint").join("rust.tmpl"), "API Rust template")?;
        
        Ok(temp_dir)
    }

    #[test]
    fn test_resolve_all_templates() -> Result<()> {
        let temp_dir = create_test_templates()?;
        let resolver = TemplateResolver::new(temp_dir.path().join("templates"));
        
        let paths = resolver.resolve("cli", "subcommand", None)?;
        assert_eq!(paths.len(), 2);
        assert!(paths.iter().any(|p| p.ends_with("rust.tmpl")));
        assert!(paths.iter().any(|p| p.ends_with("python.tmpl")));
        
        Ok(())
    }

    #[test]
    fn test_resolve_specific_template() -> Result<()> {
        let temp_dir = create_test_templates()?;
        let resolver = TemplateResolver::new(temp_dir.path().join("templates"));
        
        let paths = resolver.resolve("cli", "subcommand", Some("rust"))?;
        assert_eq!(paths.len(), 1);
        assert!(paths[0].ends_with("rust.tmpl"));
        
        Ok(())
    }

    #[test]
    fn test_resolve_nonexistent_generator() -> Result<()> {
        let temp_dir = create_test_templates()?;
        let resolver = TemplateResolver::new(temp_dir.path().join("templates"));
        
        let result = resolver.resolve("nonexistent", "action", None);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("No templates found"));
        
        Ok(())
    }

    #[test]
    fn test_resolve_nonexistent_template() -> Result<()> {
        let temp_dir = create_test_templates()?;
        let resolver = TemplateResolver::new(temp_dir.path().join("templates"));
        
        let result = resolver.resolve("cli", "subcommand", Some("nonexistent"));
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Template 'nonexistent' not found"));
        
        Ok(())
    }

    #[test]
    fn test_path_traversal_protection() -> Result<()> {
        let temp_dir = create_test_templates()?;
        let resolver = TemplateResolver::new(temp_dir.path().join("templates"));
        
        // Test path traversal attempts
        assert!(resolver.resolve("..", "action", None).is_err());
        assert!(resolver.resolve("generator", "../action", None).is_err());
        assert!(resolver.resolve("generator", "action", Some("../template")).is_err());
        
        Ok(())
    }

    #[test]
    fn test_list_generators() -> Result<()> {
        let temp_dir = create_test_templates()?;
        let resolver = TemplateResolver::new(temp_dir.path().join("templates"));
        
        let generators = resolver.list_generators()?;
        assert_eq!(generators, vec!["api", "cli"]);
        
        Ok(())
    }

    #[test]
    fn test_list_actions() -> Result<()> {
        let temp_dir = create_test_templates()?;
        let resolver = TemplateResolver::new(temp_dir.path().join("templates"));
        
        let actions = resolver.list_actions("cli")?;
        assert_eq!(actions, vec!["subcommand"]);
        
        Ok(())
    }

    #[test]
    fn test_list_templates() -> Result<()> {
        let temp_dir = create_test_templates()?;
        let resolver = TemplateResolver::new(temp_dir.path().join("templates"));
        
        let templates = resolver.list_templates("cli", "subcommand")?;
        assert_eq!(templates, vec!["python", "rust"]);
        
        Ok(())
    }

    #[test]
    fn test_empty_inputs() -> Result<()> {
        let temp_dir = create_test_templates()?;
        let resolver = TemplateResolver::new(temp_dir.path().join("templates"));
        
        assert!(resolver.resolve("", "action", None).is_err());
        assert!(resolver.resolve("generator", "", None).is_err());
        
        Ok(())
    }
}
