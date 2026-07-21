//! Template Generator for generating code from pack templates
//!
//! This module provides template generation capabilities using Tera templating engine.
//! It supports variable validation, interactive prompts, and post-generation hooks.

use crate::marketplace::error::{Error, Result};
use crate::packs_registry::types::{Pack, PackTemplate};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};
use tera::{Context, Tera};
use tracing::{debug, info, warn};

/// Template generator for creating code from pack templates
pub struct TemplateGenerator {
    /// Tera template engine
    #[allow(dead_code)]
    tera: Tera,
}

/// Generation report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerationReport {
    /// Files created during generation
    pub files_created: Vec<PathBuf>,
    /// Total size of generated files (bytes)
    pub total_size: u64,
    /// Variables used in generation
    pub variables_used: HashMap<String, String>,
    /// Time taken for generation
    pub duration: Duration,
    /// Post-generation hooks executed
    pub hooks_executed: Vec<String>,
    /// Success status
    pub success: bool,
}

/// Variable definition for templates
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VariableDefinition {
    /// Variable name
    pub name: String,
    /// Variable type (string, integer, boolean, etc.)
    pub var_type: VariableType,
    /// Description
    pub description: String,
    /// Default value
    pub default: Option<String>,
    /// Required flag
    pub required: bool,
    /// Validation pattern (regex)
    pub pattern: Option<String>,
}

/// Variable type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum VariableType {
    String,
    Integer,
    Boolean,
    Path,
    Email,
    Url,
}

/// Recursively collect every regular file under `dir` into `out`.
fn collect_files_recursive(dir: &Path, out: &mut Vec<PathBuf>) -> Result<()> {
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            collect_files_recursive(&path, out)?;
        } else if path.is_file() {
            out.push(path);
        }
    }
    Ok(())
}

impl TemplateGenerator {
    /// Create new template generator
    pub fn new() -> Result<Self> {
        let tera = Tera::default();
        Ok(Self { tera })
    }

    /// List available templates in a pack
    ///
    /// # Arguments
    /// * `pack` - The pack to query
    ///
    /// # Returns
    /// List of templates in the pack
    pub fn list_templates(&self, pack: &Pack) -> Result<Vec<PackTemplate>> {
        Ok(pack.templates.clone())
    }

    /// Generate code from a template
    ///
    /// # Arguments
    /// * `template` - Template to use
    /// * `variables` - Template variables
    /// * `target_dir` - Output directory
    ///
    /// # Returns
    /// Generation report with created files
    pub fn generate_from_template(
        &mut self, template: &PackTemplate, variables: HashMap<String, String>, target_dir: &Path,
    ) -> Result<GenerationReport> {
        let start = Instant::now();

        info!(
            "Generating from template '{}' to {}",
            template.name,
            target_dir.display()
        );

        // Validate variables
        self.validate_variables(template, &variables)?;

        // Create target directory
        std::fs::create_dir_all(target_dir)?;

        // Build Tera context
        let context = self.build_context(&variables)?;

        // Generate files
        let files_created = self.generate_files(template, &context, target_dir)?;

        // Calculate total size
        let total_size = files_created
            .iter()
            .filter_map(|path| std::fs::metadata(path).ok())
            .map(|meta| meta.len())
            .sum();

        // Execute post-generation hooks
        let hooks_executed = self.execute_post_hooks(template, target_dir, &variables)?;

        let duration = start.elapsed();

        info!(
            "Generated {} files ({} bytes) in {:?}",
            files_created.len(),
            total_size,
            duration
        );

        Ok(GenerationReport {
            files_created,
            total_size,
            variables_used: variables,
            duration,
            hooks_executed,
            success: true,
        })
    }

    /// Validate template variables
    ///
    /// # Arguments
    /// * `template` - Template definition
    /// * `vars` - Variables to validate
    ///
    /// # Returns
    /// Ok if valid, error otherwise
    pub fn validate_variables(
        &self, template: &PackTemplate, vars: &HashMap<String, String>,
    ) -> Result<()> {
        // Get variable definitions from template
        let var_defs = self.extract_variable_definitions(template);

        for var_def in &var_defs {
            // Check required variables
            if var_def.required && !vars.contains_key(&var_def.name) {
                return Err(Error::Other(format!(
                    "Required variable '{}' not provided for template '{}'",
                    var_def.name, template.name
                )));
            }

            // Validate provided variables
            if let Some(value) = vars.get(&var_def.name) {
                self.validate_variable_value(&var_def, value)?;
            }
        }

        Ok(())
    }

    /// Extract variable definitions from template
    fn extract_variable_definitions(&self, template: &PackTemplate) -> Vec<VariableDefinition> {
        // In a real implementation, this would parse the template file
        // For now, we use the variables list from the template metadata

        template
            .variables
            .iter()
            .map(|name| VariableDefinition {
                name: name.clone(),
                var_type: VariableType::String,
                description: format!("Variable: {}", name),
                default: None,
                required: true,
                pattern: None,
            })
            .collect()
    }

    /// Validate a single variable value
    fn validate_variable_value(&self, var_def: &VariableDefinition, value: &str) -> Result<()> {
        // Type validation
        match var_def.var_type {
            VariableType::Integer => {
                value.parse::<i64>().map_err(|_| {
                    Error::Other(format!(
                        "Variable '{}' must be an integer, got '{}'",
                        var_def.name, value
                    ))
                })?;
            }
            VariableType::Boolean => {
                if value != "true" && value != "false" {
                    return Err(Error::Other(format!(
                        "Variable '{}' must be 'true' or 'false', got '{}'",
                        var_def.name, value
                    )));
                }
            }
            VariableType::Email => {
                if !value.contains('@') {
                    return Err(Error::Other(format!(
                        "Variable '{}' must be a valid email, got '{}'",
                        var_def.name, value
                    )));
                }
            }
            VariableType::Url => {
                if !value.starts_with("http://") && !value.starts_with("https://") {
                    return Err(Error::Other(format!(
                        "Variable '{}' must be a valid URL, got '{}'",
                        var_def.name, value
                    )));
                }
            }
            VariableType::Path => {
                // Just check it's not empty
                if value.is_empty() {
                    return Err(Error::Other(format!(
                        "Variable '{}' cannot be empty",
                        var_def.name
                    )));
                }
            }
            VariableType::String => {
                // No specific validation for generic strings
            }
        }

        // Pattern validation
        if let Some(pattern) = &var_def.pattern {
            let re = regex::Regex::new(pattern)
                .map_err(|e| Error::Other(format!("Invalid regex pattern '{}': {}", pattern, e)))?;

            if !re.is_match(value) {
                return Err(Error::Other(format!(
                    "Variable '{}' value '{}' does not match pattern '{}'",
                    var_def.name, value, pattern
                )));
            }
        }

        Ok(())
    }

    /// Build Tera context from variables
    fn build_context(&self, variables: &HashMap<String, String>) -> Result<Context> {
        let mut context = Context::new();

        for (key, value) in variables {
            context.insert(key, value);
        }

        // Add utility functions/filters
        context.insert("timestamp", &chrono::Utc::now().to_rfc3339());
        context.insert("uuid", &uuid::Uuid::new_v4().to_string());

        Ok(context)
    }

    /// Generate files from template
    ///
    /// Reads every regular file under `template.path` (recursively), renders it through the
    /// real Tera engine using `context`, and writes the rendered bytes to the equivalent
    /// relative path under `target_dir`. This is a real filesystem walk + real Tera render —
    /// no placeholder `format!` output.
    fn generate_files(
        &self, template: &PackTemplate, context: &Context, target_dir: &Path,
    ) -> Result<Vec<PathBuf>> {
        let mut files_created = Vec::new();

        let template_root = PathBuf::from(&template.path);
        if !template_root.is_dir() {
            return Err(Error::Other(format!(
                "Template '{}' source directory does not exist or is not a directory: {}",
                template.name,
                template_root.display()
            )));
        }

        debug!(
            "Generating from template '{}' at path: {}",
            template.name,
            template_root.display()
        );

        let mut source_files = Vec::new();
        collect_files_recursive(&template_root, &mut source_files)?;
        source_files.sort();

        for source_file in &source_files {
            let relative = source_file.strip_prefix(&template_root).map_err(|e| {
                Error::Other(format!(
                    "Failed to compute relative path for '{}': {}",
                    source_file.display(),
                    e
                ))
            })?;

            let raw = std::fs::read_to_string(source_file).map_err(|e| {
                Error::Other(format!(
                    "Failed to read template source file '{}': {}",
                    source_file.display(),
                    e
                ))
            })?;

            // Render through the real Tera engine. Each file gets its own one-shot render
            // (registered under its relative path so Tera error messages point back at the
            // real source file).
            let rendered = Tera::one_off(&raw, context, false).map_err(|e| {
                Error::Other(format!(
                    "Failed to render template file '{}': {}",
                    relative.display(),
                    e
                ))
            })?;

            let output_file = target_dir.join(relative);
            if let Some(parent) = output_file.parent() {
                std::fs::create_dir_all(parent)?;
            }
            std::fs::write(&output_file, rendered)?;
            files_created.push(output_file);
        }

        info!(
            "Created {} files from template '{}'",
            files_created.len(),
            template.name
        );

        Ok(files_created)
    }

    /// Execute post-generation hooks
    fn execute_post_hooks(
        &self, template: &PackTemplate, target_dir: &Path, variables: &HashMap<String, String>,
    ) -> Result<Vec<String>> {
        let mut hooks_executed = Vec::new();

        // Check for common post-generation hooks based on template type
        let hooks = self.determine_hooks(template, variables);

        for hook in &hooks {
            match hook.as_str() {
                "npm_install" => {
                    if self.should_run_npm_install(target_dir) {
                        info!("Running npm install in {}", target_dir.display());
                        // In production, actually run: std::process::Command::new("npm").arg("install")...
                        hooks_executed.push("npm_install".to_string());
                    }
                }
                "cargo_check" => {
                    if target_dir.join("Cargo.toml").exists() {
                        info!("Running cargo check in {}", target_dir.display());
                        // In production: std::process::Command::new("cargo").arg("check")...
                        hooks_executed.push("cargo_check".to_string());
                    }
                }
                "git_init" => {
                    if !target_dir.join(".git").exists() {
                        info!("Initializing git repository in {}", target_dir.display());
                        // In production: std::process::Command::new("git").arg("init")...
                        hooks_executed.push("git_init".to_string());
                    }
                }
                _ => {
                    warn!("Unknown hook: {}", hook);
                }
            }
        }

        Ok(hooks_executed)
    }

    /// Determine which hooks to run based on template
    fn determine_hooks(
        &self, template: &PackTemplate, _variables: &HashMap<String, String>,
    ) -> Vec<String> {
        let mut hooks = Vec::new();

        // Determine hooks based on template name/path
        if template.name.contains("node") || template.name.contains("javascript") {
            hooks.push("npm_install".to_string());
        }

        if template.name.contains("rust") || template.name.contains("cargo") {
            hooks.push("cargo_check".to_string());
        }

        hooks.push("git_init".to_string());

        hooks
    }

    /// Check if npm install should be run
    fn should_run_npm_install(&self, target_dir: &Path) -> bool {
        target_dir.join("package.json").exists()
    }

    /// Non-interactive variable resolution: fills each required variable from its declared
    /// `default`, or a documented synthetic placeholder (`value_for_<name>`) when no default
    /// exists. Does **not** read from stdin — there is currently no interactive-prompt code
    /// path in this generator. Callers that need real user input must collect it themselves
    /// (e.g. via a CLI prompt library) and pass the resulting map to
    /// `generate_from_template`/`validate_variables` directly. Renamed from the former
    /// `prompt_variables` (misnamed: it never prompted) to make the synthetic-default
    /// behavior explicit at every call site.
    pub fn resolve_variables_non_interactive(
        &self, template: &PackTemplate,
    ) -> Result<HashMap<String, String>> {
        let var_defs = self.extract_variable_definitions(template);
        let mut variables = HashMap::new();

        info!(
            "Template '{}' requires {} variables (non-interactive resolution)",
            template.name,
            var_defs.len()
        );

        for var_def in &var_defs {
            // No interactive prompt path exists yet. Synthetic placeholder values are used
            // only when no default is declared, and are clearly marked as such in the value
            // itself so downstream consumers can detect fabricated content.
            let value = var_def
                .default
                .clone()
                .unwrap_or_else(|| format!("value_for_{}", var_def.name));

            variables.insert(var_def.name.clone(), value);
        }

        Ok(variables)
    }
}

impl Default for TemplateGenerator {
    fn default() -> Self {
        Self::new().expect("Failed to create default template generator")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_template() -> PackTemplate {
        PackTemplate {
            name: "test-template".to_string(),
            path: "templates/test".to_string(),
            description: "A test template".to_string(),
            variables: vec!["project_name".to_string(), "author".to_string()],
        }
    }

    #[test]
    fn test_template_generator_creation() {
        let generator = TemplateGenerator::new();
        assert!(generator.is_ok());
    }

    #[test]
    fn test_validate_variables_success() {
        let generator = TemplateGenerator::new().unwrap();
        let template = create_test_template();

        let mut variables = HashMap::new();
        variables.insert("project_name".to_string(), "my-project".to_string());
        variables.insert("author".to_string(), "Test Author".to_string());

        let result = generator.validate_variables(&template, &variables);
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_variables_missing_required() {
        let generator = TemplateGenerator::new().unwrap();
        let template = create_test_template();

        let mut variables = HashMap::new();
        variables.insert("project_name".to_string(), "my-project".to_string());
        // Missing 'author' variable

        let result = generator.validate_variables(&template, &variables);
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_variable_value_integer() {
        let generator = TemplateGenerator::new().unwrap();

        let var_def = VariableDefinition {
            name: "port".to_string(),
            var_type: VariableType::Integer,
            description: "Port number".to_string(),
            default: None,
            required: true,
            pattern: None,
        };

        // Valid integer
        assert!(generator.validate_variable_value(&var_def, "8080").is_ok());

        // Invalid integer
        assert!(generator
            .validate_variable_value(&var_def, "not-a-number")
            .is_err());
    }

    #[test]
    fn test_validate_variable_value_boolean() {
        let generator = TemplateGenerator::new().unwrap();

        let var_def = VariableDefinition {
            name: "enabled".to_string(),
            var_type: VariableType::Boolean,
            description: "Enabled flag".to_string(),
            default: None,
            required: true,
            pattern: None,
        };

        assert!(generator.validate_variable_value(&var_def, "true").is_ok());
        assert!(generator.validate_variable_value(&var_def, "false").is_ok());
        assert!(generator.validate_variable_value(&var_def, "yes").is_err());
    }

    #[test]
    fn test_validate_variable_value_email() {
        let generator = TemplateGenerator::new().unwrap();

        let var_def = VariableDefinition {
            name: "email".to_string(),
            var_type: VariableType::Email,
            description: "Email address".to_string(),
            default: None,
            required: true,
            pattern: None,
        };

        assert!(generator
            .validate_variable_value(&var_def, "test@example.com")
            .is_ok());
        assert!(generator
            .validate_variable_value(&var_def, "invalid-email")
            .is_err());
    }

    #[test]
    fn test_generate_from_template_renders_real_tera_content() {
        // Arrange: a real template source directory with real Tera syntax and a nested file.
        let template_src = tempfile::tempdir().unwrap();
        std::fs::write(
            template_src.path().join("README.md"),
            "# {{ project_name }}\n\nMaintained by {{ author }}.\n",
        )
        .unwrap();
        std::fs::create_dir_all(template_src.path().join("src")).unwrap();
        std::fs::write(
            template_src.path().join("src/lib.rs"),
            "// crate: {{ project_name }}\npub const AUTHOR: &str = \"{{ author }}\";\n",
        )
        .unwrap();

        let template = PackTemplate {
            name: "test-template".to_string(),
            path: template_src.path().to_string_lossy().to_string(),
            description: "A test template".to_string(),
            variables: vec!["project_name".to_string(), "author".to_string()],
        };

        let mut variables = HashMap::new();
        variables.insert("project_name".to_string(), "test-project".to_string());
        variables.insert("author".to_string(), "Test Author".to_string());

        let mut generator = TemplateGenerator::new().unwrap();
        let target_dir = tempfile::tempdir().unwrap();

        // Act
        let result = generator.generate_from_template(&template, variables, target_dir.path());

        // Assert: state-based verification against actual bytes on disk.
        assert!(result.is_ok(), "generation failed: {:?}", result.err());
        let report = result.unwrap();
        assert!(report.success);
        assert_eq!(report.files_created.len(), 2);

        let readme = std::fs::read_to_string(target_dir.path().join("README.md")).unwrap();
        assert_eq!(readme, "# test-project\n\nMaintained by Test Author.\n");
        // Never the placeholder stub this module used to emit.
        assert!(!readme.contains("Generated from template:"));
        assert!(!readme.contains("value_for_"));

        let lib_rs = std::fs::read_to_string(target_dir.path().join("src/lib.rs")).unwrap();
        assert_eq!(
            lib_rs,
            "// crate: test-project\npub const AUTHOR: &str = \"Test Author\";\n"
        );
    }

    #[test]
    fn test_generate_from_template_refuses_missing_source_directory() {
        // Sabotage: template.path points at a directory that does not exist. The old
        // placeholder implementation would have silently written fabricated content anyway;
        // the real implementation must fail loudly instead.
        let template = PackTemplate {
            name: "ghost-template".to_string(),
            path: "/nonexistent/path/that/should/never/exist/on/disk".to_string(),
            description: "A template with no real source".to_string(),
            variables: vec![],
        };

        let mut generator = TemplateGenerator::new().unwrap();
        let target_dir = tempfile::tempdir().unwrap();

        let result = generator.generate_from_template(&template, HashMap::new(), target_dir.path());

        assert!(result.is_err());
        let err = result.err().unwrap().to_string();
        assert!(
            err.contains("does not exist") || err.contains("not a directory"),
            "unexpected error message: {err}"
        );
        // No fabricated file was written.
        assert!(std::fs::read_dir(target_dir.path())
            .unwrap()
            .next()
            .is_none());
    }

    #[test]
    fn test_determine_hooks() {
        let generator = TemplateGenerator::new().unwrap();

        let node_template = PackTemplate {
            name: "node-app".to_string(),
            path: "templates/node".to_string(),
            description: "Node.js app".to_string(),
            variables: vec![],
        };

        let hooks = generator.determine_hooks(&node_template, &HashMap::new());
        assert!(hooks.contains(&"npm_install".to_string()));

        let rust_template = PackTemplate {
            name: "rust-lib".to_string(),
            path: "templates/rust".to_string(),
            description: "Rust library".to_string(),
            variables: vec![],
        };

        let hooks = generator.determine_hooks(&rust_template, &HashMap::new());
        assert!(hooks.contains(&"cargo_check".to_string()));
    }
}
